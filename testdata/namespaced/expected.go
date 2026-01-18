package main

import (
	"bufio"
	"crypto/sha256"
	"database/sql"
	_ "embed"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	uuid "github.com/google/uuid"
	_ "github.com/mattn/go-sqlite3"
	unix "golang.org/x/sys/unix"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

//go:embed MyApp__Counter.trash
var _sourceCode string

var _contentHash string

func init() {
	hash := sha256.Sum256([]byte(_sourceCode))
	_contentHash = hex.EncodeToString(hash[:])
}

var ErrUnknownSelector = errors.New("unknown selector")

type Counter struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
	Value     string   `json:"value"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: MyApp__Counter.native <instance_id> <selector> [args...]")
		fmt.Fprintln(os.Stderr, "       MyApp__Counter.native --source")
		fmt.Fprintln(os.Stderr, "       MyApp__Counter.native --hash")
		os.Exit(1)
	}

	switch os.Args[1] {
	case "--source":
		fmt.Print(_sourceCode)
		return
	case "--hash":
		fmt.Println(_contentHash)
		return
	case "--info":
		fmt.Printf("Class: MyApp::Counter\nPackage: MyApp\nHash: %s\nSource length: %d bytes\n", _contentHash, len(_sourceCode))
		return
	case "--serve":
		runServeMode()
		return
	}

	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "Usage: MyApp__Counter.native <instance_id> <selector> [args...]")
		os.Exit(1)
	}

	receiver := os.Args[1]
	selector := os.Args[2]
	args := os.Args[3:]

	if receiver == "Counter" || receiver == "MyApp::Counter" {
		result, err := dispatchClass(selector, args)
		if err != nil {
			if errors.Is(err, ErrUnknownSelector) {
				os.Exit(200)
			}
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		if result != "" {
			fmt.Println(result)
		}
		return
	}

	db, err := openDB()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	instance, err := loadInstance(db, receiver)
	if err != nil {
		os.Exit(200)
	}

	result, err := dispatch(instance, receiver, selector, args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			os.Exit(200)
		}
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if selector == "delete" {
		if err := deleteInstance(db, receiver); err != nil {
			fmt.Fprintf(os.Stderr, "Error deleting instance: %v\n", err)
			os.Exit(1)
		}
	} else {
		if err := saveInstance(db, receiver, instance); err != nil {
			fmt.Fprintf(os.Stderr, "Error saving instance: %v\n", err)
			os.Exit(1)
		}
	}

	if result != "" {
		fmt.Println(result)
	}
}

func openDB() (*sql.DB, error) {
	dbPath := os.Getenv("SQLITE_JSON_DB")
	if dbPath == "" {
		home, _ := os.UserHomeDir()
		dbPath = filepath.Join(home, ".trashtalk", "instances.db")
	}
	return sql.Open("sqlite3", dbPath)
}

func loadInstance(db *sql.DB, id string) (*Counter, error) {
	var data string
	err := db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		return nil, err
	}
	var instance Counter
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, err
	}
	return &instance, nil
}

func saveInstance(db *sql.DB, id string, instance *Counter) error {
	data, err := json.Marshal(instance)
	if err != nil {
		return err
	}
	_, err = db.Exec("INSERT OR REPLACE INTO instances (id, data) VALUES (?, json(?))", id, string(data))
	return err
}

func generateInstanceID(className string) string {
	uuid := uuid.New().String()
	return strings.ToLower(className) + "_" + uuid
}

func createInstance(db *sql.DB, id string, instance *Counter) error {
	data, err := json.Marshal(instance)
	if err != nil {
		return err
	}
	_, err = db.Exec("INSERT INTO instances (id, data) VALUES (?, json(?))", id, string(data))
	return err
}

func deleteInstance(db *sql.DB, id string) error {
	_, err := db.Exec("DELETE FROM instances WHERE id = ?", id)
	return err
}

func sendMessage(receiver interface{}, selector string, args ...interface{}) string {
	receiverStr := fmt.Sprintf("%v", receiver)
	cmdArgs := []string{receiverStr, selector}
	for _, arg := range args {
		cmdArgs = append(cmdArgs, fmt.Sprintf("%v", arg))
	}
	home, _ := os.UserHomeDir()
	dispatchScript := filepath.Join(home, ".trashtalk", "bin", "trash-send")
	cmd := exec.Command(dispatchScript, cmdArgs...)
	output, _ := cmd.Output()
	return strings.TrimSpace(string(output))
}

// ServeRequest is the JSON request format for --serve mode
type ServeRequest struct {
	InstanceID string   `json:"instance_id"`
	Instance   string   `json:"instance"`
	Selector   string   `json:"selector"`
	Args       []string `json:"args"`
}

// ServeResponse is the JSON response format for --serve mode
type ServeResponse struct {
	Instance string `json:"instance,omitempty"`
	Result   string `json:"result,omitempty"`
	ExitCode int    `json:"exit_code"`
	Error    string `json:"error,omitempty"`
}

func runServeMode() {
	db, err := openDB()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	scanner := bufio.NewScanner(os.Stdin)
	// Increase buffer for large instance JSON
	buf := make([]byte, 1048576)
	scanner.Buffer(buf, len(buf))

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		var req ServeRequest
		if err := json.Unmarshal([]byte(line), &req); err != nil {
			respond(ServeResponse{
				Error:    "invalid JSON: " + err.Error(),
				ExitCode: 1,
			})
			continue
		}

		resp := handleServeRequest(db, &req)
		respond(resp)
	}
}

func respond(resp ServeResponse) {
	out, _ := json.Marshal(resp)
	fmt.Println(string(out))
}

func handleServeRequest(db *sql.DB, req *ServeRequest) ServeResponse {
	if req.Instance == "" || req.Instance == "Counter" || req.Instance == "MyApp::Counter" {
		result, err := dispatchClass(req.Selector, req.Args)
		if err != nil {
			if errors.Is(err, ErrUnknownSelector) {
				return ServeResponse{ExitCode: 200}
			}
			return ServeResponse{
				Error:    err.Error(),
				ExitCode: 1,
			}
		}
		return ServeResponse{
			ExitCode: 0,
			Result:   result,
		}
	}

	var instance Counter
	if err := json.Unmarshal([]byte(req.Instance), &instance); err != nil {
		return ServeResponse{
			Error:    "invalid instance JSON: " + err.Error(),
			ExitCode: 1,
		}
	}

	result, err := dispatch(&instance, req.InstanceID, req.Selector, req.Args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			return ServeResponse{ExitCode: 200}
		}
		return ServeResponse{
			Error:    err.Error(),
			ExitCode: 1,
		}
	}

	if req.Selector == "delete" {
		if err := deleteInstance(db, req.InstanceID); err != nil {
			return ServeResponse{
				Error:    err.Error(),
				ExitCode: 1,
			}
		}
		return ServeResponse{
			ExitCode: 0,
			Result:   result,
		}
	}

	updatedJSON, _ := json.Marshal(&instance)
	return ServeResponse{
		ExitCode: 0,
		Instance: string(updatedJSON),
		Result:   result,
	}
}

// Common conversion helpers
func _boolToString(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

func _toStr(v interface{}) string {
	if v == nil {
		return ""
	}
	return fmt.Sprintf("%v", v)
}

// Array helpers for native slice operations
func _arrayFirst(arr []interface{}) interface{} {
	if len(arr) == 0 {
		return nil
	}
	return arr[0]
}

func _arrayLast(arr []interface{}) interface{} {
	if len(arr) == 0 {
		return nil
	}
	return arr[len(arr)-1]
}

func _arrayAtPut(arr []interface{}, idx int, val interface{}) []interface{} {
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return arr
	}
	result := make([]interface{}, len(arr))
	copy(result, arr)
	result[idx] = val
	return result
}

func _arrayRemoveAt(arr []interface{}, idx int) []interface{} {
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return arr
	}
	result := make([]interface{}, 0, len(arr)-1)
	result = append(result, arr[:idx]...)
	result = append(result, arr[idx+1:]...)
	return result
}

// Map helpers for native map operations
func _mapKeys(m map[string]interface{}) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func _mapValues(m map[string]interface{}) []interface{} {
	vals := make([]interface{}, 0, len(m))
	for _, v := range m {
		vals = append(vals, v)
	}
	return vals
}

func _mapHasKey(m map[string]interface{}, key string) bool {
	_, ok := m[key]
	return ok
}

func _mapAtPut(m map[string]interface{}, key string, val interface{}) map[string]interface{} {
	result := make(map[string]interface{}, len(m)+1)
	for k, v := range m {
		result[k] = v
	}
	result[key] = val
	return result
}

func _mapRemoveKey(m map[string]interface{}, key string) map[string]interface{} {
	result := make(map[string]interface{}, len(m))
	for k, v := range m {
		if k != key {
			result[k] = v
		}
	}
	return result
}

// JSON string parsing helpers (for string-typed variables containing JSON)
func _jsonArrayLen(jsonStr string) int {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return 0
	}
	return len(arr)
}

func _jsonArrayFirst(jsonStr string) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil || len(arr) == 0 {
		return ""
	}
	return fmt.Sprintf("%v", arr[0])
}

func _jsonArrayLast(jsonStr string) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil || len(arr) == 0 {
		return ""
	}
	return fmt.Sprintf("%v", arr[len(arr)-1])
}

func _jsonArrayIsEmpty(jsonStr string) bool {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return true
	}
	return len(arr) == 0
}

func _jsonArrayPush(jsonVal interface{}, val interface{}) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var arr []interface{}
	json.Unmarshal([]byte(jsonStr), &arr)
	arr = append(arr, val)
	result, _ := json.Marshal(arr)
	return string(result)
}

func _jsonArrayAt(jsonStr string, idx int) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return ""
	}
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return ""
	}
	return fmt.Sprintf("%v", arr[idx])
}

func _jsonArrayAtPut(jsonStr string, idx int, val interface{}) string {
	var arr []interface{}
	json.Unmarshal([]byte(jsonStr), &arr)
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx >= 0 && idx < len(arr) {
		arr[idx] = val
	}
	result, _ := json.Marshal(arr)
	return string(result)
}

func _jsonArrayRemoveAt(jsonStr string, idx int) string {
	var arr []interface{}
	json.Unmarshal([]byte(jsonStr), &arr)
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx >= 0 && idx < len(arr) {
		arr = append(arr[:idx], arr[idx+1:]...)
	}
	result, _ := json.Marshal(arr)
	return string(result)
}

func _jsonObjectLen(jsonStr string) int {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return 0
	}
	return len(m)
}

func _jsonObjectKeys(jsonStr string) []string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return nil
	}
	return _mapKeys(m)
}

func _jsonObjectValues(jsonStr string) []interface{} {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return nil
	}
	return _mapValues(m)
}

func _jsonObjectIsEmpty(jsonStr string) bool {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return true
	}
	return len(m) == 0
}

func _jsonObjectAt(jsonVal any, key string) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return ""
	}
	if v, ok := m[key]; ok {
		return fmt.Sprintf("%v", v)
	}
	return ""
}

func _jsonObjectAtPut(jsonVal any, key string, val any) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	json.Unmarshal([]byte(jsonStr), &m)
	if m == nil {
		m = make(map[string]interface{})
	}
	m[key] = val
	result, _ := json.Marshal(m)
	return string(result)
}

func _jsonObjectHasKey(jsonVal any, key string) bool {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return false
	}
	_, ok := m[key]
	return ok
}

func _jsonObjectRemoveKey(jsonVal any, key string) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	json.Unmarshal([]byte(jsonStr), &m)
	delete(m, key)
	result, _ := json.Marshal(m)
	return string(result)
}

// String primitive helpers
func _stringSubstring(s string, start int, length int) string {
	if start < 0 {
		start = 0
	}
	if start >= len(s) {
		return ""
	}
	end := start + length
	if end > len(s) {
		end = len(s)
	}
	return s[start:end]
}

// File primitive helpers
func _fileExists(path string) string {
	_, err := os.Stat(path)
	return _boolToString(err == nil)
}

func _fileIsFile(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Mode().IsRegular())
}

func _fileIsDirectory(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.IsDir())
}

func _fileIsSymlink(path string) string {
	info, err := os.Lstat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Mode()&os.ModeSymlink != 0)
}

func _fileIsFifo(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Mode()&os.ModeNamedPipe != 0)
}

func _fileIsSocket(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Mode()&os.ModeSocket != 0)
}

func _fileIsBlockDevice(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Mode()&os.ModeDevice != 0 && info.Mode()&os.ModeCharDevice == 0)
}

func _fileIsCharDevice(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Mode()&os.ModeCharDevice != 0)
}

func _fileIsReadable(path string) string {
	err := unix.Access(path, unix.R_OK)
	return _boolToString(err == nil)
}

func _fileIsWritable(path string) string {
	err := unix.Access(path, unix.W_OK)
	return _boolToString(err == nil)
}

func _fileIsExecutable(path string) string {
	err := unix.Access(path, unix.X_OK)
	return _boolToString(err == nil)
}

func _fileIsEmpty(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Size() == 0)
}

func _fileNotEmpty(path string) string {
	info, err := os.Stat(path)
	if err != nil {
		return "false"
	}
	return _boolToString(info.Size() > 0)
}

func _fileIsNewer(path1 string, path2 string) string {
	info1, err1 := os.Stat(path1)
	info2, err2 := os.Stat(path2)
	if err1 != nil || err2 != nil {
		return "false"
	}
	return _boolToString(info1.ModTime().After(info2.ModTime()))
}

func _fileIsOlder(path1 string, path2 string) string {
	info1, err1 := os.Stat(path1)
	info2, err2 := os.Stat(path2)
	if err1 != nil || err2 != nil {
		return "false"
	}
	return _boolToString(info1.ModTime().Before(info2.ModTime()))
}

func _fileIsSame(path1 string, path2 string) string {
	info1, err1 := os.Stat(path1)
	info2, err2 := os.Stat(path2)
	if err1 != nil || err2 != nil {
		return "false"
	}
	return _boolToString(os.SameFile(info1, info2))
}

// toInt converts interface{} to int for arithmetic in iteration blocks
func toInt(v interface{}) int {
	switch x := v.(type) {
	case int:
		return x
	case int64:
		return int(x)
	case float64:
		return int(x)
	case string:
		n, _ := strconv.Atoi(x)
		return n
	default:
		return 0
	}
}

// toBool converts interface{} to bool for predicates in iteration blocks
func toBool(v interface{}) bool {
	switch x := v.(type) {
	case bool:
		return x
	case int:
		return x != 0
	case string:
		return x != ""
	default:
		return v != nil
	}
}

// invokeBlock calls a Trashtalk block through the Bash runtime
// blockID is the instance ID of the Block object
// args are the values to pass to the block
func invokeBlock(blockID string, args ...interface{}) string {
	var cmdStr string
	switch len(args) {
	case 0:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q value", blockID)
	case 1:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q", blockID, fmt.Sprint(args[0]))
	case 2:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q and: %q", blockID, fmt.Sprint(args[0]), fmt.Sprint(args[1]))
	default:
		return ""
	}

	cmd := exec.Command("bash", "-c", cmdStr)
	output, _ := cmd.Output()
	return strings.TrimSpace(string(output))
}

func dispatch(c *Counter, instanceID string, selector string, args []string) (string, error) {
	switch selector {
	case "class":
		return "MyApp::Counter", nil
	case "id":
		return instanceID, nil
	case "delete":
		return instanceID, nil
	case "getValue":
		return c.GetValue(), nil
	case "increment":
		return c.Increment(), nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func dispatchClass(selector string, args []string) (string, error) {
	switch selector {
	case "new":
		id := generateInstanceID("Counter")
		instance := &Counter{
			Class:     "MyApp::Counter",
			CreatedAt: time.Now().Format(time.RFC3339),
			Value:     "0",
		}
		db, err := openDB()
		if err != nil {
			return "", err
		}
		defer db.Close()
		if err := createInstance(db, id, instance); err != nil {
			return "", err
		}
		return id, nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func (c *Counter) GetValue() string {
	return c.Value
}

func (c *Counter) Increment() string {
	c.Value = strconv.Itoa(toInt(c.Value) + toInt(1))
	return c.Value
}
