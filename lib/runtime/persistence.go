package runtime

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	_ "github.com/mattn/go-sqlite3"
)

// ErrInstanceNotFound indicates the requested instance doesn't exist
var ErrInstanceNotFound = errors.New("instance not found")

// Persistence handles SQLite storage for instances
type Persistence struct {
	db          *sql.DB
	dbPath      string
	os          *ObjectSpace
	blockRunner *BlockRunner
	mu          sync.Mutex
}

// NewPersistence creates a new persistence layer
func NewPersistence(dbPath string, os *ObjectSpace) (*Persistence, error) {
	p := &Persistence{
		dbPath: dbPath,
		os:     os,
	}

	// Open database
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return nil, fmt.Errorf("opening database: %w", err)
	}
	p.db = db

	// Set busy timeout for concurrent access
	_, err = db.Exec("PRAGMA busy_timeout = 5000")
	if err != nil {
		db.Close()
		return nil, fmt.Errorf("setting busy timeout: %w", err)
	}

	// Create table if needed
	_, err = db.Exec(`CREATE TABLE IF NOT EXISTS instances (
		id TEXT PRIMARY KEY,
		data JSON NOT NULL
	)`)
	if err != nil {
		db.Close()
		return nil, fmt.Errorf("creating table: %w", err)
	}

	return p, nil
}

// NewPersistenceDefault creates persistence with default database path
func NewPersistenceDefault(objSpace *ObjectSpace) (*Persistence, error) {
	dbPath := os.Getenv("SQLITE_JSON_DB")
	if dbPath == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("getting home dir: %w", err)
		}
		dbPath = filepath.Join(home, ".trashtalk", "instances.db")
	}
	return NewPersistence(dbPath, objSpace)
}

// Close closes the database connection
func (p *Persistence) Close() error {
	if p.db != nil {
		return p.db.Close()
	}
	return nil
}

// SetBlockRunner sets the block runner for resolving block references
func (p *Persistence) SetBlockRunner(br *BlockRunner) {
	p.blockRunner = br
}

// Save persists an instance to the database
func (p *Persistence) Save(inst *Instance) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	data := inst.ToJSON()

	_, err := p.db.Exec(
		"INSERT OR REPLACE INTO instances (id, data) VALUES (?, json(?))",
		inst.ID, data,
	)
	if err != nil {
		return fmt.Errorf("saving instance: %w", err)
	}

	return nil
}

// Load retrieves an instance from the database
func (p *Persistence) Load(id string) (*Instance, error) {
	// Check if already in object space
	if inst := p.os.GetInstance(id); inst != nil {
		return inst, nil
	}

	// Load from database
	var data string
	err := p.db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, ErrInstanceNotFound
		}
		return nil, fmt.Errorf("querying instance: %w", err)
	}

	// Parse JSON
	inst, err := p.instanceFromJSON(id, data)
	if err != nil {
		return nil, err
	}

	// Register in object space
	p.os.RegisterInstance(inst)

	return inst, nil
}

// instanceFromJSON parses instance JSON and creates an Instance
func (p *Persistence) instanceFromJSON(id, jsonData string) (*Instance, error) {
	var raw map[string]interface{}
	if err := json.Unmarshal([]byte(jsonData), &raw); err != nil {
		return nil, fmt.Errorf("parsing instance JSON: %w", err)
	}

	className, _ := raw["class"].(string)
	if className == "" {
		return nil, fmt.Errorf("instance missing class")
	}

	// Get class (may be nil if not registered)
	class := p.os.GetClass(className)

	inst := &Instance{
		ID:        id,
		Class:     class,
		ClassName: className,
		Vars:      make(map[string]Value),
	}

	// Extract _vars list
	var varNames []string
	if vars, ok := raw["_vars"].([]interface{}); ok {
		for _, v := range vars {
			if s, ok := v.(string); ok {
				varNames = append(varNames, s)
			}
		}
	}

	// Load instance variables
	for _, name := range varNames {
		if val, ok := raw[name]; ok {
			inst.Vars[name] = valueFromInterfaceWithBlockRunner(val, p.blockRunner)
		} else {
			inst.Vars[name] = NilValue()
		}
	}

	return inst, nil
}

// valueFromInterface converts a JSON-parsed interface{} to a Value
// This is a package-level function for use without a Persistence instance
func valueFromInterface(v interface{}) Value {
	return valueFromInterfaceWithBlockRunner(v, nil)
}

// valueFromInterfaceWithBlockRunner converts a JSON-parsed interface{} to a Value
// with optional block resolution via BlockRunner
func valueFromInterfaceWithBlockRunner(v interface{}, br *BlockRunner) Value {
	if v == nil {
		return NilValue()
	}
	switch x := v.(type) {
	case bool:
		return BoolValue(x)
	case float64:
		// Check if it's an integer
		if x == float64(int64(x)) {
			return IntValue(int64(x))
		}
		return FloatValue(x)
	case string:
		return StringValue(x)
	case []interface{}:
		arr := NewArray()
		for _, elem := range x {
			arr.Push(valueFromInterfaceWithBlockRunner(elem, br))
		}
		return ArrayValue(arr)
	case map[string]interface{}:
		// Could be an object or instance reference
		if blockID, ok := x["_block_id"].(string); ok {
			// Block reference - look up from BlockRunner if available
			if br != nil {
				if block := br.GetBlock(blockID); block != nil {
					return BlockValue(block)
				}
			}
			// Fall back to storing the block ID as a string for later resolution
			return StringValue(blockID)
		}
		// Return as JSON string for now
		data, _ := json.Marshal(x)
		return StringValue(string(data))
	default:
		return StringValue(fmt.Sprintf("%v", v))
	}
}

// instanceFromJSONNoID parses JSON to create an instance without persistence
// Used when persistence is disabled
func instanceFromJSONNoID(jsonStr string, objSpace *ObjectSpace) (*Instance, error) {
	var raw map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &raw); err != nil {
		return nil, fmt.Errorf("parsing instance JSON: %w", err)
	}

	className, _ := raw["class"].(string)
	if className == "" {
		return nil, fmt.Errorf("instance missing class")
	}

	// Generate an ID if not present
	id, _ := raw["id"].(string)
	if id == "" {
		id = objSpace.GenerateID(className)
	}

	// Get class (may be nil if not registered)
	class := objSpace.GetClass(className)

	inst := &Instance{
		ID:        id,
		Class:     class,
		ClassName: className,
		Vars:      make(map[string]Value),
	}

	// Extract _vars list
	var varNames []string
	if vars, ok := raw["_vars"].([]interface{}); ok {
		for _, v := range vars {
			if s, ok := v.(string); ok {
				varNames = append(varNames, s)
			}
		}
	}

	// Load instance variables
	for _, name := range varNames {
		if val, ok := raw[name]; ok {
			inst.Vars[name] = valueFromInterface(val)
		} else {
			inst.Vars[name] = NilValue()
		}
	}

	// Register in object space
	objSpace.RegisterInstance(inst)

	return inst, nil
}

// Delete removes an instance from the database
func (p *Persistence) Delete(id string) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	_, err := p.db.Exec("DELETE FROM instances WHERE id = ?", id)
	if err != nil {
		return fmt.Errorf("deleting instance: %w", err)
	}

	// Also remove from object space
	p.os.RemoveInstance(id)

	return nil
}

// FindByClass returns all instance IDs for a given class
func (p *Persistence) FindByClass(className string) ([]string, error) {
	rows, err := p.db.Query("SELECT id FROM instances WHERE json_extract(data, '$.class') = ?", className)
	if err != nil {
		return nil, fmt.Errorf("querying by class: %w", err)
	}
	defer rows.Close()

	var ids []string
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("scanning id: %w", err)
		}
		ids = append(ids, id)
	}
	return ids, rows.Err()
}

// SaveAll persists all instances from the object space
func (p *Persistence) SaveAll() error {
	p.os.instMu.RLock()
	instances := make([]*Instance, 0, len(p.os.instances))
	for _, inst := range p.os.instances {
		instances = append(instances, inst)
	}
	p.os.instMu.RUnlock()

	for _, inst := range instances {
		if err := p.Save(inst); err != nil {
			return err
		}
	}
	return nil
}

// LoadAll loads all instances from the database into the object space
func (p *Persistence) LoadAll() error {
	rows, err := p.db.Query("SELECT id, data FROM instances")
	if err != nil {
		return fmt.Errorf("querying all instances: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var id, data string
		if err := rows.Scan(&id, &data); err != nil {
			return fmt.Errorf("scanning instance: %w", err)
		}

		inst, err := p.instanceFromJSON(id, data)
		if err != nil {
			// Log but continue
			fmt.Fprintf(os.Stderr, "Warning: failed to load instance %s: %v\n", id, err)
			continue
		}

		p.os.RegisterInstance(inst)
	}
	return rows.Err()
}

