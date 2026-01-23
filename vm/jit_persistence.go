package vm

import (
	"bytes"
	"compress/gzip"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"plugin"
	"runtime"
	"strings"
)

// ---------------------------------------------------------------------------
// Persistence Modes
// ---------------------------------------------------------------------------

// PersistenceMode specifies how AOT-compiled code should be persisted.
type PersistenceMode int

const (
	// PersistenceNone disables persistence (hot code is lost on exit)
	PersistenceNone PersistenceMode = iota

	// PersistenceStatic writes Go files for static compilation into binary.
	// Requires rebuilding the application to use compiled code.
	// Pros: Zero runtime overhead, works everywhere
	// Cons: Requires rebuild
	PersistenceStatic

	// PersistencePlugin writes Go files and compiles to .so plugins.
	// Can be loaded at runtime without rebuilding.
	// Pros: No rebuild needed
	// Cons: Linux/macOS only, requires same Go version, some overhead
	PersistencePlugin

	// PersistenceImage stores compiled Go source in the image file.
	// Can be loaded and either interpreted or compiled on-the-fly.
	// Pros: Self-contained, portable
	// Cons: Larger image files, compilation overhead on load
	PersistenceImage
)

// ---------------------------------------------------------------------------
// AOT Image Section Format
// ---------------------------------------------------------------------------

// AOT section magic number (appended after regular image)
var aotSectionMagic = [4]byte{'A', 'O', 'T', '!'}

// AOT section version
const aotSectionVersion uint32 = 1

// AOTEntry represents a single compiled method entry in the image.
type AOTEntry struct {
	ClassName  string // Class name
	MethodName string // Method name (selector)
	GoSource   string // Compiled Go source code
	Checksum   uint32 // CRC32 of original bytecode (for validation)
}

// ---------------------------------------------------------------------------
// Static Build Persistence
// ---------------------------------------------------------------------------

// WriteForStaticBuild writes all compiled methods as Go files for static compilation.
// The generated files should be placed in your source tree and rebuilt with go build.
func (jit *JITCompiler) WriteForStaticBuild(outputDir, packageName string) error {
	if err := os.MkdirAll(outputDir, 0755); err != nil {
		return fmt.Errorf("failed to create output directory: %w", err)
	}

	// Write main package file with all methods
	mainFile := filepath.Join(outputDir, "aot_methods.go")
	content := jit.GenerateAOTPackage(packageName)
	if err := os.WriteFile(mainFile, []byte(content), 0644); err != nil {
		return fmt.Errorf("failed to write AOT package: %w", err)
	}

	return nil
}

// ---------------------------------------------------------------------------
// Plugin Persistence
// ---------------------------------------------------------------------------

// WriteAsPlugin writes compiled methods as a Go plugin source file.
func (jit *JITCompiler) WriteAsPlugin(outputDir, pluginName string) error {
	if runtime.GOOS == "windows" {
		return errors.New("plugin mode not supported on Windows")
	}

	if err := os.MkdirAll(outputDir, 0755); err != nil {
		return fmt.Errorf("failed to create output directory: %w", err)
	}

	// Generate plugin source
	source := jit.generatePluginSource(pluginName)

	// Write source file
	sourceFile := filepath.Join(outputDir, pluginName+".go")
	if err := os.WriteFile(sourceFile, []byte(source), 0644); err != nil {
		return fmt.Errorf("failed to write plugin source: %w", err)
	}

	return nil
}

// BuildPlugin compiles the plugin source to a .so file.
func (jit *JITCompiler) BuildPlugin(sourceDir, pluginName string) (string, error) {
	if runtime.GOOS == "windows" {
		return "", errors.New("plugin mode not supported on Windows")
	}

	sourceFile := filepath.Join(sourceDir, pluginName+".go")
	outputFile := filepath.Join(sourceDir, pluginName+".so")

	// Run go build -buildmode=plugin
	cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", outputFile, sourceFile)
	cmd.Dir = sourceDir

	output, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("failed to build plugin: %w\nOutput: %s", err, output)
	}

	return outputFile, nil
}

// LoadPlugin loads a compiled plugin and registers its methods.
func (jit *JITCompiler) LoadPlugin(pluginPath string) error {
	if runtime.GOOS == "windows" {
		return errors.New("plugin mode not supported on Windows")
	}

	// Open the plugin
	p, err := plugin.Open(pluginPath)
	if err != nil {
		return fmt.Errorf("failed to open plugin: %w", err)
	}

	// Look up the RegisterAll function
	sym, err := p.Lookup("RegisterAll")
	if err != nil {
		return fmt.Errorf("plugin missing RegisterAll function: %w", err)
	}

	// Call RegisterAll with our VM
	registerFn, ok := sym.(func(*VM))
	if !ok {
		return errors.New("RegisterAll has wrong signature, expected func(*VM)")
	}

	registerFn(jit.vm)
	return nil
}

// generatePluginSource generates Go source code for a plugin.
func (jit *JITCompiler) generatePluginSource(pluginName string) string {
	jit.mu.RLock()
	defer jit.mu.RUnlock()

	var sb strings.Builder

	sb.WriteString("// Code generated by Maggie JIT. DO NOT EDIT.\n")
	sb.WriteString("// Plugin: " + pluginName + "\n\n")
	sb.WriteString("package main\n\n")
	sb.WriteString("import (\n")
	sb.WriteString("\t\"math\"\n")
	sb.WriteString("\t. \"github.com/chazu/maggie/vm\"\n")
	sb.WriteString(")\n\n")
	sb.WriteString("var _ = math.Float64frombits // silence unused import\n\n")

	// Write all compiled methods
	for key, code := range jit.hotMethods {
		sb.WriteString(fmt.Sprintf("// %s\n", key))
		sb.WriteString(code)
		sb.WriteString("\n")
	}

	// Generate RegisterAll function (exported for plugin)
	sb.WriteString("// RegisterAll registers all AOT-compiled methods with the VM.\n")
	sb.WriteString("func RegisterAll(vm *VM) {\n")
	sb.WriteString("\ttable := make(AOTDispatchTable)\n")

	for key := range jit.hotMethods {
		parts := strings.SplitN(key, ">>", 2)
		if len(parts) == 2 {
			className := parts[0]
			methodName := parts[1]
			funcName := jit.aot.sanitizeName(className) + "_" + jit.aot.sanitizeName(methodName)
			sb.WriteString(fmt.Sprintf("\ttable[AOTDispatchKey{%q, %q}] = aot_%s\n",
				className, methodName, funcName))
		}
	}

	sb.WriteString("\tvm.RegisterAOTMethods(table)\n")
	sb.WriteString("}\n")

	return sb.String()
}

// ---------------------------------------------------------------------------
// Image Persistence
// ---------------------------------------------------------------------------

// WriteToImage writes compiled methods to a new image file that includes AOT code.
// The AOT section is appended after the regular image data.
func (jit *JITCompiler) WriteToImage(imagePath string, compress bool) error {
	// Read existing image
	existingData, err := os.ReadFile(imagePath)
	if err != nil {
		return fmt.Errorf("failed to read image: %w", err)
	}

	// Check if image already has AOT section (look for magic at various positions)
	baseData := jit.stripExistingAOT(existingData)

	// Build AOT section
	aotSection, err := jit.buildAOTSection(compress)
	if err != nil {
		return fmt.Errorf("failed to build AOT section: %w", err)
	}

	// Write new image: base + AOT section
	newData := make([]byte, len(baseData)+len(aotSection))
	copy(newData, baseData)
	copy(newData[len(baseData):], aotSection)

	if err := os.WriteFile(imagePath, newData, 0644); err != nil {
		return fmt.Errorf("failed to write image: %w", err)
	}

	return nil
}

// LoadFromImage loads AOT-compiled methods from an image file.
// Returns the number of methods loaded.
func (jit *JITCompiler) LoadFromImage(imagePath string) (int, error) {
	data, err := os.ReadFile(imagePath)
	if err != nil {
		return 0, fmt.Errorf("failed to read image: %w", err)
	}

	return jit.LoadFromImageData(data)
}

// LoadFromImageData loads AOT-compiled methods from image data in memory.
func (jit *JITCompiler) LoadFromImageData(data []byte) (int, error) {
	// Find AOT section
	aotStart := jit.findAOTSection(data)
	if aotStart < 0 {
		return 0, nil // No AOT section, not an error
	}

	// Parse AOT section
	entries, err := jit.parseAOTSection(data[aotStart:])
	if err != nil {
		return 0, fmt.Errorf("failed to parse AOT section: %w", err)
	}

	// Store entries in JIT (for later use or package generation)
	jit.mu.Lock()
	for _, entry := range entries {
		key := fmt.Sprintf("%s>>%s", entry.ClassName, entry.MethodName)
		jit.hotMethods[key] = entry.GoSource
		jit.compiledKeys[key] = true
	}
	jit.mu.Unlock()

	return len(entries), nil
}

// stripExistingAOT removes any existing AOT section from image data.
func (jit *JITCompiler) stripExistingAOT(data []byte) []byte {
	aotStart := jit.findAOTSection(data)
	if aotStart < 0 {
		return data
	}
	return data[:aotStart]
}

// findAOTSection finds the start of the AOT section in image data.
// Returns -1 if not found.
func (jit *JITCompiler) findAOTSection(data []byte) int {
	// Search for AOT magic from the end (it's appended)
	magicLen := len(aotSectionMagic)
	for i := len(data) - magicLen; i >= ImageHeaderSize; i-- {
		if bytes.Equal(data[i:i+magicLen], aotSectionMagic[:]) {
			return i
		}
	}
	return -1
}

// buildAOTSection builds the AOT section binary data.
func (jit *JITCompiler) buildAOTSection(compress bool) ([]byte, error) {
	jit.mu.RLock()
	entries := make([]AOTEntry, 0, len(jit.hotMethods))
	for key, code := range jit.hotMethods {
		parts := strings.SplitN(key, ">>", 2)
		if len(parts) == 2 {
			entries = append(entries, AOTEntry{
				ClassName:  parts[0],
				MethodName: parts[1],
				GoSource:   code,
				Checksum:   0, // TODO: compute from bytecode
			})
		}
	}
	jit.mu.RUnlock()

	// Build section data
	var buf bytes.Buffer

	// Magic
	buf.Write(aotSectionMagic[:])

	// Version
	versionBytes := make([]byte, 4)
	binary.LittleEndian.PutUint32(versionBytes, aotSectionVersion)
	buf.Write(versionBytes)

	// Flags (bit 0 = compressed)
	flags := uint32(0)
	if compress {
		flags |= 1
	}
	binary.LittleEndian.PutUint32(versionBytes, flags)
	buf.Write(versionBytes)

	// Entry count
	binary.LittleEndian.PutUint32(versionBytes, uint32(len(entries)))
	buf.Write(versionBytes)

	// Build entry data
	var entryBuf bytes.Buffer
	for _, entry := range entries {
		jit.writeAOTEntry(&entryBuf, &entry)
	}

	// Optionally compress
	entryData := entryBuf.Bytes()
	if compress {
		var compBuf bytes.Buffer
		gz := gzip.NewWriter(&compBuf)
		if _, err := gz.Write(entryData); err != nil {
			return nil, err
		}
		if err := gz.Close(); err != nil {
			return nil, err
		}
		entryData = compBuf.Bytes()
	}

	// Write data length and data
	lenBytes := make([]byte, 4)
	binary.LittleEndian.PutUint32(lenBytes, uint32(len(entryData)))
	buf.Write(lenBytes)
	buf.Write(entryData)

	return buf.Bytes(), nil
}

// writeAOTEntry writes a single AOT entry to a buffer.
func (jit *JITCompiler) writeAOTEntry(buf *bytes.Buffer, entry *AOTEntry) {
	lenBytes := make([]byte, 4)

	// Class name
	binary.LittleEndian.PutUint32(lenBytes, uint32(len(entry.ClassName)))
	buf.Write(lenBytes)
	buf.WriteString(entry.ClassName)

	// Method name
	binary.LittleEndian.PutUint32(lenBytes, uint32(len(entry.MethodName)))
	buf.Write(lenBytes)
	buf.WriteString(entry.MethodName)

	// Go source
	binary.LittleEndian.PutUint32(lenBytes, uint32(len(entry.GoSource)))
	buf.Write(lenBytes)
	buf.WriteString(entry.GoSource)

	// Checksum
	binary.LittleEndian.PutUint32(lenBytes, entry.Checksum)
	buf.Write(lenBytes)
}

// parseAOTSection parses the AOT section from raw data.
func (jit *JITCompiler) parseAOTSection(data []byte) ([]AOTEntry, error) {
	if len(data) < 20 { // magic(4) + version(4) + flags(4) + count(4) + len(4)
		return nil, errors.New("AOT section too short")
	}

	offset := 0

	// Verify magic
	if !bytes.Equal(data[offset:offset+4], aotSectionMagic[:]) {
		return nil, errors.New("invalid AOT section magic")
	}
	offset += 4

	// Version
	version := binary.LittleEndian.Uint32(data[offset:])
	offset += 4
	if version != aotSectionVersion {
		return nil, fmt.Errorf("unsupported AOT version: %d", version)
	}

	// Flags
	flags := binary.LittleEndian.Uint32(data[offset:])
	offset += 4
	compressed := (flags & 1) != 0

	// Entry count
	count := binary.LittleEndian.Uint32(data[offset:])
	offset += 4

	// Data length
	dataLen := binary.LittleEndian.Uint32(data[offset:])
	offset += 4

	if offset+int(dataLen) > len(data) {
		return nil, errors.New("AOT section data truncated")
	}

	entryData := data[offset : offset+int(dataLen)]

	// Decompress if needed
	if compressed {
		gz, err := gzip.NewReader(bytes.NewReader(entryData))
		if err != nil {
			return nil, fmt.Errorf("failed to decompress AOT data: %w", err)
		}
		decompressed, err := io.ReadAll(gz)
		if err != nil {
			return nil, fmt.Errorf("failed to read decompressed data: %w", err)
		}
		entryData = decompressed
	}

	// Parse entries
	entries := make([]AOTEntry, 0, count)
	pos := 0
	for i := uint32(0); i < count; i++ {
		entry, bytesRead, err := jit.readAOTEntry(entryData[pos:])
		if err != nil {
			return nil, fmt.Errorf("failed to read AOT entry %d: %w", i, err)
		}
		entries = append(entries, entry)
		pos += bytesRead
	}

	return entries, nil
}

// readAOTEntry reads a single AOT entry from data.
func (jit *JITCompiler) readAOTEntry(data []byte) (AOTEntry, int, error) {
	var entry AOTEntry
	pos := 0

	// Helper to read a length-prefixed string
	readString := func() (string, error) {
		if pos+4 > len(data) {
			return "", errors.New("unexpected end of data")
		}
		length := binary.LittleEndian.Uint32(data[pos:])
		pos += 4
		if pos+int(length) > len(data) {
			return "", errors.New("string data truncated")
		}
		s := string(data[pos : pos+int(length)])
		pos += int(length)
		return s, nil
	}

	var err error

	// Class name
	entry.ClassName, err = readString()
	if err != nil {
		return entry, 0, err
	}

	// Method name
	entry.MethodName, err = readString()
	if err != nil {
		return entry, 0, err
	}

	// Go source
	entry.GoSource, err = readString()
	if err != nil {
		return entry, 0, err
	}

	// Checksum
	if pos+4 > len(data) {
		return entry, 0, errors.New("unexpected end of data")
	}
	entry.Checksum = binary.LittleEndian.Uint32(data[pos:])
	pos += 4

	return entry, pos, nil
}

// ---------------------------------------------------------------------------
// Convenience Methods
// ---------------------------------------------------------------------------

// SaveCompiledCode saves compiled code using the specified persistence mode.
func (jit *JITCompiler) SaveCompiledCode(mode PersistenceMode, path string) error {
	switch mode {
	case PersistenceNone:
		return nil

	case PersistenceStatic:
		return jit.WriteForStaticBuild(path, "aot")

	case PersistencePlugin:
		if err := jit.WriteAsPlugin(path, "maggie_aot"); err != nil {
			return err
		}
		_, err := jit.BuildPlugin(path, "maggie_aot")
		return err

	case PersistenceImage:
		return jit.WriteToImage(path, true) // compressed

	default:
		return fmt.Errorf("unknown persistence mode: %d", mode)
	}
}

// LoadCompiledCode loads compiled code using the specified persistence mode.
func (jit *JITCompiler) LoadCompiledCode(mode PersistenceMode, path string) error {
	switch mode {
	case PersistenceNone:
		return nil

	case PersistenceStatic:
		// Static mode requires rebuild, can't load at runtime
		return errors.New("static mode requires rebuilding the binary")

	case PersistencePlugin:
		return jit.LoadPlugin(path)

	case PersistenceImage:
		_, err := jit.LoadFromImage(path)
		return err

	default:
		return fmt.Errorf("unknown persistence mode: %d", mode)
	}
}
