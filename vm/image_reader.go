package vm

import (
	"fmt"
	"io"
	"os"
)

// ---------------------------------------------------------------------------
// Bytecode Selector Remapping
// ---------------------------------------------------------------------------

// remapBytecodeSelectors scans bytecode and remaps selector IDs from image space
// to VM space. This is necessary because selector IDs in the image may differ
// from VM selector IDs due to primitives being registered before the image is
// loaded. The selectorIDMap maps image selector IDs to VM selector IDs.
func remapBytecodeSelectors(bytecode []byte, selectorIDMap map[int]int) {
	i := 0
	for i < len(bytecode) {
		op := Opcode(bytecode[i])
		switch op {
		case OpSend, OpSendSuper, OpTailSend:
			// Format: opcode (1 byte) + selector (2 bytes little-endian) + argc (1 byte)
			if i+3 < len(bytecode) {
				// Read original selector ID (little-endian 16-bit)
				imageSelectorID := int(bytecode[i+1]) | (int(bytecode[i+2]) << 8)

				// Look up the VM selector ID
				if vmSelectorID, ok := selectorIDMap[imageSelectorID]; ok {
					// Only remap if different
					if vmSelectorID != imageSelectorID {
						// Write remapped selector ID (little-endian)
						bytecode[i+1] = byte(vmSelectorID)
						bytecode[i+2] = byte(vmSelectorID >> 8)
					}
				}
			}
			i += 4 // opcode + 2-byte selector + 1-byte argc
		default:
			// Skip instruction based on operand size
			info := op.Info()
			i += 1 + info.OperandBytes
		}
	}
}

// ---------------------------------------------------------------------------
// VM Load Methods
// ---------------------------------------------------------------------------

// LoadImage loads a VM state from an image file.
func (vm *VM) LoadImage(path string) error {
	f, err := os.Open(path)
	if err != nil {
		return fmt.Errorf("failed to open image file: %w", err)
	}
	defer f.Close()

	return vm.LoadImageFrom(f)
}

// LoadImageFrom loads a VM state from an io.Reader.
// It reads all bytes into memory and delegates to LoadImageFromBytes.
func (vm *VM) LoadImageFrom(r io.Reader) error {
	data, err := io.ReadAll(r)
	if err != nil {
		return fmt.Errorf("failed to read image data: %w", err)
	}
	return vm.LoadImageFromBytes(data)
}

// LoadImageFromBytes loads a VM state from a byte slice.
// Only CBOR format is supported.
func (vm *VM) LoadImageFromBytes(data []byte) error {
	cr, err := NewCborImageReader(data)
	if err != nil {
		return fmt.Errorf("failed to parse CBOR image: %w", err)
	}
	return cr.ReadAll(vm)
}
