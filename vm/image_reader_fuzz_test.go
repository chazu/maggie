package vm

import "testing"

func FuzzCborImageReader(f *testing.F) {
	vm := NewVM()
	validImage, _ := vm.SaveImageCborBytes()
	f.Add(validImage)

	f.Fuzz(func(t *testing.T, data []byte) {
		vm := NewVM()
		_ = vm.LoadImageFromBytes(data)
	})
}
