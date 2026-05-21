package vm

import "testing"

func FuzzImageReader(f *testing.F) {
	vm := NewVM()
	validImage, _ := vm.SaveImageBytes()
	f.Add(validImage)

	f.Fuzz(func(t *testing.T, data []byte) {
		vm := NewVM()
		_ = vm.LoadImageFromBytes(data)
	})
}
