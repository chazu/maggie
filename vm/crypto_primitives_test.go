package vm

import (
	"crypto/ed25519"
	"crypto/sha256"
	"encoding/hex"
	"testing"
)

// helper: invoke a class-side method on a class and return the result.
func sendClass(t *testing.T, vm *VM, className, selector string, args ...Value) Value {
	t.Helper()
	cls, ok := vm.Globals[className]
	if !ok {
		t.Fatalf("%s not registered", className)
	}
	return vm.Send(cls, selector, args)
}

func TestSha256HashKnownAnswers(t *testing.T) {
	vm := NewVM()

	empty := sendClass(t, vm, "Sha256", "hash:", vm.registry.NewStringValue(""))
	if got := vm.registry.GetStringContent(empty); got != "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" {
		t.Fatalf("hash empty = %q", got)
	}

	abc := sendClass(t, vm, "Sha256", "hash:", vm.registry.NewStringValue("abc"))
	if got := vm.registry.GetStringContent(abc); got != "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" {
		t.Fatalf("hash abc = %q", got)
	}

	// hashBytes: returns raw 32 bytes
	sum := sha256.Sum256([]byte("abc"))
	raw := sendClass(t, vm, "Sha256", "hashBytes:", vm.registry.NewStringValue("abc"))
	got := vm.registry.GetStringContent(raw)
	if got != string(sum[:]) {
		t.Fatalf("hashBytes mismatch")
	}
}

func TestEd25519GenerateSignVerify(t *testing.T) {
	vm := NewVM()
	for i := 0; i < 10; i++ {
		pair := sendClass(t, vm, "Ed25519", "generate")
		dict := vm.registry.GetDictionaryObject(pair)
		if dict == nil {
			t.Fatalf("generate did not return dictionary")
		}
		priv := dict.Data[hashValue(vm.registry, vm.Symbols.SymbolValue("priv"))]
		pub := dict.Data[hashValue(vm.registry, vm.Symbols.SymbolValue("pub"))]
		privBytes := vm.registry.GetStringContent(priv)
		pubBytes := vm.registry.GetStringContent(pub)
		if len(privBytes) != ed25519.SeedSize {
			t.Fatalf("priv size = %d", len(privBytes))
		}
		if len(pubBytes) != ed25519.PublicKeySize {
			t.Fatalf("pub size = %d", len(pubBytes))
		}

		msg := vm.registry.NewStringValue("hello world")
		sig := sendClass(t, vm, "Ed25519", "sign:key:", msg, priv)
		sigBytes := vm.registry.GetStringContent(sig)
		if len(sigBytes) != ed25519.SignatureSize {
			t.Fatalf("sig size = %d", len(sigBytes))
		}

		ok := sendClass(t, vm, "Ed25519", "verify:data:pub:", sig, msg, pub)
		if ok != True {
			t.Fatalf("verify returned false for valid signature")
		}

		bad := sendClass(t, vm, "Ed25519", "verify:data:pub:", sig, vm.registry.NewStringValue("tampered"), pub)
		if bad != False {
			t.Fatalf("verify returned true for tampered data")
		}
	}
}

func TestEd25519GenerateFromSeedDeterministic(t *testing.T) {
	vm := NewVM()
	seed := make([]byte, ed25519.SeedSize)
	for i := range seed {
		seed[i] = byte(i)
	}
	seedVal := vm.registry.NewStringValue(string(seed))

	pair := sendClass(t, vm, "Ed25519", "generateFromSeed:", seedVal)
	dict := vm.registry.GetDictionaryObject(pair)
	if dict == nil {
		t.Fatalf("no dict")
	}
	pubVal := dict.Data[hashValue(vm.registry, vm.Symbols.SymbolValue("pub"))]
	expectedPub := ed25519.NewKeyFromSeed(seed).Public().(ed25519.PublicKey)
	if vm.registry.GetStringContent(pubVal) != string(expectedPub) {
		t.Fatalf("deterministic pubkey mismatch")
	}
}

func TestHexRoundtrip(t *testing.T) {
	vm := NewVM()
	cases := [][]byte{
		{},
		{0x00, 0xFF},
		{0xDE, 0xAD, 0xBE, 0xEF},
		[]byte("arbitrary binary \x00\x01\x02"),
	}
	for _, c := range cases {
		s := vm.registry.NewStringValue(string(c))
		asHex := vm.Send(s, "asHex", nil)
		if vm.registry.GetStringContent(asHex) != hex.EncodeToString(c) {
			t.Fatalf("asHex mismatch for %x", c)
		}
		back := vm.Send(asHex, "fromHex", nil)
		if vm.registry.GetStringContent(back) != string(c) {
			t.Fatalf("fromHex roundtrip mismatch for %x", c)
		}
	}

	// malformed hex returns Failure (not a string with the original content)
	bad := vm.Send(vm.registry.NewStringValue("zz"), "fromHex", nil)
	if IsStringValue(bad) {
		t.Fatalf("fromHex of malformed input should not return a plain string")
	}
}
