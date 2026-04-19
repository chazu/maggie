package vm

import (
	"crypto/ed25519"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
)

// registerCryptoPrimitives registers the Sha256 and Ed25519 utility classes
// as thin wrappers around the Go crypto standard library, plus hex helpers
// on String.
//
// ByteArray semantics: Maggie strings are used as raw byte buffers. String
// values (NewStringValue / GetStringContent) preserve arbitrary bytes.
func (vm *VM) registerCryptoPrimitives() {
	vm.registerSha256Primitives()
	vm.registerEd25519Primitives()
	vm.registerHexPrimitives()
}

func (vm *VM) registerSha256Primitives() {
	c := vm.createClass("Sha256", vm.ObjectClass)
	vm.Globals["Sha256"] = vm.classValue(c)

	// Sha256 hash: aByteArrayOrString — returns lowercase hex string
	c.AddClassMethod1(vm.Selectors, "hash:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		data := v.valueToString(arg)
		sum := sha256.Sum256([]byte(data))
		return v.registry.NewStringValue(hex.EncodeToString(sum[:]))
	})

	// Sha256 hashBytes: aByteArrayOrString — returns byte array (as string)
	c.AddClassMethod1(vm.Selectors, "hashBytes:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		data := v.valueToString(arg)
		sum := sha256.Sum256([]byte(data))
		return v.registry.NewStringValue(string(sum[:]))
	})
}

func (vm *VM) registerEd25519Primitives() {
	c := vm.createClass("Ed25519", vm.ObjectClass)
	vm.Globals["Ed25519"] = vm.classValue(c)

	// Ed25519 generate — returns Dictionary {#priv -> 32-byte seed, #pub -> 32-byte pubkey}
	c.AddClassMethod0(vm.Selectors, "generate", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		pub, priv, err := ed25519.GenerateKey(rand.Reader)
		if err != nil {
			return v.newFailureResult("Ed25519 generate: " + err.Error())
		}
		seed := priv.Seed()
		return v.ed25519KeyPairDict(seed, pub)
	})

	// Ed25519 generateFromSeed: aByteArray — returns same dict; seed must be 32 bytes
	c.AddClassMethod1(vm.Selectors, "generateFromSeed:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		seed := []byte(v.valueToString(arg))
		if len(seed) != ed25519.SeedSize {
			return v.newFailureResult("Ed25519 generateFromSeed: seed must be 32 bytes")
		}
		priv := ed25519.NewKeyFromSeed(seed)
		pub := priv.Public().(ed25519.PublicKey)
		return v.ed25519KeyPairDict(seed, pub)
	})

	// Ed25519 sign: data key: priv — priv is a 32-byte seed OR 64-byte key. Returns 64-byte signature.
	c.AddClassMethod2(vm.Selectors, "sign:key:", func(vmPtr interface{}, recv Value, dataVal, keyVal Value) Value {
		v := vmPtr.(*VM)
		data := []byte(v.valueToString(dataVal))
		keyBytes := []byte(v.valueToString(keyVal))

		var priv ed25519.PrivateKey
		switch len(keyBytes) {
		case ed25519.SeedSize: // 32 bytes — treat as seed
			priv = ed25519.NewKeyFromSeed(keyBytes)
		case ed25519.PrivateKeySize: // 64 bytes — full private key
			priv = ed25519.PrivateKey(keyBytes)
		default:
			return v.newFailureResult("Ed25519 sign:key: private key must be 32 or 64 bytes")
		}
		sig := ed25519.Sign(priv, data)
		return v.registry.NewStringValue(string(sig))
	})

	// Ed25519 verify: sig data: bytes pub: pubkey — returns Boolean
	c.AddClassMethod3(vm.Selectors, "verify:data:pub:", func(vmPtr interface{}, recv Value, sigVal, dataVal, pubVal Value) Value {
		v := vmPtr.(*VM)
		sig := []byte(v.valueToString(sigVal))
		data := []byte(v.valueToString(dataVal))
		pub := []byte(v.valueToString(pubVal))
		if len(pub) != ed25519.PublicKeySize || len(sig) != ed25519.SignatureSize {
			return False
		}
		if ed25519.Verify(ed25519.PublicKey(pub), data, sig) {
			return True
		}
		return False
	})
}

// ed25519KeyPairDict builds a dictionary with #priv (32-byte seed) and #pub (32-byte pubkey).
func (vm *VM) ed25519KeyPairDict(seed, pub []byte) Value {
	dictVal := vm.registry.NewDictionaryValue()
	dict := vm.registry.GetDictionaryObject(dictVal)
	if dict == nil {
		return Nil
	}
	privKey := vm.Symbols.SymbolValue("priv")
	pubKey := vm.Symbols.SymbolValue("pub")
	privVal := vm.registry.NewStringValue(string(seed))
	pubVal := vm.registry.NewStringValue(string(pub))
	h1 := hashValue(vm.registry, privKey)
	h2 := hashValue(vm.registry, pubKey)
	dict.Data[h1] = privVal
	dict.Keys[h1] = privKey
	dict.Data[h2] = pubVal
	dict.Keys[h2] = pubKey
	return dictVal
}

func (vm *VM) registerHexPrimitives() {
	// String>>asHex — treat string content as bytes, return lowercase hex.
	vm.StringClass.AddMethod0(vm.Selectors, "asHex", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		return v.registry.NewStringValue(hex.EncodeToString([]byte(s)))
	})

	// ByteArray>>asHex — mirror for when receivers are classified as ByteArray.
	vm.ByteArrayClass.AddMethod0(vm.Selectors, "asHex", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.valueToString(recv)
		return v.registry.NewStringValue(hex.EncodeToString([]byte(s)))
	})

	// String>>fromHex — decode hex string into a byte array (represented as a String).
	// Raises on malformed input by returning a Failure result.
	vm.StringClass.AddMethod0(vm.Selectors, "fromHex", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		b, err := hex.DecodeString(s)
		if err != nil {
			return v.newFailureResult("fromHex: " + err.Error())
		}
		return v.registry.NewStringValue(string(b))
	})
}
