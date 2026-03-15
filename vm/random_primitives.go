package vm

import (
	"math/rand/v2"
	"reflect"
)

// ---------------------------------------------------------------------------
// Random Primitives
// ---------------------------------------------------------------------------
// Random provides random number generation. Class methods use the global
// source (auto-seeded by the runtime). Instance methods use a per-instance
// *rand.Rand stored via GoObjectWrapper.

func (vm *VM) registerRandomPrimitives() {
	c := vm.RandomClass

	// Register *rand.Rand as a Go type so GoObjectClass dispatch works
	vm.RegisterGoType("Random", reflect.TypeOf((*rand.Rand)(nil)))

	// --- Class methods (use global rand source) ---

	// Random next → float [0.0, 1.0)
	c.AddClassMethod0(vm.Selectors, "next", func(_ interface{}, _ Value) Value {
		return FromFloat64(rand.Float64())
	})

	// Random nextInt: n → int [0, n)
	c.AddClassMethod1(vm.Selectors, "nextInt:", func(_ interface{}, _ Value, arg Value) Value {
		n := int64(0)
		if arg.IsSmallInt() {
			n = arg.SmallInt()
		} else {
			return Nil
		}
		if n <= 0 {
			return FromSmallInt(0)
		}
		return FromSmallInt(rand.Int64N(n))
	})

	// Random nextBetween: lo and: hi → int in [lo, hi]
	c.AddClassMethod2(vm.Selectors, "nextBetween:and:", func(_ interface{}, _ Value, loVal, hiVal Value) Value {
		if !loVal.IsSmallInt() || !hiVal.IsSmallInt() {
			return Nil
		}
		lo := loVal.SmallInt()
		hi := hiVal.SmallInt()
		if lo > hi {
			lo, hi = hi, lo
		}
		span := hi - lo + 1
		if span <= 0 {
			return FromSmallInt(lo)
		}
		return FromSmallInt(lo + rand.Int64N(span))
	})

	// --- Instance creation ---

	// Random new → auto-seeded instance
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, _ Value) Value {
		v := vmPtr.(*VM)
		rng := rand.New(rand.NewPCG(rand.Uint64(), rand.Uint64()))
		return v.wrapRandom(rng)
	})

	// Random new: seed → reproducible instance
	c.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, _ Value, seedVal Value) Value {
		v := vmPtr.(*VM)
		seed := uint64(0)
		if seedVal.IsSmallInt() {
			seed = uint64(seedVal.SmallInt())
		} else {
			return Nil
		}
		rng := rand.New(rand.NewPCG(seed, seed))
		return v.wrapRandom(rng)
	})

	// --- Instance methods ---

	// next → float [0.0, 1.0)
	c.AddMethod0(vm.Selectors, "next", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		rng := v.unwrapRandom(recv)
		if rng == nil {
			return Nil
		}
		return FromFloat64(rng.Float64())
	})

	// nextInt: n → int [0, n)
	c.AddMethod1(vm.Selectors, "nextInt:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		rng := v.unwrapRandom(recv)
		if rng == nil {
			return Nil
		}
		n := int64(0)
		if arg.IsSmallInt() {
			n = arg.SmallInt()
		} else {
			return Nil
		}
		if n <= 0 {
			return FromSmallInt(0)
		}
		return FromSmallInt(rng.Int64N(n))
	})

	// nextBetween: lo and: hi → int in [lo, hi]
	c.AddMethod2(vm.Selectors, "nextBetween:and:", func(vmPtr interface{}, recv Value, loVal, hiVal Value) Value {
		v := vmPtr.(*VM)
		rng := v.unwrapRandom(recv)
		if rng == nil {
			return Nil
		}
		if !loVal.IsSmallInt() || !hiVal.IsSmallInt() {
			return Nil
		}
		lo := loVal.SmallInt()
		hi := hiVal.SmallInt()
		if lo > hi {
			lo, hi = hi, lo
		}
		span := hi - lo + 1
		if span <= 0 {
			return FromSmallInt(lo)
		}
		return FromSmallInt(lo + rng.Int64N(span))
	})
}

// wrapRandom stores a *rand.Rand via GoObjectWrapper.
func (vm *VM) wrapRandom(rng *rand.Rand) Value {
	v, err := vm.RegisterGoObject(rng)
	if err != nil {
		return Nil
	}
	return v
}

// unwrapRandom retrieves a *rand.Rand from a GoObject value.
func (vm *VM) unwrapRandom(v Value) *rand.Rand {
	wrapper := vm.registry.GetGoObject(v)
	if wrapper == nil {
		return nil
	}
	rng, _ := wrapper.Value.(*rand.Rand)
	return rng
}
