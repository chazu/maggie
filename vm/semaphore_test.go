package vm

import "testing"

// TestSemaphoreNonPositiveCapacitySignals guards the regression where
// `Semaphore new: 0` silently clamped to capacity 1 (so tryAcquire answered
// true on a semaphore the caller asked to have no permits) and a negative
// capacity did the same. A non-positive capacity is a programmer error.
func TestSemaphoreNonPositiveCapacitySignals(t *testing.T) {
	for _, sel := range []string{"new:", "primNew:"} {
		for _, n := range []int64{0, -5} {
			v := NewVM()
			expectSignal(t, sel+" non-positive", func() {
				v.Send(v.classValue(v.SemaphoreClass), sel, []Value{FromSmallInt(n)})
			})
		}
	}
}

// TestSemaphoreNonIntegerCapacitySignals guards the regression where
// `Semaphore new: 'x'` answered nil (a distant nil-DNU) instead of signaling.
func TestSemaphoreNonIntegerCapacitySignals(t *testing.T) {
	v := NewVM()
	expectSignal(t, "new: 'x'", func() {
		v.Send(v.classValue(v.SemaphoreClass), "new:", []Value{v.registry.NewStringValue("x")})
	})
}
