package vm

import (
	"errors"
	"reflect"
	"testing"
	"time"
)

// TestChannelCloseVsSelectSameChannelNoDeadlock guards the regression where a
// select with two send cases on one channel deadlocked against Close(): the
// select registered case 1 as a parked sender, then blocked on co.mu to
// register case 2, while Close() held co.mu waiting in senders.Wait() for
// case 1 to vacate.
func TestChannelCloseVsSelectSameChannelNoDeadlock(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	for i := 0; i < 3000; i++ {
		co := createChannel(0)
		cases := []SelectCase{
			{Channel: co, Dir: reflect.SelectSend, Value: FromSmallInt(1), Handler: Nil},
			{Channel: co, Dir: reflect.SelectSend, Value: FromSmallInt(2), Handler: Nil},
		}
		selDone := make(chan struct{})
		go func() {
			defer close(selDone)
			v.primitiveSelectLocal(cases, Nil)
		}()
		closeDone := make(chan struct{})
		go func() {
			defer close(closeDone)
			co.Close()
		}()
		timeout := time.After(5 * time.Second)
		for _, ch := range []chan struct{}{selDone, closeDone} {
			select {
			case <-ch:
			case <-timeout:
				t.Fatalf("iteration %d: select/Close deadlocked", i)
			}
		}
	}
}

// TestExitReasonConstructorsUseNil guards the regression where ExitError /
// ExitException left Result (and ExceptionValue) as the zero Value, which
// decodes as the Float 0.0 rather than nil.
func TestExitReasonConstructorsUseNil(t *testing.T) {
	err := errors.New("boom")
	for name, r := range map[string]ExitReason{
		"ExitError":     ExitError(err),
		"ExitException": ExitException(err, Nil),
		"ExitNormal":    ExitNormal(Nil),
		"ExitSignal":    ExitSignal("kill", Nil),
	} {
		if r.Result != Nil {
			t.Errorf("%s: Result = %v, want Nil", name, r.Result)
		}
		if r.ExceptionValue != Nil {
			t.Errorf("%s: ExceptionValue = %v, want Nil", name, r.ExceptionValue)
		}
	}
}
