package main

import "testing"

func TestEntryExitCode(t *testing.T) {
	for n, want := range map[int64]int{0: 0, 1: 1, 255: 255, 256: 1, 512: 1, -1: 1} {
		if got := entryExitCode(n); got != want {
			t.Errorf("entryExitCode(%d) = %d, want %d", n, got, want)
		}
	}
}
