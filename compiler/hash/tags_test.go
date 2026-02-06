package hash

import "testing"

func TestTagUniqueness(t *testing.T) {
	seen := make(map[byte]bool, len(allTags))
	for _, tag := range allTags {
		if seen[tag] {
			t.Errorf("duplicate tag: 0x%02X", tag)
		}
		seen[tag] = true
	}
}

func TestTagsInRange(t *testing.T) {
	for _, tag := range allTags {
		if tag >= 0xFE {
			t.Errorf("tag 0x%02X is in reserved range 0xFE-0xFF", tag)
		}
	}
}

func TestHashVersionNonZero(t *testing.T) {
	if HashVersion == 0 {
		t.Error("HashVersion must be non-zero")
	}
}
