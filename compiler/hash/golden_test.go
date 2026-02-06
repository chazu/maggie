package hash

import (
	"crypto/sha256"
	"encoding/hex"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
)

// TestGoldenFiles verifies that known methods produce expected hashes.
// If the golden files don't exist, they are created (first run).
// This prevents accidental format drift.
func TestGoldenFiles(t *testing.T) {
	cases := []struct {
		name     string
		src      string
		instVars map[string]int
	}{
		{
			name: "simple_unary",
			src: `Foo subclass: Object
  method: answer [ ^42 ]
`,
		},
		{
			name: "binary_with_params",
			src: `Foo subclass: Object
  method: add: x to: y [ ^x + y ]
`,
		},
		{
			name: "block_with_capture",
			src: `Foo subclass: Object
  method: test: x [ ^[:y | x + y] ]
`,
		},
		{
			name: "instance_var_access",
			src: `Foo subclass: Object
  instanceVars: name
  method: name [ ^name ]
`,
			instVars: map[string]int{"name": 0},
		},
		{
			name: "keyword_message",
			src: `Foo subclass: Object
  method: test [ ^Array new: 3 ]
`,
		},
	}

	goldenDir := filepath.Join("testdata")
	if err := os.MkdirAll(goldenDir, 0o755); err != nil {
		t.Fatalf("create testdata dir: %v", err)
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			sf, err := compiler.ParseSourceFileFromString(tc.src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			md := sf.Classes[0].Methods[0]

			hm := NormalizeMethod(md, tc.instVars, nil)
			data := Serialize(hm)
			h := sha256.Sum256(data)

			serializedHex := hex.EncodeToString(data)
			hashHex := hex.EncodeToString(h[:])

			goldenPath := filepath.Join(goldenDir, tc.name+".golden")
			expected, err := os.ReadFile(goldenPath)
			if err != nil {
				// First run: create golden file
				content := serializedHex + "\n" + hashHex + "\n"
				if writeErr := os.WriteFile(goldenPath, []byte(content), 0o644); writeErr != nil {
					t.Fatalf("write golden file: %v", writeErr)
				}
				t.Logf("created golden file: %s", goldenPath)
				return
			}

			lines := strings.Split(strings.TrimSpace(string(expected)), "\n")
			if len(lines) != 2 {
				t.Fatalf("golden file %s: expected 2 lines, got %d", goldenPath, len(lines))
			}

			if serializedHex != lines[0] {
				t.Errorf("serialized bytes mismatch:\n  got:  %s\n  want: %s", serializedHex, lines[0])
			}
			if hashHex != lines[1] {
				t.Errorf("hash mismatch:\n  got:  %s\n  want: %s", hashHex, lines[1])
			}
		})
	}
}
