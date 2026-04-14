package main

import (
	"testing"
)

func TestShortHash(t *testing.T) {
	tests := []struct {
		name string
		hash [32]byte
		want string
	}{
		{
			name: "all zeros",
			hash: [32]byte{},
			want: "000000000000",
		},
		{
			name: "known bytes",
			hash: func() [32]byte {
				var h [32]byte
				h[0] = 0xde
				h[1] = 0xad
				h[2] = 0xbe
				h[3] = 0xef
				h[4] = 0xca
				h[5] = 0xfe
				return h
			}(),
			want: "deadbeefcafe",
		},
		{
			name: "all ff",
			hash: func() [32]byte {
				var h [32]byte
				for i := range h {
					h[i] = 0xff
				}
				return h
			}(),
			want: "ffffffffffff",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := shortHash(tt.hash)
			if got != tt.want {
				t.Errorf("shortHash() = %q, want %q", got, tt.want)
			}
			if len(got) != 12 {
				t.Errorf("shortHash() length = %d, want 12", len(got))
			}
		})
	}
}

func TestExtractSelector(t *testing.T) {
	tests := []struct {
		name   string
		source string
		want   string
	}{
		{
			name:   "unary selector",
			source: "method: size [^items size]",
			want:   "size",
		},
		{
			name:   "keyword selector with arg",
			source: "method: at: index put: value [items at: index put: value]",
			want:   "at: index put: value",
		},
		{
			name:   "binary selector",
			source: "method: + other [^self primPlus: other]",
			want:   "+ other",
		},
		{
			name:   "selector with newline before bracket",
			source: "method: initialize\n[self setup]",
			want:   "initialize",
		},
		{
			name:   "leading whitespace",
			source: "  method: foo [bar]",
			want:   "foo",
		},
		{
			name:   "no method keyword",
			source: "some random source text",
			want:   "(unknown)",
		},
		{
			name:   "empty string",
			source: "",
			want:   "(unknown)",
		},
		{
			name:   "method keyword with no selector before bracket",
			source: "method: [oops]",
			want:   "(unknown)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := extractSelector(tt.source)
			if got != tt.want {
				t.Errorf("extractSelector(%q) = %q, want %q", tt.source, got, tt.want)
			}
		})
	}
}

func TestNormalizeAddr(t *testing.T) {
	tests := []struct {
		name string
		addr string
		want string
	}{
		{
			name: "plain host:port",
			addr: "localhost:8080",
			want: "http://localhost:8080",
		},
		{
			name: "already http",
			addr: "http://localhost:8080",
			want: "http://localhost:8080",
		},
		{
			name: "already https",
			addr: "https://example.com:443",
			want: "https://example.com:443",
		},
		{
			name: "port only",
			addr: ":8080",
			want: "http://localhost:8080",
		},
		{
			name: "hostname without port",
			addr: "example.com",
			want: "http://example.com",
		},
		{
			name: "ip address with port",
			addr: "192.168.1.1:9200",
			want: "http://192.168.1.1:9200",
		},
		{
			name: "empty string",
			addr: "",
			want: "",
		},
		{
			name: "http without port",
			addr: "http://example.com",
			want: "http://example.com",
		},
		{
			name: "https without port",
			addr: "https://example.com",
			want: "https://example.com",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := normalizeAddr(tt.addr)
			if got != tt.want {
				t.Errorf("normalizeAddr(%q) = %q, want %q", tt.addr, got, tt.want)
			}
		})
	}
}

func TestHexToHash(t *testing.T) {
	tests := []struct {
		name    string
		hex     string
		want    [32]byte
		wantErr bool
	}{
		{
			name: "valid all zeros",
			hex:  "0000000000000000000000000000000000000000000000000000000000000000",
			want: [32]byte{},
		},
		{
			name: "valid known bytes",
			hex:  "deadbeef00000000000000000000000000000000000000000000000000000000",
			want: func() [32]byte {
				var h [32]byte
				h[0] = 0xde
				h[1] = 0xad
				h[2] = 0xbe
				h[3] = 0xef
				return h
			}(),
		},
		{
			name: "valid all ff",
			hex:  "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
			want: func() [32]byte {
				var h [32]byte
				for i := range h {
					h[i] = 0xff
				}
				return h
			}(),
		},
		{
			name:    "too short",
			hex:     "deadbeef",
			wantErr: true,
		},
		{
			name:    "too long",
			hex:     "00000000000000000000000000000000000000000000000000000000000000000000",
			wantErr: true,
		},
		{
			name:    "invalid hex chars",
			hex:     "gggggggg00000000000000000000000000000000000000000000000000000000",
			wantErr: true,
		},
		{
			name:    "empty",
			hex:     "",
			wantErr: true,
		},
		{
			name: "mixed case",
			hex:  "DeAdBeEf00000000000000000000000000000000000000000000000000000000",
			want: func() [32]byte {
				var h [32]byte
				h[0] = 0xde
				h[1] = 0xad
				h[2] = 0xbe
				h[3] = 0xef
				return h
			}(),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := hexToHash(tt.hex)
			if (err != nil) != tt.wantErr {
				t.Errorf("hexToHash(%q) error = %v, wantErr %v", tt.hex, err, tt.wantErr)
				return
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("hexToHash(%q) = %x, want %x", tt.hex, got, tt.want)
			}
		})
	}
}

func TestHexByte(t *testing.T) {
	tests := []struct {
		name    string
		hi, lo  byte
		want    byte
		wantErr bool
	}{
		{name: "00", hi: '0', lo: '0', want: 0x00},
		{name: "ff", hi: 'f', lo: 'f', want: 0xff},
		{name: "FF", hi: 'F', lo: 'F', want: 0xff},
		{name: "a5", hi: 'a', lo: '5', want: 0xa5},
		{name: "9A", hi: '9', lo: 'A', want: 0x9a},
		{name: "invalid hi", hi: 'g', lo: '0', wantErr: true},
		{name: "invalid lo", hi: '0', lo: 'z', wantErr: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := hexByte(tt.hi, tt.lo)
			if (err != nil) != tt.wantErr {
				t.Errorf("hexByte(%c, %c) error = %v, wantErr %v", tt.hi, tt.lo, err, tt.wantErr)
				return
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("hexByte(%c, %c) = %x, want %x", tt.hi, tt.lo, got, tt.want)
			}
		})
	}
}

func TestHexNibble(t *testing.T) {
	tests := []struct {
		name    string
		c       byte
		want    byte
		wantErr bool
	}{
		// Digits 0-9
		{name: "0", c: '0', want: 0},
		{name: "5", c: '5', want: 5},
		{name: "9", c: '9', want: 9},
		// Lowercase a-f
		{name: "a", c: 'a', want: 10},
		{name: "c", c: 'c', want: 12},
		{name: "f", c: 'f', want: 15},
		// Uppercase A-F
		{name: "A", c: 'A', want: 10},
		{name: "C", c: 'C', want: 12},
		{name: "F", c: 'F', want: 15},
		// Invalid
		{name: "g", c: 'g', wantErr: true},
		{name: "G", c: 'G', wantErr: true},
		{name: "space", c: ' ', wantErr: true},
		{name: "slash", c: '/', wantErr: true},
		{name: "colon", c: ':', wantErr: true},
		{name: "at", c: '@', wantErr: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := hexNibble(tt.c)
			if (err != nil) != tt.wantErr {
				t.Errorf("hexNibble(%c) error = %v, wantErr %v", tt.c, err, tt.wantErr)
				return
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("hexNibble(%c) = %d, want %d", tt.c, got, tt.want)
			}
		})
	}
}
