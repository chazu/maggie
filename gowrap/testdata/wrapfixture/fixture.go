// Package wrapfixture exercises gowrap signatures that once generated
// uncompilable or crashing bindings. It lives under testdata so it is only
// ever loaded explicitly by gowrap's tests.
package wrapfixture

import "os"

// Point is returned by value from MakePoint.
type Point struct{ X, Y int }

// MakePoint returns a struct value (must be registered as *Point).
func MakePoint(x, y int) Point { return Point{X: x, Y: y} }

// Divmod returns two values plus an error.
func Divmod(a, b int) (int, int, error) {
	if b == 0 {
		return 0, 0, os.ErrInvalid
	}
	return a / b, a % b, nil
}

// Sum is variadic.
func Sum(xs ...int) int {
	n := 0
	for _, x := range xs {
		n += x
	}
	return n
}

// Identity is generic.
func Identity[T any](x T) T { return x }

// Box is a generic type.
type Box[T any] struct{ V T }

// Label is a same-package named string.
type Label string

// Describe takes every checked scalar kind.
func Describe(s string, ok bool, f float64, raw []byte, l Label) string {
	return s + string(raw) + string(l)
}

// ModeBits takes an alias to another package's type (os.FileMode = fs.FileMode).
func ModeBits(m os.FileMode) uint32 { return uint32(m) }

// Opaque has only methods whose bindings are skipped.
type Opaque struct{}

// Feed takes an unconvertible channel.
func (o *Opaque) Feed(c chan int) {}

// Join is a variadic method.
func (o *Opaque) Join(parts ...string) string { return "" }
