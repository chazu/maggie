package vm

import (
	"reflect"
	"regexp"
)

// ---------------------------------------------------------------------------
// Regex Primitives — wraps Go regexp.Regexp for pattern matching
// ---------------------------------------------------------------------------

// RegexObject wraps a compiled Go *regexp.Regexp for use in Maggie.
type RegexObject struct {
	re      *regexp.Regexp
	pattern string
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerRegexPrimitives() {
	reType := reflect.TypeOf((*RegexObject)(nil))
	regexClass := vm.RegisterGoType("Regex", reType)

	vm.registerRegexClassMethods(regexClass)
	vm.registerRegexInstanceMethods(regexClass)
	vm.registerStringRegexMethods()
}

// ---------------------------------------------------------------------------
// Regex class methods
// ---------------------------------------------------------------------------

func (vm *VM) registerRegexClassMethods(regexClass *Class) {
	// Regex compile: pattern — compile a regex pattern, returns Success(Regex) or Failure
	regexClass.AddClassMethod1(vm.Selectors, "compile:", func(vmPtr interface{}, recv Value, patternVal Value) Value {
		v := vmPtr.(*VM)
		pattern := v.valueToString(patternVal)
		if pattern == "" {
			return v.newFailureResult("Regex compile: requires a non-empty pattern string")
		}

		re, err := regexp.Compile(pattern)
		if err != nil {
			return v.newFailureResult("Regex compile error: " + err.Error())
		}

		obj := &RegexObject{re: re, pattern: pattern}
		val, regErr := v.RegisterGoObject(obj)
		if regErr != nil {
			return v.newFailureResult("Cannot register Regex: " + regErr.Error())
		}
		return v.newSuccessResult(val)
	})
}

// ---------------------------------------------------------------------------
// Regex instance methods
// ---------------------------------------------------------------------------

func (vm *VM) registerRegexInstanceMethods(regexClass *Class) {
	// Helper to extract RegexObject from receiver
	getRegex := func(v *VM, recv Value) *RegexObject {
		goVal, ok := v.GetGoObject(recv)
		if !ok {
			return nil
		}
		ro, ok := goVal.(*RegexObject)
		if !ok {
			return nil
		}
		return ro
	}

	// matches: string — test if string matches the pattern
	regexClass.AddMethod1(vm.Selectors, "matches:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return False
		}
		s := v.valueToString(strVal)
		if ro.re.MatchString(s) {
			return True
		}
		return False
	})

	// findIn: string — find first match, returns string or nil
	regexClass.AddMethod1(vm.Selectors, "findIn:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return Nil
		}
		s := v.valueToString(strVal)
		match := ro.re.FindString(s)
		if match == "" && !ro.re.MatchString(s) {
			return Nil
		}
		return v.registry.NewStringValue(match)
	})

	// findAllIn: string — find all matches, returns Array of strings
	regexClass.AddMethod1(vm.Selectors, "findAllIn:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return v.NewArrayWithElements(nil)
		}
		s := v.valueToString(strVal)
		matches := ro.re.FindAllString(s, -1)
		if matches == nil {
			return v.NewArrayWithElements(nil)
		}
		values := make([]Value, len(matches))
		for i, m := range matches {
			values[i] = v.registry.NewStringValue(m)
		}
		return v.NewArrayWithElements(values)
	})

	// replaceIn:with: string replacement — replace all matches
	regexClass.AddMethod2(vm.Selectors, "replaceIn:with:", func(vmPtr interface{}, recv Value, strVal Value, replVal Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return strVal
		}
		s := v.valueToString(strVal)
		repl := v.valueToString(replVal)
		result := ro.re.ReplaceAllString(s, repl)
		return v.registry.NewStringValue(result)
	})

	// split: string — split string by pattern, returns Array of strings
	regexClass.AddMethod1(vm.Selectors, "split:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return v.NewArrayWithElements(nil)
		}
		s := v.valueToString(strVal)
		parts := ro.re.Split(s, -1)
		values := make([]Value, len(parts))
		for i, p := range parts {
			values[i] = v.registry.NewStringValue(p)
		}
		return v.NewArrayWithElements(values)
	})

	// pattern — return the original pattern string
	regexClass.AddMethod0(vm.Selectors, "pattern", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return Nil
		}
		return v.registry.NewStringValue(ro.pattern)
	})

	// printString — human-readable representation
	regexClass.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ro := getRegex(v, recv)
		if ro == nil {
			return v.registry.NewStringValue("a Regex")
		}
		return v.registry.NewStringValue("Regex(/" + ro.pattern + "/)")
	})
}

// ReRegisterRegexPrimitives forces re-registration of Regex primitives.
// Call this after loading an image to ensure primitives override any compiled
// method stubs from <primitive> declarations in Regex.mag.
func (vm *VM) ReRegisterRegexPrimitives() {
	regexClass := vm.Classes.Lookup("Regex")
	if regexClass == nil {
		return
	}

	// Preserve docstrings from existing methods (may come from image-loaded stubs).
	instDocs := collectDocStrings(regexClass.VTable)
	classDocs := collectDocStrings(regexClass.ClassVTable)

	vm.registerRegexClassMethods(regexClass)
	vm.registerRegexInstanceMethods(regexClass)
	vm.registerStringRegexMethods()

	// Restore docstrings onto the newly-registered primitive methods.
	applyDocStrings(regexClass.VTable, instDocs)
	applyDocStrings(regexClass.ClassVTable, classDocs)
}

// collectDocStrings gathers docstrings from all methods in a VTable, keyed by selector ID.
func collectDocStrings(vt *VTable) map[int]string {
	if vt == nil {
		return nil
	}
	docs := make(map[int]string)
	for selID, method := range vt.LocalMethods() {
		if ds, ok := method.(DocStringable); ok {
			if doc := ds.DocString(); doc != "" {
				docs[selID] = doc
			}
		}
	}
	return docs
}

// applyDocStrings restores docstrings onto methods in a VTable.
func applyDocStrings(vt *VTable, docs map[int]string) {
	if vt == nil || len(docs) == 0 {
		return
	}
	for selID, doc := range docs {
		method := vt.Lookup(selID)
		if method == nil {
			continue
		}
		if ds, ok := method.(DocStringable); ok {
			ds.SetDocString(doc)
		}
	}
}

// ---------------------------------------------------------------------------
// String convenience methods for regex
// ---------------------------------------------------------------------------

func (vm *VM) registerStringRegexMethods() {
	c := vm.StringClass

	// matchesRegex: pattern — test if string matches a regex pattern string
	c.AddMethod1(vm.Selectors, "matchesRegex:", func(vmPtr interface{}, recv Value, patternVal Value) Value {
		v := vmPtr.(*VM)
		s := v.valueToString(recv)
		pattern := v.valueToString(patternVal)
		if pattern == "" {
			return False
		}
		matched, err := regexp.MatchString(pattern, s)
		if err != nil {
			return False
		}
		if matched {
			return True
		}
		return False
	})

	// splitRegex: pattern — split string by regex pattern
	c.AddMethod1(vm.Selectors, "splitRegex:", func(vmPtr interface{}, recv Value, patternVal Value) Value {
		v := vmPtr.(*VM)
		s := v.valueToString(recv)
		pattern := v.valueToString(patternVal)
		if pattern == "" {
			return v.NewArrayWithElements([]Value{recv})
		}
		re, err := regexp.Compile(pattern)
		if err != nil {
			return v.NewArrayWithElements([]Value{recv})
		}
		parts := re.Split(s, -1)
		values := make([]Value, len(parts))
		for i, p := range parts {
			values[i] = v.registry.NewStringValue(p)
		}
		return v.NewArrayWithElements(values)
	})
}
