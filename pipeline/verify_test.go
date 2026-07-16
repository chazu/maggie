package pipeline

import (
	"strings"
	"testing"
)

// TestHashMethodSource_RoundTripsPipelineHashes is the compile → chunk-source
// → verify round trip: hashes computed by HashMethodSource from a method's
// stored Source must reproduce the content hashes the pipeline stamped at
// compile time. This is the regression test for the sync-verifier divergence
// (wrong parser entry point + missing ivar/namespace context) that caused
// every pipeline-compiled chunk to fail verification — and, via the
// hash-mismatch strike rule, banned honest peers.
func TestHashMethodSource_RoundTripsPipelineHashes(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)

	// A two-level hierarchy: the subclass method references an ivar
	// inherited from the superclass, so its hash depends on the full
	// root-first ivar chain — the exact context the old verifier lacked.
	base := `VerifyHashBase subclass: Object
  instanceVars: baseCount

  method: baseBump [ baseCount := baseCount + 1. ^baseCount ]
`
	sub := `VerifyHashSub subclass: VerifyHashBase
  instanceVars: subCount

  method: bump [ subCount := (subCount + baseCount) + 1. ^subCount ]
  classMethod: makeOne [ ^VerifyHashSub new ]
`
	if _, err := pipe.CompileSourceFile(base, "verify_hash_base.mag", ""); err != nil {
		t.Fatalf("compile base: %v", err)
	}
	if _, err := pipe.CompileSourceFile(sub, "verify_hash_sub.mag", ""); err != nil {
		t.Fatalf("compile sub: %v", err)
	}

	store := vmInst.ContentStore()
	for _, className := range []string{"VerifyHashBase", "VerifyHashSub"} {
		cls := vmInst.Classes.Lookup(className)
		if cls == nil {
			t.Fatalf("class %s not found in class table", className)
		}
		digest := store.LookupClassByName(className)
		if digest == nil {
			t.Fatalf("class digest for %s not found in content store", className)
		}

		for _, mh := range digest.MethodHashes {
			m := store.LookupMethod(mh)
			if m == nil {
				t.Fatalf("%s: method %x not in content store", className, mh[:8])
			}
			var ivars []string
			if !strings.HasPrefix(m.Source, "classMethod:") {
				ivars = cls.AllInstVarNames()
			}

			semantic, typed, err := HashMethodSource(m.Source, ivars, cls.Namespace, vmInst.Classes)
			if err != nil {
				t.Fatalf("%s>>%s: HashMethodSource: %v", className, m.Name(), err)
			}
			if semantic != mh {
				t.Errorf("%s>>%s: semantic hash diverged: pipeline %x, verifier %x",
					className, m.Name(), mh[:8], semantic[:8])
			}
			if th := m.GetTypedHash(); th != ([32]byte{}) && typed != th {
				t.Errorf("%s>>%s: typed hash diverged: pipeline %x, verifier %x",
					className, m.Name(), th[:8], typed[:8])
			}
		}
	}
}
