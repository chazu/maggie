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

// A method with a docstring must round-trip through HashMethodSource: its
// Source (which starts at "method:" and carries no docstring) is what sync
// ships and verifiers re-hash. Previously the docstring was folded into the
// semantic hash, so every documented method failed verification and each
// failure recorded a hash-mismatch strike against an honest peer.
func TestHashMethodSource_DocumentedMethodRoundTrips(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)

	src := `VerifyDocHash subclass: Object
  instanceVars: w

  """Answers two."""
  method: w2 [ ^2 ]

  """Class-side doc."""
  classMethod: make [ ^self new ]
`
	if _, err := pipe.CompileSourceFile(src, "verify_doc_hash.mag", ""); err != nil {
		t.Fatalf("compile: %v", err)
	}

	cls := vmInst.Classes.Lookup("VerifyDocHash")
	if cls == nil {
		t.Fatal("VerifyDocHash not found")
	}
	store := vmInst.ContentStore()
	digest := store.LookupClassByName("VerifyDocHash")
	if digest == nil {
		t.Fatal("digest for VerifyDocHash not in store")
	}
	if len(digest.MethodHashes) != 2 {
		t.Fatalf("expected 2 method hashes, got %d", len(digest.MethodHashes))
	}
	for _, mh := range digest.MethodHashes {
		m := store.LookupMethod(mh)
		if m == nil {
			t.Fatalf("method %x not in store", mh[:8])
		}
		if m.DocString() == "" {
			t.Fatalf("%s: expected a docstring on the compiled method", m.Name())
		}
		if strings.Contains(m.Source, `"""`) {
			t.Fatalf("%s: Source unexpectedly carries the docstring: %q", m.Name(), m.Source)
		}
		var ivars []string
		if !strings.HasPrefix(m.Source, "classMethod:") {
			ivars = cls.AllInstVarNames()
		}
		semantic, typed, err := HashMethodSource(m.Source, ivars, cls.Namespace, vmInst.Classes)
		if err != nil {
			t.Fatalf("%s: HashMethodSource: %v", m.Name(), err)
		}
		if semantic != mh {
			t.Errorf("%s: semantic hash diverged: pipeline %x, verifier %x", m.Name(), mh[:8], semantic[:8])
		}
		if th := m.GetTypedHash(); th != ([32]byte{}) && typed != th {
			t.Errorf("%s: typed hash diverged: pipeline %x, verifier %x", m.Name(), th[:8], typed[:8])
		}
	}
}
