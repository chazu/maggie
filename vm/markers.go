package vm

// ---------------------------------------------------------------------------
// Centralized NaN-boxing marker allocation table
// ---------------------------------------------------------------------------
//
// Every symbol-encoded (non-object, non-float) value type in the VM uses a
// unique marker byte stored in bits 24-31 of the symbol ID. This file is the
// single source of truth for all marker allocations.
//
// To add a new marker:
//   1. Pick the next available value from the table below.
//   2. Define the constant here (e.g. myTypeMarker).
//   3. Use the constant in the relevant file and register it in registerSymbolDispatch().
//
// IMPORTANT: Once assigned, marker values must NEVER change — they are part of
// the image format and wire protocol.

const (
	// Concurrency primitives
	channelMarker               uint32 = 1 << 24
	processMarker               uint32 = 2 << 24
	grpcClientMarker            uint32 = 7 << 24
	exceptionMarker             uint32 = 8 << 24
	grpcStreamMarker            uint32 = 9 << 24
	weakRefMarker               uint32 = 16 << 24
	mutexMarker                 uint32 = 32 << 24
	waitGroupMarker             uint32 = 33 << 24
	semaphoreMarker             uint32 = 34 << 24
	cancellationContextMarker   uint32 = 35 << 24
	classValueMarker            uint32 = 36 << 24
	characterMarker             uint32 = 37 << 24
	httpServerMarker            uint32 = 38 << 24
	httpRequestMarker           uint32 = 39 << 24
	httpResponseMarker          uint32 = 40 << 24
	goObjectMarker              uint32 = 41 << 24

	// OS/exec
	externalProcessMarker       uint32 = 45 << 24

	// Unix domain sockets
	unixListenerMarker          uint32 = 46 << 24
	unixConnMarker              uint32 = 47 << 24

	// JSON
	jsonReaderMarker            uint32 = 48 << 24
	jsonWriterMarker            uint32 = 49 << 24

	// gRPC primitives (exported for vm/contrib/grpc)
	GrpcClientMarker            = grpcClientMarker
	GrpcStreamMarker            = grpcStreamMarker

	// CUE evaluation primitives (exported for vm/contrib/cue)
	CueContextMarker            uint32 = 50 << 24
	CueValueMarker              uint32 = 51 << 24

	// HTTP client
	httpClientMarker            uint32 = 52 << 24

	// TupleSpace (exported for vm/contrib/cue)
	TupleSpaceMarker            uint32 = 53 << 24

	// ConstraintStore (exported for vm/contrib/cue)
	ConstraintStoreMarker       uint32 = 54 << 24

	// Distributed channels
	remoteChannelMarker         uint32 = 55 << 24

	// Growable array (ArrayList)
	arrayListMarker             uint32 = 56 << 24

	// SSE (Server-Sent Events) connection
	sseConnectionMarker         uint32 = 57 << 24

	// Cli (cobra-backed CLI command wrapper)
	cliCommandMarker            uint32 = 58 << 24

	// Distribution protocol (reserved for Phase 6)
	chunkMarker                 uint32 = 42 << 24
	remoteRefMarker             uint32 = 43 << 24
	promiseMarker               uint32 = 44 << 24
)

// markerMask extracts the marker byte from a symbol ID.
const markerMask uint32 = 0xFF << 24

// MarkerMask is the exported version for contrib packages.
const MarkerMask = markerMask

func markedToValue(marker uint32, id uint32) Value  { return FromSymbolID(id | marker) }
func markedIDFromValue(v Value) uint32               { return v.SymbolID() & ^markerMask }
func isMarkedValue(marker uint32, v Value) bool {
	return v.IsSymbolEncoded() && (v.SymbolID()&markerMask) == marker
}

// MarkedToValue is the exported version for contrib packages.
func MarkedToValue(marker uint32, id uint32) Value { return markedToValue(marker, id) }

// MarkedIDFromValue is the exported version for contrib packages.
func MarkedIDFromValue(v Value) uint32 { return markedIDFromValue(v) }

// IsMarkedValue is the exported version for contrib packages.
func IsMarkedValue(marker uint32, v Value) bool { return isMarkedValue(marker, v) }
