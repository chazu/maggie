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
	resultMarker                uint32 = 4 << 24
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

	// Distribution protocol (reserved for Phase 6)
	chunkMarker                 uint32 = 42 << 24
	remoteRefMarker             uint32 = 43 << 24
	promiseMarker               uint32 = 44 << 24
)

// markerMask extracts the marker byte from a symbol ID.
const markerMask uint32 = 0xFF << 24
