package vm

import (
	"encoding/json"
	"fmt"
	"math/big"

	"cuelang.org/go/cue/cuecontext"
	"github.com/fxamacker/cbor/v2"
)

// ---------------------------------------------------------------------------
// Value Serialization: CBOR-based encoding for the Maggie value space
// ---------------------------------------------------------------------------
//
// Design principles:
//   - Semantic hash is the identity key for classes (not typed hash)
//   - CBOR tags in the 27000-27099 private range for Maggie-specific types
//   - Backreference tags for circular object graphs
//   - CUE values serialized as canonical JSON text (round-trips through
//     CueContext.CompileString)
//
// Serializable types:
//   SmallInt, BigInteger, Float, Boolean, Nil, String, Symbol, Character,
//   Array, Dictionary, Object (with named ivars), CueValue
//
// Non-serializable types (raise error):
//   Process, Channel, Mutex, Semaphore, WaitGroup, CancellationContext,
//   GoObjectWrapper, Block, Cell, Context, and all I/O handles

// CBOR tags for Maggie-specific types (private range 27000-27099)
const (
	cborTagSymbol         = 27001
	cborTagObject         = 27002
	cborTagRemoteProcess  = 27003
	cborTagCueValue       = 27004
	cborTagDictionary     = 27005
	cborTagCharacter      = 27006
	cborTagBackref        = 27010
	cborTagBigIntPositive = 27011
	cborTagBigIntNegative = 27012
	cborTagSpawnBlock     = 27013
	cborTagRemoteChannel  = 27014
	cborTagException      = 27015
)

// serializedException is the CBOR representation of a Maggie exception.
type serializedException struct {
	ClassName   string `cbor:"1,keyasint"`           // e.g. "Error", "ZeroDivide"
	MessageText []byte `cbor:"2,keyasint,omitempty"` // serialized message Value
	TagValue    []byte `cbor:"3,keyasint,omitempty"` // serialized tag Value
}

// serializedObject is the CBOR representation of a Maggie object.
type serializedObject struct {
	ClassHash [32]byte `cbor:"1,keyasint"`
	ClassName string   `cbor:"2,keyasint"` // for diagnostics / class-pull
	Slots     [][]byte `cbor:"3,keyasint"` // each slot is a serialized Value
}

// serializedDictionary is the CBOR representation of a Maggie Dictionary.
type serializedDictionary struct {
	Entries []dictEntry `cbor:"1,keyasint"`
}

type dictEntry struct {
	Key   []byte `cbor:"1,keyasint"`
	Value []byte `cbor:"2,keyasint"`
}

var cborSerialEncMode cbor.EncMode

// CborSerialEncode marshals a value using the canonical CBOR encoding mode.
// Exported for use by cmd/mag wiring layer.
func CborSerialEncode(v interface{}) ([]byte, error) {
	return cborSerialEncMode.Marshal(v)
}

func init() {
	em, err := cbor.CanonicalEncOptions().EncMode()
	if err != nil {
		panic(fmt.Sprintf("serial: failed to create CBOR enc mode: %v", err))
	}
	cborSerialEncMode = em
}

// ---------------------------------------------------------------------------
// Serializer
// ---------------------------------------------------------------------------

type valueSerializer struct {
	vm   *VM
	seen map[uintptr]uint32 // object pointer → backreference index
	next uint32
}

// SerializeValue encodes a Maggie Value to CBOR bytes. Returns an error if
// the value contains non-serializable types (Process, Channel, etc.) or if
// serialization fails for any other reason.
func (vm *VM) SerializeValue(v Value) ([]byte, error) {
	s := &valueSerializer{
		vm:   vm,
		seen: make(map[uintptr]uint32),
	}
	return s.serialize(v)
}

func (s *valueSerializer) serialize(v Value) ([]byte, error) {
	switch {
	case v.IsNil():
		return cborSerialEncMode.Marshal(nil)

	case v.IsTrue():
		return cborSerialEncMode.Marshal(true)

	case v.IsFalse():
		return cborSerialEncMode.Marshal(false)

	case v.IsSmallInt():
		return cborSerialEncMode.Marshal(v.SmallInt())

	case v.IsFloat():
		return cborSerialEncMode.Marshal(v.Float64())

	case v.IsSymbol():
		return s.serializeSymbolEncoded(v)

	case v.IsObject():
		return s.serializeObject(v)

	case v.IsBlock():
		return nil, fmt.Errorf("serial: cannot serialize Block (non-serializable type)")

	case v.IsCell():
		return nil, fmt.Errorf("serial: cannot serialize Cell (non-serializable type)")

	case v.IsContext():
		return nil, fmt.Errorf("serial: cannot serialize Context (non-serializable type)")

	default:
		return nil, fmt.Errorf("serial: unknown value type %016x", uint64(v))
	}
}

// serializeSymbolEncoded handles all symbol-encoded types: strings, symbols,
// BigIntegers, Characters, Dictionaries, CUE values, and non-serializable
// marker types (Channel, Process, Mutex, etc.)
func (s *valueSerializer) serializeSymbolEncoded(v Value) ([]byte, error) {
	// Check specific types in order of frequency
	if IsStringValue(v) {
		content := s.vm.registry.GetStringContent(v)
		return cborSerialEncMode.Marshal(content)
	}

	if IsCharacterValue(v) {
		cp := GetCharacterCodePoint(v)
		return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagCharacter, Content: uint32(cp)})
	}

	if IsBigIntValue(v) {
		bi := s.vm.registry.GetBigInt(v)
		if bi == nil {
			return nil, fmt.Errorf("serial: BigInteger registry miss")
		}
		return s.serializeBigInt(bi.Value)
	}

	if IsDictionaryValue(v) {
		return s.serializeDictionary(v)
	}

	if isCueValueValue(v) {
		return s.serializeCueValue(v)
	}

	// Check for non-serializable marker types
	id := v.SymbolID()
	marker := id & markerMask
	switch marker {
	case channelMarker:
		return s.serializeChannel(v)
	case remoteChannelMarker:
		return s.serializeRemoteChannel(v)
	case processMarker:
		return nil, fmt.Errorf("serial: cannot serialize Process (non-serializable type)")
	case mutexMarker:
		return nil, fmt.Errorf("serial: cannot serialize Mutex (non-serializable type)")
	case waitGroupMarker:
		return nil, fmt.Errorf("serial: cannot serialize WaitGroup (non-serializable type)")
	case semaphoreMarker:
		return nil, fmt.Errorf("serial: cannot serialize Semaphore (non-serializable type)")
	case cancellationContextMarker:
		return nil, fmt.Errorf("serial: cannot serialize CancellationContext (non-serializable type)")
	case goObjectMarker:
		return nil, fmt.Errorf("serial: cannot serialize GoObject (non-serializable type)")
	case resultMarker:
		return nil, fmt.Errorf("serial: cannot serialize Result (non-serializable type)")
	case exceptionMarker:
		return s.serializeException(v)
	case arrayListMarker:
		return nil, fmt.Errorf("serial: cannot serialize ArrayList (non-serializable type)")
	}

	// Regular symbol (interned name)
	if id < stringIDOffset {
		name := s.vm.Symbols.Name(id)
		return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagSymbol, Content: name})
	}

	return nil, fmt.Errorf("serial: unknown symbol-encoded type (marker=%d)", marker>>24)
}

func (s *valueSerializer) serializeBigInt(n *big.Int) ([]byte, error) {
	tag := uint64(cborTagBigIntPositive)
	b := n.Bytes() // absolute value, big-endian
	if n.Sign() < 0 {
		tag = cborTagBigIntNegative
	}
	return cborSerialEncMode.Marshal(cbor.Tag{Number: tag, Content: b})
}

func (s *valueSerializer) serializeObject(v Value) ([]byte, error) {
	obj := ObjectFromValue(v)
	if obj == nil {
		return cborSerialEncMode.Marshal(nil) // nil object → CBOR null
	}

	// Check for circular reference
	ptr := objectPointer(obj)
	if idx, ok := s.seen[ptr]; ok {
		return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagBackref, Content: idx})
	}

	// Register this object before serializing slots (handles cycles)
	idx := s.next
	s.seen[ptr] = idx
	s.next++

	// Determine if this is an Array
	vt := obj.VTablePtr()
	if vt != nil && vt.Class() != nil && vt.Class().Name == "Array" {
		return s.serializeArray(obj)
	}

	// Regular object: serialize as {classHash, className, slots}
	class := vt.Class()
	if class == nil {
		return nil, fmt.Errorf("serial: object has no class")
	}

	// Compute class hash for identity
	digest := DigestClass(class)
	className := class.Name
	if class.Namespace != "" {
		className = class.Namespace + "::" + class.Name
	}

	// Serialize each slot
	numSlots := obj.NumSlots()
	slots := make([][]byte, numSlots)
	for i := 0; i < numSlots; i++ {
		slotData, err := s.serialize(obj.GetSlot(i))
		if err != nil {
			return nil, fmt.Errorf("serial: object %s slot %d: %w", className, i, err)
		}
		slots[i] = slotData
	}

	so := &serializedObject{
		ClassHash: digest.Hash,
		ClassName: className,
		Slots:     slots,
	}
	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagObject, Content: so})
}

func (s *valueSerializer) serializeArray(obj *Object) ([]byte, error) {
	n := obj.NumSlots()
	elems := make([]cbor.RawMessage, n)
	for i := 0; i < n; i++ {
		data, err := s.serialize(obj.GetSlot(i))
		if err != nil {
			return nil, fmt.Errorf("serial: array element %d: %w", i, err)
		}
		elems[i] = data
	}
	return cborSerialEncMode.Marshal(elems)
}

func (s *valueSerializer) serializeDictionary(v Value) ([]byte, error) {
	dict := s.vm.registry.GetDictionaryObject(v)
	if dict == nil {
		return nil, fmt.Errorf("serial: Dictionary registry miss")
	}

	entries := make([]dictEntry, 0, len(dict.Keys))
	for h, key := range dict.Keys {
		val := dict.Data[h]
		keyBytes, err := s.serialize(key)
		if err != nil {
			return nil, fmt.Errorf("serial: dictionary key: %w", err)
		}
		valBytes, err := s.serialize(val)
		if err != nil {
			return nil, fmt.Errorf("serial: dictionary value: %w", err)
		}
		entries = append(entries, dictEntry{Key: keyBytes, Value: valBytes})
	}

	sd := &serializedDictionary{Entries: entries}
	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagDictionary, Content: sd})
}

func (s *valueSerializer) serializeCueValue(v Value) ([]byte, error) {
	cv := s.vm.vmGetCueValue(v)
	if cv == nil {
		return nil, fmt.Errorf("serial: CueValue registry miss")
	}
	// Serialize as JSON text (canonical, round-trippable via CompileString)
	data, err := json.Marshal(cueToInterface(cv.val))
	if err != nil {
		return nil, fmt.Errorf("serial: CueValue JSON: %w", err)
	}
	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagCueValue, Content: string(data)})
}

// serializedChannel is the CBOR representation of a channel reference.
type serializedChannel struct {
	OwnerNode [32]byte `cbor:"1,keyasint"`
	ChannelID uint64   `cbor:"2,keyasint"`
	Capacity  int      `cbor:"3,keyasint"`
}

func (s *valueSerializer) serializeChannel(v Value) ([]byte, error) {
	ch := s.vm.getChannel(v)
	if ch == nil {
		return nil, fmt.Errorf("serial: Channel registry miss")
	}
	// Export the channel for remote access
	exportID := s.vm.ExportChannel(ch)

	// Get the local node ID
	var ownerNode [32]byte
	if s.vm.localIdentity != nil {
		copy(ownerNode[:], s.vm.localIdentity.pub)
	}

	sc := &serializedChannel{
		OwnerNode: ownerNode,
		ChannelID: exportID,
		Capacity:  cap(ch.ch),
	}
	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagRemoteChannel, Content: sc})
}

func (s *valueSerializer) serializeRemoteChannel(v Value) ([]byte, error) {
	ref := s.vm.getRemoteChannel(v)
	if ref == nil {
		return nil, fmt.Errorf("serial: RemoteChannel registry miss")
	}
	sc := &serializedChannel{
		OwnerNode: ref.OwnerNode,
		ChannelID: ref.ChannelID,
		Capacity:  ref.Capacity,
	}
	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagRemoteChannel, Content: sc})
}

func (s *valueSerializer) serializeException(v Value) ([]byte, error) {
	exObj := s.vm.registry.GetException(v.ExceptionID())
	if exObj == nil {
		// Unknown exception — serialize as a generic Error with no message
		se := &serializedException{ClassName: "Error"}
		return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagException, Content: se})
	}

	className := "Exception"
	if exObj.ExceptionClass != nil {
		className = exObj.ExceptionClass.Name
	}

	se := &serializedException{ClassName: className}

	// Serialize message text if present
	if exObj.MessageText != Nil {
		msgBytes, err := s.serialize(exObj.MessageText)
		if err != nil {
			return nil, fmt.Errorf("serial: exception messageText: %w", err)
		}
		se.MessageText = msgBytes
	}

	// Serialize tag if present
	if exObj.Tag != Nil {
		tagBytes, err := s.serialize(exObj.Tag)
		if err != nil {
			return nil, fmt.Errorf("serial: exception tag: %w", err)
		}
		se.TagValue = tagBytes
	}

	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagException, Content: se})
}

// objectPointer returns a stable identity for an Object (for cycle detection).
func objectPointer(obj *Object) uintptr {
	return uintptr(obj.ToValue().ObjectPtr())
}

// ---------------------------------------------------------------------------
// Deserializer
// ---------------------------------------------------------------------------

type valueDeserializer struct {
	vm      *VM
	refs    map[uint32]Value // backreference index → deserialized value
	nextRef uint32           // next backreference index to assign
}

// DeserializeValue decodes CBOR bytes back into a Maggie Value. Returns an
// error if the data references a class that is not available locally.
func (vm *VM) DeserializeValue(data []byte) (Value, error) {
	d := &valueDeserializer{
		vm:   vm,
		refs: make(map[uint32]Value),
	}
	return d.deserialize(data)
}

func (d *valueDeserializer) deserialize(data []byte) (Value, error) {
	if len(data) == 0 {
		return Nil, fmt.Errorf("serial: empty data")
	}

	// Decode to generic interface first
	var raw interface{}
	if err := cbor.Unmarshal(data, &raw); err != nil {
		return Nil, fmt.Errorf("serial: CBOR decode: %w", err)
	}

	// Check if it decoded as a tag
	if tag, ok := raw.(cbor.Tag); ok {
		return d.deserializeTag(tag, data)
	}

	return d.fromInterface(raw)
}

func (d *valueDeserializer) fromInterface(raw interface{}) (Value, error) {
	switch v := raw.(type) {
	case nil:
		return Nil, nil
	case bool:
		return FromBool(v), nil
	case uint64:
		if v <= uint64(MaxSmallInt) {
			return FromSmallInt(int64(v)), nil
		}
		bi := new(big.Int).SetUint64(v)
		return d.vm.registry.NewBigIntValue(bi), nil
	case int64:
		if v >= MinSmallInt && v <= MaxSmallInt {
			return FromSmallInt(v), nil
		}
		bi := big.NewInt(v)
		return d.vm.registry.NewBigIntValue(bi), nil
	case float32:
		return FromFloat64(float64(v)), nil
	case float64:
		return FromFloat64(v), nil
	case string:
		return d.vm.registry.NewStringValue(v), nil
	case []interface{}:
		// CBOR array → Maggie Array
		elems := make([]Value, len(v))
		for i, elem := range v {
			elemBytes, err := cborSerialEncMode.Marshal(elem)
			if err != nil {
				return Nil, fmt.Errorf("serial: re-encode array elem %d: %w", i, err)
			}
			val, err := d.deserialize(elemBytes)
			if err != nil {
				return Nil, fmt.Errorf("serial: array elem %d: %w", i, err)
			}
			elems[i] = val
		}
		return d.vm.NewArrayWithElements(elems), nil
	default:
		return Nil, fmt.Errorf("serial: unsupported CBOR type %T", raw)
	}
}

func (d *valueDeserializer) deserializeTag(tag cbor.Tag, rawData []byte) (Value, error) {
	switch tag.Number {
	case cborTagSymbol:
		name, ok := tag.Content.(string)
		if !ok {
			return Nil, fmt.Errorf("serial: Symbol tag content not string")
		}
		id := d.vm.Symbols.Intern(name)
		return FromSymbolID(id), nil

	case cborTagCharacter:
		var cp uint32
		switch v := tag.Content.(type) {
		case uint64:
			cp = uint32(v)
		case int64:
			cp = uint32(v)
		default:
			return Nil, fmt.Errorf("serial: Character tag content not integer")
		}
		return FromCharacter(rune(cp)), nil

	case cborTagBigIntPositive:
		b, ok := tag.Content.([]byte)
		if !ok {
			return Nil, fmt.Errorf("serial: BigInt tag content not bytes")
		}
		n := new(big.Int).SetBytes(b)
		return d.vm.registry.NewBigIntValue(n), nil

	case cborTagBigIntNegative:
		b, ok := tag.Content.([]byte)
		if !ok {
			return Nil, fmt.Errorf("serial: BigInt tag content not bytes")
		}
		n := new(big.Int).SetBytes(b)
		n.Neg(n)
		return d.vm.registry.NewBigIntValue(n), nil

	case cborTagBackref:
		var idx uint32
		switch v := tag.Content.(type) {
		case uint64:
			idx = uint32(v)
		case int64:
			idx = uint32(v)
		default:
			return Nil, fmt.Errorf("serial: Backref tag content not integer")
		}
		val, ok := d.refs[idx]
		if !ok {
			return Nil, fmt.Errorf("serial: backreference %d not found", idx)
		}
		return val, nil

	case cborTagObject:
		return d.deserializeObject(tag)

	case cborTagDictionary:
		return d.deserializeDictionary(tag)

	case cborTagCueValue:
		return d.deserializeCueValue(tag)

	case cborTagRemoteChannel:
		return d.deserializeChannel(tag)

	case cborTagException:
		return d.deserializeException(tag)

	default:
		// Unknown tag — try to deserialize the content as a plain value
		return d.fromInterface(tag.Content)
	}
}

func (d *valueDeserializer) deserializeObject(tag cbor.Tag) (Value, error) {
	// CBOR library decodes tag content automatically. Re-marshal to bytes
	// so we can unmarshal into our typed struct.
	raw, err := cborSerialEncMode.Marshal(tag.Content)
	if err != nil {
		return Nil, fmt.Errorf("serial: Object re-encode: %w", err)
	}

	var so serializedObject
	if err := cbor.Unmarshal(raw, &so); err != nil {
		return Nil, fmt.Errorf("serial: Object decode: %w", err)
	}

	// Look up the class by content hash, then by name
	class := d.lookupClass(so.ClassHash, so.ClassName)
	if class == nil {
		return Nil, fmt.Errorf("serial: class %q (hash %x) not found locally", so.ClassName, so.ClassHash[:8])
	}

	// Create instance
	obj := NewObject(class.VTable, class.NumSlots)
	objVal := obj.ToValue()

	// Register with VM's keepAlive — NaN-boxed pointers are invisible to the GC
	d.vm.KeepAlive(obj)

	// Register backref before filling slots (handles cycles)
	idx := d.nextRef
	d.nextRef++
	d.refs[idx] = objVal

	// Fill slots
	for i, slotData := range so.Slots {
		if i >= obj.NumSlots() {
			break
		}
		val, err := d.deserialize(slotData)
		if err != nil {
			return Nil, fmt.Errorf("serial: object %s slot %d: %w", so.ClassName, i, err)
		}
		obj.SetSlot(i, val)
	}

	return objVal, nil
}

func (d *valueDeserializer) lookupClass(hash [32]byte, name string) *Class {
	// Try by name first (faster, works for local classes)
	if name != "" {
		if cls := d.vm.Classes.Lookup(name); cls != nil {
			return cls
		}
		// Try Globals
		if gv, ok := d.vm.Globals[name]; ok {
			if cls := d.vm.GetClassFromValue(gv); cls != nil {
				return cls
			}
		}
	}
	// Try content store by hash
	if d.vm.contentStore != nil {
		if digest := d.vm.contentStore.LookupClass(hash); digest != nil {
			if cls := d.vm.Classes.Lookup(digest.Name); cls != nil {
				return cls
			}
		}
	}
	return nil
}

func (d *valueDeserializer) deserializeDictionary(tag cbor.Tag) (Value, error) {
	// Re-marshal decoded content back to bytes for typed unmarshaling
	raw, err := cborSerialEncMode.Marshal(tag.Content)
	if err != nil {
		return Nil, fmt.Errorf("serial: Dictionary re-encode: %w", err)
	}

	var sd serializedDictionary
	if err := cbor.Unmarshal(raw, &sd); err != nil {
		return Nil, fmt.Errorf("serial: Dictionary decode: %w", err)
	}

	dictVal := d.vm.NewDictionary()
	dict := d.vm.registry.GetDictionaryObject(dictVal)
	if dict == nil {
		return Nil, fmt.Errorf("serial: failed to create Dictionary")
	}

	for _, entry := range sd.Entries {
		key, err := d.deserialize(entry.Key)
		if err != nil {
			return Nil, fmt.Errorf("serial: dictionary key: %w", err)
		}
		val, err := d.deserialize(entry.Value)
		if err != nil {
			return Nil, fmt.Errorf("serial: dictionary value: %w", err)
		}
		h := hashValue(d.vm.registry, key)
		dict.Keys[h] = key
		dict.Data[h] = val
	}

	return dictVal, nil
}

func (d *valueDeserializer) deserializeChannel(tag cbor.Tag) (Value, error) {
	raw, err := cborSerialEncMode.Marshal(tag.Content)
	if err != nil {
		return Nil, fmt.Errorf("serial: Channel re-encode: %w", err)
	}

	var sc serializedChannel
	if err := cbor.Unmarshal(raw, &sc); err != nil {
		return Nil, fmt.Errorf("serial: Channel decode: %w", err)
	}

	// Check if this is a local channel (we are the owner)
	var localNode [32]byte
	if d.vm.localIdentity != nil {
		copy(localNode[:], d.vm.localIdentity.pub)
	}

	if sc.OwnerNode == localNode && localNode != ([32]byte{}) {
		// Local channel — look up by export ID
		ch := d.vm.LookupExportedChannel(sc.ChannelID)
		if ch != nil {
			// Find the NaN-boxed value for this channel in the registry
			// We need to return the existing channel Value
			return d.vm.findChannelValue(ch), nil
		}
		return Nil, fmt.Errorf("serial: local channel export %d not found", sc.ChannelID)
	}

	// Remote channel — create a proxy
	ref := &RemoteChannelRef{
		OwnerNode: sc.OwnerNode,
		ChannelID: sc.ChannelID,
		Capacity:  sc.Capacity,
	}

	// Wire up RPC callbacks if we have a RemoteChannelFactory
	if d.vm.RemoteChannelFactory != nil {
		d.vm.RemoteChannelFactory(ref)
	}

	return d.vm.registerRemoteChannel(ref), nil
}

func (d *valueDeserializer) deserializeException(tag cbor.Tag) (Value, error) {
	raw, err := cborSerialEncMode.Marshal(tag.Content)
	if err != nil {
		return Nil, fmt.Errorf("serial: Exception re-encode: %w", err)
	}

	var se serializedException
	if err := cbor.Unmarshal(raw, &se); err != nil {
		return Nil, fmt.Errorf("serial: Exception decode: %w", err)
	}

	// Look up the exception class by name; fall back to Error if not found
	exClass := d.lookupExceptionClass(se.ClassName)
	if exClass == nil {
		exClass = d.vm.ErrorClass
	}

	// Deserialize message text
	var messageText Value = Nil
	if len(se.MessageText) > 0 {
		messageText, err = d.deserialize(se.MessageText)
		if err != nil {
			return Nil, fmt.Errorf("serial: exception messageText: %w", err)
		}
	}

	// Deserialize tag value
	var tagVal Value = Nil
	if len(se.TagValue) > 0 {
		tagVal, err = d.deserialize(se.TagValue)
		if err != nil {
			return Nil, fmt.Errorf("serial: exception tag: %w", err)
		}
	}

	// Create and register the exception object
	exObj := &ExceptionObject{
		ExceptionClass: exClass,
		MessageText:    messageText,
		Tag:            tagVal,
		Resumable:      false, // remote exceptions are not resumable
	}
	id := d.vm.registry.RegisterException(exObj)
	return FromExceptionID(id), nil
}

// lookupExceptionClass finds an exception class by name, checking the standard
// exception hierarchy. Returns nil if not found.
func (d *valueDeserializer) lookupExceptionClass(name string) *Class {
	if name == "" {
		return nil
	}
	// Try Globals first (covers all registered exception classes)
	if gv, ok := d.vm.Globals[name]; ok {
		if cls := d.vm.GetClassFromValue(gv); cls != nil {
			return cls
		}
	}
	// Try ClassTable
	if cls := d.vm.Classes.Lookup(name); cls != nil {
		return cls
	}
	return nil
}

func (d *valueDeserializer) deserializeCueValue(tag cbor.Tag) (Value, error) {
	jsonText, ok := tag.Content.(string)
	if !ok {
		return Nil, fmt.Errorf("serial: CueValue tag content not string")
	}
	ctx := cuecontext.New()
	cueVal := ctx.CompileString(jsonText)
	if cueVal.Err() != nil {
		return Nil, fmt.Errorf("serial: CueValue compile: %w", cueVal.Err())
	}
	obj := &CueValueObject{val: cueVal}
	return d.vm.vmRegisterCueValue(obj), nil
}
