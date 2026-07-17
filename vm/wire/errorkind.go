package wire

// ErrorKind values carried in DeliverMessageResponse.ErrorKind (and echoed
// into Future error strings on the sending side). These exact strings are
// the wire contract; the server handlers and any client-side interpretation
// must share this single set — previously they were ~10 free-form literals
// duplicated between a comment and the handlers (SD-14).
const (
	ErrKindInternal         = "internalError"
	ErrKindDeserialization  = "deserializationError"
	ErrKindSignatureInvalid = "signatureInvalid"
	ErrKindReplayRejected   = "replayRejected"
	ErrKindPermissionDenied = "permissionDenied"
	ErrKindProcessNotFound  = "processNotFound"
	ErrKindMailboxFull      = "mailboxFull"
)
