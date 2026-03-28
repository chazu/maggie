package vm

import (
	"bufio"
	"net"
	"os"
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// UnixListenerObject: wraps net.Listener for Unix domain sockets
// ---------------------------------------------------------------------------

type UnixListenerObject struct {
	listener net.Listener
	path     string
	running  atomic.Bool
	closed   atomic.Bool
	mu       sync.Mutex
}

// ---------------------------------------------------------------------------
// UnixConnObject: wraps net.Conn for a Unix domain socket connection
// ---------------------------------------------------------------------------

type UnixConnObject struct {
	conn   net.Conn
	reader *bufio.Reader
	closed atomic.Bool
	mu     sync.Mutex
}

// ---------------------------------------------------------------------------
// Value encoding helpers
// ---------------------------------------------------------------------------

func unixListenerToValue(id uint32) Value {
	return FromSymbolID(id | unixListenerMarker)
}

func isUnixListenerValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == unixListenerMarker
}

func unixListenerIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^markerMask
}

func unixConnToValue(id uint32) Value {
	return FromSymbolID(id | unixConnMarker)
}

func isUnixConnValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == unixConnMarker
}

func unixConnIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^markerMask
}

// ---------------------------------------------------------------------------
// VM helper methods
// ---------------------------------------------------------------------------

func (vm *VM) vmRegisterUnixListener(l *UnixListenerObject) Value {
	id := vm.registry.RegisterUnixListener(l)
	return unixListenerToValue(id)
}

func (vm *VM) vmGetUnixListener(v Value) *UnixListenerObject {
	if !isUnixListenerValue(v) {
		return nil
	}
	return vm.registry.GetUnixListener(unixListenerIDFromValue(v))
}

func (vm *VM) vmUnregisterUnixListener(v Value) {
	if !isUnixListenerValue(v) {
		return
	}
	vm.registry.UnregisterUnixListener(unixListenerIDFromValue(v))
}

func (vm *VM) vmRegisterUnixConn(c *UnixConnObject) Value {
	id := vm.registry.RegisterUnixConn(c)
	return unixConnToValue(id)
}

func (vm *VM) vmGetUnixConn(v Value) *UnixConnObject {
	if !isUnixConnValue(v) {
		return nil
	}
	return vm.registry.GetUnixConn(unixConnIDFromValue(v))
}

func (vm *VM) vmUnregisterUnixConn(v Value) {
	if !isUnixConnValue(v) {
		return
	}
	vm.registry.UnregisterUnixConn(unixConnIDFromValue(v))
}

// ---------------------------------------------------------------------------
// Primitive Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerUnixSocketPrimitives() {
	serverClass := vm.createClass("UnixSocketServer", vm.ObjectClass)
	clientClass := vm.createClass("UnixSocketClient", vm.ObjectClass)
	connClass := vm.createClass("SocketConnection", vm.ObjectClass)

	vm.Globals["UnixSocketServer"] = vm.classValue(serverClass)
	vm.Globals["UnixSocketClient"] = vm.classValue(clientClass)
	vm.Globals["SocketConnection"] = vm.classValue(connClass)

	vm.symbolDispatch.Register(unixListenerMarker, &SymbolTypeEntry{Class: serverClass})
	vm.symbolDispatch.Register(unixConnMarker, &SymbolTypeEntry{Class: connClass})

	// -----------------------------------------------------------------------
	// UnixSocketServer class methods
	// -----------------------------------------------------------------------

	// UnixSocketServer primListenAt: path
	serverClass.AddClassMethod1(vm.Selectors, "primListenAt:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("UnixSocketServer.listenAt: requires a path string")
		}

		// Remove stale socket file if it exists
		if _, err := os.Stat(path); err == nil {
			// Check if something is actually listening
			testConn, testErr := net.Dial("unix", path)
			if testErr != nil {
				// Stale socket — remove it
				os.Remove(path)
			} else {
				testConn.Close()
				return v.newFailureResult("UnixSocketServer.listenAt: socket already in use: " + path)
			}
		}

		listener, err := net.Listen("unix", path)
		if err != nil {
			return v.newFailureResult("UnixSocketServer.listenAt: " + err.Error())
		}

		srv := &UnixListenerObject{
			listener: listener,
			path:     path,
		}
		srv.running.Store(true)
		return v.vmRegisterUnixListener(srv)
	})

	// UnixSocketServer primListenAtMode:mode:
	serverClass.AddClassMethod2(vm.Selectors, "primListenAtMode:mode:", func(vmPtr interface{}, recv Value, pathVal, modeVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("UnixSocketServer.listenAt:mode: requires a path string")
		}
		if !modeVal.IsSmallInt() {
			return v.newFailureResult("UnixSocketServer.listenAt:mode: requires an integer mode")
		}
		mode := os.FileMode(modeVal.SmallInt())

		// Remove stale socket file
		if _, err := os.Stat(path); err == nil {
			testConn, testErr := net.Dial("unix", path)
			if testErr != nil {
				os.Remove(path)
			} else {
				testConn.Close()
				return v.newFailureResult("UnixSocketServer.listenAt:mode: socket already in use: " + path)
			}
		}

		listener, err := net.Listen("unix", path)
		if err != nil {
			return v.newFailureResult("UnixSocketServer.listenAt:mode: " + err.Error())
		}

		// Set socket file permissions
		if err := os.Chmod(path, mode); err != nil {
			listener.Close()
			os.Remove(path)
			return v.newFailureResult("UnixSocketServer.listenAt:mode: chmod failed: " + err.Error())
		}

		srv := &UnixListenerObject{
			listener: listener,
			path:     path,
		}
		srv.running.Store(true)
		return v.vmRegisterUnixListener(srv)
	})

	// -----------------------------------------------------------------------
	// UnixSocketServer instance methods
	// -----------------------------------------------------------------------

	// primAccept — blocking accept, returns a SocketConnection
	serverClass.AddMethod0(vm.Selectors, "primAccept", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetUnixListener(recv)
		if srv == nil {
			return v.newFailureResult("Invalid UnixSocketServer")
		}
		if srv.closed.Load() {
			return v.newFailureResult("UnixSocketServer is closed")
		}

		conn, err := srv.listener.Accept()
		if err != nil {
			return v.newFailureResult("accept failed: " + err.Error())
		}

		connObj := &UnixConnObject{
			conn:   conn,
			reader: bufio.NewReader(conn),
		}
		return v.vmRegisterUnixConn(connObj)
	})

	// primAcceptToChannel: ch — spawn goroutine that sends new connections to channel
	serverClass.AddMethod1(vm.Selectors, "primAcceptToChannel:", func(vmPtr interface{}, recv Value, chVal Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetUnixListener(recv)
		if srv == nil {
			return v.newFailureResult("Invalid UnixSocketServer")
		}
		ch := v.vmGetChannel(chVal)
		if ch == nil {
			return v.newFailureResult("acceptToChannel: requires a Channel")
		}
		if srv.closed.Load() {
			return v.newFailureResult("UnixSocketServer is closed")
		}

		go func() {
			for {
				conn, err := srv.listener.Accept()
				if err != nil {
					// Listener was closed
					return
				}
				connObj := &UnixConnObject{
					conn:   conn,
					reader: bufio.NewReader(conn),
				}
				connVal := v.vmRegisterUnixConn(connObj)
				ch.ch <- connVal
			}
		}()

		return recv
	})

	// primClose — stop accepting, remove socket file
	serverClass.AddMethod0(vm.Selectors, "primClose", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetUnixListener(recv)
		if srv == nil {
			return recv
		}

		srv.mu.Lock()
		defer srv.mu.Unlock()

		if !srv.closed.Load() {
			srv.listener.Close()
			srv.closed.Store(true)
			srv.running.Store(false)
			// Clean up socket file
			os.Remove(srv.path)
		}

		v.vmUnregisterUnixListener(recv)
		return recv
	})

	// primPath — returns the socket file path
	serverClass.AddMethod0(vm.Selectors, "primPath", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetUnixListener(recv)
		if srv == nil {
			return Nil
		}
		return v.registry.NewStringValue(srv.path)
	})

	// primIsRunning
	serverClass.AddMethod0(vm.Selectors, "primIsRunning", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetUnixListener(recv)
		if srv == nil {
			return False
		}
		if srv.running.Load() {
			return True
		}
		return False
	})

	// primIsClosed
	serverClass.AddMethod0(vm.Selectors, "primIsClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetUnixListener(recv)
		if srv == nil {
			return True
		}
		if srv.closed.Load() {
			return True
		}
		return False
	})

	// -----------------------------------------------------------------------
	// UnixSocketClient class methods
	// -----------------------------------------------------------------------

	// UnixSocketClient primConnectTo: path
	clientClass.AddClassMethod1(vm.Selectors, "primConnectTo:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("UnixSocketClient.connectTo: requires a path string")
		}

		conn, err := net.Dial("unix", path)
		if err != nil {
			return v.newFailureResult("UnixSocketClient.connectTo: " + err.Error())
		}

		connObj := &UnixConnObject{
			conn:   conn,
			reader: bufio.NewReader(conn),
		}
		return v.vmRegisterUnixConn(connObj)
	})

	// -----------------------------------------------------------------------
	// SocketConnection instance methods
	// -----------------------------------------------------------------------

	// primSend: data — write string data to connection
	connClass.AddMethod1(vm.Selectors, "primSend:", func(vmPtr interface{}, recv Value, dataVal Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return v.newFailureResult("Invalid SocketConnection")
		}
		if c.closed.Load() {
			return v.newFailureResult("SocketConnection is closed")
		}

		data := v.valueToString(dataVal)
		_, err := c.conn.Write([]byte(data))
		if err != nil {
			return v.newFailureResult("send: " + err.Error())
		}
		return recv
	})

	// primSendLine: data — write string data followed by newline
	connClass.AddMethod1(vm.Selectors, "primSendLine:", func(vmPtr interface{}, recv Value, dataVal Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return v.newFailureResult("Invalid SocketConnection")
		}
		if c.closed.Load() {
			return v.newFailureResult("SocketConnection is closed")
		}

		data := v.valueToString(dataVal)
		_, err := c.conn.Write([]byte(data + "\n"))
		if err != nil {
			return v.newFailureResult("sendLine: " + err.Error())
		}
		return recv
	})

	// primReceive — read up to 4096 bytes
	connClass.AddMethod0(vm.Selectors, "primReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return v.newFailureResult("Invalid SocketConnection")
		}
		if c.closed.Load() {
			return v.newFailureResult("SocketConnection is closed")
		}

		buf := make([]byte, 4096)
		n, err := c.conn.Read(buf)
		if err != nil {
			return v.newFailureResult("receive: " + err.Error())
		}
		return v.registry.NewStringValue(string(buf[:n]))
	})

	// primReceiveMax: maxBytes — read up to maxBytes
	connClass.AddMethod1(vm.Selectors, "primReceiveMax:", func(vmPtr interface{}, recv Value, maxVal Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return v.newFailureResult("Invalid SocketConnection")
		}
		if c.closed.Load() {
			return v.newFailureResult("SocketConnection is closed")
		}
		if !maxVal.IsSmallInt() {
			return v.newFailureResult("receive: requires an integer max bytes")
		}
		maxBytes := int(maxVal.SmallInt())
		if maxBytes <= 0 {
			maxBytes = 4096
		}

		buf := make([]byte, maxBytes)
		n, err := c.conn.Read(buf)
		if err != nil {
			return v.newFailureResult("receive: " + err.Error())
		}
		return v.registry.NewStringValue(string(buf[:n]))
	})

	// primReceiveLine — read one newline-delimited line (for JSON-RPC)
	connClass.AddMethod0(vm.Selectors, "primReceiveLine", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return v.newFailureResult("Invalid SocketConnection")
		}
		if c.closed.Load() {
			return v.newFailureResult("SocketConnection is closed")
		}

		c.mu.Lock()
		line, err := c.reader.ReadString('\n')
		c.mu.Unlock()

		if err != nil {
			if len(line) > 0 {
				// Partial read before error — return what we got
				return v.registry.NewStringValue(line)
			}
			return v.newFailureResult("receiveLine: " + err.Error())
		}
		// Strip trailing newline
		if len(line) > 0 && line[len(line)-1] == '\n' {
			line = line[:len(line)-1]
		}
		return v.registry.NewStringValue(line)
	})

	// primClose
	connClass.AddMethod0(vm.Selectors, "primClose", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return recv
		}

		c.mu.Lock()
		defer c.mu.Unlock()

		if !c.closed.Load() {
			c.conn.Close()
			c.closed.Store(true)
		}

		v.vmUnregisterUnixConn(recv)
		return recv
	})

	// primIsClosed
	connClass.AddMethod0(vm.Selectors, "primIsClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		c := v.vmGetUnixConn(recv)
		if c == nil {
			return True
		}
		if c.closed.Load() {
			return True
		}
		return False
	})
}

// vmGetChannel retrieves a ChannelObject from a channel Value.
// This is a convenience method for use by unix socket primitives.
func (vm *VM) vmGetChannel(v Value) *ChannelObject {
	return vm.registry.GetChannel(v)
}
