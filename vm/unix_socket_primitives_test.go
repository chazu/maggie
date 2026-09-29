package vm

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

// tempSockPath returns a short socket path under /tmp to stay within
// macOS's 104-byte Unix socket path limit.
var sockCounter atomic.Int32

func tempSockPath(t *testing.T) string {
	t.Helper()
	n := sockCounter.Add(1)
	path := fmt.Sprintf("/tmp/mag-test-%d-%d.sock", os.Getpid(), n)
	t.Cleanup(func() { os.Remove(path) })
	return path
}

func TestUnixSocketServerListenAndClose(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)

	// listenAt:
	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	result := assertSuccess(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt:")

	// Verify socket file exists
	if _, err := os.Stat(sockPath); os.IsNotExist(err) {
		t.Fatal("socket file should exist after listenAt:")
	}

	// isRunning
	running := vm.Send(result, "primIsRunning", nil)
	if running != True {
		t.Fatal("server should be running after listenAt:")
	}

	// path
	gotPath := vm.Send(result, "primPath", nil)
	if vm.valueToString(gotPath) != sockPath {
		t.Fatalf("path should return %q, got %q", sockPath, vm.valueToString(gotPath))
	}

	// close
	vm.Send(result, "primClose", nil)

	// Verify socket file removed
	if _, err := os.Stat(sockPath); !os.IsNotExist(err) {
		t.Fatal("socket file should be removed after close")
	}

	// isClosed
	closed := vm.Send(result, "primIsClosed", nil)
	if closed != True {
		t.Fatal("server should be closed after close")
	}
}

func TestUnixSocketClientConnectAndSendReceive(t *testing.T) {
	sockPath := tempSockPath(t)

	// Start a Go-side server for testing
	listener, err := net.Listen("unix", sockPath)
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()

	// Server goroutine: echo back what it receives
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		buf := make([]byte, 4096)
		n, err := conn.Read(buf)
		if err != nil {
			return
		}
		conn.Write(buf[:n])
	}()

	vm := NewVM()

	// connectTo:
	clientClass := vm.globals["UnixSocketClient"]
	pathVal := vm.registry.NewStringValue(sockPath)
	connVal := assertSuccess(t, vm, vm.Send(clientClass, "primConnectTo:", []Value{pathVal}), "primConnectTo:")

	// send:
	if sent := assertSuccess(t, vm, vm.Send(connVal, "primSend:", []Value{vm.registry.NewStringValue("hello")}), "primSend:"); sent != connVal {
		t.Fatalf("send: should answer Success wrapping the connection")
	}

	// receive
	recvResult := assertSuccess(t, vm, vm.Send(connVal, "primReceive", nil), "primReceive")
	content := vm.valueToString(recvResult)
	if content != "hello" {
		t.Fatalf("expected 'hello', got %q", content)
	}

	// close
	vm.Send(connVal, "primClose", nil)

	// isClosed
	closed := vm.Send(connVal, "primIsClosed", nil)
	if closed != True {
		t.Fatal("connection should be closed after close")
	}

	wg.Wait()
}

func TestUnixSocketLineProtocol(t *testing.T) {
	sockPath := tempSockPath(t)

	// Start Go-side server that sends line-delimited messages
	listener, err := net.Listen("unix", sockPath)
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		reader := bufio.NewReader(conn)
		line, _ := reader.ReadString('\n')
		conn.Write([]byte("echo:" + line))
	}()

	vm := NewVM()

	clientClass := vm.globals["UnixSocketClient"]
	pathVal := vm.registry.NewStringValue(sockPath)
	connVal := assertSuccess(t, vm, vm.Send(clientClass, "primConnectTo:", []Value{pathVal}), "primConnectTo:")

	// sendLine:
	assertSuccess(t, vm, vm.Send(connVal, "primSendLine:", []Value{vm.registry.NewStringValue(`{"method":"test"}`)}), "primSendLine:")

	// receiveLine
	lineResult := assertSuccess(t, vm, vm.Send(connVal, "primReceiveLine", nil), "primReceiveLine")
	lineStr := vm.valueToString(lineResult)

	expected := `echo:{"method":"test"}`
	if lineStr != expected {
		t.Fatalf("expected %q, got %q", expected, lineStr)
	}

	vm.Send(connVal, "primClose", nil)
	wg.Wait()
}

func TestUnixSocketServerAccept(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)

	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	serverVal := assertSuccess(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt:")

	// Connect a Go client
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		time.Sleep(50 * time.Millisecond)
		conn, err := net.Dial("unix", sockPath)
		if err != nil {
			t.Errorf("client connect failed: %v", err)
			return
		}
		conn.Write([]byte("from-client\n"))
		conn.Close()
	}()

	// Server accept
	connVal := assertSuccess(t, vm, vm.Send(serverVal, "primAccept", nil), "primAccept")

	lineResult := assertSuccess(t, vm, vm.Send(connVal, "primReceiveLine", nil), "primReceiveLine")
	lineStr := vm.valueToString(lineResult)
	if lineStr != "from-client" {
		t.Fatalf("expected 'from-client', got %q", lineStr)
	}

	vm.Send(connVal, "primClose", nil)
	vm.Send(serverVal, "primClose", nil)
	wg.Wait()
}

func TestUnixSocketConcurrentConnections(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)

	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	serverVal := assertSuccess(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt:")

	const numClients = 5
	var wg sync.WaitGroup

	for i := 0; i < numClients; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			time.Sleep(50 * time.Millisecond)
			conn, err := net.Dial("unix", sockPath)
			if err != nil {
				return
			}
			defer conn.Close()
			conn.Write([]byte("ping\n"))
			reader := bufio.NewReader(conn)
			reader.ReadString('\n')
		}()
	}

	for i := 0; i < numClients; i++ {
		connVal := assertSuccess(t, vm, vm.Send(serverVal, "primAccept", nil), "primAccept")
		lineResult := assertSuccess(t, vm, vm.Send(connVal, "primReceiveLine", nil), "primReceiveLine")
		lineStr := vm.valueToString(lineResult)
		if lineStr != "ping" {
			t.Errorf("expected 'ping', got %q", lineStr)
		}
		vm.Send(connVal, "primSendLine:", []Value{vm.registry.NewStringValue("pong")})
		vm.Send(connVal, "primClose", nil)
	}

	vm.Send(serverVal, "primClose", nil)
	wg.Wait()
}

func TestUnixSocketStaleSocketCleanup(t *testing.T) {
	sockPath := tempSockPath(t)

	// Create a stale socket file by writing a regular file at the path.
	// On macOS, net.Listen("unix") cleanup on Close removes the socket,
	// so we simulate a stale socket as a plain file blocking the path.
	if err := os.WriteFile(sockPath, []byte{}, 0o600); err != nil {
		t.Fatal(err)
	}

	if _, err := os.Stat(sockPath); os.IsNotExist(err) {
		t.Fatal("stale socket file should exist")
	}

	vm := NewVM()

	// listenAt: should detect the stale file (can't connect to it) and remove it
	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	result := assertSuccess(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt:")

	vm.Send(result, "primClose", nil)
}

func TestUnixSocketServerListenAtMode(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)

	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	modeVal := FromSmallInt(0o660)
	result := assertSuccess(t, vm, vm.Send(serverClass, "primListenAtMode:mode:", []Value{pathVal, modeVal}), "primListenAtMode:mode:")

	info, err := os.Stat(sockPath)
	if err != nil {
		t.Fatal(err)
	}
	perm := info.Mode().Perm()
	if perm != 0o660 {
		t.Fatalf("expected mode 0660, got %o", perm)
	}

	vm.Send(result, "primClose", nil)
}

func TestUnixSocketConnectToFailure(t *testing.T) {
	vm := NewVM()

	clientClass := vm.globals["UnixSocketClient"]
	pathVal := vm.registry.NewStringValue("/tmp/mag-nonexistent.sock")
	result := vm.Send(clientClass, "primConnectTo:", []Value{pathVal})

	if !isResultValue(result) {
		t.Fatal("connectTo: non-existent path should return a Result")
	}
	r := vm.registry.GetResultFromValue(result)
	if r == nil || r.resultType != ResultFailure {
		t.Fatal("connectTo: non-existent path should return Failure")
	}
}

func TestUnixSocketAcceptToChannel(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)

	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	serverVal := assertSuccess(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt:")

	ch := createChannel(5)
	chVal := vm.registry.RegisterChannel(ch)

	if result := assertSuccess(t, vm, vm.Send(serverVal, "primAcceptToChannel:", []Value{chVal}), "primAcceptToChannel:"); result != serverVal {
		t.Fatalf("acceptToChannel: should answer Success wrapping the server")
	}

	conn, err := net.Dial("unix", sockPath)
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()

	select {
	case connVal := <-ch.ch:
		if !isUnixConnValue(connVal) {
			t.Fatal("channel should receive SocketConnection value")
		}
		vm.Send(connVal, "primClose", nil)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for connection on channel")
	}

	vm.Send(serverVal, "primClose", nil)
}

// unixConnPair returns a SocketConnection Value for the client side and the
// raw server-side net.Conn.
func unixConnPair(t *testing.T, vm *VM) (Value, net.Conn) {
	t.Helper()
	path := tempSockPath(t)
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { ln.Close() })
	accepted := make(chan net.Conn, 1)
	go func() {
		c, err := ln.Accept()
		if err == nil {
			accepted <- c
		}
	}()
	connVal := assertSuccess(t, vm, vm.Send(vm.globals["UnixSocketClient"], "primConnectTo:", []Value{vm.registry.NewStringValue(path)}), "primConnectTo:")
	if vm.vmGetUnixConn(connVal) == nil {
		t.Fatal("primConnectTo: did not return a SocketConnection")
	}
	select {
	case server := <-accepted:
		t.Cleanup(func() { server.Close() })
		return connVal, server
	case <-time.After(3 * time.Second):
		t.Fatal("accept timed out")
	}
	return Nil, nil
}

// primReceive after primReceiveLine must see the bytes the line reader
// already buffered past the newline, not skip them.
func TestUnixSocketReceiveAfterReceiveLine(t *testing.T) {
	vm := NewVM()
	connVal, server := unixConnPair(t, vm)
	if _, err := server.Write([]byte("first\nrest")); err != nil {
		t.Fatal(err)
	}
	time.Sleep(50 * time.Millisecond) // let both parts arrive in one read

	line := assertSuccess(t, vm, vm.Send(connVal, "primReceiveLine", nil), "primReceiveLine")
	if got := vm.registry.GetStringContent(line); got != "first" {
		t.Fatalf("primReceiveLine = %q, want %q", got, "first")
	}
	got := make(chan string, 1)
	go func() {
		got <- vm.registry.GetStringContent(vm.Send(vm.Send(connVal, "primReceive", nil), "value", nil))
	}()
	select {
	case s := <-got:
		if s != "rest" {
			t.Fatalf("primReceive = %q, want %q", s, "rest")
		}
	case <-time.After(2 * time.Second):
		t.Fatal("primReceive blocked: bytes buffered by primReceiveLine were lost")
	}
}

// primClose must not wait behind a process blocked in primReceiveLine —
// closing is how that reader gets unblocked.
func TestUnixSocketCloseWhileReceiveLineBlocked(t *testing.T) {
	vm := NewVM()
	connVal, _ := unixConnPair(t, vm)

	readDone := make(chan struct{})
	go func() {
		defer close(readDone)
		vm.Send(connVal, "primReceiveLine", nil)
	}()
	time.Sleep(50 * time.Millisecond) // let the reader block

	closed := make(chan struct{})
	go func() {
		defer close(closed)
		vm.Send(connVal, "primClose", nil)
	}()
	select {
	case <-closed:
	case <-time.After(2 * time.Second):
		t.Fatal("primClose deadlocked behind a blocked primReceiveLine")
	}
	select {
	case <-readDone:
	case <-time.After(2 * time.Second):
		t.Fatal("blocked primReceiveLine was not released by primClose")
	}
}

// Failure doctrine: expected I/O failures answer Failure, and a closed
// connection answers Failure from every read/write primitive.
func TestUnixSocketClosedConnectionAnswersFailure(t *testing.T) {
	vm := NewVM()
	connVal, _ := unixConnPair(t, vm)
	vm.Send(connVal, "primClose", nil)
	for _, c := range []struct {
		sel  string
		args []Value
	}{
		{"primSend:", []Value{vm.registry.NewStringValue("x")}},
		{"primSendLine:", []Value{vm.registry.NewStringValue("x")}},
		{"primReceive", nil},
		{"primReceiveMax:", []Value{FromSmallInt(10)}},
		{"primReceiveLine", nil},
	} {
		assertFailure(t, vm, vm.Send(connVal, c.sel, c.args), c.sel+" on closed connection")
	}
}

// EOF from the peer is an expected failure: Failure, not a bare value.
func TestUnixSocketReceiveLineEOFAnswersFailure(t *testing.T) {
	vm := NewVM()
	connVal, server := unixConnPair(t, vm)
	server.Close()
	assertFailure(t, vm, vm.Send(connVal, "primReceiveLine", nil), "primReceiveLine at EOF")
}

// A non-Integer max byte count is a programmer error: it signals.
func TestUnixSocketReceiveMaxNonIntegerSignals(t *testing.T) {
	vm := NewVM()
	connVal, _ := unixConnPair(t, vm)
	if _, signaled := signalsPrimitiveError(vm, func() {
		vm.Send(connVal, "primReceiveMax:", []Value{vm.registry.NewStringValue("x")})
	}); !signaled {
		t.Error("primReceiveMax: with a non-Integer should signal")
	}
}

func TestUnixSocketListenInUseAnswersFailure(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)
	serverClass := vm.globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	first := assertSuccess(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt:")
	defer vm.Send(first, "primClose", nil)
	assertFailure(t, vm, vm.Send(serverClass, "primListenAt:", []Value{pathVal}), "primListenAt: in use")
}
