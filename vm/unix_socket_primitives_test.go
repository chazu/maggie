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
	serverClass := vm.Globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	result := vm.Send(serverClass, "primListenAt:", []Value{pathVal})

	if !isUnixListenerValue(result) {
		if isResultValue(result) {
			r := vm.registry.GetResultFromValue(result)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("listenAt: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatalf("listenAt: should return a UnixSocketServer value, got %v", result)
	}

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
	clientClass := vm.Globals["UnixSocketClient"]
	pathVal := vm.registry.NewStringValue(sockPath)
	connVal := vm.Send(clientClass, "primConnectTo:", []Value{pathVal})

	if !isUnixConnValue(connVal) {
		if isResultValue(connVal) {
			r := vm.registry.GetResultFromValue(connVal)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("connectTo: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatalf("connectTo: should return a SocketConnection value, got %v", connVal)
	}

	// send:
	sendResult := vm.Send(connVal, "primSend:", []Value{vm.registry.NewStringValue("hello")})
	if sendResult != connVal {
		if isResultValue(sendResult) {
			r := vm.registry.GetResultFromValue(sendResult)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("send: failed: %v", vm.valueToString(r.value))
			}
		}
	}

	// receive
	recvResult := vm.Send(connVal, "primReceive", nil)
	if isResultValue(recvResult) {
		r := vm.registry.GetResultFromValue(recvResult)
		if r != nil && r.resultType == ResultFailure {
			t.Fatalf("receive failed: %v", vm.valueToString(r.value))
		}
	}
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

	clientClass := vm.Globals["UnixSocketClient"]
	pathVal := vm.registry.NewStringValue(sockPath)
	connVal := vm.Send(clientClass, "primConnectTo:", []Value{pathVal})
	if !isUnixConnValue(connVal) {
		if isResultValue(connVal) {
			r := vm.registry.GetResultFromValue(connVal)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("connectTo: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatalf("connectTo: should return SocketConnection, got %v", connVal)
	}

	// sendLine:
	vm.Send(connVal, "primSendLine:", []Value{vm.registry.NewStringValue(`{"method":"test"}`)})

	// receiveLine
	lineResult := vm.Send(connVal, "primReceiveLine", nil)
	if isResultValue(lineResult) {
		r := vm.registry.GetResultFromValue(lineResult)
		if r != nil && r.resultType == ResultFailure {
			t.Fatalf("receiveLine failed: %v", vm.valueToString(r.value))
		}
	}
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

	serverClass := vm.Globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	serverVal := vm.Send(serverClass, "primListenAt:", []Value{pathVal})
	if !isUnixListenerValue(serverVal) {
		if isResultValue(serverVal) {
			r := vm.registry.GetResultFromValue(serverVal)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("listenAt: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatalf("listenAt: should return UnixSocketServer value")
	}

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
	connVal := vm.Send(serverVal, "primAccept", nil)
	if !isUnixConnValue(connVal) {
		if isResultValue(connVal) {
			r := vm.registry.GetResultFromValue(connVal)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("accept failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatalf("accept should return SocketConnection")
	}

	lineResult := vm.Send(connVal, "primReceiveLine", nil)
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

	serverClass := vm.Globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	serverVal := vm.Send(serverClass, "primListenAt:", []Value{pathVal})
	if !isUnixListenerValue(serverVal) {
		if isResultValue(serverVal) {
			r := vm.registry.GetResultFromValue(serverVal)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("listenAt: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatal("listenAt: should return UnixSocketServer value")
	}

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
		connVal := vm.Send(serverVal, "primAccept", nil)
		if !isUnixConnValue(connVal) {
			continue
		}
		lineResult := vm.Send(connVal, "primReceiveLine", nil)
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
	serverClass := vm.Globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	result := vm.Send(serverClass, "primListenAt:", []Value{pathVal})

	if !isUnixListenerValue(result) {
		if isResultValue(result) {
			r := vm.registry.GetResultFromValue(result)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("listenAt: should succeed on stale socket, got: %v", vm.valueToString(r.value))
			}
		}
		t.Fatal("listenAt: should return UnixSocketServer value on stale socket cleanup")
	}

	vm.Send(result, "primClose", nil)
}

func TestUnixSocketServerListenAtMode(t *testing.T) {
	vm := NewVM()
	sockPath := tempSockPath(t)

	serverClass := vm.Globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	modeVal := FromSmallInt(0o660)
	result := vm.Send(serverClass, "primListenAtMode:mode:", []Value{pathVal, modeVal})

	if !isUnixListenerValue(result) {
		if isResultValue(result) {
			r := vm.registry.GetResultFromValue(result)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("listenAt:mode: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatal("listenAt:mode: should return UnixSocketServer value")
	}

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

	clientClass := vm.Globals["UnixSocketClient"]
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

	serverClass := vm.Globals["UnixSocketServer"]
	pathVal := vm.registry.NewStringValue(sockPath)
	serverVal := vm.Send(serverClass, "primListenAt:", []Value{pathVal})
	if !isUnixListenerValue(serverVal) {
		if isResultValue(serverVal) {
			r := vm.registry.GetResultFromValue(serverVal)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("listenAt: failed: %v", vm.valueToString(r.value))
			}
		}
		t.Fatal("listenAt: should return UnixSocketServer value")
	}

	ch := createChannel(5)
	chVal := vm.registry.RegisterChannel(ch)

	result := vm.Send(serverVal, "primAcceptToChannel:", []Value{chVal})
	if result != serverVal {
		if isResultValue(result) {
			r := vm.registry.GetResultFromValue(result)
			if r != nil && r.resultType == ResultFailure {
				t.Fatalf("acceptToChannel: failed: %v", vm.valueToString(r.value))
			}
		}
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
