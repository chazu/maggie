package runtime

import (
	"testing"
)

// TestCrossClassMessaging verifies that one class can send messages to another class.
// This simulates the cross-plugin messaging scenario.
func TestCrossClassMessaging(t *testing.T) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Register "Plugin A" class: Counter
	counterMethods := NewMethodTable()
	counterMethods.AddInstanceMethod("getValue", func(self *Instance, args []Value) Value {
		return self.GetVar("value")
	}, 0, 0)
	counterMethods.AddInstanceMethod("setValue:", func(self *Instance, args []Value) Value {
		self.SetVar("value", args[0])
		return NilValue()
	}, 1, 0)
	counterMethods.AddInstanceMethod("increment", func(self *Instance, args []Value) Value {
		current := self.GetVar("value").AsInt()
		self.SetVar("value", IntValue(current+1))
		return self.GetVar("value")
	}, 0, 0)

	os.RegisterClass("Counter", "", []string{"value"}, counterMethods)

	// Register "Plugin B" class: CounterManager
	// This class will interact with Counter instances (cross-plugin messaging)
	managerMethods := NewMethodTable()
	managerMethods.AddInstanceMethod("incrementCounter:", func(self *Instance, args []Value) Value {
		// Get the counter instance from the argument
		counterInst := args[0].InstanceVal
		if counterInst == nil {
			return ErrorValue("expected Counter instance")
		}
		// Send message to the counter (cross-class messaging via SendDirect)
		return d.SendDirect(counterInst, "increment", nil)
	}, 1, 0)
	managerMethods.AddInstanceMethod("getCounterValue:", func(self *Instance, args []Value) Value {
		counterInst := args[0].InstanceVal
		if counterInst == nil {
			return ErrorValue("expected Counter instance")
		}
		return d.SendDirect(counterInst, "getValue", nil)
	}, 1, 0)

	os.RegisterClass("CounterManager", "", nil, managerMethods)

	// Create instances
	counter, err := os.NewInstance("Counter")
	if err != nil {
		t.Fatalf("Failed to create Counter: %v", err)
	}
	counter.SetVar("value", IntValue(10))

	manager, err := os.NewInstance("CounterManager")
	if err != nil {
		t.Fatalf("Failed to create CounterManager: %v", err)
	}

	// Test cross-class messaging: manager calls counter.increment
	result := d.SendDirect(manager, "incrementCounter:", []Value{InstanceValue(counter)})
	if result.Type == TypeError {
		t.Fatalf("incrementCounter: failed: %s", result.ErrorMsg)
	}
	if result.AsInt() != 11 {
		t.Errorf("Expected 11, got %d", result.AsInt())
	}

	// Verify via another cross-class call
	result = d.SendDirect(manager, "getCounterValue:", []Value{InstanceValue(counter)})
	if result.AsInt() != 11 {
		t.Errorf("Expected 11, got %d", result.AsInt())
	}
}

// TestCrossClassInstantiation verifies that one class can create instances of another class.
// This simulates the cross-plugin TT_New scenario.
func TestCrossClassInstantiation(t *testing.T) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Register "Plugin A" class: Event
	eventMethods := NewMethodTable()
	eventMethods.AddInstanceMethod("type", func(self *Instance, args []Value) Value {
		return self.GetVar("eventType")
	}, 0, 0)
	eventMethods.AddInstanceMethod("setType:", func(self *Instance, args []Value) Value {
		self.SetVar("eventType", args[0])
		return NilValue()
	}, 1, 0)

	os.RegisterClass("Event", "", []string{"eventType", "payload"}, eventMethods)

	// Register "Plugin B" class: EventFactory
	// This class creates Event instances (cross-class instantiation)
	factoryMethods := NewMethodTable()
	factoryMethods.AddClassMethod("createEvent:", func(self *Instance, args []Value) Value {
		// Create an Event instance (cross-class instantiation)
		result := d.Send("Event", "new", nil)
		if result.Type == TypeError {
			return result
		}
		eventInst := result.InstanceVal
		// Set the type on the new event
		d.SendDirect(eventInst, "setType:", args)
		return result
	}, 1, 0)

	os.RegisterClass("EventFactory", "", nil, factoryMethods)

	// Test cross-class instantiation
	result := d.Send("EventFactory", "createEvent:", []Value{StringValue("click")})
	if result.Type == TypeError {
		t.Fatalf("createEvent: failed: %s", result.ErrorMsg)
	}
	if result.Type != TypeInstance {
		t.Fatalf("Expected instance, got %v", result.Type)
	}

	event := result.InstanceVal
	if event.ClassName != "Event" {
		t.Errorf("Expected Event class, got %s", event.ClassName)
	}

	// Verify the event was properly initialized
	eventType := d.SendDirect(event, "type", nil)
	if eventType.AsString() != "click" {
		t.Errorf("Expected 'click', got '%s'", eventType.AsString())
	}
}

// TestEventDispatchChain simulates the Yutani event chain:
// GrpcClient -> Event -> EventDispatcher -> Widget
func TestEventDispatchChain(t *testing.T) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Track method calls for verification
	var callLog []string

	// Register Widget class
	widgetMethods := NewMethodTable()
	widgetMethods.AddInstanceMethod("handleEvent:", func(self *Instance, args []Value) Value {
		callLog = append(callLog, "Widget.handleEvent:")
		eventInst := args[0].InstanceVal
		if eventInst == nil {
			return ErrorValue("expected Event instance")
		}
		// Get event type and update widget state
		eventType := d.SendDirect(eventInst, "type", nil)
		self.SetVar("lastEvent", eventType)
		return StringValue("handled")
	}, 1, 0)
	widgetMethods.AddInstanceMethod("lastEvent", func(self *Instance, args []Value) Value {
		return self.GetVar("lastEvent")
	}, 0, 0)

	os.RegisterClass("Widget", "", []string{"lastEvent"}, widgetMethods)

	// Register Event class
	eventMethods := NewMethodTable()
	eventMethods.AddInstanceMethod("type", func(self *Instance, args []Value) Value {
		return self.GetVar("eventType")
	}, 0, 0)
	eventMethods.AddInstanceMethod("setType:", func(self *Instance, args []Value) Value {
		self.SetVar("eventType", args[0])
		return NilValue()
	}, 1, 0)

	os.RegisterClass("Event", "", []string{"eventType", "payload"}, eventMethods)

	// Register EventDispatcher class
	dispatcherMethods := NewMethodTable()
	dispatcherMethods.AddInstanceMethod("dispatch:to:", func(self *Instance, args []Value) Value {
		callLog = append(callLog, "EventDispatcher.dispatch:to:")
		event := args[0].InstanceVal
		widget := args[1].InstanceVal
		if event == nil || widget == nil {
			return ErrorValue("expected Event and Widget instances")
		}
		// Forward event to widget (cross-class messaging)
		return d.SendDirect(widget, "handleEvent:", []Value{args[0]})
	}, 2, 0)

	os.RegisterClass("EventDispatcher", "", nil, dispatcherMethods)

	// Register GrpcClient class (the entry point)
	grpcMethods := NewMethodTable()
	grpcMethods.AddInstanceMethod("receiveEvent:dispatchVia:toWidget:", func(self *Instance, args []Value) Value {
		callLog = append(callLog, "GrpcClient.receiveEvent:dispatchVia:toWidget:")
		eventType := args[0].AsString()
		dispatcher := args[1].InstanceVal
		widget := args[2].InstanceVal

		// Create a new Event (cross-class instantiation)
		eventResult := d.Send("Event", "new", nil)
		if eventResult.Type == TypeError {
			return eventResult
		}
		event := eventResult.InstanceVal
		d.SendDirect(event, "setType:", []Value{StringValue(eventType)})

		// Dispatch the event (cross-class messaging chain)
		return d.SendDirect(dispatcher, "dispatch:to:", []Value{InstanceValue(event), InstanceValue(widget)})
	}, 3, 0)

	os.RegisterClass("GrpcClient", "", nil, grpcMethods)

	// Create instances
	widget, _ := os.NewInstance("Widget")
	dispatcher, _ := os.NewInstance("EventDispatcher")
	grpcClient, _ := os.NewInstance("GrpcClient")

	// Trigger the event chain
	result := d.SendDirect(grpcClient, "receiveEvent:dispatchVia:toWidget:", []Value{
		StringValue("keypress"),
		InstanceValue(dispatcher),
		InstanceValue(widget),
	})

	if result.Type == TypeError {
		t.Fatalf("Event chain failed: %s", result.ErrorMsg)
	}
	if result.AsString() != "handled" {
		t.Errorf("Expected 'handled', got '%s'", result.AsString())
	}

	// Verify the call chain
	expectedCalls := []string{
		"GrpcClient.receiveEvent:dispatchVia:toWidget:",
		"EventDispatcher.dispatch:to:",
		"Widget.handleEvent:",
	}
	if len(callLog) != len(expectedCalls) {
		t.Errorf("Expected %d calls, got %d: %v", len(expectedCalls), len(callLog), callLog)
	}
	for i, expected := range expectedCalls {
		if i < len(callLog) && callLog[i] != expected {
			t.Errorf("Call %d: expected %s, got %s", i, expected, callLog[i])
		}
	}

	// Verify widget received the event
	lastEvent := d.SendDirect(widget, "lastEvent", nil)
	if lastEvent.AsString() != "keypress" {
		t.Errorf("Expected widget lastEvent='keypress', got '%s'", lastEvent.AsString())
	}
}

// TestSendByStringReceiver tests the Send function with string receiver lookups
func TestSendByStringReceiver(t *testing.T) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Register a simple class
	methods := NewMethodTable()
	methods.AddInstanceMethod("ping", func(self *Instance, args []Value) Value {
		return StringValue("pong")
	}, 0, 0)
	methods.AddClassMethod("version", func(self *Instance, args []Value) Value {
		return StringValue("1.0")
	}, 0, 0)

	os.RegisterClass("PingService", "", nil, methods)

	// Test class method via string receiver
	result := d.Send("PingService", "version", nil)
	if result.AsString() != "1.0" {
		t.Errorf("Expected '1.0', got '%s'", result.AsString())
	}

	// Test instance method via string receiver (instance ID)
	inst, _ := os.NewInstance("PingService")
	result = d.Send(inst.ID, "ping", nil)
	if result.AsString() != "pong" {
		t.Errorf("Expected 'pong', got '%s'", result.AsString())
	}
}

// TestInheritanceAcrossClasses tests that method inheritance works correctly
func TestInheritanceAcrossClasses(t *testing.T) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Register base class
	baseMethods := NewMethodTable()
	baseMethods.AddInstanceMethod("baseMethod", func(self *Instance, args []Value) Value {
		return StringValue("from base")
	}, 0, 0)
	os.RegisterClass("BaseClass", "", []string{"baseVar"}, baseMethods)

	// Register derived class
	derivedMethods := NewMethodTable()
	derivedMethods.AddInstanceMethod("derivedMethod", func(self *Instance, args []Value) Value {
		return StringValue("from derived")
	}, 0, 0)
	os.RegisterClass("DerivedClass", "BaseClass", []string{"derivedVar"}, derivedMethods)

	// Create instance of derived class
	inst, err := os.NewInstance("DerivedClass")
	if err != nil {
		t.Fatalf("Failed to create DerivedClass: %v", err)
	}

	// Test that derived instance has both base and derived vars
	if len(inst.Vars) != 2 {
		t.Errorf("Expected 2 instance vars, got %d", len(inst.Vars))
	}

	// Test derived method
	result := d.SendDirect(inst, "derivedMethod", nil)
	if result.AsString() != "from derived" {
		t.Errorf("Expected 'from derived', got '%s'", result.AsString())
	}

	// Test inherited base method
	result = d.SendDirect(inst, "baseMethod", nil)
	if result.AsString() != "from base" {
		t.Errorf("Expected 'from base', got '%s'", result.AsString())
	}
}
