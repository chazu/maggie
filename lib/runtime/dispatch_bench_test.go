package runtime

import (
	"testing"
)

// BenchmarkNativeDispatch measures pure native method dispatch performance.
// This represents the optimal case when all classes are compiled as native plugins.
func BenchmarkNativeDispatch(b *testing.B) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Register a simple class with a fast method
	methods := NewMethodTable()
	methods.AddInstanceMethod("increment", func(self *Instance, args []Value) Value {
		current := self.GetVar("value").AsInt()
		self.SetVar("value", IntValue(current+1))
		return self.GetVar("value")
	}, 0, 0)

	os.RegisterClass("Counter", "", []string{"value"}, methods)
	counter, _ := os.NewInstance("Counter")
	counter.SetVar("value", IntValue(0))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		d.SendDirect(counter, "increment", nil)
	}
}

// BenchmarkNativeDispatchWithLookup measures dispatch including instance lookup.
// This is the common case for TT_Send (string receiver).
func BenchmarkNativeDispatchWithLookup(b *testing.B) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	methods := NewMethodTable()
	methods.AddInstanceMethod("increment", func(self *Instance, args []Value) Value {
		current := self.GetVar("value").AsInt()
		self.SetVar("value", IntValue(current+1))
		return self.GetVar("value")
	}, 0, 0)

	os.RegisterClass("Counter", "", []string{"value"}, methods)
	counter, _ := os.NewInstance("Counter")
	counter.SetVar("value", IntValue(0))
	counterID := counter.ID

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		d.Send(counterID, "increment", nil)
	}
}

// BenchmarkCrossClassMessaging measures dispatch across class boundaries.
// This simulates the cross-plugin messaging scenario.
func BenchmarkCrossClassMessaging(b *testing.B) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Counter class
	counterMethods := NewMethodTable()
	counterMethods.AddInstanceMethod("getValue", func(self *Instance, args []Value) Value {
		return self.GetVar("value")
	}, 0, 0)
	counterMethods.AddInstanceMethod("increment", func(self *Instance, args []Value) Value {
		current := self.GetVar("value").AsInt()
		self.SetVar("value", IntValue(current+1))
		return self.GetVar("value")
	}, 0, 0)
	os.RegisterClass("Counter", "", []string{"value"}, counterMethods)

	// Manager class that calls Counter
	managerMethods := NewMethodTable()
	managerMethods.AddInstanceMethod("incrementCounter:", func(self *Instance, args []Value) Value {
		counterInst := args[0].InstanceVal
		return d.SendDirect(counterInst, "increment", nil)
	}, 1, 0)
	os.RegisterClass("CounterManager", "", nil, managerMethods)

	counter, _ := os.NewInstance("Counter")
	counter.SetVar("value", IntValue(0))
	manager, _ := os.NewInstance("CounterManager")
	counterVal := InstanceValue(counter)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		d.SendDirect(manager, "incrementCounter:", []Value{counterVal})
	}
}

// BenchmarkEventDispatchChain measures a realistic event dispatch chain.
// GrpcClient -> Event -> EventDispatcher -> Widget (4 classes)
func BenchmarkEventDispatchChain(b *testing.B) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	// Widget
	widgetMethods := NewMethodTable()
	widgetMethods.AddInstanceMethod("handleEvent:", func(self *Instance, args []Value) Value {
		self.SetVar("lastEvent", args[0].InstanceVal.GetVar("eventType"))
		return StringValue("handled")
	}, 1, 0)
	os.RegisterClass("Widget", "", []string{"lastEvent"}, widgetMethods)

	// Event
	eventMethods := NewMethodTable()
	eventMethods.AddInstanceMethod("type", func(self *Instance, args []Value) Value {
		return self.GetVar("eventType")
	}, 0, 0)
	os.RegisterClass("Event", "", []string{"eventType"}, eventMethods)

	// EventDispatcher
	dispatcherMethods := NewMethodTable()
	dispatcherMethods.AddInstanceMethod("dispatch:to:", func(self *Instance, args []Value) Value {
		return d.SendDirect(args[1].InstanceVal, "handleEvent:", []Value{args[0]})
	}, 2, 0)
	os.RegisterClass("EventDispatcher", "", nil, dispatcherMethods)

	// Pre-create reusable instances
	widget, _ := os.NewInstance("Widget")
	dispatcher, _ := os.NewInstance("EventDispatcher")
	event, _ := os.NewInstance("Event")
	event.SetVar("eventType", StringValue("keypress"))

	eventVal := InstanceValue(event)
	widgetVal := InstanceValue(widget)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		d.SendDirect(dispatcher, "dispatch:to:", []Value{eventVal, widgetVal})
	}
}

// BenchmarkClassMethodDispatch measures class method calls (like TT_New).
func BenchmarkClassMethodDispatch(b *testing.B) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	methods := NewMethodTable()
	methods.AddClassMethod("version", func(self *Instance, args []Value) Value {
		return StringValue("1.0")
	}, 0, 0)
	os.RegisterClass("Service", "", nil, methods)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		d.Send("Service", "version", nil)
	}
}

// BenchmarkInstanceCreation measures TT_New performance.
func BenchmarkInstanceCreation(b *testing.B) {
	os := NewObjectSpace()
	d := NewDispatcher(os)

	methods := NewMethodTable()
	os.RegisterClass("Counter", "", []string{"value", "step"}, methods)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		d.Send("Counter", "new", nil)
	}
}
