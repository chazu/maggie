// Package ganso wraps the ganso SQLite-backed coordination toolkit
// (github.com/chazu/ganso) as Maggie primitives. Phase 0: a minimal
// Database/Queue/Job surface to prove the binding links and round-trips a job.
package ganso

import (
	"context"
	"reflect"

	ganso "github.com/chazu/ganso"
	vm "github.com/chazu/maggie/vm"
)

type GansoDBObject struct {
	db     *ganso.Database
	path   string
	closed bool
}

type GansoQueueObject struct {
	q *ganso.Queue
}

type GansoJobObject struct {
	job *ganso.Job
}

// RegisterGansoPrimitives registers the GansoDatabase / GansoQueue / GansoJob
// Go types and their primitive methods.
func RegisterGansoPrimitives(v *vm.VM) {
	dbClass := v.RegisterGoType("GansoDatabase", reflect.TypeOf((*GansoDBObject)(nil)))
	qClass := v.RegisterGoType("GansoQueue", reflect.TypeOf((*GansoQueueObject)(nil)))
	jClass := v.RegisterGoType("GansoJob", reflect.TypeOf((*GansoJobObject)(nil)))

	// GansoDatabase open: <path> — open/create a ganso DB at path.
	dbClass.AddClassMethod1(v.Selectors, "open:", func(v *vm.VM, recv vm.Value, pathVal vm.Value) vm.Value {
		path := v.ValueToString(pathVal)
		if path == "" {
			return v.NewFailureResult("Ganso open: requires a path string")
		}
		db, err := ganso.Open(path)
		if err != nil {
			return v.NewFailureResult("ganso open: " + err.Error())
		}
		val, regErr := v.RegisterGoObject(&GansoDBObject{db: db, path: path})
		if regErr != nil {
			db.Close()
			return v.NewFailureResult("ganso register db: " + regErr.Error())
		}
		return val
	})

	dbClass.AddMethod1(v.Selectors, "queue:", func(v *vm.VM, recv vm.Value, nameVal vm.Value) vm.Value {
		o := getGansoDB(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoDatabase")
		}
		q := o.db.Queue(v.ValueToString(nameVal))
		val, err := v.RegisterGoObject(&GansoQueueObject{q: q})
		if err != nil {
			return v.NewFailureResult("ganso register queue: " + err.Error())
		}
		return val
	})

	dbClass.AddMethod0(v.Selectors, "close", func(v *vm.VM, recv vm.Value) vm.Value {
		o := getGansoDB(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoDatabase")
		}
		if !o.closed {
			o.db.Close()
			o.closed = true
		}
		return vm.True
	})

	// GansoDatabase exec: <sql> args: <Array> — single positional-param write
	// statement in an IMMEDIATE transaction. Returns true, or a failure.
	dbClass.AddMethod2(v.Selectors, "execute:with:", func(v *vm.VM, recv vm.Value, sqlVal, argsVal vm.Value) vm.Value {
		o := getGansoDB(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoDatabase")
		}
		if err := o.db.Exec(v.ValueToString(sqlVal), gansoArgs(v, argsVal)...); err != nil {
			return v.NewFailureResult("ganso exec: " + err.Error())
		}
		return vm.True
	})

	// GansoDatabase query: <sql> args: <Array> — positional-param read.
	// Returns an Array of Dictionaries (column name -> value).
	dbClass.AddMethod2(v.Selectors, "query:with:", func(v *vm.VM, recv vm.Value, sqlVal, argsVal vm.Value) vm.Value {
		o := getGansoDB(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoDatabase")
		}
		rows, err := o.db.QueryArgs(context.Background(), v.ValueToString(sqlVal), gansoArgs(v, argsVal)...)
		if err != nil {
			return v.NewFailureResult("ganso query: " + err.Error())
		}
		if rows == nil {
			rows = []map[string]any{}
		}
		return v.GoToValue(rows)
	})

	// GansoQueue enqueue: <payloadString> — returns the job id.
	qClass.AddMethod1(v.Selectors, "enqueue:", func(v *vm.VM, recv vm.Value, payloadVal vm.Value) vm.Value {
		o := getGansoQueue(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoQueue")
		}
		id, err := o.q.Enqueue(v.ValueToString(payloadVal))
		if err != nil {
			return v.NewFailureResult("ganso enqueue: " + err.Error())
		}
		return v.Registry().NewStringValue(id)
	})

	// GansoQueue claimOne: <workerID> — returns a GansoJob, or nil if none.
	qClass.AddMethod1(v.Selectors, "claimOne:", func(v *vm.VM, recv vm.Value, workerVal vm.Value) vm.Value {
		o := getGansoQueue(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoQueue")
		}
		job, err := o.q.ClaimOne(v.ValueToString(workerVal))
		if err != nil {
			return v.NewFailureResult("ganso claim: " + err.Error())
		}
		if job == nil {
			return vm.Nil
		}
		val, regErr := v.RegisterGoObject(&GansoJobObject{job: job})
		if regErr != nil {
			return v.NewFailureResult("ganso register job: " + regErr.Error())
		}
		return val
	})

	jClass.AddMethod0(v.Selectors, "id", func(v *vm.VM, recv vm.Value) vm.Value {
		o := getGansoJob(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoJob")
		}
		return v.Registry().NewStringValue(o.job.ID)
	})

	jClass.AddMethod0(v.Selectors, "payload", func(v *vm.VM, recv vm.Value) vm.Value {
		o := getGansoJob(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoJob")
		}
		return v.Registry().NewStringValue(string(o.job.Payload))
	})

	jClass.AddMethod0(v.Selectors, "ack", func(v *vm.VM, recv vm.Value) vm.Value {
		o := getGansoJob(v, recv)
		if o == nil {
			return v.NewFailureResult("not a GansoJob")
		}
		if err := o.job.Ack(); err != nil {
			return v.NewFailureResult("ganso ack: " + err.Error())
		}
		return vm.True
	})
}

// gansoArgs converts a Maggie Array of params to a Go []any (positional).
// Mirrors the sqlite contrib's valueToGoArgs. A non-array value is treated as
// a single arg; nil/empty yields no args.
func gansoArgs(v *vm.VM, val vm.Value) []interface{} {
	if val == vm.Nil {
		return nil
	}
	if !val.IsObject() {
		return []interface{}{v.ValueToGo(val)}
	}
	obj := vm.ObjectFromValue(val)
	if obj == nil {
		return []interface{}{v.ValueToGo(val)}
	}
	n := obj.NumSlots()
	args := make([]interface{}, n)
	for i := 0; i < n; i++ {
		args[i] = v.ValueToGo(obj.GetSlot(i))
	}
	return args
}

func getGansoDB(v *vm.VM, val vm.Value) *GansoDBObject {
	g, ok := v.GetGoObject(val)
	if !ok {
		return nil
	}
	o, ok := g.(*GansoDBObject)
	if !ok {
		return nil
	}
	return o
}

func getGansoQueue(v *vm.VM, val vm.Value) *GansoQueueObject {
	g, ok := v.GetGoObject(val)
	if !ok {
		return nil
	}
	o, ok := g.(*GansoQueueObject)
	if !ok {
		return nil
	}
	return o
}

func getGansoJob(v *vm.VM, val vm.Value) *GansoJobObject {
	g, ok := v.GetGoObject(val)
	if !ok {
		return nil
	}
	o, ok := g.(*GansoJobObject)
	if !ok {
		return nil
	}
	return o
}
