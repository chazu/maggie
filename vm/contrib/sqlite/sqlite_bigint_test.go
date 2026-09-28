package sqlite

import (
	"math"
	"math/big"
	"strings"
	"testing"

	vm "github.com/chazu/maggie/vm"
)

// SQLite INTEGER is 64-bit but Maggie's SmallInteger is 48-bit. Values in
// between must round-trip as BigIntegers in both directions: reading them
// used to panic the VM, and binding a BigInteger parameter used to write NULL.

func sqliteIntArg(vmInst *vm.VM, n *big.Int) vm.Value {
	return vmInst.Registry().NewBigIntValue(n)
}

func TestSqliteLargeIntegerRoundTrip(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	sqliteExec(t, vmInst, db, "CREATE TABLE big (id INTEGER PRIMARY KEY, n INTEGER)")

	for _, n := range []int64{1 << 50, math.MaxInt64, math.MinInt64} {
		want := big.NewInt(n)
		sqliteExec(t, vmInst, db, "DELETE FROM big")

		// Write through a bound BigInteger parameter.
		params := vmInst.NewArrayWithElements([]vm.Value{sqliteIntArg(vmInst, want)})
		ins := vmInst.Send(db, "primExecuteWith:params:", []vm.Value{
			vmInst.Registry().NewStringValue("INSERT INTO big (n) VALUES (?)"), params})
		if sqliteIsFailure(vmInst, ins) {
			t.Fatalf("insert %d: %s", n, sqliteFailureMsg(vmInst, ins))
		}

		// NULL here means the parameter was silently dropped.
		row := vmInst.Send(db, "primQueryRow:", []vm.Value{
			vmInst.Registry().NewStringValue("SELECT n, typeof(n) AS t FROM big")})
		if sqliteIsFailure(vmInst, row) {
			t.Fatalf("select %d: %s", n, sqliteFailureMsg(vmInst, row))
		}
		typ := vmInst.DictionaryAt(row, vmInst.Registry().NewStringValue("t"))
		if got := vmInst.ValueToString(typ); got != "integer" {
			t.Fatalf("stored %d as %q, want integer", n, got)
		}
		got := vmInst.DictionaryAt(row, vmInst.Registry().NewStringValue("n"))
		bi := vmInst.Registry().GetBigInt(got)
		if bi == nil || bi.Value.Cmp(want) != 0 {
			t.Fatalf("read back %d: got non-matching value", n)
		}
	}
}

func TestSqliteLastInsertIdLarge(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	sqliteExec(t, vmInst, db, "CREATE TABLE lid (id INTEGER PRIMARY KEY)")
	sqliteExec(t, vmInst, db, "INSERT INTO lid (id) VALUES (1125899906842624)") // 2^50

	got := vmInst.Send(db, "primLastInsertId", nil)
	bi := vmInst.Registry().GetBigInt(got)
	if bi == nil || bi.Value.Cmp(big.NewInt(1<<50)) != 0 {
		t.Fatalf("lastInsertId: want 2^50 as BigInteger")
	}
}

func TestSqliteBindBeyondInt64Fails(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	sqliteExec(t, vmInst, db, "CREATE TABLE big (n INTEGER)")

	tooBig := new(big.Int).Lsh(big.NewInt(1), 70)
	params := vmInst.NewArrayWithElements([]vm.Value{sqliteIntArg(vmInst, tooBig)})
	res := vmInst.Send(db, "primExecuteWith:params:", []vm.Value{
		vmInst.Registry().NewStringValue("INSERT INTO big (n) VALUES (?)"), params})
	if !sqliteIsFailure(vmInst, res) {
		t.Fatal("binding a BigInteger beyond int64 must fail, not store NULL")
	}
	if msg := sqliteFailureMsg(vmInst, res); !strings.Contains(msg, "64-bit") {
		t.Errorf("failure message should explain the range: %q", msg)
	}
}
