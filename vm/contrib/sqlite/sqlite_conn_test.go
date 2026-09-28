package sqlite

import (
	"math/big"
	"path/filepath"
	"testing"

	vm "github.com/chazu/maggie/vm"
)

// database/sql pools connections. Each connection to ":memory:" is a separate
// empty database, and BEGIN/COMMIT only apply to the connection they ran on,
// so anything that forces a second connection (an open cursor) used to see a
// different database or escape the transaction.

func sqliteStr(vmInst *vm.VM, s string) vm.Value { return vmInst.Registry().NewStringValue(s) }

func sqliteCount(t *testing.T, vmInst *vm.VM, db vm.Value, table string) int64 {
	t.Helper()
	row := vmInst.Send(db, "primQueryRow:", []vm.Value{sqliteStr(vmInst, "SELECT COUNT(*) AS c FROM "+table)})
	if sqliteIsFailure(vmInst, row) {
		t.Fatalf("count %s: %s", table, sqliteFailureMsg(vmInst, row))
	}
	c := vmInst.DictionaryAt(row, sqliteStr(vmInst, "c"))
	if !c.IsSmallInt() {
		t.Fatalf("count %s: not an integer", table)
	}
	return c.SmallInt()
}

func TestSqliteMemoryVisibleWithOpenCursor(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	sqliteExec(t, vmInst, db, "CREATE TABLE m (n INTEGER)")
	sqliteExec(t, vmInst, db, "INSERT INTO m VALUES (1), (2)")

	// Hold a cursor open so the next query needs another connection.
	cursor := vmInst.Send(db, "primQuery:", []vm.Value{sqliteStr(vmInst, "SELECT n FROM m")})
	if sqliteIsFailure(vmInst, cursor) {
		t.Fatal(sqliteFailureMsg(vmInst, cursor))
	}
	vmInst.Send(cursor, "primNext", nil)

	all := vmInst.Send(db, "primQueryAll:", []vm.Value{sqliteStr(vmInst, "SELECT n FROM m")})
	if sqliteIsFailure(vmInst, all) {
		t.Fatalf("second query saw a different in-memory database: %s", sqliteFailureMsg(vmInst, all))
	}
	vmInst.Send(cursor, "primClose", nil)
}

func TestSqliteTransactionPinsConnection(t *testing.T) {
	vmInst := vm.NewVM()
	dbClass := vmInst.MustGlobal("SqliteDatabase")
	db := vmInst.Send(dbClass, "primOpen:", []vm.Value{sqliteStr(vmInst, filepath.Join(t.TempDir(), "tx.db"))})
	if sqliteIsFailure(vmInst, db) {
		t.Fatal(sqliteFailureMsg(vmInst, db))
	}
	sqliteExec(t, vmInst, db, "CREATE TABLE tx (n INTEGER)")

	vmInst.Send(db, "primBeginTransaction", nil)
	cursor := vmInst.Send(db, "primQuery:", []vm.Value{sqliteStr(vmInst, "SELECT 1")})
	vmInst.Send(cursor, "primNext", nil)
	sqliteExec(t, vmInst, db, "INSERT INTO tx VALUES (1)")
	vmInst.Send(cursor, "primClose", nil)

	if res := vmInst.Send(db, "primRollbackTransaction", nil); res != vm.True {
		t.Fatalf("rollback: %s", sqliteFailureMsg(vmInst, res))
	}
	if n := sqliteCount(t, vmInst, db, "tx"); n != 0 {
		t.Fatalf("insert escaped the transaction: %d rows after rollback", n)
	}

	// Prepared statements must run inside the transaction too.
	stmt := vmInst.Send(db, "primPrepare:", []vm.Value{sqliteStr(vmInst, "INSERT INTO tx VALUES (?)")})
	vmInst.Send(db, "primBeginTransaction", nil)
	vmInst.Send(stmt, "primExecuteWith:", []vm.Value{vmInst.NewArrayWithElements([]vm.Value{vm.FromSmallInt(7)})})
	vmInst.Send(db, "primRollbackTransaction", nil)
	if n := sqliteCount(t, vmInst, db, "tx"); n != 0 {
		t.Fatalf("prepared insert escaped the transaction: %d rows", n)
	}

	vmInst.Send(db, "primBeginTransaction", nil)
	sqliteExec(t, vmInst, db, "INSERT INTO tx VALUES (2)")
	if res := vmInst.Send(db, "primCommitTransaction", nil); res != vm.True {
		t.Fatalf("commit: %s", sqliteFailureMsg(vmInst, res))
	}
	if n := sqliteCount(t, vmInst, db, "tx"); n != 1 {
		t.Fatalf("want 1 committed row, got %d", n)
	}
}

func TestSqliteTransactionStateErrors(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	if res := vmInst.Send(db, "primCommitTransaction", nil); !sqliteIsFailure(vmInst, res) {
		t.Error("commit without a transaction should fail")
	}
	vmInst.Send(db, "primBeginTransaction", nil)
	if res := vmInst.Send(db, "primBeginTransaction", nil); !sqliteIsFailure(vmInst, res) {
		t.Error("nested begin should fail")
	}
	vmInst.Send(db, "primRollbackTransaction", nil)
}

func TestSqliteMigrateLargeVersion(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	version := vmInst.Registry().NewBigIntValue(big.NewInt(20260928120000)) // timestamp-style; fits a SmallInteger
	big50 := vmInst.Registry().NewBigIntValue(big.NewInt(1 << 50))
	for _, v := range []vm.Value{version, big50} {
		res := vmInst.Send(db, "primMigrate:version:", []vm.Value{sqliteStr(vmInst, "SELECT 1"), v})
		if sqliteIsFailure(vmInst, res) {
			t.Fatalf("migrate: %s", sqliteFailureMsg(vmInst, res))
		}
	}
	got := vmInst.Registry().GetBigInt(vmInst.Send(db, "primMigrationVersion", nil))
	if got == nil || got.Value.Cmp(big.NewInt(1<<50)) != 0 {
		t.Fatal("migrationVersion should round-trip a BigInteger version")
	}
}

func TestSqliteMigrateJoinsActiveTransaction(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	vmInst.Send(db, "primBeginTransaction", nil)
	res := vmInst.Send(db, "primMigrate:version:", []vm.Value{sqliteStr(vmInst, "CREATE TABLE mig (n INTEGER)"), vm.FromSmallInt(1)})
	if res != vm.True {
		t.Fatalf("migrate inside transaction: %s", sqliteFailureMsg(vmInst, res))
	}
	vmInst.Send(db, "primRollbackTransaction", nil)
	if exists := vmInst.Send(db, "primTableExists:", []vm.Value{sqliteStr(vmInst, "mig")}); exists != vm.False {
		t.Fatal("rolled-back migration left its table behind")
	}
}
