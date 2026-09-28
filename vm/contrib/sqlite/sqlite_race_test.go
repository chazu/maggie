package sqlite

import (
	"sync"
	"testing"

	vm "github.com/chazu/maggie/vm"
)

// Forked Maggie processes can share statement and cursor objects; their
// primitives must not race on the closed flag or on the underlying *sql.Rows.
// Run with -race.

func TestSqliteStatementAndRowsConcurrentUse(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	sqliteExec(t, vmInst, db, "CREATE TABLE r (n INTEGER)")
	for i := 0; i < 50; i++ {
		sqliteExec(t, vmInst, db, "INSERT INTO r VALUES (1)")
	}

	stmt := vmInst.Send(db, "primPrepare:", []vm.Value{sqliteStr(vmInst, "SELECT n FROM r")})
	rows := vmInst.Send(db, "primQuery:", []vm.Value{sqliteStr(vmInst, "SELECT n FROM r")})

	var wg sync.WaitGroup
	for g := 0; g < 4; g++ {
		wg.Add(2)
		go func() {
			defer wg.Done()
			for i := 0; i < 20; i++ {
				if vmInst.Send(rows, "primNext", nil) == vm.True {
					vmInst.Send(rows, "primAsDict", nil)
				}
			}
			vmInst.Send(rows, "primClose", nil)
		}()
		go func() {
			defer wg.Done()
			for i := 0; i < 5; i++ {
				if r := vmInst.Send(stmt, "primQuery", nil); !sqliteIsFailure(vmInst, r) {
					vmInst.Send(r, "primClose", nil)
				}
			}
			vmInst.Send(stmt, "primClose", nil)
		}()
	}
	wg.Wait()
}
