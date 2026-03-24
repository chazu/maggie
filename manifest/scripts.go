package manifest

import (
	"fmt"
	"os"
	"os/exec"
)

// RunScript runs a lifecycle script if non-empty.
// name is used for logging (e.g., "prebuild").
func RunScript(name, script, dir string, verbose bool) error {
	if script == "" {
		return nil
	}
	if verbose {
		fmt.Printf("Running %s: %s\n", name, script)
	}
	cmd := exec.Command("sh", "-c", script)
	cmd.Dir = dir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s script failed: %w", name, err)
	}
	return nil
}
