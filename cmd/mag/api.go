package main

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/chazu/maggie/vm"
)

// handleApiCommand emits a compact, machine-readable index of the class/selector
// surface — built for an AI agent (or a newcomer) that needs the API without
// loading ~11k lines of tutorial prose. Usage:
//
//	mag api            compact text index of core classes
//	mag api --json     the same as JSON
//	mag api --all      include tutorial (GuideNN) and IDE (Yutani) classes
//	mag api Array      just one class (matches by short or full name)
func handleApiCommand(vmInst *vm.VM, args []string) int {
	asJSON := false
	showAll := false
	var only string
	for _, a := range args {
		switch a {
		case "--json":
			asJSON = true
		case "--all":
			showAll = true
		default:
			if !strings.HasPrefix(a, "-") {
				only = a
			}
		}
	}

	classes := vmInst.Classes.All()
	sort.Slice(classes, func(i, j int) bool { return apiClassName(classes[i]) < apiClassName(classes[j]) })

	var out []apiClass
	for _, cls := range classes {
		if only != "" {
			if cls.Name != only && apiClassName(cls) != only {
				continue
			}
		} else if !showAll && isNonCoreHelpClass(cls) {
			continue
		}
		ac := apiClass{
			Name:  apiClassName(cls),
			Doc:   firstLine(cls.DocString),
			Super: superName(cls),
		}
		if cls.VTable != nil {
			ac.Instance = apiSelectors(cls.VTable, vmInst.Selectors)
		}
		if cls.ClassVTable != nil {
			ac.Class = apiSelectors(cls.ClassVTable, vmInst.Selectors)
		}
		out = append(out, ac)
	}

	if asJSON {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(out); err != nil {
			fmt.Fprintf(os.Stderr, "api: %v\n", err)
			return 1
		}
		return 0
	}

	fmt.Printf("# Maggie API — %d classes. `mag api --json` for structured output, `mag api <Class>` for one class.\n\n", len(out))
	for _, ac := range out {
		header := ac.Name
		if ac.Super != "" {
			header += " < " + ac.Super
		}
		fmt.Printf("## %s\n", header)
		if ac.Doc != "" {
			fmt.Printf("%s\n", ac.Doc)
		}
		for _, m := range ac.Instance {
			printAPISelector(m, "")
		}
		for _, m := range ac.Class {
			printAPISelector(m, "class ")
		}
		fmt.Println()
	}
	return 0
}

type apiSelector struct {
	Selector string `json:"selector"`
	Doc      string `json:"doc,omitempty"`
}

type apiClass struct {
	Name     string        `json:"name"`
	Super    string        `json:"super,omitempty"`
	Doc      string        `json:"doc,omitempty"`
	Instance []apiSelector `json:"instance,omitempty"`
	Class    []apiSelector `json:"class,omitempty"`
}

func printAPISelector(m apiSelector, prefix string) {
	if m.Doc != "" {
		fmt.Printf("  %s%-28s %s\n", prefix, m.Selector, m.Doc)
	} else {
		fmt.Printf("  %s%s\n", prefix, m.Selector)
	}
}

func apiSelectors(vt *vm.VTable, selectors *vm.SelectorTable) []apiSelector {
	local := vt.LocalMethods()
	result := make([]apiSelector, 0, len(local))
	for selectorID, method := range local {
		name := selectors.Name(selectorID)
		if name == "" {
			continue
		}
		result = append(result, apiSelector{Selector: name, Doc: firstLine(vm.MethodDocString(method))})
	}
	sort.Slice(result, func(i, j int) bool { return result[i].Selector < result[j].Selector })
	return result
}

func apiClassName(cls *vm.Class) string {
	if cls.Namespace != "" {
		return cls.Namespace + "::" + cls.Name
	}
	return cls.Name
}

func superName(cls *vm.Class) string {
	if cls.Superclass == nil {
		return ""
	}
	return apiClassName(cls.Superclass)
}

// firstLine returns the first non-empty line of a docstring, trimmed and
// bounded so the index stays compact.
func firstLine(s string) string {
	for _, line := range strings.Split(s, "\n") {
		line = strings.TrimSpace(strings.Trim(line, `"`))
		if line == "" {
			continue
		}
		if len(line) > 80 {
			line = line[:77] + "..."
		}
		return line
	}
	return ""
}
