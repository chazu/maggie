package vm

import (
	"sync"

	"github.com/chazu/yutani/pkg/client"
	pb "github.com/chazu/yutani/pkg/proto/yutani"
)

// ---------------------------------------------------------------------------
// Yutani Client Registry
// ---------------------------------------------------------------------------

// YutaniClientObject wraps a Yutani client for use in Smalltalk.
type YutaniClientObject struct {
	client       *client.Client
	eventChannel Value // Maggie Channel for receiving events
	eventProcess Value // Maggie Process running the event loop
	eventRunning bool
	mu           sync.Mutex
}

// yutaniClientRegistry stores active Yutani clients
var yutaniClientRegistry = make(map[uint32]*YutaniClientObject)
var yutaniClientRegistryMu sync.RWMutex
var nextYutaniClientID uint32 = 1

// Marker bits for Yutani resources (as specified in the plan)
const (
	yutaniClientMarker = 5 << 24 // 0x05000000
	yutaniWidgetMarker = 6 << 24 // 0x06000000
)

func yutaniClientToValue(id uint32) Value {
	return FromSymbolID(id | yutaniClientMarker)
}

func isYutaniClientValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == yutaniClientMarker
}

func getYutaniClient(v Value) *YutaniClientObject {
	if !isYutaniClientValue(v) {
		return nil
	}
	id := v.SymbolID() & ^uint32(0xFF<<24)

	yutaniClientRegistryMu.RLock()
	defer yutaniClientRegistryMu.RUnlock()
	return yutaniClientRegistry[id]
}

func registerYutaniClient(c *YutaniClientObject) Value {
	yutaniClientRegistryMu.Lock()
	defer yutaniClientRegistryMu.Unlock()

	id := nextYutaniClientID
	nextYutaniClientID++
	yutaniClientRegistry[id] = c

	return yutaniClientToValue(id)
}

func unregisterYutaniClient(v Value) {
	if !isYutaniClientValue(v) {
		return
	}
	id := v.SymbolID() & ^uint32(0xFF<<24)

	yutaniClientRegistryMu.Lock()
	defer yutaniClientRegistryMu.Unlock()
	delete(yutaniClientRegistry, id)
}

// ---------------------------------------------------------------------------
// Yutani Widget Registry
// ---------------------------------------------------------------------------

// YutaniWidgetObject wraps a Yutani widget for use in Smalltalk.
type YutaniWidgetObject struct {
	widget     client.Widget
	widgetType string // "list", "flex", "textView", "textArea", "button", etc.
	clientVal  Value  // Reference to the owning client
}

// yutaniWidgetRegistry stores active Yutani widgets
var yutaniWidgetRegistry = make(map[uint32]*YutaniWidgetObject)
var yutaniWidgetRegistryMu sync.RWMutex
var nextYutaniWidgetID uint32 = 1

func yutaniWidgetToValue(id uint32) Value {
	return FromSymbolID(id | yutaniWidgetMarker)
}

func isYutaniWidgetValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == yutaniWidgetMarker
}

func getYutaniWidget(v Value) *YutaniWidgetObject {
	if !isYutaniWidgetValue(v) {
		return nil
	}
	id := v.SymbolID() & ^uint32(0xFF<<24)

	yutaniWidgetRegistryMu.RLock()
	defer yutaniWidgetRegistryMu.RUnlock()
	return yutaniWidgetRegistry[id]
}

func registerYutaniWidget(w *YutaniWidgetObject) Value {
	yutaniWidgetRegistryMu.Lock()
	defer yutaniWidgetRegistryMu.Unlock()

	id := nextYutaniWidgetID
	nextYutaniWidgetID++
	yutaniWidgetRegistry[id] = w

	return yutaniWidgetToValue(id)
}

func unregisterYutaniWidget(v Value) {
	if !isYutaniWidgetValue(v) {
		return
	}
	id := v.SymbolID() & ^uint32(0xFF<<24)

	yutaniWidgetRegistryMu.Lock()
	defer yutaniWidgetRegistryMu.Unlock()
	delete(yutaniWidgetRegistry, id)
}

// ---------------------------------------------------------------------------
// Yutani Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniPrimitives() {
	// Create YClient and YWidget classes
	yClientClass := vm.createClass("YClient", vm.ObjectClass)
	yWidgetClass := vm.createClass("YWidget", vm.ObjectClass)

	// Register globals
	vm.Globals["YClient"] = vm.classValue(yClientClass)
	vm.Globals["YWidget"] = vm.classValue(yWidgetClass)

	// Phase 1: Foundation - Connection and Basic Widget Creation
	vm.registerYutaniConnectionPrimitives(yClientClass)
	vm.registerYutaniWidgetCreationPrimitives(yClientClass, yWidgetClass)

	// Phase 2: Properties and Layout
	vm.registerYutaniPropertyPrimitives(yClientClass, yWidgetClass)
	vm.registerYutaniLayoutPrimitives(yWidgetClass)

	// Phase 3: List Operations
	vm.registerYutaniListPrimitives(yWidgetClass)

	// Phase 4: Event Handling
	vm.registerYutaniEventPrimitives(yClientClass)

	// Phase 5: Additional Widget Operations
	vm.registerYutaniTextPrimitives(yWidgetClass)
}

// ---------------------------------------------------------------------------
// Phase 1: Foundation - Connection Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniConnectionPrimitives(yClientClass *Class) {
	// YClient class>>connectTo: address - connect to Yutani server
	yClientClass.AddClassMethod1(vm.Selectors, "primConnect:", func(_ interface{}, recv Value, addr Value) Value {
		addrStr := GetStringContent(addr)
		if addrStr == "" {
			return Nil
		}

		c, err := client.Connect(addrStr)
		if err != nil {
			return Nil
		}

		clientObj := &YutaniClientObject{
			client: c,
		}

		return registerYutaniClient(clientObj)
	})

	// YClient>>primClose - close connection
	yClientClass.AddMethod0(vm.Selectors, "primClose", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return recv
		}

		// Stop event loop if running
		clientObj.mu.Lock()
		clientObj.eventRunning = false
		clientObj.mu.Unlock()

		clientObj.client.Close()
		unregisterYutaniClient(recv)
		return recv
	})

	// YClient>>primIsConnected - check if connected
	yClientClass.AddMethod0(vm.Selectors, "primIsConnected", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return False
		}
		if clientObj.client.IsConnected() {
			return True
		}
		return False
	})

	// YClient>>primIsHealthy - check if connection is healthy
	yClientClass.AddMethod0(vm.Selectors, "primIsHealthy", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return False
		}
		if clientObj.client.IsHealthy() {
			return True
		}
		return False
	})
}

// ---------------------------------------------------------------------------
// Phase 1: Foundation - Widget Creation Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniWidgetCreationPrimitives(yClientClass *Class, yWidgetClass *Class) {
	// YClient>>primNewList - create a new list widget
	yClientClass.AddMethod0(vm.Selectors, "primNewList", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		list, err := clientObj.client.NewList().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     list,
			widgetType: "list",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewFlex - create a new flex container
	yClientClass.AddMethod0(vm.Selectors, "primNewFlex", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		flex, err := clientObj.client.NewFlex().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     flex,
			widgetType: "flex",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewTextView - create a new text view widget
	yClientClass.AddMethod0(vm.Selectors, "primNewTextView", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		textView, err := clientObj.client.NewTextView().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     textView,
			widgetType: "textView",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewTextArea - create a new text area widget
	yClientClass.AddMethod0(vm.Selectors, "primNewTextArea", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		textArea, err := clientObj.client.NewTextArea().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     textArea,
			widgetType: "textArea",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewButton - create a new button widget
	yClientClass.AddMethod0(vm.Selectors, "primNewButton", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		button, err := clientObj.client.NewButton().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     button,
			widgetType: "button",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewInputField - create a new input field widget
	yClientClass.AddMethod0(vm.Selectors, "primNewInputField", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		inputField, err := clientObj.client.NewInputField().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     inputField,
			widgetType: "inputField",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewGrid - create a new grid container
	yClientClass.AddMethod0(vm.Selectors, "primNewGrid", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		grid, err := clientObj.client.NewGrid().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     grid,
			widgetType: "grid",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewPages - create a new pages container
	yClientClass.AddMethod0(vm.Selectors, "primNewPages", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		pages, err := clientObj.client.NewPages().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     pages,
			widgetType: "pages",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewTable - create a new table widget
	yClientClass.AddMethod0(vm.Selectors, "primNewTable", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		table, err := clientObj.client.NewTable().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     table,
			widgetType: "table",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YClient>>primNewTreeView - create a new tree view widget
	yClientClass.AddMethod0(vm.Selectors, "primNewTreeView", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		tree, err := clientObj.client.NewTreeView().Build()
		if err != nil {
			return Nil
		}

		widgetObj := &YutaniWidgetObject{
			widget:     tree,
			widgetType: "treeView",
			clientVal:  recv,
		}
		return registerYutaniWidget(widgetObj)
	})

	// YWidget>>primDelete - delete widget
	yWidgetClass.AddMethod0(vm.Selectors, "primDelete", func(_ interface{}, recv Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return Nil
		}

		widgetObj.widget.Delete()
		unregisterYutaniWidget(recv)
		return Nil
	})

	// YWidget>>primWidgetType - return the widget type as a symbol
	yWidgetClass.AddMethod0(vm.Selectors, "primWidgetType", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return Nil
		}
		return v.Symbols.SymbolValue(widgetObj.widgetType)
	})
}

// ---------------------------------------------------------------------------
// Phase 2: Properties and Layout Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniPropertyPrimitives(yClientClass *Class, yWidgetClass *Class) {
	// YWidget>>primSetTitle: - set widget title
	yWidgetClass.AddMethod1(vm.Selectors, "primSetTitle:", func(_ interface{}, recv Value, title Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return recv
		}

		titleStr := GetStringContent(title)
		widgetObj.widget.SetTitle(titleStr)
		return recv
	})

	// YWidget>>primSetBorder: - set widget border
	yWidgetClass.AddMethod1(vm.Selectors, "primSetBorder:", func(_ interface{}, recv Value, border Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return recv
		}

		widgetObj.widget.SetBorder(border == True)
		return recv
	})

	// YWidget>>primSetFocus - set focus to this widget
	yWidgetClass.AddMethod0(vm.Selectors, "primSetFocus", func(_ interface{}, recv Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return recv
		}

		widgetObj.widget.SetFocus()
		return recv
	})

	// YWidget>>primWidgetId - get widget ID string
	yWidgetClass.AddMethod0(vm.Selectors, "primWidgetId", func(_ interface{}, recv Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return Nil
		}

		return NewStringValue(widgetObj.widget.ID())
	})

	// YClient>>primSetRoot: - set root widget
	yClientClass.AddMethod1(vm.Selectors, "primSetRoot:", func(_ interface{}, recv Value, widget Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return recv
		}

		widgetObj := getYutaniWidget(widget)
		if widgetObj == nil {
			return recv
		}

		clientObj.client.SetRoot(widgetObj.widget)
		return recv
	})

	// YClient>>primScreenSize - get screen size as Array(width, height)
	yClientClass.AddMethod0(vm.Selectors, "primScreenSize", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		width, height, err := clientObj.client.GetScreenSize()
		if err != nil {
			return Nil
		}

		return v.NewArrayWithElements([]Value{
			FromSmallInt(int64(width)),
			FromSmallInt(int64(height)),
		})
	})

	// YClient>>primClearScreen - clear the screen
	yClientClass.AddMethod0(vm.Selectors, "primClearScreen", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return recv
		}

		clientObj.client.ClearScreen()
		return recv
	})

	// YClient>>primSync - synchronize display
	yClientClass.AddMethod0(vm.Selectors, "primSync", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return recv
		}

		clientObj.client.Sync()
		return recv
	})
}

func (vm *VM) registerYutaniLayoutPrimitives(yWidgetClass *Class) {
	// YWidget>>primFlexAdd:proportion:fixed:focus: - add widget to flex container
	yWidgetClass.AddMethod4(vm.Selectors, "primFlexAdd:proportion:fixed:focus:", func(_ interface{}, recv Value, child Value, proportion Value, fixed Value, focus Value) Value {
		flexObj := getYutaniWidget(recv)
		if flexObj == nil || flexObj.widgetType != "flex" {
			return recv
		}

		childObj := getYutaniWidget(child)
		if childObj == nil {
			return recv
		}

		flex, ok := flexObj.widget.(*client.Flex)
		if !ok {
			return recv
		}

		prop := 0
		if proportion.IsSmallInt() {
			prop = int(proportion.SmallInt())
		}
		fixedSize := 0
		if fixed.IsSmallInt() {
			fixedSize = int(fixed.SmallInt())
		}

		flex.AddItem(childObj.widget, prop, fixedSize, focus == True)
		return recv
	})

	// YWidget>>primFlexRemove: - remove widget from flex container
	yWidgetClass.AddMethod1(vm.Selectors, "primFlexRemove:", func(_ interface{}, recv Value, child Value) Value {
		flexObj := getYutaniWidget(recv)
		if flexObj == nil || flexObj.widgetType != "flex" {
			return recv
		}

		childObj := getYutaniWidget(child)
		if childObj == nil {
			return recv
		}

		flex, ok := flexObj.widget.(*client.Flex)
		if !ok {
			return recv
		}

		flex.RemoveItem(childObj.widget)
		return recv
	})

	// YWidget>>primFlexDirection: - set flex direction (#row or #column)
	yWidgetClass.AddMethod1(vm.Selectors, "primFlexDirection:", func(vmPtr interface{}, recv Value, direction Value) Value {
		v := vmPtr.(*VM)
		flexObj := getYutaniWidget(recv)
		if flexObj == nil || flexObj.widgetType != "flex" {
			return recv
		}

		flex, ok := flexObj.widget.(*client.Flex)
		if !ok {
			return recv
		}

		if direction.IsSymbol() {
			dirName := v.Symbols.Name(direction.SymbolID())
			var dir pb.FlexDirection
			if dirName == "row" {
				dir = pb.FlexDirection_FLEX_ROW
			} else {
				dir = pb.FlexDirection_FLEX_COLUMN
			}
			flex.SetDirection(dir)
		}
		return recv
	})

	// Grid operations
	// YWidget>>primGridAdd:row:col:rowSpan:colSpan:minW:minH:focus: - add widget to grid
	yWidgetClass.AddMethod8(vm.Selectors, "primGridAdd:row:col:rowSpan:colSpan:minW:minH:focus:", func(_ interface{}, recv Value, child, row, col, rowSpan, colSpan, minW, minH, focus Value) Value {
		gridObj := getYutaniWidget(recv)
		if gridObj == nil || gridObj.widgetType != "grid" {
			return recv
		}

		childObj := getYutaniWidget(child)
		if childObj == nil {
			return recv
		}

		grid, ok := gridObj.widget.(*client.Grid)
		if !ok {
			return recv
		}

		r := 0
		if row.IsSmallInt() {
			r = int(row.SmallInt())
		}
		c := 0
		if col.IsSmallInt() {
			c = int(col.SmallInt())
		}
		rs := 1
		if rowSpan.IsSmallInt() {
			rs = int(rowSpan.SmallInt())
		}
		cs := 1
		if colSpan.IsSmallInt() {
			cs = int(colSpan.SmallInt())
		}
		mw := 0
		if minW.IsSmallInt() {
			mw = int(minW.SmallInt())
		}
		mh := 0
		if minH.IsSmallInt() {
			mh = int(minH.SmallInt())
		}

		grid.AddItem(childObj.widget, r, c, rs, cs, mw, mh, focus == True)
		return recv
	})

	// YWidget>>primGridRemove: - remove widget from grid
	yWidgetClass.AddMethod1(vm.Selectors, "primGridRemove:", func(_ interface{}, recv Value, child Value) Value {
		gridObj := getYutaniWidget(recv)
		if gridObj == nil || gridObj.widgetType != "grid" {
			return recv
		}

		childObj := getYutaniWidget(child)
		if childObj == nil {
			return recv
		}

		grid, ok := gridObj.widget.(*client.Grid)
		if !ok {
			return recv
		}

		grid.RemoveItem(childObj.widget)
		return recv
	})

	// Pages operations
	// YWidget>>primPagesAdd:name:resize:visible: - add page
	yWidgetClass.AddMethod4(vm.Selectors, "primPagesAdd:name:resize:visible:", func(_ interface{}, recv Value, child, name, resize, visible Value) Value {
		pagesObj := getYutaniWidget(recv)
		if pagesObj == nil || pagesObj.widgetType != "pages" {
			return recv
		}

		childObj := getYutaniWidget(child)
		if childObj == nil {
			return recv
		}

		pages, ok := pagesObj.widget.(*client.Pages)
		if !ok {
			return recv
		}

		nameStr := GetStringContent(name)
		pages.AddPage(nameStr, childObj.widget, resize == True, visible == True)
		return recv
	})

	// YWidget>>primPagesRemove: - remove page by name
	yWidgetClass.AddMethod1(vm.Selectors, "primPagesRemove:", func(_ interface{}, recv Value, name Value) Value {
		pagesObj := getYutaniWidget(recv)
		if pagesObj == nil || pagesObj.widgetType != "pages" {
			return recv
		}

		pages, ok := pagesObj.widget.(*client.Pages)
		if !ok {
			return recv
		}

		nameStr := GetStringContent(name)
		pages.RemovePage(nameStr)
		return recv
	})

	// YWidget>>primPagesShow: - show page by name
	yWidgetClass.AddMethod1(vm.Selectors, "primPagesShow:", func(_ interface{}, recv Value, name Value) Value {
		pagesObj := getYutaniWidget(recv)
		if pagesObj == nil || pagesObj.widgetType != "pages" {
			return recv
		}

		pages, ok := pagesObj.widget.(*client.Pages)
		if !ok {
			return recv
		}

		nameStr := GetStringContent(name)
		pages.ShowPage(nameStr)
		return recv
	})

	// YWidget>>primPagesCurrentPage - get current page name
	yWidgetClass.AddMethod0(vm.Selectors, "primPagesCurrentPage", func(_ interface{}, recv Value) Value {
		pagesObj := getYutaniWidget(recv)
		if pagesObj == nil || pagesObj.widgetType != "pages" {
			return Nil
		}

		pages, ok := pagesObj.widget.(*client.Pages)
		if !ok {
			return Nil
		}

		name, err := pages.GetCurrentPage()
		if err != nil {
			return Nil
		}
		return NewStringValue(name)
	})
}

// ---------------------------------------------------------------------------
// Phase 3: List Operations Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniListPrimitives(yWidgetClass *Class) {
	// YWidget>>primListAdd:secondary:shortcut: - add item to list
	yWidgetClass.AddMethod3(vm.Selectors, "primListAdd:secondary:shortcut:", func(_ interface{}, recv Value, main, secondary, shortcut Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return FromSmallInt(-1)
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return FromSmallInt(-1)
		}

		mainStr := GetStringContent(main)
		secStr := GetStringContent(secondary)
		var shortcutPtr *string
		if shortcut != Nil {
			s := GetStringContent(shortcut)
			shortcutPtr = &s
		}

		idx, err := list.AddItem(mainStr, secStr, shortcutPtr)
		if err != nil {
			return FromSmallInt(-1)
		}
		return FromSmallInt(int64(idx))
	})

	// YWidget>>primListRemove: - remove item at index
	yWidgetClass.AddMethod1(vm.Selectors, "primListRemove:", func(_ interface{}, recv Value, index Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return recv
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return recv
		}

		if !index.IsSmallInt() {
			return recv
		}
		idx := int(index.SmallInt())
		list.RemoveItem(idx)
		return recv
	})

	// YWidget>>primListClear - clear all items
	yWidgetClass.AddMethod0(vm.Selectors, "primListClear", func(_ interface{}, recv Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return recv
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return recv
		}

		list.Clear()
		return recv
	})

	// YWidget>>primListSelected - get selected index
	yWidgetClass.AddMethod0(vm.Selectors, "primListSelected", func(_ interface{}, recv Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return FromSmallInt(-1)
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return FromSmallInt(-1)
		}

		idx, err := list.GetSelected()
		if err != nil {
			return FromSmallInt(-1)
		}
		return FromSmallInt(int64(idx))
	})

	// YWidget>>primListSetSelected: - set selected index
	yWidgetClass.AddMethod1(vm.Selectors, "primListSetSelected:", func(_ interface{}, recv Value, index Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return recv
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return recv
		}

		if !index.IsSmallInt() {
			return recv
		}
		idx := int(index.SmallInt())
		list.SetSelected(idx)
		return recv
	})

	// YWidget>>primListCount - get item count
	yWidgetClass.AddMethod0(vm.Selectors, "primListCount", func(_ interface{}, recv Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return FromSmallInt(0)
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return FromSmallInt(0)
		}

		count, err := list.GetItemCount()
		if err != nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(count))
	})

	// YWidget>>primListItemAt: - get item at index (returns main text or nil)
	yWidgetClass.AddMethod1(vm.Selectors, "primListItemAt:", func(_ interface{}, recv Value, index Value) Value {
		listObj := getYutaniWidget(recv)
		if listObj == nil || listObj.widgetType != "list" {
			return Nil
		}

		list, ok := listObj.widget.(*client.List)
		if !ok {
			return Nil
		}

		if !index.IsSmallInt() {
			return Nil
		}
		idx := int(index.SmallInt())
		mainText, _, _, err := list.GetItem(idx)
		if err != nil {
			return Nil
		}
		return NewStringValue(mainText)
	})
}

// ---------------------------------------------------------------------------
// Phase 4: Event Handling Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniEventPrimitives(yClientClass *Class) {
	// YClient>>primStartEvents - start event stream, returns a Channel for receiving events
	yClientClass.AddMethod0(vm.Selectors, "primStartEvents", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		clientObj.mu.Lock()
		if clientObj.eventRunning {
			// Already running, return existing channel
			ch := clientObj.eventChannel
			clientObj.mu.Unlock()
			return ch
		}

		// Create a Maggie channel for events
		ch := createChannel(100) // Buffered channel
		chVal := registerChannel(ch)
		clientObj.eventChannel = chVal
		clientObj.eventRunning = true
		clientObj.mu.Unlock()

		// Start the Yutani event stream
		err := clientObj.client.StartEventStream()
		if err != nil {
			clientObj.mu.Lock()
			clientObj.eventRunning = false
			clientObj.mu.Unlock()
			return Nil
		}

		// Register event handler that converts events to Maggie values and sends to channel
		clientObj.client.OnEvent(func(event *client.Event) {
			clientObj.mu.Lock()
			running := clientObj.eventRunning
			clientObj.mu.Unlock()
			if !running {
				return
			}

			// Convert event to Maggie Dictionary or Array
			eventVal := v.convertYutaniEvent(event)

			// Try to send to channel (non-blocking)
			select {
			case ch.ch <- eventVal:
			default:
				// Channel full, drop event
			}
		})

		return chVal
	})

	// YClient>>primStopEvents - stop event stream
	yClientClass.AddMethod0(vm.Selectors, "primStopEvents", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return recv
		}

		clientObj.mu.Lock()
		clientObj.eventRunning = false
		// Close the channel
		if clientObj.eventChannel != Nil {
			ch := getChannel(clientObj.eventChannel)
			if ch != nil && !ch.closed.Load() {
				ch.closed.Store(true)
				close(ch.ch)
			}
		}
		clientObj.mu.Unlock()

		return recv
	})

	// YClient>>primEventChannel - get the event channel
	yClientClass.AddMethod0(vm.Selectors, "primEventChannel", func(_ interface{}, recv Value) Value {
		clientObj := getYutaniClient(recv)
		if clientObj == nil {
			return Nil
		}

		clientObj.mu.Lock()
		ch := clientObj.eventChannel
		clientObj.mu.Unlock()
		return ch
	})
}

// convertYutaniEvent converts a Yutani event to a Maggie Dictionary
func (vm *VM) convertYutaniEvent(event *client.Event) Value {
	dict := vm.NewDictionary()

	// Add event type
	var typeStr string
	switch event.Type {
	case client.EventTypeKey:
		typeStr = "key"
		if event.Key != nil {
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("key"), NewStringValue(event.Key.Key))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("rune"), FromSmallInt(int64(event.Key.Rune)))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("mod"), FromSmallInt(int64(event.Key.Mod)))
		}
	case client.EventTypeMouse:
		typeStr = "mouse"
		if event.Mouse != nil {
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("x"), FromSmallInt(int64(event.Mouse.X)))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("y"), FromSmallInt(int64(event.Mouse.Y)))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("button"), FromSmallInt(int64(event.Mouse.Button)))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("action"), NewStringValue(event.Mouse.Action))
		}
	case client.EventTypeResize:
		typeStr = "resize"
		if event.Resize != nil {
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("width"), FromSmallInt(int64(event.Resize.Width)))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("height"), FromSmallInt(int64(event.Resize.Height)))
		}
	case client.EventTypeFocus:
		typeStr = "focus"
		if event.Focus != nil {
			if event.Focus.Focused {
				vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("focused"), True)
			} else {
				vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("focused"), False)
			}
		}
	case client.EventTypeWidget:
		typeStr = "widget"
		if event.Widget != nil {
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("widgetId"), NewStringValue(event.Widget.WidgetID))
			vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("widgetEventType"), NewStringValue(event.Widget.Type))
			// Add data map if present
			if len(event.Widget.Data) > 0 {
				dataDict := vm.NewDictionary()
				for k, v := range event.Widget.Data {
					vm.DictionaryAtPut(dataDict, vm.Symbols.SymbolValue(k), NewStringValue(v))
				}
				vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("data"), dataDict)
			}
		}
	}

	vm.DictionaryAtPut(dict, vm.Symbols.SymbolValue("type"), vm.Symbols.SymbolValue(typeStr))
	return dict
}

// ---------------------------------------------------------------------------
// Phase 5: Text Widget Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerYutaniTextPrimitives(yWidgetClass *Class) {
	// YWidget>>primSetText: - set widget text (for TextView, TextArea, Button, InputField)
	yWidgetClass.AddMethod1(vm.Selectors, "primSetText:", func(_ interface{}, recv Value, text Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return recv
		}

		textStr := GetStringContent(text)

		switch widgetObj.widgetType {
		case "textView":
			if tv, ok := widgetObj.widget.(*client.TextView); ok {
				tv.SetText(textStr)
			}
		case "textArea":
			if ta, ok := widgetObj.widget.(*client.TextArea); ok {
				ta.SetText(textStr)
			}
		case "button":
			if btn, ok := widgetObj.widget.(*client.Button); ok {
				btn.SetLabel(textStr)
			}
		case "inputField":
			if inp, ok := widgetObj.widget.(*client.InputField); ok {
				inp.SetText(textStr)
			}
		}
		return recv
	})

	// YWidget>>primGetText - get widget text
	yWidgetClass.AddMethod0(vm.Selectors, "primGetText", func(_ interface{}, recv Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil {
			return Nil
		}

		var text string
		var err error

		switch widgetObj.widgetType {
		case "textView":
			if tv, ok := widgetObj.widget.(*client.TextView); ok {
				text, err = tv.GetText()
			}
		case "textArea":
			if ta, ok := widgetObj.widget.(*client.TextArea); ok {
				text, err = ta.GetText()
			}
		case "inputField":
			if inp, ok := widgetObj.widget.(*client.InputField); ok {
				text, err = inp.GetText()
			}
		default:
			return Nil
		}

		if err != nil {
			return Nil
		}
		return NewStringValue(text)
	})

	// YWidget>>primSetLabel: - set button label
	yWidgetClass.AddMethod1(vm.Selectors, "primSetLabel:", func(_ interface{}, recv Value, label Value) Value {
		widgetObj := getYutaniWidget(recv)
		if widgetObj == nil || widgetObj.widgetType != "button" {
			return recv
		}

		btn, ok := widgetObj.widget.(*client.Button)
		if !ok {
			return recv
		}

		labelStr := GetStringContent(label)
		btn.SetLabel(labelStr)
		return recv
	})

	// Table operations
	// YWidget>>primTableSetCell:row:col: - set table cell content
	yWidgetClass.AddMethod3(vm.Selectors, "primTableSetCell:row:col:", func(_ interface{}, recv Value, content, row, col Value) Value {
		tableObj := getYutaniWidget(recv)
		if tableObj == nil || tableObj.widgetType != "table" {
			return recv
		}

		table, ok := tableObj.widget.(*client.Table)
		if !ok {
			return recv
		}

		contentStr := GetStringContent(content)
		r := 0
		if row.IsSmallInt() {
			r = int(row.SmallInt())
		}
		c := 0
		if col.IsSmallInt() {
			c = int(col.SmallInt())
		}

		table.SetCell(r, c, client.NewTableCell(contentStr))
		return recv
	})

	// YWidget>>primTableGetCellRow:col: - get table cell content
	yWidgetClass.AddMethod2(vm.Selectors, "primTableGetCellRow:col:", func(_ interface{}, recv Value, row, col Value) Value {
		tableObj := getYutaniWidget(recv)
		if tableObj == nil || tableObj.widgetType != "table" {
			return Nil
		}

		table, ok := tableObj.widget.(*client.Table)
		if !ok {
			return Nil
		}

		r := 0
		if row.IsSmallInt() {
			r = int(row.SmallInt())
		}
		c := 0
		if col.IsSmallInt() {
			c = int(col.SmallInt())
		}

		cell, err := table.GetCell(r, c)
		if err != nil || cell == nil {
			return Nil
		}
		return NewStringValue(cell.Text)
	})

	// YWidget>>primTableClear - clear table
	yWidgetClass.AddMethod0(vm.Selectors, "primTableClear", func(_ interface{}, recv Value) Value {
		tableObj := getYutaniWidget(recv)
		if tableObj == nil || tableObj.widgetType != "table" {
			return recv
		}

		table, ok := tableObj.widget.(*client.Table)
		if !ok {
			return recv
		}

		table.Clear()
		return recv
	})

	// YWidget>>primTableGetSelection - get selected row and column as Array(row, col)
	yWidgetClass.AddMethod0(vm.Selectors, "primTableGetSelection", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		tableObj := getYutaniWidget(recv)
		if tableObj == nil || tableObj.widgetType != "table" {
			return Nil
		}

		table, ok := tableObj.widget.(*client.Table)
		if !ok {
			return Nil
		}

		row, col, err := table.GetSelection()
		if err != nil {
			return Nil
		}
		return v.NewArrayWithElements([]Value{FromSmallInt(int64(row)), FromSmallInt(int64(col))})
	})

	// YWidget>>primTableSetSelectionRow:col: - set selected cell
	yWidgetClass.AddMethod2(vm.Selectors, "primTableSetSelectionRow:col:", func(_ interface{}, recv Value, row, col Value) Value {
		tableObj := getYutaniWidget(recv)
		if tableObj == nil || tableObj.widgetType != "table" {
			return recv
		}

		table, ok := tableObj.widget.(*client.Table)
		if !ok {
			return recv
		}

		r := 0
		if row.IsSmallInt() {
			r = int(row.SmallInt())
		}
		c := 0
		if col.IsSmallInt() {
			c = int(col.SmallInt())
		}

		table.SetSelection(r, c)
		return recv
	})

	// TreeView operations
	// YWidget>>primTreeSetRoot: - set root node with text
	yWidgetClass.AddMethod1(vm.Selectors, "primTreeSetRoot:", func(_ interface{}, recv Value, text Value) Value {
		treeObj := getYutaniWidget(recv)
		if treeObj == nil || treeObj.widgetType != "treeView" {
			return Nil
		}

		tree, ok := treeObj.widget.(*client.TreeView)
		if !ok {
			return Nil
		}

		textStr := GetStringContent(text)
		nodeID, err := tree.SetRoot(client.NewTreeNode(textStr))
		if err != nil {
			return Nil
		}
		// Return the node ID as a string
		return NewStringValue(nodeID.Id)
	})

	// YWidget>>primTreeGetSelected - get selected node info as Dictionary
	yWidgetClass.AddMethod0(vm.Selectors, "primTreeGetSelected", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		treeObj := getYutaniWidget(recv)
		if treeObj == nil || treeObj.widgetType != "treeView" {
			return Nil
		}

		tree, ok := treeObj.widget.(*client.TreeView)
		if !ok {
			return Nil
		}

		nodeID, text, reference, err := tree.GetSelected()
		if err != nil || nodeID == nil {
			return Nil
		}

		dict := v.NewDictionary()
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("nodeId"), NewStringValue(nodeID.Id))
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("text"), NewStringValue(text))
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("reference"), NewStringValue(reference))
		return dict
	})
}
