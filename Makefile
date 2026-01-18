.PHONY: build test clean install

# Build the procyon binary
build:
	go build -o procyon ./cmd/procyon

# Run all tests
test:
	go test ./...

# Run tests with verbose output
test-verbose:
	go test -v ./...

# Clean build artifacts
clean:
	rm -f procyon
	go clean

# Install to GOPATH/bin
install:
	go install ./cmd/procyon

# Generate Counter.native from trashtalk and test it
test-counter:
	@echo "Generating Counter..."
	@mkdir -p /tmp/procyon-counter
	@cd ~/.trashtalk/lib/jq-compiler && ./driver.bash parse ~/.trashtalk/trash/Counter.trash 2>/dev/null | $(CURDIR)/procyon > /tmp/procyon-counter/main.go 2>/dev/stderr
	@cp ~/.trashtalk/trash/Counter.trash /tmp/procyon-counter/
	@echo "Building..."
	@cd /tmp/procyon-counter && go mod init test 2>/dev/null || true && go get github.com/mattn/go-sqlite3 && go build -o Counter.native .
	@echo "Testing..."
	@/tmp/procyon-counter/Counter.native --info
	@echo "Done!"

# Update testdata from current trashtalk
update-testdata:
	cd ~/.trashtalk/lib/jq-compiler && ./driver.bash parse ~/.trashtalk/trash/Counter.trash 2>/dev/null > $(CURDIR)/testdata/counter/input.json
