.PHONY: all bootstrap mag install clean test test-ide test-all proto serve

# Default target builds everything
all: mag

# Build the bootstrap tool and generate maggie.image
bootstrap:
	go run ./cmd/bootstrap/

# Copy image to mag directory and build the mag binary
mag: bootstrap
	cp maggie.image cmd/mag/
	go build -o mag ./cmd/mag/
	codesign -s - mag

# Install the mag binary into GOBIN (or GOPATH/bin)
install: mag
	cp mag $(shell go env GOBIN 2>/dev/null || echo "$(shell go env GOPATH)/bin")/mag

# Generate Go code from protobuf definitions
proto:
	cd proto && buf generate

# Start the language server on port 4567
serve: mag
	./mag --serve --port 4567

# Push docs to pgs.sh
deploy-docs:
	rsync -rv site/ pgs.sh:/maggie/
# Run Go unit tests
test:
	go test ./...

# Run headless IDE integration tests (requires yutani binary in PATH)
test-ide: mag
	bash test/ide/run_all.sh

# Run all tests (Go unit + IDE integration)
test-all: test test-ide

# Clean build artifacts
clean:
	rm -f mag maggie.image cmd/mag/maggie.image
