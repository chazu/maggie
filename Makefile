.PHONY: all bootstrap mag clean test test-ide test-all

# Default target builds everything
all: mag

# Build the bootstrap tool and generate maggie.image
bootstrap:
	go run ./cmd/bootstrap/

# Copy image to mag directory and build the mag binary
mag: bootstrap
	cp maggie.image cmd/mag/
	go build -o mag ./cmd/mag/

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
