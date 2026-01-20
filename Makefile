.PHONY: all bootstrap mag clean test

# Default target builds everything
all: mag

# Build the bootstrap tool and generate maggie.image
bootstrap:
	go run ./cmd/bootstrap/ --new-syntax

# Copy image to mag directory and build the mag binary
mag: bootstrap
	cp maggie.image cmd/mag/
	go build -o mag ./cmd/mag/

# Run all tests
test:
	go test ./...

# Clean build artifacts
clean:
	rm -f mag maggie.image cmd/mag/maggie.image
