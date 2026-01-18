# Makefile for libtrashtalk shared runtime

# Determine shared library extension
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    LIB_EXT := dylib
    LDFLAGS := -ldflags="-s -w"
else
    LIB_EXT := so
    LDFLAGS := -ldflags="-s -w"
endif

# Output paths
LIB_NAME := libtrashtalk.$(LIB_EXT)
BUILD_DIR := ../../bin
LIB_PATH := $(BUILD_DIR)/$(LIB_NAME)

# Go build settings
GO := go
GOFLAGS := -buildmode=c-shared

.PHONY: all clean install test

all: $(LIB_PATH)

$(LIB_PATH): *.go
	@mkdir -p $(BUILD_DIR)
	$(GO) build $(GOFLAGS) $(LDFLAGS) -o $(LIB_PATH) .
	@echo "Built $(LIB_PATH)"
	@# Copy header to build dir
	cp libtrashtalk.h $(BUILD_DIR)/

clean:
	rm -f $(LIB_PATH)
	rm -f $(BUILD_DIR)/libtrashtalk.h

install: $(LIB_PATH)
	@# Install to standard locations
	@mkdir -p ~/.trashtalk/lib
	cp $(LIB_PATH) ~/.trashtalk/lib/
	cp libtrashtalk.h ~/.trashtalk/lib/
	@echo "Installed to ~/.trashtalk/lib/"

test:
	$(GO) test -v ./...

# Build for debugging (with symbols)
debug:
	@mkdir -p $(BUILD_DIR)
	$(GO) build $(GOFLAGS) -o $(LIB_PATH) .
	@echo "Built $(LIB_PATH) (debug)"
