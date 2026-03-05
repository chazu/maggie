# Multi-stage build for the Maggie VM
# The maggie.image is embedded via go:embed, so it is baked into the binary.

# --- Builder stage ---
FROM golang:1.24 AS builder

WORKDIR /build

# Cache dependency downloads
COPY go.mod go.sum ./
RUN go mod download

# Copy source tree
COPY . .

# Build the mag binary (pure Go, no CGO)
RUN CGO_ENABLED=0 go build -o /mag ./cmd/mag/

# --- Runtime stage ---
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /mag /usr/local/bin/mag

# Copy the standard library for runtime fileIn use
COPY lib/ /app/lib/

WORKDIR /app

ENTRYPOINT ["/usr/local/bin/mag"]
