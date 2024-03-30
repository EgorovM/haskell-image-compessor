# Makefile for the Image Compressor project using Stack

# Project name
PROJECT = ImageCompressor

# Build the project using Stack
build:
	stack build

# Copy the binary to the project root
install: build
	stack install --local-bin-path .

clean:
	stack clean

# Phony targets
.PHONY: build install clean