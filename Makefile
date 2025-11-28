# Makefile -- automate some simple stack functions
.PHONY: build clean test release 

RELEASE_DIR=release
VERSION=$(shell git describe --tags --always)
FILES=warp-bin warp.sh

# Build the project
build: 
	stack build 

# remove .stack-work
clean: 
	stack clean
	rm -rf $(RELEASE_DIR)/*.tar.gz

# Run test suite -- currently unimplemented
test: 
	stack test

release: build 
	mkdir -p release 
	BIN=$$(stack path --local-install-root)/bin/warp-bin ; \
		cp $$BIN release/warp-bin
	cp scripts/warp.sh release/warp.sh
	tar -czvf $(RELEASE_DIR)/$(VERSION).tar.gz -C $(RELEASE_DIR) $(FILES)
	@echo "Release package created at $(RELEASE_DIR)/$(VERSION).tar.gz"
	# chmod +x release/warp.sh
	# echo "Release prepared in ./release"
