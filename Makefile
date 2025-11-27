# Makefile -- automate some simple stack functions
.PHONY: build clean test release 

# Build the project
build: 
	stack build 

# remove .stack-work
clean: 
	stack clean

# Run test suite -- currently unimplemented
test: 
	stack test

release: build 
	mkdir -p release 
	BIN=$$(stack path --local-install-root)/bin/warp-bin ; \
		cp $$BIN release/warp-bin
	cp scripts/warp.sh release/warp.sh
	chmod +x release/warp.sh
	echo "Release prepared in ./release"
