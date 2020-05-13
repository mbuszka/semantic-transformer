.PHONY: all clean test build

all:
	@echo "clean 		- clean generated interpreters"
	@echo "test			- run self-test on all files in interpreters/src"

clean:
	rm -rf interpreters/out

test: clean
	cabal run semt-test