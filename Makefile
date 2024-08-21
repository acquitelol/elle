DIST_PATH = /usr/local
BIN_PATH = $(DIST_PATH)/bin/ellec
STD_PATH = $(DIST_PATH)/include/elle/std/

# !! Requires root !!
default: install-release

install-debug: install-compiler-debug install-std
install-release: install-compiler-release install-std

install-compiler-debug: compile-debug
	mkdir -p $(DIST_PATH)
	mv ./ellec $(BIN_PATH)

install-compiler-release: compile-release
	mkdir -p $(DIST_PATH)
	mv ./ellec $(BIN_PATH)

install-std:
	mkdir -p $(STD_PATH)
	cp -rf std/* $(STD_PATH)

compile-debug:
	cargo build && mv ./target/debug/ellec ./ellec

compile-release:
	cargo build --release && mv ./target/release/ellec ./ellec

clean:
	rm -rf dist
	@make compile-release
