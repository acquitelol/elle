DIST_PATH = $(HOME)/.local
BIN_PATH = $(DIST_PATH)/bin/
STD_PATH = $(DIST_PATH)/include/elle/std
RUNTIME_PATH = $(DIST_PATH)/lib

default: install-release

install-debug: install-compiler-debug install-std install-runtime
install-release: install-compiler-release install-std install-runtime

install-compiler-debug: compile-debug
	mkdir -p $(BIN_PATH)
	mv ./ellec $(BIN_PATH)ellec

install-compiler-release: compile-release
	mkdir -p $(BIN_PATH)
	mv ./ellec $(BIN_PATH)ellec

install-std:
	rm -rf $(STD_PATH)
	mkdir -p $(STD_PATH)
	cp -rf std/* $(STD_PATH)

install-runtime:
	@rm -f $(RUNTIME_PATH)/libelle.o
	@rm -f $(RUNTIME_PATH)/libelle.a
	mkdir -p $(RUNTIME_PATH)
	# must be compiled without anything because this is the module creating it
	# its fine because those modules are actually just headers anyway
	# this is just so the headers dont overwrite the implementation in the stdlib
	ellec $(STD_PATH)/runtime/index.le -o libelle.o -c --noalloc --nogc --nosm --nofmt --nostd
	@ar -rcs $(RUNTIME_PATH)/libelle.a libelle.o
	@rm -f libelle.o

compile-debug:
	cargo build && mv ./target/debug/ellec ./ellec

compile-release:
	cargo build --release && mv ./target/release/ellec ./ellec

.PHONY: test
test:
	pypy3 tools/examples.py all

repl:
	ellec tools/repl.le

clean:
	rm -rf dist
	rm repl
	@make compile-release
