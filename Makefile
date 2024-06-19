ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif

EXAMPLES_PATH = examples
DIST_PATH = dist/$(RUN_ARGS)
IR_PATH = $(DIST_PATH)/target.ssa
TMP_PATH = $(DIST_PATH)/tmp
ASM_PATH = $(DIST_PATH)/out.s
EXEC_PATH = $(DIST_PATH)/build

run: $(EXEC_PATH)

$(EXEC_PATH): $(ASM_PATH)
	cc -o $@ $<

$(ASM_PATH): $(TMP_PATH)/out.new.s
	mv $< $@
	rm -rf $(TMP_PATH)

$(TMP_PATH)/out.new.s: $(TMP_PATH)/out.tmp.s
	sed -E 's/Lfp([0-9]+):/_Lfp\1:/g' $< > $@

$(TMP_PATH)/out.tmp.s: $(IR_PATH)
	mkdir -p $(TMP_PATH)
	qbe -o $@ $<

$(IR_PATH): $(EXAMPLES_PATH)/$(RUN_ARGS).elle dist/compiler
	rm -rf $(DIST_PATH)
	mkdir -p $(DIST_PATH)
	dist/compiler $< $@ > /dev/null

.PHONY: run
run:
	clear
	@$(EXEC_PATH) "you can pass arguments to Elle programs!" ":3"

all:
	@$(foreach file, \
	    $(filter-out $(EXAMPLES_PATH)/donut%, $(wildcard $(EXAMPLES_PATH)/*)), \
		make run $$(echo "$(file)" | sed 's|examples/\(.*\)\.elle|\1|') \
		$$(sleep 1) \
	;)

compile:
	mkdir -p dist
	rustc -o dist/compiler src/main.rs
