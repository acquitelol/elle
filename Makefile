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
ifeq ("$(RUN_ARGS)","ball")
	cc -lraylib -o $@ $<
else
	cc -o $@ $<
endif

$(ASM_PATH): $(TMP_PATH)/out.tmp3.s
	mv $< $@
	rm -rf $(TMP_PATH)

# Fix codegen issue when taking size for a buffer as an argument
$(TMP_PATH)/out.tmp3.s: $(TMP_PATH)/out.tmp2.s
	sed -E 's/and\t(.*), #(.*), lsl (.*)/lsl\t\1, \3\n\tand\t\1, #\2/g' $< > $@

# Fix codegen issue for generating floating point numbers
$(TMP_PATH)/out.tmp2.s: $(TMP_PATH)/out.tmp1.s
	sed -E 's/Lfp([0-9]+):/_Lfp\1:/g' $< > $@

$(TMP_PATH)/out.tmp1.s: $(IR_PATH)
	mkdir -p $(TMP_PATH)
	qbe -o $@ $<

$(IR_PATH): $(EXAMPLES_PATH)/$(RUN_ARGS).l dist/ellec
	rm -rf $(DIST_PATH)
	mkdir -p $(DIST_PATH)
	dist/ellec $< $@

.PHONY: run
run: $(EXEC_PATH)
	clear
	@$(EXEC_PATH) "you can pass arguments to Elle programs!" ":3"

all:
	@$(foreach file, \
	    $(filter-out $(EXAMPLES_PATH)/donut%, $(wildcard $(EXAMPLES_PATH)/*)), \
		make run $$(echo "$(file)" | sed 's|examples/\(.*\)\.l|\1|') \
		$$(sleep 1) \
	;)

compile:
	mkdir -p dist
	rustc -o dist/ellec src/main.rs

compile-release:
	mkdir -p dist
	rustc -o dist/ellec -O src/main.rs

clean:
	rm -rf dist
	@make compile
