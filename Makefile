ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif

.PHONY: run
run:
	./dist/compiler ./examples/$(RUN_ARGS).elle ./dist/$(RUN_ARGS).ssa > /dev/null
	qbe -o ./dist/$(RUN_ARGS).s ./dist/$(RUN_ARGS).ssa
	node main.js ./dist/$(RUN_ARGS).s > ./dist/$(RUN_ARGS).s.new
	mv ./dist/$(RUN_ARGS).s.new ./dist/$(RUN_ARGS).s
	cc -o ./dist/$(RUN_ARGS) ./dist/$(RUN_ARGS).s
	clear
	@./dist/$(RUN_ARGS)

compile:
	rustc -o ./dist/compiler ./src/main.rs
