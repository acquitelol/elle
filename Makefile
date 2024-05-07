run:
	./dist/compiler ./dist/main.elle > /dev/null
	qbe -o ./dist/main.s ./dist/main.ssa
	cc -o ./dist/main ./dist/main.s
	clear
	@./dist/main

all: main.s
	cc -o ./dist/main ./dist/main.s

main.s: main.ssa
	qbe -o ./dist/main.s ./dist/main.ssa

main.ssa: compiler
	./dist/compiler ./dist/main.elle
	qbe -o ./dist/main.s ./dist/main.ssa
	cc -o ./dist/main ./dist/main.s

compiler:
	rustc -o ./dist/compiler ./src/main.rs
