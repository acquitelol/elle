compile: main
	clear && ./dist/main

main: main.s
	cc -o ./dist/main ./dist/main.s

main.s: main.ssa
	qbe -o ./dist/main.s ./dist/main.ssa

main.ssa: compiler
	./dist/compiler ./dist/main.elle

compiler:
	rustc -o ./dist/compiler ./src/main.rs