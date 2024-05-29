run:
	./dist/compiler ./dist/main.elle > /dev/null
	qbe -o ./dist/main.s ./dist/main.ssa
	node main.js ./dist/main.s > ./dist/main.s.new
	mv ./dist/main.s.new ./dist/main.s
	cc -o ./dist/main ./dist/main.s
	clear
	@./dist/main

run-donut:
	./dist/compiler ./dist/donut.elle > /dev/null
	qbe -o ./dist/donut.s ./dist/donut.ssa
	node main.js ./dist/donut.s > ./dist/donut.s.new
	mv ./dist/donut.s.new ./dist/donut.s
	cc -o ./dist/donut ./dist/donut.s
	clear
	@./dist/donut

all: main.s
	cc -o ./dist/main ./dist/main.s

main.s: main.ssa
	qbe -o ./dist/main.s ./dist/main.ssa

main.ssa: compiler
	./dist/compiler ./dist/main.elle
	qbe -o ./dist/main.s ./dist/main.ssa
	cc -o ./dist/main ./dist/main.s

compile:
	rustc -o ./dist/compiler ./src/main.rs
