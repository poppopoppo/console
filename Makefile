all:
console.exe: console.ml dune
	dune build console.exe
state.o:
parser.o:
lexer.o:

run:
	_build/default/console.exe
