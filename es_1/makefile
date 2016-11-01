all:
	ocamlc -c environment.ml
	ocamlc -c syntax.ml
	ocamlc -c utils.ml
	ocamlc -c interpreter.ml

test:
	ocamlc -c test.ml
	ocamlc -o run environment.cmo utils.cmo syntax.cmo interpreter.cmo test.cmo
	ocamlrun run

