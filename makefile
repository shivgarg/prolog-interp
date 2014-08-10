all:
	ocamlyacc plgparser.mly
	ocamlc -c plgparser.mli
	ocamllex plglex.mll
	ocamlc -c plglex.ml
	ocamlc -c plgparser.ml
	ocamlc -c main.ml
	ocamlc -o parser plglex.cmo plgparser.cmo main.cmo
