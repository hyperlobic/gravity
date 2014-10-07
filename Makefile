all: stuff

stuff: *.ml
	ocamlfind ocamlopt -thread -o gravity graphics.cmxa unix.cmxa threads.cmxa vector.ml gravity.ml 
