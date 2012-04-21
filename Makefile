all:
	ocamlbuild -pkg batteries -I src main.native -use-ocamlfind