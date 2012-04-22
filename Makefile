all:
	ocamlbuild -pkg batteries -I src main.native -use-ocamlfind

bytecode:
	ocamlbuild -pkg batteries -I src main.byte -use-ocamlfind

debug:
	ocamlbuild -pkg batteries -I src main.d.byte -use-ocamlfind