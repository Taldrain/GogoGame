INC := -I src -I src/building -I src/talking -I src/thinking

all:
	ocamlbuild -pkg batteries $(INC) main.native -use-ocamlfind

bytecode:
	ocamlbuild -pkg batteries $(INC) main.byte -use-ocamlfind

debug:
	ocamlbuild -pkg batteries $(INC) main.d.byte -use-ocamlfind

clean:
	ocamlbuild -pkg batteries $(INC) main.d.byte -use-ocamlfind -clean