FLAGS := -use-ocamlfind

# A DECOMMENTER POUR AUGMENTER LA VITESSE
#FLAGS := $(FLAGS) -noassert


INC := -I src -I src/building -I src/talking -I src/thinking
PKG := -pkg batteries

CC := ocamlbuild
COMPILE := $(CC) $(FLAGS) $(PKG) $(INC)

TEST_FLAGS := -pkg oUnit -I test

all: speed_test
exe: native

native:
	$(COMPILE) main.native
bytecode:
	$(COMPILE) main.byte
debug:
	$(COMPILE) main.d.byte

speed_test: native
	@echo "****************************************"
	$(COMPILE) $(TEST_FLAGS) testing.native
	@echo "Beginning speed tests..."
	./testing.native
	@echo
	@echo "****************************************"

test: speed_test
	@echo "****************************************"
	$(COMPILE) $(TEST_FLAGS) long_testing.native
	@echo "Beginning long running test..."
	@echo "****************************************"

clean:
	ocamlbuild -clean
