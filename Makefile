FLAGS := -use-ocamlfind
INC := -I src -I src/building -I src/talking -I src/thinking
PKG := -pkg batteries

CC := ocamlbuild
COMPILE := $(CC) $(FLAGS) $(PKG) $(INC)

TEST_FLAGS := -pkg oUnit -I test

all: speed_test

native:
	$(COMPILE) main.native
bytecode:
	$(COMPILE) main.byte
debug:
	$(COMPILE) main.d.byte

speed_test: native
	@echo "****************************************"
	@echo "Beginning speed tests..."
	$(COMPILE) $(TEST_FLAGS) testing.native
	./testing.native
	@echo
	@echo "****************************************"

test: speed_test
	@echo "****************************************"
	@echo "Beginning long running test..."
	$(COMPILE) $(TEST_FLAGS) long_testing.native
	@echo "****************************************"

clean:
	ocamlbuild -clean