# MAKEFILE ecrit par Thomas Wickham, librement modifiable et redistribuable
# selon la GPLv2
#
# objectif1 : compiler du C et du Caml, et appeller le CAML depuis le C
# objectif2 : supporter ET le byecode ET le natif
# objectif3 : etre multi-plateforme
#
# /!\ XXX TODO XXX /!\
# AVANT TOUT PREMIER LANCEMENT LANCEZ configure.sh !

########
# Chemins
########
PWD := $(shell pwd)
SRC := $(PWD)/src
BIN := $(PWD)/bin


########
# Sources
#######

# executable name, don't put the extension inside
EXE := GogoPlayer

# ne pas ajouter l'extension (.cmx, .cmo, .a, etc.), separer par des espaces
LIBS :=
COMMON_FLAGS :=

CAML_ONLY :=
FLAGS_ML := $(COMMON_FLAGS)

CAML_CALLED_BY_C := \
   $(SRC)/init.ml
FLAGS_MLC := $(COMMON_FLAGS)

C_WRAPPERS := \
   $(SRC)/caml_func.c
FLAGS_CWRAP := $(COMMON_FLAGS)

C_ONLY := interface.c
FLAGS_C := $(COMMON_FLAGS)

########
# Configuration de build
########

#GENERATE_MLI := 1
# if not defined, will compile native exe
BYTECODE := 1
# if not defined, will NOT compile the mli,cmi,etc.
#COMPILE_INTERFACES := 1
# compile with opt ?
#OPT:=.opt

# files to remove after each build
#BTRASH :=
# files to remove at each 'make clean'
TRASH :=

INCDIRS :=
LIBDIRS :=
EXTLIBDIRS :=

#DEBUG :=
#ANNOTATE :=
#USE_CAMLP4 :=

#OCAML_LINK_FLAGS :=
#OCAML_BUILD_FLAGS :=
#OCAML_MKLIB_FLAGS :=
#YACC_FLAGS :=
#LEX_FLAGS :=

include Makefile.magic
include Makefile.ocaml
