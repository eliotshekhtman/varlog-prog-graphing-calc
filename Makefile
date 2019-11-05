UNITS=authors ast main repl astLang graphing
MLS_WITHOUT_MLIS=ast 
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) parser.cmo
MLIS=$(UNITS:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,str,graphics

default: build
	utop

build:
	$(OCAMLBUILD) -pkg graphics $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean

repl:
	$(OCAMLBUILD) repl.byte -pkg graphics && ./repl.byte