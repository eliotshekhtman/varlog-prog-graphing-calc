UNITS=authors eval main repl graphing
MLS_WITHOUT_MLIS=ast varLog
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) parser.cmo
MLIS=$(UNITS:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=oUnit,str,graphics,ANSITerminal,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) -pkg graphics $(OBJECTS)

test:
	BISECT_COVERAGE=NO $(OCAMLBUILD) -tag 'debug' $(TEST) -pkg graphics && ./$(TEST) -runner sequential

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

clean:
	ocamlbuild -clean

repl:
	$(OCAMLBUILD) repl.byte -pkg graphics && ./repl.byte

docs: 
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

zip:
	zip vΛrl0g.zip *.ml* * _tags Makefile *.vl .gitignore .merlin .ocamlinit