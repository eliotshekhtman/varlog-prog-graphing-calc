UNITS=authors ast evalexpr evallang main main_lang repl graphing
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
	$(OCAMLBUILD) -tag 'debug' $(TEST) -pkg graphics && ./$(TEST)

clean:
	ocamlbuild -clean

repl:
	$(OCAMLBUILD) repl.byte -pkg graphics && ./repl.byte

zip:
	zip vΛrl0g.zip *.ml* * _tags Makefile

# docs: docs-public docs-private
	
# docs-public: build
# 	mkdir -p doc.public
# 	ocamlfind ocamldoc -I _build -package $(PKGS) \
# 		-html -stars -d doc.public $(MLIS)

# docs-private: build
# 	mkdir -p doc.private
# 	ocamlfind ocamldoc -I _build -package $(PKGS) \
# 		-html -stars -d doc.private \
# 		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)