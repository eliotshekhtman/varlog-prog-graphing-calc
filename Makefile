default: build
	utop

build:
	ocamlbuild -use-ocamlfind main.byte

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

clean:
	ocamlbuild -clean

repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte