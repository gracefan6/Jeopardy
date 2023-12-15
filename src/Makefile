MODULES=authors board command state main bot make_board # add compilation units here
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip jeopardy.zip *.ml* board1.json *.txt _tags .merlin .ocamlformat .ocamlinit Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private jeopardy.zip