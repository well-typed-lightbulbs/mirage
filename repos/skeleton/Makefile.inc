.PHONY: all clean

all:
	ocamlbuild $(TARGETS)
	
clean:
	ocamlbuild -clean
