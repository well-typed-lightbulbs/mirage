.PHONY: build clean

LIB_CMX = $(LIBS:%=lib/%.cmx)
LIB_CMI = $(LIBS:%=lib/%.cmi)
LIB_CMA = $(LIBS:%=lib/%.cma)

SYNTAX_CMA = $(SYNTAX:%=syntax/%.cma)

TARGETS = $(LIBS:%=lib/%.cmx) $(LIBS:%=lib/%.cma)
TARGETS += $(SYNTAX:%=syntax/%.cma)

BC_TARGETS = $(LIB_CMI) $(LIB_CMA)
BC_BUILT = $(BC_TARGETS:%=_build/%)

NC_TARGETS = $(LIB_CMX)
NC_BUILT = $(NC_TARGETS:%=_build/%)

BCP4_TARGETS = $(SYNTAX_CMA)
BCP4_BUILT = $(BCP4_TARGETS:%=_build/%)

build:
	ocamlbuild -classic-display $(TARGETS)

install: META build
	ocamlfind remove $(NAME) || true
	ocamlfind install $(NAME) $(FILES) META \
	  $(BC_BUILT) $(NC_BUILT) $(BCP4_BUILT)
	
clean:
	ocamlbuild -clean

META: META.in
	sed -e "s/@VERSION@/$(VERSION)/g" < $< > $@
