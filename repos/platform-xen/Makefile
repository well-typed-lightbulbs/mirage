.PHONY: all clean depend install

PREFIX ?= /usr/local
INSTALLDIR := $(DESTDIR)$(PREFIX)
INCLUDE := $(INSTALLDIR)/include/mirage
XEN_INCLUDE := $(INCLUDE)/xen

all: 
	./cmd configure xen
	./cmd build

install:
	./cmd install
	mkdir -p $(XEN_INCLUDE)
	cp -r runtime/include/* $(XEN_INCLUDE)

clean:
	./cmd clean
