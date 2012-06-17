.PHONY: all clean depend install

all: depend
	./cmd configure
	./cmd build

depend:
	@./configure.arch

install:
	./cmd install

clean:
	./cmd clean
