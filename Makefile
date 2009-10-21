
PKG=libIDL-2.0 glib-2.0
CFLAGS:=-std=gnu99 -Wall -g $(shell pkg-config --cflags $(PKG)) -O2 -march=native
LIBS:=-lm $(shell pkg-config --libs $(PKG))

.PHONY: all clean distclean


all: tags muidl


clean:
	rm -f *.o
	rm -f {test,bus}-defs.h


distclean: clean
	rm -f muidl
	@rm -f tags


muidl: muidl.o
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)


tags: $(wildcard *.[ch])
	@ctags -R *


%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

