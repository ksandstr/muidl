
PKG=libIDL-2.0 glib-2.0
CFLAGS:=-std=gnu99 -Wall -g $(shell pkg-config --cflags $(PKG)) -O2 -march=native
LIBS:=-lm $(shell pkg-config --libs $(PKG))

AUTOTEST_FILES := $(wildcard test*.idl)

.PHONY: all clean distclean


all: tags muidl


clean:
	rm -f *.o
	rm -f *-defs.h *-dispatch.c


distclean: clean
	rm -f muidl
	@rm -f tags


check:
	./autotest.pl $(AUTOTEST_FILES)


muidl: muidl.o analyse.o verify.o gen-common.o
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)


tags: $(wildcard *.[ch])
	@ctags -R *


%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)


# TODO: generate these automagically
muidl.o: muidl.c muidl.h
gen-common.o: gen-common.c muidl.h
analyse.o: analyse.c muidl.h
verify.o: verify.c muidl.h

