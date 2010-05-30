
PKG=libIDL-2.0 glib-2.0
PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
LLVM_BITS=backend
CFLAGS:=-std=gnu99 -Wall -g -O2 -march=native -pthread \
	-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS \
	$(shell pkg-config --cflags $(PKG))
LIBS:=-lm $(shell pkg-config --libs $(PKG)) $(shell llvm-config --libs $(LLVM_BITS))

AUTOTEST_FILES := $(wildcard test*.idl)

.PHONY: all clean distclean


all: tags muidl


clean:
	rm -f *.o


distclean: clean
	rm -f muidl
	rm -f *-defs.h *-service.c *-client.c
	@rm -f tags


check:
	./autotest.pl $(AUTOTEST_FILES)


muidl: muidl.o util.o analyse.o verify.o dispatch.o gen-common.o gen-stubs.o
	$(CXX) -o $@ $^ $(CFLAGS) $(LIBS)


tags: $(wildcard *.[ch])
	@ctags -R *


%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)


# TODO: generate these automagically
muidl.o: muidl.c muidl.h
util.o: util.c muidl.h
gen-common.o: gen-common.c muidl.h
gen-stubs.o: gen-stubs.c muidl.h
analyse.o: analyse.c muidl.h
verify.o: verify.c muidl.h
dispatch.o: dispatch.c muidl.h
