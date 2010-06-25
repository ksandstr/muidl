
PKG=libIDL-2.0 glib-2.0
PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
LLVM_CONFIG=llvm-config-2.7
LLVM_BITS=backend
CFLAGS:=-std=gnu99 -Wall -g -O2 -march=native -pthread \
	$(shell pkg-config --cflags $(PKG)) \
	$(shell $(LLVM_CONFIG) --cflags $(LLVM_BITS)|./remove-unwanted-opts.pl)
LIBS:=-lm -ldl $(shell pkg-config --libs $(PKG)) \
	$(shell $(LLVM_CONFIG) --libs $(LLVM_BITS))

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


muidl: muidl.o util.o analyse.o verify.o dispatch.o l4x2.o \
		gen-common.o gen-stubs.o
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
l4x2.o: l4x2.c muidl.h
