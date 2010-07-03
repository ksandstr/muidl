
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
	rm -f *-defs.h *-service.c *-client.c


distclean: clean
	rm -f muidl
	@rm -f tags
	@rm -rf .deps


check:
	./autotest.pl $(AUTOTEST_FILES)


muidl: muidl.o util.o analyse.o verify.o llvmutil.o dispatch.o l4x2.o \
		sequence.o types.o gen-common.o gen-stubs.o
	$(CXX) -o $@ $^ $(CFLAGS) $(LIBS)


tags: $(wildcard *.[ch])
	@ctags -R *


.deps:
	@mkdir -p .deps


%.o: %.c
	$(CC) -c -o $@ $< -MMD $(CFLAGS)
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


include $(wildcard .deps/*.d)
