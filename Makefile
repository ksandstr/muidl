
PKG=libIDL-2.0 glib-2.0
PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
LLVM_CONFIG=llvm-config-2.7
LLVM_BITS=backend bitwriter
CFLAGS:=-std=gnu99 -Wall -g -O2 -pthread \
	$(shell pkg-config --cflags $(PKG)) \
	$(shell $(LLVM_CONFIG) --cflags $(LLVM_BITS)|./remove-unwanted-opts.pl)
LDFLAGS:=-Wl,--as-needed
LIBS:=$(shell pkg-config --libs $(PKG)) \
	$(shell $(LLVM_CONFIG) --libs $(LLVM_BITS)) -lm -ldl

AUTOTEST_FILES := $(wildcard tests/*.idl)

.PHONY: all clean distclean


all: tags muidl


clean:
	rm -f *.o
	+make output-clean


output-clean:
	rm -f *-defs.h *-service.s *-client.s *-common.s


distclean: clean
	rm -f muidl
	@rm -f tags
	@rm -rf .deps


check:
	tests/run_all.pl
	+make output-clean


muidl: muidl.o util.o analyse.o verify.o llvmutil.o attr.o l4x2.o \
		message.o sequence.o types.o struct.o header.o common.o \
		dispatch.o stub.o except.o iface.o mapping.o op.o
	$(CXX) -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LIBS)


tags: $(wildcard *.[ch])
	@ctags -R *


.deps:
	@mkdir -p .deps


%.o: %.c
	$(CC) -c -o $@ $< -MMD $(CFLAGS)
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


include $(wildcard .deps/*.d)
