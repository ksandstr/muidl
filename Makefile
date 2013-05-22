
PKG=glib-2.0
PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
LLVM_CONFIG=llvm-config
LLVM_BITS=backend bitwriter

LIBIDL_CFLAGS=-I libIDL/include
LIBIDL_LIBS=libIDL/.libs/libIDL-2.a

CFLAGS:=-std=gnu99 -Wall -g -O2 -pthread \
	$(LIBIDL_CFLAGS) \
	$(shell pkg-config --cflags $(PKG)) \
	$(shell $(LLVM_CONFIG) --cflags $(LLVM_BITS)|./remove-unwanted-opts.pl)
LDFLAGS:=-Wl,--as-needed
LIBS:=$(shell pkg-config --libs $(PKG)) \
	$(shell $(LLVM_CONFIG) --libs $(LLVM_BITS)) -lm -ldl

AUTOTEST_FILES := $(wildcard tests/*.idl)

.PHONY: all clean distclean


all: tags $(LIBIDL_LIBS) muidl


clean:
	rm -f *.o
	+make -C libIDL clean
	+make output-clean


output-clean:
	rm -f *-defs.h *-service.s *-client.s *-common.s


distclean: clean
	rm -f muidl
	+make -C libIDL distclean
	@rm -f tags
	@rm -rf .deps


check: muidl
	tests/run_all.pl
	+make output-clean


.NOTPARALLEL libIDL/.libs/libIDL-2.a libIDL/include/libIDL/IDL.h: $(wildcard libIDL/*.[chyl])
	cd libIDL; ./autogen.sh
	+make -C libIDL


muidl: muidl.o util.o analyse.o verify.o llvmutil.o attr.o l4x2.o \
		message.o sequence.o types.o struct.o header.o common.o \
		dispatch.o stub.o except.o iface.o mapping.o op.o \
		stringfn.o $(LIBIDL_LIBS)
	@echo "  LD $@"
	@$(CXX) -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LIBS)


tags: $(wildcard *.[ch])
	@ctags -R *


.deps:
	@mkdir -p .deps


%.o: %.c libIDL/include/libIDL/IDL.h
	@echo "  CC $@"
	@$(CC) -c -o $@ $< -MMD $(CFLAGS)
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


include $(wildcard .deps/*.d)
