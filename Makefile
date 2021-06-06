
PKG=glib-2.0
PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
LLVM_CONFIG=llvm-config-11
LLVM_BITS=all

LIBIDL_CFLAGS=-I libIDL/include
LIBIDL_LIBS=libIDL/.libs/libIDL-2.a

CCAN_DIR=~/src/ccan
CCAN_BITS=hash htable str strset strmap ilog talloc

CFLAGS:=-std=gnu11 -Wall -g -O2 -pthread -I include -I $(CCAN_DIR) \
	$(LIBIDL_CFLAGS) -Wno-sign-compare -Wimplicit-fallthrough=2 \
	$(shell pkg-config --cflags $(PKG)) \
	$(shell $(LLVM_CONFIG) --cflags | ./remove-unwanted-opts.pl)
LDFLAGS:=-Wl,--as-needed $(shell $(LLVM_CONFIG) --ldflags)
LIBS:=$(shell pkg-config --libs $(PKG)) \
	$(shell $(LLVM_CONFIG) --libs --system-libs $(LLVM_BITS)) -lm -ldl

.PHONY: all clean distclean


all: tags
	+@make $(LIBIDL_LIBS)
	+@make muidl


clean:
	@rm -f *.o
	+@make output-clean


output-clean:
	@rm -f *-defs.h *-service.s *-client.s *-common.s


distclean: clean
	@rm -f muidl
	+@make -C libIDL distclean
	@rm -f tags
	@rm -rf .deps


check: muidl
	t/run_all.pl
	+@make output-clean


muidl: muidl.o util.o analyse.o verify.o llvmutil.o attr.o l4x2.o \
		message.o sequence.o types.o struct.o header.o common.o \
		dispatch.o stub.o except.o iface.o op.o signature.o \
		stringfn.o $(CCAN_BITS:%=ccan-%.o)
	@echo "  LD $@"
	@$(CXX) -o $@ $^ $(LIBIDL_LIBS) $(CFLAGS) $(LDFLAGS) $(LIBS)


libIDL/.libs/libIDL-2.a libIDL/include/libIDL/IDL.h:
	@cd libIDL; ./autogen.sh
	+@make -C libIDL


tags: $(wildcard *.[ch]) $(wildcard libIDL/*.[ch])
	@ctags -R *


.deps:
	@mkdir -p .deps


%.o: %.c libIDL/include/libIDL/IDL.h
	@echo "  CC $@"
	@$(CC) -c -o $@ $< -MMD $(CFLAGS)
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


ccan-%.o ::
	@echo "  CC $@ <ccan>"
	@$(CC) -c -o $@ $(CCAN_DIR)/ccan/$*/$*.c $(CFLAGS)


ccan-hash.o :: CFLAGS += -Wno-implicit-fallthrough


include $(wildcard .deps/*.d)
