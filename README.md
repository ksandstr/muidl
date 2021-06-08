
What
====

This directory is a source tree of µIDL (or "muidl" where not printable), an
IDL compiler for µiX and more generally the L4.X2 microkernel specification.

µIDL is licensed under the GNU General Public License version 3, or (at your
option) any later version published by the Free Software Foundation.


How?
----

Compiling µIDL, including its customized version of libIDL, requires a
post-2016/02 CCAN tree under `~/src/ccan`. Alter the Makefile if this doesn't
suit you. Also, `apt-get install`

    gnome-common flex bison llvm-11-dev libedit-dev

on Devuan GNU/Linux and compatible Debian derivatives. Note that µIDL is
currently only compatible with LLVM version 11.

After that, use the Makefile with the appropriate program.

The test suite requires a copy of the `mung` microkernel's source tree in
`../mung` for the `<l4/types.h>` header file. With tweaks to
`perl5/Muidl/Test.pm` it should be possible to use e.g. L4Ka::Pistachio's
headers instead.


Health warning
--------------

The code is an unwholesomely Lovecraftian mess. This comes from a lack of
proper testing and specification throughout development, and a good bit of
simple code rot. (who uses GLib anymore anyway.) It's getting better, but
won't be properly nice anytime soon.


  -- Kalle A. Sandström <ksandstr@iki.fi>
