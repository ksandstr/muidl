
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

    gnome-common flex bison llvm-3.6-dev libedit-dev

on Devuan GNU/Linux and compatible Debian derivatives. Note that µIDL is
currently incompatible with LLVM 3.7.

The unit test framework requires the following Perl modules on the build
system:

    File::Temp Test::More Test::Harness

After that, use the Makefile with the appropriate program.


Health warning
--------------

The code is an unwholesomely Lovecraftian mess. This is related to a lack of
proper testing and specification throughout development, and a good bit of
simple code rot. (who uses GLib anymore anyway.)


  -- Kalle A. Sandström <ksandstr@iki.fi>
