
What
====

This directory is a source tree of µIDL (or "muidl" where not printable), an
IDL compiler for µiX and more generally the L4.X2 microkernel specification.

µIDL is licensed under the GNU General Public License version 3, or (at your
option) any later version published by the Free Software Foundation.


How?
----

Compiling the custom version of libIDL that µIDL uses requires the

    gnome-common flex bison

packages on Debian GNU/Linux and compatible derivatives.

The unit test framework requires the following Perl modules on the build
system:

    File::Temp Test::More Test::Harness

Once you've got these, say

    $ make

like there's ass in your pants.


Health warning
--------------

The code is an unwholesomely Lovecraftian mess. This is caused by a lack of
formal testing and specification throughout development, a.k.a. early prototype
disease.

You have been warned.


  -- Kalle A. Sandström <ksandstr@iki.fi>
