#!/usr/bin/perl
use Modern::Perl '2020';
use FindBin qw/$Bin/;
use lib "$Bin/../perl5";

use Muidl::Test;

# test on composition of structures defined in an #include'd IDL file.
plan tests => 5;

# positive side: the include guard and redeclaration produce exactly one copy
# of the structure in the consumer when second-defs.h is included before
# first-defs.h .
ok(run_muidl("first.idl", defs => 1), "muidl first.idl");
ok(run_muidl("second.idl", defs => 1), "muidl second.idl");
ok(run_cc("consumer.c", opts => "-Werror"), "compile consumer");

# negative side: the include guard's signature component pops an error when
# structure contents don't match.
ok(run_muidl("third.idl", defs => 1), "muidl third.idl");
ok(!run_cc("failconsumer.c", hide_stderr => 1, opts => "-Werror"),
	"don't compile failconsumer");
