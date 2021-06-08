#!/usr/bin/perl
use Modern::Perl '2020';
use FindBin qw/$Bin/;
use lib "$Bin/../perl5";

use Muidl::Test;
use IO::File;

# test on structure forward declarations: for a file declaring module M,
# interface M::I, and structs M::S1 and M::I::S2, and a second file that
# #includes the first and references S1 and S2 in operation parameters, the
# defs header of the second should be inclusible in a program that neither
# includes the defs header of the first nor forward-declares S1 or S2.

plan tests => 4;
ok(run_muidl("struct-passing.idl", defs => 1), "muidl");
ok(run_cc("consumer.c", opts => '-Werror'), "compile consumer");

my $input = IO::File->new("< $IDL_OUTPUT_DIR/struct-passing-defs.h");
if(ok($input, "got struct-passing-defs.h")) {
	ok(!grep(/^struct t_s3;/, <$input>), "no forward for I::S3");
	$input->close;
} else {
	diag("output not found: $!");
	skip(1, "muidl output not found");
}
