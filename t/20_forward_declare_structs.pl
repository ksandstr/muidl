#!/usr/bin/perl
use Modern::Perl '2020';
use FindBin qw/$Bin/;

use Test::More;
use File::Temp qw/tempfile/;
use IO::File;

# test on structure forward declarations: for a file declaring module M,
# interface M::I, and structs M::S1 and M::I::S2, and a second file that
# #includes the first and references S1 and S2 in operation parameters, the
# defs header of the second should be inclusible in a program that neither
# includes the defs header of the first nor forward-declares S1 or S2.

my $MUIDL = "$Bin/../muidl";
my $INPUTDIR = "$Bin/forward_declare_structs";
my $L4DIR = "$Bin/../../mung/include";

sub report_system {
	my ($what, $n) = @_;
	if($n == -1) {
		diag("$what failed, system error $!");
	} else {
		diag("$what failed, rc " . ($n >> 8));
	}
}

plan tests => 4;
my ($fh, $outname) = tempfile();
note("outname is $outname");

my $n = system("$MUIDL -I $INPUTDIR $INPUTDIR/struct-passing.idl --defs");
if(!ok($n >> 8 == 0, "muidl")) {
	report_system("muidl", $n);
}
$n = system(qq($ENV{CC} -c -o '$outname' $INPUTDIR/consumer.c \\
	-I . -I $INPUTDIR -I $Bin/.. -I $L4DIR -std=gnu11 -Werror));
if(!ok($n >> 8 == 0, "compile consumer")) {
	report_system("\`$ENV{CC}'", $n);
}

my $input = IO::File->new("< $Bin/../struct-passing-defs.h");
if(ok($input, "got struct-passing-defs.h")) {
	ok(!grep(/^struct t_s3;/, <$input>), "no forward for I::S3");
	$input->close;
} else {
	diag("output not found: $!");
	skip(1, "muidl output not found");
}

unlink($outname) or diag("unlink($outname) failed: $!");
