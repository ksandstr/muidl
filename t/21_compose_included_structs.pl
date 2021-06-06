#!/usr/bin/perl
use Modern::Perl '2020';
use FindBin qw/$Bin/;

use Test::More;
use File::Temp qw/tempfile/;
use IO::File;

# test on composition of structures defined in an #include'd IDL file.

my $MUIDL = "$Bin/../muidl";
my $INPUTDIR = "$Bin/compose_included_structs";
my $L4DIR = "$Bin/../../mung/include";

sub report_system {
	my ($what, $n) = @_;
	if($n == -1) {
		diag("$what failed, system error $!");
	} else {
		diag("$what failed, rc " . ($n >> 8));
	}
}

plan tests => 5;
my ($fh, $outname) = tempfile();
note("outname is $outname");

# positive side: the include guard and redeclaration produce exactly one copy
# of the structure in the consumer when second-defs.h is included before
# first-defs.h .
my $n = system("$MUIDL -I $INPUTDIR $INPUTDIR/first.idl --defs");
if(!ok($n >> 8 == 0, "muidl first")) {
	report_system("muidl (first)", $n) if $n >> 8 != 1;
}
$n = system("$MUIDL -I $INPUTDIR $INPUTDIR/second.idl --defs");
if(!ok($n >> 8 == 0, "muidl second")) {
	report_system("muidl (second)", $n) if $n >> 8 != 1;
}
$n = system(qq($ENV{CC} -c -o '$outname' $INPUTDIR/consumer.c \\
	-I . -I $INPUTDIR -I $Bin/.. -I $L4DIR -std=gnu11 -Werror));
if(!ok($n >> 8 == 0, "compile consumer")) {
	report_system("\`$ENV{CC}'", $n) if $n >> 8 != 1;
}

# negative side: the include guard's signature component pops an error when
# structure contents don't match.

$n = system("$MUIDL -I $INPUTDIR $INPUTDIR/third.idl --defs");
if(!ok($n >> 8 == 0, "muidl third")) {
	report_system("muidl (third)", $n) if $n >> 8 != 1;
}
$n = system(qq($ENV{CC} -c -o '$outname' $INPUTDIR/failconsumer.c \\
	-I . -I $INPUTDIR -I $Bin/.. -I $L4DIR -std=gnu11 -Werror 2>&1));
if(!ok($n >> 8 == 1, "don't compile failconsumer")) {
	report_system("\`$ENV{CC}'", $n) if $n >> 8 != 0;
}

unlink($outname) or diag("unlink($outname) failed: $!");
