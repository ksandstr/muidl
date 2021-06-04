#!/usr/bin/perl
use Modern::Perl '2013';
use FindBin qw/$Bin/;

my @idl = <$Bin/../tests/autotest-idl/*.idl>;
exec("$Bin/../autotest.pl", @idl);
