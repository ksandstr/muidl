#!/usr/bin/perl -w
use strict;
use FindBin qw/$Bin/;

my @idl = <$Bin/../tests/*.idl>;
exec("$Bin/../autotest.pl", @idl);
