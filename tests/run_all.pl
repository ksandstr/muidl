#!/usr/bin/perl -w
use strict;
use FindBin qw/$Bin/;
use Test::Harness;

my @tests = ();
foreach(<$Bin/*.pl>) {
	push @tests, $_ unless $_ =~ /run_all\.pl$/;
}
runtests(@tests);
