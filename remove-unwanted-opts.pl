#!/usr/bin/perl -w
use strict;

# llvm-config puts optimization, linkage, and warning things in our command
# line. that won't do. off with their heads.
my %skips = map { ($_, $_) }
	qw/-DNDEBUG -D_GNU_SOURCE -pedantic -Wpedantic/;

while(<>) {
	my @bits = split /\s+/;
	my @out = ();
	foreach (@bits) {
		next if exists($skips{$_});
		next if /^-f/ && !($_ eq '-fPIC');
		next if /^-O/;
		push @out, $_;
	}
	print join(" ", @out) . "\n";
}
