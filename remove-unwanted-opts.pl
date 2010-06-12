#!/usr/bin/perl -w
use strict;

# llvm-config puts optimization and linkage things in our command line. that
# won't do.

my %skips = ( '-DNDEBUG' => 1, '-D_GNU_SOURCE' => 1 );
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
