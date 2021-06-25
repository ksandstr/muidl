#!/usr/bin/perl -w
use Modern::Perl '2020';

# llvm-config used to put optimization, linkage, and warning things in our
# command line. can't have that. off with their heads should they reappear.

my %skips = map { $_ => 1 } qw/-DNDEBUG -D_GNU_SOURCE -pedantic -Wpedantic/;
while(<>) {
	my @out;
	foreach (split /\s+/) {
		next if exists $skips{$_};	# fuck these ones in particular
		next if /^-O/ or (/^-f/ && $_ ne '-fPIC');	# these ones, categorically
		push @out, $_;
	}
	print join(" ", @out) . "\n";
}
