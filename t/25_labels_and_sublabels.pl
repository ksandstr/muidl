#!/usr/bin/perl
use Modern::Perl '2020';
use FindBin qw/$Bin/;
use lib "$Bin/../perl5";

use Muidl::Test;

# test on labels and sublabels in the defs header.
plan tests => 3;

# positive presence of labels when wanted.
ok(run_muidl("input.idl", defs => 1), "muidl input.idl");
ok(run_cc("consumer.c", opts => '-Werror'), "compile consumer");

# non-presence of labels when not wanted.
ok(run_cc("nonconsumer.c", opts => '-Werror'), "compile nonconsumer");
