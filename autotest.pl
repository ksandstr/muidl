#!/usr/bin/perl -w
use strict;
use feature "switch";	# from 5.10 on

use File::Temp qw/tempfile tempdir/;
use Fcntl qw/SEEK_SET/;
use Test::More;


# get signal names for ease of programming.
use Config;
defined $Config{sig_name} || die "No sigs?";
my $signth = 0;
my %signo;
my @signame;
foreach my $name (split(' ', $Config{sig_name})) {
    $signo{$name} = $signth;
    $signame[$signth] = $name;
    $signth++;
}


sub get_comment_sections {
	my $lines = shift;
	my @result = ();
	my $lineno = 0;
	my $com;
	my $comline;
	foreach(@$lines) {
		$lineno++;
		chomp;
		if($com) {
			# inside comment
			if(/^\s*(.+)\*\//) {
				# which finished on this line.
				$com .= "$1\n";
				push @result, [$comline, $com];
				$com = undef;
			} else {
				# which continues.
				s/^[[:blank:]\*]+//;
				$com .= "$_\n";
			}
		} else {
			# outside
			if(/\/\*\s+(.+?)(\*\/)/) {
				# single-line comment
				push @result, [$lineno, $1];
			} elsif(/\/\*\s+(.+)$/) {
				# begin of block
				$com = "$1\n";
				$comline = $lineno;
			}
		}
	}
	push @result, [$comline, $com] if $com;
	return \@result;
}


sub parse_tests {
	my $sections = shift;
	my @tests = ();
	my %ids = ();
	foreach(@$sections) {
		my ($lineno, $text) = @$_;
		my @lines = split("\n", $text);
		my %t = ();
		foreach(@lines) {
			chomp;
			given($_) {
				when(/\s*TEST\s+(\d+):\s+(.+)$/) {
					$t{id} = int($1);
					$t{desc} = $2;
				}
				when(/\s*expect\w*:\s+(.+)$/) {
					$t{expect} = $1;
				}
				default {
					if($t{id} && !($_ =~ /^\s+$/)) {
						diag("warning: `$_' line in test spec ignored");
					}
				}
			}
		}
		if(exists $t{id}) {
			my $id = $t{id};
			if(exists $ids{$id}) {
				die "Test ID `$id' redefined in stanza at line $lineno.";
			}
			# TODO: check for valid expect clause
			#	} else {
			#		die "$filename:$t->{lineno}: inexplicable expect spec `$t->{expect}'";
			#	}
			$ids{$id} = 1;
			$t{lineno} = $lineno;
			push @tests, \%t;
		}
	}
	return \@tests;
}


sub read_fh {
	my $fh = shift;
	my $filename = shift;
	seek($fh, 0, SEEK_SET) or die "can't seek $filename: $!";
	my $data = do { local $/; <$fh>; };
	return $data;
}


# like system(), but captures stdout and stderr.
sub fancy_system {
	my $cmdline = shift;
	my ($outfh, $outfilename, $errfh, $errfilename);
	my %result = ();
	eval {
		($outfh, $outfilename) = tempfile();
		($errfh, $errfilename) = tempfile();
		$result{status} = system("$cmdline >$outfilename 2>$errfilename");
		$result{stdout} = read_fh($outfh, $outfilename);
		$result{stderr} = read_fh($errfh, $errfilename);
	};
	my $evalstatus = $@;
	if($outfh) {
		close $outfh;
		unlink $outfilename;
	}
	if($errfh) {
		close $errfh;
		unlink $errfilename;
	}
	if($evalstatus) {
		die "$evalstatus";
	} else {
		return \%result;
	}
}


sub read_test_input {
	my $filename = shift;
	open(FILE, "< $filename")
		or die "can't open `$filename' for reading: $!";
	my @lines = <FILE>;
	close(FILE);

	# find the autotest header.
	my $hdr;
	foreach(@lines) {
		if(/\/\*\s+AUTOTEST:\s*(.+?)\s*(\*\/|\s+)$/) {
			$hdr = $1;
			last;
		}
	}
	die "no AUTOTEST comment found" unless $hdr;
	$hdr =~ s/%n/$filename/eg;
	note("command template for `$filename' is: $hdr");

	return ($hdr, parse_tests(get_comment_sections(\@lines)));
}


sub run_tests {
	my $hdr = shift;
	my $tests = shift;
	my $filename = shift;

	foreach(@$tests) {
		my $t = $_;
		my $test_id = "$filename:$t->{lineno}" . '[' . $t->{id} . ']';
		my $testname = $t->{desc};
		$testname .= " <ex: $t->{expect}>" if $t->{expect} !~ /^succe/;
		my $cmd = $hdr;
		$cmd =~ s/%t/$t->{id}/eg;
		note("running `$cmd' lineno $t->{lineno}");
		my $res = fancy_system($cmd);
		my $status = $res->{status};
		die "Can't execute `$cmd': $!" if $status == -1;
		my ($signum, $core_dumped, $retcode);
		# some shells embed the inner status by returning it whole,
		# so translate again until retcode is no longer > 127.
		do {
			$signum = $status & 0x7f;
			$core_dumped = $status & 0x80;
			$retcode = $status >> 8;
			$status = $retcode;
		} while($retcode > 127);

		# analyse the result value.
		if($t->{expect} =~ /^fail/ && $signum == 0 && $retcode != 0) {
			# "failure", "failed" etc -- ordinary exit with non-zero return code
			# indicating test failure. so the expect clause succeeds.
			pass($testname);
		} elsif($t->{expect} =~ /^abort/ && $signum == $signo{ABRT}) {
			# abort case, i.e. exited with SIGABRT
			pass($testname);
		} elsif($t->{expect} =~ /^succe/ && $signum == 0 && $retcode == 0) {
			# success (ignore survivor guilt)
			pass($testname);
		} else {
			diag("$test_id failed: expected `$t->{expect}',"
				. " but retval is $retcode");
			if($signum > 0) {
				diag("  exit signal $signum ($signame[$signum])");
			}
			if($res->{stdout}) {
				diag("stdout follows:\n$res->{stdout}");
			}
			if($res->{stderr}) {
				diag("stderr follows:\n$res->{stderr}");
			}
			fail($testname);
		}
	}

	return 0;
}


my $status = 0;
my @pairs = ();
foreach(@ARGV) {
	my @t = read_test_input($_);
	push @t, $_;	# add filename.
	push @pairs, \@t;
}
my $plan_total = 0;
foreach(@pairs) {
	$plan_total += @{$_->[1]};
}

# the real test starts from here.
plan tests => $plan_total;
foreach(@pairs) {
	run_tests(@$_);
}
