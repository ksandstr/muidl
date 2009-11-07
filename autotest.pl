#!/usr/bin/perl -w
use strict;
use feature "switch";	# from 5.10 on

use File::Temp qw/tempfile tempdir/;
use Fcntl qw/SEEK_SET/;


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
						# TODO: use something else to print warnings
						print STDERR "warning: `$_' line in test spec ignored\n"
					}
				}
			}
		}
		if(exists $t{id}) {
			my $id = $t{id};
			if(exists $ids{$id}) {
				die "Test ID `$id' redefined in stanza at line $lineno.";
			}
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


sub autotest {
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
	if(!$hdr) { die "no AUTOTEST comment found" }
	$hdr =~ s/%n/$filename/eg;
	print STDERR "command template is `$hdr'\n";

	my $tests = parse_tests(get_comment_sections(\@lines));
	foreach(@$tests) {
		my $t = $_;
		my $cmd = $hdr;
		$cmd =~ s/%t/$t->{id}/eg;
		print STDERR "running `$cmd' for lineno $t->{lineno}\n";
		my $res = fancy_system($cmd);
		my $status = $res->{status};
		my $retcode = $status >> 8;
		die "Can't execute `$cmd': $!" if $status == -1;
		if($t->{expect} =~ /^fail/) {
			# low 7 bits are signal number. I have no idea what bit #7 is.
			if(($status & 0x7f) == 0 && $retcode == 0) {
				# TODO: report somewhere more proper
				print STDERR "Test ID $t->{id} failed: return code $retcode"
					. " (wanted != 0)\n";
				if($res->{stdout}) {
					print STDERR "stdout follows:\n$res->{stdout}\n";
				}
				if($res->{stderr}) {
					print STDERR "stderr follows:\n$res->{stderr}\n";
				}
			}
		} else {
			die "$filename:$t->{lineno}: inexplicable expect spec `$t->{expect}'";
		}
	}

	return 0;
}


my $status = 0;
foreach(@ARGV) {
	my $fn = $_;
	eval {
		autotest $_;
	};
	if($@) {
		print STDERR "error: $@";
		$status = 1;
	}
}
exit $status;
