package Muidl::Test;
use Modern::Perl '2020';
use FindBin qw/$Bin/;
use Test::More;

require Exporter;
our @ISA = qw/Exporter/;
our @EXPORT = (@Test::More::EXPORT, qw/run_muidl run_cc $IDL_OUTPUT_DIR $TEST_NAME/);
our @EXPORT_OK = (@Test::More::EXPORT_OK, qw/report_system/);

use File::Temp qw/tempfile/;
use File::Basename;

our ($TEST_NAME, $INPUTDIR, $IDL_OUTPUT_DIR);

BEGIN {
	my ($name, $dirs, $suffix) = fileparse($0, ".pl");
	$name =~ s/^\d+_//;
	$TEST_NAME = $name;
	$INPUTDIR = "$Bin/$TEST_NAME";
	$IDL_OUTPUT_DIR = "$Bin/..";
}

# TODO get these from toplevel Makefile
our $MUIDL = "$Bin/../muidl";
our $L4DIR = "$Bin/../../mung/include";

our ($fh, $outname) = tempfile();
note("outname is $outname");

END {
	unlink($outname) or diag("unlink($outname) failed: $!");
}


sub _squash_opts {
	my %result;
	while(@_) {
		my $key = shift;
		if(exists $result{$key} && @_) {
			my $value = shift;
			my $t = ref $value eq 'ARRAY' ? $value : [ $value ];
			$result{$key} = [ @{$result{$key}}, @$t ];
		} elsif(@_) {
			$result{$key} = [ shift ];
		}
	}
	return \%result;
}


sub report_system {
	my ($what, $n) = @_;
	if($n == -1) {
		diag("$what failed, system error $!");
	} else {
		diag("$what failed, rc " . ($n >> 8));
	}
}


# run_muidl($file, %opts).
#
# runs muidl on $file, passing a set of command-line options and reporting
# configuration failures on diag w/ report_system.
sub run_muidl {
	my $file = shift;
	my $opts = _squash_opts(@_);

	die "$INPUTDIR doesn't exist" unless -d $INPUTDIR;
	my $cmd = "$MUIDL -I $INPUTDIR $INPUTDIR/$file";
	$cmd .= " --defs" if $opts->{defs};
	$cmd .= " --client" if $opts->{client};
	$cmd .= " --service" if $opts->{service};
	$cmd .= " --common" if $opts->{common};
	$cmd .= " -I $_" for @{$opts->{incdir} // []};
	$cmd .= " -D$_" for @{$opts->{define} // []};
	$cmd .= " $_" for @{$opts->{opts} // []};

	my $n = system($cmd);
	my $st = $n >> 8;
	report_system("muidl '$file'", $n) if $st != 0 && $st != 1;

	return $st == 0;
}


sub run_cc {
	my $file = shift;
	my $opts = _squash_opts(@_);

	die "$INPUTDIR doesn't exist" unless -d $INPUTDIR;
	my $cmd = qq($ENV{CC} -c -o '$outname' $INPUTDIR/$file \\
		-I . -I $INPUTDIR -I $Bin/.. -I $L4DIR -std=gnu11);
	$cmd .= " 2>&1" if $opts->{hide_stderr};
	$cmd .= " $_" for @{$opts->{opts} // []};

	my $n = system($cmd);
	my $st = $n >> 8;
	report_system("$ENV{CC} '$file'", $n) if $st != 0 && $st != 1;

	return $st == 0;
}

1;
