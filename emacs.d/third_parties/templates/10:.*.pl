#!/usr/bin/perl

################################################################################
## `(file-name-nondirectory (buffer-file-name))`
##
## Copyright `(format-time-string "%Y")` `user-full-name` (`user-mail-address`)
##
################################################################################

use lib "../ext/perl";
use lib "perl";

use strict;
use warnings;
use Getopt::Long;
use Log::Handler;

################################################################################


my $flagHelp = 0;
my $LogFilename = '';
my $flagLog = 0;
GetOptions('h|help' => \$flagHelp,'l|log=s' => \$LogFilename);


my $log = Log::Handler->new();
$log->add(screen => {log_to   => 'STDERR',newline  => 1,maxlevel => 'info'});

################################################################################

sub usage()
{
	print <<EOF;
Usage:
	`(file-name-nondirectory (buffer-file-name))`
Synopsis:

Options:
	-h, --help
	-l, --log	log filename
EOF
}


################################################################################


if($LogFilename ne '')
{
	$log->add(file => {filename => $LogFilename,mode => 'append',
                       autoflush => 1, newline  => 1,
                       maxlevel => 7,minlevel => 0});
	$flagLog = 1;
}

if (($flagHelp) or (@ARGV != $0))
{
	die usage();
}
