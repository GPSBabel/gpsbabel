#!/usr/bin/perl

use strict;
open(FF, "gpsbabel -^3|");
while (<FF>) {
    if (/^file/ || /^serial/) {
	my @x = split(/\t/, $_);
	my $out = $x[4];
	$out =~ s/"/\\"/g;
	print "QT_TR_NOOP(\"$out\"),\n";
    }
    
    if (/^option/) {
	my @x = split(/\t/, $_);
	my $out = $x[3];
	$out =~ s/"/\\"/g;
	print "QT_TR_NOOP(\"$out\"),\n";
    }
}
