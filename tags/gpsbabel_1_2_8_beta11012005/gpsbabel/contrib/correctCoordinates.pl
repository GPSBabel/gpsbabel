#!/usr/bin/perl

############################################################################################################
#
# GPX Coordinate corrector
# 3/17/2004
#
# Takes a GPX Spinner-formatted CorrectedCaches.txt file and substitutes the coordinates into a GPX file
#   most likely from a Geocaching pocket query, based on the short name (GCXXXX). 
#   Useful for updating coordinates for puzzle caches or changing to the next stage of a multi-cache.
#   Caches that appear in the corrections file but are not in the GPX are ignored.
#
# Usage:
#   correctCoordinates.pl my.gpx CorrectedCaches.txt > corrected.gpx
#
# requires XML::Twig
#
# Jeff Boulter <jeff@boulter.com>
#
############################################################################################################

use strict;
use XML::Twig;

my $fname = $ARGV[0];
my $correctFile = $ARGV[1];

open(CORR, $correctFile) || die "can't find corrections file: $!";

my %correct;

while (<CORR>)
{
    s/\#.*//;
    tr/\r//d;
    chomp;
    my $line = trim($_);
    if ($line)
    {
#	print "\"$line\"\n";
	my($wp, $lat, $lon) = split(/\,/, $line);
#	print "coords: $wp $lat $lon\n";

	$correct{$wp} = {'lat' => coordsToDec(trim($lat)), 'lon' => coordsToDec(trim($lon))};
    }
}

close(CORR);

my $twig = XML::Twig->new( 
	   twig_roots   => { 'wpt' => \&wpt, 
			     }, 
			   twig_print_outside_roots => 1,               # print the rest
);

$twig->parsefile($fname);    # build the twig

sub trim
{
    my $str = $_[0];
    $str =~ s/\s*(.*\S)\s*/$1/;
    return $str;
}

sub coordsToDec
{
    my $CoordString = @_[0];
    my $Result = 0;
    if ($CoordString =~ /\s*([N|S|E|W])\D*(\d+)\D*([\d|.]+)/i) {
	my $Direction = uc($1);
	$Result = $2 + $3 / 60.;
	if ($Direction eq 'S' || $Direction eq 'W') {
	    $Result = -$Result;
	}
    }
    elsif ($CoordString =~ /\s*-{0,1}\d+\.\d+/) {
	$Result = $CoordString;
    }
    return $Result;
}


  sub wpt 
{ 
      my( $t, $wpt)= @_;      # arguments for all twig_handlers
      my $title = $wpt->first_child( 'name')->text; # find the title

      if ($correct{$title})
      {
#	  print STDERR "correcting " . $wpt->first_child( 'urlname')->text  . "\n";
	  $wpt->set_att('lat' => $correct{$title}->{'lat'});
	  $wpt->set_att('lon' => $correct{$title}->{'lon'});
      }

      # bug in Twig doesn't keep > escaped
      $wpt->subs_text( qr{>}, '&ent( "&gt;")');

      $wpt->flush;            # outputs the section and frees memory
    }                                 

