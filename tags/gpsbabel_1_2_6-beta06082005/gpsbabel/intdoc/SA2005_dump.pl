#!/usr/bin/perl

=pod

  This script reads a DeLorme Street Atlas 2003-2005 .anr (route) file 
  and prints various pertinent data from it.  Anything with a variable 
  name starting with "unk" or "magic" or "zero" is probably something 
  we don't yet understand.  Suggestions as to what some of these fields 
  mean are welcome.  The author disclaims any liability arising from 
  the use of any information contained within this script. 

    Copyright (C) 2004 Ronald L. Parker (babelanrperl@parkrrrr.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

=cut

# Convert a longword to a latitude or longitude
sub decode {
   my $foo = shift;

   my $deg = (0x80000000-$foo)/(0x800000);
   sprintf( "%d %06.3f", $deg, 60*($deg-int($deg)));
}


# read a data structure from the input file.  
sub shiftunpack {

   my $pattern = shift;
   my @result = unpack( $pattern, $file );
   my $str = pack( $pattern, @result );
   $file = substr( $file, length( $str ));
   @result;
}

# read file
undef $/;
$file = <>;

# read file header
($magic, $version, $unk1, $unk2, $unk3) = shiftunpack( 'sslll' );

print <<END;
MAGIC   $magic
VERSION $version

END

# read route info
($rectype,
 $recsize,
 $magic1,
 $unk1,
 $unk2,
 $unk3, 
 $unk4, 
 $unk5,
 $magic2,
 $routename,
 $unk6,
 $distance,
 $time,
 $heading,
 $unkf1,
 $unkf2,
 $unk7,
 $unk8) = shiftunpack( 'sllllllsls/A*sdldffls' );

#printf( "%.8x  %d %d %d %d %d  %.4x  %d  %9.6f %9.6f %3d %d ",
#	$magic1,
#	$unk1, $unk2, $unk3, $unk4, $unk5,
#	$magic2, 
#	$unk6,
#	$unkf1, $unkf2,
#	$unk7, $unk8 );
print "$routename ($distance km, $time seconds, heading $heading)\n";



($sigpoints)=shiftunpack('l');
print "$sigpoints significant points\n";
for (1..$sigpoints) {
   ($type,
    $size,
    $actual_lon,
    $actual_lat,
    $magic1,
    $magic2,
    $unk1,
    $addr,
    $comment,
    $sigtype,
    $unk2,
    $unk3,
    $unk4,
    $closest_lon,
    $closest_lat,
    $unk5,
    $unk6,
    $unk7,
    $zero1,
    $zero2) = shiftunpack( 'slllllls/A*s/A*sllslllllll' );
   @SA2005Extras = shiftunpack('lslll') if ($version > 10);

   print "$addr ($comment)\n  ".
       decode($actual_lat).'  '.decode($actual_lon)."\n  ".
       decode($closest_lat).'  '.decode($closest_lon)."\n";

}

($routelegcount) = shiftunpack('l');     

for ( 1..$routelegcount ) {

  # read leg header
  ($type, $size, $unk1, $unk2, $unk3, $unk4, $unk5, 
   $distance, $time, $heading, $unk6, $unk7, $unk8) =
       shiftunpack( 'slllslldldlll' );
  print "leg length $distance ($time seconds, $heading°)\n";

  # read segment count
  ($segcount)=shiftunpack('l');
  # read segments
  for (1..$segcount) {
    ($type, $size, $street, $roadtype, $unk1,
     $distance, $transittime, $unk2, 
     $heading, $unk3, $unk4, $unk5, $unk6,
     $starttime, $startdist, $coords) = 
          shiftunpack( 'sls/A*lldlldlllllds' );
     print "$street ($roadtype) distance $distance heading $heading\n  starting $startdist [$starttime $transittime]\n";
     @coordpairs = shiftunpack( "(ll)[$coords]" );
     
     for $c (0..$coords-1) {
       print '    '.decode($coordpairs[$c*2]).'   '.decode($coordpairs[$c*2+1])."\n";
     }
     @SA2005Extras = shiftunpack('ll') if ($version > 10) ;
  }
}

# read footer
($size,$f1,$f2,$f3,$f4,$unk1,$unk2, $unk3,$unk4,$unk5,$f5,$f6,$unk6)=
    shiftunpack('lddddlllllddl');
printf( "%12.6f %12.6f %12.6f %12.6f   %12.6f %12.6f\n", $f1, $f2, $f3, $f4, $f5, $f6);
