#!/usr/bin/perl

=pod

  This script reads a DeLorme Street Atlas 2003 .an1 (drawing) file 
  and prints various pertinent data from it.  Anything with a variable 
  name starting with "unk" or "magic" or "zero" is probably something 
  we don't yet understand.  Suggestions as to what some of these fields 
  mean are welcome.  The author disclaims any liability arising from 
  the use of any information contained within this script. 

    Copyright (C) 2005 Ronald L. Parker (babelan1perl@parkrrrr.com)

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

sub skip_bytes {
   my $count = shift;
   $file = substr( $file, $count );
}

# read file
undef $/;
$file = <>;

# read file header
($magic, $unk1 ) = shiftunpack( 'ss' );

print <<END;
MAGIC   $magic

END

# read bitmap info
($bitmapcount) = shiftunpack( 'l' );

while ( $bitmapcount ) {
  ($rec_type) = shiftunpack( 's' );
  if ( $rec_type == 0x4c49 ) { # 'IL'
    # I don't know what this structure is, but it appears twice in my test files.
    ($unk10101, $unke, $unkc, $unk18_1, $unk18_2, $unkneg1_1,
     $unk20, $unkneg1_2, $unkneg1_3) = shiftunpack( 'lsssslsll');
  }
  elsif ( $rec_type == 0x4d42 ) { # 'BM'
    # This is a standard BMP file, documented in MSDN.
    # BITMAPFILEHEADER
    ($fhsize, $res_0_1, $res_0_2, $bitoffset) = shiftunpack( 'lssl' );
    # BITMAPINFOHEADER
    ($bmisize, $width, $height, $planes, $bpp, $compression, 
     $size, $xppm, $yppm, $colused, $colimprt ) = shiftunpack( 'lllssllllll');
    # palette
    $palettesize = $bitoffset - $bmisize - 14; # 14 bytes in BMFH, including the 'BM'
    skip_bytes( $palettesize );
    # image
    skip_bytes( $size );
  }
  else {
    # image information - the 'type' we read was actually the low word of the hotspot X coord.
    ($hotspotxhi, $hotspoty, $unk1, $unk2, $unk3, $unk4, $unk5, $name ) = 
          shiftunpack( 'sllllllC/A*' );
    $hotspotx = $rec_type + 0x10000*$hotspotxhi;
    print( "Image: $hotspotx $hotspoty   $name\n" );
    $bitmapcount--; 
  }
} 

# waypoint information

($magic, $wptcount) = shiftunpack( 'sl' );

print( "$wptcount waypoints\n" );
while ( $wptcount ) {
  ($magic, $unk0, $lat, $lon, $unk1, $unk2, $unk3, $unk4, $unk5, $serial,
   $unk6s, $unk7s, $unk8, $unk9, $unk10s, $name, $font, $unk11l, $unk11h, 
   $unk12l, $unk12h, $unk13l, $unk13h, $unk14l, $unk14h, $unk15, $unk16, $fontsize, $unk17,
   $unk18, $unk19, $unk20, $unk21, $unk22 ) = 
   shiftunpack( 'slllsllsssssllss/A*s/A*sssssssslllllllll' );
  $lat = decode( $lat );
  $lon = decode( $lon );
  print ( "$lat   $lon   $serial  $name\n" );
  $wptcount--;
} 

# line information
($magic, $linecount ) = shiftunpack( 'sl' );
print ( "$linecount lines\n" );
while ( $linecount ) {
  print "--- start line ---\n";
  ($magic, $unk1, $unk2, $unk3, $unk4, $unk5l, $unk6, $unk7, $unk8l, $unk9l,
   $unk10_3, $unk11_0, $unk12_0, $unk13_0, $unk14s, $pointcount ) = 
    shiftunpack( 'sllsslssllllllsl' );
       # unk10_3 might be a count that counts $unkXX_0
  while ( $pointcount ) {
    ($magic, $unk0, $lat, $lon, $unk0s ) = shiftunpack( 'sllls' );
    $lat = decode( $lat );
    $lon = decode( $lon );
    print "  $lat   $lon\n";
    $pointcount--;
  }
  print "--- end line ---\n";
  $linecount--;
}

