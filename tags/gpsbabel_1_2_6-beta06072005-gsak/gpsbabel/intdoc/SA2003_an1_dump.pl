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

sub decodeGuid {
   ($a, $b, $c, $d, $e, $f, $g, $h, $i, $j) = unpack( 'LSSSCCCCCC', shift );
   sprintf( '%8.8x-%4.4x-%4.4x-%4.4x-%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x',$a, $b, $c, $d, $e, $f, $g, $h, $i, $j);
}

# read file
undef $/;
$file = <>;

@filetypes = qw( drawing road trail waypoint track );

# read file header
($magic, $filetype ) = shiftunpack( 'ss' );

print <<END;
MAGIC   $magic
$filetypes[$filetype] layer

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

    open BMP, ">bitmap$filecount.bmp";
    binmode BMP;
    print BMP 'BM';
    $head = pack('lssl', ($fhsize,  $res_0_1, $res_0_2, $bitoffset));
    print BMP $head;
    $head = pack('lllssllllll', ($bmisize, $width, $height, $planes, $bpp,
            $compression, $size, $xppm, $yppm, $colused, $colimprt));
    print BMP $head;
    print BMP substr($file, 0, $palettesize+$size);
    close BMP;
    $filecount++;

    skip_bytes( $palettesize );
    # image
    skip_bytes( $size );
  }
  else {
    # image information - the 'type' we read was actually the low word of the hotspot X coord.
    ($hotspotxhi, $hotspoty, $unk1, $guid, $name ) = shiftunpack( 'slla[16]C/a*' );

    # fix the hotspot X coord
    $hotspotx = $rec_type + 0x10000*$hotspotxhi;

    printf( "Image: %2d %2d %s $name\n", $hotspotx, $hotspoty, decodeGuid( $guid ) );
    $imagenames{$guid} = $name;
    $bitmapcount--; 
  }
} 

# waypoint information

($magic, $wptcount) = shiftunpack( 'sl' );

@types = qw(none marker line polygon text circle mapnote highlight unknown8 arc spline rectangle
         unknown12 unknown13 road trail track waypoint);

print( "$wptcount waypoints\n" );
while ( $wptcount ) {

  ($magic, $unk1, $lon, $lat, $type, $height, $width, $unk2, $unk3, $serial, 
   $unk4, $create_zoom, $visible_zoom, $unk5, $circle_radius, $name, $font, 
   $guid, $fontcolor, $fontstyle, $fontsize, $unk6, $outlinecolor, $unk7, 
   $fillcolor, $unk8, $unk9 ) = 
   shiftunpack( 'slllsllssssCCsds/a*s/a*a[16]lllllllll' );

   # fontcolor is BGR (i.e. pure blue is 0xff00000, pure red is 0x0000ff)
   # fontstyle is 0x10-bold, 0x20-italic, 0x80-underline

   # width/height are in pixels for mapnotes and represent the offset of the mapnote
   #    from the point (i.e. the dimensions of the tail.)
   # width/height are in degrees times 0x800000 for rectangles

   # Note that type appears to be shared with lines.
   # type   desc
   #   1     marker (flag, dot, etc.)
   #   4     text
   #   5     circle
   #   6     mapnote
   #  11     rectangle
   #  17     waypoint

  $lat = decode( $lat );
  $lon = decode( $lon );

  $rect_height = $height/0x800000;
  $rect_width = $width/0x800000;

  printf ( "$magic -- %x %x %x %x %x %x %x %x %x -- $type $types[$type]  $lat  $lon  %s $imagenames{$guid}  '$name'\n", 
      $unk1,  $unk2,  $unk3,  $unk4,  $unk5,  $unk6,  $unk7, $unk8, $unk9, decodeGuid( $guid ) );

  $wptcount--;
} 

# line information
($magic, $linecount ) = shiftunpack( 'sl' );
print ( "$linecount lines\n" );
while ( $linecount ) {
  ($magic, $unk1, $serial, $unk2, $unk3, $type, $unk4, $name, $lineweight, $linestyle, 
   $linecolor, $unk5, $polyfillcolor, $unk6, $unk7, $unk8, $pointcount ) = 
    shiftunpack( 'ssslssls/a*sllllllsl' );

   # arcs are 4-point (3-segment) lines: start, third point, end, center (yes, that's overdetermined.)

   # Note that type appears to be shared with points.
   # type   desc
   #   2     line
   #   3     polygon
   #   7     highlight
   #   9     arc
   #  10     spline
   #  14     routable road
   #  15     trail
   #  16     track

   printf ("--- start line ---   %.4x  %x %x %x %x %x %x %x %x -- $type $types[$type] '$name'\n", $magic,
		$unk1, $unk2, $unk3, $unk4, $unk5, $unk6, $unk7, $unk8 );
  while ( $pointcount ) {
    ($magic, $unk0, $lon, $lat, $unk0s ) = shiftunpack( 'sllls' );
    $lat = decode( $lat );
    $lon = decode( $lon );
    printf (" $magic   $lat   $lon   %x %x\n", $unk0, $unk0s);
    $pointcount--;
  }
  print "--- end line ---\n";
  $linecount--;
}

