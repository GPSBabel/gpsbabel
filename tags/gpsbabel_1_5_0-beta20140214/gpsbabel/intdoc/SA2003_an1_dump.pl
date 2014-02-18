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

   my $intermed = 0x80000000-$foo;
   my $deg = $intermed/(0x800000);
   sprintf( "%8.8x %8.8x %d %06.3f", $foo, $intermed, $deg, 60*($deg-int($deg)));
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
    # This is a Windows ImageList stream.  It actually includes the
    # 'BM' structures following in the stream, so we could be smarter
    # about how many we expect to find. (2 if bit 0 of ilflags is set,
    # 1 otherwise.  That bit is ILC_MASK.) For now, though, this works 
    # just fine.  Newer versions of the IL structure supposedly contain
    # more overlay indices, but SA always seems to use the 0x101 version.

    # Documentation on the stream format is hard to come by.  I found
    # mine in the form of the WINE project's reimplementation of comctl32r:

    # http://cvs.winehq.org/cvsweb/wine/dlls/comctl32/#dirlist
    # typedef struct _ILHEAD
    # {
    #     USHORT	usMagic;         (RLP: this is 'IL', 0x4c49)
    #     USHORT	usVersion;       (RLP: SA likes 0x0101)
    #     WORD		cCurImage;       (RLP: image count)
    #     WORD		cMaxImage;       (RLP: max before grow)
    #     WORD		cGrow;
    #     WORD		cx;
    #     WORD		cy;
    #     COLORREF	bkcolor;         (RLP: DWORD, 0x00bbggrr)
    #     WORD		flags;           (RLP: ILC_*; See below)
    #     SHORT		ovls[4];
    # } ILHEAD;

    # #define ILC_MASK                0x00000001
    # #define ILC_COLOR               0x00000000
    # #define ILC_COLORDDB            0x000000FE
    # #define ILC_COLOR4              0x00000004
    # #define ILC_COLOR8              0x00000008
    # #define ILC_COLOR16             0x00000010
    # #define ILC_COLOR24             0x00000018
    # #define ILC_COLOR32             0x00000020
    # See Windows SDK ImageList_Create for the meanings of these flags.
    # Street Atlas appears to use ILC_COLOR32, both with and without ILC_MASK.
 
    # * The format is like this:
    # * 	ILHEAD 			ilheadstruct;
    # *
    # * for the color image part:
    # * 	BITMAPFILEHEADER	bmfh;
    # * 	BITMAPINFOHEADER	bmih;
    # * only if it has a palette:
    # * 	RGBQUAD 		rgbs[nr_of_paletted_colors];
    # *
    # * 	BYTE			colorbits[imagesize];
    # *
    # * the following only if the ILC_MASK bit is set in ILHEAD.ilFlags:
    # * 	BITMAPFILEHEADER	bmfh_mask;
    # * 	BITMAPINFOHEADER	bmih_mask;
    # * only if it has a palette (it usually does not):
    # * 	RGBQUAD		  	rgbs[nr_of_paletted_colors];
    # *
    # * 	BYTE			maskbits[imagesize];
    # *
    # * CAVEAT: Those images are within a NxM bitmap, not the 1xN we expect.

    ($ilVersion, $ilCount, $ilMax, $ilGrow, $ilcx, $ilcy,
	$ilbkColor, $ilflags, $ilovl1, $ilovl2, $ilovl3, $ilovl4 ) =
    shiftunpack( 'sssssslsssss' );
    printf( "ver %x count %d   max %d grow %d   cx %d cy %d  bkcolor %x  flags %x\n", $ilVersion, $ilCount, $ilMax, $ilGrow, $ilcx, $ilcy, $ilbkColor, $ilflags );
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

    printf( "Image: %2d %2d %s %x $name\n", $hotspotx, $hotspoty, decodeGuid( $guid ), $unk1 );
    $imagenames{$guid} = $name;
    $bitmapcount--; 
  }
} 

# waypoint information

($magic, $wptcount) = shiftunpack( 'sl' );

@types = qw(none marker line polygon text circle mapnote highlight 
            unknown8 arc spline rectangle unknown12 unknown13 road 
            trail track waypoint photo);

print( "$wptcount waypoints\n" );
while ( $wptcount ) {

  ($magic, $unk1, $lon, $lat, $type, $height, $width, $unk2, $unk3, $serial, 
   $unk4, $create_zoom, $visible_zoom, $unk5, $circle_radius, $name, $font, 
   $guid, $fontcolor, $fontstyle, $fontsize, $outlineweight, $outlinecolor, 
   $outlineflags, $fillcolor, $unk6, $fillflags ) = 
   shiftunpack( 'slllsllssssCCsds/a*s/a*a[16]lllllllll' );

   # fontcolor is BGR (i.e. pure blue is 0xff00000, pure red is 0x0000ff)
   # fontstyle is 0x10-bold, 0x20-italic, 0x80-underline

   # width/height are in pixels for mapnotes and represent the offset of the 
   #     mapnote from the point (i.e. the dimensions of the tail.)
   # width/height are in degrees times 0x800000 for rectangles

   # circle_radius is in kilometers.

   # Note that type appears to be shared with lines.
   # type   desc
   #   1     marker (flag, dot, etc.)
   #   4     text
   #   5     circle
   #   6     mapnote
   #  11     rectangle
   #  17     waypoint
   #  18     photo

   

  $lat = decode( $lat );
  $lon = decode( $lon );

  $rect_height = $height/0x800000;
  $rect_width = $width/0x800000;

  printf ( "$magic -- %x %x %x %x %x %x %x %x %x -- $type $types[$type]  $lat  $lon  %s $imagenames{$guid}  '$name'\n", 
      $unk1,  $unk2,  $unk3,  $unk4,  $unk5,  $unk6,  decodeGuid( $guid ) );

	printf ("   %d height   %d width   %x fill %x outline %x fillstyle font %s\n", $height, $width, $fillcolor, $outlinecolor, $fillflags, $font);

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

