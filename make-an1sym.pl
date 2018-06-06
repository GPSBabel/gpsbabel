#!/usr/bin/perl

=pod

    This script reads the DeLorme Stock Symbols .dim file and writes code
    to be included in the .an1 format handler.

    You MUST have a copy of the DeLorme .dim file; you can download it
    from the support section of DeLorme's Web site.  You want the one
    that contains all of the symbols.

    To use this script:

      perl make-an1sym.pl <DeLormeStockSymbols.dim >an1sym.h

    Copyright (C) 2005 Ronald L. Parker (babelan1perl@parkrrrr.com) 
                   and Robert Lipe

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
   @guid = unpack( 'LSSSCCCCCC', shift );
   my $sub = undef;
   my $guid2 = sprintf( '{0x%8.8x,{0x%4.4x, 0x%4.4x, 0x%4.4x},'.
            ' {0x%2.2x, 0x%2.2x, 0x%2.2x, 0x%2.2x, 0x%2.2x, 0x%2.2x}}',
            @guid );
   $sub = $substitutions{ sprintf('%8.8x', @guid)};
   ($guid2, $sub);
}

sub DoImage {
    # image information - the 'type' we read was actually the low word of the hotspot X coord.
    ($hotspotxhi, $hotspoty, $unk1, $guid, $name ) = 
          shiftunpack( 'slla[16]C/a*' );
    $hotspotx = $rec_type + 0x10000*$hotspotxhi;

    ($guid2,$sub) = decodeGuid( $guid );
    $name = $sub if ( $sub );
    print qq(  {$guid2,\n            "$name"},\n);
}

%substitutions = (
    # everything up to and including "mud" is defined
    "012dfac2", "Tractor",
    "012dfac3", "Combine Harvester",
    "012dfac7", "Front-End Loader",
    "fd163780", "Power Shovel",
    "fd163781", "Road Grader",
    "fd163784", "Road Roller",
    "fd163787", "Dump Truck",
    "5673d712", "Skid-Steer Loader",   # Bobcat is a registered trademark
    "b86045ac", "Highway Sign",
    "1e129e95", "Orange Cone",
    "adee7d54", "Barricade",
    "a170000f", "Flagger",
    "a425f90e", "Construction Sign",
    "0805b240", "Construction Flasher",
    "56721a6c", "Transit",
    # this group of 8 arrows is defined
    "83f91421", "Small Arrow Left",
    "83f91422", "Small Arrow Right",
    "83f91423", "Small Arrow Up",
    "83f91424", "Small Arrow Down",
    "83f91425", "Small Arrow Up Left",
    "83f91426", "Small Arrow Up Right",
    "83f91427", "Small Arrow Down Left",
    "83f91428", "Small Arrow Down Right",
    "83f91429", "Large Arrow Left",
    "83f9142a", "Large Arrow Right",
    "83f9142b", "Large Arrow Up",
    "83f9142c", "Large Arrow Down",
    "83f9142d", "Large Arrow Down Right",
    "83f9142e", "Large Arrow Down Left",
    "83f9142f", "Large Arrow Up Left",
    "83f91430", "Large Arrow Up Right",
    "8ff0aad1", "Accommodation",
    "af7bf199", "Australia",
    "6bbcc9d1", "Blue Dome Cross",
    "fff920fe", "Green Dome Cross", 
    "57e75924", "Business",
    "b09ef4a7", "Airplane",
    "f2833c22", "Amusement Recreation", # tent? on a green background
    "6f0317d6", "Green Square", 
    "18a6d3c0", "Red Triangle", 
    "86e68ea7", "Red Triangle and Green Square",
    "6afd74bf", "City 4",
    "49dfeb74", "White Square",
    "3eed62c6", "White Triangle",
    "6b521940", "Red Black Diamond Flag",
    "bb8ebaa3", "Yellow Diamond Flag",
    "8e118862", "Small Pink Square",
    "d0ef64c2", "Store",
    "a22b08fb", "Camping",
    "27f57c69", "Green Diamond Flag",
    "e07abb38", "Red Diamond Flag",
    "3a124ac9", "Red Green Diamond Flag",
    "64ed669b", "White Globe",
    "3cb10adc", "Yellow Globe",
    "2779347d", "", #???
    "3ad63f7b", "Black Cross",
    "3e89481e", "Church",
    "68622c10", "Small Dark Green Square",
    "42c6a873", "Small Black Square",
    "50e3b06e", "Danger",
    "369d0b22", "Construction Business",
    "10603b6c", "Airport",
    "8328aab7", "City 5",
    "96411287", "USA",
    "b2f98627", "Diver Down",
    "3fce26d0", "Light Yellow Square",
    "b4b68597", "Education Technology", 
    "35d2e6a8", "Computer",
    "4ddc4e96", "Amusement Recreation Red", 
    "79f58929", "Telephone Red",
    "0083b377", "Exit",
    "0c232891", "Exit with Services",
    "af63e7c2", "Pizza",
    "d419c693", "Financial Services",
    "70740a81", "City 3",
    "9a582ff6", "Food Store",
    "3cd31689", "", #???
    "952557a6", "", #??? white/black circle
    "03dc278c", "Driving Range",
    "acd28bab", "Golf Municipal",
    "984e7139", "Golf Private",
    "ec5828ab", "Golf Public",
    "b0120d99", "Golf Resort",
    "2ce7685a", "Golf Semi Private",
    "10397049", "Medical Service",
    "2fc28df6", "Home Furnishings",
    "910313db", "Industrial",
    "9e442c6e", "", #???
    "37e2fe4a", "", #???
    "3c756e09", "", #???
    "a1245b1c", "Manufacturing",
    "5bddbd7a", "Note",
    "cb6777e1", "City",
    "bc168c08", "Air Base",
    "a8857b0f", "Battlefield",
    "06db55c1", "Mining",
    "cc61b277", "Mountain",
    "fde13186", "Capital",
    "b14d90d1", "Route",
    "7eabc63f", "Overnight",
    "ac39d8b9", "Route End Active",
    "e1b9d86b", "Route End Inactive",
    "98712315", "Fuel Stop",
    "e5ea5b38", "Route Start Active",
    "18fd0d49", "Route Start Inactive",
    "2f52144b", "Route Stop Active",
    "faf8d826", "Route Stop Inactive",
    "ff44cae2", "Route Via",
    "5a50d59b", "Radiation Green",
    "19556023", "Radiation Red",
    "a54be251", "Electricity",
    "d793ff0c", "Personal Furnishings",
    "00f90733", "Personal Services",
    "ea677f24", "Telephone Black",
    "2d8a05b5", "Government Light",
    "40c64dfc", "Airport Red Square",
    "f27adb5d", "Propeller Aircraft",
    "5a718e13", "Jet Aircraft",
    "0a471039", "Government",
    "4a59da2f", "USA Regional",
    "f16500a9", "House 2",
    "7b05524d", "Picnic",
    "b88ad7a1", "Restaurant",
    "dc48a20a", "Store 2",
    "6b5ab040", "", # ???
    "153b2cff", "Blue Star",
    "f276f6b3", "", # ???
    "91d242c8", "Running",
    "8b0078db", "Transportation",
    "0599f6c9", "Fishing 2",
    "7389128c", "Automotive",
    "0362b593", "Cloudy",
    "f0717a94", "Partly Cloudy",
    "14486bbc", "Mostly Cloudy",
    "7a258c70", "Hurricane",
    "eff260d4", "Lightning",
    "c3d70220", "Rain",
    # everything else is defined
    # They defined two red flags.  Ooops.
    "f2dfbc95", "Red Flag 2"
);

sub print_header {
print <<'END';
/* 









                       THIS FILE IS AUTOMATICALLY GENERATED


                       Please change make-an1sym.pl and
                       regenerate it rather than changing
                       this file directly.























*/

/*
    Read DeLorme drawing files (.an1) - supplemental (included by an1.c)
 
    Copyright (C) 2005 Ron Parker and Robert Lipe.

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

 */

static struct defguid {
	GUID guid;
	const char *name;
} default_guids[] = {
END
}

sub print_footer {
print <<'END';
};

static int FindIconByName( const char *name, GUID *guid ) {
	unsigned int i = 0; 
	for ( i = 0; i < (sizeof(default_guids)/sizeof(struct defguid)); i++ )
	{
		if ( !case_ignore_strcmp(name, default_guids[i].name)) {
			memcpy( guid, &(default_guids[i].guid), sizeof(GUID));
			return 1;
		}
	}
	return 0;
}

static int FindIconByGuid( GUID *guid, char **name ) {
	unsigned int i = 0; 
	for ( i = 0; i < (sizeof(default_guids)/sizeof(struct defguid)); i++ )
	{
		if ( !memcmp(guid, &default_guids[i].guid, sizeof(GUID))) {
			*name = default_guids[i].name;
			return 1;
		}
	}
	return 0;
}
END
}


# read file
undef $/;
$file = <>;

# read file header
($magic, $unk1 ) = shiftunpack( 'ss' );

# read bitmap info
($unk2) = shiftunpack( 'l' );

print_header;

while ( length($file) ) {
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
  elsif ($rec_type == 0 ) { # crap
    ($a, $b, $c, $d, $e, $f) = shiftunpack( 'llllll' );
    if ( $c ) { 
      $file = pack( 'llllll', ($a, $b, $c, $d, $e, $f)) . $file;
      DoImage;
    }
  }
  else {
    DoImage;
  }
} 

print_footer;
