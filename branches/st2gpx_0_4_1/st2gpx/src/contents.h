/*
	contents.h

	Extract data from MS Streets & Trips .est and Autoroute .axe files in GPX format.

    Copyright (C) 2003 James Sherring, james_sherring@yahoo.com

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


	This app depends on istorage & istorage-make from Pabs (pabs3@zip.to)
	and James Clark's Expat xml parser from http://www.libexpat.org/.

*/
#ifdef	__cplusplus
extern "C" {
#endif

typedef struct f_contents0
{
	// @ 00
	unsigned short usunkn0; // 0c 00
	float	map_center_X;
	float	map_center_Y;
	float	map_center_Z;
	// in km?
	float	map_scale;
	// @ 12
	int		iunkn0;	// 00 00 00 00
	// next two are interesting when there are ppins imported from a file
	// got 180213657 = ABDD799
	// and -1073143305 = C00921F7
	int		iunkn1;
	int		iunkn2;
	int		iunkn3; // 20 03 00 00
	// @ 22
	int		iunkn4; // 01 00 00 00
	int		iunkn5; // 06 00 00 00
	int		iunkn6; // c4 09 00 00
	// 0=legend/route directions if
	// 1=no legend
	// (related to iunkn13)
	unsigned short legend_or_directions; // 00 00 / 01 00
	// @ 30
	int		iunkn7; // 01 00 00 00
	int		iunkn8; // 06 00 00 00
	int     iunkn9; // e4 0c 00 00 for eur8, c4 09 00 00 for usa9
	unsigned short usunkn2; // 00 00
	int		iunkn10; // 1
	// @ 42
	int		iunkn11; // 06 00 00 00
	int     iunkn12; // XX XX 00 00
	unsigned short iunkn13; // 00 00

	// Does not seem to match these values...
	//GeoPaneState Value Description 
	//geoPaneLegend 0 Legend is displayed 
	//geoPaneNearbyPlaces 2 Find Nearby Places pane is displayed 
	//geoPaneNone 3 Only the map is displayed 
	//geoPaneRoutePlanner 1 Route Planner pane is displayed 
	//geoPaneTerritory 4 Territory Manager pane is displayed 

	// 0=no legend or show route directions ( iunkn6)
	// 1=normal
	// 2=route planner
	// 4=find nearby places
	int		map_format_pannels;
	// @ 50
	int		iunkn14; // 1 0 4 0

//	GeoMapStyle Value Description 
//	geoMapStyleData 2 Data map 
//	geoMapStylePolitical 4 Political map 
//	geoMapStyleRoad 0 Road map 
//	geoMapStyleRoadData 1 Road and data map 
//	geoMapStyleTerrain 3 Terrain map 
  
	// 0 = Road
	// 3 = Terrain
	// 4 = Political
	int     map_format_style;
	int		iunkn16; // 1 0 4 0
	// 0-4,
	// 0=largest
	// 4=smallest
	int		map_format_font_size; // 03 00 00 00
	// @ 60
	unsigned short usunkn4;    //03 00
	//length of section from here until end of EUR/USA string
	int		section_len;
	unsigned short usunkn5; // 03 00
	// array_len = 0x14 + 2*num ppin sets
	// if array_len=0x14, str1@81. if array_len=0x16 str1@83
	unsigned int		array_len;  		// xx 00 00 00 (14/16)
} tag_f_contents0;

/*
typedef struct f_contents_array
{
	unsigned short usunkn6; // 03 00
	unsigned short usunkn7; // 02/01 00
	// @ 70
	unsigned short usunkn8; // 00 00
	// number of user ppin sets
	unsigned short usunkn9; // 00/01 00 (0 for empty, 1 for ppins?)
	// from UserData/GEODB_LastId/LastSetId
	// note the size mismatch: short/long
	unsigned short LastSetId;
	unsigned short usunkn9_1; // 0
	unsigned short usunkn9_2; // 0
	unsigned short usunkn10; // 0
	unsigned short usunkn10_1; // 0
	unsigned short usunkn10_2; // 0
	int		iunkn19;		 // 3
	// @ 80
	unsigned short usunkn11; // 0
} tag_f_contents_array;
*/

// + 	unsigned char cbText0;
// + cbText0 bytes of text

// @ 8b

typedef struct f_contents1
{
	int		iunkn20;		 // 00 00 00 00/ff ff ff ff
	unsigned char cbText1;
} tag_f_contents1;

// + cbText1 bytes of text
// @ 98
typedef struct f_contents2
{
	int		iunkn21;		 // ff ff ff ff
	// This is a count of strings before contents3
	unsigned short count_strings; // 3
} tag_f_contents2;

//	unsigned char cbText2;
// + cbText2 bytes of text
// @ a6
// + unsigned char cbText3
// + cbText3 bytes of text
// @ ae
// + optional ? unsigned char cbText4
// + optional ? cbText4 bytes of text
// @ b7

//+	unsigned short usunkn14;
// + int (if usunkn14)

typedef struct f_contents3
{
	unsigned short usunkn15;
	unsigned short usunkn16;
	unsigned short usunkn17;
	unsigned short usunkn18;
	// FIXME + an extra short for usa9, + 2 extra shorts for usa10
	unsigned char cbCountryText;
} tag_f_contents3;

// + cbText5 bytes of text: USA/EUR

// more for USA9
// + int
// + unsigned char cbText6
// + cbText6 bytes of text
// + unsigned char cbText7
// + cbText7 bytes of text : path to html: html export file?
// + 0x40 bytes


typedef struct contents
{
	int fully_parsed_flag;
	unsigned int buf_len;
	// The data as read from the contents stream.
	char* buf;
	struct f_contents0 * f_conts0;
	unsigned short * f_conts_array;
	unsigned char * f_pcbtext0;
	char* f_text0;
	struct f_contents1 * f_conts1;
	char* f_text1;
	struct f_contents2 * f_conts2;
	// Lengths for conts2->count_strings number of strings.
	// Note that this is array of *pointers* to string-length
	unsigned char ** list_f_pcbtext;
	// conts2->count_strings number of strings.
	// Strings are not null-terminated,
	// but they are prefixed with their length.
	char **	list_f_text;
	// Normally 0, sometimes 1
	unsigned short * pusunkn0;
	// Only when usunkn0=1 ???
	int * piunkn1;
	struct f_contents3 * f_conts3;
	char * CountryText;
	// The end part of the buffer that has not been interpreted.
	char* rest;
} tag_contents;

struct contents * read_contents(char* conts_file_name);
struct contents * contents_insert_ppinset(struct contents * old_conts, unsigned short newSetId);
void write_contents(struct contents * conts, char* conts_file_name);
void contents_delete(struct contents * conts);

#ifdef	__cplusplus
}
#endif

