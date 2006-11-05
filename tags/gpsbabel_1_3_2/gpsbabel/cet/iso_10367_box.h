/*

    Data automatically generated from recode output:

        'recode -lf "ISO_10367-box" 2>/dev/null'


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

#ifndef iso_10367_box_h
#define iso_10367_box_h

#define cet_cs_name_iso_10367_box "ISO_10367-box"

const char *cet_cs_alias_iso_10367_box[] = 
{
	"ISO_10367-box", "iso-ir-155", NULL
};

#define cet_ucs4_ofs_iso_10367_box 160
#define cet_ucs4_cnt_iso_10367_box 62

const int cet_ucs4_map_iso_10367_box[cet_ucs4_cnt_iso_10367_box] =
{
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x2551, 0x2550, 0x2554, 0x2557, 0x255a, 0x255d, 0x2560, 0x2563, 
	0x2566, 0x2569, 0x256c, 0xe019, 0x2584, 0x2588, 0x25aa,     -1, 
	0x2502, 0x2500, 0x250c, 0x2510, 0x2514, 0x2518, 0x251c, 0x2524, 
	0x252c, 0x2534, 0x253c, 0x2591, 0x2592, 0x2593
};

#define cet_ucs4_to_iso_10367_box_ct 29

const cet_ucs4_link_t cet_ucs4_to_iso_10367_box_links[cet_ucs4_to_iso_10367_box_ct] =
{
	 {0x2500, 0xd1} /* drawings light horizontal */,
	 {0x2502, 0xd0} /* drawings light vertical */,
	 {0x250c, 0xd2} /* drawings light down and right */,
	 {0x2510, 0xd3} /* drawings light down and left */,
	 {0x2514, 0xd4} /* drawings light up and right */,
	 {0x2518, 0xd5} /* drawings light up and left */,
	 {0x251c, 0xd6} /* drawings light vertical and right */,
	 {0x2524, 0xd7} /* drawings light vertical and left */,
	 {0x252c, 0xd8} /* drawings light down and horizontal */,
	 {0x2534, 0xd9} /* drawings light up and horizontal */,
	 {0x253c, 0xda} /* drawings light vertical and horizontal */,
	 {0x2550, 0xc1} /* drawings heavy horizontal */,
	 {0x2551, 0xc0} /* drawings heavy vertical */,
	 {0x2554, 0xc2} /* drawings heavy down and right */,
	 {0x2557, 0xc3} /* drawings heavy down and left */,
	 {0x255a, 0xc4} /* drawings heavy up and right */,
	 {0x255d, 0xc5} /* drawings heavy up and left */,
	 {0x2560, 0xc6} /* drawings heavy vertical and right */,
	 {0x2563, 0xc7} /* drawings heavy vertical and left */,
	 {0x2566, 0xc8} /* drawings heavy down and horizontal */,
	 {0x2569, 0xc9} /* drawings heavy up and horizontal */,
	 {0x256c, 0xca} /* drawings heavy vertical and horizontal */,
	 {0x2584, 0xcc} /* half block */,
	 {0x2588, 0xcd} /* block */,
	 {0x2591, 0xdb} /* shade */,
	 {0x2592, 0xdc} /* shade */,
	 {0x2593, 0xdd} /* shade */,
	 {0x25aa, 0xce} /* small square */,
	 {0xe019, 0xcb} /* space b (iso-ir-8-1 096) */
};

/*
#define cet_ucs4_to_iso_10367_box_extra_ct 0
const cet_ucs4_link_t cet_ucs4_to_iso_10367_box_extra[cet_ucs4_to_iso_10367_box_extra_ct] = {};
*/

cet_cs_vec_t cet_cs_vec_iso_10367_box =	/* defined in cet.h */
{	
	cet_cs_name_iso_10367_box,		/* name of character set	*/
	cet_cs_alias_iso_10367_box,		/* alias table			*/

	NULL,				/* ... to UCS-4 converter (multi-byte) */
	NULL,				/* UCS-4 to ... converter (multi-byte) */

	cet_ucs4_map_iso_10367_box,		/* char to UCS-4 value table	*/
	cet_ucs4_ofs_iso_10367_box,		/* first non standard character	*/
	cet_ucs4_cnt_iso_10367_box,		/* number of values in table	*/

	cet_ucs4_to_iso_10367_box_links,	/* UCS-4 to char links		*/
	cet_ucs4_to_iso_10367_box_ct,		/* number of links		*/

	NULL,				/* hand made UCS-4 links	*/
	0,				/* number of extra links	*/

	NULL	/* for internal use */
};


/*
const int iso_10367_box_ucs4_full_map[] =
{
	0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 
	0x0008, 0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f, 
	0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 
	0x0018, 0x0019, 0x001a, 0x001b, 0x001c, 0x001d, 0x001e, 0x001f, 
	0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 
	0x0028, 0x0029, 0x002a, 0x002b, 0x002c, 0x002d, 0x002e, 0x002f, 
	0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
	0x0038, 0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e, 0x003f, 
	0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
	0x0048, 0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f, 
	0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
	0x0058, 0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f, 
	0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 
	0x0068, 0x0069, 0x006a, 0x006b, 0x006c, 0x006d, 0x006e, 0x006f, 
	0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 
	0x0078, 0x0079, 0x007a, 0x007b, 0x007c, 0x007d, 0x007e, 0x007f, 
	0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
	0x0088, 0x0089, 0x008a, 0x008b, 0x008c, 0x008d, 0x008e, 0x008f, 
	0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
	0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x2551, 0x2550, 0x2554, 0x2557, 0x255a, 0x255d, 0x2560, 0x2563, 
	0x2566, 0x2569, 0x256c, 0xe019, 0x2584, 0x2588, 0x25aa,     -1, 
	0x2502, 0x2500, 0x250c, 0x2510, 0x2514, 0x2518, 0x251c, 0x2524, 
	0x252c, 0x2534, 0x253c, 0x2591, 0x2592, 0x2593,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1
};
*/

#endif
