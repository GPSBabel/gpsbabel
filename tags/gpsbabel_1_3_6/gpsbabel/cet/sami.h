/*

    Data automatically generated from recode output:

        'recode -lf "sami" 2>/dev/null'


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

#ifndef sami_h
#define sami_h

#define cet_cs_name_sami "sami"

const char *cet_cs_alias_sami[] = 
{
	"sami", "iso-ir-158", "lap", "latin-lap",
	NULL
};

#define cet_ucs4_ofs_sami 160
#define cet_ucs4_cnt_sami 80

const int cet_ucs4_map_sami[cet_ucs4_cnt_sami] =
{
	0x00b4,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x02bb,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x0102, 0x00c0, 0x01de, 0x01e0, 0x01e2, 0x0114, 0x00c8, 0x01e4, 
	0x01e6, 0x01e8, 0x014e, 0x00d2, 0x01ea, 0x01ec, 0x01b7, 0x01ee, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x0103, 0x00e0, 0x01df, 0x01e1, 0x01e3, 0x0115, 0x00e8, 0x01e5, 
	0x01e7, 0x01e9, 0x014f, 0x00f2, 0x01eb, 0x01ed, 0x0292, 0x01ef
};

#define cet_ucs4_to_sami_ct 34

const cet_ucs4_link_t cet_ucs4_to_sami_links[cet_ucs4_to_sami_ct] =
{
	 {0x00b4, 0xa0} /* accent */,
	 {0x00c0, 0xc1} /* capital letter a with grave */,
	 {0x00c8, 0xc6} /* capital letter e with grave */,
	 {0x00d2, 0xcb} /* capital letter o with grave */,
	 {0x00e0, 0xe1} /* small letter a with grave */,
	 {0x00e8, 0xe6} /* small letter e with grave */,
	 {0x00f2, 0xeb} /* small letter o with grave */,
	 {0x0102, 0xc0} /* capital letter a with breve */,
	 {0x0103, 0xe0} /* small letter a with breve */,
	 {0x0114, 0xc5} /* capital letter e with breve */,
	 {0x0115, 0xe5} /* small letter e with breve */,
	 {0x014e, 0xca} /* capital letter o with breve */,
	 {0x014f, 0xea} /* small letter o with breve */,
	 {0x01b7, 0xce} /* capital letter ezh */,
	 {0x01de, 0xc2} /* capital letter a with diaeresis and macron */,
	 {0x01df, 0xe2} /* small letter a with diaeresis and macron */,
	 {0x01e0, 0xc3} /* capital letter a with dot above and macron */,
	 {0x01e1, 0xe3} /* small letter a with dot above and macron */,
	 {0x01e2, 0xc4} /* capital letter ae with macron */,
	 {0x01e3, 0xe4} /* small letter ae with macron */,
	 {0x01e4, 0xc7} /* capital letter g with stroke */,
	 {0x01e5, 0xe7} /* small letter g with stroke */,
	 {0x01e6, 0xc8} /* capital letter g with caron */,
	 {0x01e7, 0xe8} /* small letter g with caron */,
	 {0x01e8, 0xc9} /* capital letter k with caron */,
	 {0x01e9, 0xe9} /* small letter k with caron */,
	 {0x01ea, 0xcc} /* capital letter o with ogonek */,
	 {0x01eb, 0xec} /* small letter o with ogonek */,
	 {0x01ec, 0xcd} /* capital letter o with ogonek and macron */,
	 {0x01ed, 0xed} /* small letter o with ogonek and macron */,
	 {0x01ee, 0xcf} /* capital letter ezh with caron */,
	 {0x01ef, 0xef} /* small letter ezh with caron */,
	 {0x0292, 0xee} /* small letter ezh (iso-ir-158 142) */,
	 {0x02bb, 0xb0} /* letter left half ring */
};

/*
#define cet_ucs4_to_sami_extra_ct 0
const cet_ucs4_link_t cet_ucs4_to_sami_extra[cet_ucs4_to_sami_extra_ct] = {};
*/

cet_cs_vec_t cet_cs_vec_sami =	/* defined in cet.h */
{	
	cet_cs_name_sami,		/* name of character set	*/
	cet_cs_alias_sami,		/* alias table			*/

	NULL,				/* ... to UCS-4 converter (multi-byte) */
	NULL,				/* UCS-4 to ... converter (multi-byte) */

	cet_ucs4_map_sami,		/* char to UCS-4 value table	*/
	cet_ucs4_ofs_sami,		/* first non standard character	*/
	cet_ucs4_cnt_sami,		/* number of values in table	*/

	cet_ucs4_to_sami_links,	/* UCS-4 to char links		*/
	cet_ucs4_to_sami_ct,		/* number of links		*/

	NULL,				/* hand made UCS-4 links	*/
	0,				/* number of extra links	*/

	NULL	/* for internal use */
};


/*
const int sami_ucs4_full_map[] =
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
	0x00b4,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x02bb,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x0102, 0x00c0, 0x01de, 0x01e0, 0x01e2, 0x0114, 0x00c8, 0x01e4, 
	0x01e6, 0x01e8, 0x014e, 0x00d2, 0x01ea, 0x01ec, 0x01b7, 0x01ee, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x0103, 0x00e0, 0x01df, 0x01e1, 0x01e3, 0x0115, 0x00e8, 0x01e5, 
	0x01e7, 0x01e9, 0x014f, 0x00f2, 0x01eb, 0x01ed, 0x0292, 0x01ef, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1
};
*/

#endif
