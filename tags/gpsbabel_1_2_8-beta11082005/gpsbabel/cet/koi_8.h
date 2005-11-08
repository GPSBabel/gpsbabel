/*

    Data automatically generated from recode output:

        'recode -lf "KOI-8" 2>/dev/null'


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

#ifndef koi_8_h
#define koi_8_h

#define cet_cs_name_koi_8 "KOI-8"

const char *cet_cs_alias_koi_8[] = 
{
	"KOI-8", "GOST_19768-74", NULL
};

#define cet_ucs4_ofs_koi_8 128
#define cet_ucs4_cnt_koi_8 127

const int cet_ucs4_map_koi_8[cet_ucs4_cnt_koi_8] =
{
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x044e, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433, 
	0x0445, 0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, 
	0x043f, 0x044f, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432, 
	0x044c, 0x044b, 0x0437, 0x0448, 0x044d, 0x0449, 0x0447, 0x044a, 
	0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413, 
	0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 
	0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412, 
	0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427
};

#define cet_ucs4_to_koi_8_ct 63

const cet_ucs4_link_t cet_ucs4_to_koi_8_links[cet_ucs4_to_koi_8_ct] =
{
	 {0x0410, 0xe1} /* capital letter a */,
	 {0x0411, 0xe2} /* capital letter be */,
	 {0x0412, 0xf7} /* capital letter ve */,
	 {0x0413, 0xe7} /* capital letter ghe */,
	 {0x0414, 0xe4} /* capital letter de */,
	 {0x0415, 0xe5} /* capital letter ie */,
	 {0x0416, 0xf6} /* capital letter zhe */,
	 {0x0417, 0xfa} /* capital letter ze */,
	 {0x0418, 0xe9} /* capital letter i */,
	 {0x0419, 0xea} /* capital letter short i */,
	 {0x041a, 0xeb} /* capital letter ka */,
	 {0x041b, 0xec} /* capital letter el */,
	 {0x041c, 0xed} /* capital letter em */,
	 {0x041d, 0xee} /* capital letter en */,
	 {0x041e, 0xef} /* capital letter o */,
	 {0x041f, 0xf0} /* capital letter pe */,
	 {0x0420, 0xf2} /* capital letter er */,
	 {0x0421, 0xf3} /* capital letter es */,
	 {0x0422, 0xf4} /* capital letter te */,
	 {0x0423, 0xf5} /* capital letter u */,
	 {0x0424, 0xe6} /* capital letter ef */,
	 {0x0425, 0xe8} /* capital letter ha */,
	 {0x0426, 0xe3} /* capital letter tse */,
	 {0x0427, 0xfe} /* capital letter che */,
	 {0x0428, 0xfb} /* capital letter sha */,
	 {0x0429, 0xfd} /* capital letter shcha */,
	 {0x042b, 0xf9} /* capital letter yeru */,
	 {0x042c, 0xf8} /* capital letter soft sign */,
	 {0x042d, 0xfc} /* capital letter e */,
	 {0x042e, 0xe0} /* capital letter yu */,
	 {0x042f, 0xf1} /* capital letter ya */,
	 {0x0430, 0xc1} /* small letter a */,
	 {0x0431, 0xc2} /* small letter be */,
	 {0x0432, 0xd7} /* small letter ve */,
	 {0x0433, 0xc7} /* small letter ghe */,
	 {0x0434, 0xc4} /* small letter de */,
	 {0x0435, 0xc5} /* small letter ie */,
	 {0x0436, 0xd6} /* small letter zhe */,
	 {0x0437, 0xda} /* small letter ze */,
	 {0x0438, 0xc9} /* small letter i */,
	 {0x0439, 0xca} /* small letter short i */,
	 {0x043a, 0xcb} /* small letter ka */,
	 {0x043b, 0xcc} /* small letter el */,
	 {0x043c, 0xcd} /* small letter em */,
	 {0x043d, 0xce} /* small letter en */,
	 {0x043e, 0xcf} /* small letter o */,
	 {0x043f, 0xd0} /* small letter pe */,
	 {0x0440, 0xd2} /* small letter er */,
	 {0x0441, 0xd3} /* small letter es */,
	 {0x0442, 0xd4} /* small letter te */,
	 {0x0443, 0xd5} /* small letter u */,
	 {0x0444, 0xc6} /* small letter ef */,
	 {0x0445, 0xc8} /* small letter ha */,
	 {0x0446, 0xc3} /* small letter tse */,
	 {0x0447, 0xde} /* small letter che */,
	 {0x0448, 0xdb} /* small letter sha */,
	 {0x0449, 0xdd} /* small letter shcha */,
	 {0x044a, 0xdf} /* small letter hard sign */,
	 {0x044b, 0xd9} /* small letter yeru */,
	 {0x044c, 0xd8} /* small letter soft sign */,
	 {0x044d, 0xdc} /* small letter e */,
	 {0x044e, 0xc0} /* small letter yu */,
	 {0x044f, 0xd1} /* small letter ya */
};

/*
#define cet_ucs4_to_koi_8_extra_ct 0
const cet_ucs4_link_t cet_ucs4_to_koi_8_extra[cet_ucs4_to_koi_8_extra_ct] = {};
*/

cet_cs_vec_t cet_cs_vec_koi_8 =	/* defined in cet.h */
{	
	cet_cs_name_koi_8,		/* name of character set	*/
	cet_cs_alias_koi_8,		/* alias table			*/

	NULL,				/* ... to UCS-4 converter (multi-byte) */
	NULL,				/* UCS-4 to ... converter (multi-byte) */

	cet_ucs4_map_koi_8,		/* char to UCS-4 value table	*/
	cet_ucs4_ofs_koi_8,		/* first non standard character	*/
	cet_ucs4_cnt_koi_8,		/* number of values in table	*/

	cet_ucs4_to_koi_8_links,	/* UCS-4 to char links		*/
	cet_ucs4_to_koi_8_ct,		/* number of links		*/

	NULL,				/* hand made UCS-4 links	*/
	0,				/* number of extra links	*/

	NULL	/* for internal use */
};


/*
const int koi_8_ucs4_full_map[] =
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
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	0x044e, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433, 
	0x0445, 0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, 
	0x043f, 0x044f, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432, 
	0x044c, 0x044b, 0x0437, 0x0448, 0x044d, 0x0449, 0x0447, 0x044a, 
	0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413, 
	0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 
	0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412, 
	0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427,     -1
};
*/

#endif
