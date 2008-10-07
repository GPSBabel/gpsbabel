/*

    Data automatically generated from recode output:

        'recode -lf "KOI-7" 2>/dev/null'


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

#ifndef koi_7_h
#define koi_7_h

#define cet_cs_name_koi_7 "KOI-7"

const char *cet_cs_alias_koi_7[] = 
{
	"KOI-7", NULL
};

#define cet_ucs4_ofs_koi_7 36
#define cet_ucs4_cnt_koi_7 92

const int cet_ucs4_map_koi_7[cet_ucs4_cnt_koi_7] =
{
	0x00a4, 0x0025, 0x0026, 0x0027, 0x0028, 0x0029, 0x002a, 0x002b, 
	0x002c, 0x002d, 0x002e, 0x002f, 0x0030, 0x0031, 0x0032, 0x0033, 
	0x0034, 0x0035, 0x0036, 0x0037, 0x0038, 0x0039, 0x003a, 0x003b, 
	0x003c, 0x003d, 0x003e, 0x003f, 0x0040, 0x0041, 0x0042, 0x0043, 
	0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0x0049, 0x004a, 0x004b, 
	0x004c, 0x004d, 0x004e, 0x004f, 0x0050, 0x0051, 0x0052, 0x0053, 
	0x0054, 0x0055, 0x0056, 0x0057, 0x0058, 0x0059, 0x005a, 0x005b, 
	0x005c, 0x005d, 0x005e, 0x005f, 0x042e, 0x0410, 0x0411, 0x0426, 
	0x0414, 0x0415, 0x0424, 0x0413, 0x0425, 0x0418, 0x0419, 0x041a, 
	0x041b, 0x041c, 0x041d, 0x041e, 0x041f, 0x042f, 0x0420, 0x0421, 
	0x0422, 0x0423, 0x0416, 0x0412, 0x042c, 0x042b, 0x0417, 0x0428, 
	0x042d, 0x0429, 0x0427, 0x007f
};

#define cet_ucs4_to_koi_7_ct 32

const cet_ucs4_link_t cet_ucs4_to_koi_7_links[cet_ucs4_to_koi_7_ct] =
{
	 {0x00a4, 0x24} /* sign */,
	 {0x0410, 0x61} /* capital letter a */,
	 {0x0411, 0x62} /* capital letter be */,
	 {0x0412, 0x77} /* capital letter ve */,
	 {0x0413, 0x67} /* capital letter ghe */,
	 {0x0414, 0x64} /* capital letter de */,
	 {0x0415, 0x65} /* capital letter ie */,
	 {0x0416, 0x76} /* capital letter zhe */,
	 {0x0417, 0x7a} /* capital letter ze */,
	 {0x0418, 0x69} /* capital letter i */,
	 {0x0419, 0x6a} /* capital letter short i */,
	 {0x041a, 0x6b} /* capital letter ka */,
	 {0x041b, 0x6c} /* capital letter el */,
	 {0x041c, 0x6d} /* capital letter em */,
	 {0x041d, 0x6e} /* capital letter en */,
	 {0x041e, 0x6f} /* capital letter o */,
	 {0x041f, 0x70} /* capital letter pe */,
	 {0x0420, 0x72} /* capital letter er */,
	 {0x0421, 0x73} /* capital letter es */,
	 {0x0422, 0x74} /* capital letter te */,
	 {0x0423, 0x75} /* capital letter u */,
	 {0x0424, 0x66} /* capital letter ef */,
	 {0x0425, 0x68} /* capital letter ha */,
	 {0x0426, 0x63} /* capital letter tse */,
	 {0x0427, 0x7e} /* capital letter che */,
	 {0x0428, 0x7b} /* capital letter sha */,
	 {0x0429, 0x7d} /* capital letter shcha */,
	 {0x042b, 0x79} /* capital letter yeru */,
	 {0x042c, 0x78} /* capital letter soft sign */,
	 {0x042d, 0x7c} /* capital letter e */,
	 {0x042e, 0x60} /* capital letter yu */,
	 {0x042f, 0x71} /* capital letter ya */
};

/*
#define cet_ucs4_to_koi_7_extra_ct 0
const cet_ucs4_link_t cet_ucs4_to_koi_7_extra[cet_ucs4_to_koi_7_extra_ct] = {};
*/

cet_cs_vec_t cet_cs_vec_koi_7 =	/* defined in cet.h */
{	
	cet_cs_name_koi_7,		/* name of character set	*/
	cet_cs_alias_koi_7,		/* alias table			*/

	NULL,				/* ... to UCS-4 converter (multi-byte) */
	NULL,				/* UCS-4 to ... converter (multi-byte) */

	cet_ucs4_map_koi_7,		/* char to UCS-4 value table	*/
	cet_ucs4_ofs_koi_7,		/* first non standard character	*/
	cet_ucs4_cnt_koi_7,		/* number of values in table	*/

	cet_ucs4_to_koi_7_links,	/* UCS-4 to char links		*/
	cet_ucs4_to_koi_7_ct,		/* number of links		*/

	NULL,				/* hand made UCS-4 links	*/
	0,				/* number of extra links	*/

	NULL	/* for internal use */
};


/*
const int koi_7_ucs4_full_map[] =
{
	0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 
	0x0008, 0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f, 
	0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 
	0x0018, 0x0019, 0x001a, 0x001b, 0x001c, 0x001d, 0x001e, 0x001f, 
	0x0020, 0x0021, 0x0022, 0x0023, 0x00a4, 0x0025, 0x0026, 0x0027, 
	0x0028, 0x0029, 0x002a, 0x002b, 0x002c, 0x002d, 0x002e, 0x002f, 
	0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
	0x0038, 0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e, 0x003f, 
	0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
	0x0048, 0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f, 
	0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
	0x0058, 0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f, 
	0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413, 
	0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 
	0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412, 
	0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427, 0x007f, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, 
	    -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1
};
*/

#endif
