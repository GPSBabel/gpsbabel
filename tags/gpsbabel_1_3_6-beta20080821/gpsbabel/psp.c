/*
    PocketStreets 2002 Pushpin Files
    Contributed to gpsbabel by Alex Mottram (geo_alexm at cox-internet.com)
    
    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

#include "defs.h"
#include "cet.h"
#include "cet_util.h"
#include <ctype.h>
#include "grtcirc.h"

#define MYNAME	"PSP"

#define MAXPSPSTRINGSIZE	256
#define MAXPSPOUTPUTPINS	8192   /* Any more points than this is ludicrous */

static gbfile *psp_file_in;
static gbfile *psp_file_out;
static short_handle mkshort_handle;

#define psp_fread(b,s,m,f) gbfread((b),(s),(m),f)
#define psp_fread_double(f) gbfgetdbl(f)
#define psp_fwrite_double(v,f) gbfputdbl((v),f)

/* ToDo: move the code inside to CET library */
static void
psp_write_str(const char *str)
{
	if (str && *str) {
		short *unicode;
		int len;
		
		/* convert UTF-8 string into a unicode sequence */
		/* not perfect, but enough for us */
		unicode = cet_str_any_to_uni(str, global_opts.charset, &len);
		if (len > MAXPSPSTRINGSIZE) len = MAXPSPSTRINGSIZE;
		gbfputc((unsigned char)len, psp_file_out);
		if (len) gbfwrite(unicode, 2, len, psp_file_out);

		xfree(unicode);
	}
	else
		gbfputc(0, psp_file_out);
}

/* ToDo: move the code inside to CET library */
static char *
psp_read_str(gbfile *fin)
{
	int len;
	gbint16 *buff;
	char *res;
	
	len = (unsigned char)gbfgetc(fin);
	if (len == 0) return NULL;
	
	buff = xmalloc(len * sizeof(*buff));
	gbfread(buff, sizeof(*buff), len, fin);
	res = cet_str_uni_to_utf8(buff, len);
	xfree(buff);

	return res;
}

/* Implement the grid in ascii art... This makes a bit of sense if you stand
   on a point over the north pole and look down on the earth.

-180   -90        0       90      180 
------------------------------------    /\
| 0x03  U|S 0x02 U|k 0x00  | 0x01   |   90
|--------|--------|--------|--------|   0
| 0x07   |  0x06  |  0x04  | 0x05   |   -90
------------------------------------    \/
*/    
static 
char grid_byte(double lat, double lon)
{
    char c = 0x00;
    
    if ((lon >= 0.0) && (lon < 90.0)) {
        if (lat >= 0.0) {
            c = 0x00;
        } else {
            c = 0x04;
        }
    } else
    if (lon >= 90.0) {
        if (lat >= 0.0) {
            c = 0x01;
        } else {
            c = 0x05;
        }
    } else
    if ((lon < 0.0) && (lon >= -90.0)) {
        if (lat >= 0.0) {
            c = 0x02;
        } else {
            c = 0x06;
        }
    } else 
    if (lon < -90.0) {
        if (lat >= 0.0) {
            c = 0x03;
        } else {
            c = 0x07;
        }
    }
        
    return (c);
}    

void decode_psp_coordinates(double * lat, double * lon, const char lonbyte)
{
    /* This is some sort of 1/2 Polar,  1/2 Cartesian coordinate mess in  */
    /* the pin file.  I really shouldn't have to do this. Zones 02 and 03 */
    /* work properly.  The other zones are assumptions based on 02 and 03 */

    if ((lonbyte == 0x02) || (lonbyte == 0x06)) {
        /* one step west of zero longitude */
        if (*lon > 0.0) 
            *lon *= -1.0;
    } else 
    if ((lonbyte == 0x03) || (lonbyte == 0x07)) {
        /* two steps west of zero longitude */
        if (*lon > 0.0)
            *lon -= 180.0;
    } else 
    if ((lonbyte == 0x00) || (lonbyte == 0x04)) {
        /* one step east of zero longitude */
        if (*lon < 0.0)
            *lon *= -1.0;
    } else 
    if ((lonbyte == 0x01) || (lonbyte == 0x05)) {
        /* two steps east of zero longitude */
        if (*lon < 0.0)
            *lon += 180.0;
    }
}

static int
valid_psp_header(char * header)
{
    char header_bytes[] = { 0x31, 0x6E, 0x69, 0x50 }; /* 1niP <stop> */

    return (memcmp(header_bytes, header, 4));
    
}

static void
psp_rd_init(const char *fname)
{
	psp_file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
psp_rd_deinit(void)
{
	gbfclose(psp_file_in);
}

static void
psp_wr_init(const char *fname)
{
	psp_file_out = gbfopen_le(fname, "wb", MYNAME);
	mkshort_handle = mkshort_new_handle();
}

static void
psp_wr_deinit(void)
{
	mkshort_del_handle(&mkshort_handle);
	gbfclose(psp_file_out);
}

static void
psp_read(void)
{
	char buff[MAXPSPSTRINGSIZE + 1];
	double radians;
	double lat, lon;
	waypoint *wpt_tmp;
	short int pincount;
	short int pindex;
        char gridbyte = 0x00;
	char *tmp;

        /* 32 bytes - file header */
        psp_fread(&buff[0], 1, 32, psp_file_in);

        if (valid_psp_header(buff) != 0) {
            fatal(MYNAME ": input file does not appear to be a valid .PSP file.\n");
        }

	pincount = le_read16(&buff[12]);

	while (pincount--) {
	    wpt_tmp = waypt_new();

            wpt_tmp->altitude = unknown_alt;
            
            /* offset 0x20 - 0x21 pin index */
    	    psp_fread(&pindex, 1, 2, psp_file_in);

            /* offset 0x22 - 0x23 */
    	    psp_fread(&buff[0], 1, 2, psp_file_in);

            /* offset 0x24 */
            /* 1 byte, the grid byte - needed for sign corrections later*/
            psp_fread(&gridbyte, 1, 1, psp_file_in);

            /* 8 bytes - latitude in radians */
	    radians = psp_fread_double(psp_file_in);
            lat = DEG(radians);

            /* 8 bytes - longitude in radians */
	    radians = psp_fread_double(psp_file_in);
            lon = DEG(radians);

            /* since we don't know the origin of this PSP file, we use  */
            /* the grid byte adjust longitude, if necessary, mimicing   */
            /* the behavior of pocketstreets correcting the data.  This */
            /* does not correct the fact that points in eastern US are  */
            /* written with the wrong coordinates by S&T. (MS bug)      */

            decode_psp_coordinates(&lat, &lon, gridbyte);

            wpt_tmp->latitude = lat;
            wpt_tmp->longitude = lon;
           
            /* 1 byte - pin display properties */
            psp_fread(&buff[0], 1, 1, psp_file_in);

	    /* 3 bytes - unknown */
            psp_fread(&buff[0], 1, 3, psp_file_in);

            /* 1 bytes - icon (values: 0x00 - 0x27) */
            psp_fread(&buff[0], 1, 1, psp_file_in);

	    /* 3 bytes - unknown */
    	    psp_fread(&buff[0], 1, 3, psp_file_in);

	    wpt_tmp->shortname = psp_read_str(psp_file_in);
	    wpt_tmp->description = psp_read_str(psp_file_in);
	    tmp = psp_read_str(psp_file_in); /* (address?) */
	    if (tmp) xfree(tmp);

	    waypt_add(wpt_tmp);
	}
}

static void
psp_waypt_pr(const waypoint *wpt)
{
	double lon, lat;
	char tbuf[64];
	char c;
	static short int pindex = 0;
	char *shortname;
	char *description;

        if ((! wpt->shortname) || (global_opts.synthesize_shortnames)) {
            if (wpt->description) {
                if (global_opts.synthesize_shortnames)
                    shortname = mkshort_from_wpt(mkshort_handle, wpt);
                else
                    shortname = xstrdup(wpt->description);
            } else {
                /* no description available */
                shortname = xstrdup("");
            }
        } else{
            shortname = xstrdup(wpt->shortname);
        }

        if (! wpt->description) {
            if (shortname) {
                description = xstrdup(shortname);
            } else {
                description = xstrdup("");
            }
        } else{
            description = xstrdup(wpt->description);
        }

        /* convert lat/long back to radians */
	lat = RAD(wpt->latitude);
        lon = RAD(wpt->longitude);
        
	pindex++;
        /* 2 bytes - pin index */
        gbfputint16(pindex, psp_file_out);
        
        /* 2 bytes - null bytes */
        gbfputint16(0, psp_file_out);
        

        /* set the grid byte */
	c = grid_byte(wpt->latitude, 
	              wpt->longitude);

	/* since the grid byte matches with what pocketstreets does to   */
	/* input files, our output appears identical to a pin file that  */
        /* has already been processed and corrected by pocketstreets.    */
        /* Due to the grid and signs, it'll look different than one that */
        /* comes straight from S&T.                                      */
	
        /* the grid byte */
        gbfwrite(&c, 1, 1, psp_file_out);

        /* 8 bytes - latitude/radians */
        psp_fwrite_double(lat, psp_file_out);

        /* 8 bytes - longitude/radians */
        psp_fwrite_double(lon, psp_file_out);

        /* 1 byte - pin properties */
        c = 0x14; /* display pin name on, display notes on. 0x04 = no notes */
        gbfwrite(&c, 1, 1, psp_file_out);

        memset(tbuf, '\0', sizeof(tbuf));

        /* 3 unknown bytes */
        gbfwrite(tbuf, 1, 3, psp_file_out);

        /* 1 icon byte 0x00 = PIN */
        gbfwrite(tbuf, 1, 1, psp_file_out);

        /* 3 unknown bytes */
        gbfwrite(tbuf, 1, 3, psp_file_out); /* 3 junk */

	psp_write_str(shortname);
	psp_write_str(description);

        /* just for the hell of it, we'll scrap the third string. */
	psp_write_str("");

        xfree(shortname);
        xfree(description);
}

static void
psp_write(void)
{
        short int s;
	unsigned char header_bytes[] = { 0x31, 0x6E, 0x69, 0x50, 0x20, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

        /* the header: */
        /* 31 6E 69 50 20 00 00 00 08 00 00 00 11 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 */
        /* offset 0x0C - 0x0D = 2 byte pin count */

        s = waypt_count();
        
        if (global_opts.synthesize_shortnames) {
            setshort_length(mkshort_handle, 32);
            setshort_whitespace_ok(mkshort_handle, 1);
        }

        if (s > MAXPSPOUTPUTPINS) {
            fatal(MYNAME ": attempt to output too many pushpins (%d).  The max is %d.  Sorry.\n", s, MAXPSPOUTPUTPINS);
        }

        /* insert waypoint count into header */
	le_write16(&header_bytes[12], s);

        gbfwrite(header_bytes, 1,  32, psp_file_out);

        waypt_disp_all(psp_waypt_pr);
}

ff_vecs_t psp_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	psp_rd_init,
	psp_wr_init,
	psp_rd_deinit,
	psp_wr_deinit,
	psp_read,
	psp_write,
	NULL,
	NULL,
	CET_CHARSET_UTF8, 1	/* Fixed because of unicode strings in psp files (see psp_read_str / psp_write_str) */
};
