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
#include <ctype.h>
#include <math.h>  /* for M_PI */

#ifndef M_PI
#  define M_PI 3.141592653589
#endif

#define MYNAME	"PSP"

#define MAXPSPSTRINGSIZE	256
#define MAXPSPOUTPUTPINS	8192   /* Any more points than this is ludicrous */

static FILE *psp_file_in;
static FILE *psp_file_out;

static int
psp_fread(void *buff, size_t size, size_t members, FILE * fp) {
    size_t br;

    br = fread(buff, size, members, fp);

    if (br != members) {
        fatal(MYNAME ": requested to read %d bytes, read %d bytes.\n", members, br);
    }

    return (br);
}

static int
valid_psp_header(char * header, int len) {
    char header_bytes[] = { 0x31, 0x6E, 0x69, 0x50, 0x00 }; /* 1niP <stop> */
    char *p, *s;

    if (len != 32) {
        return (-1);
    }

    p = header_bytes;
    s = header;

    while (*p) {
        if (*p++ != *s++) {
            return(-1);
        }
    }

    return (0);
}

static char *
buffer_washer(char * buff, int buffer_len)
{
    int i;

    for (i = 0 ; i < buffer_len - 1; i++) {
	if (buff[i] == '\0') {
	    memcpy(&buff[i], &buff[i+1], buffer_len - i);
	    buffer_len--;
	    buff[buffer_len] = '\0';
	}
    }

    return (buff);
}

static void
psp_rd_init(const char *fname)
{
	psp_file_in = fopen(fname, "rb");
	if (psp_file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
psp_rd_deinit(void)
{
	fclose(psp_file_in);
}

static void
psp_wr_init(const char *fname)
{
	psp_file_out = fopen(fname, "wb");
	if (psp_file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
}

static void
psp_wr_deinit(void)
{
	fclose(psp_file_out);
}

static void
psp_read(void)
{
	char buff[MAXPSPSTRINGSIZE + 1];
	double radians;
	waypoint *wpt_tmp;
	int stringsize;
	short int pincount;

        /* 32 bytes - file header */
        psp_fread(&buff[0], 1, 32, psp_file_in);

        if (valid_psp_header(buff, 32) != 0) {
            fatal(MYNAME ": input file does not appear to be a valid .PSP file.\n");
        }

	pincount = *(short int *)&buff[12];

	while (pincount--) {
	    wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

	    /* things we will probably never know about this waypoint */
	    /* coming from a pushpin file.                            */

	    /*
	        wpt_tmp->creation_time;
  	        wpt_tmp->position.altitude.altitude_meters;
  	        wpt_tmp->url;
  	        wpt_tmp->url_link_text;
  	        wpt_tmp->icon_descr;  TODO: map this.
	    */

            /* 4 bytes at start of record */
            /* coming out of S&T, this 1st byte is probably the pin # (0x01, 0x02, etc...) */
            /* coming from pocketstreets, it's generally 0x00. Sometimes 0xC3. ?           */

    	    psp_fread(&buff[0], 1, 4, psp_file_in);

            /* 1 byte, unkown */
            psp_fread(&buff[0], 1, 1, psp_file_in);

            /* 8 bytes - latitude in radians */
            psp_fread(&buff[0], 1, 8, psp_file_in);
            radians = *(double *)&buff[0];
            wpt_tmp->position.latitude.degrees = (radians * 180.0) / M_PI;

            /* 8 bytes - longitude in radians */
            psp_fread(&buff[0], 1, 8, psp_file_in);
            radians = *(double *)&buff[0];
            wpt_tmp->position.longitude.degrees = (radians * 180.0) / M_PI;

            /* 1 byte - pin display properties */
            psp_fread(&buff[0], 1, 1, psp_file_in);

	    /* 3 bytes - unknown */
            psp_fread(&buff[0], 1, 3, psp_file_in);

            /* 1 bytes - icon 0x00 - 0x27 */
            psp_fread(&buff[0], 1, 1, psp_file_in);

	    /* 3 bytes - unknown */
    	    psp_fread(&buff[0], 1, 3, psp_file_in);

            /* 1 byte - string size */
    	    psp_fread(&buff[0], 1, 1, psp_file_in);

    	    stringsize = buff[0];
    	    stringsize *= 2;

    	    if (stringsize > MAXPSPSTRINGSIZE) {
		fatal(MYNAME ": variable string size (%d) in PSP file exceeds MAX (%d).\n", stringsize, MAXPSPSTRINGSIZE);
    	    }

            /* stringsize bytes - string data */
    	    psp_fread(&buff[0], 1, stringsize, psp_file_in);

	    buffer_washer(buff, stringsize);

	    wpt_tmp->shortname = xstrdup(buff);

            /* 1 bytes string size */
    	    psp_fread(&buff[0], 1, 1, psp_file_in);

    	    stringsize = buff[0];
    	    stringsize *= 2;

    	    if (stringsize > MAXPSPSTRINGSIZE) {
		fatal(MYNAME ": variable string size (%d) in PSP file exceeds MAX (%d).\n", stringsize, MAXPSPSTRINGSIZE);
    	    }

            /* stringsize bytes - string data */
    	    psp_fread(&buff[0], 1, stringsize, psp_file_in);

	    buffer_washer(buff, stringsize);

	    wpt_tmp->description = xstrdup(buff);

            /* 1 bytes - string size */
    	    psp_fread(&buff[0], 1, 1, psp_file_in);

    	    stringsize = buff[0];
    	    stringsize *= 2;

    	    if (stringsize > MAXPSPSTRINGSIZE) {
		fatal(MYNAME ": variable string size (%d) in PSP file exceeds MAX (%d).\n", stringsize, MAXPSPSTRINGSIZE);
    	    }

            /* stringsize bytes - string data (address?) */
    	    psp_fread(&buff[0], 1, stringsize, psp_file_in);

	    buffer_washer(buff, stringsize);

	    waypt_add(wpt_tmp);
	}
}

static void
psp_disp(waypoint *wpt)
{
	double lon, lat;
	char tbuf[64];
	char c;
	int i;

        /* convert lat/long back to radians */
	lat = (wpt->position.latitude.degrees * M_PI) / 180.0;
        lon = (wpt->position.longitude.degrees * M_PI) / 180.0;

        /* 4 leading bytes */
        memset(&tbuf, '\0', sizeof(tbuf));
        fwrite(&tbuf, 1, 4, psp_file_out);

        /* my test files seem to always have this byte as 0x03, */
        /* although nothing seems to really care.               */
	c = 0x03;

        /* 1 unknown bytes */
        fwrite(&c, 1, 1, psp_file_out);

        /* 8 bytes - latitude/radians */
        fwrite(&lat, 1, 8, psp_file_out);

        /* 8 bytes - longitude/radians */
        fwrite(&lon, 1, 8, psp_file_out);

        /* 1 byte - pin properties */
        c = 0x14; /* display pin name on! display notes on! */
        fwrite(&c, 1, 1, psp_file_out);

        memset(&tbuf, '\0', sizeof(tbuf));

        /* 3 unknown bytes */
        fwrite(&tbuf, 1, 3, psp_file_out);

        /* 1 icon byte 0x00 = PIN */
        fwrite(&tbuf, 1, 1, psp_file_out);

        /* 3 unknown bytes */
        fwrite(&tbuf, 1, 3, psp_file_out); /* 3 junk */

        c = strlen(wpt->shortname);
        /* 1 string size */
        fwrite(&c, 1, 1, psp_file_out);

        for (i = 0 ; wpt->shortname[i] ; i++) {
             fwrite(&wpt->shortname[i], 1, 1, psp_file_out);    /* char */
             fwrite(&tbuf[0], 1, 1, psp_file_out);              /* null */
        }

        /*  1 byte string size */
        c = strlen(wpt->description);
        fwrite(&c, 1, 1, psp_file_out);

        for (i = 0 ; wpt->description[i] ; i++) {
             fwrite(&wpt->description[i], 1, 1, psp_file_out);  /* char */
             fwrite(&tbuf[0], 1, 1, psp_file_out);              /* null */
        }

        /* just for the hell of it, we'll scrap the third string. */
        c = strlen(tbuf);
        /* 1 byte string size */
        fwrite(&c, 1, 1, psp_file_out);

        for (i = 0 ; tbuf[i] ; i++) {
             fwrite(&tbuf[i], 1, 1, psp_file_out);              /* char */
             fwrite(&tbuf[0], 1, 1, psp_file_out);              /* null */
        }
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

        if (s > MAXPSPOUTPUTPINS) {
            fatal(MYNAME ": attempt to output too many pushpins (%d).  The max is %d.  Sorry.\n", s, MAXPSPOUTPUTPINS);
        }

        /* insert waypoint count into header */
        memcpy(&header_bytes[12], &s, 2);

        fwrite(&header_bytes, 1,  32, psp_file_out);

        waypt_disp_all(psp_disp);
}

ff_vecs_t psp_vecs = {
	psp_rd_init,
	psp_wr_init,
	psp_rd_deinit,
	psp_wr_deinit,
	psp_read,
	psp_write,
};
