/*
    National Geographic Topo! Waypoint
    Contributed to gpsbabel by Alex Mottram
    
    Copyright (C) 2002 Alex Mottram, geo_alexm at cox-internet.com

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
#include "jeeps/gpsmath.h" /* for datum conversions */

#define MYNAME	"TPG"

#define MAXTPGSTRINGSIZE	256
#define MAXTPGOUTPUTPINS	65535   

static FILE *tpg_file_in;
static FILE *tpg_file_out;
static void *mkshort_handle;

static int i_am_little_endian;
static int endianness_tested;

static void
test_endianness(void)
{
        union {
                long l;
                unsigned char uc[sizeof (long)];
        } u;

        u.l = 1;
        i_am_little_endian = u.uc[0];

        endianness_tested = 1;

}

static int
tpg_fread(void *buff, size_t size, size_t members, FILE * fp) 
{
    size_t br;

    br = fread(buff, size, members, fp);

    if (br != members) {
        fatal(MYNAME ": requested to read %d bytes, read %d bytes.\n", members, br);
    }

    return (br);
}

static double
tpg_fread_double(FILE *fp)
{
	unsigned char buf[8];
	unsigned char sbuf[8];

	if (!endianness_tested) {
		test_endianness();
	}

	tpg_fread(buf, 1, 8, tpg_file_in);
	if (i_am_little_endian) {
		return *(double *) buf;
	}
	sbuf[0] = buf[7];
	sbuf[1] = buf[6];
	sbuf[2] = buf[5];
	sbuf[3] = buf[4];
	sbuf[4] = buf[3];
	sbuf[5] = buf[2];
	sbuf[6] = buf[1];
	sbuf[7] = buf[0];
	return *(double *)sbuf;
}

static void
tpg_fwrite_double(double x, FILE *fp)
{
	unsigned char *cptr = (unsigned char *)&x;
	unsigned char cbuf[8];

	if (!endianness_tested) {
		test_endianness();
	}
	if (i_am_little_endian) {
		fwrite(&x, 8, 1, fp);
	} else {
                cbuf[0] = cptr[7];
                cbuf[1] = cptr[6];
                cbuf[2] = cptr[5];
                cbuf[3] = cptr[4];
                cbuf[4] = cptr[3];
                cbuf[5] = cptr[2];
                cbuf[6] = cptr[1];
                cbuf[7] = cptr[0];
                fwrite(cbuf, 8, 1, fp);
	}

}

static int
valid_tpg_header(char * header, int len) 
{
    unsigned char header_bytes[] = { 0xFF, 0xFF, 0x01, 0x00, 0x0D, 
                                     0x00, 0x43, 0x54, 0x6F, 0x70, 
                                     0x6F, 0x57, 0x61, 0x79, 0x70, 
                                     0x6F, 0x69, 0x6E, 0x74 };
    if (len != 19) {
        return (-1);
    }

    return memcmp(header_bytes, header, len);
}

static void
tpg_rd_init(const char *fname, const char *args)
{
	tpg_file_in = fopen(fname, "rb");
	if (tpg_file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
tpg_rd_deinit(void)
{
	fclose(tpg_file_in);
}

static void
tpg_wr_init(const char *fname, const char *args)
{
	tpg_file_out = fopen(fname, "wb");
	mkshort_handle = mkshort_new_handle();

	if (tpg_file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
}

static void
tpg_wr_deinit(void)
{
	mkshort_del_handle(mkshort_handle);
	fclose(tpg_file_out);
}

static void
tpg_read(void)
{
	char buff[MAXTPGSTRINGSIZE + 1];
	waypoint *wpt_tmp;
	double lat, lon, elev;
	double amt;
	int stringsize;
	short int pointcount;

        tpg_fread(&buff[0], 2, 1, tpg_file_in);
        
        pointcount = le_read16(&buff[0]);
        
        /* the rest of the header */
        tpg_fread(&buff[0], 19, 1, tpg_file_in);

        if (valid_tpg_header(buff, 19) != 0) {
            fatal(MYNAME ": input file does not appear to be a valid .TPG file.\n");
        }
        
        
	while (pointcount--) {
	    wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

            /* 1 bytes at start of record - string size for shortname */
    	    tpg_fread(&buff[0], 1, 1, tpg_file_in);
    	    
    	    stringsize = buff[0];
    	    
    	    tpg_fread(&buff[0], stringsize, 1, tpg_file_in);

    	    buff[stringsize] = '\0';
    	    
    	    wpt_tmp->shortname = xstrdup(buff);

            /* for some very odd reason, signs on longitude are swapped */
            /* coordinates are in NAD27/CONUS datum                     */
            
            /* 8 bytes - longitude, sign swapped  */
	    lon = tpg_fread_double(tpg_file_in);

            /* 8 bytes - latitude */
	    lat = tpg_fread_double(tpg_file_in);
	    
            /* swap sign before we do datum conversions */
	    lon *= -1.0;
	    
            /* 2 bytes - elevation in feet */
    	    tpg_fread(&buff[0], 2, 1, tpg_file_in);
    	    elev = (le_read16(&buff[0]) * .3048); /* feets to meters */

            /* convert incoming NAD27/CONUS coordinates to WGS84 */
            GPS_Math_Known_Datum_To_WGS84_M(
            	lat, 
                lon,
                0.0,
                &wpt_tmp->position.latitude.degrees,
                &wpt_tmp->position.longitude.degrees,
                &amt,
                78);

            wpt_tmp->position.altitude.altitude_meters = elev;
            

            /* 4 bytes? */
    	    tpg_fread(&buff[0], 4, 1, tpg_file_in);

            /* 1 bytes - string size for description */
    	    tpg_fread(&buff[0], 1, 1, tpg_file_in);
    	    
    	    stringsize = buff[0];
    	    
    	    tpg_fread(&buff[0], stringsize, 1, tpg_file_in);
    	    buff[stringsize] = '\0';
    	    
    	    wpt_tmp->description = xstrdup(buff);
    	    
    	    /* 2 bytes */
    	    tpg_fread(&buff[0], 2, 1, tpg_file_in);

	    waypt_add(wpt_tmp);
	}
}

static void
tpg_waypt_pr(const waypoint *wpt)
{
	double lon, lat;
	double amt;
	short int elev;
	char tbuf[64];
	char c;
	char *shortname;
	char *description;

        /* these unknown 4 are probably point properties (color, icon, etc..) */
	unsigned char unknown4[] = { 0x78, 0x56, 0x34, 0x12 }; 

        /* these 2 appear to be constant across test files */
	unsigned char unknown2[] = { 0x01, 0x80 };  
	
        /* this output format pretty much requires a description
         * and a shortname 
         */

        if ((! wpt->shortname) || (global_opts.synthesize_shortnames)) {
            if (wpt->description) {
                if (global_opts.synthesize_shortnames)
                    shortname = mkshort(mkshort_handle, wpt->description);
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

        /* convert lat/long to NAD27/CONUS datum */
        GPS_Math_WGS84_To_Known_Datum_M(
        	wpt->position.latitude.degrees,
                wpt->position.longitude.degrees,
                0.0,
                &lat,
                &lon,
                &amt,
                78);
        

        /* swap the sign back *after* the datum conversion */
        lon *= -1.0;

        /* convert meters back to feets */
        elev = (short int) (wpt->position.altitude.altitude_meters * 3.2808);

        /* 1 bytes stringsize for shortname */
        c = strlen(shortname);
        fwrite(&c, 1, 1, tpg_file_out);
        
        /* shortname */
        fwrite(shortname, 1, c, tpg_file_out);

        /* 8 bytes - longitude */
        tpg_fwrite_double(lon, tpg_file_out);

        /* 8 bytes - latitude */
        tpg_fwrite_double(lat, tpg_file_out);

        /* 2 bytes - elevation_feet */
        fwrite(&elev, 1, 2, tpg_file_out);

        /* 4 unknown bytes */
        memset(tbuf, '\0', sizeof(tbuf));
        fwrite(unknown4, 1, 4, tpg_file_out);

        /* 1 bytes stringsize for description */
        c = strlen(description);
        fwrite(&c, 1, 1, tpg_file_out);
        
        /* description */
        fwrite(description, 1, c, tpg_file_out);

        /* and finally 2 unknown bytes */
        fwrite(unknown2, 1, 2, tpg_file_out);
        
        xfree(shortname);
        xfree(description);
}

static void
tpg_write(void)
{
        int s;
        unsigned char uc[2];
        unsigned char header_bytes[] = { 0xFF, 0xFF, 0x01, 0x00, 0x0D, 
                                         0x00, 0x43, 0x54, 0x6F, 0x70, 
                                         0x6F, 0x57, 0x61, 0x79, 0x70, 
                                         0x6F, 0x69, 0x6E, 0x74 };

        s = waypt_count();

        if (global_opts.synthesize_shortnames) {
            setshort_length(mkshort_handle, 32);
            setshort_whitespace_ok(mkshort_handle, 1);
        }

        if (s > MAXTPGOUTPUTPINS) {
            fatal(MYNAME ": attempt to output too many points (%d).  The max is %d.  Sorry.\n", s, MAXTPGOUTPUTPINS);
        }

	le_write16(uc, s);

        /* write the waypoint count */
        fwrite(uc, 1,  2, tpg_file_out);

        /* write the rest of the header */
        fwrite(header_bytes, 1, 19, tpg_file_out);

        waypt_disp_all(tpg_waypt_pr);
}

ff_vecs_t tpg_vecs = {
	tpg_rd_init,
	tpg_wr_init,
	tpg_rd_deinit,
	tpg_wr_deinit,
	tpg_read,
	tpg_write,
};
