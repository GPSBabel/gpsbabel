/*
    Magellan ".gs" files as they appear on USB of Explorist 400,500,600.

    Copyright (C) 2005, 2006 robertlipe@usa.net

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

#include <ctype.h>
#include "defs.h"
#include "xmlgeneric.h"
#include "magellan.h"

#define MYNAME "maggeo"

/* Turn this on (remove) after 5.2 becomes widespread. */
#define FIRMWARE_DOES_88591 0		

static gbfile *maggeofile_in;
static gbfile *maggeofile_out;
static short_handle desc_handle = NULL;

static void
maggeo_writemsg(const char * const buf)
{
	unsigned int osum = mag_checksum(buf);
	gbfprintf(maggeofile_out, "$%s*%02X\r\n",buf, osum);
}

static void
maggeo_rd_init(const char *fname)
{
	maggeofile_in = gbfopen(fname, "rb", MYNAME);
}

static void
maggeo_rd_deinit(void)
{
        gbfclose(maggeofile_in);
}

static void
maggeo_wr_init(const char *fname)
{
	if (waypt_count() > 200) {
		fatal(MYNAME ": eXplorist does not support more than 200 waypoints in one .gs file.\nDecrease the number of waypoints sent.\n");
	}
	maggeofile_out = gbfopen(fname, "wb", MYNAME);
	desc_handle = mkshort_new_handle();
	setshort_length(desc_handle, 20);
	setshort_badchars(desc_handle, "\"$,");
}

static void
maggeo_wr_deinit(void)
{
	maggeo_writemsg("PMGNCMD,END");
	mkshort_del_handle(&desc_handle);
	gbfclose(maggeofile_out);
}

static void
maggeo_read(void)
{
	fatal(MYNAME ": Reading maggeo is not implemented yet.\n");
}

/*
 * Note: returns allocated buffer that must be freed by caller.
 */
static 
char *
maggeo_fmtdate(time_t t)
{
	#define SZ 16

	char *cbuf = xmalloc(SZ);
	struct tm *tm = NULL;
	int date;
	tm = gmtime(&t);

	if ( t && tm ) {
		date = tm->tm_mday * 100000 + (1+tm->tm_mon) * 1000 + 
			   tm->tm_year;
		snprintf(cbuf, SZ, "%07d", date);
	} else {
		cbuf[0] = '\0';
	}
	return cbuf;
}

/*
 * Append an optional UTF string to buf, prepending a comma, 
 * cleansing it of NMEA-isms and decomposing to ASCII as we go.
 */
static
void
append(char *buf, const char *str)
{
	char *cleansed1, *cleansed2;

	strcat(buf, ",");

	if (!str) {
		return;
	}

	cleansed1 = xstrdup(str);
#if FIRMWARE_DOES_88591
/* Actually, this function needs needs refactored... */
	cleansed2 = xstrdup(cleansed1);
#else
	cleansed2 = m330_cleanse(cleansed1);
#endif

	strcat(buf, cleansed2);

	xfree(cleansed1);
	xfree(cleansed2);

}

static void
maggeo_waypt_pr(const waypoint *waypointp)
{
	char obuf[4096];
	double ilon, ilat;
	double lon, lat;
	int lon_deg, lat_deg;
	char *shortname;
	char *cname = NULL;
	const char *ctype = NULL;
	char *placer = NULL;
	char *lfounddate = NULL;
	char *placeddate = NULL;

	ilat = waypointp->latitude;
	ilon = waypointp->longitude;
	shortname = waypointp->shortname;

	lon = fabs(ilon);
	lat = fabs(ilat);

	lon_deg = lon;
	lat_deg = lat;

	lon = (lon - lon_deg) * 60.0;
	lat = (lat - lat_deg) * 60.0;

	lon = (lon_deg * 100.0 + lon);
	lat = (lat_deg * 100.0 + lat);

	/* 
	 * For some reason, Magellan used exactly the GPX spellings of 
	 * everything except this one...
	 */
	if (waypointp->gc_data.type == gt_suprise) {
		ctype = "Mystery Cache";
	} else {
		ctype = gs_get_cachetype(waypointp->gc_data.type);
	}
	placeddate = maggeo_fmtdate(waypointp->creation_time);
	lfounddate = maggeo_fmtdate(waypointp->gc_data.last_found);
	cname = mkshort(desc_handle, waypointp->notes ? waypointp->notes : waypointp->description);
	placer = waypointp->gc_data.placer;

	/*
	 * As of this writing on 05/04, the firmware in the units will
	 * let you write fields of just about any width, but appears to
	 * only use the following:
	 * shortname - 8 chars
	 * cname - 20 chars (scrolls in some places, not others)
	 * placer - display limited by width
	 * hint - 50 chars
	 * cache type - appears to be parsed by f/w for icon matching.
	 * 
	 * 
	 */
	snprintf(obuf, sizeof(obuf),
		"PMGNGEO,%4.3f,%c,%08.3f,%c,%04.0f,F",
                lat, ilat < 0 ? 'S' : 'N',
                lon, ilon < 0 ? 'W' : 'E',
                waypointp->altitude == unknown_alt ?
                        0 : waypointp->altitude);
	append(obuf, shortname);
	append(obuf, cname);
	append(obuf, placer);
	append(obuf, waypointp->gc_data.hint);
	append(obuf, ctype);
	append(obuf, placeddate);
	append(obuf, lfounddate);

	if (waypointp->gc_data.diff/10.0)
		sprintf(obuf + strlen(obuf), ",%3.1f", 
			waypointp->gc_data.diff/10.0);
	else
		strcat(obuf, ",");

	if (waypointp->gc_data.terr/10.0)
		sprintf(obuf + strlen(obuf), ",%3.1f", 
			waypointp->gc_data.terr/10.0);
	else
		strcat(obuf, ",");

	if (lfounddate) xfree(lfounddate);
	if (placeddate) xfree(placeddate);
	if (cname) xfree(cname);

	maggeo_writemsg(obuf);
}

static void
maggeo_write(void)
{
	waypt_disp_all(maggeo_waypt_pr);
}

ff_vecs_t maggeo_vecs = {
	ff_type_file,
	{ ff_cap_write, ff_cap_none, ff_cap_none },
	maggeo_rd_init,
	maggeo_wr_init,
	maggeo_rd_deinit,
	maggeo_wr_deinit,
	maggeo_read,
	maggeo_write,
	NULL,
	NULL,
#if FIRMWARE_DOES_88591
	CET_CHARSET_LATIN1, 0	/* CET-REVIEW */
#else
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
#endif
};
