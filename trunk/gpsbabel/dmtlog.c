/*

    Support for TrackLogs digital mapping (.trl) files,

    Copyright (C) 2006 Olaf Klein, o.b.klein@t-online.de

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
#include "jeeps/gpsmath.h"
#include "xmlgeneric.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define MYNAME "dmtlog"

#define DEFLATE_BUFF_SIZE 16384

static gbfile *fin, *fout;

static char *xmlbin;
static waypoint *xmlwpt;
static route_head *xmltrk;
static char *xmlgrid;
static int xmldatum, datum_WGS84, datum_OSGB36;
static double xmlEasting, xmlNorthing;
static double xmlLatitude, xmlLongitude;
static double xmlAltitude;

#if !ZLIB_INHIBITED
static int xmlbinsize;
#endif

static char header_written;
static char *opt_index;
static int track_index, this_index;

static
arglist_t dmtlog_args[] = {
	{ "index", &opt_index,
		"Index of track (if more the one in source)", "1", ARGTYPE_INT, "1", NULL },
	ARG_TERMINATOR
};


#if !ZLIB_INHIBITED
static xg_callback tlog3a_xgcb_version, tlog3a_xgcb_length, tlog3a_xgcb_data;

static xg_tag_mapping tlog3a_xgcb_map[] = {
	{ tlog3a_xgcb_version, 	cb_cdata, "/CXMLSafe/Version" },
	{ tlog3a_xgcb_length, 	cb_cdata, "/CXMLSafe/Length" },
	{ tlog3a_xgcb_data, 	cb_cdata, "/CXMLSafe/Data" },
	{ NULL, 	0,         NULL}
};
#endif

static xg_callback tlog3b_xgcb_tfna, tlog3b_xgcb_tfdes;
static xg_callback tlog3b_xgcb_wptst, tlog3b_xgcb_tptst;
static xg_callback tlog3b_xgcb_tpten, tlog3b_xgcb_wpten;
static xg_callback tlog3b_xgcb_wptid, tlog3b_xgcb_wptdt;
static xg_callback tlog3b_xgcb_wptgr, tlog3b_xgcb_wptea;
static xg_callback tlog3b_xgcb_wptno, tlog3b_xgcb_wptal;
static xg_callback tlog3b_xgcb_tptdt;

static xg_tag_mapping tlog3b_xgcb_map[] = {
	{ tlog3b_xgcb_tfna,	cb_cdata, "/CTrackFile/Name" },
	{ tlog3b_xgcb_tfdes,	cb_cdata, "/CTrackFile/Description" },
	{ tlog3b_xgcb_wptst,	cb_start, "/CTrackFile/CWayPoint" },
	{ tlog3b_xgcb_wptid,	cb_cdata, "/CTrackFile/CWayPoint/Id" },
	{ tlog3b_xgcb_wptdt,	cb_cdata, "/CTrackFile/CWayPoint/Datum" },
	{ tlog3b_xgcb_wptgr,	cb_cdata, "/CTrackFile/CWayPoint/Grid" },
	{ tlog3b_xgcb_wptea,	cb_cdata, "/CTrackFile/CWayPoint/Easting" },
	{ tlog3b_xgcb_wptno,	cb_cdata, "/CTrackFile/CWayPoint/Northing" },
	{ tlog3b_xgcb_wptal,	cb_cdata, "/CTrackFile/CWayPoint/Altitude" },
	{ tlog3b_xgcb_wpten,	cb_end,   "/CTrackFile/CWayPoint" },
	{ tlog3b_xgcb_tptst,	cb_start, "/CTrackFile/CTrackPoint" },
	{ tlog3b_xgcb_wptid,	cb_cdata, "/CTrackFile/CTrackPoint/Id" },
	{ tlog3b_xgcb_tptdt,	cb_cdata, "/CTrackFile/CTrackPoint/Datum" },
	{ tlog3b_xgcb_wptgr,	cb_cdata, "/CTrackFile/CTrackPoint/Grid" },
	{ tlog3b_xgcb_wptea,	cb_cdata, "/CTrackFile/CTrackPoint/Easting" },
	{ tlog3b_xgcb_wptno,	cb_cdata, "/CTrackFile/CTrackPoint/Northing" },
	{ tlog3b_xgcb_wptal,	cb_cdata, "/CTrackFile/CTrackPoint/Altitude" },
	{ tlog3b_xgcb_tpten,	cb_end,   "/CTrackFile/CTrackPoint" },
	{ NULL, 	0,         NULL}
};

/* helpers */

static void
convert_datum(waypoint *wpt, int datum)
{
	if (datum != datum_WGS84) {
		double lat = wpt->latitude;
		double lon = wpt->longitude;
		double alt = wpt->altitude;
		GPS_Math_Known_Datum_To_WGS84_C(lat, lon, alt,
			&wpt->latitude, &wpt->longitude, &wpt->altitude, 
			datum);
	}
}


static void
finalize_pt(waypoint *wpt)
{
	if (strcmp(xmlgrid, "BNG") == 0) {
		GPS_Math_NGENToAiry1830LatLon(xmlEasting, xmlNorthing, 
		&wpt->latitude, &wpt->longitude);
		xmldatum = datum_OSGB36;
	}
	else {
		wpt->latitude = xmlLatitude;
		wpt->longitude = xmlLongitude;
	}
	wpt->altitude = xmlAltitude;
	convert_datum(wpt, xmldatum);
}

/* xml-reader callbacks */

#if !ZLIB_INHIBITED
static void  	
tlog3a_xgcb_version(const char *args, const char **unused)
{
	if (strcmp(args, "1") != 0)
		fatal(MYNAME ": Unsupported file version '%s'!\n", args);
}

static void  	
tlog3a_xgcb_length(const char *args, const char **unused)
{
}

static void  	
tlog3a_xgcb_data(const char *args, const char **unused)
{
	int len;
	char *bin;
	char *cin, *cout;
	char cl, ch;
	
	len = strlen(args);
	bin = xmalloc((len >> 1) + 1);
	
	cin = (char *)args;
	cout = bin;
	
	cl = 0x10;
	while (*cin) {
		char c = *cin++;
		
		if (c == '\0') break;
		else if ((c >= 'A') && (c <= 'F')) c -= 'A' - 10;
		else if ((c >= 'a') && (c <= 'f')) c -= 'a' - 10;
		else if ((c >= '0') && (c <= '9')) c -= '0';
		else continue;

		if (cl == 0x10) cl = c;
		else {
			ch = (cl << 4) | c;
			*cout++ = ch;
			cl = 0x10;
		}
	}
	xmlbin = bin;
	xmlbinsize = (cout - bin);
}
#endif


static void
tlog3b_xgcb_tfna(const char *args, const char **unused)
{
	if (xmltrk == NULL) {
		xmltrk = route_head_alloc();
		track_add_head(xmltrk);
	}
	xmltrk->rte_name = strdup(args);
}


static void
tlog3b_xgcb_tfdes(const char *args, const char **unused)
{
	if (xmltrk == NULL) {
		xmltrk = route_head_alloc();
		track_add_head(xmltrk);
	}
	xmltrk->rte_desc = strdup(args);
}


static void
tlog3b_xgcb_wptst(const char *args, const char **unused)
{
	xmlwpt = waypt_new();
	xmldatum = datum_WGS84;
}


static void
tlog3b_xgcb_tptst(const char *args, const char **unused)
{
	xmlwpt = waypt_new();
	xmldatum = datum_WGS84;
}


static void
tlog3b_xgcb_tpten(const char *args, const char **unused)
{
	finalize_pt(xmlwpt);
	
	if (xmltrk == NULL) {
		xmltrk = route_head_alloc();
		track_add_head(xmltrk);
	}
	track_add_wpt(xmltrk, xmlwpt);
	xmlwpt = NULL;
}


static void
tlog3b_xgcb_wptid(const char *args, const char **unused)
{
	if (*args)
		xmlwpt->shortname = xstrdup(args);
}


static void
tlog3b_xgcb_wptdt(const char *args, const char **unused)
{
	xmldatum = GPS_Lookup_Datum_Index(args);
}


static void
tlog3b_xgcb_wptgr(const char *args, const char **unused)
{
	if (xmlgrid != NULL) {
		if (strcmp(xmlgrid, args) == 0) return;
		xfree(xmlgrid);
	}
	xmlgrid = xstrdup(args);
}


static void
tlog3b_xgcb_wptno(const char *args, const char **unused)
{
	xmlNorthing = atof(args);
}


static void
tlog3b_xgcb_wptea(const char *args, const char **unused)
{
	xmlEasting = atof(args);
}


static void
tlog3b_xgcb_wptal(const char *args, const char **unused)
{
	xmlAltitude = atof(args);
}


static void
tlog3b_xgcb_tptdt(const char *args, const char **unused)
{
	xmldatum = GPS_Lookup_Datum_Index(args);
}


static void
tlog3b_xgcb_wpten(const char *args, const char **unused)
{
	finalize_pt(xmlwpt);
	waypt_add(xmlwpt);
	xmlwpt = NULL;
}


static int
read_datum(gbfile *f)
{
	int res;
	char *d, *g;
	
	d = gbfgetpstr(f);
	g = gbfgetpstr(f);
	
	res = GPS_Lookup_Datum_Index(d);
	
	if (*g && (strcmp(d, g) != 0)) {
		fatal(MYNAME ": Unsupported combination of datum '%s' and grid '%s'!\n",
			d, g);
	}
	xfree(d);
	xfree(g);
	
	return res;
}


static void
read_CTrackFile(const int version)
{
	char buf[128];
	gbuint32 ver;
	gbint32 tcount, wcount;
	gbint16 u1;
	gbint32 ux;
	route_head *track;
	int i;
	int datum;
	
	u1 = gbfgetint16(fin);

	gbfread(buf, 1, 10, fin);
	if ((u1 != 0x0a) || (strncmp("CTrackFile", buf, 10) != 0))
		fatal(MYNAME ": Unknown or invalid track file.\n");

	if (version == 8) {
		for (i = 1; i <= 9; i++)
			gbfread(buf, 1, 4, fin);
	}
	ver = gbfgetint32(fin);
	if (ver != version)
		fatal(MYNAME ": Unknown or invalid track file (%d).\n", ver);
	
	ux = gbfgetint32(fin); // Unknown 2
	ux = gbfgetint32(fin); // Unknown 3
	ux = gbfgetint32(fin); // Unknown 4
	
	track = route_head_alloc();
	track_add_head(track);
	
	/* S1 .. S9: comments, hints, jokes, aso */
	for (i = 0; i < 9; i++) {
		int c = gbfgetc(fin);
		gbfseek(fin, c, SEEK_CUR);
	}
	
	tcount = gbfgetint32(fin);
	if (tcount > 0) {
		datum = read_datum(fin);
		if (version == 8) {
			int len;
			
			gbfread(buf, 1, 4, fin);
			len = gbfgetint16(fin);
			gbfseek(fin, len, SEEK_CUR);
		}
	}

	while (tcount > 0)
	{
		waypoint *wpt;
		
		tcount--;
		
		if (version == 8)
			datum = read_datum(fin);

		wpt = waypt_new();
		
		wpt->latitude = gbfgetdbl(fin);
		wpt->longitude = gbfgetdbl(fin);
		wpt->altitude = gbfgetdbl(fin);
		
		convert_datum(wpt, datum);
		
		track_add_wpt(track, wpt);
		
		if (version == 8)
			gbfseek(fin, 34, SEEK_CUR);
	}

	wcount = gbfgetint32(fin);
	
	if (wcount == 0) return;

	if (version == 8) {
		warning(MYNAME ": We don't yet support waypoints for this file version!\n");
		return;
	}
		
	datum = read_datum(fin);

	while (wcount > 0) {
		waypoint *wpt;
		gbint32 namect, i;
		
		wcount--;

		if (version == 8)
			datum = read_datum(fin);
		
		wpt = waypt_new();
		
		wpt->latitude = gbfgetdbl(fin);
		wpt->longitude = gbfgetdbl(fin);
		wpt->altitude = gbfgetdbl(fin);
		
		convert_datum(wpt, datum);
		
		namect = gbfgetint32(fin);
		
		// variants of shortname
		
		for (i = 0; i < namect; i++) {
			char *name = gbfgetpstr(fin);
			if (name && *name) {
				switch(i) {
					case 0: wpt->description = xstrdup(name); break;
					case 1: wpt->shortname = xstrdup(name); break;
				}
			}
			xfree(name);
		}
		if (version == 8)
			gbfseek(fin, 34, SEEK_CUR);
		
		waypt_add(wpt);
	}
}


#if !ZLIB_INHIBITED

static int
inflate_buff(const char *buff, const size_t size, char **out_buff)
{
	int res = Z_OK;
	z_stream strm;
	char out[DEFLATE_BUFF_SIZE];
	char *cout = NULL;
	gbuint32 bytes = 0;
	gbuint32 have;

	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	strm.avail_in = 0;
	strm.next_in = Z_NULL;
	
	res = inflateInit(&strm);
	if (res != Z_OK) {
		return res;
	}
	
	strm.avail_in = size;
	strm.next_in = (void *)buff;
	
	do {
		strm.avail_out = DEFLATE_BUFF_SIZE;
		strm.next_out = (void *)out;
		res = inflate(&strm, Z_NO_FLUSH);
		
		switch (res) {
			case Z_NEED_DICT:
				res = Z_DATA_ERROR;     /* and fall through */
			case Z_DATA_ERROR:
			case Z_MEM_ERROR:
				(void)inflateEnd(&strm);
				return res;
		}
		have = DEFLATE_BUFF_SIZE - strm.avail_out;
		if (have > 0) {
			cout = xrealloc(cout, bytes + have);
			memcpy(cout+bytes, out, have);
			bytes+=have;
		}
	} while (strm.avail_out == 0);
	
	*out_buff = cout;
	return res;
}


static void
read_CXMLSafe(void)
{
	char *xmlstr = NULL;
	
	xmlbin = NULL;
	xmlbinsize = 0;
	
	xml_init(fin->name, tlog3a_xgcb_map, NULL);
	xml_read();
	xml_deinit();
	
	if (xmlbin != NULL) {
		inflate_buff(xmlbin, xmlbinsize, &xmlstr);
		xfree(xmlbin);

		xml_init(NULL, tlog3b_xgcb_map, NULL);
		xml_readstring(xmlstr);
		xml_deinit();
		
		xfree(xmlstr);
	}
}

#endif

static void
read_XML(void)
{
	xml_init(fin->name, tlog3b_xgcb_map, NULL);
	xml_read();
	xml_deinit();
	
	return;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
dmtlog_rd_init(const char *fname)
{
	fin = gbfopen_le(fname, "rb", MYNAME);
	
	datum_OSGB36 = GPS_Lookup_Datum_Index("OSGB36");
	datum_WGS84 = GPS_Lookup_Datum_Index("WGS84");
	
	xmlbin = NULL;
	xmltrk = NULL;
	xmlwpt = NULL;
	xmlgrid = NULL;
}

static void 
dmtlog_rd_deinit(void)
{
	gbfclose(fin);
	if (xmlgrid != NULL) xfree(xmlgrid);
}

static void
dmtlog_read(void)
{
	switch(gbfgetuint32(fin)) {
	
		case 0x4FFFF:
			read_CTrackFile(4);
			break;

		case 0x8FFFF:
			read_CTrackFile(8);
			break;
			
		case 0x4d58433c:
#if !ZLIB_INHIBITED
			read_CXMLSafe();
#else
			fatal(MYNAME ": Zlib was not included in this build.\n");
#endif			
			break;
		case 0x7254433c:
			read_XML();
			break;

		default:
			fatal(MYNAME ": Unknown or unsupported file type.\n");
	}
}

static void
dmtlog_wr_init(const char *fname)
{
	fout = gbfopen_le(fname, "wb", MYNAME);
}

static void
dmtlog_wr_deinit(void)
{
	gbfclose(fout);
}

static void
write_header(const route_head *trk)
{
	int count, i;
	char *cout;
	const char ZERO = '\0';
	
	header_written = 1;
	
	count = 0;
	if (trk != NULL) {
		queue *curr, *prev;
		QUEUE_FOR_EACH(&trk->waypoint_list, curr, prev) count++;
	}
	gbfputpstr(trk && trk->rte_name && *trk->rte_name ? trk->rte_name : "Name", fout);
	
	xasprintf(&cout, "%d trackpoints and %d waypoints", count, waypt_count());
	gbfputpstr(cout, fout);
	xfree(cout);
	
	for (i = 3; i <= 8; i++) gbfputc(ZERO, fout);
	gbfputpstr("GPSBabel", fout);
	gbfputint32(count, fout);
	if (count > 0) {
		gbfputpstr("WGS84", fout);
		gbfputpstr("WGS84", fout);
	}
}

static void
track_hdr_cb(const route_head *trk)
{
	
	this_index++;
	if (this_index != track_index) return;
	write_header(trk);
}

static void
track_tlr_cb(const route_head *trk)
{
}

static void
track_wpt_cb(const waypoint *wpt)
{
	if (this_index != track_index) return;
	
	gbfputdbl(wpt->latitude, fout);
	gbfputdbl(wpt->longitude, fout);
	gbfputdbl(wpt->altitude, fout);
}

static void
wpt_cb(const waypoint *wpt)
{
	int names;
	
	gbfputdbl(wpt->latitude, fout);
	gbfputdbl(wpt->longitude, fout);
	gbfputdbl(wpt->altitude, fout);
	
	names = 1;
	if (wpt->description && *wpt->description) names = 2;
	gbfputint32(names, fout);
	if (names > 1) gbfputpstr(wpt->description, fout);
	gbfputpstr(wpt->shortname && *wpt->shortname ? wpt->shortname : "Name", fout);
}

static void
dmtlog_write(void)
{
	track_index = atoi(opt_index);
	/* ... validate index */
	
	gbfputint32(0x4FFFF, fout);
	gbfputuint16(0x0A, fout);
	gbfputs("CTrackFile", fout);
	gbfputint32(4, fout);
	gbfputint32(1, fout);
	gbfputint32(0x100001, fout);
	gbfputuint32((const gbuint32)gpsbabel_now, fout);
	
	header_written = 0;
	this_index = 0;
	track_disp_all(track_hdr_cb, track_tlr_cb, track_wpt_cb);
	if (!header_written) 
		write_header(NULL);
	gbfputint32(waypt_count(), fout);
	if (waypt_count() > 0) {
		gbfputpstr("WGS84", fout);
		gbfputpstr("WGS84", fout);
		waypt_disp_all(wpt_cb);
	}
}

/**************************************************************************/

ff_vecs_t dmtlog_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write	/* waypoints */, 
	  	ff_cap_read | ff_cap_write	/* tracks */, 
	  	ff_cap_none			/* routes */
	},
	dmtlog_rd_init,	
	dmtlog_wr_init,
	dmtlog_rd_deinit,	
	dmtlog_wr_deinit,
	dmtlog_read,
	dmtlog_write,
	NULL,
	dmtlog_args,
	CET_CHARSET_ASCII, 0

};

/**************************************************************************/
