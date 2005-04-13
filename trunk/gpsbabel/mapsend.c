/*
    Access Magellan Mapsend files.

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

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "mapsend.h"
#include "magellan.h"

static FILE *mapsend_file_in;
static FILE *mapsend_file_out;
static void *mkshort_handle;

static int route_wp_count;
static int mapsend_infile_version;
static int trk_version = 30;

#define MYNAME "mapsend"

static void
mapsend_rd_init(const char *fname)
{
	mapsend_file_in = xfopen(fname, "rb", MYNAME);
}

static void
mapsend_rd_deinit(void)
{
	fclose(mapsend_file_in);
}

static 
void 
my_fread8(void *ptr, FILE *stream)
{
	unsigned char cbuf[8];
	size_t rv;

	rv = fread(cbuf, 8, 1, stream);
	le_read64(ptr, cbuf);
	
}

static 
void
my_fwrite8(void *ptr, FILE *stream)
{
	unsigned char cbuf[8];

	le_read64(cbuf, ptr);
	fwrite(cbuf, 8, 1, stream);
}

static 
void 
my_fread4(void *ptr, FILE *stream)
{
	unsigned int *iptr = ptr;
	unsigned char cbuf[4];
	size_t rv;

	rv = fread(cbuf, 4, 1, stream);
	*iptr = le_read32(cbuf);
}

static 
size_t
my_fwrite4(int *ptr, FILE *stream)
{
	int i = le_read32(ptr);
	return fwrite(&i, 4, 1, stream);
}

static void
mapsend_wr_init(const char *fname)
{
	mapsend_file_out = xfopen(fname, "wb", MYNAME);
	mkshort_handle = mkshort_new_handle();
	route_wp_count = 0;
}

static void
mapsend_wr_deinit(void)
{
	fclose(mapsend_file_out);
	mkshort_del_handle(mkshort_handle);
}

static void
mapsend_wpt_read(void)
{
	char tbuf[256];
	char name[257];
	char comment[257];
	char *p;
	int wpt_count, rte_count, rte_num;
	unsigned char scount;
	int wpt_number;
	char wpt_icon;
	char wpt_status;
	double wpt_alt;
	double wpt_long;
	double wpt_lat;
	waypoint *wpt_tmp;
	route_head *rte_head;

	my_fread4(&wpt_count, mapsend_file_in);
	
	while (wpt_count--) {
		wpt_tmp = waypt_new();

		if (fread(&scount, sizeof(scount), 1, mapsend_file_in) < 1) {
			fatal(MYNAME ": out of data reading %d waypoints\n",
					wpt_count);
		}
		fread(tbuf, scount, 1, mapsend_file_in);
		p = strncpy(name, tbuf, scount);
		p[scount] = '\0';

		fread(&scount, sizeof(scount), 1, mapsend_file_in);
		fread(tbuf, scount, 1, mapsend_file_in);
		p = strncpy(comment, tbuf, scount);
		p[scount] = '\0';

		my_fread4(&wpt_number, mapsend_file_in);
		fread(&wpt_icon, sizeof(wpt_icon), 1, mapsend_file_in);
		fread(&wpt_status, sizeof(wpt_status), 1, mapsend_file_in);
		my_fread8(&wpt_alt, mapsend_file_in);
		my_fread8(&wpt_long, mapsend_file_in);
		my_fread8(&wpt_lat, mapsend_file_in);

		wpt_tmp->shortname = xstrdup(name);
		wpt_tmp->description = xstrdup(comment);
		wpt_tmp->altitude = wpt_alt;
		wpt_tmp->latitude = -wpt_lat;
		wpt_tmp->longitude = wpt_long;

		if (wpt_icon < 26)
			sprintf(tbuf, "%c", wpt_icon + 'a');
		else
			sprintf(tbuf, "a%c", wpt_icon - 26 + 'a');
		wpt_tmp->icon_descr = mag_find_descr_from_token(tbuf);

		waypt_add(wpt_tmp);
	}
	
	/* now read the routes... */
	my_fread4(&rte_count, mapsend_file_in);
	
	while (rte_count--) {
		rte_head = route_head_alloc();
		route_add_head(rte_head);
		
		/* route name */
		fread(&scount, sizeof(scount), 1, mapsend_file_in);
		fread(tbuf, scount, 1, mapsend_file_in);
		tbuf[scount] = '\0';
		rte_head->rte_name = xstrdup(tbuf);
		
		/* route # */
		my_fread4(&rte_num, mapsend_file_in);
		rte_head->rte_num = rte_num;
		
		/* points this route */
		my_fread4(&wpt_count, mapsend_file_in);
		
		while (wpt_count--) {
			wpt_tmp = waypt_new();

			/* waypoint name */
			fread(&scount, sizeof(scount), 1, mapsend_file_in);
			fread(tbuf, scount, 1, mapsend_file_in);
			tbuf[scount] = '\0';

			wpt_tmp->shortname = xstrdup(tbuf);
			
			/* waypoint # */
			my_fread4(&wpt_number, mapsend_file_in);
			my_fread8(&wpt_long, mapsend_file_in);
			my_fread8(&wpt_lat, mapsend_file_in);
			fread(&wpt_icon, sizeof(wpt_icon), 1, mapsend_file_in);

			wpt_tmp->longitude = wpt_long;
			wpt_tmp->latitude = -wpt_lat;

			if (wpt_icon < 26)
				sprintf(tbuf, "%c", wpt_icon + 'a');
			else
				sprintf(tbuf, "a%c", wpt_icon - 26 + 'a');
			wpt_tmp->icon_descr = mag_find_descr_from_token(tbuf);

			route_add_wpt(rte_head, wpt_tmp);
		}
	}
}

static void
mapsend_track_read(void)
{
	char *trk_name;
	unsigned char scount;
	unsigned int trk_count;
	double wpt_long;
	double wpt_lat;
	double wpt_alt;
	int i = 0;
	float f = 0;
	int time;
	int valid;
	unsigned char centisecs;
	route_head *track_head;
	waypoint *wpt_tmp;

	fread(&scount, sizeof(scount), 1, mapsend_file_in);
	trk_name = xmalloc(scount + 1);
	fread(trk_name, scount, 1, mapsend_file_in);
	trk_name[scount] = '\0';
	my_fread4(&trk_count, mapsend_file_in);

	track_head = route_head_alloc();
	track_head->rte_name = trk_name;
	track_add_head(track_head);

	while (trk_count--) {
		my_fread8(&wpt_long, mapsend_file_in);
		my_fread8(&wpt_lat, mapsend_file_in);
		if (mapsend_infile_version < 36) { /* < version 4.0 */
			my_fread4(&i, mapsend_file_in);
			wpt_alt = i;
		} else {
			my_fread4(&f, mapsend_file_in);
			wpt_alt = f;
		}
		my_fread4(&time, mapsend_file_in);
		my_fread4(&valid, mapsend_file_in);

		/* centiseconds only in >= version 3.0 */
		if (mapsend_infile_version >= 34) {
			fread(&centisecs, 1, 1, mapsend_file_in);
		} else {
			centisecs = 0;
		}

		wpt_tmp = waypt_new();
		wpt_tmp->latitude = -wpt_lat;
		wpt_tmp->longitude = wpt_long;
		wpt_tmp->creation_time = time;
		wpt_tmp->centiseconds = centisecs;
		wpt_tmp->altitude = wpt_alt;
		route_add_wpt(track_head, wpt_tmp);
	}
}

static void
mapsend_read(void)
{
	mapsend_hdr hdr;
	int type;
	char buf[3];

	/*
	 * Because of the silly struct packing and the goofy variable-length
	 * strings, each member has to be read in one at a time.  Grrr.
	 */

	fread(&hdr, sizeof(hdr), 1, mapsend_file_in);
	type = le_read16(&hdr.ms_type);
	strncpy(buf, hdr.ms_version, 2);
	buf[2] = '\0';
	
	mapsend_infile_version = atoi(buf);

	switch(type) {
		case ms_type_wpt:
			mapsend_wpt_read();
			break;
		case ms_type_track:
			mapsend_track_read();
			break;
		case ms_type_log:
			fatal(MYNAME ", GPS logs not supported.\n");
		case ms_type_rgn:
			fatal(MYNAME ", GPS regions not supported.\n");
		default:
			fatal(MYNAME ", unknown file type %d\n", type);
	}
}


static void
mapsend_waypt_pr(const waypoint *waypointp)
{
	int n;
	unsigned char c;
	double falt;
	double flong;
	double flat;
	static int cnt = 0;
	const char *iconp = NULL;
	const char *sn = global_opts.synthesize_shortnames ? 
		mkshort_from_wpt(mkshort_handle, waypointp) :
	       	waypointp->shortname;
	char *tmp;

	/*
	 * The format spec doesn't call out the character set of waypoint
	 * name and description.  Empirically, we can see that it's 8859-1,
	 * but if we create mapsend files containing those, Mapsend becomes
	 * grumpy uploading the resulting waypoints and being unable to deal
	 * with the resulting comm errors.
 	 * 
	 * Ironically, our own Magellan serial module strips the "naughty"
	 * characters, keeping it more in definition with their own serial
	 * spec. :-)
	 * 
	 * So we just decompose the utf8 strings to ascii before stuffing
	 * them into the Mapsend file.
	 */

	tmp = str_utf8_to_ascii(sn);
	c = tmp ? strlen(tmp) : 0;
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(tmp, c, 1, mapsend_file_out);
	if (tmp)
		xfree(tmp);

	tmp = str_utf8_to_ascii(waypointp->description);
	if (tmp)
		c = strlen(tmp);
	else
		c = 0;

	if (c > 30) c = 30;
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(tmp, c, 1, mapsend_file_out);
	if (tmp)
		xfree(tmp);

	/* #, icon, status */
	n = ++cnt;
	my_fwrite4(&n, mapsend_file_out);

	if (waypointp->icon_descr) {
		iconp = mag_find_token_from_descr(waypointp->icon_descr);
		if (1 == strlen(iconp)) {
			c = iconp[0] - 'a';
		} else {
			c = iconp[1] - 'a' + 26;
		}
	} else  {
		c = 0;
	}
	if (get_cache_icon(waypointp)) {
		iconp = mag_find_token_from_descr(get_cache_icon(waypointp));
		if (1 == strlen(iconp)) {
			c = iconp[0] - 'a';
		} else {
			c = iconp[1] - 'a' + 26;
		}
	}

	fwrite(&c, 1, 1, mapsend_file_out);
	c = 1;
	fwrite(&c, 1, 1, mapsend_file_out);

	falt = waypointp->altitude;
	if (falt == unknown_alt)
		falt = 0;
	my_fwrite8(&falt, mapsend_file_out);

	flong = waypointp->longitude;
	my_fwrite8(&flong, mapsend_file_out);
	flat = -waypointp->latitude;
	my_fwrite8(&flat, mapsend_file_out);
}

static void 
mapsend_route_hdr(const route_head *rte)
{
	int i;
	unsigned char c;
	char * rname;
	
	/* route name -- mapsend really seems to want something here.. */
	if (!rte->rte_name)
		rname = xstrdup("Route");
	else
		rname = xstrdup(rte->rte_name);

	c = strlen(rname);
	
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(rname, c, 1, mapsend_file_out);

	xfree(rname);
	
	/* route # */
	i = rte->rte_num;
 	my_fwrite4(&i, mapsend_file_out);
	
	i = rte->rte_waypt_ct;

	/* # of waypoints to follow... */
	my_fwrite4(&i, mapsend_file_out);
}

static void 
mapsend_noop(const route_head *wp)
{
	/* no-op */
}

static void 
mapsend_route_disp(const waypoint *waypointp)
{
	unsigned char c;
	const char *iconp;
	double dbl;

	route_wp_count++;
	
	/* waypoint name */
	c = waypointp->shortname ? strlen(waypointp->shortname) : 0;
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(waypointp->shortname, c, 1, mapsend_file_out);
	
	/* waypoint number */
	my_fwrite4(&route_wp_count, mapsend_file_out);

	dbl = waypointp->longitude;
	my_fwrite8(&dbl, mapsend_file_out);

	dbl = -waypointp->latitude;
	my_fwrite8(&dbl, mapsend_file_out);

	if (waypointp->icon_descr) {
		iconp = mag_find_token_from_descr(waypointp->icon_descr);
		if (1 == strlen(iconp)) {
			c = iconp[0] - 'a';
		} else {
			c = iconp[1] - 'a' + 26;
		}
	} else  {
		c = 0;
	}
	fwrite(&c, 1, 1, mapsend_file_out);
}

void mapsend_track_hdr(const route_head * trk)
{
	/* 
	 * we write mapsend v3.0 tracks as mapsend v2.0 tracks get
	 * tremendously out of whack time/date wise.
	 */
	char *verstring = "30";
	queue *elem, *tmp;
	char *tname;
	unsigned char c;
	int i;
	mapsend_hdr hdr = {13, "4D533334 MS", "30", ms_type_track, {0}};
	
	switch (trk_version) {
		case 20: verstring = "30"; break;
		case 30: verstring = "34"; break;
		case 40: verstring = "36"; break;
		default: fatal("Unknown track version."); break;
	}

	hdr.ms_version[0] = verstring[0];
	hdr.ms_version[1] = verstring[1];

	fwrite(&hdr, sizeof(hdr), 1, mapsend_file_out);

	/* track name */
	if (!trk->rte_name)
		tname = xstrdup("Track");
	else
		tname = xstrdup(trk->rte_name);

	c = strlen(tname);
	
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(tname, c, 1, mapsend_file_out);
	xfree(tname);
	
	/* total nodes (waypoints) this track */
	i = 0;
	QUEUE_FOR_EACH(&trk->waypoint_list, elem, tmp) {
		i++;
	}		

	my_fwrite4(&i, mapsend_file_out);
	
}

void mapsend_track_disp(const waypoint * wpt)
{
	unsigned char c;
	double dbl;
	int i;
	int t;
	int valid = 1;
	static int last_time;

	/*
	 * Firmware Ver 4.06 (at least) has a defect when it's set for .01km
	 * tracking that will sometimes result in timestamps in the track
	 * going BACKWARDS.   When mapsend sees this, it (stupidly) advances
	 * the date by one, ignoring the date on the TRK lines.   This looks 
	 * for time travel and just uses the previous time - it's better to
	 * be thought to be standing still than to be time-travelling!
	 *
	 * This is rumoured (but yet unconfirmed) to be fixed in f/w 5.12.
	 */
	t = wpt->creation_time;
	if (t < last_time)  {
		t = last_time;
	}
	
	/* x = longitude */
	dbl = wpt->longitude;
	my_fwrite8(&dbl, mapsend_file_out);

	/* x = latitude */
	dbl = -wpt->latitude;
	my_fwrite8(&dbl, mapsend_file_out);

	/* altitude */
	i = (int) wpt->altitude;
	my_fwrite4(&i, mapsend_file_out);
	
	/* time */
	my_fwrite4(&t, mapsend_file_out);
	last_time = t;

	/* validity */
	my_fwrite4(&valid, mapsend_file_out);

	/* 0 centiseconds */
	if (trk_version >= 30) {
		c = wpt->centiseconds;
		fwrite(&c, 1, 1, mapsend_file_out);
	}
}

void 
mapsend_track_write(void)
{
	track_disp_all(mapsend_track_hdr, mapsend_noop, mapsend_track_disp);
}

static void
mapsend_wpt_write(void)
{
	mapsend_hdr hdr = {13, {"4D533330 MS"}, {"30"}, ms_type_wpt, {0}};
	int wpt_count = waypt_count();
	int n = 0;
	
	if (global_opts.objective == trkdata) {
		mapsend_track_write();
	} else {
		fwrite(&hdr, sizeof(hdr), 1, mapsend_file_out);

		if (global_opts.objective == wptdata) {
			my_fwrite4(&wpt_count, mapsend_file_out);
			waypt_disp_all(mapsend_waypt_pr);
		} else 
		if (global_opts.objective == rtedata) {

			/* # of points - all routes */
			n = route_waypt_count();
			my_fwrite4(&n, mapsend_file_out);

			/* write points - all routes */
			route_disp_all(mapsend_noop, mapsend_noop, mapsend_waypt_pr);
		}
		
		n = route_count();

		my_fwrite4(&n, mapsend_file_out);
	
		if (n)
			route_disp_all(mapsend_route_hdr, mapsend_noop, mapsend_route_disp);
	}
}



ff_vecs_t mapsend_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	mapsend_rd_init,
	mapsend_wr_init,
	mapsend_rd_deinit,
	mapsend_wr_deinit,
	mapsend_read,
	mapsend_wpt_write,
	NULL
};
