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

static int endianness_tested;
static int i_am_little_endian;

static int route_wp_count;

#define MYNAME "mapsend"

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

static void
mapsend_rd_init(const char *fname, const char *args)
{
	mapsend_file_in = fopen(fname, "rb");
	if (mapsend_file_in == NULL) {
		fprintf(stderr, "Cannot open '%s' for reading\n", fname);
		exit(1);
	}
}

static void
mapsend_rd_deinit(void)
{
	fclose(mapsend_file_in);
}

static 
size_t 
my_fread8(void *ptr, FILE *stream)
{
	unsigned char cbuf[8];
	unsigned char *cptr = ptr;
	size_t rv;

	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		rv = fread(ptr, 8, 1, stream);
	} else { 
		rv = fread(cbuf, 8, 1, stream);
		cptr[0] = cbuf[7];
		cptr[1] = cbuf[6];
		cptr[2] = cbuf[5];
		cptr[3] = cbuf[4];
		cptr[4] = cbuf[3];
		cptr[5] = cbuf[2];
		cptr[6] = cbuf[1];
		cptr[7] = cbuf[0];
	}
	return rv;
}

static 
size_t
my_fwrite8(void *ptr, FILE *stream)
{
	unsigned char cbuf[8];
	unsigned char *cptr = ptr;

	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		return fwrite(ptr, 8, 1, stream);
	} else {
		cbuf[0] = cptr[7];
		cbuf[1] = cptr[6];
		cbuf[2] = cptr[5];
		cbuf[3] = cptr[4];
		cbuf[4] = cptr[3];
		cbuf[5] = cptr[2];
		cbuf[6] = cptr[1];
		cbuf[7] = cptr[0];
		return fwrite(cbuf, 8, 1, stream);
	}
}

static 
size_t 
my_fread4(void *ptr, FILE *stream)
{
	unsigned char cbuf[4];
	unsigned char *cptr = ptr;
	size_t rv;
	
	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		rv = fread(ptr, 4, 1, stream);
	} else {
		rv = fread(cbuf, 4, 1, stream);
		cptr[0] = cbuf[3];
		cptr[1] = cbuf[2];
		cptr[2] = cbuf[1];
		cptr[3] = cbuf[0];
	}
	return rv;
}

static 
size_t
my_fwrite4(int *ptr, FILE *stream)
{
	unsigned char cbuf[4];
	unsigned char *cptr = (char *) ptr;

	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		return fwrite(ptr, 4, 1, stream);
	} else {
		cbuf[0] = cptr[3];
		cbuf[1] = cptr[2];
		cbuf[2] = cptr[1];
		cbuf[3] = cptr[0];
		return fwrite(cbuf, 4, 1, stream);
	}
}

static void
mapsend_wr_init(const char *fname, const char *args)
{
	mapsend_file_out = fopen(fname, "wb");
	if (mapsend_file_out == NULL) {
		fprintf(stderr, "Cannot open '%s' for writing\n", fname);
		exit(1);
	}
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
		wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);

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
		wpt_tmp->position.altitude.altitude_meters = wpt_alt;
		wpt_tmp->position.latitude.degrees = -wpt_lat;
		wpt_tmp->position.longitude.degrees = wpt_long;

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
			wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);

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

			wpt_tmp->position.longitude.degrees = wpt_long;
			wpt_tmp->position.latitude.degrees = -wpt_lat;

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
	float wpt_alt;
	int time;
	int valid;
	int centisecs;
	route_head *track_head;
	waypoint *wpt_tmp;

	fread(&scount, sizeof(scount), 1, mapsend_file_in);
	trk_name = xmalloc(scount + 1);
	fread(trk_name, scount, 1, mapsend_file_in);
	trk_name[scount] = '\0';
	my_fread4(&trk_count, mapsend_file_in);

	track_head = route_head_alloc();
	track_head->rte_name = trk_name;
	route_add_head(track_head);

	while (trk_count--) {
		my_fread8(&wpt_long, mapsend_file_in);
		my_fread8(&wpt_lat, mapsend_file_in);
		my_fread4(&wpt_alt, mapsend_file_in);
		my_fread4(&time, mapsend_file_in);
		my_fread4(&valid, mapsend_file_in);
		fread(&centisecs, 1, 1, mapsend_file_in);

		wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
		wpt_tmp->position.altitude.altitude_meters = wpt_alt;
		wpt_tmp->position.latitude.degrees = -wpt_lat;
		wpt_tmp->position.longitude.degrees = wpt_long;
		wpt_tmp->creation_time = time;
		route_add_wpt(track_head, wpt_tmp);
	}
}

static void
mapsend_read(void)
{
	mapsend_hdr hdr;
	int type;

	/*
	 * Becuase of the silly struct packing and the goofy variable-length
	 * strings, each member has to be read in one at a time.  Grrr.
	 */

	fread(&hdr, sizeof(hdr), 1, mapsend_file_in);
	type = le_read16(&hdr.ms_type);

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
	const char *iconp;
	const char *sn = global_opts.synthesize_shortnames ? 
		mkshort(mkshort_handle, waypointp->description) :
	       	waypointp->shortname;

	c = strlen(sn);
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(sn, c, 1, mapsend_file_out);

	if (waypointp->description) 
		c = strlen(waypointp->description);
	else
		c = 0;
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(waypointp->description, c, 1, mapsend_file_out);

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
	fwrite(&c, 1, 1, mapsend_file_out);
c = 1;
		
	fwrite(&c, 1, 1, mapsend_file_out);

	falt = waypointp->position.altitude.altitude_meters;
	my_fwrite8(&falt, mapsend_file_out);

	flong = waypointp->position.longitude.degrees;
	my_fwrite8(&flong, mapsend_file_out);
	flat = -waypointp->position.latitude.degrees;
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
mapsend_noop()
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
	c = strlen(waypointp->shortname);
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(waypointp->shortname, c, 1, mapsend_file_out);
	
	/* waypoint number */
	my_fwrite4(&route_wp_count, mapsend_file_out);

	dbl = waypointp->position.longitude.degrees;
	my_fwrite8(&dbl, mapsend_file_out);

	dbl = -waypointp->position.latitude.degrees;
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

static void
mapsend_wpt_write(void)
{
	mapsend_hdr hdr = {13, "4D533330 MS", "30", ms_type_wpt, 0};
	int wpt_count = waypt_count();
	int n = 0;
	
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

#if LATER
void 
mapsend_trk_write(void)
{
	mapsend_hdr hdr = {13, "4D533334 MS", "34", ms_type_track, 0};
}
#endif

ff_vecs_t mapsend_vecs = {
	mapsend_rd_init,
	mapsend_wr_init,
	mapsend_rd_deinit,
	mapsend_wr_deinit,
	mapsend_read,
	mapsend_wpt_write,
};
