/*
    Read and write Magellan Navigator Companion files.

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
#include "coldsync/palm.h"
#include "coldsync/pdb.h"

#define MYNAME "Companion Waypoints"
#define MYTYPE  0x54777074  	/* Twpt */
#define MYCREATOR 0x4d47747a 	/* MGtz */

struct record {
	pdb_16 crt_sec; /* Big endian, creation time */
	pdb_16 crt_min;
	pdb_16 crt_hour;
	pdb_16 crt_mday; 
	pdb_16 crt_mon; /* 1 = Jan */
	pdb_16 crt_year; /* includes century. */
	pdb_16 unknown;
	pdb_16 xx_sec; /* appears to be time, but we don't know what it is. */
	pdb_16 xx_min;
	pdb_16 xx_hour;
	pdb_16 xx_mday; 
	pdb_16 xx_mon; 
	pdb_16 xx_year;
	pdb_16 unknown2;
	pdb_32 latitude; /* lat * 1e5 */
	pdb_32 longitude; /* lon * 1e5 */
	pdb_32 elevation; /* meters */
	char plot; /* 1 = plot on map screen.   default = 0 */
};

static FILE *file_in;
static FILE *file_out;
static const char *out_fname;
struct pdb *opdb;
struct pdb_record *opdb_rec;

static void
rd_init(const char *fname)
{
	file_in = fopen(fname, "r");
	if (file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = fopen(fname, "w");
	out_fname = fname;
	if (file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
}

static void
wr_deinit(void)
{
	fclose(file_out);
}

static void
data_read(void)
{
	struct record *rec;
	struct pdb *pdb;
	struct pdb_record *pdb_rec;

	if (NULL == (pdb = pdb_Read(fileno(file_in)))) {
		fatal(MYNAME ": pdb_Read failed");
	}

	if ((pdb->creator != MYCREATOR) || (pdb->type != MYTYPE)) {
		fatal(MYNAME ": Not a Magellan Navigator file.");
	}

	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;
		char *vdata;
		struct tm tm = {0};

		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);
		rec = (struct record *) pdb_rec->data;
		wpt_tmp->position.altitude.altitude_meters = be_read32(&rec->elevation); 

		wpt_tmp->position.longitude.degrees = be_read32(&rec->longitude) / 1e5; 
		wpt_tmp->position.latitude.degrees = be_read32(&rec->latitude) / 1e5; 

		vdata = (char *) pdb_rec->data + sizeof(*rec);

                wpt_tmp->shortname = xstrdup(vdata);
		vdata += strlen (vdata) + 1;

		wpt_tmp->description = xstrdup(vdata);
		vdata += strlen (vdata) + 1;
		
		tm.tm_sec = be_read16(&rec->crt_sec);
		tm.tm_min = be_read16(&rec->crt_min);
		tm.tm_hour = be_read16(&rec->crt_hour);
		tm.tm_mday = be_read16(&rec->crt_mday);
		tm.tm_mon = be_read16(&rec->crt_mon) - 1;
		tm.tm_year = be_read16(&rec->crt_year) - 1900;
		wpt_tmp->creation_time = mktime(&tm); 

		waypt_add(wpt_tmp);

	} 
	free_pdb(pdb);
}


static void
my_writewpt(waypoint *wpt)
{
	struct record *rec;
	static int ct;
	struct tm *tm;
abort();
	rec = xcalloc(sizeof(*rec),1);
#if 0
	strncpy(rec->ID, wpt->shortname, sizeof(rec->ID));
	rec->ID[sizeof(rec->ID)-1] = 0;
	strncpy(rec->name, wpt->description, sizeof(rec->name));
	rec->name[sizeof(rec->name)-1] = 0;

	if (wpt->creation_time) {
		tm = gmtime(&wpt->creation_time);
		rec->min = tm->tm_min;
		rec->hour = tm->tm_hour;
		rec->sec = tm->tm_sec;
		rec->day = tm->tm_mday;
		rec->mon = tm->tm_mon + 1;
		rec->year = tm->tm_year - 100;
	} else {
		rec->min = 0xff;
		rec->hour = 0xff;
		rec->sec = 0xff;
		rec->day = 0xff;
		rec->mon = 0xff;
		rec->year = 0xff;
	}

	be_write32(&rec->longitude, wpt->position.longitude.degrees * 10000000.0);
	be_write32(&rec->latitude, wpt->position.latitude.degrees * 10000000.0);
	be_write32(&rec->elevation, wpt->position.altitude.altitude_meters * 100.0);

	opdb_rec = new_Record (0, 0, ct++, sizeof(*rec), (const ubyte *)rec);

	if (opdb_rec == NULL) {
		fatal(MYNAME ": libpdb couldn't create record");
	}

	if (pdb_AppendRecord(opdb, opdb_rec)) {
		fatal(MYNAME ": libpdb couldn't append record");
	}
#endif
}

struct hdr{
	char *wpt_name; 
	waypoint *wpt;
};

static void
data_write(void)
{
	int ct = waypt_count();
	struct hdr *htable;
        queue *elem, *tmp;

	if (NULL == (opdb = new_pdb())) { 
		fatal (MYNAME ": new_pdb failed\n");
	}

	strncpy(opdb->name, out_fname, PDB_DBNAMELEN);
	opdb->name[PDB_DBNAMELEN-1] = 0;
	opdb->attributes = PDB_ATTR_BACKUP;
	opdb->ctime = opdb->mtime = time(NULL) + 2082844800U;
	opdb->type = MYTYPE;  /* CWpt */
	opdb->creator = MYCREATOR; /* cGPS */
	opdb->version = 0;

	pdb_Write(opdb, fileno(file_out));
	free(htable);
}


ff_vecs_t magnav_vec = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
