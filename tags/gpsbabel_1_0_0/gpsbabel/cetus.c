/*
    Read and write Cetus files.

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

#define MYNAME "Cetus"
#define MYTYPE  0x43577074  	/* CWpt */
#define MYCREATOR 0x63475053 	/* cGPS */

struct record {
	char type;
	char ID[16]; /* Zero-terminated string. */
	char name[61]; /* zero-terminated */
	pdb_32 latitude; /* Big endian, degrees*1e7, s=negative */
	pdb_32 longitude; /* same as lat; w=negative */
	unsigned char hour;  /* sample time, UTC */
	unsigned char min;
	unsigned char sec;
	unsigned char year; /* 2002 = 02 */
	unsigned char mon;
	unsigned char day;
	pdb_32 elevation; /* Big endian, meters*100. blank=-1e8 */
	char fix_quality; /* 3 = 3d fix, 0=no fix */
	char sats_tracked; /* ff if averaged or unknown */
	pdb_16 hdop;
	pdb_16 dgpstime;
	pdb_32 dgpsstn;
	pdb_32 avgtime;
	pdb_32 avgite;
	pdb_16 icon;
	char category;
	char flagged;
	char readonly;
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
		fatal(MYNAME ": Not a Cetus file.");
	}

	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;

		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

		rec = (struct record *) pdb_rec->data;
		wpt_tmp->shortname = xstrdup(rec->ID);
		wpt_tmp->description = xstrdup(rec->name);
		wpt_tmp->position.altitude.altitude_meters = be_read32(&rec->elevation) / 100.0;

		wpt_tmp->position.longitude.degrees = be_read32(&rec->longitude) / 10000000.0; 
		wpt_tmp->position.latitude.degrees = be_read32(&rec->latitude) / 10000000.0; 
		if (rec->year != 0xff) {
			struct tm tm = {0};
		
			tm.tm_min = rec->min;
			tm.tm_hour = rec->hour;
			tm.tm_mday = rec->day;
			tm.tm_mon = rec->mon - 1;
			tm.tm_year = rec->year + 100;

			wpt_tmp->creation_time = mktime(&tm); 
			
		}
		waypt_add(wpt_tmp);

	} 
	free_pdb(pdb);
}


static void
cetus_writewpt(waypoint *wpt)
{
	struct record *rec;
	static int ct;
	struct tm *tm;

	rec = xcalloc(sizeof(*rec),1);

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
}

struct hdr{
	char *wpt_name; 
	waypoint *wpt;
};

static
int 
compare(const void *a, const void *b)
{
	const struct hdr *wa = a;
	const struct hdr *wb = b;

	return strcmp(wa->wpt->shortname, wb->wpt->shortname);
}

static void
data_write(void)
{
	int i, ct = waypt_count();
	struct hdr *htable, *bh;
        queue *elem, *tmp;
	extern queue waypt_head;
        waypoint *waypointp;

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

	/*
	 * All this is to sort by waypoint names before going to Cetus.
	 * Turns out plain old strcmp will do the trick...
	 */

	htable = xmalloc(ct * sizeof(*htable));
	bh = htable;

        QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
                waypointp = (waypoint *) elem;
		bh->wpt = waypointp;
		bh->wpt_name = waypointp->shortname;
		bh ++;
	}
	qsort(htable, ct, sizeof(*bh), compare);

	for (i=0;i<ct;i++) {
		cetus_writewpt(htable[i].wpt);
	}

	pdb_Write(opdb, fileno(file_out));
	free(htable);
}


ff_vecs_t cetus_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
