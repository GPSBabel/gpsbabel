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
	char unknown3; /* always 'a' */
};

static FILE *file_in;
static FILE *file_out;
static const char *out_fname;
static void *mkshort_handle;

struct pdb *opdb;
struct pdb_record *opdb_rec;

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "wb", MYNAME);
	out_fname = fname;
	mkshort_handle = mkshort_new_handle();
	setshort_length(mkshort_handle, 20);
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(mkshort_handle);
}

static void
data_read(void)
{
	struct record *rec;
	struct pdb *pdb;
	struct pdb_record *pdb_rec;

	if (NULL == (pdb = pdb_Read(fileno(file_in)))) {
		fatal(MYNAME ": pdb_Read failed\n");
	}

	if ((pdb->creator != MYCREATOR) || (pdb->type != MYTYPE)) {
		fatal(MYNAME ": Not a Magellan Navigator file.\n");
	}

	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;
		char *vdata;
		struct tm tm;

		memset (&tm, sizeof(tm), 0);
		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);
		rec = (struct record *) pdb_rec->data;
		wpt_tmp->altitude = be_read32(&rec->elevation); 

		wpt_tmp->longitude = be_read32(&rec->longitude) / 1e5; 
		wpt_tmp->latitude = be_read32(&rec->latitude) / 1e5; 

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
my_writewpt(const waypoint *wpt)
{
	struct record *rec;
	static int ct;
	struct tm *tm;
	char *vdata;
	time_t tm_t;
	const char *sn = global_opts.synthesize_shortnames ?
		mkshort(mkshort_handle, wpt->description) :
		wpt->shortname;

	rec = xcalloc(sizeof(*rec)+56,1);

        tm = NULL;	
	if ( wpt->creation_time ) {
		tm = gmtime( &wpt->creation_time);
	}
	if ( !tm ) {
		tm_t = current_time();
		tm = gmtime( &tm_t );
	}
	
	be_write16( &rec->crt_sec, tm->tm_sec );
	be_write16( &rec->crt_min, tm->tm_min );
	be_write16( &rec->crt_hour, tm->tm_hour );
	be_write16( &rec->crt_mday, tm->tm_mday );
	be_write16( &rec->crt_mon, tm->tm_mon + 1 );
	be_write16( &rec->crt_year, tm->tm_mon + 1900 );
	
        be_write16( &rec->unknown, 0);
	
	be_write16( &rec->xx_sec, tm->tm_sec );
	be_write16( &rec->xx_min, tm->tm_min );
	be_write16( &rec->xx_hour, tm->tm_hour );
	be_write16( &rec->xx_mday, tm->tm_mday );
	be_write16( &rec->xx_mon, tm->tm_mon + 1 );
	be_write16( &rec->xx_year, tm->tm_mon + 1900 );
	
        be_write16( &rec->unknown2, 0);
	
	be_write32(&rec->longitude, si_round(wpt->longitude * 100000.0));
	be_write32(&rec->latitude, si_round(wpt->latitude * 100000.0));
	be_write32(&rec->elevation, wpt->altitude);

	rec->plot = 0;
	rec->unknown3 = 'a';
	
	vdata = (char *)rec + sizeof(*rec);
	if ( sn ) {
		strncpy( vdata, sn, 21 );
		vdata[20] = '\0';
	}
	else { 
		vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;
	if ( wpt->description ) {
		strncpy( vdata, wpt->description, 33 );
		vdata[32] = '\0';
	}
	else {
		vdata[0] = '\0';
	}
	vdata += strlen( vdata ) + 1;
	vdata[0] = '\0';
	vdata[1] = '\0';
	vdata += 2;
	
	opdb_rec = new_Record (0, 0, ct++, vdata-(char *)rec, (const ubyte *)rec);

	if (opdb_rec == NULL) {
		fatal(MYNAME ": libpdb couldn't create record\n");
	}

	if (pdb_AppendRecord(opdb, opdb_rec)) {
		fatal(MYNAME ": libpdb couldn't append record\n");
	}
	xfree(rec);
}

static void
data_write(void)
{
	static char *appinfo = 
		"\0\x01"
		"User\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\x01\x02\x03\x04\x05\x06\x07\x08"
		"\x09\x0a\x0b\x0c\x0d\x0e\x0f\0\0";
	
	if (NULL == (opdb = new_pdb())) { 
		fatal (MYNAME ": new_pdb failed\n");
	}

	strncpy(opdb->name, "Companion Waypoints", PDB_DBNAMELEN);
	opdb->name[PDB_DBNAMELEN-1] = 0;
	opdb->attributes = PDB_ATTR_BACKUP;
	opdb->ctime = opdb->mtime = current_time() + 2082844800U;
	opdb->type = MYTYPE;  /* CWpt */
	opdb->creator = MYCREATOR; /* cGPS */
	opdb->version = 1;
	opdb->appinfo = (void *)appinfo;
	opdb->appinfo_len = 276;

	waypt_disp_all(my_writewpt);
	
	pdb_Write(opdb, fileno(file_out));
}


ff_vecs_t magnav_vec = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL
};
