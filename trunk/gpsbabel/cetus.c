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

#define NOTESZ 4096
#define DESCSZ 4096

typedef enum {
	WptEdit = 0, 	/* the position has been edited or it was */
			/* imported from another source */
	WptGPS2D = 1,   /* retrieved from a GPS with a 2D fix */
	WptGPS3D = 2,	/* retrieved from a GPS with a 3D fix */
	WptDGPS2D = 3,	/* retrieved from a GPS with a 2D fix and DGPS signal */
	WptDGPS3D = 4, 	/* retrieved from a GPS with a 3D fix and DGPS signal */
	WptAverage = 5,	/* averaging over 3D positions */
	WptCache = 50,	/* this position is a geocache reference */
	WptGarmin = 70	/* this position was imported from a Garmin GPS */
			/* the icon field contains the garmin symbol number */
} wpt_type;

struct record {
	char type;	
	
	char   readonly;
	
	pdb_32 latitude; /* Big endian, degrees*1e7, s=negative */
	pdb_32 longitude; /* same as lat; w=negative */
	pdb_32 elevation; /* Big endian, meters*100. blank=-1e8 */

	pdb_16        year; /* sample time, UTC */
	unsigned char mon;
	unsigned char day;
	unsigned char hour;
	unsigned char min;
	unsigned char sec;
	
	/* accuracy and precision information for use where applicable */
	char  sat; /* ff if averaged or unknown */
	pdb_16 pdop; /* pdop * 100 */
	pdb_16 hdop;
	pdb_16 vdop;
	pdb_16 dgpstime;
	pdb_32 dgpsstn;
	pdb_32 avgtime;
	pdb_32 avgite;

	pdb_16 dopmask;
	pdb_16 elevmask;
	
	pdb_16 radius;
	pdb_32 distance;
	
	pdb_16 vyear; /* date visited */
	unsigned char vmon;
	unsigned char vday;
	unsigned char vhour;
	unsigned char vmin;
	unsigned char vsec;
	
	char   flagged;
	
	pdb_32 icon;
	pdb_16 category;
};

static FILE *file_in;
static FILE *file_out;
static const char *out_fname;
struct pdb *opdb;
struct pdb_record *opdb_rec;
static void *mkshort_wr_handle;

static char *dbname = NULL;
static char *appendicon = NULL;

static
arglist_t cetus_args[] = {
	{"dbname", &dbname, "Database name", ARGTYPE_STRING },
	{"appendicon", &appendicon, "Append icon_descr to description.",
		ARGTYPE_BOOL },
	{0, 0, 0, 0 }
};

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(file_in);
	if ( dbname ) {
	    xfree(dbname);
	    dbname = NULL;
	}
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "wb", MYNAME);
	out_fname = fname;
}

static void
wr_deinit(void)
{
	fclose(file_out);
	if ( dbname ) {
	    xfree(dbname);
	    dbname = NULL;
	}
}

static void
data_read(void)
{
	struct record *rec;
	struct pdb *pdb;
	struct pdb_record *pdb_rec;
	char *vdata;

	if (NULL == (pdb = pdb_Read(fileno(file_in)))) {
		fatal(MYNAME ": pdb_Read failed\n");
	}

	if ((pdb->creator != MYCREATOR) || (pdb->type != MYTYPE)) {
		fatal(MYNAME ": Not a Cetus file.\n");
	}
	
	if (pdb->version < 1) {
	       fatal(MYNAME ": This file is from an obsolete beta version of Cetus GPS and is unsupported.\n");
        }
        if (pdb->version > 1) {
	       fatal(MYNAME ": This file is from an unsupported newer version of Cetus GPS.  It may be supported in a newer version of GPSBabel.\n");
	}

	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;

		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

		rec = (struct record *) pdb_rec->data;
		if ( be_read32(&rec->elevation) == -100000000 ) {
			wpt_tmp->altitude = unknown_alt;
		}
		else {
			wpt_tmp->altitude = be_read32(&rec->elevation) / 100.0;
		}
			
		wpt_tmp->longitude = be_read32(&rec->longitude) / 10000000.0; 
		wpt_tmp->latitude = be_read32(&rec->latitude) / 10000000.0;
	        	
		if (be_read16(&rec->year) != 0xff) {
			struct tm tm;

			memset (&tm, sizeof(tm), 0);
			tm.tm_min = rec->min;
			tm.tm_hour = rec->hour;
			tm.tm_mday = rec->day;
			tm.tm_mon = rec->mon - 1;
			tm.tm_year = be_read16(&rec->year) - 1900;

			wpt_tmp->creation_time = mktime(&tm); 
			
		}

		vdata = (char *) pdb_rec->data + sizeof(*rec);
		
		wpt_tmp->shortname = xstrdup(vdata);
		vdata = vdata + strlen(vdata) + 1;
		
		wpt_tmp->description = xstrdup(vdata);
		vdata = vdata + strlen(vdata) + 1;
		
		wpt_tmp->notes = xstrdup(vdata);
		
		waypt_add(wpt_tmp);

	} 
	free_pdb(pdb);
}


static void
cetus_writewpt(const waypoint *wpt)
{
	struct record *rec;
	static int ct;
	struct tm *tm;
	char *vdata;
	char *desc_long;
	char *desc_short;
	char *desc;

	rec = xcalloc(sizeof(*rec)+18 + NOTESZ + DESCSZ,1);

	if (wpt->creation_time && (NULL != (tm = gmtime(&wpt->creation_time)))){
		rec->min = tm->tm_min;
		rec->hour = tm->tm_hour;
		rec->sec = tm->tm_sec;
		rec->day = tm->tm_mday;
		rec->mon = tm->tm_mon + 1;
		be_write16( &rec->year, tm->tm_year + 1900);
	} else {
		rec->min = 0xff;
		rec->hour = 0xff;
		rec->sec = 0xff;
		rec->day = 0xff;
		rec->mon = 0xff;
		be_write16(&rec->year, 0xff);
	}

	be_write32(&rec->longitude, wpt->longitude * 10000000.0);
	be_write32(&rec->latitude, wpt->latitude * 10000000.0);
	if ( wpt->altitude == unknown_alt ) {
		be_write32(&rec->elevation, -100000000U);
	}
	else {
		be_write32(&rec->elevation, wpt->altitude * 100.0);
	}
	
	be_write16( &rec->pdop, 0xffff );
	be_write16( &rec->hdop, 0xffff );
	be_write16( &rec->vdop, 0xffff );
	be_write16( &rec->dgpstime, 0xffff );	
	be_write32( &rec->distance, 0xffffffff );
	
	rec->vmin = 0xff;
	rec->vhour = 0xff;
	rec->vsec = 0xff;
	rec->vday = 0xff;
	rec->vmon = 0xff;
	be_write16(&rec->vyear, 0xff);
	
	rec->sat = 0xff;

	vdata = (char *)rec + sizeof(*rec);
	if ( wpt->shortname ) {
			char *sn = str_utf8_to_cp1252(wpt->shortname);
		        strncpy( vdata, sn, 16 );
		        vdata[15] = '\0';
			xfree(sn);
	}
	else {
		        vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;

	if (wpt->gc_data.desc_short.utfstring) {
		char *stripped_html = strip_html(&wpt->gc_data.desc_short);
		desc_short = xstrdup("\n\n");
		desc_short = xstrappend(desc_short, str_utf8_to_cp1252(stripped_html));
		xfree(stripped_html);
	} else {
		desc_short = xstrdup("");
	}

	if (wpt->gc_data.desc_long.utfstring) {
		char *stripped_html = strip_html(&wpt->gc_data.desc_long);
		desc_long = xstrdup("\n\n");
		desc_long = xstrappend(desc_long, str_utf8_to_cp1252(stripped_html));
		xfree(stripped_html);
	} else {
		desc_long = xstrdup("");
	}

	desc = wpt->description ? str_utf8_to_cp1252(wpt->description) : 
		xstrdup("");

	snprintf(vdata, DESCSZ, "%s%s%s", 
		desc,
		desc_short,
		desc_long);

	xfree(desc);
	xfree(desc_short);
	xfree(desc_long);

	if (appendicon && wpt->icon_descr) {
		int left = DESCSZ - strlen( vdata );
		int ilen = strlen( wpt->icon_descr );
		if (ilen && left > (ilen+3)) {
			strcat( vdata, " (" );
			strcat( vdata, wpt->icon_descr );
			strcat( vdata, ")" );
		}
	}
	vdata += strlen( vdata ) + 1;

	if (wpt->gc_data.hint) {
		char *hint = str_utf8_to_cp1252(wpt->gc_data.hint);
		rec->type = WptCache;
		strncpy( vdata, hint, NOTESZ + 1 ) ;
		xfree(hint);
		vdata[NOTESZ] = '\0';
	} else {
		rec->type = WptEdit;
		vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;
	
	opdb_rec = new_Record (0, 2, ct++, vdata-(char *)rec, (const ubyte *)rec);
	
	if (opdb_rec == NULL) {
		fatal(MYNAME ": libpdb couldn't create record\n");
	}

	if (pdb_AppendRecord(opdb, opdb_rec)) {
		fatal(MYNAME ": libpdb couldn't append record\n");
	}
	xfree(rec);
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
	mkshort_wr_handle = mkshort_new_handle();
	setshort_length(mkshort_wr_handle, 15);
	setshort_whitespace_ok(mkshort_wr_handle, 0);

	if (NULL == (opdb = new_pdb())) { 
		fatal (MYNAME ": new_pdb failed\n");
	}

	if ( dbname ) {
	    strncpy( opdb->name, dbname, PDB_DBNAMELEN );
	}
	else {
	    strncpy(opdb->name, out_fname, PDB_DBNAMELEN);
	}
	opdb->name[PDB_DBNAMELEN-1] = 0;
	opdb->attributes = PDB_ATTR_BACKUP;
	opdb->ctime = opdb->mtime = time(NULL) + 2082844800U;
	opdb->type = MYTYPE;  /* CWpt */
	opdb->creator = MYCREATOR; /* cGPS */
	opdb->version = 1;

	/*
	 * All this is to sort by waypoint names before going to Cetus.
	 * Turns out plain old strcmp will do the trick...
	 */

	htable = xmalloc(ct * sizeof(*htable));
	bh = htable;

        QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
	    waypointp = (waypoint *) elem;
	    bh->wpt = waypointp;
	    if (global_opts.synthesize_shortnames && waypointp->description) {
		if (waypointp->shortname)
		    xfree(waypointp->shortname);
		waypointp->shortname = mkshort(mkshort_wr_handle, waypointp->description);
	    }
	    bh->wpt_name = waypointp->shortname;
	    bh ++;
	}
	qsort(htable, ct, sizeof(*bh), compare);

	for (i=0;i<ct;i++) {
		cetus_writewpt(htable[i].wpt);
	}

	pdb_Write(opdb, fileno(file_out));
	xfree(htable);
	mkshort_del_handle(mkshort_wr_handle);
}


ff_vecs_t cetus_vecs = {
	ff_type_file,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	cetus_args,
};
