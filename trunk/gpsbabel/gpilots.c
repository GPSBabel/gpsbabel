/*
    Read and write GPilotS files.

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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

#define MYNAME "GPilotS"
#define MYWPT  0x57707473  	/* Wpts */
#define MYTRK  0x54726b73  	/* Trks */
#define MYRTE  0x57707473  	/* Wpts */
#define MYCREATOR 0x4750696c 	/* GPil */


/*
 * Structures grafted from http://www.cru.fr/perso/cc/GPilotS/
 */

typedef struct
{
	long lat;			/* latitude in semicircles */
	long lon;			/* longitude in semicircles */
}
Semicircle_Type;

typedef struct
{
	char ident[6];			/* identifier */
	unsigned char lat[4];		/* position */
	unsigned char lon[4];		/* position */
	unsigned char unused[4];	/* should be set to zero */
	char cmnt[40];			/* comment */
	unsigned char smbl;		/* symbol id */
	unsigned char dspl;		/* display option */
} D103_Wpt_Type;


typedef struct			       /* structure de waypoint "interne" */
{
    unsigned char ident[51];	       /* identifier (50 + '0') */
    Semicircle_Type posn;	       /* position (common to all Garmin types) */
    unsigned char cmnt[51];	       /* comment (50 + '0') */
    float dst;			       /* proximity distance */
    float alt;			       /* altitude */
    int smbl;			       /* symbol id */
    unsigned char dspl;		       /* display option */
    unsigned char color;	       /* color */
}
Custom_Wpt_Type;

typedef struct			       /* internal track header */
{
    char name[256];		       /* nom du groupe de trackpoints */
    unsigned char dspl;		       /* display on the map ? */
    unsigned char color;	       /* color */
    unsigned char type;		       /* type of following track points */
    unsigned char number[4];	       /* number of track points */
    unsigned char latmin[4];	       /* latitude min */
    unsigned char latmax[4];	       /* latitude max */
    unsigned char lonmin[4];	       /* longitude min */
    unsigned char lonmax[4];	       /* longitude max */
}
Custom_Trk_Hdr_Type;

typedef struct
{
	unsigned char lat[4];		/* position */
	unsigned char lon[4];		/* position */
	unsigned char time[4];
	unsigned char alt[4];
	unsigned char new_trk;
} Custom_Trk_Point_Type;

struct record
{
	struct {
		unsigned char type;
		unsigned short size;
		unsigned int version; 
	} header;
	union {
		D103_Wpt_Type d103;
		Custom_Wpt_Type CustWpt;
		Custom_Trk_Hdr_Type CustTrkHdr;
#if LATER
		Custom_Rte_Hdr_Type CustRteHdr;
#endif
	} wpt;
};


#if 0
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
#endif

static FILE *file_in;
static FILE *file_out;
static const char *out_fname;
struct pdb *opdb;
struct pdb_record *opdb_rec;

static char *dbname = NULL;

static
arglist_t my_args[] = {
	{"dbname", &dbname, "Database name"},
	{0, 0, 0}
};

static void
rd_init(const char *fname, const char *args)
{
	file_in = fopen(fname, "rb");
	if (file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
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
wr_init(const char *fname, const char *args)
{
	file_out = fopen(fname, "wb");
	out_fname = fname;
	if (file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
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
	route_head *track_head;

	if (NULL == (pdb = pdb_Read(fileno(file_in)))) {
		fatal(MYNAME ": pdb_Read failed\n");
	}

	if (pdb->creator != MYCREATOR) {
		fatal(MYNAME ": Not a %s file.\n", MYNAME);
	}

	switch(pdb->type) {
		case MYWPT:
			/* blah */
			break;
		case MYTRK:
			/* blah */
			break;
		default:
			fatal(MYNAME ": Unknown file type 0x%x\n", pdb->type);
	}
	
	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;
		Custom_Trk_Point_Type *tp;
		int lat;
		int lon;
		int sz;

		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

		rec = (struct record *) pdb_rec->data;
		switch(rec->header.type) {
			/*
			 * G103Type
			 */
			case 4:
				wpt_tmp->shortname = xstrndupt(rec->wpt.d103.cmnt, sizeof(rec->wpt.d103.ident));
				wpt_tmp->description = xstrndupt(rec->wpt.d103.cmnt, sizeof(rec->wpt.d103.cmnt));
				/* This is odd.   This is a Palm DB file,
				 * yet the data appears to be little endian,
				 * not appropriate the the actual Palm.
				 */
				lon = le_read32(&rec->wpt.d103.lon);
				lat = le_read32(&rec->wpt.d103.lat);
				wpt_tmp->position.longitude.degrees = lon / 2147483648.0 * 180.0;
				wpt_tmp->position.latitude.degrees = lat / 2147483648.0 * 180.0;
				waypt_add(wpt_tmp);
				break;
			/*
			 * CustomTrkHdr
			 */
			case 101:
				track_head = route_head_alloc();
				route_add_head(track_head);
				track_head->rte_name = xstrndup(rec->wpt.CustTrkHdr.name, sizeof(rec->wpt.CustTrkHdr.name));
				sz = be_read32(&rec->wpt.CustTrkHdr.number);
				tp = (Custom_Trk_Point_Type *) ((char *) pdb_rec->data + sizeof(rec->wpt.CustTrkHdr));
				/* FIXME: This is incomplete and probably wrong */
				while (sz--) {
					wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);
					lon = le_read32(&tp->lon);
					lat = le_read32(&tp->lat);
					wpt_tmp->position.longitude.degrees = lon / 2147483648.0 * 180.0;
					wpt_tmp->position.latitude.degrees = lat / 2147483648.0 * 180.0;
					route_add_wpt(track_head, wpt_tmp);
					tp++;
				}
				break;
			default:
				fatal(MYNAME ": input record type %d not supported.\n", rec->header.type); 
		}
#if 0
		if ( be_read32(&rec->elevation) == -100000000 ) {
			wpt_tmp->position.altitude.altitude_meters = unknown_alt;
		}
		else {
			wpt_tmp->position.altitude.altitude_meters = be_read32(&rec->elevation) / 100.0;
		}
			
		wpt_tmp->position.longitude.degrees = be_read32(&rec->longitude) / 10000000.0; 
		wpt_tmp->position.latitude.degrees = be_read32(&rec->latitude) / 10000000.0;
			
		if (be_read16(&rec->year) != 0xff) {
			struct tm tm = {0};
		
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
#endif

	} 
	free_pdb(pdb);
}


static void
my_writept(waypoint *wpt)
{
	struct record *rec;
	static int ct;
	struct tm *tm;
	char *vdata;

	rec = xcalloc(sizeof(*rec)+1018,1);
#if 0
	if (wpt->creation_time) {
		tm = gmtime(&wpt->creation_time);
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

	be_write32(&rec->longitude, wpt->position.longitude.degrees * 10000000.0);
	be_write32(&rec->latitude, wpt->position.latitude.degrees * 10000000.0);
	if ( wpt->position.altitude.altitude_meters == unknown_alt ) {
		be_write32(&rec->elevation, -100000000U);
	}
	else {
		be_write32(&rec->elevation, wpt->position.altitude.altitude_meters * 100.0);
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
			strncpy( vdata, wpt->shortname, 16 );
				vdata[15] = '\0';
	}
	else {
			vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;
	
	if ( wpt->description ) {
			strncpy( vdata, wpt->description, 501 );
				vdata[500] = '\0';
	}
	else {
			vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;
	
	if ( wpt->notes ) {
			strncpy( vdata, wpt->notes, 501 );
				vdata[500] = '\0';
	}
	else {
			vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;
#endif	
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
#if SOON
	opdb->type = MYTYPE;  /* CWpt */
	opdb->creator = MYCREATOR; /* cGPS */
#endif
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
		bh->wpt_name = waypointp->shortname;
		bh ++;
	}
	qsort(htable, ct, sizeof(*bh), compare);

	for (i=0;i<ct;i++) {
		my_writept(htable[i].wpt);
	}

	pdb_Write(opdb, fileno(file_out));
	xfree(htable);
}


ff_vecs_t gpilots_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	my_args
};
