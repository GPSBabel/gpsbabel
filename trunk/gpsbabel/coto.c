/*
    Read and write Coto files.

    Copyright (C) 2005 Tobias Minich, robertlipe@usa.net
    
    Based on the Cetus I/O Filter,
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
#include "csv_util.h"
#include "coldsync/palm.h"
#include "coldsync/pdb.h"

#define MYNAME "cotoGPS"
#define MYTYPETRACK	0x5452434b  	/* TRCK */
#define MYTYPEWPT	0x44415441  	/* DATA */
#define MYCREATOR		0x636f4750	/* coGP */

#define NOTESZ 4096
#define DESCSZ 4096

typedef enum {
	cotofixNone = 0,		/* No Fix or Warning */
	cotofixReserved = 1,	/* Shouldn't occur*/
	cotofix2D = 2,			/* retrieved from a GPS with a 2D fix */
	cotofix3D = 3,			/* retrieved from a GPS with a 3D fix  */
	cotofixDGPS = 4,		/* retrieved from a GPS with a DGPS signal */
} fix_quality;

struct record_track {

	pdb_double latitude;	/* radians, s=negative */
	pdb_double longitude;	/* same as lat; e=negative */
	pdb_double distance;	/* Distance to thel last point; discarded since it's calculated by gpsbabel on write */
	pdb_double arc;		/* Course, unknown dimension */
	pdb_double x,y;		/* Internal virtual coordinates used for drawing the track on the Palm */

	word alt;		/* Altitude */

	/* accuracy and precision information for use where applicable */
	uword hdop; /* _dop * 10 */
	uword vdop;
	uword pdop;
	ubyte sat_tracked;
	ubyte fix_quality;

	uword speed; /* *10 */
	udword time; /* Palm Time */
};

#define MAX_MARKER_NAME_LENGTH 20
struct record_wpt {
     double lon, lat;
     char name[MAX_MARKER_NAME_LENGTH];
};


// We need the pdb AppInfo for waypoint categories

typedef char appinfo_category[16];

#define APPINFO_PACKED_SIZE sizeof(uword)+16*sizeof(appinfo_category)+17*sizeof(ubyte)
struct appinfo {
	uword renamedCategories;
	appinfo_category categories[16];
	ubyte ids[16];
	ubyte maxid;
};

static FILE *file_in;
static FILE *file_out;
static const char *out_fname;
static const char *in_fname; /* We might need that for naming tracks */
struct pdb *opdb;

static char *trackname = NULL;
static char *zerocat = NULL;
static char *internals = NULL;

static
arglist_t coto_args[] = {
	{"trackname", &trackname, "Track name", NULL, ARGTYPE_STRING },
	{"zerocat", &zerocat, "Name of the 'unassigned' category.", NULL, ARGTYPE_STRING },
	{"internals", &internals, "Export some internal stuff to notes.", NULL, ARGTYPE_STRING|ARGTYPE_HIDDEN },
	{0, 0, 0, 0, 0 }
};

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "rb", MYNAME);
	in_fname = fname;
}

static void
rd_deinit(void)
{
	fclose(file_in);
	if ( trackname ) {
	    xfree(trackname);
	    trackname = NULL;
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
	if ( trackname ) {
	    xfree(trackname);
	    trackname = NULL;
	}
}

static void
coto_track_read(struct pdb *pdb)
{
	struct record_track *rec;
	struct pdb_record *pdb_rec;
	route_head *trk_head;
	char *loctrackname = NULL;
	
	if (trackname)
		// Given by user
		loctrackname = xstrdup(trackname);
	else if (strncmp(pdb->name, "cotoGPS TrackDB", PDB_DBNAMELEN))
		// Use database name if not default
		loctrackname = xstrndup(pdb->name, PDB_DBNAMELEN);
	else {
		// Use filename
		const char *fnametmp = strrchr(in_fname, '/'); // FIXME: Don't know if this works on Windows
		if (fnametmp)
			fnametmp++;
		else
			fnametmp = in_fname;
		loctrackname = xstrndup(fnametmp, strrchr(fnametmp,'.')-fnametmp);
	}
	
	trk_head = route_head_alloc();
	track_add_head(trk_head);
		
	trk_head->rte_name = xstrdup(loctrackname);
	
	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;

		wpt_tmp = waypt_new();

		rec = (struct record_track *) pdb_rec->data;

		wpt_tmp->longitude = -pdb_read_double(&rec->longitude)*360.0/(2.0*M_PI); 
		wpt_tmp->latitude = pdb_read_double(&rec->latitude)*360.0/(2.0*M_PI);
		// It's not the course, so leave it out for now
		// wpt_tmp->course = pdb_read_double(&rec->arc);
		wpt_tmp->altitude = be_read16(&rec->alt);
		
		if (internals) {
			// Parse the option as xcsv delimiter
			const char *inter = xcsv_get_char_from_constant_table(internals);
			char temp[256];
			snprintf(temp, sizeof(temp), "%.20f%s%.20f%s%.20f%s%.20f", pdb_read_double(&rec->distance), inter, 
				pdb_read_double(&rec->arc), inter, pdb_read_double(&rec->x), inter, pdb_read_double(&rec->y));
			wpt_tmp->notes = xstrdup(temp);
		}
		
		wpt_tmp->pdop = be_read16(&rec->pdop)/10.0;
		wpt_tmp->hdop = be_read16(&rec->hdop)/10.0;
		wpt_tmp->vdop = be_read16(&rec->vdop)/10.0;
		wpt_tmp->sat = rec->sat_tracked;
		switch (rec->fix_quality) {
			case cotofixNone:  wpt_tmp->fix = fix_none; break;
			case cotofixReserved:  wpt_tmp->fix = fix_unknown; break;
			case cotofix2D: wpt_tmp->fix = fix_2d; break;
			case cotofix3D: wpt_tmp->fix = fix_3d; break;
			case cotofixDGPS: wpt_tmp->fix = fix_dgps; break;
		}
		wpt_tmp->speed = be_read16(&rec->speed)/10.0;
		wpt_tmp->creation_time = be_read32(&rec->time) - 2082844800U;

		route_add_wpt(trk_head, wpt_tmp);

	} 
	
}

static void
coto_wpt_read(struct pdb *pdb)
{
	struct record_wpt *rec;
	struct pdb_record *pdb_rec;
	char *vdata;
	struct appinfo *app;
		
	app = (struct appinfo *) pdb->appinfo;
	
	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;
		int c=-1;
		
		wpt_tmp = waypt_new();

		rec = (struct record_wpt *) pdb_rec->data;
			
		// Find category
		/* I thought this would be the proper way. Leaving it in in case it becomes the proper one =)
		for(i=0;i<16;i++)
			if (app->ids[i] == pdb_rec->category) {c=i; break;}
		*/
		c = pdb_rec->category;
		
		wpt_tmp->longitude = -pdb_read_double(&rec->lon)*360.0/(2.0*M_PI); 
		wpt_tmp->latitude = pdb_read_double(&rec->lat)*360.0/(2.0*M_PI);
		
		wpt_tmp->shortname = xstrdup((char *) &rec->name);
		wpt_tmp->description = xstrdup((char *) &rec->name);
			
		if (c>0)
			wpt_tmp->icon_descr = xstrndup(app->categories[c], 16);
		else if (c<0) 
			wpt_tmp->icon_descr = xstrdup("Unknown");
		if (wpt_tmp->icon_descr)
			wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1; 
		
		if (pdb_rec->data_len>sizeof(*rec)) {
			vdata = (char *) pdb_rec->data + sizeof(*rec);
			wpt_tmp->notes = xstrdup(vdata);
		}
		
		waypt_add(wpt_tmp);
	}
}

static void
data_read(void)
{
	struct pdb *pdb;

	if (NULL == (pdb = pdb_Read(fileno(file_in)))) {
		fatal(MYNAME ": pdb_Read failed\n");
	}

	if ((pdb->creator != MYCREATOR) || ((pdb->type != MYTYPETRACK) && (pdb->type != MYTYPEWPT))) {
		warning("Creator %x Type %x Version %d\n", (int) pdb->creator, (int) pdb->type, (int) pdb->version);
		fatal(MYNAME ": Not a cotoGPS file.\n");
	}
	
        if (pdb->version > 0) {
	      fatal(MYNAME ": This file is from an unsupported newer version of cotoGPS.  It may be supported in a newer version of GPSBabel.\n");
	}
	
	if (pdb->type == MYTYPETRACK)
		coto_track_read(pdb);
	if (pdb->type == MYTYPEWPT)
		coto_wpt_read(pdb);

	free_pdb(pdb);
}

static void
coto_prepare_wpt_write(struct pdb *opdb)
{
	opdb->name[PDB_DBNAMELEN-1] = 0;
	opdb->attributes = PDB_ATTR_BACKUP;
	opdb->type = MYTYPEWPT;  
	opdb->creator = MYCREATOR; 
	opdb->version = 0;
	
	strncpy(opdb->name, "cotoGPS MarkerDB", PDB_DBNAMELEN);
	
	opdb->appinfo_len = APPINFO_PACKED_SIZE;
	opdb->appinfo = xcalloc(APPINFO_PACKED_SIZE,1);
	
	struct appinfo *ai = (struct appinfo *) opdb->appinfo;
	
	ai->renamedCategories = 31; // Don't ask me why...
	if (zerocat)
		strncpy(ai->categories[0], zerocat, 16);
	else
		strncpy(ai->categories[0], "Not Assigned", 16); // FIXME: Replace by default English Palm 'Not Assigned' category
	
}

static void
coto_wpt_write(const waypoint *wpt)
{
	struct record_wpt *rec;
	struct appinfo *ai = (struct appinfo *) opdb->appinfo;
	static int ct;
	struct pdb_record *opdb_rec;
	static void *mkshort_wr_handle;
	char *notes = NULL;
	char *vdata;
	int size = sizeof(*rec);
	ubyte cat = 0;
	int i;
	
	mkshort_wr_handle = mkshort_new_handle();
	setshort_length(mkshort_wr_handle, MAX_MARKER_NAME_LENGTH);
	setshort_whitespace_ok(mkshort_wr_handle, 1);
	
	if ((global_opts.synthesize_shortnames && wpt->description) || (!wpt->shortname)) {
		if (wpt->shortname)
			xfree(wpt->shortname);
		wpt->shortname = mkshort_from_wpt(mkshort_wr_handle, wpt);
	}
	if ((wpt->description) && ((strlen(wpt->description) > MAX_MARKER_NAME_LENGTH) || (strcmp(wpt->description, wpt->shortname)))) {
		if ((wpt->notes) && (strcmp(wpt->description, wpt->notes))) {
			size+=strlen(wpt->description)+strlen(wpt->notes)+9;
			notes = xcalloc(strlen(wpt->description)+strlen(wpt->notes)+9,1);
			sprintf(notes,"%s\nNotes:\n%s", wpt->description, wpt->notes);
		} else {
			size+=strlen(wpt->description)+1;
			notes = xstrdup(wpt->description);
		}
	} else if (wpt->notes) {
		size+=strlen(wpt->notes)+1;
		notes = xstrdup(wpt->notes);
	}
	rec = xcalloc(size,1);
	pdb_write_double(&rec->lon, -2.0*M_PI*wpt->longitude/360.0);
	pdb_write_double(&rec->lat, 2.0*M_PI*wpt->latitude/360.0);
	snprintf((char *) &rec->name, MAX_MARKER_NAME_LENGTH, "%s", wpt->shortname);
	
	if (notes) {
		vdata = (char *) rec + sizeof(*rec);
		strcpy(vdata, notes);
		xfree(notes);
	}
	
	if (wpt->icon_descr) {
		for(i=1;i<16;i++)
			if (!strncmp(wpt->icon_descr, ai->categories[i], 16)) {cat=i; break;}
		if (!cat) {
			// We have a new one
			if (ai->maxid<15) {
				i = ++ai->maxid;
				snprintf(ai->categories[i], 16, "%s", wpt->icon_descr);
				cat=ai->ids[i]=i;
			} else {
				// We're full!
				warning(MYNAME ": Categories full. Category '%s' written as %s.\n", wpt->icon_descr, zerocat?zerocat:"Not Assigned");
			}
		}
	}
	
	opdb_rec = new_Record (0, cat, ct++, size, (const ubyte *)rec);
	
	if (opdb_rec == NULL) {
		fatal(MYNAME ": libpdb couldn't create record\n");
	}

	if (pdb_AppendRecord(opdb, opdb_rec)) {
		fatal(MYNAME ": libpdb couldn't append record\n");
	}
	xfree(rec);
	
	mkshort_del_handle(mkshort_wr_handle);
	
}

static void
data_write(void)
{
	if (NULL == (opdb = new_pdb())) { 
		fatal (MYNAME ": new_pdb failed\n");
	}
	
	coto_prepare_wpt_write(opdb);
	waypt_disp_all(coto_wpt_write);
	track_disp_all(NULL, NULL, coto_wpt_write);
	route_disp_all(NULL, NULL, coto_wpt_write);
	
	pdb_Write(opdb, fileno(file_out));
	
}


ff_vecs_t coto_vecs = {
	ff_type_file,
	{ff_cap_read|ff_cap_write, ff_cap_read, ff_cap_none},
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL,
	coto_args,
};
