/*
    Read and write GeocachingPDB files.

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
#if PDBFMTS_ENABLED
#include "pdbfile.h"

#define MYNAME "GeocachingDB"
#define MYTYPE  0x44415441  	/* DATA */
#define MYCREATOR 0x42726174 	/* Brat */

#define MAXRECSZ 500 /* This is overkill as the records seem to be around 100
			 bytes a piece, but being conservative and dealing 
			 with realloc issues just doesn't seem worth it. */

typedef enum {
	RECTYPE_TEXT = 0,
	RECTYPE_DATE = 2
} gcdb_rectype;

struct dbfld {
	char	fldname[4];
	pdb_16  fldtype;
	pdb_16  fldlen;
};

struct dbrec {
	pdb_16  nflds;
	struct dbfld dbfld[1];
};

static pdbfile *file_in, *file_out;
static const char *out_fname;
static int ct;

static char *tbuf = NULL;
static char *tbufp = NULL;

static void
rd_init(const char *fname)
{
	file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
	pdb_close(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = pdb_create(fname, MYNAME);
	out_fname = fname;
	ct = 0;
}

static void
wr_deinit(void)
{
	pdb_close(file_out);
	if ( tbuf ) 
		xfree(tbuf);
}

static void
data_read(void)
{
	pdbrec_t *pdb_rec;

	if ((file_in->creator != MYCREATOR) || (file_in->type != MYTYPE)) {
		fatal(MYNAME ": Not a GeocachingDB file.\n");
	}

	for(pdb_rec = file_in->rec_list; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt = waypt_new();
		struct dbrec *rec = (struct dbrec *) pdb_rec->data;
		int nflds;
		int length;
		int type;
		int i;
		char *recdata;
		int lat_dir = 0;
		int lat_deg = 0;
		float lat_min = 0.0;
		int lon_dir = 0;
		int lon_deg = 0;
		float lon_min = 0.0;

		nflds = be_read16(&rec->nflds);
		recdata = (char *) &rec->dbfld[nflds];

		for (i = 0; i < nflds; i++) {
			length = (unsigned short) be_read16(&rec->dbfld[i].fldlen);
			type = be_read16(&rec->dbfld[i].fldtype);

			switch(type) {
			case RECTYPE_TEXT: /* Text */
				if (!strncmp("gcid", rec->dbfld[i].fldname,4)) {
					wpt->shortname = xstrdup(recdata);
				} else 
				if (!strncmp("gcna", rec->dbfld[i].fldname,4)) {
					wpt->description = xstrdup(recdata);
				} else 
				if (!strncmp("lat0", rec->dbfld[i].fldname,4)) {
					lat_dir = *recdata == 'N' ? 1 : -1;
				} else 
				if (!strncmp("lat1", rec->dbfld[i].fldname,4)) {
					lat_deg = atoi(recdata);
				} else 
				if (!strncmp("lat2", rec->dbfld[i].fldname,4)) {
					lat_min = atof(recdata);
				} 
				if (!strncmp("lon0", rec->dbfld[i].fldname,4)) {
					lon_dir = *recdata == 'E' ? 1 : -1;
				} else 
				if (!strncmp("lon1", rec->dbfld[i].fldname,4)) {
					lon_deg = atoi(recdata);
				} else 
				if (!strncmp("lon2", rec->dbfld[i].fldname,4)) {
					lon_min = atof(recdata);
				} else
				if (!strncmp("take", rec->dbfld[i].fldname,4)) {
					wpt->notes = xstrappend(wpt->notes,  " Took ");
					wpt->notes = xstrappend(wpt->notes,  recdata);
				} else
				if (!strncmp("left", rec->dbfld[i].fldname,4)) {
					wpt->notes = xstrappend(wpt->notes, " Left ");
					wpt->notes = xstrappend(wpt->notes,  recdata);
				} else
				if (!strncmp("diff", rec->dbfld[i].fldname,4)) {
					wpt->gc_data.diff = 10 * atof(recdata);
				} else
				if (!strncmp("terr", rec->dbfld[i].fldname,4)) {
					wpt->gc_data.terr = 10 * atof(recdata);
				} 
				break;
#if 0
				/* This really is the date of the find, 
				 * not the cache creation date.   
				 */
			case RECTYPE_DATE:
				if (!strncmp("date", rec->dbfld[i].fldname,4)) {
					time_t tm;
					tm = be_read32(recdata) * 24 * 3600;
					tm -= EPOCH_1904;
					wpt->creation_time = tm;
					warning( "date %d\n", tm);
				}
				break;
#endif
			}
			recdata += (length + 1) & (~1);
		}
		wpt->latitude = lat_dir * (lat_deg + lat_min/60);
		wpt->longitude = lon_dir * (lon_deg + lon_min/60);
		waypt_add(wpt);
	}
}


static int 
gcdb_add_to_rec(struct dbrec *rec, char *fldname, gcdb_rectype rectype, void *data)
{
	int length;
	static int rec_cnt;

	if (!tbuf) {
		tbuf = xcalloc(MAXRECSZ, 1);
		tbufp = tbuf;
	}
	
	if (fldname == NULL) {
		length = tbufp - tbuf;
		be_write16(&rec->nflds, rec_cnt);
		memcpy(&rec->dbfld[rec_cnt],tbuf, length);
		tbufp = tbuf;
		length += 4 + sizeof(struct dbfld) * rec_cnt;
		rec_cnt = 0;
		return length;
	}

	be_write16(&rec->dbfld[rec_cnt].fldtype,rectype); 
	strncpy(rec->dbfld[rec_cnt].fldname, fldname, 4);

	switch (rectype) {
	case RECTYPE_TEXT:
		length = 1 + strlen(data);
		be_write16(&rec->dbfld[rec_cnt].fldlen, length);
		strcpy(tbufp, data);
		tbufp += (length + 1) & (~1);
		break;
	case RECTYPE_DATE:
		length = 4;
		be_write16(&rec->dbfld[rec_cnt].fldlen, length);
		be_write32(tbufp, ((time_t)data - EPOCH_1904)/ (3600 * 24));
		tbufp += length;
		break;
	default:
		abort();
	}
	rec_cnt++;

	return length;
}

static void
gcdb_write_wpt(const waypoint *wpt)
{
	struct dbrec *rec;
	int reclen;
	char tbuf[100];

	/*
	 * We don't really know how many fields we'll have or how long
	 * they'll be so we'll just lazily create a huge place to hold them.
	 */
	rec = xcalloc(sizeof(*rec) + 500, 1);

	gcdb_add_to_rec(rec, "gcna", RECTYPE_TEXT, wpt->description);
	gcdb_add_to_rec(rec, "gcid", RECTYPE_TEXT, wpt->shortname);

	gcdb_add_to_rec(rec, "lat0", RECTYPE_TEXT, 
			wpt->latitude < 0 ? "S" : "N");

	sprintf(tbuf, "%d", (int) wpt->latitude);
	gcdb_add_to_rec(rec, "lat1", RECTYPE_TEXT, tbuf);

	sprintf(tbuf, "%f", 60 * (wpt->latitude - 
				(int) wpt->latitude));
	gcdb_add_to_rec(rec, "lat2", RECTYPE_TEXT, tbuf);


	gcdb_add_to_rec(rec, "lon0", RECTYPE_TEXT, 
			wpt->longitude < 0 ? "W" : "E");

	sprintf(tbuf, "%d", (int) wpt->longitude);
	gcdb_add_to_rec(rec, "lon1", RECTYPE_TEXT, tbuf);

	sprintf(tbuf, "%f", 60 * (wpt->longitude - 
				(int) wpt->longitude));
	gcdb_add_to_rec(rec, "lon2", RECTYPE_TEXT, tbuf);

	if (wpt->gc_data.diff) {
		sprintf(tbuf, "%f", wpt->gc_data.diff / 10.0);
		gcdb_add_to_rec(rec, "diff", RECTYPE_TEXT, tbuf);
	}

	if (wpt->gc_data.terr) {
		sprintf(tbuf, "%f", wpt->gc_data.terr / 10.0);
		gcdb_add_to_rec(rec, "terr", RECTYPE_TEXT, tbuf);
	}

#if 0
				/* This really is the date of the find, 
				 * not the cache creation date.   
				 */
	if (wpt->creation_time) {
		gcdb_add_to_rec(rec, "date", RECTYPE_DATE, (void *) wpt->creation_time);
	}
#endif

	/*
	 * We're done.  Build the record.
	 */
	reclen = gcdb_add_to_rec(rec, NULL, 0, NULL);

	pdb_write_rec(file_out, 0, 2, ct++, rec, reclen);
	xfree(rec);
}

static void
data_write(void)
{
	strncpy(file_out->name, out_fname, PDB_DBNAMELEN);
	strncpy(file_out->name, "GeocachingDB", PDB_DBNAMELEN);
	file_out->name[PDB_DBNAMELEN-1] = 0;
	file_out->attr = PDB_FLAG_BACKUP;
	file_out->ctime = file_out->mtime = current_time() + 2082844800U;
	file_out->type = MYTYPE;  /* CWpt */
	file_out->creator = MYCREATOR; /* cGPS */
	file_out->version = 1;

	waypt_disp_all(gcdb_write_wpt);
}


ff_vecs_t gcdb_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL,
	NULL,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
