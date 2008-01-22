/*
    Read and write QuoVadis files.

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

#include "quovadis.h"

#if PDBFMTS_ENABLED
static pdbfile *file_in, *file_out;

static gbuint8* rec_ptr = NULL;
static gbuint8* current_rec = NULL;
static int rec_index = 0;
static int ct;
static char *dbname = NULL;

static
arglist_t quovadis_args[] = {
	{"dbname", &dbname, "Database name", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
	ARG_TERMINATOR
};

static struct qv_icon_mapping mapping[] = {
    { gt_unknown, QUESTION_ICON },
    { gt_traditional, HOSPITAL_ICON },
    { gt_multi, DOCUMENT_ICON },
    { gt_virtual, CAMERA_ICON },
    { gt_letterbox, MAILBOX_ICON },
    { gt_event, MEETING_ICON },
    { gt_suprise, GIFTSHOP_ICON },
};

#define num_mappings (sizeof(mapping) / sizeof(struct qv_icon_mapping))

static geocache_type icon_to_wpt(int	icon_bitmap) {
    unsigned int i;

    for (i = 0; i < num_mappings; i++) {
	if (icon_bitmap == mapping[i].bitmap_id) {
	    return mapping[i].gc_type;
	}
    }
    return gt_unknown;
}

static int wpt_to_icon(geocache_type	type) {
    unsigned int i;

    for (i = 0; i < num_mappings; i++) {
	if (type == mapping[i].gc_type) {
	    return mapping[i].bitmap_id;
	}
    }
    return QUESTION_ICON;
}

static void
rd_init(const char *fname)
{
	file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
	pdb_close(file_in);
	if ( dbname ) {
	    xfree(dbname);
	    dbname = NULL;
	}
}

static void
wr_init(const char *fname)
{
	file_out = pdb_create(fname, MYNAME);
	ct = 0;
}

static void
wr_deinit(void)
{
	pdb_close(file_out);
	if ( dbname ) {
	    xfree(dbname);
	    dbname = NULL;
	}
}

static void
data_read(void)
{
    struct record *rec;
    pdbrec_t *pdb_rec;
    int	i;

    if ((file_in->creator != MYCREATOR) || (file_in->type != MYTYPE)) {
	fatal(MYNAME ": Not a QuoVadis file.\n");
    }
	
    /* Ignore the first record, it contains one zero byte */
    for(pdb_rec = file_in->rec_list->next; pdb_rec; pdb_rec = pdb_rec->next) {
	int num_recs = pdb_rec->size / sizeof(struct record);
	for (i = 0; i < num_recs; i++) {
	    waypoint *wpt_tmp;

	    wpt_tmp = waypt_new();

	    rec = (struct record *)
		&(pdb_rec->data[i * sizeof(struct record)]);

	    wpt_tmp->longitude =
		(be_read32(&rec->longitude) / 1000000.0) - 180.0; 
	    wpt_tmp->latitude =
		90.0 - (be_read32(&rec->latitude) / 1000000.0);
	    wpt_tmp->shortname = xstrdup(rec->name);

	    wpt_tmp->gc_data.type =
		icon_to_wpt(be_read16(&rec->icon_bitmap));

	    waypt_add(wpt_tmp);
	}
    } 
}


static void
quovadis_writewpt(waypoint *wpt)
{
    struct record *rec;
    int	i;

    if (current_rec == NULL) {
	gbuint8 dummy = 0;
	
	pdb_write_rec(file_out, 0, 0, ct++, &dummy, 1);

	current_rec = (gbuint8 *) xcalloc(MAXCHUNKSIZE, 1);
	rec_index = 0;
	rec_ptr = current_rec;
    }

    rec = (struct record *) xcalloc(sizeof(*rec),1);

    be_write32(&rec->longitude, (unsigned int) ((wpt->longitude +
				 180.0) * 1000000.0));
    be_write32(&rec->latitude, (unsigned int) ((90.0 - wpt->latitude) * 1000000.0));
    if ( wpt->shortname ) {
	strncpy(rec->name, wpt->shortname, 32 );
	rec->name[31] = '\0';
    }
    else {
	rec->name[0] = '\0';
    }
    be_write16(&rec->icon_bitmap, wpt_to_icon(wpt->gc_data.type));
    be_write32(&rec->note_id, 0);
    rec->name_scale = DEFAULT_NAME_SCALE;
    rec->icon_scale = DEFAULT_ICON_SCALE;
    for (i = 0; i < 7; i++) {
	rec->reserved[i] = 0;
    }

    memcpy(rec_ptr, rec, sizeof(*rec));
    rec_ptr += sizeof(*rec);
    rec_index += 1;
    xfree(rec);

    if (rec_index == MAXRECORDS) {
	fatal(MYNAME ": cannot store more than %lu records at this time.\n",
	      (unsigned long) MAXRECORDS);
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
	const struct hdr *wa = (const struct hdr *) a;
	const struct hdr *wb = (const struct hdr *) b;

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

	if ( dbname ) {
	    strncpy( file_out->name, dbname, PDB_DBNAMELEN );
	}
	else {
	    strncpy(file_out->name, "QuoVadisMarkerDB", PDB_DBNAMELEN);
	}
	file_out->name[PDB_DBNAMELEN-1] = 0;
	file_out->attr = PDB_FLAG_BACKUP;
	file_out->ctime = file_out->mtime = current_time() + 2082844800U;
	file_out->type = MYTYPE;  /* CWpt */
	file_out->creator = MYCREATOR; /* cGPS */
	file_out->version = 1;

	/*
	 * All this is to sort by waypoint names before going to QuoVadis.
	 * Turns out plain old strcmp will do the trick...
	 */

	htable = (struct hdr *) xmalloc(ct * sizeof(*htable));
	bh = htable;

        QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
                waypointp = (waypoint *) elem;
		bh->wpt = waypointp;
		bh->wpt_name = waypointp->shortname;
		bh ++;
	}
	qsort(htable, ct, sizeof(*bh), compare);

	for (i=0;i<ct;i++) {
		quovadis_writewpt(htable[i].wpt);
	}

	if (rec_index != 0) {
	    pdb_write_rec(file_out, 0, 0, ct++, current_rec, rec_index * sizeof(struct record));
	}
	xfree(current_rec);

	xfree(htable);
}


ff_vecs_t quovadis_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL, 
	quovadis_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
