/*
    Read and write GPSPilot Tracker files.

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

#define MYNAME "GPSPilot"
#define MYTYPE 0x706f696e  	/* poin */
#define MYCREATOR 0x47704c69 	/* GpLi */

struct record {
	pdb_32 longitude; 	/* Big endian, long * 3.6e6 */
	pdb_32 latitude; 	/* similarly */
	pdb_16 elevation; 	/* meters */
	pdb_16 magvar; 		/* magnetic variation in degrees, neg = west */
};

static FILE *file_in;
static FILE *file_out;
static const char *out_fname;
struct pdb *opdb;
struct pdb_record *opdb_rec;

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
		fatal(MYNAME ": Not a gpspilot file.\n");
	}

	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;
		char *vdata;

		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

		rec = (struct record *) pdb_rec->data;
		wpt_tmp->position.longitude.degrees = be_read32(&rec->longitude) / 3.6e6; 
		wpt_tmp->position.latitude.degrees = be_read32(&rec->latitude) / 3.6e6; 
		wpt_tmp->position.altitude.altitude_meters = be_read16(&rec->elevation) / 100.0;
	
		vdata = (char *) pdb_rec->data + sizeof(*rec);

		/*
		 * This maping is a bit contrived.   
		 * Name is up to 36. ID is up to 9.
		 * Since 'ID' maps more clearly to "shortname" (and this
		 * more likely to be resemble  a wayoint name in another
		 * receiver) we use that for shortname and use 'name' as
		 * our description.
		 */
		wpt_tmp->description = xstrdup(vdata);
		vdata = vdata + strlen(vdata) + 1;

		wpt_tmp->shortname = xstrdup(vdata);
		
		waypt_add(wpt_tmp);

	} 
	free_pdb(pdb);
}


static void
gpspilot_writewpt(waypoint *wpt)
{
	struct record *rec;
	static int ct = 0;
	char *vdata;

	rec = xcalloc(sizeof(*rec)+46,1);
	
	be_write32(&rec->longitude, si_round(wpt->position.longitude.degrees * 3.6e6));
	be_write32(&rec->latitude, si_round(wpt->position.latitude.degrees * 3.6e6));
	be_write16(&rec->elevation, si_round(wpt->position.altitude.altitude_meters));
	be_write16(&rec->magvar, 0 );
	
	vdata = (char *)rec + sizeof(*rec);
	if ( wpt->description ) {
                strncpy( vdata, wpt->description, 36 );
                vdata[35] = '\0';
        }
        else {
                vdata[0] ='\0';
        }
        vdata += strlen( vdata ) + 1;
	if ( wpt->shortname ) {
                strncpy( vdata, wpt->shortname, 9 );
                vdata[8] = '\0';
        }
        else {
                vdata[0] ='\0';
        }
        vdata += strlen( vdata ) + 1;
	vdata[0] = '\0'; /* long description - currently unsupported */
	vdata++;

        opdb_rec = new_Record (0, 2, ct++, vdata-(char *)rec, (const ubyte *)rec);	       

	if (opdb_rec == NULL) {
		fatal(MYNAME ": libpdb couldn't create record\n");
	}

	if (pdb_AppendRecord(opdb, opdb_rec)) {
		fatal(MYNAME ": libpdb couldn't append record\n");
	}
}

static void
data_write(void)
{
        extern queue waypt_head;
	queue *elem, *tmp;

	if (NULL == (opdb = new_pdb())) { 
		fatal (MYNAME ": new_pdb failed\n");
	}

	strncpy(opdb->name, out_fname, PDB_DBNAMELEN);
	opdb->name[PDB_DBNAMELEN-1] = 0;
	opdb->attributes = PDB_ATTR_BACKUP;
	opdb->ctime = opdb->mtime = time(NULL) + 2082844800U;
	opdb->type = MYTYPE;
	opdb->creator = MYCREATOR; 
	opdb->version = 0;
        QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		gpspilot_writewpt((waypoint *)elem);
	}
	
	pdb_Write(opdb, fileno(file_out));
}


ff_vecs_t gpspilot_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
