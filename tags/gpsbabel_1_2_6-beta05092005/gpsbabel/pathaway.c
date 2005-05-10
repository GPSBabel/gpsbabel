/* 
	Support for PathAway Palm Database, 
	Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de

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

FILE *fd;

#define MYNAME "pathaway pdb"
#define MYTYPE 		0x55735472		/* ??? */
#define MYCREATOR	0x4b6e5772 		/* pathaway */

/*
 *
 */

static void
ppdb_rd_init(const char *fname)
{
	fd = xfopen(fname, "rb", MYNAME);
}

/*
 *
 */
 
static void
ppdb_rd_deinit(void)
{
	fclose(fd);
}

/*
 *
 */

static void
ppdb_read(void)
{
	struct pdb *pdb;
	struct pdb_record *pdb_rec;
	char *data;
	char latdir, longdir;
	int latdeg, longdeg;
	double latval, longval, altfeet;
	struct tm dttm;
	route_head *track_head;

	if (NULL == (pdb = pdb_Read(fileno(fd)))) {
		fatal(MYNAME ": pdb_Read failed\n");
	}

	if ((pdb->creator != MYCREATOR) || (pdb->type != MYTYPE)) {
		fatal(MYNAME ": Not a PathAway pdb file.\n");
	}
	
	if (pdb->version != 3) {
	       fatal(MYNAME ": This file is from an untested version of PathAway and is unsupported.\n");
        }

	track_head = route_head_alloc();
	track_add_head(track_head);

	track_head->rte_name = xstrdup(pdb->name);

	for (pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) 
	{
		waypoint *wpt_tmp = waypt_new();
		data = (char *) pdb_rec->data;
		memset(&dttm, 0, sizeof(dttm));
		sscanf(data,"%c%d %lf,%c%d %lf,%lf,%02d%02d%02d %02d%02d%04d,",
		    &latdir, &latdeg, &latval, &longdir, &longdeg, &longval, &altfeet, 
		    &dttm.tm_hour, &dttm.tm_min, &dttm.tm_sec,
		    &dttm.tm_mday, &dttm.tm_mon, &dttm.tm_year
		);
		
		dttm.tm_year -= 1900;
		dttm.tm_mon--;
		    
		wpt_tmp->creation_time = mktime(&dttm) + get_tz_offset();
		
		if (latdir == 'S') latdeg = -latdeg;
		if (longdir == 'W') longdir = -longdir;
		wpt_tmp->latitude = latdeg + (latval / 60.0);
		wpt_tmp->longitude = longdeg + (longval / 60.0 );
		wpt_tmp->altitude = altfeet / 3.2808;

		route_add_wpt(track_head, wpt_tmp);		
	} 
	free_pdb(pdb);
}

ff_vecs_t ppdb_vecs = {
	ff_type_file,
	{ ff_cap_none, ff_cap_read, ff_cap_none }, // We can only read track information
	ppdb_rd_init,	
	NULL,	
	ppdb_rd_deinit,
	NULL,
	ppdb_read,
	NULL,
	NULL, 
	NULL
};
