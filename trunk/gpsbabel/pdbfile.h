/*

    Minimum support for Palm/OS database files
    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

    Written after study the Coldsync project
    
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

#ifndef PDBFILE_H
#define PDBFILE_H

#include "defs.h"

#if PDBFMTS_ENABLED

#include "gbfile.h"
#include "gbtypes.h"

#include <time.h>

#define PDB_DBNAMELEN		32
#define PDB_FLAG_RESOURCE	0x0001
#define PDB_FLAG_BACKUP		0x0008
#define EPOCH_1904		2082844800L

typedef struct pdbrec_s {
	gbuint32  offs;
	gbuint32  size;
	gbuint32 id;
	gbuint8  category;
	gbuint8  flags;
	char    *data;
	struct pdbrec_s *next;
} pdbrec_t;

typedef struct {
	gbfile *file;
	char mode;		/* file-mode: 1 = read / 2 = write */
	char name[PDB_DBNAMELEN + 1];	/* database name */
	gbuint16 attr;		/* attributes */
	gbuint16 version;	/* version */
	time_t ctime;		/* creation time */
	time_t mtime;		/* modification time */
	time_t btime;		/* backup time */
	gbuint32 revision;
	gbuint32 appinfo_offs;	/* offset to application info */
	gbuint32 index_offs;	/* offset to sort-index info */
	gbuint32 creator;
	gbuint32 type;
	gbuint32 uid;
	gbuint16 rec_ct;
	struct pdbrec_s *rec_list;
	struct pdbrec_s *rec_curr;
	void *appinfo;
	int appinfo_len;
} pdbfile;


pdbfile *pdb_open(const char *filename, const char *module);
pdbfile *pdb_create(const char *filename, const char *module);
void pdb_close(pdbfile *file);
int pdb_eof(pdbfile *fin);
void pdb_rewind(pdbfile *fin);
int pdb_read_rec(pdbfile *fin, gbuint8 *flags, gbuint8 *category, gbuint32 *rec_id, void **data);
int pdb_read_rec_by_id(pdbfile *fin, const gbuint32 rec_id, gbuint8 *flags, gbuint8 *category, void **data);
void pdb_write_rec(pdbfile *fout, const gbuint8 flags, const gbuint8 category, const gbuint32 rec_id, const void *data, const gbuint32 size);

#endif
#endif
