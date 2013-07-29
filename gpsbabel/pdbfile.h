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
  uint32_t  offs;
  int32_t  size;
  uint32_t id;
  uint8_t  category;
  uint8_t  flags;
  char*    data;
  struct pdbrec_s* next;
} pdbrec_t;

typedef struct {
  gbfile* file;
  char mode;		/* file-mode: 1 = read / 2 = write */
  char name[PDB_DBNAMELEN + 1];	/* database name */
  uint16_t attr;		/* attributes */
  uint16_t version;	/* version */
  time_t ctime;		/* creation time */
  time_t mtime;		/* modification time */
  time_t btime;		/* backup time */
  uint32_t revision;
  int32_t appinfo_offs;	/* offset to application info */
  int32_t index_offs;	/* offset to sort-index info */
  uint32_t creator;
  uint32_t type;
  uint32_t uid;
  uint16_t rec_ct;
  struct pdbrec_s* rec_list;
  struct pdbrec_s* rec_curr;
  void* appinfo;
  int appinfo_len;
} pdbfile;


pdbfile* pdb_open(const char* filename, const char* module);
pdbfile* pdb_create(const char* filename, const char* module);
void pdb_close(pdbfile* file);
int pdb_eof(pdbfile* fin);
void pdb_rewind(pdbfile* fin);
int pdb_read_rec(pdbfile* fin, uint8_t* flags, uint8_t* category, uint32_t* rec_id, void** data);
int pdb_read_rec_by_id(pdbfile* fin, const uint32_t rec_id, uint8_t* flags, uint8_t* category, void** data);
void pdb_write_rec(pdbfile* fout, const uint8_t flags, const uint8_t category, const uint32_t rec_id, const void* data, const uint32_t size);

#endif
#endif
