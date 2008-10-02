/*
    Library for inifile like data files.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef HAVE_INIFILE_H
#define HAVE_INIFILE_H

#include "defs.h"

typedef struct inifile_s
{
	int isecs;			/* number of sections */
	queue secs;			/* sections */
} inifile_t;

/*
	inifile_init:
	  reads inifile filename into memory
	  myname represents the calling module
 */
inifile_t * inifile_init(const char *filename, const char *myname);
void inifile_done(inifile_t *inifile);

int inifile_has_section(const inifile_t *inifile, const char *section);

/* 
     inifile_readstr:
       returns NULL if not found, otherwise a pointer to the value of key ...
       all key values are valid entities until "inifile_done" 
 */
char *inifile_readstr(const inifile_t *inifile, const char *section, const char *key);

/* 
     inifile_readint:
       on success the value is stored into "*value" and "inifile_readint" returns 1,
       otherwise inifile_readint returns 0
 */
int inifile_readint(const inifile_t *inifile, const char *section, const char *key, int *value);

/*
     inifile_readint_def:
       if found inifile_readint_def returns value of key, otherwise a default value "def"
 */
int inifile_readint_def(const inifile_t *inifile, const char *section, const char *key, const int def);

#endif
