/*
    Library for inifile like data files.
    
    Copyright (C) 2006 Olaf Klein, o.b.klein@t-online.de

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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "inifile.h"

#define MYNAME "inifile"

typedef struct inifile_entry_s
{
	queue Q;
	char *key;
	char *val;
} inifile_entry_t;

typedef struct inifile_section_s
{
	queue Q;
	char *name;
	int ientries;
	queue entries;
} inifile_section_t;

/* internal procedures */

#define START_BUFSIZE 257
#define DELTA_BUFSIZE 128

static void
inifile_load_file(FILE *fin, inifile_t *inifile, const char *myname)
{
	char *buf;
	size_t bufsize = START_BUFSIZE;
	inifile_section_t *sec = NULL;

	buf = xmalloc(bufsize);
	
	while ((fgets(buf, bufsize, fin)))
	{
		char *cin;
	
		while (strchr(buf, '\n') == NULL)
		{
			buf = xrealloc(buf, bufsize + DELTA_BUFSIZE);
			cin = fgets(buf + bufsize - 1, DELTA_BUFSIZE + 1, fin);
			bufsize+=DELTA_BUFSIZE;
			if (cin == NULL) break;
		}
		
		cin = lrtrim(buf);
		if (*cin == '\0') continue;
		
		if (*cin == '[')
		{
			
			char *cend = strchr(++cin, ']');

			if (cend != NULL)
			{
				*cend = '\0';
				cin = lrtrim(cin);
			}
			if ((*cin == '\0') || (cend == NULL))
				fatal("%s: invalid section header!\n", myname);
				
			sec = xcalloc(1, sizeof(*sec));
			
			sec->name = xstrdup(cin);
			QUEUE_INIT(&sec->entries);
			ENQUEUE_TAIL(&inifile->secs, &sec->Q);
			inifile->isecs++;
		}
		else
		{
			char *cx;
			inifile_entry_t *entry;
			
			if (sec == NULL)
				fatal("%s: missing section header!\n", myname);
			
			entry = xcalloc(1, sizeof(*entry));
			ENQUEUE_TAIL(&sec->entries, &entry->Q);
			sec->ientries++;
			
			cx = strchr(cin, '=');
			if (cx != NULL)
			{
				*cx = '\0';
				cin = lrtrim(cin);
			}
			
			entry->key = xstrdup(cin);
			
			if (cx != NULL)
			{
				cx = lrtrim(++cx);
				entry->val = xstrdup(cx);
			}
			else
				entry->val = xstrdup("");
		}
	}
	xfree(buf);
}

static char *
inifile_find_value(const inifile_t *inifile, const char *sec_name, const char *key)
{
	queue *elem, *tmp;
	
	QUEUE_FOR_EACH(&inifile->secs, elem, tmp)
	{
		inifile_section_t *sec = (inifile_section_t *) elem;
		
		if (case_ignore_strcmp(sec->name, sec_name) == 0)
		{
			queue *elem, *tmp;
			
			QUEUE_FOR_EACH(&sec->entries, elem, tmp)
			{
				inifile_entry_t *entry = (inifile_entry_t *) elem;
			
				if (case_ignore_strcmp(entry->key, key) == 0)
				{
					return entry->val;
				}
			}
		}
	}
	return NULL;
}

/* public procedures */

/*
	inifile_init:
	  reads inifile filename into memory
	  myname represents the calling module
 */
inifile_t *
inifile_init(const char *filename, const char *myname)
{
	inifile_t *result;
	FILE *fin;
	
	fin = xfopen(filename, "r", myname);
	
	result = xcalloc(1, sizeof(*result));
	QUEUE_INIT(&result->secs);
	inifile_load_file(fin, result, myname);
	
	fclose(fin);
	return result;
}

void
inifile_done(inifile_t *inifile)
{
	if (inifile == NULL) return;
	
	if (inifile->isecs > 0) 
	{
		queue *elem, *tmp;
		
		QUEUE_FOR_EACH(&inifile->secs, elem, tmp) {
			inifile_section_t *sec = (inifile_section_t *) elem;
			
			if (sec->ientries > 0) {
				queue *elem, *tmp;
				
				QUEUE_FOR_EACH(&sec->entries, elem, tmp) {
					inifile_entry_t *entry = (inifile_entry_t *) elem;
					
					if (entry->key) xfree(entry->key);
					if (entry->val) xfree(entry->val);
					dequeue(elem);
					xfree(entry);
				}
			}
			dequeue(elem);
			if (sec->name) xfree(sec->name);
			xfree(sec);
		}
		xfree(inifile);
	}
}

int 
inifile_has_section(const inifile_t *inifile, const char *section)
{
	queue *elem, *tmp;
	
	QUEUE_FOR_EACH(&inifile->secs, elem, tmp)
	{
		inifile_section_t *sec = (inifile_section_t *) elem;
		if (case_ignore_strcmp(sec->name, section) == 0)
			return 1;
	}
	return 0;
}

/* 
     inifile_readstr:
       returns NULL if not found, otherwise a pointer to the value of key ...
       all key values are valid entities until "inifile_done" 
 */

char *
inifile_readstr(const inifile_t *inifile, const char *section, const char *key)
{
	return inifile_find_value(inifile, section, key);
}

/* 
     inifile_readint:
       on success the value is stored into "*value" and "inifile_readint" returns 1,
       otherwise inifile_readint returns 0
 */
 
int 
inifile_readint(const inifile_t *inifile, const char *section, const char *key, int *value)
{
	char *str;
	
	str = inifile_find_value(inifile, section, key);
	
	if (str == NULL) {
		return 0;
	}
	
	if (value != NULL) {
		*value = atoi(str);
	}
	return 1;
}

/*
     inifile_readint_def:
       if found inifile_readint_def returns value of key, otherwise a default value "def"
 */

int 
inifile_readint_def(const inifile_t *inifile, const char *section, const char *key, const int def)
{
	int result;
	
	if (inifile_readint(inifile, section, key, &result) == 0) {
		return def;
	}
	else {
		return result;
	}
}
