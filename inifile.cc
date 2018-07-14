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

#include "defs.h"
#include "inifile.h"
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <cstdio>
#include <cstdlib>

#define MYNAME "inifile"

typedef struct inifile_entry_s {
  queue Q;
  char* key;
  char* val;
} inifile_entry_t;

typedef struct inifile_section_s {
  queue Q;
  char* name;
  int ientries;
  queue entries;
} inifile_section_t;

/* internal procedures */

#define START_BUFSIZE 257
#define DELTA_BUFSIZE 128

#define GPSBABEL_INIFILE "gpsbabel.ini"
#define GPSBABEL_SUBDIR ".gpsbabel"

/* Remember the filename we used so we can include it in errors. */
static QString gbinipathname;

static QString
find_gpsbabel_inifile(const QString& path)  /* can be empty or NULL */
{
  if (path.isNull()) {
    return QString();
  }
  QString inipath(QDir(path).filePath(GPSBABEL_INIFILE));
  return QFile(inipath).open(QIODevice::ReadOnly) ? inipath : QString();
}

static gbfile*
open_gpsbabel_inifile()
{
  gbfile* res = nullptr;

  QString envstr = ugetenv("GPSBABELINI");
  if (!envstr.isNull()) {
    if (QFile(envstr).open(QIODevice::ReadOnly)) {
      return gbfopen(envstr, "r", "GPSBabel");
    }
    warning("WARNING: GPSBabel-inifile, defined in environment, NOT found!\n");
    return nullptr;
  }
  QString name = find_gpsbabel_inifile("");  // Check in current directory first.
  if (name.isNull()) {
#ifdef __WIN32__
    // Use &&'s early-out behaviour to try successive file locations: first
    // %APPDATA%, then %WINDIR%, then %SYSTEMROOT%.
    (name = find_gpsbabel_inifile(ugetenv("APPDATA"))).isNull()
        && (name = find_gpsbabel_inifile(ugetenv("WINDIR"))).isNull()
        && (name = find_gpsbabel_inifile(ugetenv("SYSTEMROOT"))).isNull();
#else
    // Use &&'s early-out behaviour to try successive file locations: first
    // ~/.gpsbabel, then /usr/local/etc, then /etc.
    (name = find_gpsbabel_inifile(QDir::home().filePath(GPSBABEL_SUBDIR))).
            isNull()
        && (name = find_gpsbabel_inifile("/usr/local/etc")).isNull()
        && (name = find_gpsbabel_inifile("/etc")).isNull();
#endif
  }
  if (!name.isNull()) {
    res = gbfopen(name, "r", "GPSBabel");
    gbinipathname = name;
  }
  return res;
}

static void
inifile_load_file(gbfile* fin, inifile_t* inifile, const char* myname)
{
  char* buf;
  inifile_section_t* sec = nullptr;
  int line = 0;

  while ((buf = gbfgetstr(fin))) {
    char* cin = lrtrim(buf);

    if ((line++ == 0) && fin->unicode) {
      inifile->unicode = 1;
    }

    if (*cin == '\0') {
      continue;  /* skip empty lines */
    }
    if ((*cin == '#') || (*cin == ';')) {
      continue;  /* skip comments */
    }

    if (*cin == '[') {

      char* cend = strchr(++cin, ']');

      if (cend != nullptr) {
        *cend = '\0';
        cin = lrtrim(cin);
      }
      if ((*cin == '\0') || (cend == nullptr)) {
        fatal("%s: invalid section header '%s' in '%s'.\n", myname, cin,
              qPrintable(gbinipathname));
      }

      sec = (inifile_section_t*) xcalloc(1, sizeof(*sec));

      sec->name = xstrdup(cin);
      QUEUE_INIT(&sec->entries);
      ENQUEUE_TAIL(&inifile->secs, &sec->Q);
      inifile->isecs++;
    } else {
      if (sec == nullptr) {
        fatal("%s: missing section header in '%s'.\n", myname,
              qPrintable(gbinipathname));
      }

      inifile_entry_t* entry = (inifile_entry_t*) xcalloc(1, sizeof(*entry));
      ENQUEUE_TAIL(&sec->entries, &entry->Q);
      sec->ientries++;

      char* cx = strchr(cin, '=');
      if (cx != nullptr) {
        *cx = '\0';
        cin = lrtrim(cin);
      }

      entry->key = xstrdup(cin);

      if (cx != nullptr) {
        cx = lrtrim(++cx);
        entry->val = xstrdup(cx);
      } else {
        entry->val = xstrdup("");
      }
    }
  }
}

static char*
inifile_find_value(const inifile_t* inifile, const char* sec_name, const char* key)
{
  queue* elem, *tmp;

  if (inifile == nullptr) {
    return nullptr;
  }

  QUEUE_FOR_EACH(&inifile->secs, elem, tmp) {
    inifile_section_t* sec = reinterpret_cast<inifile_section_t *>(elem);

    if (case_ignore_strcmp(sec->name, sec_name) == 0) {
      queue* elem, *tmp;

      QUEUE_FOR_EACH(&sec->entries, elem, tmp) {
        inifile_entry_t* entry = reinterpret_cast<inifile_entry_t *>(elem);

        if (case_ignore_strcmp(entry->key, key) == 0) {
          return entry->val;
        }
      }
    }
  }
  return nullptr;
}

/* public procedures */

/*
	inifile_init:
	  reads inifile filename into memory
	  myname represents the calling module

	  filename == NULL: try to open global gpsbabel.ini
 */
inifile_t*
inifile_init(const QString& filename, const char* myname)
{
  gbfile* fin = nullptr;

  if (filename.isEmpty()) {
    fin = open_gpsbabel_inifile();
    if (fin == nullptr) {
      return nullptr;
    }
  } else {
    fin = gbfopen(filename, "rb", myname);
  }

  inifile_t* result = (inifile_t*) xcalloc(1, sizeof(*result));
  QUEUE_INIT(&result->secs);
  inifile_load_file(fin, result, myname);

  gbfclose(fin);
  return result;
}

void
inifile_done(inifile_t* inifile)
{
  if (inifile == nullptr) {
    return;
  }

  if (inifile->isecs > 0) {
    queue* elem, *tmp;

    QUEUE_FOR_EACH(&inifile->secs, elem, tmp) {
      inifile_section_t* sec = reinterpret_cast<inifile_section_t *>(elem);

      if (sec->ientries > 0) {
        queue* elem, *tmp;

        QUEUE_FOR_EACH(&sec->entries, elem, tmp) {
          inifile_entry_t* entry = reinterpret_cast<inifile_entry_t *>(elem);

          if (entry->key) {
            xfree(entry->key);
          }
          if (entry->val) {
            xfree(entry->val);
          }
          dequeue(elem);
          xfree(entry);
        }
      }
      dequeue(elem);
      if (sec->name) {
        xfree(sec->name);
      }
      xfree(sec);
    }
    xfree(inifile);
  }
  gbinipathname.clear();
}

int
inifile_has_section(const inifile_t* inifile, const char* section)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&inifile->secs, elem, tmp) {
    inifile_section_t* sec = reinterpret_cast<inifile_section_t *>(elem);
    if (case_ignore_strcmp(sec->name, section) == 0) {
      return 1;
    }
  }
  return 0;
}

/*
     inifile_readstr:
       returns NULL if not found, otherwise a pointer to the value of key ...
       all key values are valid entities until "inifile_done"
 */

char*
inifile_readstr(const inifile_t* inifile, const char* section, const char* key)
{
  return inifile_find_value(inifile, section, key);
}

/*
     inifile_readint:
       on success the value is stored into "*value" and "inifile_readint" returns 1,
       otherwise inifile_readint returns 0
 */

int
inifile_readint(const inifile_t* inifile, const char* section, const char* key, int* value)
{
  char* str = inifile_find_value(inifile, section, key);

  if (str == nullptr) {
    return 0;
  }

  if (value != nullptr) {
    *value = atoi(str);
  }
  return 1;
}

/*
     inifile_readint_def:
       if found inifile_readint_def returns value of key, otherwise a default value "def"
 */

int
inifile_readint_def(const inifile_t* inifile, const char* section, const char* key, const int def)
{
  int result;

  if (inifile_readint(inifile, section, key, &result) == 0) {
    return def;
  } else {
    return result;
  }
}

