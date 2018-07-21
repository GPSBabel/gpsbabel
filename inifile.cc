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
#include <QtCore/QList>
#include <QtCore/QSharedPointer>
#include <cstdio>
#include <cstdlib>

#define MYNAME "inifile"

struct inifile_entry_t {
  QSharedPointer<char> key;
  QSharedPointer<char> val;
};

struct inifile_section_t {
  QSharedPointer<char> name;
  QList<inifile_entry_t> entries;
};

/* internal procedures */

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
      inifile->unicode = true;
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

      inifile->secs.append(inifile_section_t());
      sec = &inifile->secs.last();
      sec->name.reset(xstrdup(cin), xfree);
    } else {
      if (sec == nullptr) {
        fatal("%s: missing section header in '%s'.\n", myname,
              qPrintable(gbinipathname));
      }

      sec->entries.append(inifile_entry_t());
      inifile_entry_t* entry = &sec->entries.last();

      char* cx = strchr(cin, '=');
      if (cx != nullptr) {
        *cx = '\0';
        cin = lrtrim(cin);
      }

      entry->key.reset(xstrdup(cin), xfree);

      if (cx != nullptr) {
        cx = lrtrim(++cx);
        entry->val.reset(xstrdup(cx), xfree);
      } else {
        entry->val.reset(xstrdup(""), xfree);
      }
    }
  }
}

static char*
inifile_find_value(const inifile_t* inifile, const char* sec_name, const char* key)
{
  if (inifile == nullptr) {
    return nullptr;
  }

  for (const auto& sec : inifile->secs) {

    if (case_ignore_strcmp(sec.name.data(), sec_name) == 0) {

      for (const auto& entry : sec.entries) {

        if (case_ignore_strcmp(entry.key.data(), key) == 0) {
          return entry.val.data();
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

  auto* result = new inifile_t;
  inifile_load_file(fin, result, myname);

  gbfclose(fin);
  return result;
}

void
inifile_done(inifile_t* inifile)
{
  delete inifile;
  gbinipathname.clear();
}

bool
inifile_has_section(const inifile_t* inifile, const char* section)
{
  for (const auto& sec : inifile->secs) {
    if (case_ignore_strcmp(sec.name.data(), section) == 0) {
      return true;
    }
  }
  return false;
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

