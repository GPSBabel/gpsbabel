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

#include "defs.h"              // for fatal, ugetenv, warning
#include "inifile.h"
#include "src/core/file.h"     // for File
#include <QtCore/QByteArray>   // for QByteArray
#include <QtCore/QChar>        // for operator==, QChar
#include <QtCore/QDir>         // for QDir
#include <QtCore/QFile>        // for QFile
#include <QtCore/QFileInfo>    // for QFileInfo
#include <QtCore/QHash>        // for QHash
#include <QtCore/QIODevice>    // for QIODevice::ReadOnly, QIODevice
#include <QtCore/QTextStream>  // for QTextStream
#include <QtCore/Qt>           // for CaseInsensitive
#include <QtCore/QtGlobal>     // for qPrintable

#define MYNAME "inifile"

struct InifileSection {
  QString name;
  QHash<QString, QString> entries;

  InifileSection() = default;
  explicit InifileSection(QString nm) : name{nm} {}
};

/* internal procedures */

#define GPSBABEL_INIFILE "gpsbabel.ini"
#define GPSBABEL_SUBDIR ".gpsbabel"


static QString
find_gpsbabel_inifile(const QString& path)  /* can be empty or NULL */
{
  if (path.isNull()) {
    return QString();
  }
  QString inipath(QDir(path).filePath(GPSBABEL_INIFILE));
  return QFile(inipath).open(QIODevice::ReadOnly) ? inipath : QString();
}

static QString
open_gpsbabel_inifile()
{
  QString res;

  QString envstr = ugetenv("GPSBABELINI");
  if (!envstr.isNull()) {
    if (QFile(envstr).open(QIODevice::ReadOnly)) {
      return envstr;
    }
    warning("WARNING: GPSBabel-inifile, defined in environment, NOT found!\n");
    return res;
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
    res = name;
  }
  return res;
}

static void
inifile_load_file(QTextStream* stream, inifile_t* inifile, const char* myname)
{
  QString buf;
  InifileSection* sec = nullptr;

  while (!(buf = stream->readLine()).isNull()) {
    buf = buf.trimmed();

    if (buf.isEmpty()) {
      continue;  /* skip empty lines */
    }
    if ((buf.at(0) == '#') || (buf.at(0) == ';')) {
      continue;  /* skip comments */
    }

    if (buf.at(0) == '[') {
      QString secname;
      if (buf.contains(']')) {
        secname = buf.mid(1, buf.indexOf(']') - 1).trimmed();
      }
      if (secname.isEmpty()) {
        fatal("%s: invalid section header '%s' in '%s'.\n", myname, qPrintable(secname),
              qPrintable(inifile->source));
      }

      inifile->secs.append(InifileSection(secname));
      sec = &inifile->secs.last();
    } else {
      if (sec == nullptr) {
        fatal("%s: missing section header in '%s'.\n", myname,
              qPrintable(inifile->source));
      }

      // Store key in lower case to implement CaseInsensitive matching.
      QString key = buf.section('=', 0, 0).trimmed().toLower();
      // Take some care so the return from inifile_find_value can
      // be used to distinguish between a key that isn't found
      // and a found key without a value, i.e. force value
      // to be non-null but possibly empty.
      QString value = buf.section('=', 1).append("").trimmed();
      sec->entries.insert(key, value);
    }
  }
}

static const QString
inifile_find_value(const inifile_t* inifile, const QString& sec_name, const QString& key)
{
  if (inifile == nullptr) {
    return QString();
  }

  for (const auto& sec : inifile->secs) {

    if (sec.name.compare(sec_name, Qt::CaseInsensitive) == 0) {
      // CaseInsensitive matching implemented by forcing key to lower case.
      return sec.entries.value(key.toLower());
    }
  }
  return QString();
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
  QString name;

  if (filename.isEmpty()) {
    name = open_gpsbabel_inifile();
    if (name.isEmpty()) {
      return nullptr;
    }
  } else {
    name = filename;
  }

  gpsbabel::File file(name);
  file.open(QFile::ReadOnly);
  QTextStream stream(&file);
  stream.setCodec("UTF-8");
  stream.setAutoDetectUnicode(true);

  auto* result = new inifile_t;
  QFileInfo fileinfo(file);
  result->source = fileinfo.absoluteFilePath();
  inifile_load_file(&stream, result, myname);

  file.close();
  return result;
}

void
inifile_done(inifile_t* inifile)
{
  delete inifile;
}

bool
inifile_has_section(const inifile_t* inifile, const char* section)
{
  for (const auto& sec : inifile->secs) {
    if (sec.name.compare(section, Qt::CaseInsensitive) == 0) {
      return true;
    }
  }
  return false;
}

/*
     inifile_readstr:
       returns a null QString if not found, otherwise a non-null but possibly
       empty Qstring with the value of key ...
 */

QString
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
  const QString str = inifile_find_value(inifile, section, key);

  if (str.isNull()) {
    return 0;
  }

  if (value != nullptr) {
    *value = str.toInt();
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

