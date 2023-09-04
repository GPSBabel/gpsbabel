/*
    Generate unique short names.

    Copyright (C) 2003-2006, 2023 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef MKSHORT_H_INCLUDED_
#define MKSHORT_H_INCLUDED_

#include <QByteArray>  // for QByteArray
#include <QHash>       // for QHash, QHash<>::iterator, qHash, QHash<>::size_type
#include <QString>     // for QString
#include <QVector>     // for QVector
#include <Qt>          // for CaseInsensitive

#include "defs.h"


class MakeShort
{
public:

  /* Member Functions */

  void set_length(int l);
  void set_whitespace_ok(bool ok);
  void set_repeating_whitespace_ok(bool ok);
  void set_defname(const char* s);
  void set_badchars(const char* s);
  void set_goodchars(const char* s);
  void set_mustupper(bool must);
  void set_mustuniq(bool must);
  QByteArray mkshort(const QByteArray& istring, bool is_utf8);
  QString mkshort(const QString& istring);
  QString mkshort_from_wpt(const Waypoint* wpt);

private:

  /* Types */

  class ShortNameKey;
  using ShortNameHash = QHash<ShortNameKey, int>;
  class ShortNameKey
  {
  public:
    ShortNameKey(const QByteArray& name) : shortname(name) {} /* converting constructor */

    friend qhash_result_t qHash(const ShortNameKey& key, qhash_result_t seed = 0) noexcept
    {
      // We hash all strings as upper case.
      return qHash(key.shortname.toUpper(), seed);
    }
    bool operator==(const ShortNameKey& other) const
    {
      return shortname.compare(other.shortname, Qt::CaseInsensitive) == 0;
    }

  private:
    QByteArray shortname;
  };

  struct replacement_t {
    QByteArray orig;
    QByteArray replacement;
  };

  /* Constants */

  static const QByteArray vowels;
  static constexpr int default_target_len = 8;
  static constexpr const char default_badchars[] = "\"$.,'!-";
  static const QVector<replacement_t> replacements;

  /* Member Functions */

  void mkshort_add_to_list(QByteArray& name, bool is_utf8);
  static bool delete_last_vowel(int start, QByteArray& iostring);
  static void replace_constants(QByteArray& s);

  /* Data Members */

  int target_len_{default_target_len};
  QByteArray badchars_{default_badchars};
  QByteArray goodchars_;
  QByteArray defname_{"WPT"};
  ShortNameHash namelist_;

  /* Various internal flags */
  bool mustupper_{false};
  bool whitespaceok_{true};
  bool repeating_whitespaceok_{false};
  bool must_uniq_{true};
};
#endif // MKSHORT_H_INCLUDED_
