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

#include "mkshort.h"

#include <cassert>     // for assert
#include <cctype>      // for isspace, isdigit
#include <utility>     // for as_const

#include <QByteArray>  // for QByteArray
#include <QChar>       // for QChar, QChar::ReplacementCharacter
#include <QString>     // for QString
#include <QVector>     // for QVector
#include <Qt>          // for CaseInsensitive

#include "defs.h"
#include "geocache.h"  // for Geocache


const QByteArray MakeShort::vowels = "aeiouAEIOU";

const QVector<MakeShort::replacement_t> MakeShort::replacements = {
  {"zero", "0"},
  {"one", "1"},
  {"two", "2"},
  {"three", "3"},
  {"four", "4"},
  {"five", "5"},
  {"six", "6"},
  {"seven", "7"},
  {"eight", "8"},
  {"nine", "9"}
};

void MakeShort::mkshort_add_to_list(QByteArray& name, bool is_utf8)
{
  while (namelist_.contains(name)) {
    auto& conflictctr = namelist_[name];

    QByteArray suffix(".");
    suffix.append(QByteArray::number(++conflictctr));
    int suffixcnt = suffix.size();

    if (name.size() + suffixcnt <= target_len_) {
      name.append(suffix);
    } else if (int keepcnt = target_len_ - suffixcnt; keepcnt >= 0) {
      if (is_utf8) {
        QString result = grapheme_truncate(QString::fromUtf8(name), keepcnt);
        name = result.toUtf8().append(suffix);
      } else {
        name.truncate(keepcnt);
        name.append(suffix);
      }
    } else {
      gbFatal("mkshort failure, the specified short length is insufficient.\n");
    }
  }

  namelist_.insert(name, 0);
}

/*
 * This is the stuff that makes me ashamed to be a C programmer...
 */

bool MakeShort::delete_last_vowel(int start, QByteArray& iostring)
{
  /*
   * Basically implement strrchr.
   */
  assert(start >= 1);
  bool replaced = false;
  for (int l = iostring.size(); l > start; --l) {
    if (vowels.contains(iostring.at(l - 1))) {
      /* If vowel is the first letter of a word, keep it.*/
      if (iostring.at(l - 2) == ' ') {
        continue;
      }
      iostring.remove(l - 1, 1);
      replaced = true;
      break;
    }
  }
  return replaced;
}

/*
 * Open the slippery slope of literal replacement.   Right now, replacements
 * are made only at the end of the string.
 */
void MakeShort::replace_constants(QByteArray& s)
{
  for (const auto& r : replacements) {
    /*
     * If word is in replacement list and preceded by a
     * space, replace it.
     */
    if ((s.size() > r.orig.size()) &&
        (0 == r.orig.compare(s.mid(s.size() - r.orig.size()), Qt::CaseInsensitive)) &&
        (s.at(s.size() - r.orig.size() - 1) == ' ')) {
      s.truncate(s.size() - r.orig.size());
      s.append(r.replacement);
      return;
    }
  }
}

/*
 * Externally callable function to set the max length of the
 * strings returned by mkshort().  0 resets to default.
 */
void MakeShort::set_length(int l)
{
  if (l < 0) {
    gbFatal("mkshort: short length must be non-negative.\n");
  } else if (l == 0) {
    target_len_ = default_target_len;
  } else {
    target_len_ = l;
  }
}

/*
 * Call with ok = true if whitespace in the generated shortname is wanted.
 */

void MakeShort::set_whitespace_ok(bool ok)
{
  whitespaceok_ = ok;
}

/*
 * Call with ok = true if multiple consecutive whitespace in the
 * generated shortname is wanted.
 */

void MakeShort::set_repeating_whitespace_ok(bool ok)
{
  repeating_whitespaceok_ = ok;
}

/*
 * Set default name given to a waypoint if no valid is possible
 * because it was filtered by charsets or null or whatever.
 */
void MakeShort::set_defname(const char* s)
{
  if (s == nullptr) {
    gbFatal("set_defname called without a valid name.\n");
  }
  defname_ = s;
}

/*
 * Externally callable function to set the string of characters
 * that must never appear in a string returned by mkshort.  NULL
 * resets to default.
 */
void MakeShort::set_badchars(const char* s)
{
  badchars_ = (s  == nullptr)? default_badchars : s;
}

/*
 * Only characters that appear in *s are "whitelisted" to appear
 * in generated names.
 */
void MakeShort::set_goodchars(const char* s)
{
  if (s == nullptr) {
    goodchars_.clear();
  } else {
    goodchars_ = s;
  }
}

/*
 *  Call with must = true if generated names must be uppercase only.
 */
void MakeShort::set_mustupper(bool must)
{
  mustupper_ = must;
}


/*
 *  Call with must = false if the generated names don't have to be unique.
 *  (By default, they are.)
 */
void MakeShort::set_mustuniq(bool must)
{
  must_uniq_ = must;
}

QByteArray MakeShort::mkshort(const QByteArray& istring, bool is_utf8)
{
  QByteArray ostring;

  if (is_utf8) {
    /* clean UTF-8 string */
    QString result = QString::fromUtf8(istring);
    // QString::fromUtf8() doesn't quite promise to use QChar::ReplacementCharacter,
    // but if it did toss them.
    result.remove(QChar::ReplacementCharacter);
    ostring = result.toUtf8();
  } else {
    ostring = istring;
  }

  /*
   * A rather horrible special case hack.
   * If the target length is "6" and the source length is "7" and
   * the first two characters are "GC", we'll assume it's one of the
   * the new seven digit geocache numbers and special case whacking
   * the 'G' off the front.
   */
  if ((target_len_ == 6) && (ostring.size() == 7) &&
      ostring.startsWith("GC")) {
    ostring.remove(0, 1);
  }

  /*
   * Whack leading "[Tt]he "
   */
  if ((ostring.size() > (target_len_ + 4)) &&
      (ostring.startsWith("The ") || ostring.startsWith("the "))) {
    ostring.remove(0, 4);
  }

  /* In all cases eliminate leading and trailing whitespace */
  ostring = ostring.trimmed();

  if (!whitespaceok_) {
    /*
     * Eliminate Whitespace
     */
    QByteArray tstring;
    ostring.swap(tstring);
    for (const auto ch : std::as_const(tstring)) {
      if (!isspace(ch)) {
        ostring.append(ch);
      }
    }
  }

  if (mustupper_) {
    ostring = ostring.toUpper();
  }

  /* Before we do any of the vowel or character removal, look for
   * constants to replace.
   */

  replace_constants(ostring);

  /*
   * Eliminate chars on the blacklist.
   */
  {
    QByteArray tstring;
    ostring.swap(tstring);
    for (const auto ch : std::as_const(tstring)) {
      if (badchars_.contains(ch)) {
        continue;
      }
      if (!goodchars_.isEmpty() && (!goodchars_.contains(ch))) {
        continue;
      }
      ostring.append(ch);
    }
  }

  /*
   * Eliminate whitespace.
   * In all cases remove leading and trailing whitespace.
   * Leading and/or trailing whitespace may have been created by earlier
   * operations that removed character(s) before and/or after whitespace.
   * Conditionally simplify embedded whitespace.
   */
  ostring = repeating_whitespaceok_? ostring.trimmed() : ostring.simplified();

  /*
   * Toss vowels to approach target length, but don't toss them
   * if we don't have to.  We always keep the leading two letters
   * to preserve leading vowels and some semblance of pronouncability.
   *
   */

  /*
   * Delete vowels starting from the end.  If it fits, quit stomping
   * them.  If we run out of string, give up.
   *
   * Skip this test is our target length is arbitrarily considered
   * "long" as it turns out a truncated string of full words is easier
   * to read than a full string of vowelless words.
   *
   * It also helps units with speech synthesis.
   */
  bool replaced = target_len_ < 15;

  while (replaced && (ostring.size() > target_len_)) {
    replaced = delete_last_vowel(2, ostring);
  }

  /*
   * Next to last thing, we look for trailing numbers and try to
   * preserve those.  This ensures that.
   * Walk in the Woods 1.
   * Walk in the Woods 2.
   */

  /*
   * Now brutally truncate the resulting string, preserve trailing
   * numeric data.
   * If the numeric component alone is longer than our target string
   * length, use the trailing part of the the numeric component.
   */
  if (int delcnt = ostring.size() - target_len_; delcnt > 0) {
    int suffixcnt = 0;
    for (auto it = ostring.crbegin(); it != ostring.crend(); ++it) {
      if (isdigit(*it)) {
        ++suffixcnt;
      }
      if (suffixcnt == target_len_) {
        break;
      }
    }

    int keepcnt = target_len_ - suffixcnt;
    assert(keepcnt >= 0);

    if (is_utf8) {
      QString result = grapheme_truncate(QString::fromUtf8(ostring), keepcnt);
      ostring = result.toUtf8().append(ostring.right(suffixcnt));
    } else {
      ostring.remove(keepcnt, delcnt);
    }
    while (isspace(ostring.back())) {
      ostring.chop(1);
    }
  }

  /*
   * If, after all that, we have an empty string, punt and
   * let the must_uniq code handle it.
   */
  if (ostring.isEmpty()) {
    ostring = defname_;
  }

  if (must_uniq_) {
    mkshort_add_to_list(ostring, is_utf8);
  }
  return ostring;
}

QString MakeShort::mkshort(const QString& istring)
{
  return mkshort(istring.toUtf8(), true);
}

/*
 * As above, but arg list is a waypoint so we can centralize
 * the code that considers the alternate sources.
 */
QString MakeShort::mkshort_from_wpt(const Waypoint* wpt)
{
  /* This probably came from a Groundspeak Pocket Query
   * so use the 'cache name' instead of the description field
   * which contains placer name, diff, terr, and generally way
   * more stuff than should be in any one field...
   */
  if (wpt->gc_data->diff && wpt->gc_data->terr &&
      !wpt->notes.isEmpty()) {
    return mkshort(wpt->notes);
  }

  if (!wpt->description.isEmpty()) {
    return mkshort(wpt->description);
  }

  if (!wpt->notes.isEmpty()) {
    return mkshort(wpt->notes);
  }

  /* Should probably never actually happen... */
  /* O.K.: But this can happen (waypoints transformed from trackpoints )! */
  /*       Now we return every time a valid entity." */

  return mkshort(wpt->shortname);
}
