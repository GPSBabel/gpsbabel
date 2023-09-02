/*
    Generate unique short names.

    Copyright (C) 2003, 2004, 2005, 2006 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cassert>     // for assert
#include <cctype>      // for isspace, isdigit

#include <QByteArray>  // for QByteArray
#include <QChar>       // for QChar, QChar::ReplacementCharacter
#include <QHash>       // for QHash, QHash<>::iterator, qHash, QHash<>::size_type
#include <QString>     // for QString
#include <QVector>     // for QVector
#include <Qt>          // for CaseInsensitive

#include "defs.h"
#include "geocache.h"  // for Geocache


#define MYNAME	"mkshort"

static const QByteArray vowels("aeiouAEIOU");
static constexpr int default_target_len = 8;
static constexpr const char default_badchars[] = "\"$.,'!-";

class ShortNameKey;
using ShortNameHash = QHash<ShortNameKey, int>;
class ShortNameKey {
public:
  ShortNameKey(const QByteArray& name) : shortname(name) {} /* converting constructor */

  friend qhash_result_t qHash(const ShortNameKey &key, qhash_result_t seed = 0) noexcept
  {
    // We hash all strings as upper case.
    return qHash(key.shortname.toUpper(), seed);
  }

  QByteArray shortname;
};

inline bool operator==(const ShortNameKey& lhs, const ShortNameKey &rhs) noexcept
{
  return lhs.shortname.compare(rhs.shortname, Qt::CaseInsensitive) == 0;
}

struct  mkshort_handle_imp {
  int target_len{default_target_len};
  QByteArray badchars{default_badchars};
  QByteArray goodchars;
  QByteArray defname{"WPT"};
  ShortNameHash namelist;

  /* Various internal flags */
  bool mustupper{false};
  bool whitespaceok{true};
  bool repeating_whitespaceok{false};
  bool must_uniq{true};
};

struct replacement_t {
  QByteArray orig;
  QByteArray replacement;
};
static const QVector<replacement_t> replacements = {
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

short_handle
mkshort_new_handle()
{
  return new mkshort_handle_imp;
}

static
void
mkshort_add_to_list(mkshort_handle_imp* h, QByteArray& name)
{
  while (h->namelist.contains(name)) {
    auto& conflictctr = h->namelist[name];

    QByteArray suffix(".");
    suffix.append(QByteArray::number(++conflictctr));

    if (name.size() + suffix.size() <= h->target_len) {
      name.append(suffix);
    } else if (suffix.size() <= h->target_len) {
      // FIXME: utf8 => grapheme truncate
      name.truncate(h->target_len - suffix.size());
      name.append(suffix);
    } else {
      fatal("mkshort failure, the specified short length is insufficient.\n");
    }
  }

  h->namelist.insert(name, 0);
}

void
mkshort_del_handle(short_handle* h)
{
  if (!h) {
    return;
  }

  auto* hdr = (mkshort_handle_imp*) *h;

  if (!hdr) {
    return;
  }

#if 0
  for (auto it = hdr->namelist.cbegin(), end = hdr->namelist.cend(); it != end; ++it) {
    if (global_opts.verbose_status >= 2 && it.value()->conflictctr) {
      fprintf(stderr, "%d Output name conflicts: '%s'\n",
              it.value()->conflictctr, it.key().shortname.constData());
    }
  }
#endif

  delete hdr;
  *h = nullptr;
}

/*
 * This is the stuff that makes me ashamed to be a C programmer...
 */

static
bool
delete_last_vowel(int start, QByteArray& iostring)
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
void
replace_constants(QByteArray& s)
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
void
setshort_length(short_handle h, int l)
{
  auto* hdl = (mkshort_handle_imp*) h;
  if (l < 0) {
    fatal("mkshort: short length must be non-negative.\n");
  } else if (l == 0) {
    hdl->target_len = default_target_len;
  } else {
    hdl->target_len = l;
  }
}

/*
 * Call with L nonzero if whitespace in the generated shortname is wanted.
 */

void
setshort_whitespace_ok(short_handle h, int l)
{
  auto* hdl = (mkshort_handle_imp*) h;
  hdl->whitespaceok = l;
}

/*
 * Call with L nonzero if multiple consecutive whitespace in the
 * generated shortname is wanted.
 */

void
setshort_repeating_whitespace_ok(short_handle h, int l)
{
  auto* hdl = (mkshort_handle_imp*) h;
  hdl->repeating_whitespaceok = l;
}

/*
 * Set default name given to a waypoint if no valid is possible
 * because it was filtered by charsets or null or whatever.
 */
void
setshort_defname(short_handle h, const char* s)
{
  auto* hdl = (mkshort_handle_imp*) h;
  if (s == nullptr) {
    fatal("setshort_defname called without a valid name.");
  }
  hdl->defname = s;
}

/*
 * Externally callable function to set the string of characters
 * that must never appear in a string returned by mkshort.  NULL
 * resets to default.
 */
void
setshort_badchars(short_handle h, const char* s)
{
  auto* hdl = (mkshort_handle_imp*) h;

  hdl->badchars = s ? s : default_badchars;
}

/*
 * Only characters that appear in *s are "whitelisted" to appear
 * in generated names.
 */
void
setshort_goodchars(short_handle h, const char* s)
{
  auto* hdl = (mkshort_handle_imp*) h;

  if (s != nullptr) {
    hdl->goodchars = s;
  } else {
    hdl->goodchars.clear();
  }
}

/*
 *  Call with i non-zero if generated names must be uppercase only.
 */
void
setshort_mustupper(short_handle h, int i)
{
  auto* hdl = (mkshort_handle_imp*) h;
  hdl->mustupper = i;
}


/*
 *  Call with i zero if the generated names don't have to be unique.
 *  (By default, they are.)
 */
void
setshort_mustuniq(short_handle h, int i)
{
  auto* hdl = (mkshort_handle_imp*) h;
  hdl->must_uniq = i;
}

QByteArray
mkshort(short_handle h, const QByteArray& istring, bool is_utf8)
{
  QByteArray rval;
  auto* hdl = (mkshort_handle_imp*) h;

  if (is_utf8) {
    /* clean UTF-8 string */
    QString result = QString::fromUtf8(istring);
    // QString::fromUtf8() doesn't quite promise to use QChar::ReplacementCharacter,
    // but if it did toss them.
    result.remove(QChar::ReplacementCharacter);
    rval = result.toUtf8();
  } else {
    rval = istring;
  }

  /*
   * A rather horrible special case hack.
   * If the target length is "6" and the source length is "7" and
   * the first two characters are "GC", we'll assume it's one of the
   * the new seven digit geocache numbers and special case whacking
   * the 'G' off the front.
   */
  if ((hdl->target_len == 6) && (rval.size() == 7) &&
      rval.startsWith("GC")) {
    rval.remove(0, 1);
  }

  /*
   * Whack leading "[Tt]he "
   */
  if ((rval.size() > (hdl->target_len + 4)) &&
      (rval.startsWith("The ") || rval.startsWith("the "))) {
    rval.remove(0, 4);
  }

  /* Eliminate leading whitespace in all cases */
  while (isspace(rval.front())) {
    rval.remove(0, 1);
  }

  if (!hdl->whitespaceok) {
    /*
     * Eliminate Whitespace
     */
    QByteArray tval;
    rval.swap(tval);
    for (auto ch : tval) {
      if (!isspace(ch)) {
        rval.append(ch);
      }
    }
  }

  if (hdl->mustupper) {
    rval = rval.toUpper();
  }

  /* Before we do any of the vowel or character removal, look for
   * constants to replace.
   */

  replace_constants(rval);

  /*
   * Eliminate chars on the blacklist.
   */
  QByteArray tval;
  rval.swap(tval);
  for (auto ch : tval) {
    if (hdl->badchars.contains(ch)) {
      continue;
    }
    if (!hdl->goodchars.isEmpty() && (!hdl->goodchars.contains(ch))) {
      continue;
    }
    rval.append(ch);
  }

  /*
   * Eliminate repeated whitespace.  This can only shorten the string
   * so we do it in place.
   */
  if (!hdl->repeating_whitespaceok) {
    // simplified also removes any leading and trailing whitespace, which is fine.
    rval = rval.simplified();
  } 

  /*
   * Eliminate trailing whitespace in all cases. This is done late because
   * earlier operations may have vacated characters after the space.
   */
  while (isspace(rval.back())) {
    rval.chop(1);
  }


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
  bool replaced = hdl->target_len < 15;

  while (replaced && (rval.size() > hdl->target_len)) {
    replaced = delete_last_vowel(2, rval);
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
  if (int delcnt = rval.size() - hdl->target_len; delcnt > 0) {
    int suffixcnt = 0;
    for (auto it = rval.crbegin(); it != rval.crend(); ++it) {
      if (isdigit(*it)) {
        ++suffixcnt;
      }
      if (suffixcnt == hdl->target_len) {
        break;
      }
    }

    int keepcnt = hdl->target_len - suffixcnt;
    assert(keepcnt >= 0);

    if (true && is_utf8) {
      QString result = grapheme_truncate(QString::fromUtf8(rval), keepcnt);
      rval = result.toUtf8().append(rval.right(suffixcnt));
    } else {
      rval.remove(keepcnt, delcnt);
    }
    while (isspace(rval.back())) {
      rval.chop(1);
    }
  }

  /*
   * If, after all that, we have an empty string, punt and
   * let the must_uniq code handle it.
   */
  if (rval.isEmpty()) {
    rval = hdl->defname;
  }

  if (hdl->must_uniq) {
    mkshort_add_to_list(hdl, rval);
  }
  return rval;
}

QString
mkshort(short_handle h, const QString& istring)
{
  return mkshort(h, istring.toUtf8(), true);
}

/*
 * As above, but arg list is a waypoint so we can centralize
 * the code that considers the alternate sources.
 */
QString
mkshort_from_wpt(short_handle h, const Waypoint* wpt)
{
  /* This probably came from a Groundspeak Pocket Query
   * so use the 'cache name' instead of the description field
   * which contains placer name, diff, terr, and generally way
   * more stuff than should be in any one field...
   */
  if (wpt->gc_data->diff && wpt->gc_data->terr &&
      !wpt->notes.isEmpty()) {
    return mkshort(h, wpt->notes);
  }

  if (!wpt->description.isEmpty()) {
    return mkshort(h, wpt->description);
  }

  if (!wpt->notes.isEmpty()) {
    return mkshort(h, wpt->notes);
  }

  /* Should probably never actually happen... */
  /* O.K.: But this can happen (waypoints transformed from trackpoints )! */
  /*       Now we return every time a valid entity." */

  return mkshort(h, wpt->shortname);
}
