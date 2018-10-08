/*
    XCSV - X Character Separated Values (.???)

    A hopefully not too feeble attempt at parsing whatever separated values
    files into the waypoint structure and back out again.  This is a config-
    file wrapper around csv_util.c.

    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)

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

#include <QtCore/QTextCodec>
#include <QtCore/QTextStream>
#include <QtCore/QDebug>

#include <cctype>
#include <cstdlib>
#include "csv_util.h"
#include "defs.h"
#include "jeeps/gpsmath.h"
#include "src/core/file.h"
#include "src/core/logging.h"

#if CSVFMTS_ENABLED
#define MYNAME	"XCSV"

static char* styleopt = nullptr;
static char* snlenopt = nullptr;
static char* snwhiteopt = nullptr;
static char* snupperopt = nullptr;
static char* snuniqueopt = nullptr;
char* prefer_shortnames = nullptr;
char* xcsv_urlbase = nullptr;
static char* opt_datum;

static const char* intstylebuf = nullptr;

static
arglist_t xcsv_args[] = {
  {
    "style", &styleopt, "Full path to XCSV style file", nullptr,
    ARGTYPE_FILE | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
  },
  {
    "snlen", &snlenopt, "Max synthesized shortname length", nullptr,
    ARGTYPE_INT, "1", nullptr, nullptr
  },
  {
    "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "snupper", &snupperopt, "UPPERCASE synth. shortnames",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "snunique", &snuniqueopt, "Make synth. shortnames unique",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "urlbase", &xcsv_urlbase, "Basename prepended to URL on output",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "prefer_shortnames", &prefer_shortnames,
    "Use shortname instead of description",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "datum", &opt_datum, "GPS datum (def. WGS 84)",
    "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

/* a table of config file constants mapped to chars */
static
char_map_t xcsv_char_table[] = {
  { "COMMA",		"," 	},
  { "COMMASPACE",		", " 	},
  { "SINGLEQUOTE",	"'"	},
  { "DOUBLEQUOTE",	"\""	},
  { "COLON",		":"	},
  { "SEMICOLON",		";"	},
  { "NEWLINE",		"\n"	},
  { "CR",			"\n"	},
  { "CRNEWLINE",  	"\r\n"	},
  { "TAB",  		"\t"	},
  { "SPACE",  		" "	},
  { "HASH",  		"#"	},
  { "WHITESPACE",		"\\w"	},
  { "PIPE",		"|"	},
  { nullptr, 		nullptr	}
};

void
xcsv_destroy_style(void)
{
  queue* elem, *tmp;
  field_map_t* fmp;

  /*
   * If this xcsv_file struct came from a file we can free it all.
   * If not, we can at least free the queue elements.
   */

  /* destroy the prologue */
  xcsv_file.prologue.clear();

  /* destroy the epilogue */
  xcsv_file.epilogue.clear();

  /* destroy the ifields */
  QUEUE_FOR_EACH(&xcsv_file.ifield, elem, tmp) {
    fmp = reinterpret_cast<field_map_t *>(elem);
    if (fmp->key) {
      xfree(fmp->key);
    }
    if (fmp->val) {
      xfree(fmp->val);
    }
    if (fmp->printfc) {
      xfree(fmp->printfc);
    }
    if (elem) {
      xfree(elem);
    }
  }

  /* destroy the ofields, if they are not re-mapped to ifields. */
  if (xcsv_file.ofield != &xcsv_file.ifield) {
    QUEUE_FOR_EACH(xcsv_file.ofield, elem, tmp) {
      fmp = reinterpret_cast<field_map_t *>(elem);
      if (fmp->key) {
        xfree(fmp->key);
      }
      if (fmp->val) {
        xfree(fmp->val);
      }
      if (fmp->printfc) {
        xfree(fmp->printfc);
      }
      if (elem) {
        xfree(elem);
      }
    }

    if (xcsv_file.ofield) {
      xfree(xcsv_file.ofield);
    }
  }
  xcsv_file.ifields.clear();
  xcsv_file.ofields.clear();

  /* other alloc'd glory */
  xcsv_file.field_delimiter = QString();
  xcsv_file.field_encloser = QString();
  xcsv_file.record_delimiter = QString();
  xcsv_file.badchars = QString();
  xcsv_file.description = QString();
  xcsv_file.extension = QString();

  if (xcsv_file.mkshort_handle) {
    mkshort_del_handle(&xcsv_file.mkshort_handle);
  }

  /* return everything to zeros */
  int internal = xcsv_file.is_internal;
  xcsv_file.is_internal = internal;
}

// Given a keyword of "COMMASPACE", return ", ".
QString
xcsv_get_char_from_constant_table(QString key)
{
  static QHash<QString, QString> substitutions;
  if (substitutions.empty()) {
    for (char_map_t* cm = xcsv_char_table; !cm->key.isNull(); cm++) {
      substitutions.insert(cm->key, cm->chars);
    }
  }
  if (substitutions.contains(key)) {
    return substitutions[key];
  }
  // No substition found? Just return original.
  return key;
}

// Remove outer quotes.
// Should probably be in csv_util.
static QString dequote(QString in) {
  QString r = in.simplified();
  if (r.startsWith("\"")) r = r.mid(1);
  if (r.endsWith("\"")) r.chop(1);
  return r;
}

static void
xcsv_parse_style_line(QString line)
{
  // The lines to be parsed have a leading operation |op| that is
  // separated by whitespace from the rest. Each op may have zero or
  // more comma separated tokens  |token[]|.

  // Handle comments, with an escape. Probably not optimal.
  int escape_idx = line.indexOf('\\');
  int comment_idx = line.indexOf('#');
  if (comment_idx > 0 && escape_idx +1 != comment_idx) {
    line = line.mid(0, line.indexOf("#")).trimmed();
  } else {
    line = line.replace("\\#", "#");
  }

  // Separate op and tokens.
  int sep = line.indexOf(QRegExp("\\s+"));

  // the first token is the operation, e.g. "IFIELD"
  QString op = line.mid(0, sep).trimmed().toUpper();
  QString tokenstr = line.mid(sep).trimmed();
  QStringList tokens = tokenstr.split(",");

  if (op == "FIELD_DELIMITER") {
    auto cp = xcsv_get_char_from_constant_table(tokens[0]);
    xcsv_file.field_delimiter = cp;

    char* p = csv_stringtrim(CSTR(xcsv_file.field_delimiter), " ", 0);
      /* field delimiters are always bad characters */
    if (0 == strcmp(p, "\\w")) {
      xcsv_file.badchars = " \n\r";
    } else {
      xcsv_file.badchars += p;
    }
    xfree(p);

  } else

  if (op == "FIELD_ENCLOSER") {
    auto cp = xcsv_get_char_from_constant_table(tokens[0]);
    xcsv_file.field_encloser = cp;

    char* p = csv_stringtrim(CSTR(xcsv_file.field_encloser), " ", 0);
    xcsv_file.badchars += p;
    xfree(p);
  } else

  if (op == "RECORD_DELIMITER") {
    auto cp = xcsv_get_char_from_constant_table(tokens[0]);
    xcsv_file.record_delimiter = cp;

      // Record delimiters are always bad characters.
    auto p = csv_stringtrim(CSTR(xcsv_file.record_delimiter), " ", 0);
    xcsv_file.badchars += p;
    xfree(p);

  } else

  if (op == "FORMAT_TYPE") {
    if (tokens[0] == "INTERNAL") {
      xcsv_file.type = ff_type_internal;
    }
      // this is almost inconcievable...
    if (tokens[0] == "SERIAL") {
      xcsv_file.type = ff_type_serial;
    }
  } else

  if (op == "DESCRIPTION") {
    xcsv_file.description = tokens[0];
  } else

  if (op == "EXTENSION") {
    xcsv_file.extension = tokens[0];
  } else

  if (op == "SHORTLEN") {
    if (xcsv_file.mkshort_handle) {
      setshort_length(xcsv_file.mkshort_handle, tokens[0].toInt());
    }
  } else

  if (op == "SHORTWHITE") {
    if (xcsv_file.mkshort_handle) {
      setshort_whitespace_ok(xcsv_file.mkshort_handle, tokens[0].toInt());
    }
  } else

  if (op == "BADCHARS") {
    char* sp = csv_stringtrim(CSTR(tokenstr), "\"", 1);
    QString cp = xcsv_get_char_from_constant_table(sp);
    xcsv_file.badchars += cp;
    xfree(sp);
  } else

  if (op =="PROLOGUE") {
    xcsv_prologue_add(tokenstr);
  } else

  if (op == "EPILOGUE") {
    xcsv_epilogue_add(tokenstr);
  } else

  if (op == "ENCODING") {
    QByteArray ba;
    ba.append(tokens[0]);
    xcsv_file.codec = QTextCodec::codecForName(ba);
    if (!xcsv_file.codec) {
      Fatal() << "Unsupported character set '" << QString(tokens[0]) << "'.";
    }
  } else

  if (op == "DATUM") {
    xcsv_file.gps_datum = GPS_Lookup_Datum_Index(tokens[0]);
    is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", CSTR(tokens[0]));
  } else

  if (op == "DATATYPE") {
    QString p = tokens[0].toUpper();
    if (p == "TRACK") {
      xcsv_file.datatype = trkdata;
    } else if (p == "ROUTE") {
      xcsv_file.datatype = rtedata;
    } else if (p == "WAYPOINT") {
      xcsv_file.datatype = wptdata;
    } else {
      Fatal() << MYNAME << ": Unknown data type" << p;
    }
  } else

  if (op == "IFIELD") {
    if (tokens.size() < 3) {
      Fatal() << "Invalid IFIELD line: " << tokenstr;
    }

    // The key ("LAT_DIR") should never contain quotes.

    const char* key = xstrdup(tokens[0].simplified());
    QString s1 = dequote(tokens[1]);
    char* val = xstrdup(s1);

    QString s2 = dequote(tokens[2]);
    char* pfc = xstrdup(s2);
    xcsv_ifield_add(key, val, pfc);
  } else

      //
      //  as OFIELDs are implemented as an after-thought, I'll
      //  leave this as it's own parsing for now.  We could
      //  change the world on ifield vs ofield format later..
      //
  if (op == "OFIELD") {
    int options = 0;
      // Note: simplifieid() has to run after split().
    if (tokens.size() < 3) {
      Fatal() << "Invalid OFIELD line: " << tokenstr;
    }

    // The key ("LAT_DIR") should never contain quotes.
    const char *key = xstrdup(tokens[0].simplified());

    QString s1 = dequote(tokens[1]);
    char *val = xstrdup(s1);

    QString s2 = dequote(tokens[2]);
    const char* pfc = xstrdup(s2);

    // This is pretty lazy way to parse write options.
    // They've very rarely used, so we'll go for simple.
    if (tokens.size() > 4) {
      QString options_string = tokens[3].simplified();
      if (options_string.contains("no_delim_before")) {
        options |= OPTIONS_NODELIM;
      }
      if (options_string.contains("absolute")) {
        options |= OPTIONS_ABSOLUTE;
      }
      if (options_string.contains("optional")) {
        options |= OPTIONS_OPTIONAL;
      }
    }
    xcsv_ofield_add(key, val, pfc, options);
  }
}


/*
 * A wrapper for xcsv_parse_style_line that reads until it hits
 * a terminating null.   Makes multiple calls to that function so
 * that "ignore to end of line" comments work right.
 */
static void
xcsv_parse_style_buff(const char* sbuff)
{
  QStringList lines = QString(sbuff).split('\n');
  for (const auto& line : lines) {
    xcsv_parse_style_line(line);
  }
}

static void
xcsv_read_style(const char* fname)
{
  char* sbuff;

  xcsv_file_init();

  gbfile* fp = gbfopen(fname, "rb", MYNAME);
  while ((sbuff = gbfgetstr(fp))) {
    sbuff = lrtrim(sbuff);
    xcsv_parse_style_line(sbuff);
  }
  while (!gbfeof(fp));

  /* if we have no output fields, use input fields as output fields */
  if (xcsv_file.ofield_ct == 0) {
    if (xcsv_file.ofield) {
      xfree(xcsv_file.ofield);
    }
    xcsv_file.ofield = &xcsv_file.ifield;
    xcsv_file.ofield_ct = xcsv_file.ifield_ct;
  }
  gbfclose(fp);
}

/*
 * Passed a pointer to an internal buffer that would be identical
 * to the series of bytes that would be in a style file, we set up
 * the xcsv parser and make it ready for general use.
 */
void
xcsv_read_internal_style(const char* style_buf)
{
  xcsv_file_init();
  xcsv_file.is_internal = true;

  xcsv_parse_style_buff(style_buf);

  /* if we have no output fields, use input fields as output fields */
  if (xcsv_file.ofield_ct == 0) {
    if (xcsv_file.ofield) {
      xfree(xcsv_file.ofield);
    }
    xcsv_file.ofield = &xcsv_file.ifield;
    xcsv_file.ofield_ct = xcsv_file.ifield_ct;
  }
}

void
xcsv_setup_internal_style(const char* style_buf)
{
  xcsv_file_init();
  xcsv_destroy_style();
  xcsv_file.is_internal = !!style_buf;
  intstylebuf = style_buf;
}


static void
xcsv_rd_init(const QString& fname)
{

  /*
   * if we don't have an internal style defined, we need to
   * read it from a user-supplied style file, or die trying.
   */
  if (xcsv_file.is_internal) {
    xcsv_read_internal_style(intstylebuf);
  } else {
    if (!styleopt) {
      fatal(MYNAME ": XCSV input style not declared.  Use ... -i xcsv,style=path/to/file.style\n");
    }

    xcsv_read_style(styleopt);
  }

  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == wptdata)) {
    if (global_opts.masked_objective & (TRKDATAMASK|RTEDATAMASK)) {
      warning(MYNAME " attempt to read %s as a track or route, but this format only supports waypoints on read.  Reading as waypoints instead.\n", qPrintable(fname));
    }
  }

  xcsv_file.file = new gpsbabel::File(fname);
  xcsv_file.file->open(QFile::ReadOnly);
  xcsv_file.stream = new QTextStream(xcsv_file.file);
  if (xcsv_file.codec) {
    xcsv_file.stream->setCodec(xcsv_file.codec);
  } else {
    // default to UTF-8.
    xcsv_file.stream->setCodec("UTF-8");
    xcsv_file.stream->setAutoDetectUnicode(true);
  }
  xcsv_file.gps_datum = GPS_Lookup_Datum_Index(opt_datum);
  is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", opt_datum);
}

static void
xcsv_rd_deinit(void)
{
  xcsv_file.file->close();
  delete xcsv_file.file;
  xcsv_file.file = nullptr;
  delete xcsv_file.stream;
  xcsv_file.stream = nullptr;
  xcsv_file.codec = nullptr;

  xcsv_destroy_style();
}

static void
xcsv_wr_init(const QString& fname)
{
  /* if we don't have an internal style defined, we need to
   * read it from a user-supplied style file, or die trying.
   * 8/19 - add test for styleopt to ensure that a write of a style
   * after a read of a style works.
   */
  if (xcsv_file.is_internal && !styleopt) {
    xcsv_read_internal_style(intstylebuf);
  } else {

    if (!styleopt) {
      fatal(MYNAME ": XCSV output style not declared.  Use ... -o xcsv,style=path/to/file.style\n");
    }

    xcsv_read_style(styleopt);
  }

  xcsv_file.file = new gpsbabel::File(fname);
  xcsv_file.file->open(QFile::WriteOnly | QFile::Text);
  xcsv_file.stream = new QTextStream(xcsv_file.file);
  if (xcsv_file.codec) {
    xcsv_file.stream->setCodec(xcsv_file.codec);
    // enable bom for all UTF codecs except UTF-8
    if (xcsv_file.codec->mibEnum() != 106) {
      xcsv_file.stream->setGenerateByteOrderMark(true);
    }
  } else {
    // emulate gbfputs which assumes UTF-8.
    xcsv_file.stream->setCodec("UTF-8");
  }
  xcsv_file.fname = fname;

  /* set mkshort options from the command line */
  if (global_opts.synthesize_shortnames) {

    if (snlenopt) {
      setshort_length(xcsv_file.mkshort_handle, atoi(snlenopt));
    }

    if (snwhiteopt) {
      setshort_whitespace_ok(xcsv_file.mkshort_handle, atoi(snwhiteopt));
    }

    if (snupperopt) {
      setshort_mustupper(xcsv_file.mkshort_handle, atoi(snupperopt));
    }

    if (snuniqueopt) {
      setshort_mustuniq(xcsv_file.mkshort_handle, atoi(snuniqueopt));
    }

    setshort_badchars(xcsv_file.mkshort_handle, CSTR(xcsv_file.badchars));

  }
  xcsv_file.gps_datum = GPS_Lookup_Datum_Index(opt_datum);
  is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", opt_datum);
}

static void
xcsv_wr_position_init(const QString& fname)
{
  xcsv_wr_init(fname);
}

static void
xcsv_wr_deinit(void)
{
  xcsv_file.stream->flush();
  xcsv_file.file->close();
  delete xcsv_file.file;
  xcsv_file.file = nullptr;
  delete xcsv_file.stream;
  xcsv_file.stream = nullptr;
  xcsv_file.codec = nullptr;

  xcsv_destroy_style();
}

static void
xcsv_wr_position_deinit(void)
{
  xcsv_wr_deinit();
}


static void
xcsv_wr_position(Waypoint* wpt)
{
  /* Tweak incoming name if we don't have a fix */
  switch (wpt->fix) {
  case fix_none:
    wpt->shortname = "ESTIMATED Position";
    break;
  default:
    break;
  }

  waypt_add(wpt);
  xcsv_data_write();
  waypt_del(wpt);

  xcsv_file.stream->flush();
}

ff_vecs_t xcsv_vecs = {
  ff_type_internal,
  FF_CAP_RW_WPT, /* This is a bit of a lie for now... */
  xcsv_rd_init,
  xcsv_wr_init,
  xcsv_rd_deinit,
  xcsv_wr_deinit,
  xcsv_data_read,
  xcsv_data_write,
  nullptr,
  xcsv_args,
  CET_CHARSET_ASCII, 0,	/* CET-REVIEW */
  { nullptr, nullptr, nullptr, xcsv_wr_position_init, xcsv_wr_position, xcsv_wr_position_deinit },
  nullptr

};
#else
void xcsv_read_internal_style(const char* style_buf) {}
void xcsv_setup_internal_style(const char* style_buf) {}
#endif //CSVFMTS_ENABLED
