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

#include <ctype.h>
#include "defs.h"
#include "csv_util.h"
#include "jeeps/gpsmath.h"

#if CSVFMTS_ENABLED
#define MYNAME	"XCSV"
#define ISSTOKEN(a,b) (strncmp(a,b, strlen(b)) == 0)

static char *styleopt = NULL;
static char *snlenopt = NULL;
static char *snwhiteopt = NULL;
static char *snupperopt = NULL;
static char *snuniqueopt = NULL;
char *prefer_shortnames = NULL;
char *xcsv_urlbase = NULL;
static char *opt_datum;

static const char *intstylebuf = NULL;

static
arglist_t xcsv_args[] = {
  {
    "style", &styleopt, "Full path to XCSV style file", NULL,
    ARGTYPE_FILE | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "snlen", &snlenopt, "Max synthesized shortname length", NULL,
    ARGTYPE_INT, "1", NULL
  },
  {
    "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "snupper", &snupperopt, "UPPERCASE synth. shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "snunique", &snuniqueopt, "Make synth. shortnames unique",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "urlbase", &xcsv_urlbase, "Basename prepended to URL on output",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "prefer_shortnames", &prefer_shortnames,
    "Use shortname instead of description",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "datum", &opt_datum, "GPS datum (def. WGS 84)",
    "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX
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
  { NULL, 		NULL	}
};

void
xcsv_destroy_style(void)
{
  queue *elem, *tmp;
  field_map_t *fmp;
  ogue_t *ogp;
  int internal = 0;

  /*
   * If this xcsv_file struct came from a file we can free it all.
   * If not, we can at least free the queue elements.
   */

  /* destroy the prologue */
  QUEUE_FOR_EACH(&xcsv_file.prologue, elem, tmp) {
    ogp = (ogue_t *)elem;
    if (ogp->val) {
      xfree(ogp->val);
    }
    if (elem) {
      xfree(elem);
    }
  }

  /* destroy the epilogue */
  QUEUE_FOR_EACH(&xcsv_file.epilogue, elem, tmp) {
    ogp = (ogue_t *)elem;
    if (ogp->val) {
      xfree(ogp->val);
    }
    if (elem) {
      xfree(elem);
    }
  }

  /* destroy the ifields */
  QUEUE_FOR_EACH(&xcsv_file.ifield, elem, tmp) {
    fmp = (field_map_t *) elem;
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
      fmp = (field_map_t *) elem;
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

  /* other alloc'd glory */
  if (xcsv_file.field_delimiter) {
    xfree(xcsv_file.field_delimiter);
  }

  if (xcsv_file.field_encloser) {
    xfree(xcsv_file.field_encloser);
  }

  if (xcsv_file.record_delimiter) {
    xfree(xcsv_file.record_delimiter);
  }

  if (xcsv_file.badchars) {
    xfree(xcsv_file.badchars);
  }

  if (xcsv_file.description) {
    xfree(xcsv_file.description);
  }

  if (xcsv_file.extension) {
    xfree(xcsv_file.extension);
  }

  if (xcsv_file.mkshort_handle) {
    mkshort_del_handle(&xcsv_file.mkshort_handle);
  }

  /* return everything to zeros */
  internal = xcsv_file.is_internal;
  memset(&xcsv_file, '\0', sizeof(xcsv_file));
  xcsv_file.is_internal = internal;
}

const char *
xcsv_get_char_from_constant_table(char *key)
{
  char_map_t *cm = xcsv_char_table;

  while ((cm->key) && (strcmp(key, cm->key) != 0)) {
    cm++;
  }

  return (cm->chars);
}

static void
xcsv_parse_style_line(const char *sbuff)
{
  int i, linecount = 0;
  char *s, *p, *sp;
  const char *cp;
  char *key, *val, *pfc;

  /*
   * tokens should be parsed longest to shortest, unless something
   * requires a previously set value.  This way something like
   * SHORT and SHORTNAME don't collide.
   */

  /* whack off any comments */
  if ((p = strchr(sbuff, '#')) != NULL) {
    if ((p > sbuff) && p[-1] == '\\') {
      memmove(p-1, p, strlen(p));
      p[strlen(p)-1] = '\0';
    } else {
      *p = '\0';
    }
  }

  if (strlen(sbuff)) {
    if (ISSTOKEN(sbuff, "FIELD_DELIMITER")) {
      sp = csv_stringtrim(&sbuff[16], "\"", 1);
      cp = xcsv_get_char_from_constant_table(sp);
      if (cp) {
        xcsv_file.field_delimiter = xstrdup(cp);
        xfree(sp);
      } else {
        xcsv_file.field_delimiter = sp;
      }

      p = csv_stringtrim(xcsv_file.field_delimiter, " ", 0);

      /* field delimiters are always bad characters */
      if (0 == strcmp(p, "\\w")) {
        char *s = xstrappend(xcsv_file.badchars, " \n\r");
        if (xcsv_file.badchars) {
          xfree(xcsv_file.badchars);
        }
        xcsv_file.badchars = s;
      } else {
        xcsv_file.badchars = xstrappend(xcsv_file.badchars, p);
      }

      xfree(p);

    } else

      if (ISSTOKEN(sbuff, "FIELD_ENCLOSER")) {
        sp = csv_stringtrim(&sbuff[15], "\"", 1);
        cp = xcsv_get_char_from_constant_table(sp);
        if (cp) {
          xcsv_file.field_encloser = xstrdup(cp);
          xfree(sp);
        } else {
          xcsv_file.field_encloser = sp;
        }

        p = csv_stringtrim(xcsv_file.field_encloser, " ", 0);

        /* field_enclosers are always bad characters */
        if (xcsv_file.badchars) {
          xcsv_file.badchars = (char *) xrealloc(xcsv_file.badchars,
                                                 strlen(xcsv_file.badchars) +
                                                 strlen(p) + 1);
        } else {
          xcsv_file.badchars = (char *) xcalloc(strlen(p) + 1, 1);
        }

        strcat(xcsv_file.badchars, p);

        xfree(p);

    } else

      if (ISSTOKEN(sbuff, "RECORD_DELIMITER")) {
        sp = csv_stringtrim(&sbuff[17], "\"", 1);
        cp = xcsv_get_char_from_constant_table(sp);
        if (cp) {
          xcsv_file.record_delimiter = xstrdup(cp);
          xfree(sp);
        } else {
          xcsv_file.record_delimiter = sp;
        }

        p = csv_stringtrim(xcsv_file.record_delimiter, " ", 0);

        /* record delimiters are always bad characters */
        if (xcsv_file.badchars) {
          xcsv_file.badchars = (char *) xrealloc(xcsv_file.badchars,
                                                 strlen(xcsv_file.badchars) +
                                                 strlen(p) + 1);
        } else {
          xcsv_file.badchars = (char *) xcalloc(strlen(p) + 1, 1);
        }

        strcat(xcsv_file.badchars, p);

        xfree(p);

      } else

        if (ISSTOKEN(sbuff, "FORMAT_TYPE")) {
          const char *p;
          for (p = &sbuff[11]; *p && isspace(*p); p++) {
            ;
          }
          if (ISSTOKEN(p, "INTERNAL")) {
            xcsv_file.type = ff_type_internal;
          }
          /* this is almost inconcievable... */
          if (ISSTOKEN(p, "SERIAL")) {
            xcsv_file.type = ff_type_serial;
          }
        } else

          if (ISSTOKEN(sbuff, "DESCRIPTION")) {
            xcsv_file.description = csv_stringtrim(&sbuff[11],"", 0);
          } else

            if (ISSTOKEN(sbuff, "EXTENSION")) {
              xcsv_file.extension = csv_stringtrim(&sbuff[10],"", 0);
            } else

              if (ISSTOKEN(sbuff, "SHORTLEN")) {
                if (xcsv_file.mkshort_handle) {
                  setshort_length(xcsv_file.mkshort_handle, atoi(&sbuff[9]));
                }
              } else

                if (ISSTOKEN(sbuff, "SHORTWHITE")) {
                  if (xcsv_file.mkshort_handle) {
                    setshort_whitespace_ok(xcsv_file.mkshort_handle, atoi(&sbuff[12]));
                  }
                } else

                  if (ISSTOKEN(sbuff, "BADCHARS")) {
                    sp = csv_stringtrim(&sbuff[9], "\"", 1);
                    cp = xcsv_get_char_from_constant_table(sp);

                    if (cp) {
                      p = xstrdup(cp);
                      xfree(sp);
                    } else {
                      p = sp;
                    }

                    if (xcsv_file.badchars) {
                      xcsv_file.badchars = (char *) xrealloc(xcsv_file.badchars,
                                                             strlen(xcsv_file.badchars) +
                                                             strlen(p) + 1);
                    } else {
                      xcsv_file.badchars = (char *) xcalloc(strlen(p) + 1, 1);
                    }

                    strcat(xcsv_file.badchars, p);

                    xfree(p);

                  } else

                    if (ISSTOKEN(sbuff, "PROLOGUE")) {
                      xcsv_prologue_add(xstrdup(&sbuff[9]));
                    } else

                      if (ISSTOKEN(sbuff, "EPILOGUE")) {
                        xcsv_epilogue_add(xstrdup(&sbuff[9]));
                      } else

                        if (ISSTOKEN(sbuff, "ENCODING")) {
                          p = csv_stringtrim(&sbuff[8], "\"", 1);
                          cet_convert_init(p, 1);
                          xfree(p);
                        } else

                          if (ISSTOKEN(sbuff, "DATUM")) {
                            p = csv_stringtrim(&sbuff[5], "\"", 1);
                            xcsv_file.gps_datum = GPS_Lookup_Datum_Index(p);
                            is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", p);
                            xfree(p);
                          } else

                            if (ISSTOKEN(sbuff, "DATATYPE")) {
                              p = csv_stringtrim(&sbuff[8], "\"", 1);
                              if (case_ignore_strcmp(p, "TRACK") == 0) {
                                xcsv_file.datatype = trkdata;
                              } else if (case_ignore_strcmp(p, "ROUTE") == 0) {
                                xcsv_file.datatype = rtedata;
                              } else if (case_ignore_strcmp(p, "WAYPOINT") == 0) {
                                xcsv_file.datatype = wptdata;
                              } else {
                                fatal(MYNAME ": Unknown data type \"%s\"!\n", p);
                              }
                              xfree(p);

                            } else

                              if (ISSTOKEN(sbuff, "IFIELD")) {
                                key = val = pfc = NULL;

                                s = csv_lineparse(&sbuff[6], ",", "", linecount);

                                i = 0;
                                while (s) {
                                  switch (i) {
                                  case 0:
                                    /* key */
                                    key = csv_stringtrim(s, "\"", 1);
                                    break;
                                  case 1:
                                    /* default value */
                                    val = csv_stringtrim(s, "\"", 1);
                                    break;
                                  case 2:
                                    /* printf conversion */
                                    pfc = csv_stringtrim(s, "\"", 1);
                                    break;
                                  default:
                                    break;
                                  }
                                  i++;

                                  s = csv_lineparse(NULL, ",", "", linecount);
                                }

                                xcsv_ifield_add(key, val, pfc);

                              } else

                                /*
                                 * as OFIELDs are implemented as an after-thought, I'll
                                 * leave this as it's own parsing for now.  We could
                                 * change the world on ifield vs ofield format later..
                                 */
                                if (ISSTOKEN(sbuff, "OFIELD")) {
                                  int options = 0;
                                  key = val = pfc = NULL;

                                  s = csv_lineparse(&sbuff[6], ",", "", linecount);

                                  i = 0;
                                  while (s) {
                                    switch (i) {
                                    case 0:
                                      /* key */
                                      key = csv_stringtrim(s, "\"", 1);
                                      break;
                                    case 1:
                                      /* default value */
                                      val = csv_stringtrim(s, "\"", 1);
                                      break;
                                    case 2:
                                      /* printf conversion */
                                      pfc = csv_stringtrim(s, "\"", 1);
                                      break;
                                    case 3:
                                      /* Any additional options. */
                                      if (strstr(s, "no_delim_before")) {
                                        options |= OPTIONS_NODELIM;
                                      }
                                      if (strstr(s, "absolute")) {
                                        options |= OPTIONS_ABSOLUTE;
                                      }
                                      if (strstr(s, "optional")) {
                                        options |= OPTIONS_OPTIONAL;
                                      }
                                    default:
                                      break;
                                    }
                                    i++;
                                    s = csv_lineparse(NULL, ",", "", linecount);
                                  }

                                  xcsv_ofield_add(key, val, pfc, options);
                                }
  }
}


/*
 * A wrapper for xcsv_parse_style_line that reads until it hits
 * a terminating null.   Makes multiple calls to that function so
 * that "ignore to end of line" comments work right.
 */
static void
xcsv_parse_style_buff(const char *sbuff)
{
  char ibuf[256];
  char *ibufp;
  size_t i;

  while (*sbuff) {
    ibuf[0] = 0;
    i = 0;
    for (ibufp = ibuf; *sbuff != '\n' && i++ < sizeof(ibuf);) {
      *ibufp++ = *sbuff++;
    }
    while (*sbuff == '\n' || *sbuff == '\r') {
      sbuff++;
    }
    *ibufp = 0;
    xcsv_parse_style_line(ibuf);
  }
}

static void
xcsv_read_style(const char *fname)
{
  char *sbuff;
  gbfile *fp;

  xcsv_file_init();

  fp = gbfopen(fname, "rb", MYNAME);
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
xcsv_read_internal_style(const char *style_buf)
{
  xcsv_file_init();
  xcsv_file.is_internal = 1;

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
xcsv_setup_internal_style(const char *style_buf)
{
  xcsv_file_init();
  xcsv_destroy_style();
  xcsv_file.is_internal = !!style_buf;
  intstylebuf = style_buf;
}


static void
xcsv_rd_init(const char *fname)
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
      warning(MYNAME " attempt to read %s as a track or route, but this format only supports waypoints on read.  Reading as waypoints instead.\n", fname);
    }
  }

  xcsv_file.xcsvfp = gbfopen(fname, "r", MYNAME);
  xcsv_file.gps_datum = GPS_Lookup_Datum_Index(opt_datum);
  is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", opt_datum);
}

static void
xcsv_rd_deinit(void)
{
  gbfclose(xcsv_file.xcsvfp);

  xcsv_destroy_style();
}

static void
xcsv_wr_init(const char *fname)
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

  xcsv_file.xcsvfp = gbfopen(fname, "w", MYNAME);
  xcsv_file.fname = (char *)fname;

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

    setshort_badchars(xcsv_file.mkshort_handle, xcsv_file.badchars);

  }
  xcsv_file.gps_datum = GPS_Lookup_Datum_Index(opt_datum);
  is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", opt_datum);
}

static void
xcsv_wr_position_init(const char *fname)
{
  xcsv_wr_init(fname);
}

static void
xcsv_wr_deinit(void)
{
  gbfclose(xcsv_file.xcsvfp);

  xcsv_destroy_style();
}

static void
xcsv_wr_position_deinit(void)
{
  xcsv_wr_deinit();
}


static void
xcsv_wr_position(waypoint *wpt)
{
  /* Tweak incoming name if we don't have a fix */
  switch (wpt->fix) {
  case fix_none:
    if (wpt->shortname) {
      xfree(wpt->shortname);
    }
    wpt->shortname = xstrdup("ESTIMATED Position");
    break;
  default:
    break;
  }

  waypt_add(wpt);
  xcsv_data_write();
  waypt_del(wpt);

  gbfflush(xcsv_file.xcsvfp);
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
  NULL,
  xcsv_args,
  CET_CHARSET_ASCII, 0,	/* CET-REVIEW */
  { NULL, NULL, NULL, xcsv_wr_position_init, xcsv_wr_position, xcsv_wr_position_deinit }

};
#else
void xcsv_read_internal_style(const char *style_buf) {}
void xcsv_setup_internal_style(const char *style_buf) {}
#endif //CSVFMTS_ENABLED
