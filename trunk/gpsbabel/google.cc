/*
    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

#include <QtCore/QFile>
#include <QtCore/QXmlStreamAttributes>

#include "defs.h"
#include "xmlgeneric.h"

static char* encoded_points = NULL;
static char* encoded_levels = NULL;
static char* script = NULL;
static route_head** routehead;
static int* routecount;
static short_handle desc_handle;
static const char* rd_fname;

static int serial = 0;

#define MYNAME "google"

static xg_callback      goog_points, goog_levels, goog_poly_e, goog_script;
static xg_callback	goog_segment_s, goog_segment, goog_td_s, goog_td_b;
static xg_callback	goog_td_e;

static
xg_tag_mapping google_map[] = {
  { goog_points,  cb_cdata,       "/page/directions/polyline/points" },
  { goog_levels,  cb_cdata,       "/page/directions/polyline/levels" },
  { goog_poly_e,  cb_end,         "/page/directions/polyline" },
  { goog_script,  cb_cdata,       "/html/head/script" },
  { goog_segment_s, cb_start,      "/page/directions/segments/segment" },
  { goog_segment, cb_cdata,      "/page/directions/segments/segment" },
  { goog_td_s,    cb_start,      "/div/table/tr/td" },
  { goog_td_s,    cb_start,      "/div/div/table/tr/td" },
  { goog_td_b,      cb_cdata,      "/div/table/tr/td/b" },
  { goog_td_b,      cb_cdata,      "/div/div/table/tr/td/b" },
  { goog_td_e,    cb_end,        "/div/table/tr/td" },
  { goog_td_e,    cb_end,        "/div/div/table/tr/td" },
  { NULL, (xg_cb_type)0,              NULL }
};

void goog_script(xg_string args, const QXmlStreamAttributes* unused)
{
#if NEW_STRINGS
  if (true) {
#else
  if (args) {
#endif
    if (script) {
      script = xstrappend(script, CSTRc(args));
    } else {
      script = xstrdup(args);
    }
  }
}

void goog_points(xg_string args, const QXmlStreamAttributes* unused)
{
#if NEW_STRINGS
  if (true) {
#else
  if (args) {
#endif
    if (encoded_points) {
      encoded_points = xstrappend(encoded_points, CSTRc(args));
    } else {
      encoded_points = xstrdup(args);
    }
  }
}

void goog_levels(xg_string args, const QXmlStreamAttributes* unused)
{
#if NEW_STRINGS
  if (true) {
#else
  if (args) {
#endif
    if (encoded_levels) {
      encoded_levels = xstrappend(encoded_levels, CSTRc(args));
    } else {
      encoded_levels = xstrdup(args);
    }
  }
}

static char goog_segname[7];
static char* goog_realname = NULL;
static int goog_segroute = 0;

/*
 * The segments contain an index into the points array.  We use that
 * index to find the waypoint and insert a better name for it.
 */
void goog_segment_s(xg_string args, const QXmlStreamAttributes* attrv)
{
  QStringRef ptidx = attrv->value("pointIndex");
  if (!ptidx.isEmpty()) {
    snprintf(goog_segname, sizeof(goog_segname), "\\%5.5x",
             ptidx.toString().toUInt());
  }
}

void goog_segment(xg_string args, const QXmlStreamAttributes* unused)
{
  waypoint* wpt_tmp;

  wpt_tmp = route_find_waypt_by_name(routehead[goog_segroute], goog_segname);
  if (wpt_tmp) {
#if NEW_STRINGS
    wpt_tmp->shortname = QString();
#else
    xfree(wpt_tmp->shortname);
#endif
    wpt_tmp->shortname = mkshort(desc_handle, CSTRc(args));
    wpt_tmp->description = xstrdup(args);
  }
}

void goog_td_s(xg_string args, const QXmlStreamAttributes* attrv)
{
  bool isdesc = false, isseg = false;
  QStringRef aclass = attrv->value("class");
  QStringRef id = attrv->value("id");

  if (aclass.isEmpty() || id.isEmpty()) {
    return;
  }

  isdesc = (aclass == "desc");
  isseg = (aclass == "dirsegtext");

  if (isdesc) {
    QStringRef subid(id.string(), id.position() + 6, id.length() - 6);

    goog_segroute = 0;
    snprintf(goog_segname, sizeof(goog_segname), "\\%5.5x",
             subid.toString().toUInt());
  } else if (isseg) {
    QString idstr = id.toString();
    int first_us;

    goog_segroute = 0;

    first_us = idstr.indexOf("_");
    if (idstr.indexOf("_", first_us + 1) != -1) {
      goog_segroute = idstr.mid(first_us + 1).toUInt();
    }

    snprintf(goog_segname, sizeof(goog_segname), "\\%5.5x",
             idstr.mid(idstr.lastIndexOf("_") + 1).toUInt() +
             routecount[goog_segroute]);
  }
}

void goog_td_b(xg_string args, const QXmlStreamAttributes* unused)
{
  if (goog_segname[0] == '\\' && !strchr(CSTRc(args), '\xa0')) {
    if (goog_realname) {
      xfree(goog_realname);
      goog_realname = NULL;
    }
    goog_realname = (char*) xmalloc(strlen(CSTRc(args))+1);
    strcpy(goog_realname, CSTRc(args));
  }
}
void goog_td_e(xg_string args, const QXmlStreamAttributes* unused)
{
  if (goog_segname[0] == '\\' && goog_realname) {
    goog_segment(goog_realname, NULL/*unused*/);
  }
  goog_segname[0] = '\0';
  if (goog_realname) {
    xfree(goog_realname);
    goog_realname = NULL;
  }
}

static long decode_goog64(char** str)
{
  long result = 0;
  unsigned char c = 0;
  unsigned char shift = 0;

  if (!(**str)) {
    return 0;
  }

  do {
    c = (unsigned char)(*(*str)++)-'?';
    result |= (c & 31)<<shift;
    shift += 5;
  } while (c & ~31);

  if (result & 1) {
    result = ~result;
  }
  return result/2;
}

void goog_poly_e(xg_string args, const QXmlStreamAttributes* unused)
{
  long lat = 0;
  long lon = 0;
  long level = 0;
  long level1 = -9999;
  long level2 = -9999;
  char* str = encoded_points;
  char* lstr = encoded_levels;

  routehead[goog_segroute] = route_head_alloc();
  route_add_head(routehead[goog_segroute]);
  routecount[goog_segroute] = serial;

  while (str && *str) {
    lat += decode_goog64(&str);
    lon += decode_goog64(&str);

    level = -1;
    level2 = level1;
    if (lstr && *lstr) {
      level1 = -decode_goog64(&lstr);
    } else {
      level1 = -9999;
    }
    level = (level1<level2)?level1:level2;

    /* level of -9999 happens for endpoints */
    if (level == -9999) {
      level = 99999;
    }

    {
      waypoint* wpt_tmp = waypt_new();
      wpt_tmp->latitude = lat / 100000.0;
      wpt_tmp->longitude = lon / 100000.0;
      wpt_tmp->route_priority=level;
// NEW_STRINGS FIXME(robertlipe): this is broken somehow there should be no need
// to overallocate like this, but it's needed ot get an1 to not scribble
// on itself.
      wpt_tmp->shortname = (char*) xmalloc(7000);
#if NEW_STRINGS
      wpt_tmp->shortname = QString().sprintf( "\\%5.5x", serial++);
#else
      sprintf(wpt_tmp->shortname, "\\%5.5x", serial++);
#endif
      route_add_wpt(routehead[goog_segroute], wpt_tmp);
    }
  }

}

static void
google_rd_init(const char* fname)
{
  rd_fname = fname;

  desc_handle = mkshort_new_handle();
  setshort_length(desc_handle, 12);

  xml_init(fname, google_map, NULL);
}

static void
goog_read_file(void)
{
  QFile src(QString::fromUtf8(rd_fname));

  src.open(QIODevice::ReadOnly);

  QTextStream tstr(&src);
  tstr.setCodec("ISO-8859-1");

  QString preamble = tstr.read(256);
  QString needle("http-equiv=\"content-type\" content=\"text/html; charset=");

  if (!preamble.contains(needle)) {
    // let QXmlStreamReader do its best if we can't figure it out...
    xml_read();
    return;
  }

  int idx = preamble.indexOf(needle);
  QString charset = preamble.mid(idx + needle.length());

  int endq = charset.indexOf('"');
  if (endq != -1) {
    charset = charset.left(endq);
  }

  QString wholefile;
  if (charset == "ISO-8859-1") {
    wholefile = preamble + tstr.readAll();
  } else {
    tstr.reset();
    tstr.seek(0);
    tstr.setCodec(CSTR(charset));
    wholefile = tstr.readAll();
  }

  xml_readunicode(wholefile);
}

static void
google_read(void)
{
  routehead = (route_head**)xmalloc(sizeof(route_head*));
  routecount = (int*)xmalloc(sizeof(int));
  goog_segroute = 0;

  goog_read_file();

  xfree(routehead);
  xfree(routecount);
  routehead = NULL;
  routecount = NULL;

  if (encoded_points) {
    xfree(encoded_points);
    encoded_points = NULL;
  }
  if (encoded_levels) {
    xfree(encoded_levels);
    encoded_levels = NULL;
  }
  if (script) {
    char* xml = strchr(script, '\'');
    char* dict = strstr(script, "({");

    char* end = NULL;

    if (xml && (!dict || (xml < dict))) {
      routehead = (route_head**)xmalloc(sizeof(route_head*));
      routecount = (int*)xmalloc(sizeof(int));
      goog_segroute = 0;
      xml++;
      end = strchr(xml+1, '\'');
      if (end) {
        *end = '\0';
        xml_deinit();
        xml_init(NULL, google_map, NULL);
        xml_readstring(xml);
        if (encoded_points) {
          xfree(encoded_points);
          encoded_points = NULL;
        }
        if (encoded_levels) {
          xfree(encoded_levels);
          encoded_levels = NULL;
        }
      }
    } else if (dict) {
      char qc = '\'';
      int ofs = 9;
      int panelofs = 8;
      int count = 0;
      char* tmp = NULL;
      char* start = NULL;

      char* panel = strstr(dict, "panel: '");
      encoded_points = strstr(dict, "points: '");
      encoded_levels = strstr(dict, "levels: '");
      if (!encoded_points) {
        ofs = 10;
        qc = '"';
        encoded_points = strstr(dict, "\"points\":\"");
        encoded_levels = strstr(dict, "\"levels\":\"");
        if (!encoded_points) {
          encoded_points = strstr(dict, "points:\"");
          encoded_levels = strstr(dict, "levels:\"");
          ofs = 8;
        }
      }

      if (!panel) {
        panel = strstr(dict, "panel:\"");
        panelofs = 7;
      }
      tmp=encoded_points;
      while (tmp) {
        if (qc == '"') {
          char* tmp1 = strstr(tmp, "\"points\":\"");
          if (!tmp1) {
            tmp1 = strstr(tmp, "points:\"");
          }
          tmp = tmp1;
        } else {
          tmp = strstr(tmp, "points: '");
        }
        if (tmp) {
          count++;
          tmp++;
        }
      }
      routehead = (route_head**)xmalloc(sizeof(route_head*)*count);
      routecount = (int*)xmalloc(sizeof(int)*count);
      goog_segroute = 0;

      do {

        if (encoded_points && encoded_levels) {
          encoded_points += ofs;
          encoded_levels += ofs;
          end = strchr(encoded_points, qc);
          if (end) {
            *end = '\0';
            end = encoded_points;
            while ((end = strstr(end, "\\\\"))) {
              memmove(end, end+1, strlen(end)+1);
              end++;
            }
            end = strchr(encoded_levels, qc);
            if (end) {
              start = end;
              *end = '\0';
              end = encoded_levels;
              while ((end = strstr(end, "\\\\"))) {
                memmove(end, end+1, strlen(end)+1);
                end++;
              }
              goog_poly_e(NULL, NULL);

              goog_segroute++;
              start++;
              {
                encoded_points = strstr(start, "points: '");
                encoded_levels = strstr(start, "levels: '");
              }
              if (!encoded_points) {
                encoded_points = strstr(start, "\"points\":\"");
                encoded_levels = strstr(start, "\"levels\":\"");
              }
              if (!encoded_points) {
                encoded_points = strstr(start, "points:\"");
                encoded_levels = strstr(start, "levels:\"");
              }
            }
          }
        }
      } while (start && encoded_points && encoded_levels);
      if (panel) {
        panel += panelofs;
        end = strstr(panel, "/table><div class=\\\"legal");
        if (!end) {
          end = strstr(panel, "/table\\x3e\\x3cdiv class=\\\"legal");
        }
        if (!end) {
          end = strstr(panel, "/table><div class=\\042legal");
        }
        if (!end) {
          end = strstr(panel, "/table\\u003e\\u003cdiv id=\\\"mrDragRouteTip\\\"");
        }
        if (end) {
          strcpy(end,"/table></div>");
        }
        if (!end) {
          end = strstr(panel, "/div><div class=\\042legal");
          if (end) {
            strcpy(end, "/div></div>");
          }
        }
        if (end) {
          char* to = panel;
          char* from = panel;
          while (*from) {
            if (!strncmp(from, "\\\"", 2)) {
              *to++ = '"';
              from += 2;
              if (*(to-2) != '=') {
                *to++ = ' ';
              }
            } else if (!strncmp(from, "\\042", 4)) {
              *to++ = '"';
              from += 4;

              if (*(to-2) != '=') {
                *to++ = ' ';
              }
            } else if (!strncmp(from, "\\u0026utm", 9)) {
              strcpy(to, "&amp;utm");
              to += 8;
              from += 9;
            } else if (!strncmp(from, "\\u0026", 6)) {
              *to++='&';
              from += 6;
            } else if (!strncmp(from, "\\u003c", 6)) {
              *to++='<';
              from += 6;
            } else if (!strncmp(from, "\\u003e", 6)) {
              *to++='>';
              from += 6;
            } else if (!strncmp(from, "\\x", 2)) {
              unsigned int c;
              sscanf(from+2, "%2x", &c);
              *to++ = (char)c;
              from += 4;
            } else if (!strncmp(from, "\\'", 2)) {
              *to++ = '\'';
              from += 2;
            } else if (!strncmp(from, " nowrap ", 8)) {
              *to++ = ' ';
              from += 8;
            } else if (!strncmp(from, "tr style=\\\"display:none", 23)) {
              if (strcmp(to-5, "/tr><")) {
                /* broken 6-26-07 missing </tr> that apparently doesn't bother browsers */
                strcpy(to, "/tr><");
                to += 5;
              }
              *to++ = *from++;
            } else {
              *to++ = *from++;
            }
          }
          *to = '\0';

#if 0
          {
            FILE* foo = fopen("foo.xml", "w");
            fprintf(foo, "<!DOCTYPE foo [%s]>\n", xhtml_entities);
            fwrite(panel, sizeof(char), strlen(panel), foo);
            fclose(foo);
          }
#endif

          xml_deinit();
          xml_init(NULL, google_map, NULL);
          xml_readprefixstring("<!DOCTYPE foo [");
          xml_readprefixstring(xhtml_entities);
          xml_readprefixstring("]>");
          xml_readstring(panel);
        }
      }
    }
    xfree(script);
    xfree(routehead);
    xfree(routecount);
    script = NULL;
  }

  /*
   * 'Tis better to leak than crash when we are merging and
   * don't see an 'end' in the first file.  This feels a bit
   * like plastering over a deeper problem...
   *
   */
  if (encoded_points) {
    encoded_points = NULL;
  }
  if (encoded_levels) {
    encoded_levels = NULL;
  }
}

static void
google_rd_deinit(void)
{
  xml_deinit();
  mkshort_del_handle(&desc_handle);
}

ff_vecs_t google_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none},
  google_rd_init,
  NULL,
  google_rd_deinit,
  NULL,
  google_read,
  NULL,
  NULL,
  NULL,
  CET_CHARSET_UTF8, 1	/* CET-REVIEW */
};
