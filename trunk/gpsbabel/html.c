/*
    Output only format for Human Readable formats.

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
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


#include "defs.h"
#include "jeeps/gpsmath.h"
#include <ctype.h>

static gbfile *file_out;
static short_handle mkshort_handle;

static char *stylesheet = NULL;
static char *html_encrypt = NULL;
static char *includelogs = NULL;
static char *degformat = NULL;
static char *altunits = NULL;

#define MYNAME "HTML"

static
arglist_t html_args[] = {
  {
    "stylesheet", &stylesheet,
    "Path to HTML style sheet", NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "encrypt", &html_encrypt,
    "Encrypt hints using ROT13", NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "logs", &includelogs,
    "Include groundspeak logs if present", NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "degformat", &degformat,
    "Degrees output as 'ddd', 'dmm'(default) or 'dms'", "dmm", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "altunits", &altunits,
    "Units for altitude (f)eet or (m)etres", "m", ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};



static void
wr_init(const char *fname)
{
  file_out = gbfopen(fname, "w", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}

static void
html_disp(const waypoint *wpt)
{
  char tbuf[1024];
  char *cout;
  time_t tm = wpt->creation_time;
  gbint32 utmz;
  double utme, utmn;
  char utmzc;
  fs_xml *fs_gpx = NULL;


  GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                           &utme, &utmn, &utmz, &utmzc);

  if (tm == 0) {
    tm = time(NULL);
  }
  strftime(tbuf, sizeof(tbuf), "%d-%b-%Y", localtime(&tm));


  gbfprintf(file_out, "\n<a name=\"%s\"><hr></a>\n", wpt->shortname);
  gbfprintf(file_out, "<table width=\"100%%\">\n");
  gbfprintf(file_out, "<tr><td><p class=\"gpsbabelwaypoint\">%s - ",(global_opts.synthesize_shortnames) ? mkshort_from_wpt(mkshort_handle, wpt) : wpt->shortname);
  cout = pretty_deg_format(wpt->latitude, wpt->longitude, degformat[2], " ", 1);
  gbfprintf(file_out, "%s (%d%c %6.0f %7.0f)", cout, utmz, utmzc, utme, utmn);
  xfree(cout);
  if (wpt->altitude != unknown_alt) {
    gbfprintf(file_out, " alt:%d", (int)((altunits[0]=='f')?METERS_TO_FEET(wpt->altitude):wpt->altitude));
  }
  gbfprintf(file_out, "<br>\n");
  if (strcmp(wpt->description, wpt->shortname)) {
    if (wpt->url) {
      char *d = html_entitize(wpt->description);
      gbfprintf(file_out, "<a href=\"%s\">%s</a>", wpt->url, d);
      xfree(d);
    } else {
      gbfprintf(file_out, "%s", wpt->description);
    }
    if (wpt->gc_data->placer) {
      gbfprintf(file_out, " by %s", wpt->gc_data->placer);
    }
  }
  gbfprintf(file_out, "</p></td>\n");

  gbfprintf(file_out, "<td align=\"right\">");
  if (wpt->gc_data->terr) {
    gbfprintf(file_out, "<p class=\"gpsbabelcacheinfo\">%d%s / %d%s<br>\n",
              (int)(wpt->gc_data->diff / 10), (wpt->gc_data->diff%10)?"&frac12;":"",
              (int)(wpt->gc_data->terr / 10), (wpt->gc_data->terr%10)?"&frac12;":"");
    gbfprintf(file_out, "%s / %s</p>",
              gs_get_cachetype(wpt->gc_data->type),
              gs_get_container(wpt->gc_data->container));
  }
  gbfprintf(file_out, "</td></tr>\n");


  gbfprintf(file_out, "<tr><td colspan=\"2\">");
  if (wpt->gc_data->desc_short.utfstring) {
    char *tmpstr = strip_nastyhtml(wpt->gc_data->desc_short.utfstring);
    gbfprintf(file_out, "<p class=\"gpsbabeldescshort\">%s</p>\n", tmpstr);
    xfree(tmpstr);
  }
  if (wpt->gc_data->desc_long.utfstring) {
    char *tmpstr = strip_nastyhtml(wpt->gc_data->desc_long.utfstring);
    gbfprintf(file_out, "<p class=\"gpsbabeldesclong\">%s</p>\n", tmpstr);
    xfree(tmpstr);
  }
  if (wpt->gc_data->hint) {
    char *hint = NULL;
    if (html_encrypt) {
      hint = rot13(wpt->gc_data->hint);
    } else {
      hint = xstrdup(wpt->gc_data->hint);
    }
    gbfprintf(file_out, "<p class=\"gpsbabelhint\"><strong>Hint:</strong> %s</p>\n", hint);
    xfree(hint);
  } else if (wpt->notes && (!wpt->description || strcmp(wpt->notes,wpt->description))) {
    gbfprintf(file_out, "<p class=\"gpsbabelnotes\">%s</p>\n", wpt->notes);
  }

  fs_gpx = NULL;
  if (includelogs) {
    fs_gpx = (fs_xml *)fs_chain_find(wpt->fs, FS_GPX);
  }

  if (fs_gpx && fs_gpx->tag) {
    xml_tag *root = fs_gpx->tag;
    xml_tag *curlog = NULL;
    xml_tag *logpart = NULL;
    curlog = xml_findfirst(root, "groundspeak:log");
    while (curlog) {
      time_t logtime = 0;
      struct tm *logtm = NULL;
      gbfprintf(file_out, "<p class=\"gpsbabellog\">\n");

      logpart = xml_findfirst(curlog, "groundspeak:type");
      if (logpart) {
        gbfprintf(file_out, "<span class=\"gpsbabellogtype\">%s</span> by ", logpart->cdata);
      }

      logpart = xml_findfirst(curlog, "groundspeak:finder");
      if (logpart) {
        char *f = html_entitize(logpart->cdata);
        gbfprintf(file_out, "<span class=\"gpsbabellogfinder\">%s</span> on ", f);
        xfree(f);
      }

      logpart = xml_findfirst(curlog, "groundspeak:date");
      if (logpart) {
        logtime = xml_parse_time(logpart->cdata, NULL);
        logtm = localtime(&logtime);
        if (logtm) {
          gbfprintf(file_out,
                    "<span class=\"gpsbabellogdate\">%04d-%02d-%02d</span><br>\n",
                    logtm->tm_year+1900,
                    logtm->tm_mon+1,
                    logtm->tm_mday);
        }
      }

      logpart = xml_findfirst(curlog, "groundspeak:log_wpt");
      if (logpart) {
        char *coordstr = NULL;
        float lat = 0;
        float lon = 0;
        coordstr = xml_attribute(logpart, "lat");
        if (coordstr) {
          lat = atof(coordstr);
        }
        coordstr = xml_attribute(logpart, "lon");
        if (coordstr) {
          lon = atof(coordstr);
        }
        coordstr = pretty_deg_format(lat, lon, degformat[2], " ", 1);
        gbfprintf(file_out,
                  "<span class=\"gpsbabellogcoords\">%s</span><br>\n",
                  coordstr);
        xfree(coordstr);
      }

      logpart = xml_findfirst(curlog, "groundspeak:text");
      if (logpart) {
        char *encstr = NULL;
        char *s = NULL;
        char *t = NULL;
        int encoded = 0;
        encstr = xml_attribute(logpart, "encoded");
        encoded = (encstr[0] != 'F');

        if (html_encrypt && encoded) {
          s = rot13(logpart->cdata);
        } else {
          s = xstrdup(logpart->cdata);
        }

        t = html_entitize(s);
        gbfprintf(file_out, "%s", t);
        xfree(t);
        xfree(s);
      }

      gbfprintf(file_out, "</p>\n");
      curlog = xml_findnext(root, curlog, "groundspeak:log");
    }
  }
  gbfprintf(file_out, "</td></tr></table>\n");
}

static void
html_index(const waypoint *wpt)
{
  char *sn = html_entitize(wpt->shortname);
  char *d = html_entitize(wpt->description);

  gbfprintf(file_out, "<a href=\"#%s\">%s - %s</a><br>\n", sn, sn, d);

  xfree(sn);
  xfree(d);
}

static void
data_write(void)
{
  setshort_length(mkshort_handle, 6);

  gbfprintf(file_out, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n");
  gbfprintf(file_out, "<html>\n");
  gbfprintf(file_out, "<head>\n");
  gbfprintf(file_out, " <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">\n");
  gbfprintf(file_out, " <meta name=\"Generator\" content=\"GPSBabel %s\">\n", gpsbabel_version);
  gbfprintf(file_out, " <title>GPSBabel HTML Output</title>\n");
  if (stylesheet) {
    gbfprintf(file_out, " <link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">\n", stylesheet);
  } else {
    gbfprintf(file_out, " <style>\n");
    gbfprintf(file_out, "  p.gpsbabelwaypoint { font-size: 120%%; font-weight: bold }\n");
    gbfprintf(file_out, " </style>\n");
  }
  gbfprintf(file_out, "</head>\n");
  gbfprintf(file_out, "<body>\n");

  gbfprintf(file_out, "<p class=\"index\">\n");
  waypt_disp_all(html_index);
  gbfprintf(file_out, "</p>\n");

  waypt_disp_all(html_disp);

  gbfprintf(file_out, "</body>");
  gbfprintf(file_out, "</html>");

}


ff_vecs_t html_vecs = {
  ff_type_file,
  { ff_cap_write, ff_cap_none, ff_cap_none },
  NULL,
  wr_init,
  NULL,
  wr_deinit,
  NULL,
  data_write,
  NULL,
  html_args,
  CET_CHARSET_UTF8, 0	/* CET-REVIEW */
};
