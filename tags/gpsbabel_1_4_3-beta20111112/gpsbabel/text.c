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

static char *suppresssep = NULL;
static char *txt_encrypt = NULL;
static char *includelogs = NULL;
static char *degformat = NULL;
static char *altunits = NULL;
static char *split_output = NULL;
static int waypoint_count;
static char *output_name;

#define MYNAME "TEXT"

static
arglist_t text_args[] = {
  {
    "nosep", &suppresssep,
    "Suppress separator lines between waypoints",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "encrypt", &txt_encrypt,
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
  {
    "splitoutput", &split_output,
    "Write each waypoint in a separate file", NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },

  ARG_TERMINATOR
};



static void
wr_init(const char *fname)
{
  waypoint_count = 0;
  output_name = xstrdup(fname);
  if (!split_output) {
    file_out = gbfopen(fname, "w", MYNAME);
  }
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
  if (!split_output) {
    gbfclose(file_out);
  }
  mkshort_del_handle(&mkshort_handle);
  xfree(output_name);
}

static void
text_disp(const waypoint *wpt)
{
  int latint, lonint;
  char tbuf[1024];
  time_t tm = wpt->creation_time;
  gbint32 utmz;
  double utme, utmn;
  char utmzc;
  char *tmpout1, *tmpout2;
  char *altout;
  fs_xml *fs_gpx;

  waypoint_count++;

  if (split_output) {
    char *thisfname;
    xasprintf(&thisfname, "%s%d", output_name, waypoint_count);
    file_out = gbfopen(thisfname, "w", MYNAME);
  }

  lonint = abs((int) wpt->longitude);
  latint = abs((int) wpt->latitude);

  GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                           &utme, &utmn, &utmz, &utmzc);

  if (tm == 0) {
    tm = time(NULL);
  }
  strftime(tbuf, sizeof(tbuf), "%d-%b-%Y", localtime(&tm));

  tmpout1 = pretty_deg_format(wpt->latitude, wpt->longitude, degformat[2], " ", 0);
  if (wpt->altitude != unknown_alt) {
    xasprintf(&altout, " alt:%d", (int)((altunits[0]=='f')?METERS_TO_FEET(wpt->altitude):wpt->altitude));
  } else {
    altout = "";
  }
  xasprintf(&tmpout2, "%s (%d%c %6.0f %7.0f)%s", tmpout1, utmz, utmzc, utme, utmn, altout);
  gbfprintf(file_out, "%-16s  %59s\n",
            (global_opts.synthesize_shortnames) ? mkshort_from_wpt(mkshort_handle, wpt) : wpt->shortname,
            tmpout2);
  xfree(tmpout2);
  xfree(tmpout1);
  if (altout[0]) {
    xfree(altout);
  }


  if (strcmp(wpt->description, wpt->shortname)) {
    gbfprintf(file_out, "%s", wpt->description);
    if (wpt->gc_data->placer) {
      gbfprintf(file_out, " by %s", wpt->gc_data->placer);
    }
  }
  if (wpt->gc_data->terr) {
    gbfprintf(file_out, " - %s / %s - (%d%s / %d%s)\n",
              gs_get_cachetype(wpt->gc_data->type), gs_get_container(wpt->gc_data->container),
              (int)(wpt->gc_data->diff / 10), (wpt->gc_data->diff%10)?".5":"",
              (int)(wpt->gc_data->terr / 10), (wpt->gc_data->terr%10)?".5":"");
    if (wpt->gc_data->desc_short.utfstring) {
      char *stripped_html = strip_html(&wpt->gc_data->desc_short);
      gbfprintf(file_out, "\n%s\n", stripped_html);
      xfree(stripped_html);
    }
    if (wpt->gc_data->desc_long.utfstring) {
      char *stripped_html = strip_html(&wpt->gc_data->desc_long);
      gbfprintf(file_out, "\n%s\n", stripped_html);
      xfree(stripped_html);
    }
    if (wpt->gc_data->hint) {
      char *hint = NULL;
      if (txt_encrypt) {
        hint = rot13(wpt->gc_data->hint);
      } else {
        hint = xstrdup(wpt->gc_data->hint);
      }
      gbfprintf(file_out, "\nHint: %s\n", hint);
      xfree(hint);
    }
  } else if (wpt->notes && (!wpt->description || strcmp(wpt->notes,wpt->description))) {
    gbfprintf(file_out, "\n%s\n", wpt->notes);
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
      gbfprintf(file_out, "\n");

      logpart = xml_findfirst(curlog, "groundspeak:type");
      if (logpart) {
        gbfprintf(file_out, "%s by ", logpart->cdata);
      }

      logpart = xml_findfirst(curlog, "groundspeak:finder");
      if (logpart) {
        gbfprintf(file_out, "%s on ", logpart->cdata);
      }

      logpart = xml_findfirst(curlog, "groundspeak:date");
      if (logpart) {
        logtime = xml_parse_time(logpart->cdata, NULL);
        logtm = localtime(&logtime);
        if (logtm) {
          gbfprintf(file_out,
                    "%4.4d-%2.2d-%2.2d\n",
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
        coordstr = pretty_deg_format(lat, lon, degformat[2], " ", 0);
        gbfprintf(file_out, "%s\n", coordstr);
        xfree(coordstr);
      }

      logpart = xml_findfirst(curlog, "groundspeak:text");
      if (logpart) {
        char *encstr = NULL;
        char *s = NULL;
        int encoded = 0;
        encstr = xml_attribute(logpart, "encoded");
        encoded = (encstr[0] != 'F');

        if (txt_encrypt && encoded) {
          s = rot13(logpart->cdata);
        } else {
          s = xstrdup(logpart->cdata);
        }

        gbfprintf(file_out, "%s", s);
        xfree(s);
      }

      gbfprintf(file_out, "\n");
      curlog = xml_findnext(root, curlog, "groundspeak:log");
    }
  }
  if (! suppresssep) {
    gbfprintf(file_out, "\n-----------------------------------------------------------------------------\n");
  } else {
    gbfprintf(file_out, "\n");
  }

  if (split_output) {
    gbfclose(file_out);
    file_out = NULL;
  }
}

static void
data_write(void)
{
  if (! suppresssep && !split_output) {
    gbfprintf(file_out, "-----------------------------------------------------------------------------\n");
  }
  setshort_length(mkshort_handle, 6);
  waypt_disp_all(text_disp);
}


ff_vecs_t text_vecs = {
  ff_type_file,
  { ff_cap_write, ff_cap_none, ff_cap_none},
  NULL,
  wr_init,
  NULL,
  wr_deinit,
  NULL,
  data_write,
  NULL,
  text_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */

};
