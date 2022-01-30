/*
    Output only format for Human Readable formats.

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "text.h"

#include <QString>                 // for QString, operator!=
#include <Qt>                      // for CaseInsensitive

#include <cstdint>                 // for int32_t
#include <ctime>                   // for localtime, time_t, tm

#include "defs.h"                  // for Waypoint, xfree, geocache_data, pretty_deg_format, rot13, strip_html, xasprintf, CSTR, METERS_TO_FEET, gs_get_cachetype, gs_get_container, mkshort_del_handle, mkshort_from_wpt, mkshort_new_handle, setshort_length, waypt_disp_all, xml_parse_time, utf_string
#include "formspec.h"              // for FormatSpecificDataList, kFsGpx
#include "gbfile.h"                // for gbfprintf, gbfputs, gbfclose, gbfopen
#include "jeeps/gpsmath.h"         // for GPS_Math_WGS84_To_UTM_EN
#include "src/core/datetime.h"     // for DateTime
#include "src/core/xmltag.h"       // for xml_findfirst, xml_tag, xml_attribute, fs_xml, xml_findnext


#define MYNAME "TEXT"

void
TextFormat::wr_init(const QString& fname)
{
  waypoint_count = 0;
  output_name = fname;
  if (!split_output) {
    file_out = gbfopen(fname, "w", MYNAME);
  }
  mkshort_handle = mkshort_new_handle();
}

void
TextFormat::wr_deinit()
{
  if (!split_output) {
    gbfclose(file_out);
  }
  mkshort_del_handle(&mkshort_handle);
  output_name.clear();
}

void
TextFormat::text_disp(const Waypoint* wpt)
{
  int32_t utmz;
  double utme, utmn;
  char utmzc;
  char* tmpout2;
  char* altout;

  waypoint_count++;

  if (split_output) {
    QString thisfname(output_name);
    thisfname += QString::number(waypoint_count);
    file_out = gbfopen(thisfname, "w", MYNAME);
  }

  GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                           &utme, &utmn, &utmz, &utmzc);
  char* tmpout1 = pretty_deg_format(wpt->latitude, wpt->longitude, degformat[2], " ", 0);
  if (wpt->altitude != unknown_alt) {
    xasprintf(&altout, " alt:%d", (int)((altunits[0]=='f')?METERS_TO_FEET(wpt->altitude):wpt->altitude));
  } else {
    altout = (char*) "";
  }
  xasprintf(&tmpout2, "%s (%d%c %6.0f %7.0f)%s", tmpout1, utmz, utmzc, utme, utmn, altout);
  gbfprintf(file_out, "%-16s  %59s\n",
            (global_opts.synthesize_shortnames) ? CSTR(mkshort_from_wpt(mkshort_handle, wpt)) : CSTR(wpt->shortname),
            tmpout2);
  xfree(tmpout2);
  xfree(tmpout1);
  if (altout[0]) {
    xfree(altout);
  }

  if (wpt->description != wpt->shortname) {
    gbfputs(wpt->description, file_out);
    if (!wpt->gc_data->placer.isEmpty()) {
      gbfputs(" by ", file_out);
      gbfputs(wpt->gc_data->placer, file_out);
    }
  }
  if (wpt->gc_data->terr) {
    gbfprintf(file_out, " - %s / %s - (%d%s / %d%s)\n",
              gs_get_cachetype(wpt->gc_data->type), gs_get_container(wpt->gc_data->container),
              (int)(wpt->gc_data->diff / 10), (wpt->gc_data->diff%10)?".5":"",
              (int)(wpt->gc_data->terr / 10), (wpt->gc_data->terr%10)?".5":"");
    if (!wpt->gc_data->desc_short.utfstring.isEmpty()) {
      char* stripped_html = strip_html(&wpt->gc_data->desc_short);
      gbfprintf(file_out, "\n%s\n", stripped_html);
      xfree(stripped_html);
    }
    if (!wpt->gc_data->desc_long.utfstring.isEmpty()) {
      char* stripped_html = strip_html(&wpt->gc_data->desc_long);
      gbfprintf(file_out, "\n%s\n", stripped_html);
      xfree(stripped_html);
    }
    if (!wpt->gc_data->hint.isEmpty()) {
      QString hint;
      if (txt_encrypt) {
        hint = rot13(wpt->gc_data->hint);
      } else {
        hint = wpt->gc_data->hint;
      }
      gbfprintf(file_out, "\nHint: %s\n", CSTR(hint));
    }
  } else if (!wpt->notes.isEmpty() && (wpt->description.isEmpty() || wpt->notes != wpt->description)) {
    gbfputs("\n", file_out);
    gbfputs(wpt->notes, file_out);
    gbfputs("\n", file_out);
  }

  if (includelogs) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(wpt->fs.FsChainFind(kFsGpx));
    if (fs_gpx && fs_gpx->tag) {
      xml_tag* root = fs_gpx->tag;
      xml_tag* curlog = xml_findfirst(root, "groundspeak:log");
      while (curlog) {
        time_t logtime = 0;
        gbfprintf(file_out, "\n");

        xml_tag* logpart = xml_findfirst(curlog, "groundspeak:type");
        if (logpart) {
          gbfputs(logpart->cdata, file_out);
          gbfputs(" by ", file_out);
        }

        logpart = xml_findfirst(curlog, "groundspeak:finder");
        if (logpart) {
          gbfputs(logpart->cdata, file_out);
          gbfputs(" on ", file_out);
        }

        logpart = xml_findfirst(curlog, "groundspeak:date");
        if (logpart) {
          logtime = xml_parse_time(logpart->cdata).toTime_t();
          struct tm* logtm = localtime(&logtime);
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
          double lat = xml_attribute(logpart->attributes, "lat").toDouble();
          double lon = xml_attribute(logpart->attributes, "lon").toDouble();
          char* coordstr = pretty_deg_format(lat, lon, degformat[2], " ", 0);
          gbfprintf(file_out, "%s\n", coordstr);
          xfree(coordstr);
        }

        logpart = xml_findfirst(curlog, "groundspeak:text");
        if (logpart) {
          QString encstr = xml_attribute(logpart->attributes, "encoded");
          bool encoded = !encstr.startsWith('F', Qt::CaseInsensitive);

          QString s;
          if (txt_encrypt && encoded) {
            s = rot13(logpart->cdata);
          } else {
            s = logpart->cdata;
          }

          gbfputs(s, file_out);
        }

        gbfprintf(file_out, "\n");
        curlog = xml_findnext(root, curlog, "groundspeak:log");
      }
    }
  }
  if (! suppresssep) {
    gbfprintf(file_out, "\n-----------------------------------------------------------------------------\n");
  } else {
    gbfprintf(file_out, "\n");
  }

  if (split_output) {
    gbfclose(file_out);
    file_out = nullptr;
  }
}

void
TextFormat::write()
{
  if (! suppresssep && !split_output) {
    gbfprintf(file_out, "-----------------------------------------------------------------------------\n");
  }
  setshort_length(mkshort_handle, 6);
  auto text_disp_lambda = [this](const Waypoint* waypointp)->void {
    text_disp(waypointp);
  };
  waypt_disp_all(text_disp_lambda);
}
