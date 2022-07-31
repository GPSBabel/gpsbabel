/*
    Output only format for Human Readable formats.

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "html.h"

#include <QString>                 // for QString, operator!=
#include <Qt>                      // for CaseInsensitive

#include <cstdint>                 // for int32_t
#include <ctime>                   // for localtime, time_t, tm

#include "defs.h"                  // for Waypoint, xfree, geocache_data, html_entitize, CSTR, pretty_deg_format, rot13, strip_nastyhtml, waypt_disp_all, utf_string, METERS_TO_FEET, gpsbabel_testmode, gs_get_cachetype, gs_get_container, mkshort_del_handle, mkshort_from_wpt, mkshort_new_handle
#include "formspec.h"              // for FormatSpecificDataList, kFsGpx
#include "gbfile.h"                // for gbfprintf, gbfclose, gbfopen, gbfputs
#include "jeeps/gpsmath.h"         // for GPS_Math_WGS84_To_UTM_EN
#include "src/core/datetime.h"     // for DateTime
#include "src/core/xmltag.h"       // for xml_findfirst, xml_tag, xml_attribute, fs_xml, xml_findnext


#define MYNAME "HTML"

void
HtmlFormat::wr_init(const QString& fname)
{
  file_out = gbfopen(fname, "w", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

void
HtmlFormat::wr_deinit()
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}

void
HtmlFormat::html_disp(const Waypoint* wpt) const
{
  int32_t utmz;
  double utme;
  double utmn;
  char utmzc;


  GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                           &utme, &utmn, &utmz, &utmzc);

  gbfprintf(file_out, "\n<a name=\"%s\"><hr></a>\n", CSTR(wpt->shortname));
  gbfprintf(file_out, "<table width=\"100%%\">\n");
  gbfprintf(file_out, "<tr><td><p class=\"gpsbabelwaypoint\">%s - ",(global_opts.synthesize_shortnames) ? CSTR(mkshort_from_wpt(mkshort_handle, wpt)) : CSTR(wpt->shortname));
  gbfprintf(file_out, "%s (%d%c %6.0f %7.0f)",
            CSTR(pretty_deg_format(wpt->latitude, wpt->longitude, degformat[2], " ", true)),
            utmz, utmzc, utme, utmn);
  if (wpt->altitude != unknown_alt) {
    gbfprintf(file_out, " alt:%d", (int)((altunits[0]=='f')?METERS_TO_FEET(wpt->altitude):wpt->altitude));
  }
  gbfprintf(file_out, "<br>\n");
  if (wpt->description != wpt->shortname) {
    if (wpt->HasUrlLink()) {
      gbfputs(QStringLiteral("<a href=\"%1\">%2</a>")
              .arg(wpt->GetUrlLink().url_, html_entitize(wpt->description)),
              file_out);
    } else {
      gbfprintf(file_out, "%s", CSTR(wpt->description));
    }
    if (!wpt->gc_data->placer.isEmpty()) {
      gbfprintf(file_out, " by %s", CSTR(wpt->gc_data->placer));
    }
  }
  gbfprintf(file_out, "</p></td>\n");

  gbfprintf(file_out, "<td align=\"right\">");
  if (wpt->gc_data->terr) {
    gbfprintf(file_out, "<p class=\"gpsbabelcacheinfo\">%d%s / %d%s<br>\n",
              (int)(wpt->gc_data->diff / 10), (wpt->gc_data->diff%10) ? "&frac12;" : "",
              (int)(wpt->gc_data->terr / 10), (wpt->gc_data->terr%10) ? "&frac12;" : "");
    gbfputs(QStringLiteral("%1 / %2</p>")
            .arg(gs_get_cachetype(wpt->gc_data->type),
                 gs_get_container(wpt->gc_data->container)),
            file_out);
  }
  gbfprintf(file_out, "</td></tr>\n");


  gbfprintf(file_out, "<tr><td colspan=\"2\">");
  if (!wpt->gc_data->desc_short.utfstring.isEmpty()) {
    char* tmpstr = strip_nastyhtml(wpt->gc_data->desc_short.utfstring);
    gbfprintf(file_out, "<p class=\"gpsbabeldescshort\">%s</p>\n", tmpstr);
    xfree(tmpstr);
  }
  if (!wpt->gc_data->desc_long.utfstring.isEmpty()) {
    char* tmpstr = strip_nastyhtml(wpt->gc_data->desc_long.utfstring);
    gbfprintf(file_out, "<p class=\"gpsbabeldesclong\">%s</p>\n", tmpstr);
    xfree(tmpstr);
  }
  if (!wpt->gc_data->hint.isEmpty()) {
    QString hint;
    if (html_encrypt) {
      hint = rot13(wpt->gc_data->hint);
    } else {
      hint = wpt->gc_data->hint;
    }
    gbfprintf(file_out, "<p class=\"gpsbabelhint\"><strong>Hint:</strong> %s</p>\n", CSTR(hint));
  } else if (!wpt->notes.isEmpty() && (wpt->description.isEmpty() || wpt->notes != wpt->description)) {
    gbfprintf(file_out, "<p class=\"gpsbabelnotes\">%s</p>\n", CSTR(wpt->notes));
  }

  if (includelogs) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(wpt->fs.FsChainFind(kFsGpx));
    if (fs_gpx && fs_gpx->tag) {
      xml_tag* root = fs_gpx->tag;
      xml_tag* curlog = xml_findfirst(root, "groundspeak:log");
      while (curlog) {
        time_t logtime = 0;
        gbfprintf(file_out, "<p class=\"gpsbabellog\">\n");

        xml_tag* logpart = xml_findfirst(curlog, "groundspeak:type");
        if (logpart) {
          gbfprintf(file_out, "<span class=\"gpsbabellogtype\">%s</span> by ", CSTR(logpart->cdata));
        }

        logpart = xml_findfirst(curlog, "groundspeak:finder");
        if (logpart) {
          gbfputs(QStringLiteral("<span class=\"gpsbabellogfinder\">%1</span> on ")
                  .arg(html_entitize(logpart->cdata)),
                  file_out);
        }

        logpart = xml_findfirst(curlog, "groundspeak:date");
        if (logpart) {
          logtime = xml_parse_time(logpart->cdata).toTime_t();
          struct tm* logtm = localtime(&logtime);
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
          double lat = xml_attribute(logpart->attributes, "lat").toDouble();
          double lon = xml_attribute(logpart->attributes, "lon").toDouble();
          gbfprintf(file_out,
                    "<span class=\"gpsbabellogcoords\">%s</span><br>\n",
                    CSTR(pretty_deg_format(lat, lon, degformat[2], " ", true)));
        }

        logpart = xml_findfirst(curlog, "groundspeak:text");
        if (logpart) {
          QString encstr = xml_attribute(logpart->attributes, "encoded");
          bool encoded = !encstr.startsWith('F', Qt::CaseInsensitive);

          QString s;
          if (html_encrypt && encoded) {
            s = rot13(logpart->cdata);
          } else {
            s = logpart->cdata;
          }

          gbfputs(html_entitize(s), file_out);
        }

        gbfprintf(file_out, "</p>\n");
        curlog = xml_findnext(root, curlog, "groundspeak:log");
      }
    }
  }
  gbfprintf(file_out, "</td></tr></table>\n");
}

void
HtmlFormat::html_index(const Waypoint* wpt) const
{
  gbfputs(QStringLiteral("<a href=\"#%1\">%1 - %2</a><br>\n")
          .arg(html_entitize(wpt->shortname), html_entitize(wpt->description)),
          file_out);
}

void
HtmlFormat::write()
{
  setshort_length(mkshort_handle, 6);

  gbfprintf(file_out, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n");
  gbfprintf(file_out, "<html>\n");
  gbfprintf(file_out, "<head>\n");
  gbfprintf(file_out, " <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">\n");

  // Don't write this line when running test suite.  Actually, we should
  // probably not write this line at all...
  if (!gpsbabel_testmode()) {
    gbfprintf(file_out, " <meta name=\"Generator\" content=\"GPSBabel %s\">\n", gpsbabel_version);
  }
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

  auto html_index_lambda = [this](const Waypoint* waypointp)->void {
    html_index(waypointp);
  };

  waypt_disp_all(html_index_lambda);
  gbfprintf(file_out, "</p>\n");

  auto html_disp_lambda = [this](const Waypoint* waypointp)->void {
    html_disp(waypointp);
  };
  waypt_disp_all(html_disp_lambda);

  gbfprintf(file_out, "</body>");
  gbfprintf(file_out, "</html>");

}
