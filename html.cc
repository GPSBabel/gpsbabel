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

#include <QChar>                   // for QChar
#include <QIODevice>               // for QIODevice, QIODevice::WriteOnly
#include <QString>                 // for QString, operator!=
#include <QTextStream>             // for QTextStream
#include <Qt>                      // for CaseInsensitive

#include <cstdint>                 // for int32_t
#include <ctime>                   // for localtime, time_t

#include "defs.h"
#include "formspec.h"              // for FormatSpecificDataList, kFsGpx
#include "jeeps/gpsmath.h"         // for GPS_Math_WGS84_To_UTM_EN
#include "src/core/datetime.h"     // for DateTime
#include "src/core/textstream.h"   // for TextStream
#include "src/core/xmltag.h"       // for xml_findfirst, xml_tag, xml_attribute, fs_xml, xml_findnext


#define MYNAME "HTML"

void
HtmlFormat::wr_init(const QString& fname)
{
  file_out = new gpsbabel::TextStream;
  file_out->open(fname, QIODevice::WriteOnly, MYNAME);
  mkshort_handle = mkshort_new_handle();
}

void
HtmlFormat::wr_deinit()
{
  file_out->close();
  delete file_out;
  file_out = nullptr;
  mkshort_del_handle(&mkshort_handle);
}

QString HtmlFormat::create_id(int sequence_number)
{
  // It's easier to create a legal fragment and identifer from scratch than
  // from user supplied text.
  return QStringLiteral("WPT%1").arg(sequence_number, 3, 10, QChar('0'));
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

  *file_out << "  <div id=\"" << create_id(waypoint_number) << "\"><hr>\n";
  *file_out << "    <table style=\"width:100%\">\n";
  QString sn = global_opts.synthesize_shortnames ? mkshort_from_wpt(mkshort_handle, wpt) : wpt->shortname;
  *file_out << "      <tr>\n";
  *file_out << "        <td>\n";
  *file_out << "          <p class=\"gpsbabelwaypoint\">" << sn << " - ";
  *file_out << QStringLiteral("%1 (%2%3 %4 %5)")
            .arg(pretty_deg_format(wpt->latitude, wpt->longitude, degformat[2], " ", true))
            .arg(utmz)
            .arg(utmzc)
            .arg(utme, 6, 'f', 0)
            .arg(utmn, 7, 'f', 0);
  if (wpt->altitude != unknown_alt) {
    *file_out << QStringLiteral(" alt:%1")
              .arg((int)((altunits[0]=='f') ? METERS_TO_FEET(wpt->altitude) : wpt->altitude));
  }
  *file_out << "<br>\n";
  if (wpt->description != wpt->shortname) {
    if (wpt->HasUrlLink()) {
      *file_out << "<a href=\"" << wpt->GetUrlLink().url_ << "\">"
                << wpt->description.toHtmlEscaped() << "</a>";
    } else {
      *file_out << wpt->description.toHtmlEscaped();
    }
    if (!wpt->gc_data->placer.isEmpty()) {
      *file_out << " by " << wpt->gc_data->placer;
    }
  }
  *file_out << "</p>\n";
  *file_out << "        </td>\n";

  *file_out << "        <td style=\"text-align:right\">\n";
  if (wpt->gc_data->terr) {
    *file_out << QStringLiteral("          <p class=\"gpsbabelcacheinfo\">%1%2 / %3%4<br>\n")
              .arg((int)(wpt->gc_data->diff / 10))
              .arg((wpt->gc_data->diff%10) ? "&frac12;" : "")
              .arg((int)(wpt->gc_data->terr / 10))
              .arg((wpt->gc_data->terr%10) ? "&frac12;" : "");
    *file_out << gs_get_cachetype(wpt->gc_data->type) << " / "
              << gs_get_container(wpt->gc_data->container) << "</p>\n";
  }
  *file_out << "        </td>\n";
  *file_out << "      </tr>\n";


  *file_out << "      <tr>\n";
  *file_out << "        <td colspan=\"2\">\n";
  if (!wpt->gc_data->desc_short.utfstring.isEmpty()) {
    *file_out << "          <div><p class=\"gpsbabeldescshort\">"
              << strip_nastyhtml(wpt->gc_data->desc_short.utfstring) << "</div>\n";
  }
  if (!wpt->gc_data->desc_long.utfstring.isEmpty()) {
    *file_out << "          <div><p class=\"gpsbabeldesclong\">"
              << strip_nastyhtml(wpt->gc_data->desc_long.utfstring) << "</div>\n";
  }
  if (!wpt->gc_data->hint.isEmpty()) {
    QString hint;
    if (html_encrypt) {
      hint = rot13(wpt->gc_data->hint);
    } else {
      hint = wpt->gc_data->hint;
    }
    *file_out << "          <p class=\"gpsbabelhint\"><strong>Hint:</strong> "
              << hint << "</p>\n";
  } else if (!wpt->notes.isEmpty() && (wpt->description.isEmpty() || wpt->notes != wpt->description)) {
    *file_out << "          <p class=\"gpsbabelnotes\">" << wpt->notes << "</p>\n";
  }

  if (includelogs) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(wpt->fs.FsChainFind(kFsGpx));
    if (fs_gpx && fs_gpx->tag) {
      xml_tag* root = fs_gpx->tag;
      xml_tag* curlog = xml_findfirst(root, "groundspeak:log");
      while (curlog) {
        time_t logtime = 0;
        *file_out << "          <p class=\"gpsbabellog\">\n";

        xml_tag* logpart = xml_findfirst(curlog, "groundspeak:type");
        if (logpart) {
          *file_out << "<span class=\"gpsbabellogtype\">"
                    << logpart->cdata << "</span> by ";
        }

        logpart = xml_findfirst(curlog, "groundspeak:finder");
        if (logpart) {
          *file_out << "<span class=\"gpsbabellogfinder\">"
                    << logpart->cdata.toHtmlEscaped() << "</span> on ";
        }

        logpart = xml_findfirst(curlog, "groundspeak:date");
        if (logpart) {
          logtime = xml_parse_time(logpart->cdata).toTime_t();
          struct tm* logtm = localtime(&logtime);
          if (logtm) {
            *file_out << QStringLiteral("<span class=\"gpsbabellogdate\">%1-%2-%3</span><br>\n")
                      .arg(logtm->tm_year+1900, 4, 10, QChar('0'))
                      .arg(logtm->tm_mon+1, 2, 10, QChar('0'))
                      .arg(logtm->tm_mday, 2, 10, QChar('0'));
          }
        }

        logpart = xml_findfirst(curlog, "groundspeak:log_wpt");
        if (logpart) {
          double lat = xml_attribute(logpart->attributes, "lat").toDouble();
          double lon = xml_attribute(logpart->attributes, "lon").toDouble();
          *file_out << "<span class=\"gpsbabellogcoords\">"
                    << pretty_deg_format(lat, lon, degformat[2], " ", true) << "</span><br>\n";
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

          *file_out << s.toHtmlEscaped();
        }

        *file_out << "</p>\n";
        curlog = xml_findnext(root, curlog, "groundspeak:log");
      }
    }
  }
  *file_out << "        </td>\n";
  *file_out << "      </tr>\n";
  *file_out << "    </table>\n";
  *file_out << "  </div>\n";
}

void
HtmlFormat::html_index(const Waypoint* wpt) const
{
  *file_out << "    <a href=\"#" << create_id(waypoint_number) << "\">"
            << wpt->shortname.toHtmlEscaped() << " - " << wpt->description.toHtmlEscaped()
            << "</a><br>\n";
}

void
HtmlFormat::write()
{
  setshort_length(mkshort_handle, 6);

  *file_out << "<!DOCTYPE html>\n";
  *file_out << "<html>\n";
  *file_out << "<head>\n";
  *file_out << "  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n";

  // Don't write this line when running test suite.  Actually, we should
  // probably not write this line at all...
  if (!gpsbabel_testmode()) {
    *file_out << "  <meta name=\"Generator\" content=\"GPSBabel "
              << gpsbabel_version << "\">\n";
  }
  *file_out << "  <title>GPSBabel HTML Output</title>\n";
  if (stylesheet) {
    *file_out << "  <link rel=\"stylesheet\" type=\"text/css\" href=\""
              << stylesheet << "\">\n";
  } else {
    *file_out << "  <style>\n";
    *file_out << "    p.gpsbabelwaypoint { font-size: 120%; font-weight: bold }\n";
    *file_out << "  </style>\n";
  }
  *file_out << "</head>\n";
  *file_out << "<body>\n";

  *file_out << "  <p class=\"index\">\n";

  auto html_index_lambda = [this](const Waypoint* waypointp)->void {
    waypoint_number++;
    html_index(waypointp);
  };

  waypoint_number = 0;
  waypt_disp_all(html_index_lambda);
  *file_out << "  </p>\n";

  auto html_disp_lambda = [this](const Waypoint* waypointp)->void {
    waypoint_number++;
    html_disp(waypointp);
  };
  waypoint_number = 0;
  waypt_disp_all(html_disp_lambda);

  *file_out << "</body>\n";
  *file_out << "</html>\n";

}
