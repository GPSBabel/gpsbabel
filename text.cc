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


#define MYNAME "TEXT"

void
TextFormat::wr_init(const QString& fname)
{
  waypoint_count = 0;
  output_name = fname;
  if (!split_output) {
    file_out = new gpsbabel::TextStream;
    file_out->open(fname, QIODevice::WriteOnly, MYNAME);
  }
  mkshort_handle = mkshort_new_handle();
}

void
TextFormat::wr_deinit()
{
  if (!split_output) {
    file_out->close();
    delete file_out;
    file_out = nullptr;
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

  waypoint_count++;

  if (split_output) {
    QString thisfname(output_name);
    thisfname += QString::number(waypoint_count);
    file_out = new gpsbabel::TextStream;
    file_out->open(thisfname, QIODevice::WriteOnly, MYNAME);
  }

  GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                           &utme, &utmn, &utmz, &utmzc);
  QString position = QStringLiteral("%1 (%2%3 %4 %5)")
                     .arg(pretty_deg_format(wpt->latitude, wpt->longitude, degformat[2], " ", false))
                     .arg(utmz)
                     .arg(utmzc)
                     .arg(utme, 6, 'f', 0)
                     .arg(utmn, 7, 'f', 0);
  if (wpt->altitude != unknown_alt) {
    position += QStringLiteral(" alt:%1").arg((int)((altunits[0]=='f') ? METERS_TO_FEET(wpt->altitude) : wpt->altitude));
  }
  QString sn = global_opts.synthesize_shortnames ? mkshort_from_wpt(mkshort_handle, wpt) : wpt->shortname;
  *file_out << sn.leftJustified(16) << "  " <<  position.rightJustified(59) << "\n";

  if (wpt->description != wpt->shortname) {
    *file_out << wpt->description;
    if (!wpt->gc_data->placer.isEmpty()) {
      *file_out << " by " << wpt->gc_data->placer;
    }
  }
  if (wpt->gc_data->terr) {
    *file_out << QStringLiteral(" - %1 / %2 - (%3%4 / %5%6)\n")
              .arg(gs_get_cachetype(wpt->gc_data->type),
                   gs_get_container(wpt->gc_data->container))
              .arg((int)(wpt->gc_data->diff / 10))
              .arg((wpt->gc_data->diff%10) ? ".5" : "")
              .arg((int)(wpt->gc_data->terr / 10))
              .arg((wpt->gc_data->terr%10) ? ".5" : "");
    if (!wpt->gc_data->desc_short.utfstring.isEmpty()) {
      *file_out << "\n" << strip_html(&wpt->gc_data->desc_short) << "\n";
    }
    if (!wpt->gc_data->desc_long.utfstring.isEmpty()) {
      *file_out << "\n" << strip_html(&wpt->gc_data->desc_long) << "\n";
    }
    if (!wpt->gc_data->hint.isEmpty()) {
      QString hint;
      if (txt_encrypt) {
        hint = rot13(wpt->gc_data->hint);
      } else {
        hint = wpt->gc_data->hint;
      }
      *file_out << "\nHint: " << hint << "\n";
    }
  } else if (!wpt->notes.isEmpty() && (wpt->description.isEmpty() || wpt->notes != wpt->description)) {
    *file_out << "\n" << wpt->notes << "\n";
  }

  if (includelogs) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(wpt->fs.FsChainFind(kFsGpx));
    if (fs_gpx && fs_gpx->tag) {
      xml_tag* root = fs_gpx->tag;
      xml_tag* curlog = xml_findfirst(root, "groundspeak:log");
      while (curlog) {
        time_t logtime = 0;
        *file_out << "\n";

        xml_tag* logpart = xml_findfirst(curlog, "groundspeak:type");
        if (logpart) {
          *file_out << logpart->cdata << " by ";
        }

        logpart = xml_findfirst(curlog, "groundspeak:finder");
        if (logpart) {
          *file_out << logpart->cdata << " on ";
        }

        logpart = xml_findfirst(curlog, "groundspeak:date");
        if (logpart) {
          logtime = xml_parse_time(logpart->cdata).toTime_t();
          struct tm* logtm = localtime(&logtime);
          if (logtm) {
            *file_out << QStringLiteral("%1-%2-%3\n")
                      .arg(logtm->tm_year+1900, 4, 10, QChar('0'))
                      .arg(logtm->tm_mon+1, 2, 10, QChar('0'))
                      .arg(logtm->tm_mday, 2, 10, QChar('0'));
          }
        }

        logpart = xml_findfirst(curlog, "groundspeak:log_wpt");
        if (logpart) {
          double lat = xml_attribute(logpart->attributes, "lat").toDouble();
          double lon = xml_attribute(logpart->attributes, "lon").toDouble();
          *file_out << pretty_deg_format(lat, lon, degformat[2], " ", false) << "\n";
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

          *file_out << s;
        }

        *file_out << "\n";
        curlog = xml_findnext(root, curlog, "groundspeak:log");
      }
    }
  }
  if (!suppresssep) {
    *file_out << "\n-----------------------------------------------------------------------------\n";
  } else {
    *file_out << "\n";
  }

  if (split_output) {
    file_out->close();
    delete file_out;
    file_out = nullptr;
  }
}

void
TextFormat::write()
{
  if (!suppresssep && !split_output) {
    *file_out << "-----------------------------------------------------------------------------\n";
  }
  setshort_length(mkshort_handle, 6);
  auto text_disp_lambda = [this](const Waypoint* waypointp)->void {
    text_disp(waypointp);
  };
  waypt_disp_all(text_disp_lambda);
}
