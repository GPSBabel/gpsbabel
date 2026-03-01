/*
    Output only format for Vcard format, VCF

    Copyright (C) 2005 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "vcf.h"

#include <QChar>                  // for QChar
#include <QIODevice>              // for QIODevice
#include <QString>                // for QString
#include <QStringLiteral>         // for qMakeStringPrivate, QStringLiteral
#include <QTextStream>            // for QTextStream
#include <Qt>                     // for CaseSensitivity

#include <cmath>                  // for fabs
#include <cstdlib>                // for abs

#include "defs.h"                 // for Waypoint, UrlLink, rot13, waypt_disp_all
#include "geocache.h"             // for Geocache
#include "src/core/textstream.h"  // for TextStream


void
VcfFormat::wr_init(const QString& fname)
{
  file_out = new gpsbabel::TextStream;
  file_out->open(fname, QIODevice::WriteOnly | QIODeviceBase::Text);
}

void
VcfFormat::wr_deinit()
{
  file_out->close();
  delete file_out;
  file_out = nullptr;
}

/*
 * Print a possibly empty input string, replacing newlines with escaped
 * newlines as we go.
 */
void
VcfFormat::vcf_print_utf(const Geocache::UtfString& s)
{
  if (s.utf_string.isEmpty()) {
    return;
  }

  QString stripped_html = s.strip_html();

  stripped_html.replace("\n", "\\n", Qt::CaseInsensitive);
  stripped_html.replace("<p>", "\\n", Qt::CaseInsensitive);
  stripped_html.replace(";", "\\;");
  stripped_html.replace(",", "\\,");

  *file_out << stripped_html;
}

void
VcfFormat::vcf_print(const QString& s)
{
  if (s.isEmpty()) {
    return;
  }

  QString cleaned = s;
  cleaned.replace("\n", "\\n", Qt::CaseInsensitive);
  cleaned.replace(",", "\\,");

  *file_out << cleaned;
}

void
VcfFormat::vcf_disp(const Waypoint* wpt)
{
  int lonint = abs(static_cast<int>(wpt->longitude));
  int latint = abs(static_cast<int>(wpt->latitude));

  *file_out << "BEGIN:VCARD\nVERSION:3.0\n";
  *file_out << "N:" << wpt->description << ";" << wpt->shortname << ";;;\n";
  *file_out << QStringLiteral("ADR:%1%2 %3 %4%5 %6\n")
            .arg(wpt->latitude < 0 ? 'S' : 'N').arg(abs(latint)).arg(60.0 * (fabs(wpt->latitude) - latint), 6, 'f', 3, u'0')
            .arg(wpt->longitude < 0 ? 'W' : 'E').arg(abs(lonint)).arg(60.0 * (fabs(wpt->longitude) - lonint), 6, 'f', 3, u'0');

  if (wpt->HasUrlLink()) {
    const UrlLink& link = wpt->GetUrlLink();
    *file_out << "URL:" << link.url_ << "\n";
  }

  *file_out << "NOTE:";
  vcf_print_utf(wpt->gc_data->desc_short);
  *file_out << "\\n";
  vcf_print_utf(wpt->gc_data->desc_long);
  *file_out << R"(\n\nHINT:\n)";
  if (vcf_encrypt) {
    vcf_print(rot13(wpt->gc_data->hint));
  } else {
    vcf_print(wpt->gc_data->hint);
  }

  *file_out << "\nEND:VCARD\n";
}

void VcfFormat::write()
{
  auto vcf_disp_lambda = [this](const Waypoint* waypointp)->void {
    vcf_disp(waypointp);
  };
  waypt_disp_all(vcf_disp_lambda);
}
