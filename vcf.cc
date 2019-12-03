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

#include "defs.h"
#include "jeeps/gpsmath.h"
#include <cctype>
#include <cmath>
#include <cstdlib>

static gbfile* file_out;
static short_handle mkshort_handle;

static char* vcf_encrypt = nullptr;

#define MYNAME "VCF"

static
QVector<arglist_t> vcf_args = {
  {
    "encrypt", &vcf_encrypt,
    "Encrypt hints using ROT13", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
};

static void
wr_init(const QString& fname)
{
  file_out = gbfopen(fname, "w", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit()
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}

/*
 * Print a possibly empty input string, replacing newlines with escaped
 * newlines as we go.
 */
static void
vcf_print_utf(const utf_string* s)
{
  if (!s) {
    return;
  }

  char* stripped_html = strip_html(s);
  char* p = gstrsub(stripped_html, "\n", "\\n");
  char* p2 = gstrsub(p, "<p>", "\\n");
  char* p3 = gstrsub(p2, ";", "\\;");
  gbfputs(p3, file_out);
  xfree(p);
  xfree(p2);
  xfree(p3);
  xfree(stripped_html);
}

static void
vcf_print(const char* s)
{
  if (!s) {
    return;
  }

  char* p = gstrsub(s, "\n", "\\n");
  gbfputs(p, file_out);
  xfree(p);
}

static void
vcf_print(const QString& s)
{
  vcf_print(CSTR(s));
}

static void
vcf_disp(const Waypoint* wpt)
{
  int lonint = abs((int) wpt->longitude);
  int latint = abs((int) wpt->latitude);

  gbfprintf(file_out, "BEGIN:VCARD\nVERSION:3.0\n");
  gbfprintf(file_out, "N:%s;%s;;;\n", CSTRc(wpt->description),CSTRc(wpt->shortname));
  gbfprintf(file_out, "ADR:%c%d %06.3f %c%d %06.3f\n", wpt->latitude < 0 ? 'S' : 'N',  abs(latint), 60.0 * (fabs(wpt->latitude) - latint), wpt->longitude < 0 ? 'W' : 'E', abs(lonint), 60.0 * (fabs(wpt->longitude) - lonint));

  if (wpt->HasUrlLink()) {
    UrlLink link = wpt->GetUrlLink();
    gbfprintf(file_out, "URL:%s\n", CSTR(link.url_));
  }

  gbfprintf(file_out, "NOTE:");
  vcf_print_utf(&wpt->gc_data->desc_short);
  gbfprintf(file_out, "\\n");
  vcf_print_utf(&wpt->gc_data->desc_long);
  gbfprintf(file_out, "\\n\\nHINT:\\n");
  if (vcf_encrypt) {
    QString s = rot13(wpt->gc_data->hint);
    vcf_print(s);
  } else {
    vcf_print(CSTR(wpt->gc_data->hint));
  }

  gbfprintf(file_out, "\nEND:VCARD\n");
}

static void
data_write()
{
  setshort_length(mkshort_handle, 6);
  waypt_disp_all(vcf_disp);
}


ff_vecs_t vcf_vecs = {
  ff_type_file,
  { ff_cap_write, ff_cap_none, ff_cap_none},
  nullptr,
  wr_init,
  nullptr,
  wr_deinit,
  nullptr,
  data_write,
  nullptr,
  &vcf_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
