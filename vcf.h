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
#ifndef VCF_H_INCLUDED_
#define VCF_H_INCLUDED_

#include <QString>     // for QString
#include <QVector>     // for QVector

#include "defs.h"
#include "format.h"    // for Format
#include "gbfile.h"    // for gbfile
#include "geocache.h"  // for Geocache


class VcfFormat : public Format
{
public:
  using Format::Format;

  QVector<arglist_t>* get_args() override
  {
    return &vcf_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {ff_cap_write, ff_cap_none, ff_cap_none};
  }

  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:

  /* Member Functions */

  void vcf_print_utf(const Geocache::UtfString* s);
  void vcf_print(const char* s);
  void vcf_print(const QString& s);
  void vcf_disp(const Waypoint* wpt);

  /* Data Members */

  gbfile* file_out{};

  OptionCString vcf_encrypt;

  QVector<arglist_t> vcf_args = {
    {
      "encrypt", &vcf_encrypt,
      "Encrypt hints using ROT13", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

};
#endif // VCF_H_INCLUDED_
