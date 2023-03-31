/*
 * FAI/IGC data format translation.
 *
 * Refer to Appendix 1 of
 * http://www.fai.org:81/gliding/gnss/tech_spec_gnss.asp for the
 * specification of the IGC data format.  This translation code was
 * written when the latest amendment list for the specification was AL6.
 *
 * Copyright (C) 2004 Chris Jones
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
#ifndef IGC_H_INCLUDED_
#define IGC_H_INCLUDED_

#include <optional>              // for optional

#include <QByteArray>            // for QByteArray
#include <QDateTime>             // for QDateTime
#include <QList>                 // for QList<>::const_iterator
#include <QString>               // for QString, operator+, QStringLiteral
#include <QVector>               // for QVector

#include "defs.h"
#include "format.h"
#include "gbfile.h"              // for gbfprintf, gbfclose, gbfopen, gbfputs, gbfgetstr, gbfile
#include "src/core/datetime.h"   // for DateTime
#include "kml.h"                 // for wp_field

class IgcFormat : public Format
{
public:
  enum igc_ext_type_t {
    ext_rec_enl = 1,  // Engine Noise Level
    ext_rec_tas = 2,  // True Airspeed
    ext_rec_vat = 3,  // Total Energy Variometer
    ext_rec_oat = 4,  // Outside Air Temperature
    ext_rec_trt = 5,  // True Track
    ext_rec_gsp = 6,  // Ground Speed
    ext_rec_fxa = 7,  // Fix Accuracy
    ext_rec_gfo = 8,  // G Force? (Found in LX Eos recorders present in LS-4)
    ext_rec_siu = 9,  // Satllites In Use
    ext_rec_acz = 10  // Z Acceleration

  };

  IgcFormat::igc_ext_type_t get_ext_type(QString type) {
    IgcFormat::igc_ext_type_t def_type;
    if (type == "ENL") { def_type = IgcFormat::igc_ext_type_t::ext_rec_enl; }
    else if (type == "TAS") { def_type = IgcFormat::igc_ext_type_t::ext_rec_tas; }
    else if (type == "VAT") { def_type = IgcFormat::igc_ext_type_t::ext_rec_vat; }
    else if (type == "OAT") { def_type = IgcFormat::igc_ext_type_t::ext_rec_oat; }
    else if (type == "TRT") { def_type = IgcFormat::igc_ext_type_t::ext_rec_trt; }
    else if (type == "GSP") { def_type = IgcFormat::igc_ext_type_t::ext_rec_gsp; }
    else if (type == "FXA") { def_type = IgcFormat::igc_ext_type_t::ext_rec_fxa; }
    else if (type == "SIU") { def_type = IgcFormat::igc_ext_type_t::ext_rec_siu; }
    else if (type == "ACZ") { def_type = IgcFormat::igc_ext_type_t::ext_rec_acz; }
    else if (type == "GFO") { def_type = IgcFormat::igc_ext_type_t::ext_rec_gfo; }
    return def_type;
  }

  QVector<arglist_t>* get_args() override
  {
    return &igc_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), (ff_cap)(ff_cap_read | ff_cap_write) };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:

  /* Types */

  /*
   * IGC record types.
   * These appear as the first char in each record.
   */
  enum igc_rec_type_t {
    rec_manuf_id = 'A',		// FR manufacturer and identification
    rec_fix = 'B',		// Fix
    rec_task = 'C',		// Task/declaration
    rec_diff_gps = 'D',		// Differential GPS
    rec_event = 'E',		// Event
    rec_constel = 'F',		// Constellation
    rec_security = 'G',		// Security
    rec_header = 'H',		// File header
    rec_fix_defn = 'I',		// List of extension data included at end of each fix (B) record
    rec_extn_defn = 'J',	// List of data included in each extension (K) record
    rec_extn_data = 'K',	// Extension data
    rec_log_book = 'L',		// Logbook/comments

    // M..Z are spare

    rec_none = 0,		// No record
    rec_bad = 1,		// Bad record
  };

  class TaskRecordReader
  {
  public:
    void igc_task_rec(const char*);

  private:
    enum class state_t { id, takeoff, start, turnpoint, finish, landing };

    unsigned int num_tp{0};
    unsigned int tp_ct{0};
    route_head* rte_head{nullptr};
    QDateTime creation;
    state_t state{state_t::id};
  };

  class Interpolater
  {
  public:
    double interpolate_alt(const route_head*, const gpsbabel::DateTime&);

  private:
    std::optional<WaypointList::const_iterator> prev_wpt;
    std::optional<WaypointList::const_iterator> curr_wpt;
  };

  /* Constants */

  static constexpr int kMaxRecLen = 79;		// Includes null terminator and CR/LF
  static constexpr int kMaxDescLen = 1024;
  static constexpr char kPresTrkName[] = "PRESALTTRK";
  static constexpr char kGNSSTrkName[] = "GNSSALTTRK";

  /* Member Functions */

  static unsigned char coords_match(double, double, double, double);
  igc_rec_type_t get_record(char**);
  void detect_pres_track(const route_head*);
  void detect_gnss_track(const route_head*);
  void detect_other_track(const route_head*, int& max_waypt_ct);
  void get_tracks(const route_head**, const route_head**);
  QByteArray latlon2str(const Waypoint*);
  QByteArray date2str(const gpsbabel::DateTime&) const;
  QByteArray tod2str(const gpsbabel::DateTime&) const;
  void wr_header();
  void wr_task_wpt_name(const Waypoint*, const char*);
  void wr_task_hdr(const route_head*, unsigned int task_num);
  void wr_task_wpt(const Waypoint*);
  void wr_task_tlr(const route_head*);
  void wr_tasks();
  void wr_fix_record(const Waypoint*, int, int);
  int correlate_tracks(const route_head*, const route_head*) const;
  void wr_track();

  /* Data Members */

  gbfile* file_in{};
  gbfile* file_out{};
  char manufacturer[4] {};
  const route_head* head{};
  char* timeadj = nullptr;

  QVector<arglist_t> igc_args = {
    {
      "timeadj", &timeadj,
      "(integer sec or 'auto') Barograph to GPS time diff",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    }
  };
};
  /*
   * Extra data
   *
   * This includes any data recorded as part of an extension,
   * which will be defined in the IGC file's I record,
   * and present in individual B records.
  */

struct igc_defn_t {
  QString name;
  short start;
  short end;
};

struct igc_metadata {
  std::optional<igc_defn_t> enl;
  std::optional<igc_defn_t> tas;
  std::optional<igc_defn_t> vat;
  std::optional<igc_defn_t> oat;
  std::optional<igc_defn_t> trt;
  std::optional<igc_defn_t> gsp;
  std::optional<igc_defn_t> fxa;
  std::optional<igc_defn_t> siu;
  std::optional<igc_defn_t> acz;
  std::optional<igc_defn_t> gfo;

  bool has_data() const {
    for (const auto& member : {enl, tas, vat, oat, trt, gsp, fxa, siu, acz, gfo}) {
      if (member.has_value()) {
        return true;
      }
    }
    return false;
  }

  igc_defn_t set_defn(IgcFormat::igc_ext_type_t type, const QString& name, short start, short end) {
    switch (type) {
      case IgcFormat::igc_ext_type_t::ext_rec_enl:
        enl = igc_defn_t{name, start, end};
        return *enl;
      case IgcFormat::igc_ext_type_t::ext_rec_tas:
        tas = igc_defn_t{name, start, end};
        return *tas;
      case IgcFormat::igc_ext_type_t::ext_rec_vat:
        vat = igc_defn_t{name, start, end};
        return *vat;
      case IgcFormat::igc_ext_type_t::ext_rec_oat:
        oat = igc_defn_t{name, start, end};
        return *oat;
      case IgcFormat::igc_ext_type_t::ext_rec_trt:
        trt = igc_defn_t{name, start, end};
        return *trt;
      case IgcFormat::igc_ext_type_t::ext_rec_gsp:
        gsp = igc_defn_t{name, start, end};
        return *gsp;
      case IgcFormat::igc_ext_type_t::ext_rec_fxa:
        fxa = igc_defn_t{name, start, end};
        return *fxa;
      case IgcFormat::igc_ext_type_t::ext_rec_siu:
        siu = igc_defn_t{name, start, end};
        return *siu;
      case IgcFormat::igc_ext_type_t::ext_rec_acz:
        acz = igc_defn_t{name, start, end};
        return *acz;
      case IgcFormat::igc_ext_type_t::ext_rec_gfo:
        gfo = igc_defn_t{name, start, end};
        return *gfo;
      default:
        throw std::runtime_error("Invalid igc_defn_t type");
    }
  }

  igc_defn_t get_defn(IgcFormat::igc_ext_type_t defn_type) const {
    switch (defn_type) {
      case IgcFormat::igc_ext_type_t::ext_rec_enl:
        return *enl;
      case IgcFormat::igc_ext_type_t::ext_rec_tas:
        return *tas;
      case IgcFormat::igc_ext_type_t::ext_rec_vat:
        return *vat;
      case IgcFormat::igc_ext_type_t::ext_rec_oat:
        return *oat;
      case IgcFormat::igc_ext_type_t::ext_rec_trt:
        return *trt;
      case IgcFormat::igc_ext_type_t::ext_rec_gsp:
        return *gsp;
      case IgcFormat::igc_ext_type_t::ext_rec_fxa:
        return *fxa;
      case IgcFormat::igc_ext_type_t::ext_rec_siu:
        return *siu;
      case IgcFormat::igc_ext_type_t::ext_rec_acz:
        return *acz;
      case IgcFormat::igc_ext_type_t::ext_rec_gfo:
        return *gfo;
      default:
        throw std::runtime_error("Invalid igc_defn_t type");
    }
  }
};

struct igc_fsdata : public FormatSpecificData {
  igc_fsdata() : FormatSpecificData(kFsIGC) {}

  igc_fsdata* clone() const override
  {
    return new igc_fsdata(*this);
  }

  std::optional<short> enl; // Engine Noise Level
  std::optional<short> tas; // True Airspeed
  std::optional<short> vat; // Compensated variometer (total energy)
  std::optional<short> oat; // Outside Air Temperature
  std::optional<short> trt; // True Track
  std::optional<short> gsp; // Ground Speed
  std::optional<short> fxa; // Fix Accuracy
  std::optional<short> siu; // Satellites In Use
  std::optional<short> acz; // Z Acceleration
  std::optional<short> gfo; // G Force?

  bool set_value(IgcFormat::igc_ext_type_t type, short value) {
    bool success = true;
    switch (type) {
      case IgcFormat::igc_ext_type_t::ext_rec_enl:
        enl = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_tas:
        tas = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_vat:
        vat = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_oat:
        oat = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_trt:
        trt = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_gsp:
        gsp = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_fxa:
        fxa = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_siu:
        siu = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_acz:
        acz = value; break;
      case IgcFormat::igc_ext_type_t::ext_rec_gfo:
        gfo = value; break;
      default:
        success = false;
    }
    return success;
  }

  std::optional<short> get_value(IgcFormat::igc_ext_type_t defn_type) const {
    std::optional<short> ret;
    switch (defn_type) {
      case IgcFormat::igc_ext_type_t::ext_rec_enl:
        ret = enl;
      case IgcFormat::igc_ext_type_t::ext_rec_tas:
        ret = tas;
      case IgcFormat::igc_ext_type_t::ext_rec_vat:
        ret = vat;
      case IgcFormat::igc_ext_type_t::ext_rec_oat:
        ret = oat;
      case IgcFormat::igc_ext_type_t::ext_rec_trt:
        ret = trt;
      case IgcFormat::igc_ext_type_t::ext_rec_gsp:
        ret = gsp;
      case IgcFormat::igc_ext_type_t::ext_rec_fxa:
        ret = fxa;
      case IgcFormat::igc_ext_type_t::ext_rec_siu:
        ret = siu;
      case IgcFormat::igc_ext_type_t::ext_rec_acz:
        ret = acz;
      case IgcFormat::igc_ext_type_t::ext_rec_gfo:
        ret = gfo;
      default:
        throw std::runtime_error("Invalid igc_ext_type");
        break;
    }
    return ret;
  }
  std::optional<short> get_value(KmlFormat::wp_field defn_type) const {
    std::optional<short> ret;
    switch (defn_type) {
      case KmlFormat::wp_field::fld_igc_enl:
        ret = enl; break;
      case KmlFormat::wp_field::fld_igc_tas:
        ret = tas; break;
      case KmlFormat::wp_field::fld_igc_vat:
        ret = vat; break;
      case KmlFormat::wp_field::fld_igc_oat:
        ret = oat; break;
      case KmlFormat::wp_field::fld_igc_trt:
        ret = trt; break;
      case KmlFormat::wp_field::fld_igc_gsp:
        ret = gsp; break;
      case KmlFormat::wp_field::fld_igc_fxa:
        ret = fxa; break;
      case KmlFormat::wp_field::fld_igc_siu:
        ret = siu; break;
      case KmlFormat::wp_field::fld_igc_acz:
        ret = acz; break;
      case KmlFormat::wp_field::fld_igc_gfo:
        ret = gfo; break;
      default:
        throw std::runtime_error("Invalid igc_ext_type");
        break;
    }
    return ret;
  }
};

#endif // IGC_H_INCLUDED_
