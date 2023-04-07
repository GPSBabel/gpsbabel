/*
 * FAI/IGC data format translation.
 *
 * Refer to Appendix 1 of
 * http://www.fai.org:81/gliding/gnss/tech_spec_gnss.asp for the
 * specification of the IGC data format.  This translation code was
 * written when the latest amendment list for the specification was AL6.
 *
 * Later updates, mostly IGC extensions, by Kenneth Voort, based on AL8.
 * (https://www.fai.org/sites/default/files/igc_fr_specification_with_al8_2023-2-1_0.pdf)
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
#include <QHash>                 // for QHash

#include "defs.h"
#include "format.h"
#include "gbfile.h"              // for gbfprintf, gbfclose, gbfopen, gbfputs, gbfgetstr, gbfile
#include "src/core/datetime.h"   // for DateTime
#include "kml.h"                 // for wp_field

/*
 * Notes on IGC extensions:
 *
 * The IGC spec does not explicitly specify many scaling factors or units.
 * Most of these are assumed based on IGC files available to the author.
 *
 * - (OAT) Temperature is assumed to be three digits with one implied decimal,
 *   leading with zero for positive values and a "-" for negative vaules
 * - (TAS) True airspeed is assumed to be in km/h, with two decimals
 *   As per spec, this is supposed to be specified per IGC file, but never is
 * - (VAT) Total Enery Vario, in metres per second with one decimal and leading zero
 * - (TRT) True Track is in degrees; it's useful to record this, but not very useful to graph it
 * - (GSP) Ground speed units are not specified in spec. Km/h is assumed, with two decimals
 * - (FXA) Fix accuracy is Estimated Position Error in metres, to a 2-Sigma (95.45%) probability
 * - (GFO) is *not* in the spec, however has been seen in LX Eos recorders commonly found
 *   in LS-4 gliders, a popular high performance single seat glider. Pending verification
 *   from the vendor, it is currently assumed to be G Force/Load
 * - (SIU) Satellites in use is self explanatory
 * - (ACZ) Z Acceleration is assumed to be (G factor * 10), based on available evidence
 *   "So called 'G'" is specified, but not the number of decimals
 */
class IgcFormat : public Format
{
public:
  enum class igc_ext_type_t {
    ext_rec_unknown = 0,  // Intended for defaults
    ext_rec_enl = 1,  // Engine Noise Level
    ext_rec_tas = 2,  // True Airspeed
    ext_rec_vat = 3,  // Total Energy Variometer
    ext_rec_oat = 4,  // Outside Air Temperature
    ext_rec_trt = 5,  // True Track
    ext_rec_gsp = 6,  // Ground Speed
    ext_rec_fxa = 7,  // Fix Accuracy
    ext_rec_gfo = 8,  // G Force?
    ext_rec_siu = 9,  // Satellites In Use
    ext_rec_acz = 10,  // Z Acceleration

  };

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

  const QHash<QString, IgcFormat::igc_ext_type_t> igc_extension_map{
    {"ENL", IgcFormat::igc_ext_type_t::ext_rec_enl},
    {"TAS", IgcFormat::igc_ext_type_t::ext_rec_tas},
    {"VAT", IgcFormat::igc_ext_type_t::ext_rec_vat},
    {"OAT", IgcFormat::igc_ext_type_t::ext_rec_oat},
    {"TRT", IgcFormat::igc_ext_type_t::ext_rec_trt},
    {"GSP", IgcFormat::igc_ext_type_t::ext_rec_gsp},
    {"FXA", IgcFormat::igc_ext_type_t::ext_rec_fxa},
    {"SIU", IgcFormat::igc_ext_type_t::ext_rec_siu},
    {"ACZ", IgcFormat::igc_ext_type_t::ext_rec_acz},
    {"GFO", IgcFormat::igc_ext_type_t::ext_rec_gfo},
  };

  // Will return zero if no match
  igc_ext_type_t get_ext_type(const QString& type) const
  {
    return igc_extension_map.value(type, igc_ext_type_t::ext_rec_unknown);
  }
  // Overload to specify default value as second argument
  igc_ext_type_t get_ext_type(const QString& type, const igc_ext_type_t& default_value) const
  {
    return igc_extension_map.value(type, default_value);
  }

  /*
   * Returns zero when an extension is not found. Given that this function
   * returns a factor, this could lead to division by zero exceptions.
   * A factor can never be zero, so this looks good to me.
   * Be careful.
   */
  int get_ext_factor(IgcFormat::igc_ext_type_t type) const
  {
    int ret = 0;
    switch (type) {
    case IgcFormat::igc_ext_type_t::ext_rec_enl:
      ret = 1;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_tas:
      ret = 100;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_vat:
      ret = 10;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_oat:
      ret = 10;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_trt:
      ret = 1;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gsp:
      ret = 100;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_fxa:
      ret = 1;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_siu:
      ret = 1;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_acz:
      ret = 10;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gfo:
      ret = 1;
      break;
    default:
      warning("igc.h: IgcFormat::get_ext_factor(): unknown extension (%i), returning factor of zero.\n",int(type));
      break;
    }
    return ret;
  }

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
  igc_rec_type_t get_record(char**) const;
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

struct igc_fsdata : public FormatSpecificData {
  igc_fsdata() : FormatSpecificData(kFsIGC) {}

  igc_fsdata* clone() const override
  {
    return new igc_fsdata(*this);
  }

  std::optional<double> enl; // Engine Noise Level
  std::optional<double> tas; // True Airspeed
  std::optional<double> vat; // Compensated variometer (total energy)
  std::optional<double> oat; // Outside Air Temperature
  std::optional<double> trt; // True Track
  std::optional<double> gsp; // Ground Speed
  std::optional<double> fxa; // Fix Accuracy
  std::optional<double> siu; // Satellites In Use
  std::optional<double> acz; // Z Acceleration
  std::optional<double> gfo; // G Force?

  // Stores all data as igc_fsdata
  bool set_value(IgcFormat::igc_ext_type_t type, double value)
  {
    bool success = true;
    switch (type) {
    case IgcFormat::igc_ext_type_t::ext_rec_enl:
      enl = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_tas:
      tas = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_vat:
      vat = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_oat:
      oat = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_trt:
      trt = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gsp:
      gsp = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_fxa:
      fxa = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_siu:
      siu = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_acz:
      acz = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gfo:
      gfo = value;
      break;
    default:
      success = false;
    }
    return success;
  }
  // Overload, used to optionally match IGC data with Waypoint data
  // Stores _some_ data as Waypoint members, where appropriate
  bool set_value(IgcFormat::igc_ext_type_t type, double value, Waypoint* wp)
  {
    bool success = true;
    switch (type) {
    case IgcFormat::igc_ext_type_t::ext_rec_enl:
      enl = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_tas:
      tas = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_vat:
      vat = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_oat:
      wp->set_temperature(value);
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_trt:
      trt = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gsp:
      gsp = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_fxa:
      fxa = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_siu:
      wp->sat = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_acz:
      acz = value;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gfo:
      gfo = value;
      break;
    default:
      success = false;
    }
    return success;
  }

  // Not currently used, but already written and left for future use.
  std::optional<double> get_value(IgcFormat::igc_ext_type_t defn_type) const
  {
    std::optional<double> ret;
    switch (defn_type) {
    case IgcFormat::igc_ext_type_t::ext_rec_enl:
      ret = enl;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_tas:
      ret = tas;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_vat:
      ret = vat;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_oat:
      ret = oat;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_trt:
      ret = trt;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gsp:
      ret = gsp;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_fxa:
      ret = fxa;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_siu:
      ret = siu;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_acz:
      ret = acz;
      break;
    case IgcFormat::igc_ext_type_t::ext_rec_gfo:
      ret = gfo;
      break;
    default:
      fatal("igc.h: igc_fsdata::get_value(IgcFormat::igc_ext_type_t defn_type): Invalid igc_ext_type\n");
      break;
    }
    return ret;
  }
  std::optional<double> get_value(KmlFormat::wp_field defn_type) const
  {
    std::optional<double> ret;
    switch (defn_type) {
    case KmlFormat::wp_field::igc_enl:
      ret = enl;
      break;
    case KmlFormat::wp_field::igc_tas:
      ret = tas;
      break;
    case KmlFormat::wp_field::igc_vat:
      ret = vat;
      break;
    case KmlFormat::wp_field::igc_oat:
      ret = oat;
      break;
    case KmlFormat::wp_field::igc_trt:
      ret = trt;
      break;
    case KmlFormat::wp_field::igc_gsp:
      ret = gsp;
      break;
    case KmlFormat::wp_field::igc_fxa:
      ret = fxa;
      break;
    case KmlFormat::wp_field::igc_siu:
      ret = siu;
      break;
    case KmlFormat::wp_field::igc_acz:
      ret = acz;
      break;
    case KmlFormat::wp_field::igc_gfo:
      ret = gfo;
      break;
    default:
      fatal("igc.h: igc_fsdata::get_value(KmlFormat::wp_field defn_type): Invalid wp_field\n");
      break;
    }
    return ret;
  }
};

#endif // IGC_H_INCLUDED_
