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

class IgcFormat : public Format
{
public:
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
struct igc_fs_flags_t {
  igc_fs_flags_t() :
  has_exts(0),
  enl(0),
  tas(0),
  vat(0),
  oat(0),
  trt(0),
  gsp(0),
  fxa(0),
  enl_start(0),
  enl_end(0),
  tas_start(0),
  tas_end(0),
  vat_start(0),
  vat_end(0),
  oat_start(0),
  oat_end(0),
  trt_start(0),
  trt_end(0),
  gsp_start(0),
  gsp_end(0),
  fxa_start(0),
  fxa_end(0)

  {}

  bool has_exts;
  bool enl;
  bool tas;
  bool vat;
  bool oat;
  bool trt;
  bool gsp;
  bool fxa;
  short enl_start;
  short enl_end;
  short tas_start;
  short tas_end;
  short vat_start;
  short vat_end;
  short oat_start;
  short oat_end;
  short trt_start;
  short trt_end;
  short gsp_start;
  short gsp_end;
  short fxa_start;
  short fxa_end;
};

struct igc_fsdata : public FormatSpecificData {
  igc_fsdata() : FormatSpecificData(kFsIGC) {}

  igc_fsdata* clone() const override
  {
    return new igc_fsdata(*this);
  }

  int enl{0}; // Engine Noise Level
  int tas{0}; // True Airspeed
  int vat{0}; // Compensated variometer (total energy)
  int oat{0}; // Outside Air Temperature
  int trt{0}; // True Track
  int gsp{0}; // Ground Speed
  int fxa{0}; // Fix Accuracy
  igc_fs_flags_t igc_fs_flags;

};

#endif // IGC_H_INCLUDED_
