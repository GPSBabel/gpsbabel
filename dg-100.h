/*

    GlobalSat DG-100/DG-200 GPS data logger download.

    Copyright (C) 2007  Mirko Parthey, mirko.parthey@informatik.tu-chemnitz.de
    Copyright (C) 2005-2008  Robert Lipe, robertlipe+source@gpsbabel.org
    Copyright (C) 2012  Nicolas Boullis, nboullis@debian.org
    Copyright (C) 2014  Jean-Claude Repetto, gpsbabel@repetto.org

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

/*
    DG-100 / DG-200 communication protocol specification:
    http://www.usglobalsat.com/s-176-developer-information.aspx
 */
#ifndef DG100_H_INCLUDED_
#define DG100_H_INCLUDED_

#include <cstdint>           // for uint8_t, int16_t, uint16_t
#include <cstdio>            // for size_t

#include <QtCore/QDateTime>  // for QDateTime
#include <QtCore/QList>      // for QList
#include <QtCore/QString>    // for QString
#include <QtCore/QVector>    // for QVector

#include "defs.h"
#include "format.h"          // for Format
#include "gbfile.h"          // for gbfile


class Dg100Format : public Format
{
public:
  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &dg100_args;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read			/* waypoints */,
      ff_cap_read 			/* tracks */,
      ff_cap_none 			/* routes */
    };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_ASCII;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void read() override;
  void rd_deinit() override;

protected:
  /* Types */

  enum dg100_command_id {
    dg100cmd_getconfig     = 0xB7,
    dg100cmd_setconfig     = 0xB8,
    dg100cmd_getfileheader = 0xBB,
    dg100cmd_getfile       = 0xB5,
    dg100cmd_erase         = 0xBA,
    dg100cmd_getid         = 0xBF,
    dg100cmd_setid         = 0xC0,
    dg100cmd_gpsmouse      = 0xBC,
    dg200cmd_reset         = 0x80
  };

  struct dg100_command {
    int  id;
    int  sendsize;
    int  recvsize;
    int  trailing_bytes;
    const char* text;	/* Textual description for debugging */
  };

  struct model_t {
    const char* name;
    unsigned speed;
    bool has_trailing_bytes;
    bool has_payload_end_seq;
    const dg100_command* commands;
    unsigned int numcommands;
  };

  /* Constants */

  /* maximum frame size observed so far: 1817 bytes
   *   (dg100cmd_getfileheader returning 150 entries)
   * dg100cmd_getfileheader is the only answer type of variable length,
   * answers of other types are always shorter than 1817 bytes */
  static constexpr int FRAME_MAXLEN = 4096;

  /* Member Functions */

  const dg100_command* dg100_findcmd(int id) const;
  static QDateTime bintime2utc(int date, int time);
  static void dg100_debug(const char* hdr, int include_nl, size_t sz, unsigned char* buf);
  static void dg100_log(const char* fmt, ...);
  static float bin2deg(int val);
  void process_gpsfile(uint8_t* data, route_head** track) const;
  static uint16_t dg100_checksum(const uint8_t* buf, int count);
  size_t dg100_send(uint8_t cmd, const void* payload, size_t param_len) const;
  int dg100_recv_byte() const;
  int dg100_read_wait(void* handle, void* buf, unsigned int len, unsigned int ms) const;
  int dg100_recv_frame(const dg100_command** cmdinfo_result, uint8_t** payload) const;
  int dg100_recv(uint8_t expected_id, void* buf, unsigned int len) const;
  int dg100_request(uint8_t cmd, const void* sendbuf, void* recvbuf, size_t count) const;
  QList<int> dg100_getfileheaders() const;
  void dg100_getconfig() const;
  void dg100_getfile(int16_t num, route_head** track) const;
  void dg100_getfiles() const;
  int dg100_erase() const;
  void common_rd_init(const QString& fname);
  void dg100_rd_init(const QString& fname, bool isfile);
  void dg200_rd_init(const QString& fname, bool isfile);

  /* Data Members */

  const model_t* model{nullptr};
  void* serial_handle{nullptr};
  gbfile* fin{nullptr};
  bool isfile{false};

  static const dg100_command dg100_commands[];
  static const dg100_command dg200_commands[];

  /* GPSBabel integration */

  char* erase{nullptr};
  char* erase_only{nullptr};

  QVector<arglist_t> dg100_args = {
    {
      "erase", &erase, "Erase device data after download",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "erase_only", &erase_only, "Only erase device data, do not download anything",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

};

class Dg100SerialFormat : public Dg100Format
{
public:

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  void rd_init(const QString& fname) override
  {
    dg100_rd_init(fname, false);
  }
};

class Dg100FileFormat : public Dg100Format
{
public:

  ff_type get_type() const override
  {
    return ff_type_internal;
  }

  void rd_init(const QString& fname) override
  {
    dg100_rd_init(fname, true);
  }
};

class Dg200SerialFormat : public Dg100Format
{
public:

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  void rd_init(const QString& fname) override
  {
    dg200_rd_init(fname, false);
  }
};

class Dg200FileFormat : public Dg100Format
{
public:

  ff_type get_type() const override
  {
    return ff_type_internal;
  }

  void rd_init(const QString& fname) override
  {
    dg200_rd_init(fname, true);
  }
};
#endif // DG100_H_INCLUDED_
