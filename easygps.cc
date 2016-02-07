/*
    Access to EasyGPS files.

    Copyright (C) 2003 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA
*/


#include "defs.h"
#include <stdio.h>

static gbfile* file_in;
static gbfile* file_out;
static short_handle mkshort_handle;
/* static char *deficon = NULL; */

#define MYNAME "EasyGPS"

static
arglist_t easygps_args[] = {
  /*	{"deficon", &deficon, "Default icon name", "Waypoint",
    	ARGTYPE_STRING}, */
  ARG_TERMINATOR
};

static void
rd_init(const QString& fname)
{
  int sz;
  char ibuf[100] = {'0'} ;
  const char* ezsig = "TerraByte Location File";

  file_in = gbfopen_le(fname, "rb", MYNAME);

  sz = gbfread(ibuf, 1, 52, file_in);

  if ((sz < 52) ||
      strncmp(ibuf, ezsig, sizeof(ezsig)-1) ||
      (ibuf[51] != 'W')) {
    fatal(MYNAME ": %s is not an EasyGPS file.\n", qPrintable(fname));
  }
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}

static void
data_read(void)
{
  char p;
  char ibuf[10];
  do {
    unsigned char tag;
    Waypoint* wpt_tmp;

    wpt_tmp = new Waypoint;
    UrlLink link;

    for (tag = gbfgetc(file_in); tag != 0xff; tag = gbfgetc(file_in)) {
      switch (tag) {
      case 1:
        wpt_tmp->shortname = gbfgetpstr(file_in);
        break;
      case 2:
      case 3:
        wpt_tmp->description = gbfgetpstr(file_in);
        break;
      case 5:
        wpt_tmp->notes = gbfgetpstr(file_in);
        break;
      case 6: {
        QString ult = gbfgetpstr(file_in);
        link.url_link_text_ = ult;
      }
      break;
      case 7: {
        QString id = gbfgetpstr(file_in);
        wpt_tmp->icon_descr = id;
      }
      break;
      case 8:  /* NULL Terminated (vs. pascal) descr */
        wpt_tmp->notes = gbfgetcstr(file_in);
        break;
      case 9: { /* NULL Terminated (vs. pascal) link */
        QString url = gbfgetcstr(file_in);
        link.url_ = url;
      }
      break;
      case 0x10: {
        QString ult = gbfgetcstr(file_in);
        link.url_link_text_ = ult;
      }
      break;
      case 0x63:
        wpt_tmp->latitude = gbfgetdbl(file_in);
        break;
      case 0x64:
        wpt_tmp->longitude = gbfgetdbl(file_in);
        break;
      case 0x65:
      case 0x66:
        gbfread(ibuf, 8, 1, file_in);
        break;
      case 0x84:
      case 0x85:
        gbfread(ibuf, 4, 1, file_in);
        break;
      case 0x86: /* May be proximity.  I think it's time. */
        gbfread(ibuf, 4, 1, file_in);
        break;
      default:
        printf("Unknown tag %x\n", tag);
        ;
      }
    }
    if (!link.url_.isEmpty() || !link.url_link_text_.isEmpty()) {
      wpt_tmp->AddUrlLink(link);
    }
    waypt_add(wpt_tmp);
    p = gbfgetc(file_in);
  } while (!gbfeof(file_in) && (p == 'W'));
}


static void
ez_disp(const Waypoint* wpt)
{
  gbfputc('W', file_out);
  if (!wpt->shortname.isEmpty()) {
    gbfputc(1, file_out);
    gbfputpstr(wpt->shortname, file_out);
  }
  if (!wpt->description.isEmpty()) {
    gbfputc(3, file_out);
    gbfputpstr(wpt->description, file_out);
  }
  if (!wpt->icon_descr.isNull()) {
    gbfputc(7, file_out);
    gbfputpstr(wpt->icon_descr, file_out);
  }
  gbfputc(0x63, file_out);
  gbfputdbl(wpt->latitude, file_out);

  gbfputc(0x64, file_out);
  gbfputdbl(wpt->longitude, file_out);
  if (!wpt->notes.isEmpty()) {
    gbfputc(5, file_out);
    gbfputpstr(wpt->notes, file_out);
  }
  if (wpt->HasUrlLink()) {
    UrlLink link = wpt->GetUrlLink();
    if (!link.url_link_text_.isEmpty()) {
      gbfputc(6, file_out);
      gbfputpstr(link.url_link_text_, file_out);
    }
    if (!link.url_.isEmpty()) {
      gbfputc(9, file_out);
      gbfputcstr(link.url_, file_out);
    }
  }
  gbfputc(0xff, file_out);
}

static void
data_write(void)
{
  setshort_length(mkshort_handle, 6);

  gbfprintf(file_out,
            "TerraByte Location File Copyright 2001 TopoGrafix\n");
  /*
   * I don't know what this is.
   */
  gbfprintf(file_out, "%c", 0xb);

  waypt_disp_all(ez_disp);

  /*
   * Files seem to always end in a zero.
   */
  gbfputc(0x00, file_out);
}


ff_vecs_t easygps_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  easygps_args,
  CET_CHARSET_ASCII, 0	/* CET REVIEW */
};
