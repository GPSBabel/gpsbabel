/*
    Parse 'GarminDevice.xml' on a Garmin mass storage device (e.g. Zumo,
      Nuvi, Colorado, etc. and return key device info.

    Copyright (C) 2008 Robert Lipe, robertlipe+source@gpsbabel.org

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

// References:
// http://developer.garmin.com/web-device/garmin-mass-storage-mode-devices/
// http://developer.garmin.com/schemas/device/v2/


#include "defs.h"
#include "xmlgeneric.h"
#include "garmin_device_xml.h"
#include <QtCore/QXmlStreamAttributes>
#include <stdio.h>

#define MYNAME "whatever"

static gdx_info* my_gdx_info;
static int type;
static char* mountpoint, *base, *path, *ext;
static xg_callback device_s, id_s, path_s, ext_s, base_s, dir_s;
jmp_buf gdx_jmp_buf;

void type_s(xg_string args, const QXmlStreamAttributes*)
{
  type = args.compare("GPSData");
}

void device_s(xg_string args, const QXmlStreamAttributes*)
{
  if (my_gdx_info) {
    fatal(MYNAME ": More than one device type found in file.\n");
  }
  my_gdx_info = (gdx_info*) xcalloc(sizeof *my_gdx_info, 1);
  my_gdx_info->device_desc = xstrdup(args);
}

void id_s(xg_string args, const QXmlStreamAttributes*)
{
  my_gdx_info->device_id = xstrdup(args);
}

void path_s(xg_string args, const QXmlStreamAttributes*)
{
  path = xstrdup(args);
}

void ext_s(xg_string args, const QXmlStreamAttributes*)
{
  ext = xstrdup(args);
}

void base_s(xg_string args, const QXmlStreamAttributes*)
{
  base = xstrdup(args);
}

void dir_s(xg_string args, const QXmlStreamAttributes*)
{
  if (type) {
    return;
  }
  if ((args == "OutputFromUnit")) {
    xasprintf(&my_gdx_info->from_device.path,  "%s%c%s",
              mountpoint, GB_PATHSEP, path);
    my_gdx_info->from_device.basename = xstrdup(base);
    my_gdx_info->from_device.extension = xstrdup(ext);
    xasprintf(&my_gdx_info->from_device.canon, "%s/%s.%s",
              my_gdx_info->from_device.path,
              my_gdx_info->from_device.basename,
              my_gdx_info->from_device.extension);
  } if ((args == "InputToUnit")) {
    xasprintf(&my_gdx_info->to_device.path,  "%s%c%s",
              mountpoint, GB_PATHSEP, path);
    my_gdx_info->to_device.basename = xstrdup(base);
    my_gdx_info->to_device.extension = xstrdup(ext);
  } else {
    fatal(MYNAME ":Unknown direction '%s'\n", qPrintable(args));
  }

  if (base) {
    xfree(base) ;
  }
  base = NULL;

  if (ext) {
    xfree(ext) ;
  }
  ext = NULL;

  if (path) {
    xfree(path) ;
  }
  path = NULL;
}

static xg_tag_mapping gdx_map[] = {
  { device_s, cb_cdata, "/Device/Model/Description" },
  { id_s, cb_cdata, "/Device/Id" },
  { path_s, cb_cdata, "/Device/MassStorageMode/DataType/File/Location/Path" },
  { type_s, cb_cdata, "/Device/MassStorageMode/DataType/Name" },
  { ext_s, cb_cdata, "/Device/MassStorageMode/DataType/File/Location/FileExtension" },
  { base_s, cb_cdata, "/Device/MassStorageMode/DataType/File/Location/BaseName" },
  { dir_s, cb_cdata, "/Device/MassStorageMode/DataType/File/TransferDirection" },
  { 0, (xg_cb_type) 0, NULL }
};

const gdx_info*
gdx_read(const char* fname)
{
  // Test file open-able before gb_open gets a chance to fatal().
  FILE* fin = fopen(fname, "r");

  if (fin) {
    fclose(fin);
    xml_init(fname, gdx_map, NULL);
    xml_read();
    xml_deinit();
  }

  return my_gdx_info;
}


// Look for the Device in the incoming NULL-terminated list of directories
const gdx_info*
gdx_find_file(char** dirlist)
{
  const gdx_info* gdx;
  while (dirlist && *dirlist) {
    char* tbuf;
    xasprintf(&tbuf, "%s/%s", *dirlist, "/Garmin/GarminDevice.xml");
    mountpoint = *dirlist;
    gdx = gdx_read(tbuf);
    xfree(tbuf);
    if (gdx) {
      longjmp(gdx_jmp_buf, 1);
    }
    dirlist++;
  }
  return NULL;
}

const gdx_info*
gdx_get_info()
{
  return my_gdx_info;
}
