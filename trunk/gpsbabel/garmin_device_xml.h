/*
    Parse 'GarminDevice.xml' on a Garmin mass storage device (e.g. Zumo,
      Nuvi, Colorado, etc. and return key device info.

    Copyright (C) 2008 Robert Lipe, robertlipe@gpsbabel.org

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

/*
 * Describes a file on the unit.
 */
typedef struct {
  char* path;
  char* basename;
  char* extension;
  char* canon;	// full name, when applicable.
} gdx_file;

/*
 * The interesting traits of this device.
 */
typedef struct {
  const char* device_desc;
  const char* device_id;
  const char* device_mounted_path; // Not from the file; about the file.
  gdx_file from_device;
  gdx_file to_device;
//	gdx_file geocache_logs;
} gdx_info;

const gdx_info* gdx_read(const char* fname);
const gdx_info* gdx_get_info(void);
const gdx_info* gdx_find_file(char** dirlist);

// This is so gross.   By the time we know it's not a USB device
// and could be one of our devices, we're so deep into the callstack
// that can't back out tracefully without bludgeoning most of the
// (Mac|Lin|Win) x (USB|Serial) matrix.   Since we don't *really* want
// to progress any further, we just longjump back to the caller...
#include <setjmp.h>
extern jmp_buf gdx_jmp_buf;

