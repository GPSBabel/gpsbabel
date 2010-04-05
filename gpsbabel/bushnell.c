/*
    Read and write Bushnell files.

    Copyright (C) 2008, 2009  Robert Lipe (robertlipe@gpsbabel.org)

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
#define MYNAME "Bushnell"

static gbfile *file_in;
static char *ofname;
static short_handle mkshort_handle = NULL; 

static
arglist_t bushnell_args[] = {
  ARG_TERMINATOR
};

// Apparently, the icons are undocumented, so we made up names, 
// preferring them to be consistent with other brands where possiblde.

typedef struct  {
  const signed int symbol;
  const char *icon;
} icon_mapping_t;

icon_mapping_t bushnell_icons[] = {
  { 0x00, "Yellow Square"},
  { 0x01, "Blue Grey Circle" },
  { 0x02, "Yellow Diamond" },
  { 0x03, "Blue Asterisk" },
  { 0x04, "Blue Bulls Eye pointing NE" },
  { 0x05, "Red =O= on a 45 degree." },
  { 0x06, "House" },
  { 0x06, "Residence" },
  { 0x07, "Lodging" },
  { 0x08, "Hospital" },
  { 0x09, "Auto Repair" },
  { 0x09, "Car Repair" },
  { 0x0a, "Tools" },
  { 0x0b, "Gas" },
  { 0x0c, "Hiking" },
  { 0x0d, "Camping" },
  { 0x0e, "Picnic Area" },
  { 0x0f, "Deer Stand" },
  { 0x10, "Deer" },
  { 0x11, "Park" },
  { 0x11, "Tree" },
  { 0x12, "Highway Exit" },
  { 0x13, "Fjord"}, // Looks like a road narrows.
  { 0x14, "Bridge" },
  { 0x15, "Waypoint" }, //  or golf hole/flag
  { 0x16, "Warning" }, //  Caution Triangle with ! in it.
  { 0x17, "Bicycle" },

  { 0x18, "Blue Circle" }, // ? in it, undocumented icon.
  { 0x19, "Blue Diamond Checkmark" }, //  undocumented.

  { 0x1a, "Camera" },
  { 0x1b, "Restaraunt" }, // "Fork/Knife (meal place?)" 
  { 0x1c, "Restroom" }, // (man & Woman icon)" 
  { 0x1d, "RV Park" }, // "Bus or RV (RV campground?)" 
  { 0x1e, "Potable Water" }, // (faucet/glass or bucket)" 
  { 0x1f, "Fishing" },
  { 0x20, "Anchor in square" },
  { 0x21, "Boat ramp/launch" },
  { 0x22, "Anchor" },
  { 0x23, "Bouy" },
  { 0x24, "Man Overboard?" },
  { 0x25, "Snow Skiing" },
  { 0x26, "Mouantin/Mountain Peak" },
  { 0x27, "Turkey Tracks/animal tracks" },

  { 0x28, "Bank" }, // "Cash (ATM MAybe)" 
  { 0x29, "Bar" }, // "Martini undocumented" 
  { 0x2a, "Lighthouse" },

  { 0x2b, "Tent" },

  { 0x2c, "Cresent Wrench or can opener" },

  { 0x2d, "School" }, //? White Building with tunnel looking door and flag on top." 
  { 0x2f, "Information" }, // "i  (info/internet maybe?)" 
  { 0x30, "Picnic" }, //"Picnic table & Tree, maybe forest picnic or day use area?" 
  { 0x31, "Phone" },
  { 0x32, "Letter/Envelope" },
  { 0x33, "Forest/Park Ranger" },
  { 0x34, "Fire department" }, //? Red Square building with yellow flag."

  { 0x35, "Shopping" },
  { 0x36, "Looks like Cross+hurricane symbol, strange also undocumented." },

  { 0x37, "Tunnel" },
  { 0x38, "Mountain/Summit" },

  { 0x39, "Square split diagonally with lines between... magnet maybe? undocumented" },

  { 0x3a, "Swimmer/swimming" },
  { 0x3b, "Officer? Looks like man leaned over holding blue cube..." },
  { 0x3c, "Parking" }, //"Car Parked" 
  { 0x3d, "Airport" },
  { 0x3e, "Bus Terminal" }, // (guess) Loks like Bus under canopy." 
  { 0x3f, "Red Cross" },
  { 0x40, "Red Buidling with flag, Fire Station maybe." },
  { 0x41, "Bus" },
  { 0x42, "Officer" }, // "see 3b: duplicate" 
  { 0x43, "Railroad" },
  { 0x44, "Auto Ferry" },
  {-1, NULL}
};

static unsigned int
bushnell_get_icon_from_name(const char *name) {
  icon_mapping_t *t;
  for (t = bushnell_icons; t->icon > 0; t++) {
   if (0 == case_ignore_strcmp(name, t->icon))
    return t->symbol;
  }
  return 0;
}

static const char *
bushnell_get_name_from_symbol(signed int s) {
  icon_mapping_t *t;
  for (t = bushnell_icons; t->icon > 0; t++) {
   if (s == t->symbol)
     return t->icon;
  }
  return "Waypoint";
}

static void
rd_init(const char *fname) {
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit(void) {
  gbfclose(file_in);
}

static void
wr_init(const char *fname) {
  char *dot, *slash;
  static char valid_chars [] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789"
		".-/\\~@#$%^&*()_+=<>"
                "abcdefghijklmnopqrstuvwxyz";

  ofname = xstrdup(fname);

  // If user provided an extension in the pathname, whack it.
  dot = strrchr(ofname, '.');
  slash = strrchr(ofname, GB_PATHSEP);
  if (dot > slash) *dot = 0;

  mkshort_handle = mkshort_new_handle();
  setshort_length(mkshort_handle, 19);
  setshort_goodchars(mkshort_handle, valid_chars);
}

static void
wr_deinit(void) {
  mkshort_del_handle(&mkshort_handle);
  xfree(ofname);
}

/*
 * Each file contains a single waypoint.
 */
static void
bushnell_read(void) {
  gbint32 lat_tmp,lon_tmp;
  unsigned int proximity;
  unsigned int icon;
  waypoint *wpt_tmp = waypt_new();

  lat_tmp = gbfgetint32(file_in);
  lon_tmp = gbfgetint32(file_in);

  icon = gbfgetc(file_in);
  wpt_tmp->icon_descr = bushnell_get_name_from_symbol(icon);
  proximity = gbfgetc(file_in); // 1 = off, 3 = proximity alarm.
  (void) proximity;
  wpt_tmp->latitude = lat_tmp /  10000000.0;
  wpt_tmp->longitude = lon_tmp / 10000000.0;

  // Apparently this is always zero terminated, though it's never been
  // observed to be longer than 19 bytes + a null terminator.
  wpt_tmp->shortname = xstrdup(gbfgetstr(file_in));

  waypt_add(wpt_tmp);
}

static void
bushnell_write_one(const waypoint *wpt) {
  char tbuf[20]; // 19 text bytes + null terminator.
  char padding[2] = {0, 0};
  gbfile *file_out;
  static int wpt_count;
  char *fname;
  char *ident;
  xasprintf(&fname, "%s-%d.wpt", ofname, wpt_count++);

  file_out = gbfopen_le(fname, "wb", MYNAME);
  gbfputint32(wpt->latitude  * 10000000, file_out);
  gbfputint32(wpt->longitude * 10000000, file_out);
  gbfputc(bushnell_get_icon_from_name(wpt->icon_descr ? wpt->icon_descr : 
                                      "Waypoint"), file_out);
  gbfputc(0x01, file_out);  // Proximity alarm.  1 == "off", 3 == armed.

  ident = mkshort(mkshort_handle, wpt->shortname);
  strncpy(tbuf, wpt->shortname, sizeof(tbuf));
  tbuf[sizeof(tbuf)-1] = 0;
  gbfwrite(tbuf, sizeof(tbuf), 1, file_out);

  // two padding bytes follow name.
  gbfwrite(padding, sizeof(padding), 1, file_out);

  xfree(fname);
  gbfclose(file_out);
}

static void
bushnell_write(void) {
  waypt_disp_all(bushnell_write_one);
}

ff_vecs_t bushnell_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  bushnell_read,
  bushnell_write,
  NULL,
  bushnell_args,
  CET_CHARSET_MS_ANSI, 0  /* Not really sure... */
};
