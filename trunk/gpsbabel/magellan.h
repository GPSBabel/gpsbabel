/*
    Copyright (C) 2002-2005 Robert Lipe, robertlipe@usa.net

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
 * Table of "interesting" Magellan models.
 * Selfishly, if I haven't heard of it, it's not in the table.
 * This doesn't mean I actually have TRIED all models listed below.
 * (Donations welcome. :-)
 */
typedef enum {
  mm_unknown = 0 ,
  mm_gps315320,
  mm_map410,
  mm_map330,
  mm_gps310,
  mm_meridian,
  mm_sportrak
} meridian_model;

typedef struct pid_to_model {
  meridian_model model;
  int pid;
  const char* model_n;
} pid_to_model_t;

typedef struct icon_mapping {
  const char* token;
  const char* icon;
} icon_mapping_t;

QString mag_find_descr_from_token(const char* token);
QString mag_find_token_from_descr(const QString& icon);

unsigned int mag_checksum(const char* const buf);
char* m330_cleanse(char* istring);

waypoint* mag_trkparse(char* trkmsg);
void mag_rteparse(char* rtemsg);
