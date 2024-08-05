/*
    Support for GPS TrackMaker data file.

    Copyright (C) 2005  Gustavo Niemeyer <gustavo@niemeyer.net>.

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
 * Documentation can be found at
 * https://www.trackmaker.com/download/ref_guide_eng.pdf
 * https://www.trackmaker.com/download/GTM211_format.pdf
 */
#ifndef GTM_H_INCLUDED_
#define GTM_H_INCLUDED_

#include <QString>   // for QString
#include <QVector>   // for QVector

#include "defs.h"
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile


class GtmFormat : public Format
{
public:
  [[nodiscard]] QVector<arglist_t>* get_args() override
  {
    return nullptr;
  }

  [[nodiscard]] ff_type get_type() const override
  {
    return ff_type_file;
  }

  [[nodiscard]] QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Constants */

  static constexpr int MAX_INDATUM_INDEX = 263;

  static constexpr int indatum_array[MAX_INDATUM_INDEX] = {
    -1, // < 1
      0, 0, 0, 0, 0, 0, 0, // < 8 : Adindan
      1, // < 9 : Afgooye
      2, // < 10 : Ain el Abd
      -1, -1, -1, -1, // < 14
      6, 6, 6, 6, 6, 6, 6, 6, 6, // < 23 : ARC 1950
      7, 7, 7, // < 26 : ARC 1960
      8, // < 27 : Ascension Island 58
      -1, -1, -1, -1, -1, // < 32
      13, // < 33 : Australian Geo 84
      -1, // < 34
      15, // < 35 : Bellevue IGN
      16, // < 36 : Bermuda 1957
      -1, -1, // < 38
      17, // < 39 : Bukit Rimpah
      18, // < 40 : Camp Area Astro
      19, // < 41 : Campo Inchauspe
      22, // < 42 : Canton Islan 1966
      23, // < 43 : Cape
      24, // < 44 : Cape Canaveral
      26, // < 45 : Carthe
      28, // < 46 : Chatham
      29, // < 47 : Chua Astro
      30, // < 48 : Corrego Alegre
      -1, -1, // < 50
      33, // < 51 : Djakarta (Batavia)
      34, // < 52 : DOS 1968
      35, // < 53 : Easter Island 1967
      -1, // < 54
      38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, // < 69 : European 1950 Mean
      39, // < 70 : European 1979 Mean
      -1, // < 71
      41, // < 72 : Gandajika
      42, // < 73 : Geodetic Datum 49
      -1, // < 74
      45, // < 75 : Guam 1963
      46, // < 76 : Gunung Segara
      -1, // < 77
      49, // < 78 : Hearth North
      -1, // < 79
      50, // < 80 : Hjorsey 1955
      51, // < 81 : Hong Kong 1963
      52, // < 82 : Hu-Tzu-Shan
      53, 53, 53, 53, 53, 53, 53, // < 89 : Indian
      -1, // < 90
      55, // < 91 : Ireland 1965
      -1, // < 92
      56, // < 93 : ISTS 073 69
      57, // < 94 : Johnston Island 61
      58, // < 95 : Kandawala
      59, // < 96 : Kerguelen Island
      60, // < 97 : Kertau 48
      -1, -1, // < 99
      61, // < 100 : L.C. 5 Astro
      -1, // < 101
      63, // < 102 : Liberia 1964
      64, 64, // < 104 : Luzon
      -1, // < 105
      65, // < 106 : Mahe 1971
      -1, // < 107
      69, // < 108 : Merchich
      71, // < 109 : Midway Astro 61
      73, 73, // < 111 : Minna
      -1, // < 112
      75, 75, 75, // < 115 : Nahrwan
      76, // < 116 : Naparima BWI
      3, 3, 3, // < 119 : Alaska NAD27
      14, 14, // < 121 : Bahamas NAD27
      20, 20, 20, 20, 20, // < 126 : Canada Mean NAD27
      21, // < 127 : Canal Zone NAD27
      31, // < 128 : Cuba NAD27
      44, // < 129 : Greenland NAD27
      -1, -1, // < 131
      20, // < 132 : Canada Mean NAD27
      -1, -1, -1, // < 135
      70, // < 136 : Mexico NAD27
      -1, -1, -1, -1, -1, -1, -1, -1, // < 144
      80, // < 145 : Old Egyptian
      81, // < 146 : Old Hawaiian
      82, // < 147 : Old Hawaiian Kauai
      83, // < 148 : Old Hawaiian Maui
      81, // < 149 : Old Hawaiian Mean
      84, // < 150 : Old Hawaiian Oahu
      85, // < 151 : Oman
      86, 86, 86, 86, 86, // < 156 : OSG Britain
      87, // < 157 : Pico de Las Nieves
      88, // < 158 : Pitcairn Astro 67
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // < 171
      91, // < 172 : Puerto Rico
      92, // < 173 : Pulkovo 1942
      94, // < 174 : Quatar National
      -1, -1, // < 176
      95, // < 177 : Rome 1940
      96, 96, 96, 96, 96, 96, 96, // < 184 : S-42 (Pulkovo 1942)
      -1, // < 185
      100, // < 186 : Santo DOS
      99, // < 187 : Sao Braz
      -1, -1, -1, -1, // < 191
      105, 105, // < 193 : SAD-69/Mean
      98, // < 194 : SAD-69/Brazil
      105, 105, 105, 105, 105, 105, 105, 105, 105, 105, // < 204 : SAD-69/Mean
      106, // < 205 : South Asia
      109, // < 206 : Tananarive 1926
      111, // < 207 : Timbalai 1948
      112, 112, 112, 112, // < 211 : Tokyo mean
      113, // < 212 : Tristan Astro 1968
      115, // < 213 : Viti Levu 1916
      -1, -1, // < 215
      116, // < 216 : Wake Eniwetok 1960
      117, // < 217 : WGS 72
      118, // < 218 : WGS 84
      119, // < 219 : Yacare
      120, // < 220 : Zanderij
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // < 231
      98, // < 232 : SAD-69/Brazil
      -1, -1, // < 234
      117, // < 235 : WGS 72
      0, // < 236 : Adindan
      2, // < 237 : Ain el Abd
      7, // < 238 : ARC 1960
      8, // < 239 : Ascension Island 58
      -1, -1, // < 241
      52, // < 242 : Hu-Tzu-Shan
      53, 53, 53, // < 245 : Indian
      -1, // < 246
      57, // < 247 : Johnston Island 61
      64, // < 248 : Luzon
      -1, // < 249
      75, // < 250 : Nahrwan
      76, // < 251 : Naparima BWI
      -1, -1, -1, // < 254
      82, // < 255 : Old Hawaiian Kauai
      83, // < 256 : Old Hawaiian Maui
      84, // < 257 : Old Hawaiian Oahu
      -1, -1, // < 259
      101, // < 260 : Sapper Hill 43
      111, // < 261 : Timbalai 1948
      112, // < 262 : Tokyo mean
      116 // < 263 : Wake Eniwetok 1960
    };

  static constexpr const char* icon_descr[] = {
    "", "Airport", "Ball Park", "Bank", "Bar", "Boat Ramp", "Campground", "Car",
    "City (Large)", "City (Medium)", "City (Small)", "Dam", "Danger Area",
    "Drinking Water", "Fishing Area", "Gas Station", "Glider Area", "Golf Course",
    "Heliport", "Hotel", "Animals", "Information", "Man Overboard", "Marina",
    "Mine", "Medical Facility", "Parachute Area", "Park", "Parking Area",
    "Picnic Area", "Private Field", "Residence", "Restaurant", "Restroom",
    "Scenic Area", "School", "Seaplane Base", "Shipwreck", "Shopping Center",
    "Short Tower", "Policy Station", "Ski Resort", "Soft Field", "Swimming Area",
    "Tall Tower", "Telephone", "Tracback Point", "Ultralight Area", "Waypoint",
    "Boat", "Exit", "Flag", "Duck", "Buoy", "Back Track", "Beach", "Bridge",
    "Building", "Car Repair", "Cemetery", "Church", "Civil", "Convenience Store",
    "Crossing", "Fast Food", "Forest", "Ghost Town", "Levee", "Military",
    "Oil Field", "Post Office", "Rv Park", "Scales", "Summit", "Toll Booth",
    "Trail Head", "Truck Stop", "Tunnel", "Highway", "Gate", "Fall", "Fence",
    "Mata-Burro", "Fitness Center", "Movie Theater", "Live Theater", "Zoo", "Horn",
    "Bowling", "Car Rental", "City (Capitol)", "Controlled Area", "Stadium",
    "Museum", "Amusement Park", "Skull", "Department Store", "Pharmacy", "Pizza",
    "Diver Down Flag 1", "Light", "Pin", "", "Pigsty", "Tree", "Bamboo",
    "Banana Plant", "Arrow-Down", "Bifurcation", "Cavern", "River", "Rock",
    "Arrow-Up", "Trunk", "Soccer Field", "Sporting Court", "Flag, Green", "Trench",
    "Ship-Yellow", "Green Sign", "Swamp", "Lake", "Stop!",
    "Fishing Hot Spot Facility", "Speed Reducer", "Stairway", "Cactus", "Ship-Red",
    "Letter - S", "Letter - D", "Letter - N",
    "Crossing", "Cross", "Flag, Red", "Curve1", "Curve2", "Curve3", "Curve4",
    "Letter - W", "Letter - L", "Letter - R", "Radio Beacon", "Road Sign",
    "Geocache", "Geocache Found", "Traffic Light", "Bus Station", "Train Station",
    "School", "Mile Marker", "Conservation Area", "Waypoint", "Box", "Aerial",
    "Auto Repair", "Boat", "Exit Ramp", "Fixed Nav Aid", "Floating Buoy", "Garden",
    "Fish Farm", "Lighthouse", "Truck Service", "Resort", "Scuba", "Shooting",
    "Sight Seeing", "Sounding", "Winery", "Navaid, Amber", "Navaid, Black",
    "Navaid, Blue", "Navaid, Green", "Navaid, Green/Red", "Navaid, Green/White",
    "Navaid, Orange", "Navaid, Red", "Navaid, Red/Green", "Navaid, Red/White",
    "Navaid, Violet", "Navaid, White", "Navaid, White/Green", "Navaid, White/Red",
    "Buoy, White", "Dot, White", "Red Square", "Red Diamond", "Green Square",
    "Green Diamond", "Restricted Area", "Navaid (unlit)", "Dot (Small)", "Libraries", "Waypoint", "Waypoint1",
    "Waypoint2", "Mark (1)", "Mark (2)", "Mark (3)", "Cross (Red)", "Store",
    "Exclamation", "Flag (EUA)", "Flag (CAN)", "Flag (BRA)", "Man", "Animals",
    "Deer Tracks", "Tree Stand", "Bridge", "Fence", "Intersection",
    "Non Direct Beacon", "VHF Omni Range", "Vor/Tacan", "Vor-Dme",
    "1st Approach Fix", "Localizer Outer", "Missed Appr. Pt", "Tacan",
    "CheckPoint", nullptr
  };

  /* Member Functions */

  static QString fread_string(gbfile* fd);
  static void fread_string_discard(gbfile* fd);
  static QString fread_fixedstring(gbfile* fd, int len);
  static void fwrite_null(gbfile* fd, int len);
  static void fwrite_string(gbfile* fd, const char* str);
  static void fwrite_string(gbfile* fd, const QString& str);
  static void fwrite_fixedstring(gbfile* fd, const char* str, int fieldlen);
  static void fwrite_fixedstring(gbfile* fd, const QString& str, int fieldlen);
  void set_datum(int n);
  void convert_datum(double* lat, double* lon) const;
  void count_track_styles(const route_head* rte);
  static int icon_from_descr(const QString& descr);
  void write_waypt(const Waypoint* wpt);
  void start_rte(const route_head* rte);
  void write_trk_waypt(const Waypoint* wpt);
  void write_trk_style(const route_head* trk);
  void write_rte_waypt(const Waypoint* wpt);

  /* Data Members */

  gbfile* file_in{};
  gbfile* file_out{};
  int indatum{};
  int wp_count{};
  int ws_count{};
  int tr_count{};
  int ts_count{};
  int rt_count{};
  int im_count{};
  const route_head* rte_active{};
  int start_new{};
};
#endif // GTM_H_INCLUDED_
