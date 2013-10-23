/*
    Garmin icon tables
    Based on information provided by Ian Cowley, Sigurd Humerfelt,
	and Garmin MapSource

    Copyright (C) 2003-2007 Robert Lipe, robertlipe@usa.net

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

#include "garmin_tables.h"
#include "jeeps/gpsmath.h"

#include <stdio.h>
#include <string.h>

#define MYNAME "garmin_tables"

/* MapSource 4.13 */
icon_mapping_t garmin_icon_table[] = {
  /*	  mps    pcx    desc */
  {   107, 16384, "Airport" },
  {    73,  8204, "Amusement Park" },
  {    55,   169, "Ball Park" },
  {     6,     6, "Bank" },
  {    13,    13, "Bar" },
  {   104,  8244, "Beach" },
  {     1,     1, "Bell" },
  {    37,   150, "Boat Ramp" },
  {    74,  8205, "Bowling" },
  {    93,  8233, "Bridge" },
  {    94,  8234, "Building" },
  {    38,   151, "Campground" },
  {    56,   170, "Car" },
  {    75,  8206, "Car Rental" },
  {    76,  8207, "Car Repair" },
  {    95,  8235, "Cemetery" },
  {    96,  8236, "Church" },
  {    65,   179, "Circle with X" },
  {    72,  8203, "City (Capitol)" },
  {    71,  8200, "City (Large)" },
  {    70,  8199, "City (Medium)" },
  {    69,  8198, "City (Small)" },
  {    69,  8198, "Small City" },
  {    97,  8237, "Civil" },
  {   119,  8262, "Contact, Afro" },
  {   120,  8272, "Contact, Alien" },
  {   121,  8258, "Contact, Ball Cap" },
  {   122,  8259, "Contact, Big Ears" },
  {   123,  8271, "Contact, Biker" },
  {   124,  8273, "Contact, Bug" },
  {   125,  8274, "Contact, Cat" },
  {   126,  8275, "Contact, Dog" },
  {   127,  8263, "Contact, Dreadlocks" },
  {   128,  8264, "Contact, Female1" },
  {   129,  8265, "Contact, Female2" },
  {   130,  8266, "Contact, Female3" },
  {   131,  8261, "Contact, Goatee" },
  {   132,  8268, "Contact, Kung-Fu" },
  {   133,  8276, "Contact, Pig" },
  {   134,  8270, "Contact, Pirate" },
  {   135,  8267, "Contact, Ranger" },
  {   136,  8257, "Contact, Smiley" },
  {   137,  8260, "Contact, Spike" },
  {   138,  8269, "Contact, Sumo" },
  {    52,   165, "Controlled Area" },
  {    89,  8220, "Convenience Store" },
  {    98,  8238, "Crossing" },
  {    51,   164, "Dam" },
  {    53,   166, "Danger Area" },
  {    87,  8218, "Department Store" },
  {     4,     4, "Diver Down Flag 1" },
  {     5,     5, "Diver Down Flag 2" },
  {    41,   154, "Drinking Water" },
  {    63,   177, "Exit" },
  {    77,  8208, "Fast Food" },
  {     7,     7, "Fishing Area" },
  {    78,  8209, "Fitness Center" },
  {    64,   178, "Flag" },
  {   105,  8245, "Forest" },
  {     8,     8, "Gas Station" },
  {   117,  8255, "Geocache" },
  {   118,  8256, "Geocache Found" },
  {    99,  8239, "Ghost Town" },
  {   113, 16393, "Glider Area" },
  {    68,  8197, "Golf Course" },
  {     2,     2, "Diamond, Green" },
  {    15,    15, "Square, Green" },
  {   108, 16388, "Heliport" },
  {     9,     9, "Horn" },
  {    57,   171, "Hunting Area" },
  {    44,   157, "Information" },
  {   100,  8240, "Levee" },
  {    12,    12, "Light" },
  {    90,  8221, "Live Theater" },
  {    59,   173, "Lodging" },
  {    59,   173, "Hotel" },
  {    20,    21, "Man Overboard" },
  {     0,     0, "Anchor" },
  {    43,   156, "Medical Facility" },
  {    66,  8195, "Mile Marker" },
  {   101,  8241, "Military" },
  {    60,   174, "Mine" },
  {    79,  8210, "Movie Theater" },
  {    80,  8211, "Museum" },
  {    21,    22, "Navaid, Amber" },
  {    22,    23, "Navaid, Black" },
  {    23,    24, "Navaid, Blue" },
  {    24,    25, "Navaid, Green" },
  {    25,    26, "Navaid, Green/Red" },
  {    26,    27, "Navaid, Green/White" },
  {    27,    28, "Navaid, Orange" },
  {    28,    29, "Navaid, Red" },
  {    29,    30, "Navaid, Red/Green" },
  {    30,    31, "Navaid, Red/White" },
  {    31,    32, "Navaid, Violet" },
  {    32,    33, "Navaid, White" },
  {    33,    34, "Navaid, White/Green" },
  {    34,    35, "Navaid, White/Red" },
  {   102,  8242, "Oil Field" },
  {   115, 16395, "Parachute Area" },
  {    46,   159, "Park" },
  {    45,   158, "Parking Area" },
  {    81,  8212, "Pharmacy" },
  {    47,   160, "Picnic Area" },
  {    82,  8213, "Pizza" },
  {    83,  8214, "Post Office" },
  {   109, 16389, "Private Field" },
  {    36,    37, "Radio Beacon" },
  {     3,     3, "Diamond, Red" },
  {    16,    16, "Square, Red" },
  {    10,    10, "Residence" },
  {    10,    10, "House" },
  {    11,    11, "Restaurant" },
  {    54,   167, "Restricted Area" },
  {    39,   152, "Restroom" },
  {    84,  8215, "RV Park" },
  {    91,  8226, "Scales" },
  {    48,   161, "Scenic Area" },
  {    85,  8216, "School" },
  {   116, 16402, "Seaplane Base" },
  {    19,    19, "Shipwreck" },
  {    58,   172, "Shopping Center" },
  {   112, 16392, "Short Tower" },
  {    40,   153, "Shower" },
  {    49,   162, "Skiing Area" },
  {    14,    14, "Skull and Crossbones" },
  {   110, 16390, "Soft Field" },
  {    86,  8217, "Stadium" },
  {   106,  8246, "Summit" },
  {    50,   163, "Swimming Area" },
  {   111, 16391, "Tall Tower" },
  {    42,   155, "Telephone" },
  {    92,  8227, "Toll Booth" },
  {    67,  8196, "TracBack Point" },
  {    61,   175, "Trail Head" },
  {    62,   176, "Truck Stop" },
  {   103,  8243, "Tunnel" },
  {   114, 16394, "Ultralight Area" },
  {   139,  8282, "Water Hydrant" },	/* new in MapSource V5 */
  {    18,    18, "Waypoint" },
  {    17,    17, "Buoy, White" },
  {    35,    36, "Dot, White" },
  {    88,  8219, "Zoo" },

  /* Custom icons.   The spec reserves 7680-8191 for the custom
   * icons on the C units, Quest, 27xx, 276, 296,  and other units.
   * Note that firmware problems on the earlier unit result in these
   * being mangled, so be sure you're on a version from at least
   * late 2005.
   * {    -2,  7680, "Custom 0" },
   * ....
   * {    -2,  8192, "Custom 511" },
   */
  /* MapSource V6.x */

  {   140,  8286, "Flag, Red" },
  {   141,  8284, "Flag, Blue" },
  {   142,  8285, "Flag, Green" },
  {   143,  8289, "Pin, Red" },
  {   144,  8287, "Pin, Blue" },
  {   145,  8288, "Pin, Green" },
  {   146,  8292, "Block, Red" },
  {   147,  8290, "Block, Blue" },
  {   148,  8291, "Block, Green" },
  {   149,  8293, "Bike Trail" },
  {   150,   181, "Fishing Hot Spot Facility" },
  {   151,  8249, "Police Station"},
  {   152,  8251, "Ski Resort" },
  {   153,  8252, "Ice Skating" },
  {   154,  8253, "Wrecker" },
  {   155,   184, "Anchor Prohibited" },
  {   156,   185, "Beacon" },
  {   157,   186, "Coast Guard" },
  {   158,   187, "Reef" },
  {   159,   188, "Weed Bed" },
  {   160,   189, "Dropoff" },
  {   161,   190, "Dock" },
  {   162,   191, "Marina" },
  {   163,   192, "Bait and Tackle" },
  {   164,   193, "Stump" },

  /* New in Garmin protocol spec from June 2006.  Extracted from
   * spec and fed through some horrible awk to add ones we didn't
   * have before but normalized for consistency. */
  { -1,  8359, "Asian Food" },
  { 167,  8296, "Circle, Blue" },
  { 168,  8299, "Diamond, Blue" },
  { 178,  8317, "Letter A, Blue" },
  { 181,  8318, "Letter B, Blue" },
  { 184,  8319, "Letter C, Blue" },
  { 187,  8320, "Letter D, Blue" },
  { 190,  8341, "Number 0, Blue" },
  { 193,  8342, "Number 1, Blue" },
  { 196,  8343, "Number 2, Blue" },
  { 199,  8344, "Number 3, Blue" },
  { 202,  8345, "Number 4, Blue" },
  { 205,  8346, "Number 5, Blue" },
  { 208,  8347, "Number 6, Blue" },
  { 211,  8348, "Number 7, Blue" },
  { 214,  8349, "Number 8, Blue" },
  { 217,  8350, "Number 9, Blue" },
  { 171,  8302, "Oval, Blue" },
  { 174,  8305, "Rectangle, Blue" },
  { 175,  8308, "Square, Blue" },
  { 218,  8351, "Triangle, Blue" },
  { -1,  8254, "Border Crossing (Port Of Entry)" },
  { -1,   182, "Bottom Conditions" },
  { -1,  8360, "Deli" },
  { -1,  8228, "Elevation point" },
  { -1,  8229, "Exit without services" },
  { -1, 16398, "First approach fix" },
  { -1,  8250, "Gambling/casino" },
  { -1,  8232, "Geographic place name, land" },
  { -1,  8230, "Geographic place name, Man-made" },
  { -1,  8231, "Geographic place name, water" },
  { 166,  8295, "Circle, Green" },
  { 177,  8313, "Letter A, Green" },
  { 180,  8315, "Letter B, Green" },
  { 183,  8314, "Letter C, Green" },
  { 186,  8316, "Letter D, Green" },
  { 189,  8331, "Number 0, Green" },
  { 192,  8332, "Number 1, Green" },
  { 195,  8333, "Number 2, Green" },
  { 198,  8334, "Number 3, Green" },
  { 201,  8335, "Number 4, Green" },
  { 204,  8336, "Number 5, Green" },
  { 207,  8337, "Number 6, Green" },
  { 210,  8338, "Number 7, Green" },
  { 213,  8339, "Number 8, Green" },
  { 216,  8340, "Number 9, Green" },
  { 170,  8301, "Oval, Green" },
  { 173,  8304, "Rectangle, Green" },
  { 219,  8352, "Triangle, Green" },
  { -1, 16385, "Intersection" },
  { -1,  8201, "Intl freeway hwy" },
  { -1,  8202, "Intl national hwy" },
  { -1,  8361, "Italian food" },
  { -1,  8248, "Large exit without services" },
  { -1,  8247, "Large Ramp intersection" },
  { -1, 16399, "Localizer Outer Marker" },
  { -1, 16400, "Missed approach point" },
  { -1, 16386, "Non-directional beacon" },
  { -1,   168, "Null" },
  { -1,   180, "Open 24 Hours" },
  { -1,  8222, "Ramp intersection" },
  { 165,  8294, "Circle, Red" },
  { 176,  8309, "Letter A, Red" },
  { 179,  8310, "Letter B, Red" },
  { 182,  8311, "Letter C, Red" },
  { 185,  8312, "Letter D, Red" },
  { 188,  8321, "Number 0, Red" },
  { 191,  8322, "Number 1, Red" },
  { 194,  8323, "Number 2, Red" },
  { 197,  8324, "Number 3, Red" },
  { 200,  8325, "Number 4, Red" },
  { 203,  8326, "Number 5, Red" },
  { 206,  8327, "Number 6, Red" },
  { 209,  8328, "Number 7, Red" },
  { 212,  8329, "Number 8, Red" },
  { 215,  8330, "Number 9, Red" },
  { 169,  8300, "Oval, Red" },
  { 172,  8303, "Rectangle, Red" },
  { 220,  8353, "Triangle, Red" },
  { -1,  8362, "Seafood" },
  { -1,  8194, "State Hwy" },
  { -1,  8363, "Steak" },
  { -1,  8223, "Street Intersection" },
  { -1, 16401, "TACAN" },
  { -1,   183, "Tide/Current PRediction Station" },
  { -1,   191, "U Marina" },
  { -1,  8193, "US hwy" },
  { -1,   193, "U stump" },
  { -1, 16387, "VHF Omni-range" },
  { -1, 16397, "VOR-DME" },
  { -1, 16396, "VOR/TACAN" },

  /* This block new on 1/15 from the Mapsource 6.12 beta */
  { 221, -1, "Contact, Blonde" },
  { 222, -1, "Contact, Clown" },
  { 223, -1, "Contact, Glasses" },
  { 224, -1, "Contact, Panda" },
  { 225, -1, "Multi-Cache" },
  { 226, -1, "Letterbox Cache" },
  { 227, -1, "Puzzle Cache" },
  { 228, -1, "Library" },
  { 229, -1, "Ground Transportation" },
  { 230, -1, "City Hall" },
  { 231, -1, "Winery" },
  { 232, -1, "ATV" },
  { 233, -1, "Big Game" },
  { 234, -1, "Blind" },
  { 235, -1, "Blood Trail" },
  { 236, -1, "Cover" },
  { 237, -1, "Covey" },
  { 238, -1, "Food Source" },
  { 239, -1, "Furbearer" },
  { 240, -1, "Lodge" },
  { 241, -1, "Small Game" },
  { 242, -1, "Animal Tracks" },
  { 243, -1, "Treed Quarry" },
  { 244, -1, "Tree Stand" },
  { 245, -1, "Truck" },
  { 246, -1, "Upland Game" },
  { 247, -1, "Waterfowl" },
  { 248, -1, "Water Source" },


  {    -1,    -1, NULL },
};

icon_mapping_t garmin_smart_icon_table[] = {
  /* Additional (optional, activated with -Si) icons */
  {    92,  8227, "Micro-Cache" },   	/* icon for "Toll Booth" */
  {    48,   161, "Virtual cache" }, 	/* icon for "Scenic Area" */
  {    86,  8217, "Multi-Cache" },   	/* icon for "Stadium" */
  {    44,   157, "Unknown Cache" },   	/* icon for "Information" */
  {    64,   178, "Locationless (Reverse) Cache" }, /* Icon for "Flag" */
  {    83,  8214, "Post Office" },  	/* Icon for "Post Office" */
  {    47,   160, "Event Cache" }, 	/* Icon for "Event" */
  {    90,  8221, "Webcam Cache" }, 	/* Icon for "Live Theatre" */
  {    -1,    -1, NULL }
};

/* ICAO coutry code table */

/* source: http://en.wikipedia.org/wiki/ICAO_airport_code */

gt_country_code_t gt_country_codes[] = {
  { "ZM,", "Mongolia" },
  { "ZK,", "North Korea" },
  { "Z*,", "China" },
  { "Y*,", "Australia" },
  { "WS,", "Singapore" },
  { "WM,", "Brunei/Malaysia" },
  { "WB,", "Malaysia" },
  { "WA,WI,WQ,WR,", "Indonesia" },
  { "VV,", "Vietnam" },
  { "VT,", "Thailand" },
  { "VR,", "Maldives" },
  { "VQ,", "Bhutan" },
  { "VN,", "Nepal" },
  { "VM,", "Macau" },
  { "VL,", "Laos" },
  { "VH,", "Hong Kong" },
  { "VG,", "Bangladesh" },
  { "VD,", "Kampuchea" },
  { "VC,", "Sri Lanka" },
  { "VB,VY,", "Myanmar/Burma" },
  { "VA,VE,VI,VO,", "India" },
  { "UR,", "Kazakhstan/Russia" },
  { "UT,", "Kazakhstan/Tadzhikistan/Turkmenistan/Uzbekistan" },
  { "UM,", "Belorussia/Russia" },
  { "UK,", "Ukraine" },
  { "UB,", "Azerbaijan" },
  { "UA,", "Kazakhstan/Kirgizia" },
  { "U*,", "Russia" },
  { "TX,", "Bermuda" },
  { "TV,", "St Vincent and the Grenadines" },
  { "TU,", "British Virgin Islands" },
  { "TT,", "Trinidad and Tobago" },
  { "TR,", "Montserrat Island" },
  { "TQ,", "Anguilla" },
  { "TN,", "Aruba/Neth Antilles" },
  { "TL,", "St Lucia" },
  { "TK,", "St Kitts/Nevis Islands" },
  { "TJ,", "Puerto Rico" },
  { "TG,", "Grenada" },
  { "TF,", "Guadeloupe/Martinique" },
  { "TD,", "Dominica" },
  { "TB,", "Barbados" },
  { "TA,", "Antigua" },
  { "SY,", "Guyana" },
  { "SV,", "Venezuela" },
  { "SU,", "Uruguay" },
  { "SP,", "Peru" },
  { "SO,", "French Guiana" },
  { "SM,", "Suriname" },
  { "SL,", "Bolivia" },
  { "SK,", "Colombia/San Andres" },
  { "SG,", "Paraguay" },
  { "SF,", "Falkland Islands" },
  { "SE,", "Ecuador" },
  { "SC,", "Chile/Easter Island" },
  { "SB,SD,SN,SS,SW,", "Brazil" },
  { "SA,", "Argentina" },
  { "S1,", "Antarctica (Argentina/Chile)" },
  { "RP,", "Philippines" },
  { "RK,", "South Korea" },
  { "RJ,", "Japan" },
  { "RC,", "Taiwan" },
  { "PW,", "Wake Island" },
  { "PT,", "Caroline Islands/Micronesia/Palau" },
  { "PM,", "Midway Islands" },
  { "PK,", "Marshall Islands" },
  { "PJ,", "Johnston Atoll" },
  { "PG,", "Guam/Mariana Islands/Northern Mariana Islands" },
  { "PC,", "Kiribati" },
  { "P",   "Oakland Octa" },
  { "OY,", "Yemen Arab Rep" },
  { "OT,", "Qatar" },
  { "OS,", "Syria" },
  { "OR,", "Iraq" },
  { "OP,", "Pakistan" },
  { "OO,", "Oman" },
  { "OM,", "United Arab Emirates" },
  { "OL,", "Lebanon" },
  { "OK,", "Kuwait" },
  { "OJ,", "Jordan" },
  { "OI,", "Iran" },
  { "OE,", "Saudi Arabia" },
  { "OB,", "Bahrain" },
  { "OA,", "Afghanistan" },
  { "NZ,PL,", "New Zealand" },
  { "NW,", "New Caledonia" },
  { "NV,", "Vanuatu" },
  { "NT,", "French Polynesia/Society Islands/Tuamotu Islands" },
  { "NS,", "American Samoa/Western Samoa" },
  { "NL,", "Futuna Island/Wallis Island" },
  { "NI,", "Niue" },
  { "NG,", "Kiribati/Tuvalu" },
  { "NF,", "Fiji Island/Tonga" },
  { "NC,", "Cook Islands" },
  { "MZ,", "Belize" },
  { "MY,", "Bahamas" },
  { "MW,", "Cayman Islands" },
  { "MU,", "Cuba" },
  { "MT,", "Haiti" },
  { "MS,", "El Salvador" },
  { "MR,", "Costa rica" },
  { "MP,", "Panama" },
  { "MN,", "Nicaragua" },
  { "MM,", "Mexico" },
  { "MK,", "Jamaica" },
  { "MI,TI,", "Virgin Islands (U.S.)" },
  { "MH,", "Honduras" },
  { "MG,", "Guatemala" },
  { "MD,", "Dominican Republic" },
  { "MB,", "Turks Island/Caicos Island" },
  { "LZ,", "Slovakia" },
  { "LY,", "Yugoslavia" },
  { "LX,", "Gibraltar" },
  { "LW,", "Macedonia" },
  { "LV,", "Gaza" },
  { "LU,", "Moldova" },
  { "LT,", "Turkey" },
  { "LS,", "Switzerland" },
  { "LR,", "Romania" },
  { "LQ,", "Bosnia-Herzegovina" },
  { "LP,", "Portugal/Azores/Madeira Islands" },
  { "LO,", "Austria" },
  { "LN,", "Monaco" },
  { "LM,", "Malta" },
  { "LL,", "Israel/Jerusalem" },
  { "LK,", "Czech" },
  { "LI,", "Italy" },
  { "LH,", "Hungary" },
  { "LG,", "Slovenia" },
  { "LG,", "Greece" },
  { "LF,", "France" },
  { "LF,", "Miquelon Island/St Pierre Island" },
  { "LE,", "Spain" },
  { "LD,", "Croatia" },
  { "LC,", "Cyprus/Turkey (Northern Cyprus)" },
  { "LB,", "Bulgaria" },
  { "LA,", "Albania" },
  { "K*,X*,PA,PB,PF,PJ,PL,PM,PO,PP,PH,PW,", "United States of America" },
  { "HU,", "Uganda" },
  { "HT,", "Tanzania" },
  { "HS,", "Sudan" },
  { "HR,", "Rwanda" },
  { "HL,", "Libya, Spa Jamahiriya" },
  { "HK,", "Kenya" },
  { "HH,", "Eritrea" },
  { "HE,", "Egypt" },
  { "HD,HF,", "Djibouti" },
  { "HC,", "Somalia" },
  { "HB,", "Burundi" },
  { "HA,", "Ethiopia" },
  { "GV,", "Cape Verde" },
  { "GU,", "Guinea Tepublic" },
  { "GQ,", "Mauritania" },
  { "GO,", "Senegal" },
  { "GM,", "Morocco/Ad Dakhla/La'Youn" },
  { "GL,", "Liberia" },
  { "GG,", "Guinea-Bissau" },
  { "GF,", "Sierra Leone" },
  { "GE,", "Melilla" },
  { "GC,", "Canary Island" },
  { "GB,", "Gambia" },
  { "GA,", "Mali" },
  { "FZ,", "Democratic Republic of Congo" },
  { "FY,", "Namibia" },
  { "FX,", "Lesotho" },
  { "FW,", "Malawi" },
  { "FV,", "Zimbabwe" },
  { "FT,", "Chad" },
  { "FS,", "Seychelles" },
  { "FQ,", "Mozambique" },
  { "FP,", "Sao Tome & Principe" },
  { "FO,", "Gabon" },
  { "FN,", "Angola" },
  { "FM,", "Madagascar/Comoros/Reunion/Mayotte Islands" },
  { "FL,", "Zambia" },
  { "FK,", "Cameroon" },
  { "FJ,", "Chagos Archipelago/British Indian Ocean Territory" },
  { "FI,", "Mauritius" },
  { "FH,", "Ascension Island/St Helena Island" },
  { "FG,", "Equitorial Guinea" },
  { "FE,", "Central African Republic" },
  { "FD,", "Swaziland" },
  { "FC,", "Congo" },
  { "FB,", "Botswana" },
  { "FA,", "South African Republic" },
  { "EY,", "Lithuania" },
  { "EV,", "Latvia" },
  { "ES,", "Sweden" },
  { "EP,", "Poland" },
  { "EN,", "Norway" },
  { "EL,", "Luxembourg" },
  { "EK,", "Denmark/Faroe Island" },
  { "EI,", "Ireland" },
  { "EH,", "Netherlands" },
  { "EG,LX,", "United Kingdom" },
  { "EF,", "Finland" },
  { "EE,", "Estonia" },
  { "ED,ET,", "Germany" },
  { "EB,", "Belgium" },
  { "DX,", "Togo" },
  { "DT,", "Tunisia" },
  { "DR,", "Niger" },
  { "DN,", "Nigeria" },
  { "DI,", "Ivory Coast" },
  { "DG,", "Ghana" },
  { "DF,", "Burkina Faso" },
  { "DB,", "Benin" },
  { "DA,", "Algeria" },
  { "C*,", "Canada" },
  { "BI,", "Iceland" },
  { "BG,", "Greenland" },
  { "AY,", "Papua New Guinea" },
  { "AN,", "Nauru" },
  { "AG,", "Solomon Island" },
  { NULL, NULL }
};

/* gt_waypt_classes: gdb internal order */
const char* gt_waypt_class_names[] = {
  "User Waypoint",
  "Airport",
  "Intersection",
  "NDB",
  "VOR",
  "Runway Threshold",
  "Airport Intersection",
  "Airport NDB",
  "Map Point",
  "Map Area",
  "Map Intersection",
  "Map Address",
  "Map Line",
  NULL
};

/* gt_display_mode_names: this order is used by most devices */
const char* gt_display_mode_names[] = {
  "Symbol & Name",
  "Symbol",
  "Symbol & Description"
};

typedef struct {
  const char* shortname;
  const char* longname;
  grid_type grid;
} grid_mapping_t;

/* gt_mps_grid_names: !!! degree sign substituted with '*' !!! */

grid_mapping_t gt_mps_grid_names[] = {
  { "ddd",	"Lat/Lon hddd.ddddd*",		grid_lat_lon_ddd },
  { "dmm",	"Lat/Lon hddd*mm.mmm'",		grid_lat_lon_dmm },
  { "dms",	"Lat/Lon hddd*mm'ss.s\"",	grid_lat_lon_dms },
  { "bng",	"British National Grid",	grid_bng },
  { "utm",	"UTM",				grid_utm },
  { "swiss",	"Swiss grid",			grid_swiss },
  { NULL,	NULL,	(grid_type) 0 }
};

/* gt_mps_datum_names: */

typedef struct {
  const char* jeeps_name;
  const char* mps_name;
} datum_mapping_t;

/* will be continued (when requested) */
static datum_mapping_t gt_mps_datum_names[] = {
  { "Alaska-NAD27",	"NAD27 Alaska" },
  { "Bahamas NAD27",	"NAD27 Bahamas" },
  { "Canada_Mean(NAD27)",	"NAD27 Canada" },
  { "Canal_Zone_(NAD27)",	"NAD27 Canal Zone" },
  { "Carribean NAD27",	"NAD27 Caribbean" },
  { "Cent America NAD27",	"NAD27 Central" },
  { "Cuba NAD27",		"NAD27 Cuba" },
  { "Geodetic Datum 49",	"Geodetic Datum '49" },
  { "Greenland NAD27",	"NAD27 Greenland" },
  { "Mexico NAD27",	"NAD27 Mexico" },
  { "North America 83",	"NAD83" },
  { "OSGB36",		"Ord Srvy Grt Britn" },
  { NULL,	NULL }
};

typedef struct garmin_color_s {
  const char* name;
  int32_t rgb;
} garmin_color_t;

static garmin_color_t gt_colors[] = {
  { "Unknown",		unknown_color },
  { "Black", 		0x000000 },
  { "DarkRed",		0x00008B },
  { "DarkGreen",		0x006400 },
  { "DarkYellow",		0x008B8B },
  { "DarkBlue",		0x8B0000 },
  { "DarkMagenta",	0x8B008B },
  { "DarkCyan",		0x8B8B00 },
  { "LightGray",		0xD3D3D3 },
  { "DarkGray",		0xA9A9A9 },
  { "Red",		0x0000FF },
  { "Green",		0x008000 },
  { "Yellow",		0x00FFFF },
  { "Blue",		0xFF0000 },
  { "Magenta",		0xFF00FF },
  { "Cyan",		0xFFFF00 },
  { "White",		0xFFFFFF },
  { "Transparent",	unknown_color }, /* Currently not handled */
  { NULL }
};

#define GT_COLORS_CT ((sizeof(gt_colors) / sizeof(gt_colors[0])) - 1)

unsigned char
gt_switch_display_mode_value(const unsigned char display_mode, const int protoid, const char device)
{
  if (device) {
    switch (protoid) {

    case 103:
    case 107:
    case 108:
    case 109:
    case 110:
      return display_mode & 3;
    case 104:
      switch (display_mode) {
      case 0:
      case 1:
        return gt_display_mode_symbol;
      case 3:
        return gt_display_mode_symbol_and_name;
      case 5:
        return gt_display_mode_symbol_and_comment;
      }
    case 155:
      switch (display_mode) {
      case 1:
        return gt_display_mode_symbol;
      case 3:
        return gt_display_mode_symbol_and_name;
      case 5:
        return gt_display_mode_symbol_and_comment;
      }
    }
    return gt_display_mode_symbol_and_name;
  } else {
    switch (protoid) {

    case 103:
    case 107:
    case 108:
    case 109:
    case 110:
      return display_mode & 3;
    case 104:
    case 155:
      switch (display_mode) {
      case gt_display_mode_symbol:
        return 1;
      case gt_display_mode_symbol_and_name:
        return 3;
      case gt_display_mode_symbol_and_comment:
        return 5;
      }
    }
    return 0;
  }
}

const char*
gt_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format, int* dynamic)
{
  icon_mapping_t* i;
  char custom[] = "Custom 63 ";

  if ((garmin_format == GDB) && (icon >= 500) && (icon <= 563)) {
    snprintf(custom, sizeof(custom), "Custom %d", icon - 500);
    *dynamic = 1;
    return xstrdup(custom);
  }

  if ((garmin_format == PCX) && (icon >= 7680) && (icon <= 8191)) {
    snprintf(custom, sizeof(custom), "Custom %d", icon - 7680);
    *dynamic = 1;
    return xstrdup(custom);
  }

  if (dynamic) {
    *dynamic = 0;
  }

  for (i = garmin_icon_table; i->icon; i++) {
    switch (garmin_format) {
    case MAPSOURCE:
    case GDB:
      if (icon == i->mpssymnum) {
        return i->icon;
      }
      break;
    case PCX:
    case GARMIN_SERIAL:
      if (icon == i->pcxsymnum) {
        return i->icon;
      }
      break;
    default:
      fatal(MYNAME ": unknown garmin format.\n");
    }
  }
  return DEFAULT_ICON_DESCR;
}

int gt_find_icon_number_from_desc(const QString& desc, garmin_formats_e garmin_format)
{
  static int find_flag = 0;
  icon_mapping_t* i;
  int def_icon = DEFAULT_ICON_VALUE;
  int n;

  if (desc.isNull()) {
    return def_icon;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return that.
   */
  n = desc.toInt();
  if (n)  {
    return n;
  }

  if (desc.startsWith("Custom ", Qt::CaseInsensitive)) {
    int base = 0;
    if (garmin_format == GDB) {
      base = 500;
    }
    if (garmin_format == PCX) {
      base = 7680;
    }
    if (base) {
      n = desc.mid(7).toInt();
      return n + base;
    }
  }

  for (i = garmin_smart_icon_table; global_opts.smart_icons && i->icon; i++) {
    if (desc.compare(i->icon, Qt::CaseInsensitive) == 0) {
      switch (garmin_format) {
      case MAPSOURCE:
      case GDB:
        return i->mpssymnum;
      case PCX:
      case GARMIN_SERIAL:
        return i->pcxsymnum;
      default:
        fatal(MYNAME ": unknown garmin format.\n");
      }
    }
  }
  for (i = garmin_icon_table; i->icon; i++) {
    if (desc.compare(i->icon, Qt::CaseInsensitive) == 0) {
      switch (garmin_format) {
      case MAPSOURCE:
      case GDB:
        return i->mpssymnum;
      case PCX:
      case GARMIN_SERIAL:
        return i->pcxsymnum;
      default:
        fatal(MYNAME ": unknown garmin format.\n");
      }
    }
  }

  /*
   *  try to handle some complex icon names: i.e. "Blue Diamond" and "Diamond, Blue"
   *  "find_flag" prevents us from a possible endless loop
   */

  if (find_flag == 0) {
    const char** prefix;
    const char* prefixes[] = {
      "White ", "Red ", "Green ", "Blue ", "Black ", NULL
    };
    // Rewrite "Green Square" to "Square, Green".
    for (prefix = prefixes; *prefix != NULL; prefix++) {
      if (desc.startsWith(*prefix, Qt::CaseInsensitive)) {
        QString buff = desc;
        buff.replace(*prefix, "");
        buff.append(", ");
        buff.append(*prefix);
        buff = buff.trimmed();

        find_flag = 1;
        int result = gt_find_icon_number_from_desc(buff, garmin_format);
        find_flag = 0;

        return result;
      }
    }
  }
  return def_icon;
}

const char*
gt_get_icao_country(const QString& cc)
{
  gt_country_code_t* x = &gt_country_codes[0];

  if (cc.isEmpty()) {
    return NULL;
  }

  do {
    const char* ccx = x->cc;
    const QString qccx = x->cc;
    while (ccx != NULL) {
      if (qccx.left(2) == cc) {
        return x->country;
      }
      if ((ccx[0] == cc[0]) && (ccx[1] == '*')) {
        return x->country;
      }
      ccx = strchr(ccx, ',');
      if (ccx == NULL) {
        break;
      }
      ccx++;
    }
    x++;
  } while (x->cc != NULL);
  return NULL;
}

const char*
gt_get_icao_cc(const QString& country, const QString& shortname)
{
  static char res[3];
  gt_country_code_t* x = &gt_country_codes[0];

  if (country.isEmpty()) {
    const char* test;
    if (shortname == NULL) {
      return NULL;
    }
    switch (shortname.length()) {
    case 3:
      strncpy(res, CSTR(shortname), 1);
      break;
    case 4:
      strncpy(res, CSTR(shortname), 2);
      break;
    default:
      return NULL;
    }
    test = gt_get_icao_country(res);
    if (test != NULL) {
      return res;
    } else {
      return NULL;
    }
  }

  do {
    if (country.compare(x->country, Qt::CaseInsensitive) != 0) {
      x++;
      continue;
    }

    if (strlen(x->cc) <= 3) {
      strncpy(res, x->cc, 3);
      if (res[1] == '*') {
        res[1] = '\0';
      } else {
        res[2] = '\0';
      }
      return res;
    }
    if (shortname.length() == 4) {
      const char* ccx = x->cc;

      strncpy(res, CSTR(shortname), 2);
      res[2] = '\0';
      while (ccx != NULL) {
        if (strncmp(ccx, res, 2) == 0) {
          return res;
        }
        if ((ccx[0] == res[0]) && (ccx[1] == '*')) {
          return res;
        }
        ccx = strchr(ccx, ',');
        if (ccx == NULL) {
          break;
        }
        ccx++;
      }
    }
    return NULL;
  } while (x->country != NULL);
  return NULL;
}

grid_type
gt_lookup_grid_type(const char* grid_name, const char* module)
{
  grid_mapping_t* g;

  for (g = gt_mps_grid_names; (g->shortname); g++) {
    if ((case_ignore_strcmp(grid_name, g->shortname) == 0) ||
        (case_ignore_strcmp(grid_name, g->longname) == 0)) {
      return g->grid;
    }
  }

  fatal("%s: Unsupported grid (%s)! See GPSBabel help for supported grids.\n",
        module, grid_name);

  return grid_unknown;	/* (warnings) */
}

const char*
gt_get_mps_grid_longname(const grid_type grid, const char* module)
{
  if ((grid < GRID_INDEX_MIN) || (grid > GRID_INDEX_MAX))
    fatal("%s: Grid index out of range %d (%d..%d)!",
          module, (int) grid,
          (int)GRID_INDEX_MIN, (int)GRID_INDEX_MAX);
  return gt_mps_grid_names[grid].longname;
}

const char*
gt_get_mps_datum_name(const int datum_index)
{
  const char* result;
  datum_mapping_t* d;

  result = GPS_Math_Get_Datum_Name(datum_index);

  for (d = gt_mps_datum_names; (d->jeeps_name); d++)
    if (case_ignore_strcmp(result, d->jeeps_name) == 0) {
      return d->mps_name;
    }

  return result;
}

int
gt_lookup_datum_index(const char* datum_str, const char* module)
{
  datum_mapping_t* d;
  int result;
  const char* name = datum_str;

  for (d = gt_mps_datum_names; (d->jeeps_name); d++) {
    if (case_ignore_strcmp(name, d->mps_name) == 0) {
      name = d->jeeps_name;
      break;
    }
  }

  result = GPS_Lookup_Datum_Index(name);

  if (result < 0) {
    char* tmp;
    xasprintf(&tmp, "%s mean", datum_str);
    result = GPS_Lookup_Datum_Index(tmp);
    xfree(tmp);
  }

  is_fatal(result < 0,
           "%s: Unsupported datum (%s)! See GPSBabel help for supported datums.",
           module, datum_str);

  return result;
}

uint32_t
gt_color_value(const unsigned int garmin_index)
{
  if ((garmin_index < GT_COLORS_CT)) {
    return gt_colors[garmin_index].rgb;
  } else {
    return unknown_color;  /* -1 */
  }
}

uint32_t
gt_color_value_by_name(const char* name)
{
  unsigned int i;

  for (i = 0; i < GT_COLORS_CT; i++)
    if (case_ignore_strcmp(gt_colors[i].name, name) == 0) {
      return gt_colors[i].rgb;
    }

  return gt_colors[0].rgb;
}

int
gt_color_index_by_name(const char* name)
{
  unsigned int i;

  for (i = 0; i < GT_COLORS_CT; i++)
    if (case_ignore_strcmp(name, gt_colors[i].name) == 0) {
      return i;
    }

  return 0; /* unknown */
}

int
gt_color_index_by_rgb(const int rgb)
{
  unsigned int i;

  for (i = 0; i < GT_COLORS_CT; i++)
    if (rgb == gt_colors[i].rgb) {
      return i;
    }

  return 0; /* unknown */
}

const char*
gt_color_name(const unsigned int garmin_index)
{
  if ((garmin_index < GT_COLORS_CT)) {
    return gt_colors[garmin_index].name;
  } else {
    return gt_colors[0].name;
  }
}

#if MAKE_TABLE

/*
 * Used to generate icon tables in appendix.
 * cc -DMAKE_TABLE garmin_tables.c fatal.o util.o globals.o  -lm
 */

int cet_utf8_to_ucs4(const char* str, int* bytes, int* value)
{
  fatal("Should not be here.");
}


int
sortem(const void* a, const void* b)
{
  const icon_mapping_t* aa = a;
  const icon_mapping_t* bb = b;

//	return aa->mpssymnum - bb->mpssymnum;
  return strcmp(aa->icon, bb->icon);

}

main()
{
  icon_mapping_t* i;
  qsort(garmin_icon_table, sizeof(garmin_icon_table) / sizeof(garmin_icon_table[0]) - 1,  sizeof(garmin_icon_table[0]), sortem);
  for (i = garmin_icon_table; i->icon; i++) {
//		printf("%03d\t%s\n", i->mpssymnum, i->icon);
    printf("<member>%s</member>\n", i->icon);
  }
}
#endif
