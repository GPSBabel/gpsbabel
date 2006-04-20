/*
    Garmin icon tables
    Based on information provided by Ian Cowley, Sigurd Humerfelt,
	and Garmin MapSource

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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
	{     2,     2, "Green Diamond" },
	{    15,    15, "Green Square" },
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
	{     0,     0, "Marina" },
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
	{     3,     3, "Red Diamond" },
	{    16,    16, "Red Square" },
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
	{    17,    17, "White Buoy" },
	{    35,    36, "White Dot" },
	{    88,  8219, "Zoo" },

	/* These are experimental and for the custom icons in the new "C"
	 * models.   As of this writing, firmware problems impair their 
	 * general use.   
	 * 
	 * "Quest" supports more icons than this, but other problems
	 * prohibit us from running with that model, so we stop at 24.
 	 * 
	 * Mapsource doesn't yet know how to do these, so we made the icon
	 * numbers "-2" to signify that as a problem until we can create
	 * these in a .mps or .gdb file and see their representation there.
	 */
	{    -2,  7680, "Custom 0" },
	{    -2,  7681, "Custom 1" },
	{    -2,  7682, "Custom 2" },
	{    -2,  7683, "Custom 3" },
	{    -2,  7684, "Custom 4" },
	{    -2,  7685, "Custom 5" },
	{    -2,  7686, "Custom 6" },
	{    -2,  7687, "Custom 7" },
	{    -2,  7688, "Custom 8" },
	{    -2,  7689, "Custom 9" },
	{    -2,  7690, "Custom 10" },
	{    -2,  7691, "Custom 11" },
	{    -2,  7692, "Custom 12" },
	{    -2,  7693, "Custom 13" },
	{    -2,  7694, "Custom 14" },
	{    -2,  7695, "Custom 15" },
	{    -2,  7696, "Custom 16" },
	{    -2,  7697, "Custom 17" },
	{    -2,  7698, "Custom 18" },
	{    -2,  7699, "Custom 19" },
	{    -2,  7700, "Custom 20" },
	{    -2,  7701, "Custom 21" },
	{    -2,  7702, "Custom 22" },
	{    -2,  7703, "Custom 23" },

	{    92,  8227, "Micro-Cache" },   	/* icon for "Toll Booth" */
	{    48,   161, "Virtual cache" }, 	/* icon for "Scenic Area" */
	{    86,  8217, "Multi-Cache" },   	/* icon for "Stadium" */
	{    44,   157, "Unknown Cache" },   	/* icon for "Information" */
	{    64,   178, "Locationless (Reverse) Cache" }, /* Icon for "Flag" */
	{    83,  8214, "Post Office" },  	/* Icon for "Post Office" */
	{    47,   160, "Event Cache" }, 	/* Icon for "Event" */
	{    90,  8221, "Webcam Cache" }, 	/* Icon for "Live Theatre" */

	/* MapSource V6.x */
	
	{   140,  8286, "Flag, Red" },		
	{   141,  8284, "Flag, Blue" },
	{   142,  8285, "Flag, Green" },
	{   143,  8289, "Pin, Red" },
	{   144,  8287, "Pin, Blue" },
	{   145,  8288, "Pin, Green" },
	{   146,  8292, "Diamond, Red" },
	{   147,  8290, "Diamond, Blue" },
	{   148,  8291, "Diamond, Green" },
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
	
	{    -1,    -1, NULL },
};

/* ICAO coutry code table */

/* source: http://en.wikipedia.org/wiki/ICAO_airport_code */

gt_country_code_t gt_country_codes[] = 
{
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
char *gt_waypt_class_names[] = {
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
char *gt_display_mode_names[] = {
	"Symbol & Name",
	"Symbol",
	"Symbol & Description"
};

char *
gt_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format, int *dynamic)
{
	icon_mapping_t *i;
	char custom[] = "Custom 63";

	if ((garmin_format == GDB) && (icon >= 500) && (icon <= 563))
	{
		snprintf(custom, sizeof(custom), "Custom %d", icon - 500);
		*dynamic = 1;
		return xstrdup(custom);
	}

	if (dynamic) *dynamic = 0;
	
	for (i = garmin_icon_table; i->icon; i++) {
		switch (garmin_format) {
			case MAPSOURCE:
			case GDB:
				if (icon == i->mpssymnum)
					return (char *)i->icon;
				break;
			case PCX:
			case GARMIN_SERIAL:
				if (icon == i->pcxsymnum)
					return (char *)i->icon;
				break;
			default:
				fatal(MYNAME ": unknown garmin format.\n");
		}
	}
	return DEFAULT_ICON_DESCR;
}

int gt_find_icon_number_from_desc(const char *desc, garmin_formats_e garmin_format)
{
	icon_mapping_t *i;
	int def_icon = DEFAULT_ICON_VALUE;
	int n;

	if (!desc)
		return def_icon;

	/*
	 * If we were given a numeric icon number as a description 
	 * (i.e. 8255), just return that.
	 */
	n = atoi(desc);
	if (n)  {
		return n;
	}

	if ((garmin_format == GDB) && (case_ignore_strncmp(desc, "Custom ", 7) == 0)) {
		n = atoi((char *)desc + 7);
		if ((n >= 0) && (n <= 63))
			return n+500;
	}
	for (i = garmin_icon_table; i->icon; i++) {
		if (case_ignore_strcmp(desc,i->icon) == 0) {
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
	return def_icon;
}

char *
gt_get_icao_country(const char *cc)
{
	gt_country_code_t *x = &gt_country_codes[0];

	if ((cc == NULL) || (*cc == '\0')) return NULL;

	do {
		char *ccx = x->cc;
		while (ccx != NULL) {
			if (strncmp(ccx, cc, 2) == 0) return x->country;
			if ((ccx[0] == cc[0]) && (ccx[1] == '*'))  return x->country;
			ccx = strchr(ccx, ',');
			if (ccx == NULL) break;
			ccx++;
		}
		x++;
	} while (x->cc != NULL);
	return NULL;
}

char *
gt_get_icao_cc(const char *country, const char *shortname)
{
	static char res[3];
	gt_country_code_t *x = &gt_country_codes[0];

	if ((country == NULL) || (*country == '\0')) {
		char *test;
		if (shortname == NULL) return NULL;
		switch(strlen(shortname)) {
			case 3: strncpy(res, shortname, 1); break;
			case 4: strncpy(res, shortname, 2); break;
			default: return NULL;
		}
		test = gt_get_icao_country(res);
		if (test != NULL) 
			return res;
		else
			return NULL;
	}

	do {
		if (case_ignore_strcmp(country, x->country) != 0) {
			x++;
			continue;
		}
		
		if (strlen(x->cc) <= 3) {
			strncpy(res, x->cc, 3);
			if (res[1] == '*')
				res[1] = '\0';
			else
				res[2] = '\0';
			return res;
		}
		if (shortname && (strlen(shortname) == 4)) {
			char *ccx = x->cc;
			
			strncpy(res, shortname, 2);
			res[2] = '\0';
			while (ccx != NULL) {
				if (strncmp(ccx, res, 2) == 0) return res;
				if ((ccx[0] == res[0]) && (ccx[1] == '*')) return res;
				ccx = strchr(ccx, ',');
				if (ccx == NULL) break;
				ccx++;
			}
		}
		return NULL;
	} while (x->country != NULL);
	return NULL;
}
