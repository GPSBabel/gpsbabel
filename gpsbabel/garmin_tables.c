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
