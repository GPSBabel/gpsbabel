/*
    Locosys NaviGPS GT-31/BGT-31 common functions.

    Copyright (C) 2008  Rodney Lorrimar <rodney@rodney.id.au>
    Copyright (C) 2005  Robert Lipe, robertlipe@usa.net

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

#ifndef NAVILINK_H_INCLUDED_
#define NAVILINK_H_INCLUDED_

#include <ctime>
#include "defs.h"

#define SBP_RECORD_LEN 32

/* defined in navilink.c */
Waypoint* navilink_decode_logpoint(const unsigned char* buffer);
unsigned navilink_checksum_packet(const unsigned char* packet, unsigned length);

/* defined in sbn.c */
bool locosys_decode_file_id(char* header, size_t len);


#ifdef THIS_IS_ONLY_FOR_REFERENCE
/* Locosys SBP and SBN structures */

typedef __packed struct {
  UINT8 HDOP;        /* HDOP [0..51] with resolution 0.2 */
  UINT8 SVIDCnt;        /* Number of SVs in solution [0 to 12] */
  UINT16 UtcSec;        /* UTC Second [0 to 59] in seconds with resolution 0.001 */
  UINT32 date_time_UTC_packed; /* refer to protocol doc*/
  UINT32 SVIDList;    /* SVs in solution:  Bit 0=1: SV1, Bit 1=1: SV2, ... , Bit 31=1: SV32 */
  INT32 Lat;            /* Latitude [-90 to 90] in degrees with resolution 0.0000001 */
  INT32 Lon;            /* Longitude [-180 to 180] in degrees with resolution 0.0000001 */
  INT32 AltCM;            /* Altitude from Mean Sea Level in centi meters */
  UINT16 Sog;            /* Speed Over Ground in m/sec with resolution 0.01 */
  UINT16 Cog;            /* Course Over Ground [0 to 360] in degrees with resolution 0.01 */
  INT16 ClmbRte;        /* Climb rate in m/sec with resolution 0.01 */
  UINT8 bitFlags;     /* bitFlags, default 0x00,    bit 0=1 indicate the first point after power on */
  UINT8 reserved;
} T_SBP;

typedef struct __packed {
  UINT8 Mid;
  UINT16 Valid;
  UINT16 Mode;            /* Nav Mode:  bit map as follows:
                            *  Bits 2-0:  GPS Fix Type
                            *    000   =  No Nav
                            *    001   =  1 SV solution
                            *    010   =  2 SV solution
                            *    011   =  3 SV solution (2D)
                            *    100   =  4 or more SV solution (3D)
                            *    101   =  Least Square 2D solution
                            *    110   =  Least Square 3D solution
                            *    111   =  DR solution (no SV)
                            *  Bit  3  :  1 = Trickle Power ON
                            *  Bits 5-4:  Altitude Hold
                            *    00    =  No Altitude Hold
                            *    01    =  Altitude Hold using altitude from KF
                            *    10    =  Altitude Hold using altitude from user
                            *    11    =  forced altitude
                            *  Bit 6   :  1 = DOP exceeded
                            *  Bit 7   :  1 = DGPS correction applied
                            *  Bit 8   :
                            *    1     = Sensor-based DR
                            *    0     = Velocity DR, if bits 2-0 = 111
                            *          = see bits 15-14, if bits 2-0 != 111
                            *  Bit 9   :  1 = Solution validated
                            *  Bit 10  :  1 = Velocity DR Timeout
                            *  Bit 11  :  1 = Solution edited by UI
                            *  Bit 12  :  1 = Velocity invalid
                            *  Bit 13  :  1 = Altitude Hold disabled
                            *  Bits 15-14: SiRFDrive DR Status (applicable only when bit 8=0)
                            *    00    =  GPS Only
                            *    01    =  Calibrating
                            *    10    =  DR sensor error
                            *    11    =  DR Test mode
                            */
  UINT16 Week;            /* Extended Week Number */
  UINT32 TOW;             /* Time of Week [0 to 604800] in seconds with resolution 0.001 */
  UINT16 UtcYr;           /* UTC Year   [1980 to 3000] */
  UINT8  UtcMth;          /* UTC Month  [1 to 12] */
  UINT8  UtcDay;          /* UTC Day    [1 to 31] */
  UINT8  UtcHr;           /* UTC Hour   [0 to 23] */
  UINT8  UtcMin;          /* UTC Minute [0 to 59] */
  UINT16 UtcSec;          /* UTC Second [0 to 59] in seconds with resolution 0.001 */
  UINT32 SVIDList;        /* SVs in solution:  Bit 0=1: SV1, Bit 1=1: SV2, ... , Bit 31=1: SV32 */

  INT32  Lat;             /* Latitude [-90 to 90] in degrees with resolution 0.0000001 */
  INT32  Lon;             /* Longitude [-180 to 180] in degrees with resolution 0.0000001 */
  INT32  AltE;            /* Altitude from Ellipsoid in meters with resolution 0.01 */
  INT32  AltM;            /* Altitude from Mean Sea Level in meters with resolution 0.01 */
  UINT8  Datum;           /* Map datum */
  UINT16 Sog;             /* Speed Over Ground in m/sec with resolution 0.01 */
  UINT16 Cog;             /* Course Over Ground [0 to 360] in degrees with resolution 0.01 */
  INT16  MagVar;          /* Magnetic Variation - Reserved */
  INT16  ClmbRte;         /* Climb rate in m/sec with resolution 0.01 */
  INT16  HdRte;           /* Heading Rate in deg/sec with resolution 0.01 (SiRFDrive only) */
  UINT32 Ehpe;            /* Expected Horizontal Position Error in meters with resolution 0.01 */
  UINT32 Evpe;            /* Expected Horizontal Vertical Error in meters with resolution 0.01 */
  UINT32 Ete;             /* Expected Time Error in meters with resolution 0.01 (SiRFDrive only) - Reserved */
  UINT16 Ehve;            /* Expected Horizontal Velocity Error in m/sec with resolution 0.01 (SiRFDrive only) */
  INT32  ClkBias;         /* Clock Bias in meters with resolution 0.01 */
  UINT32 ClkBiasE;        /* Clock Bias Error in meters with resolution 0.01 (SiRFDrive only) */
  INT32  ClkDrift;        /* Clock Drift in m/sec with resolution 0.01 */
  UINT32 ClkDriftE;       /* Clock Drift in m/sec with resolution 0.01 (SiRFDrive only) */
  UINT32 Trvled;          /* Distance Traveled since reset in meters (SiRFDrive only) */
  UINT16 TrvledE;         /* Distance Traveled Error in meters (SiRFDrive only) */
  UINT16 HdE;             /* Heading Error [0 to 180] in degrees with resolution 0.01 (SiRFDrive only) */
  UINT8  SVIDCnt;         /* Number of SVs in solution [0 to 12] */
  UINT8  HDOP;            /* HDOP [0..51] with resolution 0.2 */
  UINT8  Reserved;        /* Reserved */

  UINT16 ufSog;             /* Speed Over Ground in m/sec with resolution 0.01 ,unfiltered*/
  UINT16 ufCog;             /* Course Over Ground [0 to 360] in degrees with resolution 0.01, unfiltered  */

}  T_SBN_REC;

#endif

#endif  // NAVILINK_H_INCLUDED
