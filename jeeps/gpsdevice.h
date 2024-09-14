/*
    Abstraction of underlying device types.

    Copyright (C) 2006 Robert Lipe, robertlipe@usa.net

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

#ifndef JEEPS_GPSDEVICE_H_INCLUDED_
#define JEEPS_GPSDEVICE_H_INCLUDED_

typedef struct gpsdevh gpsdevh;

#include "jeeps/gps.h"

#define usecDELAY 180000	/* Microseconds before GPS sends A001 */


int32_t GPS_Device_Chars_Ready(gpsdevh* fd);
int32_t GPS_Device_On(const char* port, gpsdevh** fd);
int32_t GPS_Device_Off(gpsdevh* fd);
int32_t GPS_Device_Wait(gpsdevh* fd);
int32_t GPS_Device_Flush(gpsdevh* fd);
int32_t GPS_Device_Read(int32_t ignored, void* ibuf, int size);
int32_t GPS_Device_Write(int32_t ignored, const void* obuf, int size);
void   GPS_Device_Error(char* hdr, ...);
int32_t GPS_Write_Packet(gpsdevh* fd, const GPS_Packet& packet);
bool   GPS_Send_Ack(gpsdevh* fd, GPS_Packet* tra, GPS_Packet* rec);
int32_t GPS_Packet_Read(gpsdevh* fd, GPS_Packet* packet);
bool   GPS_Get_Ack(gpsdevh* fd, GPS_Packet* tra, GPS_Packet* rec);

using gps_device_op = int32_t (*)(gpsdevh*);
using gps_device_op5 = int32_t (*)(const char*, gpsdevh** fd);
using gps_device_op10 = bool (*)(gpsdevh* fd, GPS_Packet* tra, GPS_Packet* rec);
using gps_device_op12 = int32_t (*)(gpsdevh* fd, const GPS_Packet& packet);
using gps_device_op13 = int32_t (*)(gpsdevh* fd, GPS_Packet* packet);

typedef struct {
  gps_device_op5 Device_On;
  gps_device_op Device_Off;
  gps_device_op Device_Chars_Ready;
  gps_device_op Device_Wait;
  gps_device_op Device_Flush;
  gps_device_op10 Send_Ack;
  gps_device_op10 Get_Ack;
  gps_device_op13 Read_Packet;
  gps_device_op12 Write_Packet;
} gps_device_ops;

#endif /* JEEPS_GPSDEVICE_H_INCLUDED_ */
