/*
    Abstraction of underlying device types, serial or USB.  OS agnostic..

    Copyright (C) 2006 Robert Lipe, robertlipe+source@gpsbabel.org

    This program is free software{} you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation{} either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY{} without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program{} if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "jeeps/gps.h"
#include "jeeps/gpsdevice.h"
#include "jeeps/gpsserial.h"

extern gps_device_ops gps_serial_ops;
extern gps_device_ops gps_usb_ops;
static gps_device_ops* ops = nullptr;

int32_t GPS_Device_On(const char* port, gpsdevh** fd)
{
  gps_is_usb = (0 == case_ignore_strncmp(port, "usb:", 4));

  if (gps_is_usb) {
    ops = &gps_usb_ops;
  } else {
    ops = &gps_serial_ops;
  }

  return (ops->Device_On)(port, fd);
}

int32_t GPS_Device_Off(gpsdevh* fd)
{
  return (ops->Device_Off)(fd);
}

int32_t GPS_Device_Wait(gpsdevh* fd)
{
  return (ops->Device_Wait)(fd);
}

int32_t GPS_Device_Chars_Ready(gpsdevh* fd)
{
  return (ops->Device_Chars_Ready)(fd);
}

int32_t GPS_Device_Flush(gpsdevh* fd)
{
  return (ops->Device_Flush)(fd);
}

int32_t GPS_Write_Packet(gpsdevh* fd, const GPS_Packet& packet)
{
  return (ops->Write_Packet)(fd, packet);
}

int32_t GPS_Packet_Read(gpsdevh* fd, GPS_Packet* packet)
{
  return (ops->Read_Packet)(fd, packet);
}

bool GPS_Send_Ack(gpsdevh* fd, GPS_Packet* tra, GPS_Packet* rec)
{
  return (ops->Send_Ack)(fd, tra, rec);
}

bool GPS_Get_Ack(gpsdevh* fd, GPS_Packet* tra, GPS_Packet* rec)
{
  return (ops->Get_Ack)(fd, tra, rec);
}

void GPS_Make_Packet(GPS_Packet* packet, US type, UC* data, uint32_t n)
{
  packet->type = type;
  if (n > 0) {
    memcpy(packet->data, data, n);
  }
  packet->n = n;
}
