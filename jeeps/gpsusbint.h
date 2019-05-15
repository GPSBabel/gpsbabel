/*
    Definitions for internal functions of Garmin USB implementation.
    These symbols should not be publicly used.  They're "friend" functions
    of USB details internal to jeeps.

    Copyright (C) 2005, 2006 Robert Lipe, robertlipe@usa.net

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

int32 GPS_Packet_Read_usb(gpsdevh* fd, GPS_PPacket* packet, int eatbulk);
void  GPS_Make_Packet_usb(GPS_PPacket* packet, UC type, UC* data, int16 n);
int32 GPS_Write_Packet_usb(gpsdevh* fd, GPS_PPacket& packet);

