/*
    Form GarminUSB packets to send.

    Copyright (C) 2004 Robert Lipe, robertlipe@usa.net

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

#include "gps.h"
#include <stdio.h>
#include <errno.h>
#include "garminusb.h"


void GPS_Make_Packet_usb(GPS_PPacket *packet, UC type, UC *data, int16 n)
{
    garmin_usb_packet **up = (garmin_usb_packet**) packet;

    /*
     * This is a little tacky that we're stuffing a garmin_usb_packet
     * into a GPS_PPacket, but the packet is big enough and we have only
     * a few places that really peek into this structure anyway...
     */
    memset(*up, 0, sizeof **packet);	
    (*up)->gusb_pkt.type = 0x14; /* Garmin protocol layer */
    le_write16((*up)->gusb_pkt.pkt_id, type);
    le_write32((*up)->gusb_pkt.datasz, n);
    memcpy(&(*up)->gusb_pkt.databuf, data, n);

    return;
}

int32
GPS_Write_Packet_usb(int32 fd, GPS_PPacket packet)
{
	size_t ret, sz;

    	garmin_usb_packet *gp = (garmin_usb_packet*) packet;
	sz = le_read32(gp->gusb_pkt.datasz);

	return  gusb_cmd_send(gp, sz + 12);
}
