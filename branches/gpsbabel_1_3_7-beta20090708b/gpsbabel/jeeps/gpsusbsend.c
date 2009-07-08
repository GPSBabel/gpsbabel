/*
    Form GarminUSB packets to send.

    Copyright (C) 2004, 2005, 2006 Robert Lipe, robertlipe@usa.net

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
#include "gpsusbint.h"

void 
GPS_Make_Packet_usb(GPS_PPacket *packet, UC type, UC *data, int16 n)
{
	/*
	 * For the USB case, it's a little tacky that we just copy
	 * the params into *packet, but we really don't have any manipulations
	 * to do here.   They're done in send_packet in order to keep the
	 * contents of *packet identical for the serial and USB cases.
	 */

	(*packet)->type = type;
	memcpy((*packet)->data, data, n);
	(*packet)->n = (UC) n;
	
	return;
}

int32
GPS_Write_Packet_usb(gpsdevh *dh, GPS_PPacket packet)
{
	garmin_usb_packet gp;
	memset(&gp, 0, sizeof(gp));


	/*
	 * Take the "portable" GPS_Packet data and put them into
	 * the USB packet that we will put on the wire.
	 */
	gp.gusb_pkt.type = 0x14;
    	le_write16(&gp.gusb_pkt.pkt_id, packet->type);
	le_write32(&gp.gusb_pkt.datasz, packet->n );
	memcpy(&gp.gusb_pkt.databuf, packet->data, packet->n);

	return  gusb_cmd_send(&gp, packet->n + 12);
}
