/*
    Decompose an incoming USB packet to make it look like a serial one.

    Copyright (C) 2004, 2006, 2006 Robert Lipe, robertlipe@usa.net

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
#include <ctype.h>
#include "gps.h"
#include "garminusb.h"
#include "gpsusbint.h"

/*
 * Return values are:
 * Negative on error.
 * 1 if read success - even if empty packet.
 */
int32 GPS_Packet_Read_usb(gpsdevh *dh, GPS_PPacket *packet, int eat_bulk)
{
	int32  n;
	int32 payload_size;

	garmin_usb_packet pkt;

	memset(&pkt, 0, sizeof(pkt));
do_over:
	n = gusb_cmd_get(&pkt, sizeof(pkt));

	if ( n < 0 ) {
		/*
		 * We (probably) used to have a GPS and it went away 
	 	 * while we were speaking with it.  Perhaps batteries 
		 * died or it was unplugged or something.
		 */
		gps_errno = PROTOCOL_ERROR;
		return n;
	}

	/*
	 * This is a horrible hack for 276/296.   This family sometimes 
	 * switches between bulk and interrupt on EVERY packet.   Rather 
	 * than bother all the callers with that bit of unpleasantness, 
	 * silently consume zero byte "switch back to intr"  packets here.
	 *
	 * The one caller that doesn't want this hidden is device discovery
	 * in the A000 handler.
	 */
	if ((n == 0) && eat_bulk)  {
		goto do_over;
	}
	
	/* 
	 * Populate members of serial packet from USB packet.   The
	 * copy here seems wasteful, but teaching all the callers about
	 * a structure with the "data" member being in a different place 
	 * (Since the protocol packets was badly exposed in the core
	 * design of jeeps) is even more painful.
	 */
	(*packet)->type = le_read16(&pkt.gusb_pkt.pkt_id);
	payload_size = le_read32(&pkt.gusb_pkt.datasz);
	(*packet)->n = (UC) payload_size;
	memcpy((*packet)->data, &pkt.gusb_pkt.databuf, payload_size);
	
	return 1;
}
