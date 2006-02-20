/*
    Decompose an incoming USB packet to make it look like a serial one.

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
#include <ctype.h>
#include "gps.h"
#include "garminusb.h"
#include "gpsusbint.h"

int32 GPS_Packet_Read_usb(int32 fd, GPS_PPacket *packet)
{
	int32  n;
	int32  i;
	int32 payload_size;
	const char *m1;
	const char *m2;

	garmin_usb_packet pkt;
	memset(&pkt, 0, sizeof(pkt));
	n = gusb_cmd_get(&pkt, sizeof(pkt));

	if ( n <= 0 ) {
// FIXME: revisit why we're intermittend getting read errors here...
// fprintf(stderr, "Eeek %d\n", n);
		(*packet)->n = (UC) 0;
		return n;
	}

	if (1 && gps_show_bytes) {
		GPS_Diag("\nRx Data:[%d]",n);
		for (i = 0; i < n; i++)
		    GPS_Diag("%02x ", pkt.dbuf[i]);
		for (i = 0; i < n; i++)
		    GPS_Diag("%c", isalnum(pkt.dbuf[i]) ? pkt.dbuf[i] : '.');
		m1 = Get_Pkt_Type(pkt.gusb_pkt.pkt_id[0], pkt.gusb_pkt.databuf[0], &m2);
		GPS_Diag("(%-8s%s)\n", m1, m2 ? m2 : "");

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
	return payload_size;
}
