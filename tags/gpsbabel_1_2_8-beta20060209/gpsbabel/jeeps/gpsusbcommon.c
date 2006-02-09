#if !defined(NO_USB)
/*
    Garmin USB layer - OS independent component.

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <usb.h>
#include "gps.h"
#include "garminusb.h"


/*
 * This receive logic is a little convoluted as we go to some efforts here
 * to hide most of the differences between the bulk only and bulk-interrupt
 * protocols as exhibited in the handhelds and dashtops.
 */

enum {
	rs_fromintr,
	rs_frombulk
} receive_state;

int
gusb_close(const char *portname)
{
	garmin_usb_packet scratch;
	switch (receive_state) {
	case rs_frombulk:
		gusb_cmd_get(&scratch, sizeof(scratch));
		break;
	}

	return 1;
}


int 
gusb_cmd_get(garmin_usb_packet *ibuf, size_t sz)
{
	int rv;
	unsigned char *buf = (unsigned char *) &ibuf->dbuf;

top:
	switch (receive_state) {
	case rs_fromintr:
			rv = gusb_cmd_get_os(ibuf, sz);
			break;
	case rs_frombulk:
			rv = gusb_cmd_get_os_bulk(ibuf, sz);
			if (rv == 0) {
				receive_state = rs_fromintr;
			}
			break;
	}

	if (gps_show_bytes) {
		int i;
		const char *m1, *m2;

		GPS_Diag("RX (%s) [%d]:", 
			receive_state == rs_fromintr ? "intr" : "bulk", rv);

		for(i=0;i<rv;i++)
			GPS_Diag("%02x ", buf[i]);

		for(i=0;i<rv;i++)
			GPS_Diag("%c", isalnum(buf[i])? buf[i] : '.');

		m1 = Get_Pkt_Type(ibuf->gusb_pkt.pkt_id[0], 
			ibuf->gusb_pkt.databuf[0], &m2);
		GPS_Diag("(%-8s%s)\n", m1, m2 ? m2 : "");
	}

	if (ibuf->gusb_pkt.pkt_id[0] == GUSB_REQUEST_BULK) {
		receive_state = rs_frombulk;
		goto top;
	}

	return rv;
}

int 
gusb_cmd_send(const garmin_usb_packet *opkt, size_t sz)
{
	int rv, i;

	unsigned char *obuf = (unsigned char *) &opkt->dbuf;
	const char *m1, *m2;

	rv = gusb_cmd_send_os(opkt, sz);

	if (gps_show_bytes) {
		GPS_Diag("TX [%d]:", sz);

		for(i=0;i<sz;i++)
			GPS_Diag("%02x ", obuf[i]);

		for(i=0;i<sz;i++)
			GPS_Diag("%c", isalnum(obuf[i])? obuf[i] : '.');

		m1 = Get_Pkt_Type(opkt->gusb_pkt.pkt_id[0], 
			opkt->gusb_pkt.databuf[0], &m2);

		GPS_Diag("(%-8s%s)\n", m1, m2 ? m2 : "");
        }
	return (rv);
}

void
gusb_list_units()
{
	int i;
	
	for (i = 0; i < GUSB_MAX_UNITS; i++) {
		if (garmin_unit_info[i].serial_number) {
			printf("%d %u %s\n", i, 
				garmin_unit_info[i].serial_number,
				garmin_unit_info[i].product_identifier
			);
		}
	}
}

char * 
gusb_id_unit(void) 
{
	static const char  oid[12] = 
		{20, 0, 0, 0, 0xfe, 0, 0, 0, 0, 0, 0, 0};
	garmin_usb_packet iresp;
	int i;
	char *rv = NULL;

	gusb_cmd_send((garmin_usb_packet *)oid, sizeof(oid));

	for (i = 0; i < 25; i++) {
		iresp.gusb_pkt.type = 0;
		if (gusb_cmd_get(&iresp, sizeof(iresp)) < 0) {
			return rv;
		}
		if (le_read16(iresp.gusb_pkt.pkt_id) == 0xff) {
			rv = strdup((char *) iresp.gusb_pkt.databuf+4);
		}
	}
fatal("eek!");
	
} 

void
gusb_syncup(void)
{
	static int unit_number;
	static const char  oinit[12] = 
		{0, 0, 0, 0, GUSB_SESSION_START, 0, 0, 0, 0, 0, 0, 0};
	garmin_usb_packet iresp;
	int i;

	for(i = 0; i < 25; i++) {
		le_write16(&iresp.gusb_pkt.pkt_id, 0);
		le_write32(&iresp.gusb_pkt.datasz, 0);
		le_write32(&iresp.gusb_pkt.databuf, 0);

		gusb_cmd_send((const garmin_usb_packet *) oinit, sizeof(oinit));
		gusb_cmd_get(&iresp, sizeof(iresp));

		if ((le_read16(iresp.gusb_pkt.pkt_id) == GUSB_SESSION_ACK) &&
			(le_read32(iresp.gusb_pkt.datasz) == 4)) {
			unsigned serial_number = le_read32(iresp.gusb_pkt.databuf);
			garmin_unit_info[unit_number].serial_number = serial_number;
			garmin_unit_info[unit_number].product_identifier = gusb_id_unit();

			unit_number++;

			return;
		}
	}
	fatal("Unable to establish USB syncup\n");
}


#endif /* !defined(NO_USB) */
