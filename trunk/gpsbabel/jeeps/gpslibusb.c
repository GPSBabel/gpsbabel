#if !defined(NO_USB)
/*
    Physical/OS USB layer to talk to libusb.

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


#include <stdio.h>
#include <ctype.h>
#include <usb.h>
#include "gps.h"
#include "garminusb.h"

#define GARMIN_VID 0x91e

/* This is very sensitive to timing; libusb and/or the unit is kind of
 * sloppy about not obeying packet boundries.  If this is too high, the
 * multiple packets responding to the device inquriy will be glommed into
 * one packet and we'll misparse them.  If it's too low, we'll get partially
 * satisfied reads.  It turns out this isn't terrible becuase we still end
 * up with DLE boundings and the upper layers (which are used to doing frame
 * coalescion into packets anyway becuase of their serial background) will
 * compensate.
 */
#define TMOUT_I 0100 /*  Milliseconds to timeout intr pipe access. */

int gusb_intr_in_ep;
int gusb_bulk_out_ep;
int gusb_bulk_in_ep;

static const char  oinit[12] = {0, 0, 0, 0, 0x05, 0, 0, 0, 0, 0, 0, 0};
garmin_usb_packet iresp;

static struct usb_bus *busses;
static	usb_dev_handle *udev;
static void garmin_usb_scan(void);
static void garmin_usb_syncup(void);

gusb_init(void)
{
//usb_set_debug(99);
	usb_init();
	usb_find_busses();
	usb_find_devices();
	busses = usb_get_busses();
	garmin_usb_scan();

	return 1;
}

static void dump(char *msg, const unsigned char *in, int r)
{
	int i;
	printf("%s: %d\n", msg, r);
	for (i = 0; i < r; i++) {
		printf ("%02x ", in[i]);
	}
	if (r) printf("\n");
	for (i = 0; i < r; i++) {
		printf ("%c", isalnum(in[i]) ? in[i] : '.');
	}
	if (r) printf("\n");
}

int
gusb_cmd_send(const garmin_usb_packet *opkt, size_t sz)
{
	int r;

        r = usb_bulk_write(udev, gusb_bulk_out_ep, (char *)(void *)opkt->dbuf, sz, TMOUT_I);
        if (gps_show_bytes) {
		dump ("Sent", &opkt->dbuf[0], r);
	}
	if (r != sz) {
		fprintf(stderr, "Bad cmdsend r %d sz %d\n", r, sz);
	}
	return r;
}

int
gusb_cmd_get(garmin_usb_packet *ibuf, size_t sz)
{
	unsigned char *buf = &ibuf->dbuf[0];
	unsigned char *obuf = buf;
	int r = -1, tsz = 0;

	r = usb_interrupt_read(udev, gusb_intr_in_ep, buf, sz, TMOUT_I);

	tsz = r;

        if (gps_show_bytes) {
		int i;
                const char *m1, *m2;
                printf("RX [%d]:", tsz);
                for(i=0;i<tsz;i++)
                        GPS_Diag("%02x ", obuf[i]);
                for(i=0;i<tsz;i++)
                        GPS_Diag("%c", isalnum(obuf[i])? obuf[i] : '.');

                m1 = Get_Pkt_Type(ibuf->gusb_pkt.pkt_id[0], ibuf->gusb_pkt.databuf[0], &m2);
                GPS_Diag("(%-8s%s)\n", m1, m2 ? m2 : "");
                printf("\n");
        }

	return (r);
}

void
garmin_usb_teardown(void)
{
	if (udev) {
		usb_release_interface(udev, 0);
		usb_close(udev);
		udev = NULL;
	}
}

void
garmin_usb_start(struct usb_device *dev)
{
	int i;

	if (udev) return;

	/*
	 * Linux _requires_ the reset.   OSX doesn't work if we DO reset it.
	 * I really should study this more, but for now, we'll just avoid the
	 * reset on Apple's OSX.
	 */
#if !defined (__APPLE__)
	udev = usb_open(dev);
	usb_reset(udev);
	usb_close(udev);
#endif /* APPLE */

	udev = usb_open(dev);
	atexit(garmin_usb_teardown);
	if (!udev) { fatal("usb_open failed"); }
	/*
	 * Hrmph.  No iManufacturer or iProduct headers....
	 */
	if (usb_claim_interface(udev, 0) < 0) {
//		abort();
	}

	if (usb_set_configuration(udev, 1) < 0) {
		fatal("usb_set_configuration failed");
	}


	for (i = 0; i < dev->config->interface->altsetting->bNumEndpoints; i++) {
		struct usb_endpoint_descriptor * ep;
		ep = &dev->config->interface->altsetting->endpoint[i];
		switch (ep->bmAttributes & USB_ENDPOINT_TYPE_MASK) {
#define EA(x) x & USB_ENDPOINT_ADDRESS_MASK
			case USB_ENDPOINT_TYPE_BULK:
				if (ep->bEndpointAddress & USB_ENDPOINT_DIR_MASK)
					gusb_bulk_in_ep = EA(ep->bEndpointAddress);
				else
					gusb_bulk_out_ep = EA(ep->bEndpointAddress);
				break;
			case USB_ENDPOINT_TYPE_INTERRUPT:
				if (ep->bEndpointAddress & USB_ENDPOINT_DIR_MASK)
					gusb_intr_in_ep = EA(ep->bEndpointAddress);
				break;
		}
	}

	garmin_usb_syncup();

	return;
}

void
garmin_usb_syncup(void)
{
	int maxct = 5;
	int maxtries;

	for (maxtries = maxct; maxtries; maxtries--) {

                le_write16(&iresp.gusb_pkt.pkt_id, 0);
                le_write32(&iresp.gusb_pkt.datasz, 0);
                le_write32(&iresp.gusb_pkt.databuf, 0);

		gusb_cmd_send((const garmin_usb_packet *) oinit, sizeof(oinit));
		gusb_cmd_get(&iresp, sizeof(iresp));

                if ((le_read16(iresp.gusb_pkt.pkt_id) == 6) &&
                        (le_read32(iresp.gusb_pkt.datasz) == 4)) {
			if (gps_show_bytes) {
				fprintf(stderr, "Synced in %d\n", maxct - maxtries);
			}
//			fprintf(stderr, "Unit number %u\n", iresp[15] << 24 | iresp[14] << 16 | iresp[13] << 8 | iresp[12]);
			return;
		}
	}
return;
	fatal("Cannot sync up with receiver\n");
}

static
void garmin_usb_scan(void)
{
	struct usb_bus *bus;

	for (bus = busses; bus; bus = bus->next) {
		struct usb_device *dev;

		for (dev = bus->devices; dev; dev = dev->next) {
			/* Probably too promiscious of a match, but since
			 * Garmin doesn't document the _proper_ matching,
			 * we just take the easy way out for now.
			 */
			if (dev->descriptor.idVendor == GARMIN_VID) {
				garmin_usb_start(dev);
			}
		}
	}
}

#endif /* !defined(NO_USB) */
