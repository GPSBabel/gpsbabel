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
#include <usb.h>
#include "garminusb.h"

#define GARMIN_VID 0x91e
#define TMOUT_W 1000 /*  Milliseconds to timeout device access. */
#define TMOUT_R 0000 /*  Milliseconds to timeout device access. */
#define TMOUT_I 0001 /*  Milliseconds to timeout device access. */

int gusb_intr_in_ep;
int gusb_bulk_out_ep;
int gusb_bulk_in_ep;

static const char  oinit[12] = {0, 0, 0, 0, 0x05, 0, 0, 0, 0, 0, 0, 0};
static const char oinit2[12] = {0, 0, 0, 0, 0x10, 0, 0, 0, 0, 0, 0, 0};
static const char RQST[12] = {0x14, 0, 0, 0, 0xfe, 0, 0, 0, 0, 0, 0, 0};
static char iresp[16];
static char iresp2[16];

static struct usb_bus *busses;
static	usb_dev_handle *udev;

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

dump(char *msg, unsigned char *in, int r)
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
        r = usb_bulk_write(udev, gusb_bulk_out_ep, &opkt->dbuf, sz, TMOUT_W);
	dump ("Sent", &opkt->dbuf, r);
	if (r != sz) {
		fprintf(stderr, "Bad cmdsend\n");
	}
}

int
gusb_cmd_get(garmin_usb_packet *ibuf, size_t sz)
{
	int rv = 0;
	unsigned char *obuf = ibuf->dbuf;
	unsigned char *buf = obuf;

	while (sz) {
		int r;
		/*
		 * Since Garmin stupidly put bulk data on an interrupt pipe
		 * with an absurdly tiny buffer, we have to coalesce reads
		 * and we have to be fast about getting them.    (High speed
		 * polling totally misses the point of USB...)
		 */

		r = usb_interrupt_read(udev, gusb_intr_in_ep, buf, sz, TMOUT_I);
		printf("Read: %d/%d \n", r, sz);
		if (r > 0) {
			buf += r;
			rv += r;
			sz -= r;
		}
		if (r < 0) return rv;
		/*
		 * A zero length read AFTER a successful read means we're
		 * done.
		 */
		if (r == 0 && rv) {
			break;
		}
	}
	dump("completed intr Got", obuf, rv);
	return rv;
}

garmin_usb_start(struct usb_device *dev)
{
	int ret;
	int i;
	char ibuf[4096];

	udev = usb_open(dev);
	if (!udev) { fatal("usb_open failed"); }
	/*
	 * Hrmph.  No iManufacturer or iProduct headers....
	 */
	if (usb_claim_interface(udev, 0) < 0) {
		abort();
	}

	if (usb_set_configuration(udev, 1) < 0) {
		abort();
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
//printf("Bulk in: %d\n", gusb_bulk_in_ep);
//printf("Bulk out: %d\n", gusb_bulk_out_ep);
//printf("intr in: %d\n", gusb_intr_in_ep);

	garmin_usb_syncup();
#if 0
	cmd_send(RQST, sizeof(RQST));
	cmd_get(ibuf, sizeof(ibuf));
exit(0);
#endif

// fprintf(stdout, "====================================================\n");

	return;
	usb_release_interface(udev, 0);
	usb_reset(udev);
	usb_close(udev);
exit(1);
}
#if 1
garmin_usb_syncup(void)
{
	int maxct = 200;
	int maxtries;
	char ibuf[4096];
#if 0
	usb_clear_halt(udev, gusb_intr_in_ep);
	usb_clear_halt(udev, gusb_bulk_out_ep);
	usb_clear_halt(udev, gusb_bulk_in_ep);
#endif

	for (maxtries = maxct; maxtries; maxtries--) {
		gusb_cmd_send(oinit, sizeof(oinit));
		gusb_cmd_get(iresp, sizeof(iresp));
		if (iresp[4] == 6 && iresp[8] == 4) {
			fprintf(stderr, "Synced in %d\n", maxct - maxtries);
//			fprintf(stderr, "Unit number %u\n", iresp[15] << 24 | iresp[14] << 16 | iresp[13] << 8 | iresp[12]);
			return;
		}
	}
#if 0
	for (maxtries = maxct; maxtries; maxtries--) {
		gusb_cmd_send(oinit2, sizeof(oinit2));
		gusb_cmd_get(iresp2, sizeof(iresp2));
		if (iresp2[4] == 0x11 && iresp2[8] == 4) {
			return;
		}
	}
#endif
fprintf(stderr, "Cannot sync up with receiver\n");
exit(1);
}
#endif

static
void garmin_usb_scan(void)
{
	struct usb_bus *bus;
	int c, i, a;

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

#if 0
Xmain()
{
		garmin_usb_init();
		garmin_usb_scan();
}
#endif
#endif /* !defined(NO_USB) */
