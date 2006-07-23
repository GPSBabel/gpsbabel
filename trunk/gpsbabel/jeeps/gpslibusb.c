/*
    Physical/OS USB layer to talk to libusb.

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


#include <stdio.h>
#include <ctype.h>
#include "config.h"
#if HAVE_LIBUSB
#include <usb.h>
#include "gps.h"
#include "garminusb.h"
#include "gpsusbcommon.h"

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
#define TMOUT_I 3000 /*  Milliseconds to timeout intr pipe access. */
#define TMOUT_B 3000 /*  Milliseconds to timeout bulk pipe access. */

typedef struct {
	struct usb_bus *busses;
} libusb_unit_data;

/* 
 * TODO: this should all be moved into libusbdata in gpslibusb.h,
 * allocated once here in gusb_start, and deallocated at the end.
 */
static int gusb_intr_in_ep;
static int gusb_bulk_out_ep;
static int gusb_bulk_in_ep;
static gusb_llops_t libusb_llops;

static usb_dev_handle *udev;
static void garmin_usb_scan(libusb_unit_data *, int);

static int
gusb_libusb_send(const garmin_usb_packet *opkt, size_t sz)
{
	int r;
	r = usb_bulk_write(udev, gusb_bulk_out_ep, (char *)(void *)opkt->dbuf, sz, TMOUT_B);

	if (r != (int) sz) {
		fprintf(stderr, "Bad cmdsend r %d sz %ld\n", r, (unsigned long) sz);
		if (r < 0) {
			fatal("usb_bulk_write failed. '%s'\n", 
				usb_strerror());
		}
	}

	return r;
}

static int
gusb_libusb_get(garmin_usb_packet *ibuf, size_t sz)
{
	unsigned char *buf = &ibuf->dbuf[0];
	int r = -1;

	r = usb_interrupt_read(udev, gusb_intr_in_ep, (char *) buf, sz, TMOUT_I);
	return r;
}

static int
gusb_libusb_get_bulk(garmin_usb_packet *ibuf, size_t sz)
{
	int r;
	unsigned char *buf = &ibuf->dbuf[0];

	r = usb_bulk_read(udev, gusb_bulk_in_ep, (char *) buf, sz, TMOUT_B);

	return r;
}


static int
gusb_teardown(gpsdevh *dh)
{
	if (udev) {
		usb_release_interface(udev, 0);
		usb_close(udev);
		udev = NULL;
	}
	return 0; 
}

/*
 * This is a function of great joy to discover.
 *
 * It turns out that as of 5/2006, every Garmin USB product has a problem
 * where the device does not reset the data toggles after a configuration
 * set.   After a reset, the toggles both match.  So we tear through the
 * conversation and life is good.  Unfortunately, the second time through,
 * if we had an odd number of transactions in the previous conversation,
 * we send a configuration set and reset the toggle on the HCI but the 
 * toggle on the device's end of the pipe is now out of whack which means
 * that the subsequent transaction will hang.
 * 
 * This isn't a problem in Windows since the configuration set is done only
 * once there.
 * 
 * This code has been tested in loops of 1000 cycles on Linux and OS/X and
 * it seems to cure this at a mere cost of complexity and startup time.  I'll
 * be delighted when all the firmware gets revved and updated and we can
 * remove this.
 *  
 */
static void 
gusb_reset_toggles(void)
{
	static const char  oinit[12] = 
		{0, 0, 0, 0, GUSB_SESSION_START, 0, 0, 0, 0, 0, 0, 0};
	static const char  oid[12] = 
		{20, 0, 0, 0, 0xfe, 0, 0, 0, 0, 0, 0, 0};
	garmin_usb_packet iresp;
	int t;

	/* Start off with three session starts.
	 * #1 resets the bulk out toggle.  It may not make it to the device.
	 * #2 resets the the intr in toggle.  It will make it to the device
	 *	since #1 reset the the bulk out toggle.   The device will
	 *      respond on the intr in pipe which will clear its toggle.
	 * #3 actually starts the session now that the above are both clear.
 	 */

	gusb_cmd_send((const garmin_usb_packet *) oinit, sizeof(oinit));
	gusb_cmd_send((const garmin_usb_packet *) oinit, sizeof(oinit));
	gusb_cmd_send((const garmin_usb_packet *) oinit, sizeof(oinit));

	t = 10;
	while(1) {
		le_write16(&iresp.gusb_pkt.pkt_id, 0);
		le_write32(&iresp.gusb_pkt.datasz, 0);
		le_write32(&iresp.gusb_pkt.databuf, 0);

		gusb_cmd_get(&iresp, sizeof(iresp));

		if ((le_read16(iresp.gusb_pkt.pkt_id) == GUSB_SESSION_ACK) &&
                        (le_read32(iresp.gusb_pkt.datasz) == 4)) {
				break;
		}
		if (t-- <= 0) {
			fatal("Could not start session in a reasonable number of tries.\n");
		}
	}

	/*
	 * Now that the bulk out and intr in packets are good, we send
	 * a product ID.    On devices that respond totally on the intr
	 * pipe, this does nothing interesting, but on devices that respon
	 * on the bulk pipe this will reset the toggles on the bulk in.
 	 */
	t = 10;
	gusb_cmd_send((const garmin_usb_packet *) oid, sizeof(oid));
	while(1) {
		le_write16(&iresp.gusb_pkt.pkt_id, 0);
		le_write32(&iresp.gusb_pkt.datasz, 0);
		le_write32(&iresp.gusb_pkt.databuf, 0);

		gusb_cmd_get(&iresp, sizeof(iresp));

		if ((le_read16(iresp.gusb_pkt.pkt_id) == GUSB_SESSION_ACK) &&
                        (le_read32(iresp.gusb_pkt.datasz) == 4)) {
		}
		if (le_read16(iresp.gusb_pkt.pkt_id) == 0xfd) return;
		if (t-- <= 0) {
			fatal("Could not start session in a reasonable number of tries.\n");
		}
	}
}

void
garmin_usb_start(struct usb_device *dev)
{
	int i;

	if (udev) return;

	udev = usb_open(dev);
	atexit((void(*)())gusb_teardown);

	if (!udev) { fatal("usb_open failed: %s\n", usb_strerror()); }
	/*
	 * Hrmph.  No iManufacturer or iProduct headers....
	 */
	if (usb_set_configuration(udev, 1) < 0) {
		fatal("usb_set_configuration failed: %s\n", usb_strerror());
	}

	if (usb_claim_interface(udev, 0) < 0) {
		fatal("Claim interfaced failed: %s\n", usb_strerror());
	}

	libusb_llops.max_tx_size = dev->descriptor.bMaxPacketSize0;

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

	/*
	 *  Zero is the configuration endpoint, so if we made it through
	 * that loop without non-zero values for all three, we're hosed.
	 */
	if (gusb_intr_in_ep && gusb_bulk_in_ep && gusb_bulk_out_ep) {
		gusb_reset_toggles();
		gusb_syncup();
		return;
	}

	fatal("Could not identify endpoints on USB device.\n"
		"Found endpoints Intr In 0x%x Bulk Out 0x%x Bulk In %0xx\n", 
		gusb_intr_in_ep, gusb_bulk_out_ep, gusb_bulk_in_ep);
}

static
void garmin_usb_scan(libusb_unit_data *lud, int req_unit_number)
{
	int found_devices = 0;
	struct usb_bus *bus;

	for (bus = lud->busses; bus; bus = bus->next) {
		struct usb_device *dev;

		for (dev = bus->devices; dev; dev = dev->next) {
			/* Probably too promiscious of a match, but since
			 * Garmin doesn't document the _proper_ matching,
			 * we just take the easy way out for now.
			 */
			if (dev->descriptor.idVendor == GARMIN_VID) {
				/* Nuvi */
				if (dev->descriptor.idProduct == 0x19) 
					continue;
				if (req_unit_number < 0) {
					garmin_usb_start(dev);	
					/* 
					 * It's important to call _close
					 * here since the bulk/intr models
				  	 * may have a "dangling" packet that
					 * needs to be drained.
					 */ 		
					gusb_close(NULL);
				} else 
				if (req_unit_number == found_devices)
					garmin_usb_start(dev);	
				found_devices++;
			}
		}
	}

	if (req_unit_number < 0) {
		gusb_list_units();
		exit (0);
	}
	if (0 == found_devices) {
		fatal("Found no Garmin USB devices.\n");
	}
}

static gusb_llops_t libusb_llops = {
	gusb_libusb_get,
	gusb_libusb_get_bulk,
	gusb_libusb_send,
	gusb_teardown
};

int
gusb_init(const char *portname, gpsdevh **dh)
{
	int req_unit_number = 0;
	libusb_unit_data *lud = xcalloc(sizeof (libusb_unit_data), 1);
	*dh = (gpsdevh*) lud;

// usb_set_debug(99);
	usb_init();
	gusb_register_ll(&libusb_llops);

	/* if "usb:N", read "N" to be the unit number. */
	if (strlen(portname) > 4) {
		if (0 == strcmp(portname+4, "list")) {
			req_unit_number = -1;
		} else {
			req_unit_number = atoi(portname + 4);
		}
	}

	usb_find_busses();
	usb_find_devices();
	lud->busses = usb_get_busses();
	garmin_usb_scan(lud, req_unit_number);

	return 1;
}

#endif /* HAVE_LIBUSB */
