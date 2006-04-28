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

// static struct usb_bus *busses;
static usb_dev_handle *udev;
static void garmin_usb_scan(libusb_unit_data *, int);

static int
gusb_libusb_send(const garmin_usb_packet *opkt, size_t sz)
{
	int r;
	
	r = usb_bulk_write(udev, gusb_bulk_out_ep, (char *)(void *)opkt->dbuf, sz, TMOUT_B);

	if (r != (int) sz) {
		fprintf(stderr, "Bad cmdsend r %d sz %d\n", r, sz);
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

void
garmin_usb_start(struct usb_device *dev)
{
	int i;

	if (udev) return;
	/*
	 * Linux _requires_ the reset.   OSX doesn't work if we 
	 * DO reset it.  I really should study this more, but for 
	 * now, we'll just avoid the reset on Apple's OSX.
	 */
#if !defined (__APPLE__)
	udev = usb_open(dev);
	usb_reset(udev);
	usb_close(udev);
#endif /* APPLE */

	udev = usb_open(dev);
	atexit((void(*)())gusb_teardown);

	if (!udev) { fatal("usb_open failed\n"); }
	/*
	 * Hrmph.  No iManufacturer or iProduct headers....
	 */
	if (usb_set_configuration(udev, 1) < 0) {
		fatal("usb_set_configuration failed: %s\n", usb_strerror());
	}

	if (usb_claim_interface(udev, 0) < 0) {
		fatal("Claim interfaced failed: %s\n", usb_strerror());
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

	/*
	 *  Zero is the configuration endpoint, so if we made it through
	 * that loop without non-zero values for all three, we're hosed.
	 */
	if (gusb_intr_in_ep && gusb_bulk_in_ep && gusb_bulk_out_ep) {
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
					gusb_teardown(NULL);
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
