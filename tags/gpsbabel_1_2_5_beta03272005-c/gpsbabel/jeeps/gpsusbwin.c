#if !defined(NO_USB)
/*
    Windows layer of Garmin/USB protocol.

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
#include <malloc.h> 
#include <windows.h>
#include <winioctl.h>
#include <initguid.h>
#include <setupapi.h>

#include "gps.h"
#include "gpsapp.h"
#include "garminusb.h"

/* Constants from Garmin doc. */

// {2C9C45C2-8E7D-4C08-A12D-816BBAE722C0} 
DEFINE_GUID(GARMIN_GUID, 0x2c9c45c2L, 0x8e7d, 0x4c08, 0xa1, 0x2d, 0x81, 0x6b, 0xba, 0xe7, 0x22, 0xc0);

#define GARMIN_USB_API_VERSION 1 
#define GARMIN_USB_MAX_BUFFER_SIZE 4096 
#define GARMIN_USB_INTERRUPT_DATA_SIZE 64

#define IOCTL_GARMIN_USB_API_VERSION CTL_CODE \
	(FILE_DEVICE_UNKNOWN, 0x800, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_GARMIN_USB_INTERRUPT_IN CTL_CODE \
	(FILE_DEVICE_UNKNOWN, 0x850, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_GARMIN_USB_BULK_OUT_PACKET_SIZE CTL_CODE \
	(FILE_DEVICE_UNKNOWN, 0x851, METHOD_BUFFERED, FILE_ANY_ACCESS)


static HANDLE *usb_handle ;
static int usb_tx_packet_size ;

static const char  oinit[12] = {0, 0, 0, 0, 0x05, 0, 0, 0, 0, 0, 0, 0};
garmin_usb_packet iresp;
garmin_usb_packet iresp_junk;
static int ct;

static char * id_unit(void);

int
gusb_open(const char *pname)
{
	int maxtries;
	int maxct = 10;
	int unit_number = 0;
	int req_unit_number = 0;

	SP_INTERFACE_DEVICE_DATA devinterface;
	PSP_INTERFACE_DEVICE_DETAIL_DATA pdd = NULL;
	SP_DEVINFO_DATA devinfo;
	HDEVINFO hdevinfo;
	DWORD size;
	if (ct++) return 1;

	if (strlen(pname) > 4) {
		req_unit_number = atoi(pname+4);
		GPS_Diag("Searching for USB unit number %d\n", unit_number);
// if (req_unit_number == -2) {
// printf("%d %u %s\n", 0, 3019840053, "GPSMap60CS Software Version 3.50");
// exit(0);
// }
	}

	hdevinfo = SetupDiGetClassDevs( (GUID *) &GARMIN_GUID, NULL, NULL, 
			DIGCF_PRESENT|DIGCF_INTERFACEDEVICE);

	if (hdevinfo == INVALID_HANDLE_VALUE) {
		GPS_Serial_Error("SetupDiGetClassDevs failed");
		return 0;
	}

	/* Get the device associated with this index. */
	devinterface.cbSize = sizeof(SP_INTERFACE_DEVICE_DATA);
	if (!SetupDiEnumDeviceInterfaces(hdevinfo, NULL, (GUID *) &GARMIN_GUID, 
			0, &devinterface)) {
		GPS_Serial_Error("SetupDiEnumDeviceInterfaces");
		warning("Is the unit powered up and connected?");
		return 0;
	}

	SetupDiGetDeviceInterfaceDetail(hdevinfo, &devinterface, 
			NULL, 0, &size, NULL);

	pdd = malloc(size);
	pdd->cbSize = sizeof(SP_INTERFACE_DEVICE_DETAIL_DATA);
	devinfo.cbSize = sizeof(SP_DEVINFO_DATA);

	if (!SetupDiGetDeviceInterfaceDetail(hdevinfo, &devinterface, pdd, size, NULL, &devinfo)) {
		GPS_Serial_Error("SetupDiGetDeviceInterfaceDetail");
		return 0;
	}

	/* Whew.  All that just to get something we can open... */
	GPS_Diag("Windows GUID for interface %d is \n\t%s\n", unit_number, 
			pdd->DevicePath);
	usb_handle = CreateFile(pdd->DevicePath, GENERIC_READ|GENERIC_WRITE, 
			0, NULL, OPEN_EXISTING, 0, NULL );
	if (usb_handle == INVALID_HANDLE_VALUE) {
		GPS_Serial_Error("CreateFile failed");
		return 0;
	}

	if(!DeviceIoControl(usb_handle, IOCTL_GARMIN_USB_BULK_OUT_PACKET_SIZE, NULL, 0,
			&usb_tx_packet_size, GARMIN_USB_INTERRUPT_DATA_SIZE, &size, NULL)) {
		fatal("Couldn't get USB packet size");
	}

	if (pdd) {
		free(pdd);
	}
	SetupDiDestroyDeviceInfoList(hdevinfo);

	for (maxtries = maxct; maxtries; maxtries--) {

		le_write16(&iresp.gusb_pkt.pkt_id, 0);
		le_write32(&iresp.gusb_pkt.datasz, 0);
		le_write32(&iresp.gusb_pkt.databuf, 0);

		gusb_cmd_send((const garmin_usb_packet *) oinit, sizeof(oinit));
		gusb_cmd_get(&iresp, sizeof(iresp));

		if ((le_read16(iresp.gusb_pkt.pkt_id) == 6) && 
			(le_read32(iresp.gusb_pkt.datasz) == 4)) {
			unsigned serial_number = le_read32(iresp.gusb_pkt.databuf);
			GPS_Diag("Serial %u. Synced in %d\n", 
					serial_number,
					maxct - maxtries);
			garmin_unit_info[unit_number].serial_number = serial_number;
			garmin_unit_info[unit_number].os_identifier = strdup(pdd->DevicePath);
			garmin_unit_info[unit_number].product_identifier = id_unit();
			if (req_unit_number < 0) {
				printf("%d %u %s\n", unit_number, serial_number, garmin_unit_info[unit_number].product_identifier);
				return 2;
			}
			return 1;
		}
	}
	fatal("Unable to establish USB syncup within %d tries.\n", maxct);
	return 0;
}

int 
gusb_close(const char *portname)
{
	if (usb_handle != INVALID_HANDLE_VALUE) {
#if 0
		/* FIXME: we should probably release things and delete the "ct" 
		 * reference count above... 
		 */
		CloseHandle(usb_handle);
		usb_handle = INVALID_HANDLE_VALUE;
#endif
	}
  return 0;
}

int
gusb_cmd_get(garmin_usb_packet *ibuf, size_t sz)
{
	DWORD rxed = GARMIN_USB_INTERRUPT_DATA_SIZE;
	unsigned char *buf = (unsigned char *) &ibuf->dbuf;
	int i;
	int tsz=0;
	unsigned char *obuf = buf;

	while (sz) {
		/* The driver wrongly (IMO) rejects reads smaller than 
		 * GARMIN_USB_INTERRUPT_DATA_SIZE 
		 */
	if(!DeviceIoControl(usb_handle, IOCTL_GARMIN_USB_INTERRUPT_IN, NULL, 0,
			buf, GARMIN_USB_INTERRUPT_DATA_SIZE, &rxed, NULL)) {
		GPS_Serial_Error("Ioctl");
		fatal("ioctl");
	}
		buf += rxed;
		sz  -= rxed;
		tsz += rxed;
		if (rxed < GARMIN_USB_INTERRUPT_DATA_SIZE) {
			break;
		}
	}

	if (gps_show_bytes) {
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
	return tsz;
}

int
gusb_cmd_send(const garmin_usb_packet *opkt, size_t sz)
{
	DWORD rsz;
	size_t i;
	unsigned char *obuf = (unsigned char *) &opkt->dbuf;
	const char *m1, *m2;

	/* The spec warns us about making writes an exact multiple
	 * of the packet size, but isn't clear whether we can issue
	 * data in a single call to WriteFile if it spans buffers.
	 */
	WriteFile(usb_handle, obuf, sz, &rsz, NULL);
	if (gps_show_bytes) {
		printf("TX [%d]:", rsz);
		for(i=0;i<rsz;i++)
			GPS_Diag("%02x ", obuf[i]);
		for(i=0;i<rsz;i++)
			GPS_Diag("%c", isalnum(obuf[i])? obuf[i] : '.');
		m1 = Get_Pkt_Type(opkt->gusb_pkt.pkt_id[0], opkt->gusb_pkt.databuf[0], &m2);
		GPS_Diag("(%-8s%s)\n", m1, m2 ? m2 : "");
	}

	if (rsz != sz) {
		fatal ("Error sending %d bytes.   Successfully sent %d\n", sz, rsz);
	}

	if (0 == sz % usb_tx_packet_size) {
		DWORD sz2;
		GPS_Diag("Writing padding buffer.\n");
		WriteFile(usb_handle, 0, 0, &sz2, NULL);
	}

	return rsz;
}

static char *
id_unit(void)
{
static const unsigned char  oid[12] = {20, 0, 0, 0, 0xfe, 0, 0, 0, 0, 0, 0, 0};
	/* 
	 * Identify the unit before getting into all the protocol gunk.
	 * We get two packets back, but we discard the protocol array 
	 * for now.
	 */

	gusb_cmd_send((garmin_usb_packet *)oid, sizeof(oid));
	gusb_cmd_get(&iresp, sizeof(iresp));
	gusb_cmd_get(&iresp_junk, sizeof(iresp_junk));

	if (iresp.gusb_pkt.type == 20 && 
		le_read16(iresp.gusb_pkt.pkt_id) == 0xff) {
		return strdup(iresp.gusb_pkt.databuf+4);
	}

	return NULL;
}


#if 0
main()
{
	DWORD sz;
char ocmd[] = {00, 00, 00, 00, 05, 00, 00, 00, 00, 00, 00, 00};
char ocmd2[] = {0x14, 00, 00, 00, 0xfe, 00, 00, 00, 00, 00, 00, 00};
	gusb_open();
	WriteFile(usb_handle, ocmd, sizeof(ocmd), &sz, NULL);
	printf("Wrote %d\n", sz);
	usb_intr_get();

	WriteFile(usb_handle, ocmd2, sizeof(ocmd2), &sz, NULL);
	printf("Wrote %d\n", sz);
	usb_intr_get();

	WriteFile(usb_handle, ocmd2, sizeof(ocmd2), &sz, NULL);
	printf("Wrote %d\n", sz);
	usb_intr_get();
}
#endif
#endif /* !defined(NO_USB) */
