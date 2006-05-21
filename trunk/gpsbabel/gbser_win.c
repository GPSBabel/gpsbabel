/*
    Serial interface - Windows layer.

    Copyright (C) 2006  Robert Lipe, robertlipe@usa.net

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

#include "defs.h"
#include "gbser.h"
#include <windows.h>
#include <setupapi.h>

typedef struct gbser_win_handle {
	HANDLE comport;
} gbser_win_handle;

#define xCloseHandle(a) if (a) { CloseHandle(a); } a = NULL;

void *
gbser_init(const char *portname)
{
//	DCB tio;	
	COMMTIMEOUTS timeout;
	HANDLE comport;
	char *xname= xstrdup("\\\\.\\\\");
	gbser_win_handle* handle = xcalloc(1, sizeof(*handle));;

	/* Amazingly, windows will fail the open below unless we
	 * prepend \\.\ to the name.   It also then fails the open
	 * unless we strip the colon from the name.  Aaaaargh!
	 */
	xname = xstrappend(xname, portname);
	if (xname[strlen(xname)-1] == ':')
		xname[strlen(xname)-1] = 0;
//	xCloseHandle(comport);

	comport = CreateFile(xname, GENERIC_READ|GENERIC_WRITE, 
				0, NULL, OPEN_EXISTING, 0, NULL);

	if (comport == INVALID_HANDLE_VALUE) {
		return NULL;
	}
	handle->comport = comport;

#if 0
	tio.DCBlength = sizeof(DCB);
	GetCommState (comport, &tio);
//	tio.BaudRate = mkspeed(bitrate);
	{
	extern int mkspeed(int);
	tio.BaudRate = mkspeed(4800);
	}
	tio.fBinary = TRUE;
	tio.fParity = TRUE;
	tio.fOutxCtsFlow = FALSE;
	tio.fOutxDsrFlow = FALSE;
	tio.fDtrControl = DTR_CONTROL_ENABLE;
	tio.fDsrSensitivity = FALSE;
	tio.fTXContinueOnXoff = TRUE;
	tio.fOutX = FALSE;
	tio.fInX = FALSE;
	tio.fErrorChar = FALSE;
	tio.fNull = FALSE;
	tio.fRtsControl = RTS_CONTROL_ENABLE;
	tio.fAbortOnError = FALSE;
	tio.ByteSize = 8;
	tio.Parity = NOPARITY;
	tio.StopBits = ONESTOPBIT;

	if (!SetCommState (comport, &tio)) {
		/*
		 *  Probably not a com port.   Let caller try it as a file.
		 */

		return NULL;
	}
#else
	gbser_setspeed(handle, 4800);
#endif

	GetCommTimeouts (comport, &timeout);
	/* We basically do single character reads and simulate line input
	 * mode, so these values are kind of fictional.
	 */
	timeout.ReadIntervalTimeout = 1000;
	timeout.ReadTotalTimeoutMultiplier = 1000;
	timeout.ReadTotalTimeoutConstant = 1000;
	timeout.WriteTotalTimeoutMultiplier = 1000;
	timeout.WriteTotalTimeoutConstant = 1000;
	if (!SetCommTimeouts (comport, &timeout)) {
		xCloseHandle (comport);
		fatal("SetCommTimeouts failed.\n");
	}
	return handle;
}

/* 
 * Returns 1 on success, 0 on errro.
 */
int
gbser_setspeed(void *handle, unsigned speed)
{
	extern int mkspeed(int);   /* From magproto.c  */
	gbser_win_handle *h = (gbser_win_handle *) handle;
	DCB tio;

	tio.DCBlength = sizeof(DCB);
	GetCommState(h->comport, &tio);
	
	tio.BaudRate = mkspeed(speed);
	tio.fBinary = TRUE;
	tio.fParity = TRUE;
	tio.fOutxCtsFlow = FALSE;
	tio.fOutxDsrFlow = FALSE;
	tio.fDtrControl = DTR_CONTROL_ENABLE;
	tio.fDsrSensitivity = FALSE;
	tio.fTXContinueOnXoff = TRUE;
	tio.fOutX = FALSE;
	tio.fInX = FALSE;
	tio.fErrorChar = FALSE;
	tio.fNull = FALSE;
	tio.fRtsControl = RTS_CONTROL_ENABLE;
	tio.fAbortOnError = FALSE;
	tio.ByteSize = 8;
	tio.Parity = NOPARITY;
	tio.StopBits = ONESTOPBIT;

	if (!SetCommState (h->comport, &tio)) {
		/*
		 *  Probably not a com port.   Let caller try it as a file.
		 */

		return 0;
	}
	return 1;
}

int
gbser_read(void *handle, char *ibuf, int size)
{
	gbser_win_handle *h = (gbser_win_handle *) handle;
	int i = 0;
	DWORD cnt;

        ibuf[0]='0';
	for(;i < size;i++) {
		if (ReadFile (h->comport, &ibuf[i], 1, &cnt, NULL) != TRUE)
			break;
		if (cnt < 1)
			return 0;
		if (ibuf[i] == '\n')
			break;
	}

	ibuf[i] = 0;
	return 1;
}

void
gbser_deinit(void *handle)
{
	gbser_win_handle *h = (gbser_win_handle *) handle;
	xfree(h);
}
