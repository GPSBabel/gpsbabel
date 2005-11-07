/********************************************************************
** @source JEEPS serial port low level functions
**
** @author Copyright (C) 1999,2000 Alan Bleasby
** @version 1.0
** @modified December 28th 1999 Alan Bleasby. First version
** @modified June 29th 2000 Alan Bleasby. NMEA additions
** @@
** 
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
** 
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
********************************************************************/
#include "gps.h"
#include "garminusb.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>

#if 0
#define GARMULATOR 1
char *rxdata[] = {
	"10 06 02 fe 00 fa 10 03",
	"10 ff 7d 97 00 0e 01 53 74 72 65 65 74 50 69 6c 6f 74 20 33 20 53 6f 66 74 77 61 72 65 20 56 65 72 73 69 6f 6e 20 32 2e 37 30 00 56 45 52 42 4d 41 50 20 41 6d 65 72 69 63 61 73 20 41 75 74 6f 72 6f 75 74 65 20 31 2e 30 30 00 56 45 52 41 55 44 20 45 6e 67 6c 69 73 68 20 33 2e 30 31 00 56 45 52 53 50 4c 53 43 52 4e 20 53 70 6c 61 73 68 20 53 63 72 65 65 6e 20 4d 69 73 73 69 6e 67 00 f1 10 03",
	"10 f8 0e 56 45 52 53 4d 41 50 31 20 4e 6f 6e 65 00 fb 10 03",

	/* Guessing from here down */
	"10 06 02 fe 00 fa 10 03", /* Ack the unknown packet */
	"10 fd 24 50 00 00 4c 01 00 41 0a 00 41 64 00 44 6d 00 41 c9 00 44 ca 00 44 6d 00 44 d2 00 41 2d 01 44 36 01 44 2d 01 66 10 03", /* PTR Array */
	"10 06 02 0a 00 ee 10 03", /* Ack */
	"10 0e 08 06 04 d4 07 00 17 3a 30 84 10 03", /* DATTIME */
	"10 06 02 0a 00 ee 10 03", /* Ack */
	"10 1b 02 09 00 da 10 03", /* RECORD */
	"10 06 02 0a 00 ee 10 03", /* Ack */
	"10 23 5f 01 00 ff 70 3f 20 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 a6 1b aa 19 6e 78 5c c2 00 00 00 00 51 59 04 69 00 00 00 00 00 00 00 00 ff ff ff ff 47 43 31 41 33 37 00 54 68 65 20 54 72 6f 6c 6c 20 62 79 20 61 31 38 32 70 69 6c 6f 74 20 26 20 46 61 6d 69 6c 79 00 00 00 00 00 59 10 03"
	"10 0c 02 07 00 eb 10 03" /* XFERCMP */
};
#endif
/* 
 * termio on Cygwin is apparently broken, so we revert to Windows serial.
 */
#if defined (__WIN32__) || defined (__CYGWIN__)

#include <windows.h>
/*
 *  Rather than teaching the rest of this code about Windows serial APIs
 *  we'll weenie out, violate good layering, and just keep our handle
 *  internal.   This means we ignore that 'fd' number that gets passed in.
 */

static HANDLE comport;

/*
 * Display an error from the serial subsystem.
 */
void GPS_Serial_Error(char *hdr)
{
	char msg[200];
	char *s;

	strcpy(msg, hdr);
	s = msg + strlen(hdr);
	*s++ = ':';
	*s++ = ' ';

	FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, 0, 
			GetLastError(), 0, s, sizeof(msg) - strlen(hdr) - 2, 0 );
	GPS_Error(msg);
}

int32 GPS_Serial_On(const char *port, int32 *fd)
{
	DCB tio;
	COMMTIMEOUTS timeout;

	if (gps_is_usb) {
	    switch (gusb_open(port)) {
		    case 0: return 0;
		    case 1: return 1;
		    case 2: exit(0);
	    }
	}

	comport = CreateFile(port, GENERIC_READ|GENERIC_WRITE, 0, NULL,
					  OPEN_EXISTING, 0, NULL);

	if (comport == INVALID_HANDLE_VALUE) {
		GPS_Serial_Error("CreateFile");
		gps_errno = SERIAL_ERROR;
		return 0;
	}

	tio.DCBlength = sizeof(DCB);
	GetCommState (comport, &tio);
	tio.BaudRate = CBR_9600;
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
		GPS_Serial_Error("SetCommState");
		CloseHandle(comport);
		comport = INVALID_HANDLE_VALUE;
		gps_errno = SERIAL_ERROR;
		return 0;
	}

	/*
	 * The timeouts are kind of fictional as we always end up doing
	 * single byte reads.   At 9600bps (the default) the individual
	 * character time is 104Millisecs, so these are mostly "dead-man"
	 * (i.e. cable unplugged, unit not turned on) values.
	 */
	GetCommTimeouts (comport, &timeout);

	timeout.ReadIntervalTimeout = 1000; /*like vtime.  In MS. */
	timeout.ReadTotalTimeoutMultiplier = 1000;
	timeout.ReadTotalTimeoutConstant = 1000;
	timeout.WriteTotalTimeoutMultiplier = 1000;
	timeout.WriteTotalTimeoutConstant = 1000;
	if (!SetCommTimeouts (comport, &timeout)) {
		GPS_Serial_Error("SetCommTimeouts");
		CloseHandle (comport);
		comport = INVALID_HANDLE_VALUE;
		gps_errno = SERIAL_ERROR;
		return 0;
	}
	*fd = 1;
	return 1;
}

int32 GPS_Serial_Off(const char *port, int32 fd)
{
	if (gps_is_usb) {
		gusb_close(port);
	} else {
		CloseHandle(comport);
		comport = INVALID_HANDLE_VALUE;
	}
	return 1;
}

int32 GPS_Serial_Chars_Ready(int32 fd)
{
	COMSTAT lpStat;
	DWORD lpErrors;

	ClearCommError(comport, &lpErrors, &lpStat);
	return (lpStat.cbInQue > 0);
}

int32 GPS_Serial_Wait(int32 fd)
{

	if (gps_is_usb) return 1;

	/* Wait a short time before testing if data is ready.
	 * The GPS II, in particular, has a noticable time responding
	 * with a response to the device inquiry and if we give up on this
	 * too soon, we fail to read the response to the A001 packet and
	 * blow our state machines when it starts streaming the capabiilties
	 * response packet.
	 */
	Sleep(usecDELAY / 1000);
	return GPS_Serial_Chars_Ready(fd);
}

int32 GPS_Serial_Flush(int32 fd)
{
	return 1;
}

int32 GPS_Serial_Write(int32 ignored, const void *obuf, int size)
{
	DWORD len;

	/* 
	 * Unbelievably, the Keyspan PDA serial driver 3.2, a "Windows 
	 * Certified driver", will crash the OS on a write of zero bytes.
	 * We get such writes from upstream when there are zero payload
	 * bytes.  SO we trap those here to stop Keyspan & Windows from
	 * nuking the system.
	 */
	if (size == 0) {
		return 0;
	}
	WriteFile (comport, obuf, size, &len, NULL);
	if (len != (DWORD) size) {
		fatal ("Write error.   Wrote %d of %d bytes.\n", len, size);
	}
	return len;
}

int32 GPS_Serial_Read(int32 ignored, void *ibuf, int size)
{
	DWORD cnt  = 0;

	ReadFile(comport, ibuf, size, &cnt, NULL);
	return cnt;
}

int32 GPS_Serial_Close(int32 fd, const char *port)
{
	return 1;
}

#else

#include <sys/ioctl.h>
#include <sys/time.h>
#include <termios.h>
#include <unistd.h>

static struct termios gps_ttysave;

/* @func GPS_Serial_Restoretty ***********************************************
**
** Save tty information for the serial post to be used
**
** @param [r] port [const char *] port e.g. ttyS1
**
** @return [int32] false upon error
************************************************************************/

int32 GPS_Serial_Savetty(const char *port)
{
    int32 fd;

    if (gps_is_usb) return 1;
    
    if((fd = open(port, O_RDWR))==-1)
    {
	perror("open");
	gps_errno = SERIAL_ERROR;
	GPS_Error("SERIAL: Cannot open serial port");
	return 0;
    }
    
    if(tcgetattr(fd,&gps_ttysave)==-1)
    {
	perror("tcgetattr");
	gps_errno = HARDWARE_ERROR;
	GPS_Error("SERIAL: tcgetattr error");
	return 0;
    }

    if(!GPS_Serial_Close(fd,port))
    {
	gps_errno = SERIAL_ERROR;
	GPS_Error("GPS_Serial_Savetty: Close error");
	return 0;
    }

    return 1;
}


/* @func GPS_Serial_Restoretty ***********************************************
**
** Restore serial post to condition before AJBGPS called
**
** @param [r] port [const char *] port e.g. ttyS1
**
** @return [int32] false upon error
************************************************************************/

int32 GPS_Serial_Restoretty(const char *port)
{
    int32 fd;

    if (gps_is_usb) return 1;
    
    if((fd = open(port, O_RDWR))==-1)
    {
	perror("open");
	gps_errno = HARDWARE_ERROR;
	GPS_Error("SERIAL: Cannot open serial port");
	return 0;
    }
    
    if(tcsetattr(fd, TCSAFLUSH, &gps_ttysave)==-1)
    {
	perror("ioctl");
	gps_errno = HARDWARE_ERROR;
	GPS_Error("SERIAL: tcsetattr error");
	return 0;
    }

    return 1;
}



/* @func GPS_Serial_Open ***********************************************
**
** Open a serial port 8bits 1 stop bit 9600 baud
**
** @param [w] fd [int32 *] file descriptor
** @param [r] port [const char *] port e.g. ttyS1
**
** @return [int32] false upon error
************************************************************************/

int32 GPS_Serial_Open(int32 *fd, const char *port)
{
    struct termios tty;
    
    /*
     * This originally had O_NDELAY | O_NOCTTY in here, but this
     * causes problems with Linux USB ttys (observed on PL2303 and MCT)
     * and the rest of the code doesn't _REALLY_ handle the partial 
     * write/retry case anyway.  - robertl
     */
    if((*fd = open(port, O_RDWR))==-1)
    {
	perror("open");
	GPS_Error("SERIAL: Cannot open serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    if(tcgetattr(*fd,&tty)==-1)
    {
	perror("tcgetattr");
	GPS_Error("SERIAL: tcgetattr error");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    tty.c_cflag &= ~(CSIZE);
    tty.c_cflag |= (CREAD | CS8 | CLOCAL);
    cfsetospeed(&tty,B9600);
    cfsetispeed(&tty,B9600);
    
    tty.c_lflag &= 0x0;
    tty.c_iflag &= 0x0;
    tty.c_oflag &= 0x0;
    tty.c_cc[VMIN] = 1;
    tty.c_cc[VTIME] = 0;

    if(tcsetattr(*fd,TCSANOW,&tty)==-1)
    {
	perror("tcsetattr");
	GPS_Error("SERIAL: tcsetattr error");
	return 0;
    }

    return 1;
}

int32 GPS_Serial_Read(int32 handle, void *ibuf, int size)
{
#if GARMULATOR
	static int l;
	static char *rp;
	char **rxp = &rxdata[l];
	char *hex;
	char *rx = *rxp;
	char *ib = ibuf;

	if (!rp) rp = rxdata[0];

	/* Skip over nulls in our pasted strings */
	if (*rp == 0) {
	        rp = rxdata[++l];
	}

	*ib = strtoul(rp, &rp, 16);
	if (*rp) rp++;
	fprintf(stderr, ".");
	return 1;

#else
	return read(handle, ibuf, size);
#endif
}

int32 GPS_Serial_Write(int32 handle, const void *obuf, int size)
{
	return write(handle, obuf, size);
}


/* @func GPS_Serial_Flush ***********************************************
**
** Flush the serial lines
**
** @param [w] fd [int32] file descriptor
**
** @return [int32] false upon error
************************************************************************/
int32 GPS_Serial_Flush(int32 fd)
{
    if (gps_is_usb) return 1;
    
    if(tcflush(fd,TCIOFLUSH))
    {
	perror("tcflush");
	GPS_Error("SERIAL: tcflush error");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    return 1;
}



/* @func GPS_Serial_Close ***********************************************
**
** Close serial port
**
** @param [r] fd [int32 ] file descriptor
** @param [r] port [const char *] port e.g. ttyS1
**
** @return [int32] false upon error
************************************************************************/

int32 GPS_Serial_Close(int32 fd, const char *port)
{
    if (gps_is_usb)  return 1;

    if(close(fd)==-1)
    {
	perror("close");
	GPS_Error("SERIAL: Error closing serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }
    
    return 1;
}


/* @func GPS_Serial_Chars_Ready *****************************************
**
** Query port to see if characters are waiting to be read
**
** @param [r] fd [int32 ] file descriptor
**
** @return [int32] true if chars waiting
************************************************************************/

int32 GPS_Serial_Chars_Ready(int32 fd)
{
    fd_set rec;
    struct timeval t;
#if GARMULATOR
    static foo;
    /* Return sporadic reads just to torment the rest of the code. */
    if ((foo++ & 0xf) == 0)
    	return 1;
    else
	return 0;
#endif

    FD_ZERO(&rec);
    FD_SET(fd,&rec);

    t.tv_sec  = 0;
    t.tv_usec = 1000;
    (void) select(fd+1,&rec,NULL,NULL,&t);
    if(FD_ISSET(fd,&rec))
	return 1;

    return 0;
}



/* @func GPS_Serial_Wait ***********************************************
**
** Wait 80 milliseconds before testing for input. The GPS delay
** appears to be around 40-50 milliseconds. Doubling the value is to
** allow some leeway. 
**
** @param [r] fd [int32 ] file descriptor
**
** @return [int32] true if serial chars waiting
************************************************************************/

int32 GPS_Serial_Wait(int32 fd)
{
    fd_set rec;
    struct timeval t;

    if (gps_is_usb) return 1;

    FD_ZERO(&rec);
    FD_SET(fd,&rec);

    t.tv_sec  = 0;
    t.tv_usec = usecDELAY;

    (void) select(fd+1,&rec,NULL,NULL,&t);
    if(FD_ISSET(fd,&rec))
	return 1;

    return 0;
}



/* @func GPS_Serial_On *****************************************
**
** Set up port
**
** @param [r] port [const char *] port
** @param [w] fd [int32 *] file descriptor
**
** @return [int32] success
************************************************************************/

int32 GPS_Serial_On(const char *port, int32 *fd)
{
    if (gps_is_usb) {
	    return gusb_init();
    }
    if(!GPS_Serial_Savetty(port))
    {
	GPS_Error("Cannot access serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }
    
    if(!GPS_Serial_Open(fd,port))
    {
	GPS_Error("Cannot open serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    return 1;
}



/* @func GPS_Serial_Off ***********************************************
**
** Done with port
**
** @param [r] port [const char *] port
** @param [r] fd [int32 ] file descriptor
**
** @return [int32] success
************************************************************************/

int32 GPS_Serial_Off(const char *port, int32 fd)
{
    if(!GPS_Serial_Close(fd,port))
    {
	GPS_Error("Error Closing port");
	gps_errno = HARDWARE_ERROR;
	return 0;
    }
    
    if(!GPS_Serial_Restoretty(port))
    {
	GPS_Error("Error restoring port");
	gps_errno = HARDWARE_ERROR;
	return 0;
    }

    return 1;
}







/* @func GPS_Serial_Open_NMEA ******************************************
**
** Open a serial port 8bits 1 stop bit 4800 baud
**
** @param [w] fd [int32 *] file descriptor
** @param [r] port [const char *] port e.g. ttyS1
**
** @return [int32] false upon error
************************************************************************/

int32 GPS_Serial_Open_NMEA(int32 *fd, const char *port)
{
    struct termios tty;
    

    if((*fd = open(port, O_RDWR | O_NDELAY | O_NOCTTY))==-1)
    {
	perror("open");
	GPS_Error("SERIAL: Cannot open serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }


    if(tcgetattr(*fd,&tty)==-1)
    {
	perror("tcgetattr");
	GPS_Error("SERIAL: tcgetattr error");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    
    tty.c_cflag |= (CREAD | CS8 | CSIZE | CLOCAL);
    cfsetospeed(&tty,B4800);
    cfsetispeed(&tty,B4800);
    
    tty.c_lflag &= 0x0;
    tty.c_iflag &= 0x0;
    tty.c_oflag &= 0x0;
    
    
    if(tcsetattr(*fd,TCSANOW,&tty)==-1)
    {
	perror("tcsetattr");
	GPS_Error("SERIAL: tcsetattr error");
	return 0;
    }

    return 1;
}







/* @func GPS_Serial_On_NMEA ********************************************
**
** Set up port for NMEA
**
** @param [r] port [const char *] port
** @param [w] fd [int32 *] file descriptor
**
** @return [int32] success
************************************************************************/
int32 GPS_Serial_On_NMEA(const char *port, int32 *fd)
{

    if(!GPS_Serial_Savetty(port))
    {
	GPS_Error("Cannot access serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }
    
    if(!GPS_Serial_Open_NMEA(fd,port))
    {
	GPS_Error("Cannot open serial port");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    return 1;
}
#endif /* __WIN32__ */
