/*
    Serial interface - POSIX layer.

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
#include <assert.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>

#define MYMAGIC 0x91827364
typedef struct {
	struct termios orig_tio;
	struct termios my_tio;
	FILE *fh;
	int fd;
	unsigned long magic;
} gbser_posix_handle;

static 
speed_t
mkspeed(unsigned br)
{
        switch (br) {
                case 1200: return B1200;
                case 2400: return B2400;
                case 4800: return B4800;
                case 9600: return B9600;
                case 19200: return B19200;
#if defined B57600
                case 57600: return B57600;
#endif
#if defined B115200
                case 115200: return B115200;
#endif
                default: return B4800;
        }
}

/*
gbser_istty(void *handle)
{
	gbser_posix_handle *h = (gbser_posix_handle *) handle;
	assert(h->magic == MYMAGIC);

	return isatty(h->fd);
}
*/

void *
gbser_init(const char *name)
{
	gbser_posix_handle *h;

	h = xcalloc(sizeof *h, 1);
	h->magic = MYMAGIC;

	h->fh = xfopen(name,  "rb", "serial layer");
	h->fd = fileno(h->fh);
// h->fd = open(name, O_RDWR | O_EXCL | O_SYNC);
	
	if (!isatty(h->fd)) { 
		goto open_fail;
	}

	tcgetattr(h->fd, &h->orig_tio);

	h->my_tio = h->orig_tio;
	h->my_tio.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|
		IGNCR|IGNCR|IXON);
        h->my_tio.c_cflag &= ~(CSIZE|PARENB);
        h->my_tio.c_cflag |= CS8;
        h->my_tio.c_oflag = 0;
        h->my_tio.c_lflag = 0;
        h->my_tio.c_iflag = 0;
        h->my_tio.c_cc[VTIME] = 10;	/* Time out after one second */
        h->my_tio.c_cc[VMIN] = 255;

	return h;

open_fail:
	if (h->fh) {
		fclose(h->fh);
	}
	xfree(h);
	return NULL;
}

void 
gbser_deinit(void *handle)
{
	gbser_posix_handle *h = (gbser_posix_handle *) handle;
	assert(h->magic == MYMAGIC);

	tcsetattr(h->fd, TCSAFLUSH, &h->orig_tio);
	fclose(h->fh);
	xfree(handle);
}

int
gbser_setspeed(void *handle, unsigned speed)
{
	speed_t s;
	gbser_posix_handle *h = (gbser_posix_handle *) handle;
	assert(h->magic == MYMAGIC);

	s = mkspeed(speed);

	cfsetospeed(&h->my_tio, s);
	cfsetispeed(&h->my_tio, s);

	return !tcsetattr(h->fd, TCSAFLUSH, &h->my_tio);
}

int
gbser_read(void *handle, char *ibuf, int size)
{
	gbser_posix_handle *h = (gbser_posix_handle *) handle;

	ibuf[0] = 0;
	assert(h->magic == MYMAGIC);

//	n = read(h->fd, ibuf, size);
// printf("Returning %d\n", n);
redo:
	fgets(ibuf, size, h->fh);
// rtrim(ibuf);
if (strlen(ibuf) == 0)
  goto redo;
	return 1;
}
