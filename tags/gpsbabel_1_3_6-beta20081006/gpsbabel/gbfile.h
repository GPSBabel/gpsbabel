/*

    Common GPSBabel file I/O API

    Copyright (C) 2006,2007,2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef GBFILE_H
#define GBFILE_H

#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include "config.h"
#include "defs.h"
#include "cet.h"

struct gbfile_s;
typedef struct gbfile_s gbfile;

typedef void (*gbfclearerr_cb) (gbfile *self);
typedef int (*gbfclose_cb) (gbfile *self);
typedef int (*gbfeof_cb) (gbfile *self);
typedef int (*gbferror_cb) (gbfile *self);
typedef int (*gbfflush_cb) (gbfile *self);
typedef gbfile* (*gbfopen_cb) (gbfile *self, const char *mode);
typedef gbsize_t (*gbfread_cb) (void *buf, const gbsize_t size, const gbsize_t members, gbfile *self);
typedef int (*gbfseek_cb) (gbfile *self, gbint32 offset, int whence);
typedef gbsize_t (*gbftell_cb) (gbfile *self);
typedef gbsize_t (*gbfwrite_cb) (const void *buf, const gbsize_t size, const gbsize_t members, gbfile *self);
typedef int (*gbfungetc_cb) (const int c, gbfile *self);

typedef struct gbfile_s {
#ifdef DEBUG_MEM
	void   *dummy;	/* ZERO pointer for stdio oop's */
#endif
	union {
	  FILE *std;
	  unsigned char *mem;
#if !ZLIB_INHIBITED
	  gzFile *gz;
#endif
	} handle;
	char   *name;
	char   *module;
	char   *buff;	/* static growing buffer, primary used by gbprintf */
	int    buffsz;
	char   mode;
	int    back;
	gbsize_t mempos;	/* curr. position in memory */
	gbsize_t memlen;	/* max. number of written bytes to memory */
	gbsize_t memsz;		/* curr. size of allocated memory */
	unsigned char big_endian:1;
	unsigned char binary:1;
	unsigned char gzapi:1;
	unsigned char memapi:1;
	unsigned char unicode:1;
	unsigned char unicode_checked:1;
	unsigned char is_pipe:1;
	gbfclearerr_cb fileclearerr;
	gbfclose_cb fileclose;
	gbfeof_cb fileeof;
	gbferror_cb fileerror;
	gbfflush_cb fileflush;
	gbfopen_cb fileopen;
	gbfread_cb fileread;
	gbfseek_cb fileseek;
	gbftell_cb filetell;
	gbfungetc_cb fileungetc;
	gbfwrite_cb filewrite;
} gbfile_t;


gbfile *gbfopen(const char *filename, const char *mode, const char *module);
gbfile *gbfopen_be(const char *filename, const char *mode, const char *module);
#define gbfopen_le gbfopen
void gbfclose(gbfile *file);

gbsize_t gbfread(void *buf, const gbsize_t size, const gbsize_t members, gbfile *file);
int gbfgetc(gbfile *file);
char *gbfgets(char *buf, int len, gbfile *file);

int gbvfprintf(gbfile *file, const char *format, va_list ap);
int gbfprintf(gbfile *file, const char *format, ...);
int gbfputc(int c, gbfile *file);
int gbfputs(const char *s, gbfile *file);
int gbfwrite(const void *buf, const gbsize_t size, const gbsize_t members, gbfile *file);
int gbfflush(gbfile *file);

void gbfclearerr(gbfile *file);
int gbferror(gbfile *file);
void gbfrewind(gbfile *file);
int gbfseek(gbfile *file, gbint32 offset, int whence);
gbsize_t gbftell(gbfile *file);
int gbfeof(gbfile *file);
int gbfungetc(const int c, gbfile *file);

gbint32 gbfgetint32(gbfile *file);
#define gbfgetuint32 (gbuint32)gbfgetint32
gbint16 gbfgetint16(gbfile *file);
#define gbfgetuint16 (gbuint16)gbfgetint16
double gbfgetdbl(gbfile *file);			// read a double value
float gbfgetflt(gbfile *file);			// read a float value
char *gbfgetstr(gbfile *file);			// read until any type of line-breaks or EOF
char *gbfgetpstr(gbfile *file);			// read a pascal string
char *gbfgetcstr(gbfile *file);			// read a null terminated string

int gbfputint16(const gbint16 i, gbfile *file);
#define gbfputuint16(a,b) gbfputint16((gbuint16)(a),(b))
int gbfputint32(const gbint32 i, gbfile *file);
#define gbfputuint32(a,b) gbfputint32((gbuint32)(a),(b))

int gbfputdbl(const double d, gbfile *file);	// write a double value
int gbfputflt(const float f, gbfile *file);	// write a float value
int gbfputcstr(const char *s, gbfile *file);	// write string including '\0'
int gbfputpstr(const char *s, gbfile *file);	// write as pascal string

gbsize_t gbfcopyfrom(gbfile *file, gbfile *src, gbsize_t count);

#endif
