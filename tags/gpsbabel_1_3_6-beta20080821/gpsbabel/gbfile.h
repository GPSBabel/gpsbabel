/*

    Common GPSBabel file I/O API

    Copyright (C) 2006 Olaf Klein 

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

#include "config.h"
#include "defs.h"
#include <stdarg.h>

typedef struct gbfile_s {
#ifdef DEBUG_MEM
	void   *dummy;	/* ZERO pointer for stdio oop's */
#endif
	union {
	  FILE *std;
#if !ZLIB_INHIBITED
	  gzFile *gz;
#endif
	} handle;
	char   *name;
	char   *module;
	char   *line;
	int    linesz;
	char   *buff;	/* static growing buffer, primary used by gbprintf */
	int    buffsz;
	char   mode;
	int    back;
	unsigned char big_endian:1;
	unsigned char binary:1;
	unsigned char gzapi:1;
} gbfile;


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

#endif
