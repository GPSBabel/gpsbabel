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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef GBFILE_H
#define GBFILE_H

#include <QtCore/QByteArray>    // for QByteArray
#include <QtCore/QString>       // for QString

#include <cstdarg>             // for va_list
#include <cstdint>             // for int32_t, int16_t, uint32_t
#include <cstdio>              // for FILE

#include "defs.h"


struct gbfile;
using gbsize_t = uint32_t;

using gbfclearerr_cb = void (*)(gbfile* self);
using gbfclose_cb = int (*)(gbfile* self);
using gbfeof_cb = int (*)(gbfile* self);
using gbferror_cb = int (*)(gbfile* self);
using gbfflush_cb = int (*)(gbfile* self);
using gbfopen_cb = gbfile* (*)(gbfile* self, const char* mode);
using gbfread_cb = gbsize_t (*)(void* buf, const gbsize_t size, const gbsize_t members, gbfile* self);
using gbfseek_cb = int (*)(gbfile* self, int32_t offset, int whence);
using gbftell_cb = gbsize_t (*)(gbfile* self);
using gbfwrite_cb = gbsize_t (*)(const void* buf, const gbsize_t size, const gbsize_t members, gbfile* self);
using gbfungetc_cb = int (*)(const int c, gbfile* self);

struct gbfile {
  union {
    FILE* std;
    unsigned char* mem;
#if !ZLIB_INHIBITED
    gzFile gz;
#endif
  } handle;
  char*   name;
  char*   module;
  char*   buff;	/* static growing buffer, primary used by gbprintf */
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
};


gbfile* gbfopen(const QString& filename, const char* mode, const char* module);
gbfile* gbfopen_be(const QString& filename, const char* mode, const char* module);
#define gbfopen_le gbfopen
void gbfclose(gbfile* file);

gbsize_t gbfread(void* buf, gbsize_t size, gbsize_t members, gbfile* file);
gbsize_t gbfread(QString& buf, gbsize_t size, gbsize_t members, gbfile* file);
// Convenience wrapper for above, but ignoring the possibility of endian swapping.
QByteArray gbfreadbuf(gbsize_t size, gbfile* file);
int gbfgetc(gbfile* file);
QString gbfgets(char* buf, int len, gbfile* file);

int gbvfprintf(gbfile* file, const char* format, va_list ap);
int gbfprintf(gbfile* file, const char* format, ...);
int gbfputc(int c, gbfile* file);
int gbfputs(const QString& s, gbfile* file);
int gbfwrite(const void* buf, gbsize_t size, gbsize_t members, gbfile* file);
int gbfflush(gbfile* file);

void gbfclearerr(gbfile* file);
int gbferror(gbfile* file);
void gbfrewind(gbfile* file);
int gbfseek(gbfile* file, int32_t offset, int whence);
gbsize_t gbftell(gbfile* file);
int gbfeof(gbfile* file);
int gbfungetc(int c, gbfile* file);

int32_t gbfgetint32(gbfile* file);
#define gbfgetuint32 (uint32_t)gbfgetint32
int16_t gbfgetint16(gbfile* file);
#define gbfgetuint16 (uint16_t)gbfgetint16
double gbfgetdbl(gbfile* file);			// read a double value
float gbfgetflt(gbfile* file);			// read a float value
char* gbfgetstr(gbfile* file);			// read until any type of line-breaks or EOF
QString gbfgetpstr(gbfile* file);		// read a pascal string
QString gbfgetcstr(gbfile* file);		// read a null terminated string
QByteArray gbfgetnativecstr(gbfile* file);  // read a null terminated string
char* gbfgetcstr_old(gbfile* file);		// read a null terminated string

int gbfputint16(int16_t i, gbfile* file);
#define gbfputuint16(a,b) gbfputint16((uint16_t)(a),(b))
int gbfputint32(int32_t i, gbfile* file);
#define gbfputuint32(a,b) gbfputint32((uint32_t)(a),(b))

int gbfputdbl(double d, gbfile* file);	// write a double value
int gbfputflt(float f, gbfile* file);	// write a float value

int gbfputcstr(const char* s, gbfile* file);	// write string including '\0'

int gbfputpstr(const QString& s, gbfile* file);	// write as pascal string

gbsize_t gbfcopyfrom(gbfile* file, gbfile* src, gbsize_t count);

#endif
