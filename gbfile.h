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

#include <QByteArray>           // for QByteArray
#include <QString>              // for QString

#include <cstdint>             // for int32_t, int16_t, uint32_t
#include <cstdio>              // for FILE

#if HAVE_LIBZ
#include <zlib.h>
#elif !ZLIB_INHIBITED
#include "zlib.h"
#endif


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
  } handle{nullptr};
  QString   name;
  char*   buff{nullptr};	/* static growing buffer, primary used by gbprintf */
  int    buffsz{0};
  char   mode{0};
  int    back{0};
  gbsize_t mempos{0};	/* curr. position in memory */
  gbsize_t memlen{0};	/* max. number of written bytes to memory */
  gbsize_t memsz{0};	/* curr. size of allocated memory */
  unsigned char big_endian:1{0};
  unsigned char binary:1{0};
  unsigned char gzapi:1{0};
  unsigned char memapi:1{0};
  unsigned char unicode:1{0};
  unsigned char unicode_checked:1{0};
  unsigned char is_pipe:1{0};
  gbfclearerr_cb fileclearerr{nullptr};
  gbfclose_cb fileclose{nullptr};
  gbfeof_cb fileeof{nullptr};
  gbferror_cb fileerror{nullptr};
  gbfflush_cb fileflush{nullptr};
  gbfopen_cb fileopen{nullptr};
  gbfread_cb fileread{nullptr};
  gbfseek_cb fileseek{nullptr};
  gbftell_cb filetell{nullptr};
  gbfungetc_cb fileungetc{nullptr};
  gbfwrite_cb filewrite{nullptr};
};


gbfile* gbfopen(const QString& filename, const char* mode);
gbfile* gbfopen_be(const QString& filename, const char* mode);
inline gbfile* gbfopen_le(const QString& filename, const char* mode)
{
  return gbfopen(filename, mode);
}
void gbfclose(gbfile* file);

gbsize_t gbfread(void* buf, gbsize_t size, gbsize_t members, gbfile* file);
gbsize_t gbfread(QString& buf, gbsize_t size, gbsize_t members, gbfile* file);
// Convenience wrapper for above, but ignoring the possibility of endian swapping.
QByteArray gbfreadbuf(gbsize_t size, gbfile* file);
int gbfgetc(gbfile* file);
QString gbfgets(char* buf, int len, gbfile* file);

[[gnu::format(printf, 2, 0)]] int gbvfprintf(gbfile* file, const char* format, va_list ap);
[[gnu::format(printf, 2, 3)]] int gbfprintf(gbfile* file, const char* format, ...);
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
inline uint32_t gbfgetuint32(gbfile* file)
{
  return gbfgetint32(file);
}
int16_t gbfgetint16(gbfile* file);
inline uint16_t gbfgetuint16(gbfile* file)
{
  return gbfgetint16(file);
}
double gbfgetdbl(gbfile* file);			// read a double value
float gbfgetflt(gbfile* file);			// read a float value
char* gbfgetstr(gbfile* file);			// read until any type of line-breaks or EOF
QString gbfgetpstr(gbfile* file);		// read a pascal string
QString gbfgetcstr(gbfile* file);		// read a null terminated string
QByteArray gbfgetnativecstr(gbfile* file);  // read a null terminated string
char* gbfgetcstr_old(gbfile* file);		// read a null terminated string

int gbfputint16(int16_t i, gbfile* file);
inline int gbfputuint16(uint16_t i, gbfile* file)
{
  return gbfputint16(i, file);
}
int gbfputint32(int32_t i, gbfile* file);
inline int gbfputuint32(uint32_t i, gbfile* file)
{
  return gbfputint32(i, file);
}

int gbfputdbl(double d, gbfile* file);	// write a double value
int gbfputflt(float f, gbfile* file);	// write a float value

int gbfputcstr(const char* s, gbfile* file);	// write string including '\0'

int gbfputpstr(const QString& s, gbfile* file);	// write as pascal string

gbsize_t gbfcopyfrom(gbfile* file, gbfile* src, gbsize_t count);

#endif
