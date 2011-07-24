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

#include "defs.h"
#include "gbfile.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>


#if __WIN32__
/* taken from minigzip.c (part of the zlib project) */
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) _setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define MYNAME "gbfile"
#define NO_ZLIB MYNAME ": No zlib support.\n"

/* About the ZLIB_INHIBITED stuff:
 *
 * If a user goes out of his way to build with ZLIB_INHIBITED set,
 * we jettison our use of zlib entirely within this file, replacing
 * all calls out to zlib with calls to abort() as that's an internal
 * consistency error.
 *
 */


/*******************************************************************************/
/* %%%                          file api wrappers                           %%% */
/*******************************************************************************/

#if !ZLIB_INHIBITED

/*******************************************************************************/
/* %%%                            Zlib file api                            %%% */
/*******************************************************************************/

static gbfile *
gzapi_open(gbfile *self, const char *mode)
{
  char openmode[32];

  self->gzapi = 1;

  /* under non-posix systems files MUST be opened in binary mode */

  strcpy(openmode, mode);
  if (strchr(mode, 'b') == NULL) {
    strncat(openmode, "b", sizeof(openmode));
  }

  if (self->is_pipe) {
    FILE *fd;
    if (self->mode == 'r') {
      fd = stdin;
    } else {
      fd = stdout;
    }
    SET_BINARY_MODE(fd);
    self->handle.gz = gzdopen(fileno(fd), openmode);
  } else {
    self->handle.gz = gzopen(self->name, openmode);
  }

  if (self->handle.gz == NULL) {
    fatal("%s: Cannot %s file '%s'!\n",
          self->module,
          (self->mode == 'r') ? "open" : "create",
          self->name);
  }

  return self;
}

static int
gzapi_close(gbfile *self)
{
  return gzclose(self->handle.gz);
}

static int
gzapi_seek(gbfile *self, gbint32 offset, int whence)
{
  int result;

  assert(whence != SEEK_END);

  if ((whence == SEEK_CUR) && (self->back != -1)) {
    offset--;
  }
  result = gzseek(self->handle.gz, offset, whence);
  self->back = -1;

  if (result < 0) {
    if (self->is_pipe) {
      fatal("%s: This format cannot be used in piped commands!\n", self->module);
    }
    fatal("%s: online compression not yet supported for this format!", self->module);
  }
  return 0;
}

static gbsize_t
gzapi_read(void *buf, const gbsize_t size, const gbsize_t members, gbfile *self)
{
  int result = 0;
  char *target = buf;
  int count = size * members;

  if (self->back != -1) {
    *target++ = self->back;
    count--;
    result++;
    self->back = -1;
  }
  result += gzread(self->handle.gz, target, count);

  /* Check for an incomplete READ */
  if ((members == 1) && (size > 1) && (result > 0) && (result < (int)size)) {
    fatal("%s: Unexpected end of file (EOF)!\n", self->module);
  }

  result /= size;

  if ((result < 0) || ((gbsize_t)result < members)) {
    int errnum;
    const char *errtxt;

    errtxt = gzerror(self->handle.gz, &errnum);

    /* Workaround for zlib bug: buffer error on empty files */
    if ((errnum == Z_BUF_ERROR) && (gztell(self->handle.gz) == 0)) {
      return (gbsize_t) 0;
    }
    if ((errnum != Z_STREAM_END) && (errnum != 0))
      fatal("%s: zlib returned error %d ('%s')!\n",
            self->module, errnum, errtxt);
  }
  return (gbsize_t) result;
}

static gbsize_t
gzapi_write(const void *buf, const gbsize_t size, const gbsize_t members, gbfile *self)
{
  return gzwrite(self->handle.gz, buf, size * members) / size;
}

static int
gzapi_flush(gbfile *self)
{
  return gzflush(self->handle.gz, Z_SYNC_FLUSH);
}

static gbsize_t
gzapi_tell(gbfile *self)
{
  gbsize_t result;

  result = gztell(self->handle.gz);
  if (self->back != -1) {
    result--;
  }

  return result;
}

static int
gzapi_eof(gbfile *self)
{
  int res = 0;

  if (self->back != -1) {
    return res;
  }

  res  = gzeof(self->handle.gz);
  if (!res) {
    unsigned char test;
    int len = gzread(self->handle.gz, &test, 1);
    if (len == 1) {
      /* No EOF, put the single byte back into stream */
      self->back = test;
    } else {
      /* we are at the end of the file */
      if (global_opts.debug_level > 0) {
        /* now gzeof() should return 1 */
        is_fatal(!gzeof(self->handle.gz), "zlib gzeof error!\n");
      }
      res = 1;
    }
  }
  return res;
}

static int
gzapi_ungetc(const int c, gbfile *self)
{
  if (self->back == -1) {
    self->back = c;
  } else {
    fatal(MYNAME ": Cannot store more than one byte back!\n");
  }
  return c;
}

static void
gzapi_clearerr(gbfile *self)
{
  gzclearerr(self->handle.gz);
}

static int
gzapi_error(gbfile *self)
{
  int errnum;

  (void)gzerror(self->handle.gz, &errnum);

  return errnum;
}
#endif	// #if !ZLIB_INHIBITED


/*******************************************************************************/
/* %%%                         Standard C file api                         %%% */
/*******************************************************************************/

static gbfile *
stdapi_open(gbfile *self, const char *mode)
{
  self->handle.std = xfopen(self->name, mode, self->module);
  return self;
}

static int
stdapi_close(gbfile *self)
{
  return fclose(self->handle.std);
}

static int
stdapi_seek(gbfile *self, gbint32 offset, int whence)
{
  int result;
  gbsize_t pos = 0;

  if (whence != SEEK_SET) {
    pos = ftell(self->handle.std);
  }

  result = fseek(self->handle.std, offset, whence);
  if (result != 0) {
    switch (whence) {
    case SEEK_CUR:
    case SEEK_END:
      pos = pos + offset;
      break;
    case SEEK_SET:
      pos = offset;
      break;
    default:
      fatal("%s: Unknown seek operation (%d) for file %s!\n",
            self->module, whence, self->name);
    }
    fatal("%s: Unable to set file (%s) to position (%llu)!\n",
          self->module, self->name, (long long unsigned) pos);
  }
  return 0;
}

static gbsize_t
stdapi_read(void *buf, const gbsize_t size, const gbsize_t members, gbfile *self)
{
  int errno;
  gbsize_t result = fread(buf, size, members, self->handle.std);

  if ((result < members) && (errno = ferror(self->handle.std))) {
    fatal("%s: Error %d occured during read of file '%s'!\n",
          self->module, errno, self->name);
  }
  return result;
}

static gbsize_t
stdapi_write(const void *buf, const gbsize_t size, const gbsize_t members, gbfile *self)
{
  return fwrite(buf, size, members, self->handle.std);
}

static int
stdapi_flush(gbfile *self)
{
  return fflush(self->handle.std);
}

static gbsize_t
stdapi_tell(gbfile *self)
{
  return ftell(self->handle.std);
}

static int
stdapi_eof(gbfile *self)
{
  return feof(self->handle.std);
}

static int
stdapi_ungetc(const int c, gbfile *self)
{
  return ungetc(c, self->handle.std);
}

static void
stdapi_clearerr(gbfile *self)
{
  clearerr(self->handle.std);
}

static int
stdapi_error(gbfile *self)
{
  return ferror(self->handle.std);
}


/*******************************************************************************/
/* %%%                     Memory stream (memapi)                          %%% */
/*******************************************************************************/

static gbfile *
memapi_open(gbfile *self, const char *mode)
{
  self->mempos = 0;
  self->memsz = 0;
  self->handle.mem = NULL;

  return self;
}

static int
memapi_close(gbfile *self)
{
  if (self->handle.mem) {
    xfree(self->handle.mem);
  }

  return 0;
}

static int
memapi_seek(gbfile *self, gbint32 offset, int whence)
{
  long long pos = (int)self->mempos;

  switch (whence) {
  case SEEK_CUR:
  case SEEK_END:
    pos = pos + offset;
    break;
  case SEEK_SET:
    pos = offset;
    break;
  }

  if ((pos < 0) || (pos > self->memlen)) {
    return -1;
  }

  self->mempos = pos;
  return 0;
}

static gbsize_t
memapi_read(void *buf, const gbsize_t size, const gbsize_t members, gbfile *self)
{
  gbsize_t count;
  gbsize_t result = (self->memlen - self->mempos) / size;

  if (result > members) {
    result = members;
  }
  count = result * size;
  if (count) {
    memcpy(buf, self->handle.mem + self->mempos, count);
    self->mempos += count;
  }

  return result;
}

static gbsize_t
memapi_write(const void *buf, const gbsize_t size, const gbsize_t members, gbfile *self)
{
  gbsize_t count;

  if ((size == 0) && (members == 0)) {	/* truncate stream */
    self->memlen = self->mempos;
    return 0;
  }

  count = size * members;

  if (self->mempos + count > self->memsz) {
    self->memsz = ((self->mempos + count + 4095) / 4096) * 4096;
    self->handle.mem = xrealloc(self->handle.mem, self->memsz);
  }
  memcpy(self->handle.mem + self->mempos, buf, count);
  self->mempos += count;
  if (self->mempos > self->memlen) {
    self->memlen = self->mempos;
  }

  return members;
}

static int
memapi_flush(gbfile *self)
{
  return 0;
}

static gbsize_t
memapi_tell(gbfile *self)
{
  return self->mempos;
}

static int
memapi_eof(gbfile *self)
{
  return (self->mempos == self->memlen);
}

static int
memapi_ungetc(const int c, gbfile *self)
{
  if (self->mempos == 0) {
    return EOF;
  } else {
    self->mempos--;
    self->handle.mem[self->mempos] = (unsigned char) c;
    return c;
  }
}

static void
memapi_clearerr(gbfile *self)
{
  return;
}

static int
memapi_error(gbfile *self)
{
  return 0;
}


/* GPSBabel 'file' standard calls */

/*
 * gbfopen: (as xfopen) plus the name of the calling GPSBabel module (MYNAME)
 */

gbfile *
gbfopen(const char *filename, const char *mode, const char *module)
{
  gbfile *file;
  const char *m;
  int len;

  file = xcalloc(1, sizeof(*file));

  file->module = xstrdup(module);
  file->mode = 'r'; // default
  file->binary = (strchr(mode, 'b') != NULL);
  file->back = -1;
  file->memapi = (filename == NULL);

  for (m = mode; *m; m++) {
    switch (tolower(*m)) {
    case 'r':
      file->mode = 'r';
#if !ZLIB_INHIBITED
      file->gzapi = 1;	/* native or transparent */
#endif
      break;
    case 'w':
      file->mode = 'w';
      break;
    }
  }

  if (file->memapi) {
    file->gzapi = 0;
    file->name = xstrdup("(Memory stream)");

    file->fileclearerr = memapi_clearerr;
    file->fileclose = memapi_close;
    file->fileeof = memapi_eof;
    file->fileerror = memapi_error;
    file->fileflush = memapi_flush;
    file->fileopen = memapi_open;
    file->fileread = memapi_read;
    file->fileseek = memapi_seek;
    file->filetell = memapi_tell;
    file->fileungetc = memapi_ungetc;
    file->filewrite = memapi_write;
  } else {
    file->name = xstrdup(filename);
    file->is_pipe = (strcmp(filename, "-") == 0);

    /* Do we have a '.gz' extension in the filename ? */
    len = strlen(file->name);
    if ((len > 3) && (case_ignore_strcmp(&file->name[len-3], ".gz") == 0)) {
#if !ZLIB_INHIBITED
      /* force gzipped files on output */
      file->gzapi = 1;
#else
      fatal(NO_ZLIB);
#endif
    }

    if (file->gzapi) {
#if !ZLIB_INHIBITED

      file->fileclearerr = gzapi_clearerr;
      file->fileclose = gzapi_close;
      file->fileeof = gzapi_eof;
      file->fileerror = gzapi_error;
      file->fileflush = gzapi_flush;
      file->fileopen = gzapi_open;
      file->fileread = gzapi_read;
      file->fileseek = gzapi_seek;
      file->filetell = gzapi_tell;
      file->fileungetc = gzapi_ungetc;
      file->filewrite = gzapi_write;
#else
      /* This is the only runtime test we make */
      fatal("%s: Zlib was not included in this build.\n", file->module);
#endif
    } else {
      file->fileclearerr = stdapi_clearerr;
      file->fileclose = stdapi_close;
      file->fileeof = stdapi_eof;
      file->fileerror = stdapi_error;
      file->fileflush = stdapi_flush;
      file->fileopen = stdapi_open;
      file->fileread = stdapi_read;
      file->fileseek = stdapi_seek;
      file->filetell = stdapi_tell;
      file->fileungetc = stdapi_ungetc;
      file->filewrite = stdapi_write;
    }
  }

  file->fileopen(file, mode);

#ifdef DEBUG_MEM
  file->buffsz = 1;
#else
  file->buffsz = 256;
#endif
  file->buff = xmalloc(file->buffsz);

  return file;
}

/*
 * gbfopen_be: as gbfopen, but set the BIG-ENDIAN flag
 */

gbfile *
gbfopen_be(const char *filename, const char *mode, const char *module)
{
  gbfile *result;

  result = gbfopen(filename, mode, module);
  result->big_endian = 1;

  return result;
}

/*
 * gbfclose: (as fclose)
 */

void
gbfclose(gbfile *file)
{
  if (!file) {
    return;
  }

  file->fileclose(file);

  xfree(file->name);
  xfree(file->module);
  xfree(file->buff);
  xfree(file);
}

/*
 * gbfgetc: (as fgetc)
 */

int
gbfgetc(gbfile *file)
{
  unsigned char c;

  /* errors are caught in gbfread */
  if (gbfread(&c, 1, 1, file) == 0) {
    return EOF;
  } else {
    return (unsigned int)c;
  }
}

/*
 * gbfgets: (as fgets)
 */

char *
gbfgets(char *buf, int len, gbfile *file)
{
  char *result = buf;

  while (--len > 0) {
    int c = gbfgetc(file);

    if (c == EOF) {
      break;
    }

    *(unsigned char *)buf = (unsigned char)c;
    buf++;

    if (c == '\r') {
      c = gbfgetc(file);
      if ((c != '\n') && (c != EOF)) {
        gbfungetc(c, file);
      }
      break;
    } else if (c == '\n') {
      break;
    }
  }
  *buf = '\0';
  return (*result != '\0') ? result : NULL;
}

/*
 * gbfread: (as fread)
 */

gbsize_t
gbfread(void *buf, const gbsize_t size, const gbsize_t members, gbfile *file)
{
  if ((size == 0) || (members == 0)) {
    return 0;
  }
  return file->fileread(buf, size, members, file);
}

/*
 * gbvfprintf: (as vfprintf)
 */

int gbvfprintf(gbfile *file, const char *format, va_list ap)
{
  int len;

  for (;;) {
    va_list args;

    va_copy(args, ap);
    len = vsnprintf(file->buff, file->buffsz, format, args);
    va_end(args);

    /* Unambiguous Success */
    if ((len > -1) && (len < file->buffsz)) {
      break;
    }

    /* First case: C99 behaviour.  Len is correctly sized.
     * add space for null terminator.  Next time through the
     * loop we're guaranteed success.
     *
     * Second case: SUS (and Windows) behaviour.  We know it
     * doesn't fit, but we don't know how big it has to be.
    `		 * double it and try again.  We'll loop until we succeed.
     *
     * Since we keep the I/O buffer in the file handle, we
     * quickly reach a steady state on the size of these buffers.
     */
    if (len > -1) {
      file->buffsz = len + 1;
    } else {
      file->buffsz *= 2;
    }

    file->buff = xrealloc(file->buff, file->buffsz);
  }
  return gbfwrite(file->buff, 1, len, file);
}

/*
 * gbfprintf: (as fprintf)
 */

int
gbfprintf(gbfile *file, const char *format, ...)
{
  va_list args;
  int result;

  va_start(args, format);
  result = gbvfprintf(file, format, args);
  va_end(args);

  return result;
}

/*
 * gbfputc: (as fputc)
 */

int
gbfputc(int c, gbfile *file)
{
  unsigned char temp = (unsigned int) c;

  gbfwrite(&temp, 1, 1, file);

  return c;
}

/*
 * gbfputs: (as fputs)
 */

int
gbfputs(const char *s, gbfile *file)
{
  return gbfwrite(s, 1, strlen(s), file);
}

/*
 * gbfwrite: (as fwrite)
 */

int
gbfwrite(const void *buf, const gbsize_t size, const gbsize_t members, gbfile *file)
{
  int result;

  result = file->filewrite(buf, size, members, file);
  if (result != members) {
    fatal("%s: Could not write %lld bytes to %s (result %d)!\n",
          file->module,
          (long long int)(members - result) * size,
          file->name,
          result);
  }

  return result;
}

/*
 * gbfflush: (as fflush)
 */

int
gbfflush(gbfile *file)
{
  return file->fileflush(file);
}

/*
 * gbfclearerr: (as clearerr)
 */

void
gbfclearerr(gbfile *file)
{
  file->fileclearerr(file);
}

/*
 * gbferror: (as ferror)
 */

int
gbferror(gbfile *file)
{
  return file->fileerror(file);
}

/*
 * gbfrewind: (as frewind)
 */

void
gbfrewind(gbfile *file)
{
  (void) gbfseek(file, 0, SEEK_SET);
  gbfclearerr(file);
}

/*
 * gbfseek: (as fseek)
 */

int
gbfseek(gbfile *file, gbint32 offset, int whence)
{
  return file->fileseek(file, offset, whence);
}

/*
 * gbftell: (as ftell)
 */

gbsize_t
gbftell(gbfile *file)
{
  gbsize_t result = file->filetell(file);
  if ((signed) result == -1)
    fatal("%s: Could not determine position of file '%s'!\n",
          file->module, file->name);
  return result;
}

/*
 * gbfeof: (as feof)
 */

int
gbfeof(gbfile *file)
{
  return file->fileeof(file);
}

/*
 * gbfungetc: (as fungetc)
 */

int
gbfungetc(const int c, gbfile *file)
{
  return file->fileungetc(c, file);
}

/* GPSBabel 'file' enhancements */

/*
 * gbfgetint32: read a signed 32-bit integer from input stream
 */

gbint32
gbfgetint32(gbfile *file)
{
  char buf[4];

  is_fatal((gbfread(&buf, 1, sizeof(buf), file) != sizeof(buf)),
           "%s: Unexpected end of file (%s)!\n", file->module, file->name);

  if (file->big_endian) {
    return be_read32(buf);
  } else {
    return le_read32(buf);
  }
}

/*
 * gbfgetint16: read a signed 16-bit integer from input stream
 */

gbint16
gbfgetint16(gbfile *file)
{
  char buf[2];

  is_fatal((gbfread(&buf, 1, sizeof(buf), file) != sizeof(buf)),
           "%s: Unexpected end of file (%s)!\n", file->module, file->name);

  if (file->big_endian) {
    return be_read16(buf);
  } else {
    return le_read16(buf);
  }
}

/*
 * gbfgetdbl: read a double value (8 byte, double precision) from input stream
 */

double
gbfgetdbl(gbfile *file)
{
  char buf[8];

  is_fatal((gbfread(&buf, 1, sizeof(buf), file) != sizeof(buf)),
           "%s: Unexpected end of file (%s)!\n", file->module, file->name);

  return endian_read_double(buf, ! file->big_endian);
}

/*
 * gbfgetflt: read a float value (4 byte, single precision) from input stream
 */

float
gbfgetflt(gbfile *file)
{
  char buf[4];

  is_fatal((gbfread(&buf, 1, sizeof(buf), file) != sizeof(buf)),
           "%s: Unexpected end of file (%s)!\n", file->module, file->name);

  return endian_read_float(buf, ! file->big_endian);
}

/*
 * gbfgetcstr: Reads a string from file until either a '\0' or eof.
 *             The result is a temporary allocated entity: use it or free it!
 */

char *
gbfgetcstr(gbfile *file)
{
  char *result;
  int len = 0;
  char *str = file->buff;

  for (;;) {
    int c = gbfgetc(file);

    if ((c == 0) || (c == EOF)) {
      break;
    }

    if (len == file->buffsz) {
      file->buffsz += 64;
      str = file->buff = xrealloc(file->buff, file->buffsz + 1);
    }
    str[len] = c;
    len++;
  }

  result = (char *) xmalloc(len + 1);
  if (len > 0) {
    memcpy(result, str, len);
  }
  result[len] = '\0';

  return result;
}

/*
 * gbfgetpstr: Reads a pascal string (first byte is length) from file.
 *             The result is a temporary allocated entity: use it or free it!
 */

char *
gbfgetpstr(gbfile *file)
{
  int len;
  char *result;

  len = gbfgetc(file);
  result = xmalloc(len + 1);
  if (len > 0) {
    gbfread(result, 1, len, file);
  }
  result[len] = '\0';

  return result;
}

static char *
gbfgetucs2str(gbfile *file)
{
  int len = 0;
  char *result = file->buff;

  for (;;) {
    char buff[8];
    int clen;
    int c0, c1;

    c0 = gbfgetc(file);
    if ((c0 == EOF) && (len == 0)) {
      return NULL;
    }
    c1 = gbfgetc(file);
    if ((c1 == EOF) && (len == 0)) {
      return NULL;
    }

    if (file->big_endian) {
      c0 = c1 | (c0 << 8);
    } else {
      c0 = c0 | (c1 << 8);
    }

    if (c0 == '\r') {

      c0 = gbfgetc(file);
      if ((c0 == EOF) && (len == 0)) {
        return NULL;
      }
      c1 = gbfgetc(file);
      if ((c1 == EOF) && (len == 0)) {
        return NULL;
      }

      if (file->big_endian) {
        c0 = c1 | (c0 << 8);
      } else {
        c0 = c0 | (c1 << 8);
      }

      if (c0 != '\n')
        fatal("%s: Invalid unicode (UCS-2/%s endian) line break!\n",
              file->module,
              file->big_endian ? "Big" : "Little");
      break;
    }

    clen = cet_ucs4_to_utf8(buff, sizeof(buff), c0);

    if (len+clen >= file->buffsz) {
      file->buffsz += 64;
      result = file->buff = xrealloc(file->buff, file->buffsz + 1);
    }
    memcpy(&result[len], buff, clen);
    len += clen;
  }
  result[len] = '\0';	// terminate resulting string

  return result;
}

/*
 * gbfgetstr: Reads a string from file (util any type of line-breaks or eof or error)
 *            except xfree and free you can do all possible things with the result
 */

char *
gbfgetstr(gbfile *file)
{
  int len = 0;
  char *result = file->buff;

  if (file->unicode) {
    return gbfgetucs2str(file);
  }

  for (;;) {
    int c = gbfgetc(file);

    if ((c == EOF) || (c == 0x1A)) {
      if (len == 0) {
        return NULL;
      }
      break;
    } else if (c == '\r') {
      c = gbfgetc(file);
      if ((c != '\n') && (c != EOF)) {
        gbfungetc(c, file);
      }
      break;
    } else if (c == '\n') {
      break;
    } else if (((c == 0xFE) || (c == 0xFF)) && (! file->unicode_checked)) {
      int cx;
      int c1 = gbfgetc(file);
      if (c1 != EOF) {
        cx = c | (c1 << 8);
        if (cx == 0xFEFF) {
          file->unicode = 1;
          file->big_endian = 0;
          return gbfgetucs2str(file);
        } else if (cx == 0xFFFE) {
          file->unicode = 1;
          file->big_endian = 1;
          return gbfgetucs2str(file);
        } else {
          gbfungetc(c1, file);
        }
      }
    }

    file->unicode_checked = 1;

    if ((len + 1) == file->buffsz) {
      file->buffsz += 64;
      result = file->buff = xrealloc(file->buff, file->buffsz + 1);
    }
    result[len] = (char)c;
    len++;
  }
  result[len] = '\0';	// terminate resulting string

  return result;
}

/*
 * gbfputint16: write a signed 16-bit integer value into output stream
 */

int
gbfputint16(const gbint16 i, gbfile *file)
{
  char buf[2];

  if (file->big_endian) {
    be_write16(buf, i);
  } else {
    le_write16(buf, i);
  }
  return gbfwrite(buf, 1, sizeof(buf), file);
}

/*
 * gbfputint32: write a signed 32-bit integer value into output stream
 */

int
gbfputint32(const gbint32 i, gbfile *file)
{
  char buf[4];

  if (file->big_endian) {
    be_write32(buf, i);
  } else {
    le_write32(buf, i);
  }
  return gbfwrite(buf, 1, sizeof(buf), file);
}

/*
 * gbfputdbl: write a double value (8 byte, double precision) into output stream
 */

int
gbfputdbl(const double d, gbfile *file)
{
  char buf[8];

  endian_write_double(buf, d, ! file->big_endian);
  return gbfwrite(buf, 1, sizeof(buf), file);
}

/*
 * gbfputflt: write a float value (4 byte, single precision) into output stream
 */

int
gbfputflt(const float f, gbfile *file)
{
  char buf[4];

  endian_write_float(buf, f, ! file->big_endian);
  return gbfwrite(buf, 1, sizeof(buf), file);
}

/*
 * gbfputcstr: write a NULL terminated string into a stream (!) including NULL
 *             return the number of written characters
 */

int
gbfputcstr(const char *s, gbfile *file)
{
  int len;

  len = (s == NULL) ? 0 : strlen(s);
  if (len > 0) {
    return gbfwrite(s, 1, len + 1, file);
  } else {
    gbfputc(0, file);
    return 1;
  }
}

/*
 * gbfputcstr: write a pascal string into a stream
 *             return the number of written characters
 */

int
gbfputpstr(const char *s, gbfile *file)
{
  int len;

  len = (s == NULL) ? 0 : strlen(s);
  if (len > 255) {
    len = 255;  /* the maximum size of a standard pascal string */
  }
  gbfputc(len, file);
  if (len > 0) {
    gbfwrite(s, 1, len, file);
  }
  return (len + 1);
}

/* Much more higher level functions */

gbsize_t
gbfcopyfrom(gbfile *file, gbfile *src, gbsize_t count)
{
  char buf[1024];
  gbsize_t copied = 0;

  while (count) {
    gbsize_t n = gbfread(buf, 1, (count < sizeof(buf)) ? count : sizeof(buf), src);
    if (n > 0) {
      gbfwrite(buf, 1, n, file);
      count -= n;
      copied += n;
    } else {
      break;
    }
  }
  return copied;
}


/* Thats all, sorry. */
