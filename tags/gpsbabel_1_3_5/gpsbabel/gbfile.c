/*

    Common GPSBabel file I/O API
    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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
	
	file->name = xstrdup(filename);
	file->module = xstrdup(module);
	file->line = xstrdup("");
	file->mode = 'r'; // default
	file->binary = (strchr(mode, 'b') != NULL);
	file->back = -1;
	
	for (m = mode; *m; m++) {
		switch(tolower(*m)) {
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
		char openmode[32];

		/* under non-posix systems files MUST be opened in binary mode */
		
		strcpy(openmode, mode);
		if (strchr(mode, 'b') == NULL)
			strncat(openmode, "b", sizeof(openmode));
		
		if (strcmp(filename, "-") == 0) {
			FILE *fd;
			if (file->mode == 'r')
				fd = stdin;
			else
				fd = stdout;
			SET_BINARY_MODE(fd);
			file->handle.gz = gzdopen(fileno(fd), openmode);
		}
		else
			file->handle.gz = gzopen(filename, openmode);
			
		if (file->handle.gz == NULL) {
			fatal("%s: Cannot %s file '%s'!\n", 
				module, 
				(file->mode == 'r') ? "open" : "create",
				filename);
		}
		file->gzapi = 1;
#else
		/* This is the only runtime test we make */
		fatal("%s: Zlib was not included in this build.\n", file->module);
#endif
	}
	else {
		file->handle.std = xfopen(filename, mode, module);
	}
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
	if (!file) return;

	if (file->gzapi) {
#if !ZLIB_INHIBITED
		gzclose(file->handle.gz);
#else
		fatal(NO_ZLIB);
#endif
	}
	else {
		fclose(file->handle.std);
	}
	xfree(file->name);
	xfree(file->module);
	xfree(file->line);
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
	}
	else {
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

		if (c == EOF) break;
		
		*(unsigned char *)buf = (unsigned char)c;
		buf++;
		
		if (c == '\r') {
			c = gbfgetc(file);
			if ((c != '\n') && (c != EOF)) gbfungetc(c, file);
			break;
		}
		else if (c == '\n')
			break;
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
	if ((size == 0) || (members == 0)) return 0;
	
	if (file->gzapi) {
#if !ZLIB_INHIBITED
		int result = 0;
		char *target = buf;
		int count = size * members;
		
		if (file->back != -1) {
			*target++ = file->back;
			count--;
			result++;
			file->back = -1;
		}
		result += gzread(file->handle.gz, target, count);
		result /= size;

		if ((result < 0) || ((gbsize_t)result < members)) {
			int errnum;
			const char *errtxt;
			
			errtxt = gzerror(file->handle.gz, &errnum);
			
			/* Workaround for zlib bug: buffer error on empty files */
			if ((errnum == Z_BUF_ERROR) && (gztell(file->handle.gz) == 0)) {
				return (gbsize_t) 0;
			}
			if ((errnum != Z_STREAM_END) && (errnum != 0))
				fatal("%s: zlib returned error %d ('%s')!\n",
					file->module, errnum, errtxt);
		}
		return (gbsize_t) result;
#else
		fatal(NO_ZLIB);
		return -1;
#endif
	}
	else {
		int errno;
		gbsize_t result = fread(buf, size, members, file->handle.std);
		
		if ((result < members) && (errno = ferror(file->handle.std))) {
			fatal("%s: Error %d occured during read of file '%s'!\n",
				file->module, errno, file->name);
		}
		return result;
	}
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
		if ((len > -1) && (len < file->buffsz))
			break;

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
		if (len > -1) 
			file->buffsz = len + 1;
		else 
			file->buffsz *= 2;

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
	
	if ((size == 0) || (members == 0)) return 0;

	if (file->gzapi) {
#if !ZLIB_INHIBITED
		result = gzwrite(file->handle.gz, buf, size * members) / size;
#else
		fatal(NO_ZLIB);
		return -1;
#endif
	}
	else {
		result = fwrite(buf, size, members, file->handle.std);
	}

	if (result != members) {
		fatal("%s: Could not write %lld bytes to %s!\n", 
			file->module,
			(long long int) (members - result) * size,
			file->name);
	}
		
	return result;
}

/*
 * gbfflush: (as fflush)
 */
 
int
gbfflush(gbfile *file)
{
	if (file->gzapi) {
#if !ZLIB_INHIBITED
		return gzflush(file->handle.gz, Z_SYNC_FLUSH);
#else
		fatal(NO_ZLIB);
		return -1;
#endif
	}
	else {
		return fflush(file->handle.std);
	}
}

/*
 * gbfclearerr: (as clearerr)
 */

void
gbfclearerr(gbfile *file)
{
	if (file->gzapi) {
#if !ZLIB_INHIBITED
		gzclearerr(file->handle.gz);
#else
		fatal(NO_ZLIB);
#endif
	}
	else {
		clearerr(file->handle.std);
	}
}

/*
 * gbferror: (as ferror)
 */
 
int
gbferror(gbfile *file)
{
	int errnum;
	
	if (file->gzapi) {
#if !ZLIB_INHIBITED
		(void)gzerror(file->handle.gz, &errnum);
#else
		fatal(NO_ZLIB);
		return -1;
#endif
	}
	else {
		errnum = ferror(file->handle.std);
	}
	return errnum;
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
	int result;

	if (file->gzapi) {
		
		assert(whence != SEEK_END);

#if !ZLIB_INHIBITED
		if ((whence == SEEK_CUR) && (file->back != -1)) offset--;
		result = gzseek(file->handle.gz, offset, whence);
		file->back = -1;
#else
		result = 1;
#endif
		if (result < 0) {
			if (strcmp(file->name, "-") == 0)
				fatal("%s: This format cannot be used in piped commands!\n", file->module);
			fatal("%s: online compression not yet supported for this format!", file->module);
		}
		return 0;
	}
	else {
		gbsize_t pos = 0;
		
		if (whence != SEEK_SET) pos = ftell(file->handle.std);

		result = fseek(file->handle.std, offset, whence);
		if (result != 0) {
			switch (whence) {
			case SEEK_CUR:
			case SEEK_END: pos = pos + offset; break;
			case SEEK_SET: pos = offset; break;
			default:
				fatal("%s: Unknown seek operation (%d) for file %s!\n",
					file->module, whence, file->name);
			}
			fatal("%s: Unable to set file (%s) to position (%llu)!\n",
				file->module, file->name, (long long unsigned) pos);
		}
		return 0;
	}
}

/*
 * gbftell: (as ftell)
 */

gbsize_t 
gbftell(gbfile *file)
{
	gbsize_t result;
	
	if (file->gzapi) {
#if !ZLIB_INHIBITED
		result = gztell(file->handle.gz);
		if (file->back != -1) {
//			file->back = -1;
			result--;
		}
#else
		fatal(NO_ZLIB);
		result = -1;
#endif
	}
	else {
		result = ftell(file->handle.std);
	}
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
	if (file->gzapi) {
#if !ZLIB_INHIBITED
		int res;
		
		if (file->back != -1) return 0;

		res  = gzeof(file->handle.gz);
		if (!res) {
			unsigned char test;
			int len = gzread(file->handle.gz, &test, 1);
			if (len == 1) {
				/* No EOF, put the single byte back into stream */
				file->back = test;
			}
			else {
				/* we are at the end of the file */
				if (global_opts.debug_level > 0) {
					/* now gzeof() should return 1 */
					is_fatal(!gzeof(file->handle.gz), "zlib gzeof error!\n");
				}
				res = 1;
			}
		}
		return res;
#else
		fatal(NO_ZLIB);
		return 0;
#endif
	}
	else {
		return feof(file->handle.std);
	}
}

/*
 * gbfungetc: (as fungetc)
 */

int
gbfungetc(const int c, gbfile *file)
{
	int res;

	if (file->gzapi) {
#if !ZLIB_INHIBITED
		if (file->back == -1) {
			file->back = c;
			res = c;
		}
		else {
			fatal(MYNAME ": Cannot store more than one byte back!\n");
		}
#else
		fatal(NO_ZLIB);
#endif
	}
	else {
		res = ungetc(c, file->handle.std);
	}
	return res;
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

	if (file->big_endian)
		return be_read32(buf);
	else
		return le_read32(buf);
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
	
	if (file->big_endian)
		return be_read16(buf);
	else
		return le_read16(buf);
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
		char c = gbfgetc(file);
		
		if ((c == 0) || (c == EOF)) break;
		
		if (len == file->buffsz) {
			file->buffsz += 64;
			str = file->buff = xrealloc(file->buff, file->buffsz + 1);
		}
		str[len] = c;
		len++;
	}
	
	result = (char *) xmalloc(len + 1);
	if (len > 0)
		memcpy(result, str, len);
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

/*
 * gbfgetstr: Reads a string from file (util any type of line-breaks or eof or error)
 *            except xfree and free you can do all possible things with the result
 */

char *
gbfgetstr(gbfile *file)
{
	int len = 0;
	char *result = file->line;
	
	for (;;) {
		char c = gbfgetc(file);
		
		if ((c == EOF) || (c == 0x1A)) {
			if (len == 0) {
				return NULL;
			}
			break;
		}
		else if (c == '\r') {
			c = gbfgetc(file);
			if ((c != '\n') && (c != EOF))
				gbfungetc(c, file);
			break;
		}
		else if (c == '\n') {
			break;
		}
		if (len == file->linesz) {
			file->linesz += 64;
			result = file->line = xrealloc(file->line, file->linesz + 1);
		}
		result[len] = c;
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
	
	if (file->big_endian)
		be_write16(buf, i);
	else
		le_write16(buf, i);
	return gbfwrite(buf, 1, sizeof(buf), file);
}

/*
 * gbfputint32: write a signed 32-bit integer value into output stream
 */

int
gbfputint32(const gbint32 i, gbfile *file)
{
	char buf[4];
	
	if (file->big_endian)
		be_write32(buf, i);
	else
		le_write32(buf, i);
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
	if (len > 255) len = 255;	/* the maximum size of a standard pascal string */
	gbfputc(len, file);
	if (len > 0) {
		gbfwrite(s, 1, len, file);
	}
	return (len + 1);
}

/* Thats all, sorry. */
