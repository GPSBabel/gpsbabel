/* util.c
 *
 * Misc. utility functions.
 *
 *	Copyright (C) 1999, Andrew Arensburger.
 *	You may distribute this file under the terms of the Artistic
 *	License, as specified in the README file.
 *
 * The get_*() functions are used to extract values out of strings of
 * ubytes and convert them to the native format.
 * The put_*() functions, conversely, are used to take a value in the
 * native format, convert them to Palm (big-endian) format, and write
 * them to a ubyte string.
 *
 * $Id: util.c,v 1.1 2002-08-16 06:13:10 robertl Exp $
 */

#include "config.h"
#include <stdio.h>
#include <ctype.h>	/* For isprint() */
#include <pconn/util.h>

#ifndef EPOCH_1904
#  define EPOCH_1904	2082844800L	/* Difference, in seconds, between
					 * Palm's epoch (Jan. 1, 1904) and
					 * Unix's epoch (Jan. 1, 1970).
					 */
#endif	/* EPOCH_1904 */

/* XXX - Most of the functions below really ought to be inlined. Not sure
 * how to do this portably, though.
 */

INLINE ubyte
peek_ubyte(const ubyte *buf)
{
	return buf[0];
}

INLINE uword
peek_uword(const ubyte *buf)
{
	return ((uword) buf[0] << 8) |
		buf[1];
}

INLINE udword
peek_udword(const ubyte *buf)
{
	return ((uword) buf[0] << 24) |
		((uword) buf[1] << 16) |
		((uword) buf[2] << 8) |
		buf[3];
}

INLINE ubyte
get_ubyte(const ubyte **buf)
{
	ubyte retval;

	retval = peek_ubyte(*buf);
	*buf += SIZEOF_UBYTE;

	return retval;
}

INLINE void
put_ubyte(ubyte **buf, ubyte value)
{
	**buf = value;
	++(*buf);
}

INLINE uword
get_uword(const ubyte **buf)
{
	uword retval;

	retval = peek_uword(*buf);
	*buf += SIZEOF_UWORD;

	return retval;
}

INLINE void
put_uword(ubyte **buf, uword value)
{
	**buf = (value >> 8) & 0xff;
	++(*buf);
	**buf = value & 0xff;
	++(*buf);
}

INLINE udword
get_udword(const ubyte **buf)
{
	udword retval;

	retval = peek_udword(*buf);
	*buf += SIZEOF_UDWORD;

	return retval;
}

INLINE void
put_udword(ubyte **buf, udword value)
{
	**buf = (value >> 24) & 0xff;
	++(*buf);
	**buf = (value >> 16) & 0xff;
	++(*buf);
	**buf = (value >>  8) & 0xff;
	++(*buf);
	**buf = value & 0xff;
	++(*buf);
}
#if TIME
/* XXX - Figure out the timezone hairiness:
 * Palms don't have timezones. Hence, the Palm's epoch is Jan. 1, 1904 in
 * the local timezone.
 * Unless you're syncing across the network, in which case its epoch is
 * Jan. 1, 1904 in the timezone it happens to be in (which may not be the
 * same as the desktop's timezone).
 * Except that there are (I'm sure) tools that add timezones to the Palm.
 * These should be consulted.
 * Times generated locally are in the local timezone (i.e., the one that
 * the desktop machine is in).
 */

/* time_dlp2time_t
 * Convert the DLP time structure into a Unix time_t, and return it.
 */
time_t
time_dlp2time_t(const struct dlp_time *dlpt)
{
	struct tm tm;

	/* Convert the dlp_time into a struct tm, then just use mktime() to
	 * do the conversion.
	 */
	tm.tm_sec = dlpt->second;
	tm.tm_min = dlpt->minute;
	tm.tm_hour = dlpt->hour;
	tm.tm_mday = dlpt->day;
	tm.tm_mon = dlpt->month - 1;
	tm.tm_year = dlpt->year - 1900;
	tm.tm_wday = 0;
	tm.tm_yday = 0;
	tm.tm_isdst = 0;
#if HAVE_TM_ZONE
	tm.tm_gmtoff = 0;
	tm.tm_zone = NULL;
#else
/* XXX - ANSI doesn't allow #warning, and we're not using the timezone for
 * anything yet.
 */
/*  #warning You do not have tm_zone */
#endif

	return mktime(&tm);
}

/* time_dlp2palmtime
 * Convert a DLP time structure into a Palm time_t (number of seconds since
 * Jan. 1. 1904), and return it.
 */
udword
time_dlp2palmtime(const struct dlp_time *dlpt)
{
	time_t now;		/* The time, as a Unix time_t */
	struct tm tm;

	/* Convert the dlp_time into a struct tm, use mktime() to do the
	 * conversion, and add the difference in epochs.
	 */
	tm.tm_sec = dlpt->second;
	tm.tm_min = dlpt->minute;
	tm.tm_hour = dlpt->hour;
	tm.tm_mday = dlpt->day;
	tm.tm_mon = dlpt->month - 1;
	tm.tm_year = dlpt->year - 1900;
	tm.tm_wday = 0;
	tm.tm_yday = 0;
	tm.tm_isdst = 0;
#if HAVE_TM_ZONE
	tm.tm_gmtoff = 0;
	tm.tm_zone = NULL;
#endif

	now = mktime(&tm);
	now += EPOCH_1904;

	return now;
}

/* time_time_t2dlp
 * Convert a Unix time_t into a DLP time structure. Put the result in
 * 'dlpt'.
 */
void
time_time_t2dlp(const time_t t,
		struct dlp_time *dlpt)
{
	struct tm *tm;

	tm = localtime(&t);	/* Break 't' down into components */

	/* Copy the relevant fields over to 'dlpt' */
	dlpt->year = tm->tm_year + 1900;
	dlpt->month = tm->tm_mon + 1;
	dlpt->day = tm->tm_mday;
	dlpt->hour = tm->tm_hour;
	dlpt->minute = tm->tm_min;
	dlpt->second = tm->tm_sec;
}

/* time_palmtime2dlp

 * Convert a Palm time (seconds since the Jan. 1, 1904) to a DLP time
 * structure. Put the result in 'dlpt'.
 */
void
time_palmtime2dlp(const udword palmt,
		  struct dlp_time *dlpt)
{
	struct tm *tm;
	time_t t;

	/* Convert the Palm time to a Unix time_t */
	t = palmt - EPOCH_1904;

	/* Break the Unix time_t into components */
	tm = localtime(&t);

	/* Copy the relevant fields over to 'dlpt' */
	dlpt->year = tm->tm_year + 1900;
	dlpt->month = tm->tm_mon + 1;
	dlpt->day = tm->tm_mday;
	dlpt->hour = tm->tm_hour;
	dlpt->minute = tm->tm_min;
	dlpt->second = tm->tm_sec;
}
#endif

/* debug_dump
 * Dump the contents of an array of ubytes to stderr, for debugging.
 */
void
debug_dump(FILE *outfile, const char *prefix,
	   const ubyte *buf, const udword len)
{
	int lineoff;

	for (lineoff = 0; lineoff < len; lineoff += 16)
	{
		int i;

		fprintf(outfile, "%s ", prefix);
		for (i = 0; i < 16; i++)
		{
			if (lineoff + i < len)
			{
				/* Regular bytes */
				fprintf(outfile, "%02x ", buf[lineoff+i]);
			} else {
				/* Filler at the end of the line */
				fprintf(outfile, "   ");
			}
		}
		fprintf(outfile, "  | ");
		for (i = 0; i < 16; i++)
		{
			if (lineoff + i < len)
			{
				/* Regular bytes */
				if (isprint(buf[lineoff+i]))
					fprintf(outfile, "%c", buf[lineoff+i]);
				else
					fprintf(outfile, ".");
			} else
				break;
		}
		fprintf(outfile, "\n");
	}
}
/* This is for Emacs's benefit:
 * Local Variables: ***
 * fill-column:	75 ***
 * End: ***
 */
