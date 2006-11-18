/* util.h
 * Misc. useful stuff.
 *
 *	Copyright (C) 1999-2000, Andrew Arensburger.
 *	You may distribute this file under the terms of the Artistic
 *	License, as specified in the README file.
 *
 * $Id: util.h,v 1.2 2002-08-24 03:01:31 robertl Exp $
 */
#ifndef _util_h_
#define _util_h_

#include <stdio.h>
#include <time.h>
#include "palm.h"

/* XXX - The functions declared INLINE, below, really ought to be inline
 * functions. I'm not sure how to do this portably, though.
 */
#ifdef __GNUC__
#  define INLINE __inline__
#else
#  define INLINE
#endif	/* __GNUC__ */

/* Functions for reading a value from an array of ubytes */
extern INLINE ubyte peek_ubyte(const ubyte *buf);
extern INLINE uword peek_uword(const ubyte *buf);
extern INLINE udword peek_udword(const ubyte *buf);

/* Functions for extracting values from an array of ubytes */
extern INLINE ubyte get_ubyte(const ubyte **buf);
extern INLINE uword get_uword(const ubyte **buf);
extern INLINE udword get_udword(const ubyte **buf);

/* Functions for writing values to an array of ubytes */
extern INLINE void put_ubyte(ubyte **buf, const ubyte value);
extern INLINE void put_uword(ubyte **buf, const uword value);
extern INLINE void put_udword(ubyte **buf, const udword value);

#if TIME
/* Functions for converting between DLP's time format and Unix's
 * time_ts and the time_t-with-offset that the rest of the Palm stuff
 * uses.
 */
extern time_t time_dlp2time_t(const struct dlp_time *dlpt);
extern udword time_dlp2palmtime(const struct dlp_time *dlpt);
extern void time_time_t2dlp(const time_t t, struct dlp_time *dlpt);
extern void time_palmtime2dlp(const udword palmt, struct dlp_time *dlpt);

extern void debug_dump(FILE *outfile, const char *prefix,
		       const ubyte *buf, const udword len);
#endif
#endif	/* _util_h_ */

/* This is for Emacs's benefit:
 * Local Variables: ***
 * fill-column:	75 ***
 * End: ***
 */
