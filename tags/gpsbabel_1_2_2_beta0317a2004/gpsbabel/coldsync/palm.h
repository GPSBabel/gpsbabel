/* palm.h
 * Definitions of various types that PalmOS likes to use.
 *
 *	Copyright (C) 2001, Andrew Arensburger.
 *	You may distribute this file under the terms of the Artistic
 *	License, as specified in the README file.
 *
 * $Id: palm.h,v 1.1 2002-08-16 06:13:10 robertl Exp $
 */
#ifndef _palm_h_
#define _palm_h_

/* Convenience types */
typedef signed   char  byte;		/* Signed 8-bit quantity */
typedef unsigned char  ubyte;		/* Unsigned 8-bit quantity */
typedef signed   short word;		/* Signed 16-bit quantity */
typedef unsigned short uword;		/* Unsigned 16-bit quantity */
typedef signed   long  dword;		/* Signed 32-bit quantity */
typedef unsigned long  udword;		/* Unsigned 32-bit quantity */

typedef udword chunkID;			/* Those IDs made up of four
					 * characters stuck together into a
					 * 32-bit quantity.
					 */

/* Explicitly define the sizes of types. Can't depend on the host's types
 * having the same size as the Palm. For instance, Alphas are 64-bit
 * machines, so 'unsigned long' is 8 bytes, whereas 'udword' is only 4
 * bytes.
 */
#define SIZEOF_BYTE	1
#define SIZEOF_UBYTE	1
#define SIZEOF_WORD	2
#define SIZEOF_UWORD	2
#define SIZEOF_DWORD	4
#define SIZEOF_UDWORD	4

/* MAKE_CHUNKID
 * A convenience macro to make a chunkID out of four characters.
 */
#define MAKE_CHUNKID(a,b,c,d) \
	(((a) << 24) | \
	 ((b) << 16) | \
	 ((c) << 8)  | \
	 (d))

/* XXX - There ought to be something to make sure that the sizes and
 * signedness above are true.
 */

typedef enum { False = 0, True = 1 } Bool;

#endif	/* _palm_h_ */
