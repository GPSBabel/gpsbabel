/*
 * $Id: pi-macros.h,v 1.1 2007-07-21 03:54:52 robertl Exp $
 *
 * pi-macros.h: pilot-link specific macro defintions
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
 
#ifndef _PILOT_MACROS_H_
#define _PILOT_MACROS_H_

#include <time.h>
#include "pi-args.h"

typedef unsigned long recordid_t;

#ifdef __cplusplus
extern "C" {
#endif

	extern double get_float PI_ARGS((void *));
	extern void set_float PI_ARGS((void *, double));
	extern int compareTm PI_ARGS((struct tm * a, struct tm * b));

#ifdef __cplusplus
}
#endif
#ifndef __cplusplus
#define get_long(ptr) \
        (unsigned long) ( \
        (((unsigned long)((unsigned char *)(ptr))[0]) << 24) | \
        (((unsigned long)((unsigned char *)(ptr))[1]) << 16) | \
        (((unsigned long)((unsigned char *)(ptr))[2]) << 8) | \
        (((unsigned long)((unsigned char *)(ptr))[3]) ) )


#define get_treble(ptr) ((unsigned long)\
                        ((((unsigned char*)(ptr))[0] << 16) | \
                         (((unsigned char*)(ptr))[1] << 8)  | \
                         (((unsigned char*)(ptr))[2])))
#define get_short(ptr) ((unsigned short)\
                       ((((unsigned char*)(ptr))[0] << 8)  | \
                        (((unsigned char*)(ptr))[1])))
#define get_byte(ptr) (((unsigned char*)(ptr))[0])
#define get_slong(ptr) (signed long)(\
				(get_long((ptr)) > 0x7FFFFFFF) ?\
                                (((signed long)(get_long((ptr)) & 0x7FFFFFFF)) - 0x80000000):\
                                ((signed long)(get_long((ptr))))\
                                )
#define get_streble(ptr) (signed long)(\
				(get_treble((ptr)) > 0x7FFFFF) ?\
                                (((signed long)(get_treble((ptr)) & 0x7FFFFF)) - 0x800000):\
                                ((signed long)(get_treble((ptr))))\
                                )
#define get_sshort(ptr) (signed short)(\
				(get_short((ptr)) > 0x7FFF) ?\
                                (((signed short)(get_short((ptr)) & 0x7FFF)) - 0x8000):\
                                ((signed short)(get_short((ptr))))\
                                )
#define get_sbyte(ptr) (signed char)(\
				(get_byte((ptr)) > 0x7F) ?\
                                (((signed char)(get_byte((ptr)) & 0x7F)) - 0x80):\
                                ((signed char)(get_byte((ptr))))\
                                )
#define set_long(ptr,val) ((((unsigned char*)(ptr))[0] = (unsigned char)(((unsigned long)(val)) >> 24) & 0xff), \
		          (((unsigned char*)(ptr))[1] = (((unsigned long)(val)) >> 16) & 0xff), \
		          (((unsigned char*)(ptr))[2] = (((unsigned long)(val)) >> 8) & 0xff), \
		          (((unsigned char*)(ptr))[3] = (((unsigned long)(val)) >> 0) & 0xff))
#define set_slong(ptr,val) set_long((ptr),((unsigned long)(\
                            (((signed long)(val)) < 0) ?\
                            ((unsigned long)(((signed long)(val)) + 0x80000000) | 0x80000000) :\
                            (val)\
                           )))
#define set_treble(ptr,val) ((((unsigned char*)(ptr))[0] = (unsigned char)(((unsigned long)(val)) >> 16) & 0xff), \
		             (((unsigned char*)(ptr))[1] = (((unsigned long)(val)) >> 8) & 0xff), \
		             (((unsigned char*)(ptr))[2] = (((unsigned long)(val)) >> 0) & 0xff))
#define set_streble(ptr,val) set_treble((ptr),((unsigned long)(\
                            (((signed long)(val)) < 0) ?\
                            ((unsigned long)(((signed long)(val)) + 0x800000) | 0x800000) :\
                            (val)\
                           )))
#define set_short(ptr,val) ((((unsigned char*)(ptr))[0] = (((unsigned short)(val)) >> 8) & 0xff), \
		            (((unsigned char*)(ptr))[1] = (((unsigned short)(val)) >> 0) & 0xff))
#define set_sshort(ptr,val) set_short((ptr),((unsigned short)(\
                            (((signed short)(val)) < 0) ?\
                            ((unsigned short)(((signed short)(val)) + 0x8000) | 0x8000) :\
                            (val)\
                           )))
#define set_byte(ptr,val) (((unsigned char*)(ptr))[0]=(val))
#define set_sbyte(ptr,val) set_byte((ptr),((unsigned char)(\
                            (((signed char)(val)) < 0) ?\
                            ((unsigned char)(((signed char)(val)) + 0x80) | 0x80) :\
                            (val)\
                           )))
#define char4(c1,c2,c3,c4) (((c1)<<24)|((c2)<<16)|((c3)<<8)|(c4))
#else				/*ifdef __cplusplus */
inline unsigned long get_long(const void *buf)
{
	unsigned char *ptr = (unsigned char *) buf;

	return (*ptr << 24) | (*(++ptr) << 16) | (*(++ptr) << 8) |
	    *(++ptr);
}

inline signed long get_slong(const void *buf)
{
	unsigned long val = get_long(buf);

	if (val > 0x7FFFFFFF)
		return ((signed long) (val & 0x7FFFFFFF)) - 0x80000000;
	else
		return val;
}

inline unsigned long get_treble(const void *buf)
{
	unsigned char *ptr = (unsigned char *) buf;

	return (*ptr << 16) | (*(++ptr) << 8) | *(++ptr);
}

inline signed long get_streble(const void *buf)
{
	unsigned long val = get_treble(buf);

	if (val > 0x7FFFFF)
		return ((signed long) (val & 0x7FFFFF)) - 0x800000;
	else
		return val;
}

inline int get_short(const void *buf)
{
	unsigned char *ptr = (unsigned char *) buf;

	return (*ptr << 8) | *(++ptr);
}

inline signed short get_sshort(const void *buf)
{
	unsigned short val = get_short(buf);

	if (val > 0x7FFF)
		return ((signed short) (val & 0x7FFF)) - 0x8000;
	else
		return val;
}

inline int get_byte(const void *buf)
{
	return *((unsigned char *) buf);
}

inline signed char get_sbyte(const void *buf)
{
	unsigned char val = get_byte(buf);

	if (val > 0x7F)
		return ((signed char) (val & 0x7F)) - 0x80;
	else
		return val;
}

inline void set_long(void *buf, const unsigned long val)
{
	unsigned char *ptr = (unsigned char *) buf;

	*ptr = (unsigned char) ((val >> 24) & 0xff);
	*(++ptr) = (unsigned char) ((val >> 16) & 0xff);
	*(++ptr) = (unsigned char) ((val >> 8) & 0xff);
	*(++ptr) = (unsigned char) (val & 0xff);
}

inline void set_slong(void *buf, const signed long val)
{
	unsigned long uval;

	if (val < 0) {
		uval = (val + 0x80000000);
		uval |= 0x80000000;
	} else
		uval = val;
	set_long(buf, uval);
}

inline void set_treble(void *buf, const unsigned long val)
{
	unsigned char *ptr = (unsigned char *) buf;

	*ptr = (unsigned char) ((val >> 16) & 0xff);
	*(++ptr) = (unsigned char) ((val >> 8) & 0xff);
	*(++ptr) = (unsigned char) (val & 0xff);
}

inline void set_streble(void *buf, const signed long val)
{
	unsigned long uval;

	if (val < 0) {
		uval = (val + 0x800000);
		uval |= 0x800000;
	} else
		uval = val;
	set_treble(buf, uval);
}

inline void set_short(void *buf, const int val)
{
	unsigned char *ptr = (unsigned char *) buf;

	*ptr = (val >> 8) & 0xff;
	*(++ptr) = val & 0xff;
}

inline void set_sshort(void *buf, const signed short val)
{
	unsigned short uval;

	if (val < 0) {
		uval = (val + 0x8000);
		uval |= 0x8000;
	} else
		uval = val;
	set_treble(buf, uval);
}

inline void set_byte(void *buf, const int val)
{
	*((unsigned char *) buf) = val;
}

inline void set_sbyte(void *buf, const signed char val)
{
	unsigned char uval;

	if (val < 0) {
		uval = (val + 0x80);
		uval |= 0x80;
	} else
		uval = val;
	set_byte(buf, uval);
}

inline struct tm *getBufTm(struct tm *t, const void *buf, int setTime)
{
	unsigned short int d = get_short(buf);

	t->tm_year = (d >> 9) + 4;
	t->tm_mon = ((d >> 5) & 15) - 1;
	t->tm_mday = d & 31;

	if (setTime) {
		t->tm_hour = 0;
		t->tm_min = 0;
		t->tm_sec = 0;
	}

	t->tm_isdst = -1;

	mktime(t);

	return t;
}

inline void setBufTm(void *buf, const struct tm *t)
{
	set_short(buf,
		  ((t->tm_year - 4) << 9) | ((t->tm_mon +
					      1) << 5) | t->tm_mday);
}

inline unsigned long char4(char c1, char c2, char c3, char c4)
{
	return (c1 << 24) | (c2 << 16) | (c3 << 8) | c4;
}

#endif				/*__cplusplus*/
#endif				/* _PILOT_MACROS_H_ */
