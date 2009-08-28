/*
    Abstract fixed size data types.

    Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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

#ifndef gb_types_h_included
#define gb_types_h_included


/*
 *  If this is a problem and any interesting system doesn't have the C99-ism
 *  of <stdint.h> we'll come up with something more clever that'll likely
 *  include a gross collection of  __STDC_VERSION >= 199901L  || __GNUC__
 */

#if defined(_MSC_VER)

typedef unsigned long gbuint32;
typedef unsigned short gbuint16;
typedef long gbint32;
typedef short gbint16;

#else

# if defined (__FreeBSD__)
#  include <inttypes.h>
# else
#  include <stdint.h>
# endif

typedef uint32_t	gbuint32;
typedef uint16_t	gbuint16;
typedef  int32_t 	 gbint32;
typedef  int16_t	 gbint16;

#endif /* defined(_MSC_VER) */

typedef gbuint32	gbsize_t;
typedef unsigned char	gbuint8;
typedef signed char	gbint8;

#endif /* gb_types_h_included */
