/*
    Copyright (C) 2004 Justin Broughton, justinbr@earthlink.net

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

#include "uuid.h"

static unsigned char
random()
{
	if (getenv("GPSBABEL_FREEZE_TIME")) {
		static unsigned char blech = 0;
		return blech++;
	} else
		return ;
}


void
uuid_generate(uuid_t uu)
{
	char *cp;
	int i;
	for (cp = uu, i = 0; i < 16; i++) {
		if (getenv("GPSBABEL_FREEZE_TIME")) {
			static unsigned char blech = 0;
			*cp++ = blech++;
		} else {
			*cp++ ^= (rand() >> 7) & 0xFF;
		}
	}
}

