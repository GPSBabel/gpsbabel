 /*

    Support for Navigon Mobile Navigator .rte files.

    Copyright (C) 2005 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
    line structure, items delimited by '|'

-|-|17|-|ZIP-Code|City|ZIP-Code2|Street|No.|-|-|longitude|latitude|-|-| + 0D0A

*/

#include "defs.h"
#include "csv_util.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static gbfile *fin, *fout;
static int curr_rte_num, target_rte_num;

#define MYNAME "navigon"

static char *index_opt;

static
arglist_t nmn4_args[] = {
	{"index", &index_opt, "Index of route to write (if more than one in source)", NULL, ARGTYPE_INT, "1", NULL },
	ARG_TERMINATOR
};


/* helpers */

static char *
nmn4_concat(char *arg0, ...)
{
	va_list args;
	char *src, *res;
	
	res = NULL;
	va_start(args, arg0);
	src = (char *)arg0;

	while (src != NULL)
	{
		char *c = lrtrim(src);
		
	    	if (*c != '\0')
	    	{
			if (res == NULL)
				res = xstrdup(c);
			else
			{
				res = xstrappend(res, " ");
				res = xstrappend(res, c);
			}
	    	}
	    	xfree(src);
	    	src = va_arg(args, char *);
	}
	va_end(args);
	
	return res;
}

static void
nmn4_check_line(char *line)
{
	char *c = line;
	int i = 0;
	while ((c = strchr(c, '|')))
	{
		c++;
		i++;
	}
	is_fatal((i != 15),
		MYNAME ": Invalid or unknown structure!");
}

static void
nmn4_read_data(void)
{
	char *buff;
	char *str, *c;
	int column;

	char *zip1, *zip2, *city, *street, *number;	
	route_head *route;
	waypoint *wpt;
	
	route = route_head_alloc();
	route_add_head(route);
	
	while ((buff = gbfgetstr(fin)))
	{
		str = buff = lrtrim(buff);
		if (*buff == '\0') continue;
		
		nmn4_check_line(buff);

		/* for a quiet compiler */
		zip1 = zip2 = city = street = number = NULL;
	    
	    	wpt = waypt_new();
	    
	    	column = -1;
	    	c = csv_lineparse(str, "|", "", column++);
	    	while (c != NULL)
	    	{
			switch(column)
			{
		    		case  0: /* "-" */	/* unknown fields for the moment */
		    		case  1: /* "-" */
		    		case  2: /* "-" */
		    		case  3: /* "-" */
		    		case  9: /* "-" */
		    		case 10: /* "-" */
		    		case 13: /* "-" */
		    		case 14: /* "-" */
				case 15: /* "" */
					break;
		    
		    		case  4: 				/* ZIP Code */
					if (*c != '-') 
						zip1 = xstrdup(c);
					else
						zip1 = xstrdup("");
					break;
			
		    		case  5: 				/* City */
					if (*c != '-')
						city = xstrdup(c); 
					else
						city = xstrdup("");
					break;
			
		    		case  6: 				/* ZIP Code -2- */
					if (*c != '-') 
						zip2 = xstrdup(c); 
					else
						zip2 = xstrdup("");	
					break;
					
				case  7: 				/* Street */
					if (*c != '-')
						street = xstrdup(c); 
					else
						street = xstrdup("");
					break;
					
				case  8: 				/* Number */
					if (*c != '-')
						number = xstrdup(c); 
					else
						number = xstrdup("");

				/* 
			    	   This is our final index
				   All stuff for generating names or comments
				   is hold locally.
				   
				   We don't have fields for street, city or zip-code.
				   Instead we construct a description from that.
				*/
			
					if (strcmp(zip1, zip2) == 0) *zip2 = '\0';
					if (*city != '\0')
					{
			    			/* 
						   if any field following city has a value, add a comma to city 
						*/
			    			if ((*street != '\0') || (*number != '\0') || (*zip2 != '\0'))
							city = xstrappend(city, ",");
					}
										
					/* concats all fields to one string and release */
					wpt->description = nmn4_concat(zip1, city, street, number, zip2, NULL);
					break;
			
		    		case 11: 				/* longitude */
					sscanf(c, "%lf", &wpt->longitude);
					break;
			
				case 12: 				/* latitude */
					sscanf(c, "%lf", &wpt->latitude);
					break;
			
			}
			c = csv_lineparse(NULL, "|", "", column++);
		}
		route_add_wpt(route, wpt);
	}
}

static void 
nmn4_route_hdr(const route_head *route)
{
	curr_rte_num++;
}

static void 
nmn4_route_tlr(const route_head *rte)
{
}

static void
nmn4_write_waypt(const waypoint *wpt)
{
	char city[128], street[128], zipc[32], number[32];
	
	if (curr_rte_num != target_rte_num) return;

	strncpy(city, "-", sizeof(city));
	strncpy(street, "-", sizeof(street));
	strncpy(zipc, "-", sizeof(zipc));
	strncpy(number, "-", sizeof(number));
	
	/* 
	   Population of specific data used by Navigon may come in the
	   future or it may be impossible. We currently output only the 
	   coordinates in the output, but this is sufficient for Navigon.
	   
	   The coordinates are the only item we are about guaranteed to have 
	   when converting to any from any format, so we leave the other
	   fields unpopulated.  So i have to pay Navigon a compliment for 
	   implementing a simple data exchange.
	 */

	gbfprintf(fout, "-|-|-|-|%s|%s|%s|%s|%s|-|-|%.5f|%.5f|-|-|\r\n",
		zipc, city, zipc, street, number,
		wpt->longitude, wpt->latitude);
}

static void
nmn4_write_data(void)
{
	
	target_rte_num = 1;
	
	if (index_opt != NULL)
	{
		target_rte_num = atoi(index_opt);
		is_fatal(((target_rte_num > (int) route_count()) || (target_rte_num < 1)),
			MYNAME ": invalid route number %d (1..%d))!\n", target_rte_num, route_count());
	}
	
	curr_rte_num = 0;
	route_disp_all(nmn4_route_hdr, nmn4_route_tlr, nmn4_write_waypt);
}


/* %%% global callbacks %%% */

static void
nmn4_rd_init(const char *fname)
{
	fin = gbfopen(fname, "rb", MYNAME);
	if (gbfunicode(fin)) cet_convert_init(CET_CHARSET_UTF8, 1);
}

static void
nmn4_rd_deinit(void)
{
	gbfclose(fin);
}

static void
nmn4_read(void)
{
	nmn4_read_data();
}

static void
nmn4_wr_init(const char *fname)
{
	fout = gbfopen(fname, "wb", MYNAME);
}

static void
nmn4_wr_deinit(void)
{
	gbfclose(fout);
}

static void
nmn4_write(void)
{
	nmn4_write_data();
}

/* --------------------------------------------------------------------------- */

ff_vecs_t nmn4_vecs = {
	ff_type_file,
	{ ff_cap_none, ff_cap_none, ff_cap_read | ff_cap_write},
	nmn4_rd_init,
	nmn4_wr_init,
	nmn4_rd_deinit,
	nmn4_wr_deinit,
	nmn4_read,
	nmn4_write,
	NULL,
	nmn4_args,
	CET_CHARSET_MS_ANSI, 1	/* CET-REVIEW */
};
