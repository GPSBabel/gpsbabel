/*
    Header for our common utilities for XML-based formats.

    Copyright (C) 2004 Robert Lipe, robertlipe@usa.net

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



typedef enum {
	cb_start = 1,
	cb_cdata,
	cb_end,
} xg_cb_type;

typedef void (xg_callback) (const char *, const char **);

typedef struct xg_tag_mapping {
	xg_callback *tag_cb;
	xg_cb_type cb_type;
	const char *tag_name;
} xg_tag_mapping;


void write_xml_entity(FILE *ofd, const char *indent,
                 const char *tag, const char *value);

void write_optional_xml_entity(FILE *ofd, const char *indent,
                 const char *tag, const char *value);
void xml_write_time(FILE *ofd, const time_t timep, char *elname);

void xml_init(const char *fname, xg_tag_mapping *tbl);
void xml_read(void);
void xml_deinit(void);
