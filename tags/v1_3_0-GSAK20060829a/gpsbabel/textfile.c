/*
    Utilities for reading textfiles.

    Copyright (C) 2006 Olaf Klein 

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

textfile_t *
textfile_init(const FILE *file_in)
{
	textfile_t *res;
	
	res = xcalloc(1, sizeof(*res));
	
	res->file_in = (FILE *)file_in;
	res->buf_pos = res->buf_end = res->buf;
	res->line = xstrdup("");
	
	return res;
}

textfile_t *
textfile_open_read(const char *filename, const char *module)
{
	textfile_t *tf;
	
	tf = textfile_init(xfopen(filename, "rb", module));
	tf->tfclose = 1;
	return tf;
}

int
textfile_getc(textfile_t *tf)
{
	int res;
	
	if (tf->buf_pos == tf->buf_end) {
		int bytes;
		
		tf->buf_pos = tf->buf_end = tf->buf;
		bytes = fread(tf->buf, 1, sizeof(tf->buf), tf->file_in);
		tf->buf_end += bytes;
		
		if (bytes == 0) {
			*tf->buf_pos = 0x1a;
			tf->buf_end++;
			return EOF;
		}
	}
	res = *tf->buf_pos;
	if (res == 0x1a) return EOF;
	else {
		tf->buf_pos++;
		return res;
	}
}

unsigned char
textfile_eof(textfile_t *tf)
{
	int ch = textfile_getc(tf);
	
	if (ch == EOF) return 1;
	else {
		tf->buf_pos--;
		return 0;
	}
}

char *textfile_read(textfile_t *tf)
{
	int len = 0;
	char *res = tf->line;
	
	tf->line_no++;
	
	while (1) {
		int c = textfile_getc(tf);
		
		if ((c == EOF) || (c == 0x1a)) {
			if (len == 0) return NULL;
			else break;
		}
		else if (c == '\r') {
			c = textfile_getc(tf);
			if (c != '\n') tf->buf_pos--;
			break;
		}
		else if (c == '\n') {
			break;
		}
		if (len == tf->line_size) {
			tf->line_size+=128;
			res = tf->line = xrealloc(tf->line, tf->line_size + 1);
		}
		res[len] = c;
		len++;
	}
	res[len] = '\0';
	return res;
}

void
textfile_done(textfile_t *tf)
{
	xfree(tf->line);
	if (tf->tfclose) fclose(tf->file_in);
	xfree(tf);
}
