/*
	contents.h

	Extract data from MS Streets & Trips .est, Autoroute .axe 
	and Mapoint .ptm files in GPX format.

    Copyright (C) 2003 James Sherring, james_sherring@yahoo.com

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


	This app depends on istorage & istorage-make from Pabs (pabs3@zip.to)
	and James Clark's Expat xml parser from http://www.libexpat.org/.

*/
#ifdef	__cplusplus
extern "C" {
#endif

#define CONT_RECSZ_C0	108
#define CONT_RECOS_SECT_LEN 98
#define CONT_RECOS_ARRAY_LEN 104
#define CONT_RECSZ_C1	5
#define CONT_RECSZ_C2	6
#define CONT_RECSZ_C3	9


struct contents
{
	int fully_parsed_flag;
	unsigned int buf_len;
	// The data as read from the contents stream.
	char* buf;
	int conts0_os;
	//x struct f_contents0 * f_conts0;
	int array_os;
	//x unsigned short * f_conts_array;
	unsigned char * f_pcbtext0;
	char* f_text0;
	int conts1_os;
	//x struct f_contents1 * f_conts1;
	char* f_text1;
	int conts2_os;
	//x struct f_contents2 * f_conts2;
	// Lengths for conts2->count_strings number of strings.
	// Note that this is array of *pointers* to string-length
	unsigned char ** list_f_pcbtext;
	// conts2->count_strings number of strings.
	// Strings are not null-terminated,
	// but they are prefixed with their length.
	char **	list_f_text;
	// Normally 0, sometimes 1
	unsigned short * pusunkn0;
	// Only when usunkn0=1 ???
	int * piunkn1;
	int conts3_os;
	//x struct f_contents3 * f_conts3;
	char * CountryText;
	// The end part of the buffer that has not been interpreted.
	char* rest;
} ;

struct contents * read_contents(char* conts_file_name);
struct contents * contents_insert_ppinset(struct contents * old_conts, unsigned short newSetId);
void write_contents(struct contents * conts, char* conts_file_name);
void contents_delete(struct contents * conts);

#ifdef	__cplusplus
}
#endif

