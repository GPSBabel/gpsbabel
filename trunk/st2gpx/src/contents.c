/*
	contents.c

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
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>

#include "st2gpx.h"
#include "gpx.h"
#include "contents.h"
#include "annotations.h"

#ifdef EXPLORE
#include "explore.h"
#endif

struct contents * contents_new()
{
	struct contents * nw = (struct contents*)xmalloc(sizeof(struct contents));
	nw->fully_parsed_flag=0;
	nw->buf_len=0;
	nw->buf=NULL;
	nw->conts0_os=0;
//x	nw->f_conts0=NULL;
	nw->array_os=0;
//x	nw->f_conts_array=NULL;
	nw->f_pcbtext0=NULL;
	nw->f_text0=NULL;
	nw->conts1_os=0;
//x	nw->f_conts1=NULL;
	nw->f_text1=NULL;
	nw->conts2_os=0;
//x	nw->f_conts2=NULL;
	// conts2->count_strings strings.
	// Strings are not nul-terminated,
	// but they are prefixed with their length.
	nw->list_f_pcbtext=NULL;
	nw->list_f_text=NULL;
	nw->conts3_os=0;
//x	nw->f_conts3=NULL;
	// the end part of the buffer that has not been interpreted.
	nw->rest=NULL;
	return nw;
}

void contents_delete(struct contents * conts)
{
	if(conts != NULL)
	{
		free(conts->list_f_pcbtext);
		free(conts->list_f_text);
		free(conts->buf);
	}
	free(conts);
}

struct contents * parse_contents_buffer(char* buf, unsigned int buf_len)
{
	struct contents * conts = contents_new();
	unsigned int buf_pos=0;
	unsigned int i;
	unsigned int numstrings;

	conts->buf = buf;
	conts->buf_len=buf_len;

	// *****************
	// Parse f_contents0
	// *****************

	//x conts->f_conts0 = (struct f_contents0 *)(conts->buf);
	conts->conts0_os=0;
	//x buf_pos += sizeof(struct f_contents0);
	buf_pos += CONT_RECSZ_C0;

	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts0.\n");
		return conts;
	}
	// **************************
	// Parse conts->f_conts_array
	// **************************

	//conts->f_conts_array=(unsigned short *)(conts->buf + buf_pos);
	conts->array_os=buf_pos;
	//buf_pos += conts->f_conts0->array_len;
	buf_pos += *(unsigned int*)(conts->buf + CONT_RECOS_ARRAY_LEN);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts array.\n");
		return conts;
	}

	// ************************
	// Parse f_cbtext0, f_text0
	// ************************

	conts->f_pcbtext0=(unsigned char*)((conts->buf)+buf_pos);
	buf_pos += sizeof(char);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with text0 size.\n");
		return conts;
	}

	conts->f_text0=(conts->buf)+buf_pos;
	buf_pos += *(conts->f_pcbtext0);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with text0.\n");
		return conts;
	}

	// *****************
	// Parse f_contents1
	// *****************

	//x conts->f_conts1 = (struct f_contents1 *)(conts->buf+buf_pos);
	conts->conts1_os = buf_pos;
	//buf_pos += sizeof(struct f_contents1);
	buf_pos += CONT_RECSZ_C1;
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts1.\n");
		return conts;
	}

	// *************
	// Parse f_text1
	// *************

	conts->f_text1 = (conts->buf)+buf_pos;
	//x buf_pos += conts->f_conts1->cbText1;
	buf_pos += *(unsigned char *)((conts->buf)+buf_pos-1);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with text1.\n");
		return conts;
	}

	// *****************
	// Parse f_contents2
	// *****************

	//x conts->f_conts2 = (struct f_contents2 *)((conts->buf)+buf_pos);
	conts->conts2_os = buf_pos;
	//x buf_pos += sizeof(struct f_contents2);
	buf_pos += CONT_RECSZ_C2;
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts2.\n");
		return conts;
	}

	// ***************************************
	// Parse list_f_cbtext[] and list_f_text[]
	// ***************************************

	numstrings = *(unsigned short*)(conts->buf + conts->conts2_os + 4);
	//x numstrings = conts->f_conts2->count_strings;
	conts->list_f_pcbtext=(unsigned char**)xmalloc(numstrings*sizeof(unsigned char*));
	conts->list_f_text=(char**)xmalloc(numstrings*sizeof(char*));
	// initialise just incase we dont make it through the parse loop
	for(i=0; i<numstrings; i++)
	{
		conts->list_f_pcbtext[i]=NULL;
		conts->list_f_text[i]=NULL;
	}
	for(i=0; i<numstrings; i++)
	{
		// list_f_cbtext[i] is a *pointer* to text len
		conts->list_f_pcbtext[i]=(unsigned char*)(conts->buf+buf_pos);
		buf_pos += sizeof(unsigned char);
		if (buf_pos>(conts->buf_len))
		{
			printf("Oops, ran out of buffer with text[%d] size.\n", i);
			return conts;
		}
		conts->list_f_text[i]=(conts->buf)+buf_pos;
		buf_pos += conts->list_f_pcbtext[i][0];
		if (buf_pos>(conts->buf_len))
		{
			printf("Oops, ran out of buffer with text%d.\n", i);
			return conts;
		}
	}

	// **************************
	// Parse pusunkn0 and piunkn1
	// **************************

    conts->pusunkn0 = (unsigned short*)(conts->buf+buf_pos);

	buf_pos += sizeof(unsigned short);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with pusunkn0.\n");
		return conts;
	}

	if(*(conts->pusunkn0) == 1)
	{
		conts->piunkn1 = (int*)((conts->buf)+buf_pos);
		buf_pos += sizeof(int);
		if (buf_pos>(conts->buf_len))
		{
			printf("Oops, ran out of buffer with piunkn1.\n");
			return conts;
		}
	}

	// *****************
	// Parse f_contents3
	// *****************

	//x conts->f_conts3 = (struct f_contents3 *)(conts->buf+buf_pos);
	conts->conts3_os = buf_pos;
	//x buf_pos += sizeof(struct f_contents3);
	buf_pos += CONT_RECSZ_C3;
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts3.\n");
		return conts;
	}

	// *****************
	// Parse CountryText
	// *****************

	conts->CountryText = (conts->buf)+buf_pos;
	//x buf_pos += conts->f_conts3->cbCountryText;
	buf_pos += *(unsigned char *)((conts->buf)+buf_pos-1);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with country text.\n");
		return conts;
	}

	if (buf_pos == conts->buf_len)
		conts->fully_parsed_flag=1;
	else
	{
		printf("Unexpected %d bytes of contents buffer still remaining.\n",
				(conts->buf_len)-buf_pos);
		if(opts.verbose_flag > 4)
			printbuf((conts->buf)+buf_pos, (conts->buf_len)-buf_pos);
	}

#ifdef EXPLORE
	if (opts.explore_flag)
		print_contents(conts);
#endif

	return conts;
}

struct contents * read_contents(char* conts_file_name)
{
	FILE* conts_file=NULL;
	const unsigned int max_conts_buf=1000;
	int readbyte=0;
	unsigned int buf_pos=0;
	char* buf=(char*)xmalloc(max_conts_buf);
	struct contents * conts=NULL;

	// ****************************************
	// Read the Contents stream into the buffer
	// ****************************************

	if ((conts_file = fopen(conts_file_name, "rb")) == NULL)
	{
		fprintf(stderr, "Cannot open property file %s\n", conts_file_name);
		debug_pause();
		exit(3);
		//return NULL;
	}

	if ((readbyte = getc(conts_file))!=EOF)
	{
		do {
			buf[buf_pos]=(char)readbyte;
			buf_pos++;
			if (buf_pos>max_conts_buf-1)
			{
				printf("oops, I didn't allocate enough buffer space for contents...\n");
				break;
			}
		} while ((readbyte = getc(conts_file))!=EOF);

	}
	fclose(conts_file);

	if (opts.verbose_flag>2)
		printf("Read %d bytes in contents stream %s\n", buf_pos, conts_file_name);
	if (opts.verbose_flag>4)
	{
		printbuf(buf, buf_pos);
		printf("Now trying to enterpret this data...\n");
		debug_pause();
	}

	conts = parse_contents_buffer(buf, buf_pos);

	debug_pause();
	return conts;
}

struct contents * contents_insert_ppinset(struct contents * old_conts, unsigned short newSetId)
{
	// After inserting pushpins into UserData db,
	// we need to update the Contents structure to include the new pushpin set
	// in the list of objects to display.
	// Otherwise the new pushpins will no be visible or editable.
	struct contents * nw_conts=NULL;
	char* nw_buf=NULL;
	unsigned int nw_buf_len=0;
	unsigned int insert_position=0;
	unsigned int arraylen_os=0;

	if (old_conts==NULL)
	{
		printf("Contents buffer is empty???. The outut file will have a corrupted pushpin set.\n");
		return NULL;
	}

	if (old_conts->fully_parsed_flag == 0)
	{
		printf("Warning - modifying a contents stream that is not fully parsed.\n");
		// Actually, we only need to have parsed as far as old_conts->f_conts_array,
		// but best to be safe.
		if(old_conts->CountryText !=NULL)
			printf("...but I seem to have parsed enough to continue.\n");
		else
		{
			printf("I have not parsed enough to continue. The outut file will have a corrupted pushpin set.\n");
			return NULL;
		}
	}

	nw_buf_len = old_conts->buf_len  + 2*sizeof(unsigned char);
	nw_buf = (char *)xmalloc(nw_buf_len);

	// insert after f_conts_array[3], ie at f_conts_array+4
	//x insert_position = ((char*)(old_conts->f_conts_array+4)-(old_conts->buf));
	insert_position = old_conts->array_os + 8;

	memcpy(nw_buf, old_conts->buf, insert_position);
	memcpy(nw_buf + insert_position, &newSetId, sizeof(unsigned short));
	memcpy(nw_buf + insert_position + sizeof(unsigned short),
			old_conts->buf + insert_position,
			old_conts->buf_len - insert_position);

	// set the size of the contents aray
	//x arraylen_os = (char*)&(old_conts->f_conts0->array_len) - old_conts->buf;
	arraylen_os = old_conts->array_os - 4;
	*(nw_buf + arraylen_os) += sizeof(unsigned short);

	// increase the number of pushpin sets in f_conts_array[3]
	*(nw_buf + insert_position - sizeof(unsigned short)) +=1;

	// increase section length
	*(nw_buf + CONT_RECOS_SECT_LEN) +=2;
	// increase iunkn20
	// *(int*)(nw_buf + old_conts->conts1_os + 2) +=1;

	nw_conts=parse_contents_buffer(nw_buf, nw_buf_len);

	return  nw_conts;
}

void write_contents(struct contents * conts, char* conts_file_name)
{
	FILE* conts_out_file;
	int status;

	//printf("Writing new Contents stream to %s\n", conts_file_name);

	if ((conts_out_file = fopen(conts_file_name, "wb")) == NULL) {
           fprintf(stderr, "Cannot open %s\n", conts_file_name);
		   fflush(stdout);
			debug_pause();
           exit(1);
	   }

	status = fwrite(conts->buf, conts->buf_len, 1, conts_out_file);
	if (status != 1)
		printf("expected to write %d, actually wrote %d\n", conts->buf_len, status*(conts->buf_len));
	else
		printf("Wrote %d bytes to new contents stream in %s\n", conts->buf_len, conts_file_name);

	fclose(conts_out_file);
}