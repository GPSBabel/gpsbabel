/*
	contents.c

	Extract data from MS Streets & Trips .est and Autoroute .axe files in GPX format.

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

char* buf2str(char* buf, int strlen)
// make a null-terminated string from a buf
{
	char*str=NULL;
	if (strlen==0)
		return NULL;
	str=(char*)xmalloc(strlen+1);
	memcpy(str, buf, strlen);
	str[strlen]=0;
	return str;
}

struct contents * contents_new()
{
	struct contents * nw = (struct contents*)xmalloc(sizeof(struct contents));
	nw->fully_parsed_flag=0;
	nw->buf_len=0;
	nw->buf=NULL;
	nw->f_conts0=NULL;
	nw->f_conts_array=NULL;
	nw->f_pcbtext0=NULL;
	nw->f_text0=NULL;
	nw->f_conts1=NULL;
	nw->f_text1=NULL;
	nw->f_conts2=NULL;
	// conts2->count_strings strings.
	// Strings are not nul-terminated,
	// but they are prefixed with their length.
	nw->list_f_pcbtext=NULL;
	nw->list_f_text=NULL;
	nw->f_conts3=NULL;
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

void print_f_contents0(struct f_contents0 * conts)
{
	struct gpxpt* map_cent=NULL;

	printf("struct f_contents1:\n");
	printf("usunkn0=%d\n", conts->usunkn0);
	printf("map_center_X=%f\n", conts->map_center_X);
	printf("map_center_Y=%f\n", conts->map_center_Y);
	printf("map_center_Z=%f\n", conts->map_center_Z);
	printf("map_scale=%f\n", conts->map_scale);
	printf("iunkn0=%d\n", conts->iunkn0);
	printf("iunkn1=%d\n", conts->iunkn1);
	printf("iunkn2=%d\n", conts->iunkn2);
	printf("iunkn3=%d\n", conts->iunkn3);
	printf("iunkn4=%d\n", conts->iunkn4);
	printf("iunkn5=%d\n", conts->iunkn5);
	printf("iunkn6=%d\n", conts->iunkn6);
	printf("legend_or_directions=%d\n", conts->legend_or_directions);
	printf("iunkn7=%d\n", conts->iunkn7);
	printf("iunkn8=%d\n", conts->iunkn8);
	printf("iunkn9=%d\n", conts->iunkn9);
	printf("usunkn2=%d\n", conts->usunkn2);
	printf("iunkn10=%d\n", conts->iunkn10);
	printf("iunkn11=%d\n", conts->iunkn11);
	debug_pause();
	printf("iunkn12=%d\n", conts->iunkn12);
	printf("iunkn13=%d\n", conts->iunkn13);
	printf("map_format_pannels=%d\n", conts->map_format_pannels);
	printf("iunkn14=%d\n", conts->iunkn14);
	printf("map_format_style=%d\n", conts->map_format_style);
	printf("iunkn16=%d\n", conts->iunkn16);
	printf("map_format_font_size=%d\n", conts->map_format_font_size);

	printf("usunkn4=%d\n", conts->usunkn4);
	printf("section_len=%d\n", conts->section_len);
	printf("usunkn5=%d\n", conts->usunkn5);
	printf("array_len=%d\n", conts->array_len);

	map_cent = gpx_get_point((char*)(&conts->map_center_X));
	printf("Got map center %f %f with scale %f (km?).\n",
		map_cent->lat, map_cent->lon, conts->map_scale);
	gpxpt_delete(map_cent);

	printf("\n");
	printf("\n");
	debug_pause();
}
/* contents_array
	printf("usunkn6=%d\n", conts->usunkn6);
	printf("usunkn7=%d\n", conts->usunkn7);
	printf("usunkn8=%d\n", conts->usunkn8);
	printf("usunkn9=%d\n", conts->usunkn9);
	printf("LastSetId=%d\n", conts->LastSetId);
	printf("iunkn18=%d\n", conts->iunkn18);
	printf("usunkn10=%d\n", conts->usunkn10);
	printf("iunkn19=%d\n", conts->iunkn19);
	printf("usunkn11=%d\n", conts->usunkn11);
	printf("cbText0=%d\n", conts->cbText0);
*/

void print_f_contents1(struct f_contents1 * conts)
{
	printf("struct f_contents1:\n");

	printf("iunkn20=%d\n", conts->iunkn20);
	printf("cbText1=%d\n", conts->cbText1);

	printf("\n");
	debug_pause();
}

void print_f_contents2(struct f_contents2 * conts)
{
	printf("struct f_contents1:\n");

	printf("iunkn21=%d\n", conts->iunkn21);
	printf("count_strings=%d\n", conts->count_strings);

	printf("\n");
	debug_pause();
}

void print_f_contents3(struct f_contents3 * conts)
{
	printf("struct f_contents3:\n");

	printf("usunkn15=%d\n", conts->usunkn15);
	printf("usunkn16=%d\n", conts->usunkn16);
	printf("usunkn17=%d\n", conts->usunkn17);
	printf("usunkn18=%d\n", conts->usunkn18);
	printf("cbCountryText=%d\n", conts->cbCountryText);

	printf("\n");
	debug_pause();
}

struct contents * parse_contents_buffer(char* buf, unsigned int buf_len)
{
	struct contents * conts = contents_new();
	unsigned int buf_pos=0;
	char* temp_str;
	unsigned int i;

	conts->buf = buf;
	conts->buf_len=buf_len;

	// *****************
	// Parse f_contents0
	// *****************

	conts->f_conts0 = (struct f_contents0 *)(conts->buf);
	buf_pos += sizeof(struct f_contents0);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts0.\n");
		return conts;
	}
	if(opts.explore_flag)
		print_f_contents0(conts->f_conts0);

	// **************************
	// Parse conts->f_conts_array
	// **************************

	conts->f_conts_array=(unsigned short *)(conts->buf + buf_pos);
	buf_pos += conts->f_conts0->array_len;
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts array.\n");
		return conts;
	}
	if(opts.explore_flag)
	{
		printf("Dumping Contents array:\n");
		printbuf((char*)(conts->f_conts_array), conts->f_conts0->array_len);

		for(i=0; i< 3; i++)
			printf("Array[%d]=%d - unknown meaning\n", i, conts->f_conts_array[i]);

		printf("n:=Array[3]=%d is number of user pushpin sets\n", (conts->f_conts_array)[3]);

//		for(i=0; i< (conts->f_conts_array[3]); i++)
//			printf("Array[%d]=%d is SetId for user PushpinSet[%d] \n",
//					4+i,
//					conts->f_conts_array[4+i],
//					conts->f_conts_array[3] - i -1);

		for(i=0; i< (conts->f_conts_array[3]); i++)
			printf("Array[4+n-(%d)]=%d is SetId for user PushpinSet[%d]\n",
					i, conts->f_conts_array[3+(conts->f_conts_array[3])-i], i);

		for(i=0; (4 + (conts->f_conts_array[3] +i)) < (conts->f_conts0->array_len)/2 ; i++)
			printf("Array[5+n+(%d)]=%d - unknown meaning\n",
					i, conts->f_conts_array[4+(conts->f_conts_array[3])+i]);

		debug_pause();

//		printf("Dumping Contents array tail:\n");
//		printbuf((char*)(conts->f_conts_array + 4 + conts->f_conts_array[3]),
//				 (conts->f_conts0->array_len)-2*(4 + conts->f_conts_array[3])  );
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
	// conts->f_cbtext0 is a *pointer* to text len
	if(opts.explore_flag)
	{
		temp_str=buf2str(conts->f_text0, *(conts->f_pcbtext0));
		printf("Got Text0='%s'\n", temp_str);
		free(temp_str);
		temp_str=NULL;
	}

	// *****************
	// Parse f_contents1
	// *****************

	conts->f_conts1 = (struct f_contents1 *)(conts->buf+buf_pos);
	buf_pos += sizeof(struct f_contents1);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts1.\n");
		return conts;
	}
	if(opts.explore_flag)
		print_f_contents1(conts->f_conts1);

	// *************
	// Parse f_text1
	// *************

	conts->f_text1 = (conts->buf)+buf_pos;
	buf_pos += conts->f_conts1->cbText1;
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with text1.\n");
		return conts;
	}
	if(opts.explore_flag)
	{
		temp_str=buf2str(conts->f_text1, conts->f_conts1->cbText1);
		printf("Got Text1='%s'\n", temp_str);
		free(temp_str);
		temp_str=NULL;
	}

	// *****************
	// Parse f_contents2
	// *****************

	conts->f_conts2 = (struct f_contents2 *)((conts->buf)+buf_pos);
	buf_pos += sizeof(struct f_contents2);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts2.\n");
		return conts;
	}
	if(opts.explore_flag)
		print_f_contents2(conts->f_conts2);

	// ***************************************
	// Parse list_f_cbtext[] and list_f_text[]
	// ***************************************

	conts->list_f_pcbtext=(unsigned char**)xmalloc((conts->f_conts2->count_strings)*sizeof(unsigned char*));
	conts->list_f_text=(char**)xmalloc((conts->f_conts2->count_strings)*sizeof(char*));
	// initialise just incase we dont make it through the parse loop
	for(i=0; i<(conts->f_conts2->count_strings); i++)
	{
		conts->list_f_pcbtext[i]=NULL;
		conts->list_f_text[i]=NULL;
	}
	for(i=0; i<(conts->f_conts2->count_strings); i++)
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
		if(opts.explore_flag)
		{
			temp_str = buf2str(conts->list_f_text[i], *(conts->list_f_pcbtext[i]));
			printf("Got list_text[%d]='%s'\n", i, temp_str);
			free(temp_str);
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
	if(opts.explore_flag)
		printf("usunkn0=%d\n", *(conts->pusunkn0) );

	if(*(conts->pusunkn0) == 1)
	{
		conts->piunkn1 = (int*)((conts->buf)+buf_pos);
		buf_pos += sizeof(int);
		if (buf_pos>(conts->buf_len))
		{
			printf("Oops, ran out of buffer with piunkn1.\n");
			return conts;
		}
		if(opts.explore_flag)
			printf("iunkn1=%d\n", *(conts->piunkn1) );
	}

	// *****************
	// Parse f_contents2
	// *****************

	conts->f_conts3 = (struct f_contents3 *)(conts->buf+buf_pos);
	buf_pos += sizeof(struct f_contents3);
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with conts3.\n");
		return conts;
	}
	if(opts.explore_flag)
		print_f_contents3(conts->f_conts3);

	// *****************
	// Parse CountryText
	// *****************

	conts->CountryText = (conts->buf)+buf_pos;
	buf_pos += conts->f_conts3->cbCountryText;
	if (buf_pos>(conts->buf_len))
	{
		printf("Oops, ran out of buffer with text[%d].\n", conts->f_conts2->count_strings);
		return conts;
	}
	if(opts.explore_flag)
	{
		temp_str=buf2str(conts->CountryText, conts->f_conts3->cbCountryText);
		if(opts.explore_flag)
		printf("Got CountryText='%s'\n", temp_str);
		free(temp_str);
		temp_str=NULL;
	}

	if (buf_pos == conts->buf_len)
		conts->fully_parsed_flag=1;
	else
	{
		printf("Unexpected %d bytes of contents buffer still remaining.\n",
				(conts->buf_len)-buf_pos);
		printbuf((conts->buf)+buf_pos, (conts->buf_len)-buf_pos);
	}

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
		return NULL;
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
	insert_position = ((char*)(old_conts->f_conts_array+4)-(old_conts->buf));

	memcpy(nw_buf, old_conts->buf, insert_position);
	memcpy(nw_buf + insert_position, &newSetId, sizeof(unsigned short));
	memcpy(nw_buf + insert_position + sizeof(unsigned short),
			old_conts->buf + insert_position,
			old_conts->buf_len - insert_position);

	// set the size of the contents aray
	arraylen_os = (char*)&(old_conts->f_conts0->array_len) - old_conts->buf;
	*(nw_buf + arraylen_os) += sizeof(unsigned short);

	// increase the number of pushpin sets in f_conts_array[3]
	*(nw_buf + insert_position - sizeof(unsigned short)) +=1;

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