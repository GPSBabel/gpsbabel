/*
    Read DeLorme drawing files (.an1)
 
    Copyright (C) 2005 Ron Parker and Robert Lipe.

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

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define MYNAME "an1"
#include "defs.h"

FILE *infile;
FILE *outfile;

static char *output_type = NULL;
long output_type_num = 0;

static long serial=10000;
static long rtserial=1;

static
arglist_t an1_args[] = {
	{"type", &output_type, "Type of .an1 file (0=drawing)", 
		"0", ARGTYPE_HIDDEN | ARGTYPE_INT },
	{0, 0, 0, 0 }
};

typedef struct guid {
	unsigned long l;
	unsigned short s[3];
	unsigned char c[6];
} GUID;

#include "an1sym.h"

static unsigned short
ReadShort(FILE * f)
{
	gbuint16 result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read16(&result);
}

static void 
WriteShort(FILE * f, unsigned short s)
{
	gbuint16 tmp = 0;
        le_write16( &tmp, s );
	fwrite( &tmp, sizeof(tmp), 1, f );
}

static unsigned long
ReadLong(FILE * f)
{
	gbuint32 result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read32(&result);
}

static void 
WriteLong(FILE * f, unsigned long l)
{
	gbuint32 tmp = 0;
        le_write32( &tmp, l );
	
	fwrite( &tmp, sizeof(tmp), 1, f );
}

static double
ReadDouble( FILE * f )
{
	double tmp = 0;
	double result = 0;
	fread(&tmp, sizeof(tmp),1,f);
	le_read64(&result, &tmp );
	return result;
}

static void 
WriteDouble(FILE * f, double d)
{
	double tmp = 0;
        le_read64( &tmp, (void *)&d );
	fwrite( &tmp, sizeof(tmp), 1, f );
}

static char *
ReadString( FILE * f, short len ) 
{
	char *result = NULL;
	result = (char *)xcalloc( 1, len + 1 );
	if ( len ) {
		fread( result, 1, len, f );
	}
	return result;
}

static unsigned char
ReadChar( FILE *f )
{
	unsigned char result = 0;
	fread( &result, 1, 1, f );
	return result;
}

static void
WriteChar( FILE *f, unsigned char c ) 
{
	fwrite( &c, 1, 1, f );
}

static void
WriteString( FILE *f, char *s )
{
	fwrite( s, 1, strlen(s), f );
}

static void 
ReadGuid( FILE *f, GUID *guid )
{
	int i = 0;
	guid->l = ReadLong( f );
	for ( i = 0; i < 3; i++ ) {
		guid->s[i] = ReadShort( f );
	}
	for ( i = 0; i < 6; i++ ) {
		guid->c[i] = ReadChar( f );
	}
}

static void 
WriteGuid( FILE *f, GUID *guid )
{
	int i = 0;
	WriteLong( f, guid->l );
	for ( i = 0; i < 3; i++ ) {
		WriteShort( f, guid->s[i] );
	}
	for ( i = 0; i < 6; i++ ) {
		WriteChar( f, guid->c[i] );
	}
}	

static void
Skip(FILE * f,
     unsigned long distance)
{
	fseek(f, distance, SEEK_CUR);
}

static double
DecodeOrd( long ord )
{
	return (double)(0x80000000-ord)/(0x800000);
}

static long
EncodeOrd( double ord )
{
	unsigned long tmp = ord * 0x800000;
	return 0x80000000UL-tmp;
}

static int
IsGuidEqual( GUID *a, GUID *b ) 
{
	return !memcmp( a, b, sizeof( GUID ));
}

typedef struct {
	short hotspotxhi;
	long hotspoty;
	long unk1;
	GUID guid;
	char *name;
} an1_symbol_record;

typedef struct {
	format_specific_data fs;
	short magic;
	long unk1;
	long lon;
	long lat;
	short type;
	long height;
	long width;
	short unk2;
	short unk3;
	short serial;
	short unk4;
	unsigned char create_zoom;
	unsigned char visible_zoom;
	short unk5;
	double radius;
	char *name;
	char *fontname;
	GUID guid;
	long fontcolor;
	long fontstyle;
	long fontsize;
	long outlineweight;
	long outlinecolor;
	long outlineflags;
	long fillcolor;
	long unk6;
	long fillflags;
} an1_waypoint_record;

typedef struct {
	format_specific_data fs;
	short magic;
	long unk0;
	long lon;
	long lat;
	short unk1;
} an1_vertex_record;

typedef struct {
	format_specific_data fs;
	short magic;
	short unk1;
	short serial;
	long unk2;
	short unk3;
	short type;
	long unk4;
	char *name;
	long lineweight;
	long linestyle;
	long linecolor;
	long unk5;
	long polyfillcolor;
	long unk6;
	long unk7;
	short unk8;
	long pointcount;
} an1_line_record;

static an1_waypoint_record *Alloc_AN1_Waypoint( );

void Destroy_AN1_Waypoint( void *vwpt ) {
	an1_waypoint_record *wpt = (an1_waypoint_record *)vwpt;
	xfree( wpt->name );
	xfree( wpt->fontname );
}

void Copy_AN1_Waypoint( void **vdwpt, void *vwpt ) {
	an1_waypoint_record *wpt = (an1_waypoint_record *)vwpt;
	an1_waypoint_record *dwpt = Alloc_AN1_Waypoint();
	memcpy( dwpt, wpt, sizeof( an1_waypoint_record ));
	dwpt->name = xstrdup( wpt->name );
	dwpt->fontname = xstrdup( wpt->fontname );
	*vdwpt = (void *)dwpt;
}

static an1_waypoint_record *Alloc_AN1_Waypoint( ) {
	an1_waypoint_record *result = NULL;
	result = (an1_waypoint_record *)xcalloc( sizeof(*result), 1 );
	result->fs.type = FS_AN1W;
	result->fs.copy = Copy_AN1_Waypoint;
	result->fs.destroy = Destroy_AN1_Waypoint;
	return result;
}
	
static an1_vertex_record *Alloc_AN1_Vertex();

void Destroy_AN1_Vertex( void *vvertex ) {
	/* do nothing */
}

void Copy_AN1_Vertex( void **vdvert, void *vvert ) {
	an1_vertex_record *vert = (an1_vertex_record *)vvert;
	an1_vertex_record *dvert = Alloc_AN1_Vertex();
	memcpy( dvert, vert, sizeof( an1_vertex_record ));
	*vdvert = (void *)dvert;
}

static an1_vertex_record *Alloc_AN1_Vertex() {
	an1_vertex_record *result = NULL;
	result = (an1_vertex_record *)xcalloc( sizeof( *result), 1 );
	result->fs.type = FS_AN1V;
	result->fs.copy = Copy_AN1_Vertex;
	result->fs.destroy = Destroy_AN1_Vertex;
	return result;
}
			

static an1_line_record *Alloc_AN1_Line();

void Destroy_AN1_Line( void *vline ) {
	an1_line_record *line = (an1_line_record *)vline;
	xfree( line->name );
}

void Copy_AN1_Line( void **vdline, void *vline ) {
	an1_line_record *line = (an1_line_record *)vline;
	an1_line_record *dline = Alloc_AN1_Line();
	memcpy( dline, line, sizeof( an1_line_record ));
	dline->name = xstrdup( line->name );
	*vdline = (void *)dline;
}

static an1_line_record *Alloc_AN1_Line( ) {
	an1_line_record *result = NULL;
	result = (an1_line_record *)xcalloc( sizeof(*result), 1 );
	result->fs.type = FS_AN1L;
	result->fs.copy = Copy_AN1_Line;
	result->fs.destroy = Destroy_AN1_Line;
	return result;
}


static void Destroy_AN1_Symbol( an1_symbol_record *symbol ) {
	xfree( symbol->name );
}

static void Read_AN1_Waypoint( FILE *f, an1_waypoint_record *wpt ) {
	short len;
	
	wpt->magic = ReadShort( f );
	wpt->unk1 = ReadLong( f );
	wpt->lon = ReadLong( f );
	wpt->lat = ReadLong( f );
	wpt->type = ReadShort( f );
	wpt->height = ReadLong( f );
	wpt->width = ReadLong( f );
	wpt->unk2 = ReadShort( f );
	wpt->unk3 = ReadShort( f );
	wpt->serial = ReadShort( f );
	wpt->unk4 = ReadShort( f );
	wpt->create_zoom = ReadChar( f );
	wpt->visible_zoom = ReadChar( f );
	wpt->unk5 = ReadShort( f );
	wpt->radius = ReadDouble( f );
        len = ReadShort( f );
        wpt->name = ReadString( f, len );
        len = ReadShort( f );
        wpt->fontname = ReadString( f, len );
	ReadGuid( f, &wpt->guid );
        wpt->fontcolor = ReadLong( f );
	wpt->fontstyle = ReadLong( f );
	wpt->fontsize = ReadLong( f );
	wpt->outlineweight = ReadLong( f );
	wpt->outlinecolor = ReadLong( f );
	wpt->outlineflags = ReadLong( f );
	wpt->fillcolor = ReadLong( f );
	wpt->unk6 = ReadLong( f );
	wpt->fillflags = ReadLong( f );	
}

static void Write_AN1_Waypoint( FILE *f, an1_waypoint_record *wpt ) {
	short len;
	
	WriteShort( f, wpt->magic );
	WriteLong( f, wpt->unk1 );
	WriteLong( f, wpt->lon );
	WriteLong( f, wpt->lat );
	WriteShort( f, wpt->type );
	WriteLong( f, wpt->height );
	WriteLong( f, wpt->width );
	WriteShort( f, wpt->unk2 );
	WriteShort( f, wpt->unk3 );
	WriteShort( f, wpt->serial );
	WriteShort( f, wpt->unk4 );
	WriteChar( f, wpt->create_zoom );
	WriteChar( f, wpt->visible_zoom );
	WriteShort( f, wpt->unk5 );
	WriteDouble( f, wpt->radius );
	len = strlen( wpt->name );
	WriteShort( f, len );
        WriteString( f, wpt->name );
	len = strlen( wpt->fontname );
	WriteShort( f, len );
        WriteString( f, wpt->fontname );
	WriteGuid( f, &wpt->guid );
        WriteLong( f, wpt->fontcolor );
	WriteLong( f, wpt->fontstyle );
	WriteLong( f, wpt->fontsize );
	WriteLong( f, wpt->outlineweight );
	WriteLong( f, wpt->outlinecolor );
	WriteLong( f, wpt->outlineflags );
	WriteLong( f, wpt->fillcolor );
	WriteLong( f, wpt->unk6 );
	WriteLong( f, wpt->fillflags );	
}

static void Read_AN1_Vertex( FILE *f, an1_vertex_record *vertex ) {
	
	vertex->magic = ReadShort( f );
	vertex->unk0 = ReadLong( f );
	vertex->lon = ReadLong( f );
	vertex->lat = ReadLong( f );
	vertex->unk1 = ReadShort( f );
}

static void Write_AN1_Vertex( FILE *f, an1_vertex_record *vertex ) {
	WriteShort( f, vertex->magic );
	WriteLong( f, vertex->unk0 );
	WriteLong( f, vertex->lon );
	WriteLong( f, vertex->lat );
	WriteShort( f, vertex->unk1 );
}

static void Read_AN1_Line( FILE *f, an1_line_record *line ) {
	
	short len;
	
	line->magic = ReadShort( f );
	line->unk1 = ReadShort( f );
	line->serial = ReadShort( f );
	line->unk2 = ReadLong( f );
	line->unk3 = ReadShort( f );
	line->type = ReadShort( f );
	line->unk4 = ReadLong( f );
	len = ReadShort( f );
	line->name = ReadString( f, len );
	line->lineweight = ReadShort( f );
	line->linestyle = ReadLong( f );
	line->linecolor = ReadLong( f );
	line->unk5 = ReadLong( f );
	line->polyfillcolor = ReadLong( f );
	line->unk6 = ReadLong( f );
	line->unk7 = ReadLong( f );
	line->unk8 = ReadShort( f );
	line->pointcount = ReadLong( f );
}

static void Write_AN1_Line( FILE *f, an1_line_record *line ) {
	short len;
	
	WriteShort( f, line->magic );
	WriteShort( f, line->unk1 );
	WriteShort( f, line->serial );
	WriteLong( f, line->unk2 );
	WriteShort( f, line->unk3 );
	WriteShort( f, line->type );
	WriteLong( f, line->unk4 );
	len = strlen( line->name );
	WriteShort( f, len );
	WriteString( f, line->name );
	WriteShort( f, (short) line->lineweight );
	WriteLong( f, line->linestyle );
	WriteLong( f, line->linecolor );
	WriteLong( f, line->unk5 );
	WriteLong( f, line->polyfillcolor );
	WriteLong( f, line->unk6 );
	WriteLong( f, line->unk7 );
	WriteShort( f, line->unk8 );
	WriteLong( f, line->pointcount );
} 

static void Skip_AN1_IL( FILE *f ) {
	Skip( f, 26 );
}

static void Skip_AN1_BM( FILE *f ) {
	unsigned long bmsize;
	unsigned long palettesize;
	unsigned long bmisize;
	unsigned long bitoffset;
	
	Skip( f, 8 );  /* BITMAPFILEHEADER fields 1-3 */
	bitoffset = ReadLong( f );
	
	bmisize = ReadLong( f );
	Skip( f, 16 );  /* BITMAPINFOHEADER fields 2-6 */
	bmsize = ReadLong( f );
	Skip( f, 16 );  /* BITMAPINFOHEADER fields 8-11 */
	
	palettesize = bitoffset - bmisize - 14;
	Skip( f, bmsize + palettesize );
}

static void Read_AN1_Symbol( FILE *f, an1_symbol_record *symbol ) {
	short len;

	/* This is just the high word of a long; we ate the low 
	 * word in the caller.  Fortunately, we don't care. */
	symbol->hotspotxhi = ReadShort( f );
	symbol->hotspoty = ReadLong( f );
	symbol->unk1 = ReadLong( f );
	ReadGuid( f, &symbol->guid );
	len = ReadChar( f );
	symbol->name = ReadString( f, len );
}

static void Read_AN1_Header( FILE *f ) {
	unsigned short magic;
	unsigned short type;
	
	magic = ReadShort( f );
	type = ReadShort( f );
}

static void Write_AN1_Header( FILE *f ) {
	WriteShort( f, 11557 );
	WriteShort( f, (short) atoi( output_type ) );
}

static void Read_AN1_Bitmaps( FILE *f ) {
	long count;
	unsigned short magic;
	an1_symbol_record symbol;
	
	count = ReadLong( f );
	
	while ( count ) {
		magic = ReadShort( f );
		switch (magic) {
			case 0x4d42:
				Skip_AN1_BM( f );
				break;
			case 0x4c49:
				Skip_AN1_IL( f );
				break;
			default:
				Read_AN1_Symbol( f, &symbol );
				Destroy_AN1_Symbol( &symbol );
				count--;
				break;
		}
	}
						
	/* Read the symbol table */
}

static void Write_AN1_Bitmaps( FILE *f ) {
	/* On write, we don't output any bitmaps, so writing them
	 * is just a matter of writing a count of zero */
	WriteLong( f, 0 );
}

static void Read_AN1_Waypoints( FILE *f ) {
	unsigned long count = 0;
	unsigned long i = 0;
	an1_waypoint_record *rec = NULL;
	waypoint *wpt_tmp;	
	char *icon = NULL;
	ReadShort( f );
	count = ReadLong( f );
	for (i = 0; i < count; i++ ) {
		rec = Alloc_AN1_Waypoint();
		Read_AN1_Waypoint( f, rec );
		wpt_tmp = waypt_new();
		
		wpt_tmp->longitude = -DecodeOrd( rec->lon );
		wpt_tmp->latitude = DecodeOrd( rec->lat );
		wpt_tmp->description = xstrdup( rec->name );
		
		if (FindIconByGuid(&rec->guid, &icon)) {
			wpt_tmp->icon_descr = icon;
		}
	
		fs_chain_add( &(wpt_tmp->fs), (format_specific_data *)rec);
		rec = NULL;
		waypt_add( wpt_tmp );
	}
}

static void
Write_One_AN1_Waypoint( const waypoint *wpt )
{
	an1_waypoint_record *rec;
	int local;
	format_specific_data *fs = NULL;
	
	fs = fs_chain_find( wpt->fs, FS_AN1W );
	
	if ( fs ) {
		rec = (an1_waypoint_record *)fs;
		xfree( rec->name );
		local = 0;
	}
	else {
		rec = Alloc_AN1_Waypoint();
		local = 1;
		rec->magic = 1;
		rec->type = 1;
		rec->unk2 = 3;
		rec->unk3 = 18561;
		rec->fontname = xstrdup( "Arial" );
		FindIconByName( "Red Flag", &rec->guid );
		rec->fontsize = 10;
	}
	rec->name = xstrdup( wpt->description );
	rec->lat = EncodeOrd( wpt->latitude );
	rec->lon = EncodeOrd( -wpt->longitude );
	rec->serial = serial++;
	
	if ( wpt->icon_descr ) {
		FindIconByName( (char *)(void *)wpt->icon_descr, &rec->guid );
	}
	
	Write_AN1_Waypoint( outfile, rec );
	if ( local ) {
		Destroy_AN1_Waypoint( rec );
		xfree( rec );
	}
}

static void Write_AN1_Waypoints( FILE *f ) {
	WriteShort( f, 2 );
	WriteLong( f, waypt_count() );
	waypt_disp_all( Write_One_AN1_Waypoint );
}

static void Read_AN1_Lines( FILE *f ) {
	unsigned long count = 0;
	unsigned long i = 0;
	unsigned long j = 0;
	an1_line_record *rec = NULL;
	an1_vertex_record *vert = NULL;
        route_head *rte_head;
        waypoint *wpt_tmp;

	ReadShort( f );
	count = ReadLong( f );
	for (i = 0; i < count; i++ ) {
		rec = Alloc_AN1_Line();
		Read_AN1_Line( f, rec );
		/* create route rec */
                rte_head = route_head_alloc();
		fs_chain_add( &rte_head->fs, (format_specific_data *)rec );
                route_add_head(rte_head);
		for (j = 0; j < (unsigned) rec->pointcount; j++ ) {
			vert = Alloc_AN1_Vertex();
			Read_AN1_Vertex( f, vert );
			
			/* create route point */
		        wpt_tmp = waypt_new();
                        wpt_tmp->latitude = DecodeOrd( vert->lat );
		        wpt_tmp->longitude = -DecodeOrd( vert->lon );
		        wpt_tmp->shortname = (char *) xmalloc(7);
		        sprintf( wpt_tmp->shortname, "\\%5.5x", rtserial++ );
			fs_chain_add( &wpt_tmp->fs,
				(format_specific_data *)vert );
		        route_add_wpt(rte_head, wpt_tmp);
		}
	}
}

static void
Write_One_AN1_Line( const route_head *rte )
{
	an1_line_record *rec;
	int local;
	format_specific_data *fs = NULL;
	
	fs = fs_chain_find( rte->fs, FS_AN1L );
	
	if ( fs ) {
		rec = (an1_line_record *)(void *)fs;	
		local = 0;
		switch (output_type_num) {
			case 1:
				if ( rec->type != 14 ) {
					rec = Alloc_AN1_Line();
					memcpy( rec, fs, sizeof(an1_line_record));
					local = 1;
					rec->magic = 4112;
					rec->unk1 = 4359;
					rec->unk2 = 655360;
					rec->type = 14;
					rec->unk8 = 2;
				} // end if
				break;
			case 2:
				if ( rec->type != 15 ) {
					rec = Alloc_AN1_Line();
					memcpy( rec, fs, sizeof(an1_line_record));
					local = 1;
					rec->type = 15;
				} // end if
				break;				
			case 4:
				if ( rec->type != 16 ) {
					rec = Alloc_AN1_Line();
					memcpy( rec, fs, sizeof(an1_line_record));
					local = 1;
					rec->type = 16;
				} // end if
				break;
		}
	}
	else {
		rec = Alloc_AN1_Line();
		local = 1;
		switch (output_type_num) {
			/*  drawing road trail waypoint track  */
			case 1: /* road */
				rec->magic = 4112;
				rec->unk1 = 4359;
				rec->unk2 = 655360;
				rec->type = 14;
				rec->unk8 = 2;
				break;
				
			case 2: /* trail */
				rec->magic = 7248;
				rec->unk1 = 4359;
				rec->unk2 = 917504;
				rec->type = 15;
				rec->unk8 = 2;
				break;
				
			case 4: /* track */
				rec->magic = 21;
				rec->unk1 = 18560;
				rec->unk2 = 917504;
				rec->type = 16;
				rec->unk4 = 2;
				rec->unk8 = 2;
				break;
				
			case 0: /* drawing */
			case 3: /* waypoint - shouldn't have lines */
			default:
				rec->magic = 21;
				rec->unk1 = 18560;
				rec->unk2 = 1048576;
				rec->type = 2;
				rec->unk4 = 2;
				rec->lineweight = 6;
				rec->linecolor = 255; /* red */
				rec->unk5 = 3;
				rec->unk8 = 2;
				break;
		}
		rec->name = xstrdup( "" );
				
	}
	rec->serial = serial++;
	rec->pointcount = rte->rte_waypt_ct;	
	Write_AN1_Line( outfile, rec );
	if ( local ) {
		Destroy_AN1_Line( rec );
		xfree( rec );
	}
}

static void
Write_One_AN1_Vertex( const waypoint *wpt )
{
	an1_vertex_record *rec;
	int local;
	format_specific_data *fs = NULL;
	
	fs = fs_chain_find( wpt->fs, FS_AN1V );
	
	if ( fs ) {
		rec = (an1_vertex_record *)(void *)fs;	
		local = 0;
	}
	else {
		rec = Alloc_AN1_Vertex();
		local = 1;
		rec->magic = 1;
	}
	rec->lat = EncodeOrd( wpt->latitude );
	rec->lon = EncodeOrd( -wpt->longitude );
	
	Write_AN1_Vertex( outfile, rec );
	if ( local ) {
		Destroy_AN1_Vertex( rec );
		xfree( rec );
	}
}

static void Write_AN1_Lines( FILE *f ) {
	WriteShort( f, 2 );
	WriteLong( f, route_count()+track_count() );
	
	route_disp_all( Write_One_AN1_Line, NULL, Write_One_AN1_Vertex );
	track_disp_all( Write_One_AN1_Line, NULL, Write_One_AN1_Vertex );
}

static void
rd_init(const char *fname)
{
	infile = xfopen(fname, "rb", MYNAME);
	output_type_num = atoi( output_type );
}

static void
rd_deinit(void)
{
	fclose(infile);
}

static void
my_read(void)
{
	Read_AN1_Header( infile );
	Read_AN1_Bitmaps( infile );
	Read_AN1_Waypoints( infile );
	Read_AN1_Lines( infile );
}

static void
wr_init(const char *fname)
{
	outfile = xfopen( fname, "wb", MYNAME );
	output_type_num = atoi( output_type );
}

static void
wr_deinit( void ) 
{
	fclose(outfile);
}

static void
my_write( void ) 
{
	Write_AN1_Header( outfile );
	Write_AN1_Bitmaps( outfile );
	Write_AN1_Waypoints( outfile );
	Write_AN1_Lines( outfile );
}

ff_vecs_t an1_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	my_read,
	my_write,
	NULL, 
	an1_args
};
