/*
    Output only format for PalmDoc

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net
    Copyright (C) 2004 Ronald L. Parker, ron@parkrrrr.com
    Portions from txt2pdbdoc, Copyright (C) 1998  Paul J. Lucas

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
#if PDBFMTS_ENABLED
#include "jeeps/gpsmath.h"
#include <ctype.h>
#include "pdbfile.h"

static pdbfile *file_out;
static short_handle mkshort_handle;
static short_handle mkshort_bookmark_handle;
static const char *out_fname;

static char *suppresssep = NULL;
static char *dbname = NULL;
static char *bmid = NULL;
static char *includelogs = NULL;

static int ct;
static int offset;

static char *palm_encrypt;

#define MYNAME "PALMDOC"

/* constants */
#define	BUFFER_SIZE    	4096     	/* big enough for uncompressed record */
#define	COMPRESSED	2
#define	COUNT_BITS	3		/* why this value?  I don't know */
#define	DISP_BITS	11		/* ditto */

#define	DOC_CREATOR	0x52454164	/* "REAd" */
#define	DOC_TYPE	0x54455874	/* "TEXt" */
#define	UNCOMPRESSED	1

struct buffer {
	unsigned char *data;
	unsigned len;
};

#define	NEW_BUFFER(b)	(b)->data = (unsigned char *)xmalloc( ((b)->len = 0,BUFFER_SIZE) )

static
arglist_t palmdoc_args[] = {
	{ "nosep", &suppresssep, 
		"No separator lines between waypoints", NULL,
		ARGTYPE_BOOL, ARG_NOMINMAX },
	{"dbname", &dbname, "Database name", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
	{"encrypt", &palm_encrypt, "Encrypt hints with ROT13", NULL,
		ARGTYPE_BOOL, ARG_NOMINMAX },
	{ "logs", &includelogs,
		"Include groundspeak logs if present", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	{ "bookmarks_short", &bmid, "Include short name in bookmarks", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	ARG_TERMINATOR
};

static struct buffer buf;

struct doc_record0                   /* 16 bytes total */
{
    gbuint16 version;          /* 1 = plain text, 2 = compressed */
    gbuint16 reserved1;
    gbuint32 doc_size;         /* in bytes, when uncompressed */
    gbuint16 num_records;      /* PDB header numRecords - 1 */
    gbuint16 rec_size;         /* usually RECORD_SIZE_MAX */
    gbuint32 reserved2;
    gbuint16 recsizes[1];
};

static struct recordsize {
	int size;
	struct recordsize *next;
} *recordsize_tail;

static struct bookmark {
	int offset;
	char *text;
	struct bookmark *next;
} *bookmark_tail;

struct bookmark_record {
	char text[16];
	gbuint32 offset;
};

static void put_byte(struct buffer *b, unsigned char c, int *space)
{
	if ( *space ) {
		*space = 0;
		/*
		** There is an outstanding space char: see if we can squeeze it
		** in with an ASCII char.
		*/
		if ( c >= 0x40 && c <= 0x7F ) {
			b->data[ b->len++ ] = c ^ 0x80;
			return;
		}
		b->data[ b->len++ ] = ' ';	/* couldn't squeeze it in */
	} else if ( c == ' ' ) {
		*space = 1;
		return;
	}

	if ( (c >= 1 && c <= 8) || c >= 0x80 )
		b->data[ b->len++ ] = '\1';

	b->data[ b->len++ ] = c;
}

static unsigned char * mem_find(unsigned char *t, int t_len, unsigned char *m, int m_len)
{
	register int i;
	for ( i = t_len - m_len + 1; i > 0; --i, ++t )
		if ( *t == *m && !memcmp( t, m, m_len ) )
			return t;
	return 0;
}


static void pd_compress( struct buffer *b )
{

	unsigned i, j;
	int space = 0;

	unsigned char *buf_orig;
	unsigned char *p;	/* walking test hit; works up on successive matches */
	unsigned char *p_prev;
	unsigned char *head;	/* current test string */
	unsigned char *tail;	/* 1 past the current test buffer */
	unsigned char *end;	/* 1 past the end of the input buffer */

	p = p_prev = head = buf_orig = b->data;
	tail = head + 1;
	end = b->data + b->len;

	NEW_BUFFER( b );
	b->len = 0;

	/* loop, absorbing one more char from the input buffer on each pass */
	while ( head != end ) {
		/* establish where the scan can begin */
		if ( head - p_prev > (( 1 << DISP_BITS )-1) )
			p_prev = head - (( 1 << DISP_BITS )-1);

		/* scan in the previous data for a match */
		p = mem_find( p_prev, tail - p_prev, head, tail - head );

		/* on a mismatch or end of buffer, issued codes */
		if ( !p || p == head || tail - head > ( 1 << COUNT_BITS ) + 2
			|| tail == end
		) {
			/* issued the codes */
			/* first, check for short runs */
            if ( tail - head < 4 ) {
				put_byte( b, *head++, &space );
            }
			else {
				unsigned dist = head - p_prev;
				unsigned compound = (dist << COUNT_BITS)
					+ tail - head - 4;

                /* for longer runs, issue a run-code */
				/* issue space char if required */
				if ( space ) {
					b->data[ b->len++ ] = ' ';
					space = 0;
				}

				b->data[ b->len++ ] = 0x80 + ( compound >> 8 );
				b->data[ b->len++ ] = compound & 0xFF;
				head = tail - 1;/* and start again */
			}
			p_prev = buf_orig;	/* start search again */
		} else
			p_prev = p;		/* got a match */

		/* when we get to the end of the buffer, don't inc past the */
		/* end; this forces the residue chars out one at a time */
		if ( tail != end )
			++tail;
	}
	xfree( buf_orig );

	if ( space )
		b->data[ b->len++ ] = ' ';	/* add left-over space */

	/* final scan to merge consecutive high chars together */
	for ( i = j = 0; i < b->len; ++i, ++j ) {
		b->data[ j ] = b->data[ i ];

		/* skip run-length codes */
		if ( b->data[ j ] >= 0x80 && b->data[ j ] < 0xC0 )
			b->data[ ++j ] = b->data[ ++i ];

		/* if we hit a high char marker, look ahead for another */
		else if ( b->data[ j ] == '\1' ) {
			b->data[ j + 1 ] = b->data[ i + 1 ];
			while ( i + 2 < b->len &&
				b->data[ i + 2 ] == 1 && b->data[ j ] < 8
			) {
				b->data[ j ]++;
				b->data[ j + b->data[ j ] ] = b->data[ i + 3 ];
				i += 2;
			}
			j += b->data[ j ];
			++i;
		}
	}
	b->len = j;
}

static void write_header( void ) {
	
	int recs = ct-1;
	struct doc_record0 *rec0;
	--ct;
	
	rec0 = xcalloc( 1, sizeof(struct doc_record0)+(ct-1)*sizeof(short));
	be_write16( &rec0->version, COMPRESSED );
	be_write16( &rec0->reserved1, 0 );
	be_write32( &rec0->doc_size, offset );
	be_write16( &rec0->num_records, ct );
	be_write16( &rec0->rec_size, 4096 );
	be_write32( &rec0->reserved2, 0 );
	while ( recs ) {
		struct recordsize *oldrec = recordsize_tail;
		be_write16( &rec0->recsizes[recs], oldrec->size );
		recordsize_tail = oldrec->next;
		xfree( oldrec );
		--recs;
	}
	
	pdb_write_rec(file_out, 0, 0, 0, (void *)rec0, sizeof(struct doc_record0) + sizeof(short)*(ct-1));

	xfree(rec0);
}

static void write_bookmarks( void ) {
	struct bookmark *oldmark = NULL;
	struct bookmark_record rec;

	struct bookmark *newtail = NULL;

	/* reverse the bookmark list */	
	while ( bookmark_tail ) {
		oldmark = bookmark_tail;
		bookmark_tail = oldmark->next;
		oldmark->next = newtail;
		newtail = oldmark;
	}
	bookmark_tail = newtail;
	
	ct++;
	while ( bookmark_tail ) {
		oldmark = bookmark_tail;
		bookmark_tail = oldmark->next;
		
		be_write32( &rec.offset, oldmark->offset );
		memset( rec.text, 0, 16 );
		strncpy( rec.text, oldmark->text, 16 );
		
		pdb_write_rec(file_out, 0, 0, ct++, (void *)&rec, sizeof(struct bookmark_record));

		xfree( oldmark );
	} 
}

static void commit_buffer( void ) {

	struct recordsize *newrec = xcalloc( 1, sizeof(struct recordsize));
	newrec->next = recordsize_tail;
	newrec->size = buf.len;
	recordsize_tail = newrec;

	pd_compress( &buf );
	
	pdb_write_rec(file_out, 0, 0, ct++, (void *)buf.data, buf.len);
}

static void create_bookmark( char *bmtext ) {
	struct bookmark *newmark = (struct bookmark *) xcalloc( 1, sizeof(struct bookmark));
	newmark->next = bookmark_tail;
	newmark->offset = offset;
	newmark->text = bmtext;
	bookmark_tail = newmark;
}	

static void docprintf( int maxlen, char *format, ... ) {

    char *txt = NULL;
    char *txt2 = NULL;
    va_list list;
    int newlen;
    int partlen;
    
    txt = (char *) xmalloc( maxlen );  
	
    va_start( list, format );
    newlen = vsprintf( txt, format, list );

    txt2 = txt;
    offset += newlen;
    while (txt2 && *txt2 ) {    
        /* append to buffer what we can */
	partlen = BUFFER_SIZE-1-buf.len;	
    	if ( buf.len + newlen + 1 > BUFFER_SIZE ) 
    	{
	    strncpy( (char *) buf.data+buf.len, txt2, partlen );
            buf.data[BUFFER_SIZE-1] = '\0';
            txt2 += partlen;
            newlen -= partlen;
            buf.len = BUFFER_SIZE-1;
            commit_buffer();
            NEW_BUFFER( &buf );
        }
        else { 
            strcpy( (char *) buf.data+buf.len, txt2 );
            buf.len += newlen;
            txt2 = NULL;
        }
    }
    
    xfree( txt );   
}

static void docfinish() {
	commit_buffer();
	write_header();
	write_bookmarks();
}

static void
wr_init(const char *fname)
{
	file_out = pdb_create(fname, MYNAME);
        out_fname = fname;
		
	mkshort_handle = mkshort_new_handle();
	mkshort_bookmark_handle = mkshort_new_handle();
	ct = 1;
	offset = 1;
	recordsize_tail = NULL;
	bookmark_tail = NULL;
	NEW_BUFFER( &buf );
}

static void
wr_deinit(void)
{
	pdb_close(file_out);
	mkshort_del_handle(&mkshort_handle);
	mkshort_del_handle(&mkshort_bookmark_handle);
	
        if ( dbname ) {
            xfree(dbname);
            dbname = NULL;
	}
}

static void
palmdoc_disp(const waypoint *wpt)
{
	int latint, lonint;
	char tbuf[1024];
	time_t tm = wpt->creation_time;
	int32 utmz;
	double utme, utmn;
	char utmzc;
	char *bm;
	fs_xml *fs_gpx = NULL;

        char bookmarktext[17];

        if ( bmid ) {
		char * s = mkshort_from_wpt(mkshort_bookmark_handle, wpt);
		sprintf( bookmarktext, "%6s:%9s", 
			wpt->shortname?wpt->shortname:"",s);
		xfree(s);
	}
	else {
		char * s = mkshort_from_wpt(mkshort_bookmark_handle, wpt);
		sprintf( bookmarktext, "%16s", s);
		xfree(s);
	}	

        bm = xstrdup(bookmarktext); 
        create_bookmark(bm);
 	
	lonint = abs((int) wpt->longitude);
	latint = abs((int) wpt->latitude);

	GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude, 
		&utme, &utmn, &utmz, &utmzc);

	if (tm == 0) 
		tm = time(NULL);
	strftime(tbuf, sizeof(tbuf), "%d-%b-%Y", localtime(&tm));

	docprintf(300, "%-16s  %c%d %06.3f  %c%d %06.3f  (%d%c %6.0f %7.0f)",
		(global_opts.synthesize_shortnames) ? mkshort_from_wpt(mkshort_handle, wpt) : wpt->shortname,
		wpt->latitude < 0 ? 'S' : 'N',  abs(latint), 60.0 * (fabs(wpt->latitude) - latint), 
		wpt->longitude < 0 ? 'W' : 'E', abs(lonint), 60.0 * (fabs(wpt->longitude) - lonint),
		utmz, utmzc, utme, utmn);
	if (wpt->altitude != unknown_alt) 
		docprintf (100, "  alt: %1.1f", wpt->altitude);
	docprintf (10, "\n");
	if (strcmp(wpt->description, wpt->shortname)) {
		docprintf(10+strlen(wpt->description), "%s\n", wpt->description);
	}
	if (wpt->gc_data.terr) {

		docprintf (100, "%s/%s\n", gs_get_cachetype(wpt->gc_data.type), 
			gs_get_container(wpt->gc_data.container));

	        if (wpt->gc_data.desc_short.utfstring) {
	                char *stripped_html = strip_html(&wpt->gc_data.desc_short);
			docprintf (10+strlen(stripped_html), "\n%s\n", stripped_html);
                	xfree(stripped_html);
       		}
	        if (wpt->gc_data.desc_long.utfstring) {
	                char *stripped_html = strip_html(&wpt->gc_data.desc_long);
			docprintf (10+strlen(stripped_html), "\n%s\n", stripped_html);
                	xfree(stripped_html);
       		}
		if (wpt->gc_data.hint) {
			char *hint = NULL;
			if ( palm_encrypt )
				hint = rot13( wpt->gc_data.hint );
			else
				hint = xstrdup( wpt->gc_data.hint );
			docprintf (10+strlen(hint), "\nHint: %s\n", hint);
			xfree( hint );
		}
	}
	else if (wpt->notes && (!wpt->description || strcmp(wpt->notes,wpt->description))) {
		docprintf (10+strlen(wpt->notes), "%s\n", wpt->notes);
	}
        
	fs_gpx = NULL;
        if ( includelogs ) {
	        fs_gpx = (fs_xml *)fs_chain_find( wpt->fs, FS_GPX);
	}
		
        if ( fs_gpx && fs_gpx->tag ) {
                xml_tag *root = fs_gpx->tag;
		xml_tag *curlog = NULL;
		xml_tag *logpart = NULL;
		curlog = xml_findfirst( root, "groundspeak:log" );
		while ( curlog ) {
			time_t logtime = 0;
			struct tm *logtm = NULL;
			docprintf( 10, "\n" );
			
			logpart = xml_findfirst( curlog, "groundspeak:type" );
			if ( logpart ) {
				docprintf( 10+strlen(logpart->cdata), "%s by ", logpart->cdata );
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:finder" );
			if ( logpart ) {
				docprintf( 10+strlen(logpart->cdata), "%s on ", logpart->cdata );
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:date" );
			if ( logpart ) {
				logtime = xml_parse_time( logpart->cdata, NULL);
				logtm = localtime( &logtime );
				if ( logtm ) {
					docprintf( 15, 
						"%2.2d/%2.2d/%4.4d\n",
						logtm->tm_mon+1,
						logtm->tm_mday,
						logtm->tm_year+1900
						);
				}
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:log_wpt" );
			if ( logpart ) {
				char *coordstr = NULL;
				float lat = 0;
				int latdeg = 0;
				float lon = 0;
				int londeg = 0;
				coordstr = xml_attribute( logpart, "lat" );
				if ( coordstr ) {
					lat = atof( coordstr );
				}
			        coordstr = xml_attribute( logpart, "lon" );
				if ( coordstr ) {
					lon = atof( coordstr );
				}
				latdeg = abs(lat);
				londeg = abs(lon);
				
				docprintf( 30,
					"%c %d\xb0 %.3f' %c %d\xb0 %.3f'\n",
				
					lat < 0 ? 'S' : 'N', latdeg, 60.0 * (fabs(lat) - latdeg), 
					lon < 0 ? 'W' : 'E', londeg, 60.0 * (fabs(lon) - londeg)
				);
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:text" );
			if ( logpart ) {
				char *encstr = NULL;
				char *s = NULL;
				int encoded = 0;
				encstr = xml_attribute( logpart, "encoded" );
				encoded = (encstr[0] != 'F');
				
				if ( palm_encrypt && encoded ) {
					s = rot13( logpart->cdata );
				}
				else {
					s = xstrdup( logpart->cdata );
				}
					
				docprintf( 5+strlen(s), "%s", s ); 
				xfree( s );
			}

			docprintf( 10, "\n" );
			curlog = xml_findnext( root, curlog, "groundspeak:log" );
		}
	}
	if (! suppresssep) 
		docprintf(50, "---------------------------\n");
	else
		docprintf(10, "\n");
}

static void
data_write(void)
{
        if ( dbname ) {
            strncpy( file_out->name, dbname, PDB_DBNAMELEN );
        }
        else {
            strncpy(file_out->name, out_fname, PDB_DBNAMELEN);
        }
        file_out->name[PDB_DBNAMELEN-1] = 0;
        file_out->attr = PDB_FLAG_BACKUP;
        file_out->ctime = file_out->mtime = current_time() + 2082844800U;
        file_out->type = DOC_TYPE; 
        file_out->creator = DOC_CREATOR;
        file_out->version = 1;
	
	if (! suppresssep) 
		docprintf(50, "---------------------------\n");
	setshort_length(mkshort_handle, 20 );
	setshort_length(mkshort_bookmark_handle, 16-(bmid?7:0));
	setshort_whitespace_ok( mkshort_bookmark_handle, 0 );
	waypt_disp_all(palmdoc_disp);

        docfinish();	
}


ff_vecs_t palmdoc_vecs = {
	ff_type_file,
	{ ff_cap_write, ff_cap_none, ff_cap_none},
	NULL,
	wr_init,
	NULL,
	wr_deinit,
	NULL,
	data_write,
	NULL, 
	palmdoc_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};


#endif
