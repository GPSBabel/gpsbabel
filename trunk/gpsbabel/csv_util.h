/*
    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)

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

/* function prototypes */

char*
#ifndef DEBUG_MEM
csv_stringtrim(const char* string, const char* enclosure, int strip_max);
#else
CSV_STRINGTRIM(const char* string, const char* enclosure, int strip_max, DEBUG_PARAMS);
#define csv_stringtrim( s, e,m ) CSV_STRINGTRIM( s, e, m, __FILE__, __LINE__)
#endif

char*
csv_lineparse(const char* stringstart, const char* delimited_by, const char* enclosed_in, const int line_no);

void
human_to_dec(const char* instr, double* outlat, double* outlon, int which);

char*
#ifndef DEBUG_MEM
csv_stringclean(const char* string, const char* chararray);
#else
CSV_STRINGCLEAN(const char* string, const char* chararray,DEBUG_PARAMS);
#define csv_stringclean(s,c) CSV_STRINGCLEAN(s,c,__FILE__,__LINE__)
#endif

void
xcsv_data_read(void);

void
xcsv_data_write(void);

void
xcsv_file_init(void);

void
xcsv_prologue_add(char*);

void
xcsv_epilogue_add(char*);

void
xcsv_ifield_add(char*, char*, char*);

void
xcsv_ofield_add(char*, char*, char*, int options);

void
xcsv_destroy_style(void);

const char*
xcsv_get_char_from_constant_table(char* key);

/****************************************************************************/
/* types required for various xcsv functions                                */
/****************************************************************************/

/* something to map fields to waypts */
#define OPTIONS_NODELIM 1
#define OPTIONS_ABSOLUTE 2
#define OPTIONS_OPTIONAL 3
typedef struct field_map {
  queue Q;
  char* key;
  char* val;
  char* printfc;
  int hashed_key;
  int options;
} field_map_t;

/* a queuing struct for prologues / epilogues */
typedef struct ogue {
  queue Q;
  char* val;
} ogue_t;

/* something to map config file constants to chars */
typedef struct char_map {
  const char* key;
  const char* chars;
} char_map_t;

/*
 * a type describing all the wonderful elements of xcsv files, in a
 * nutshell.
 */
typedef struct {
  int is_internal;		/* bool - is internal (1) or parsed (0) */

  int prologue_lines;		/* # of lines to ignore at top of the file */
  int epilogue_lines;		/* # of lines to ignore at bottom of file */

  /* header lines for writing at the top of the file. */
  queue prologue;

  /* footer lines for writing at the bottom of the file. */
  queue epilogue;

  char* field_delimiter; 	/* comma, quote, etc... */
  char* field_encloser; 	/* doublequote, etc... */
  char* record_delimiter;	/* newline, c/r, etc... */

  char* badchars;		/* characters we never write to output */

  queue ifield;		/* input field mapping */
  queue* ofield;    		/* output field mapping */

  int ifield_ct;		/* actual # of ifields */
  int ofield_ct;		/* actual # of ofields */

  gbfile* xcsvfp;		/* ptr to current *open* data file */
  char* fname;                 /* ptr to filename of above. */

  char* description;		/* Description for help text */
  char* extension;		/* preferred filename extension (for wrappers)*/

  short_handle mkshort_handle;/* handle for mkshort() */
  ff_type type;		/* format type for GUI wrappers. */

  int gps_datum;		/* result of GPS_Lookup_Datum_Index */
  gpsdata_type datatype;	/* can be wptdata, rtedata or trkdata */
  /* ... or ZERO to keep the old behaviour */

} xcsv_file_t;


/****************************************************************************/
/* obligatory global struct                                                 */
/****************************************************************************/
extern xcsv_file_t xcsv_file;
