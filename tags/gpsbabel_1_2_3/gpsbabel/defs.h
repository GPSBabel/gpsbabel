/*
    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#ifndef gpsbabel_defs_h_included
#define gpsbabel_defs_h_included
#include <time.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include "queue.h"


/*
 * Amazingly, this constant is not specified in the standard...
 */
#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

/*
 * Snprintf is in SUS (so it's in most UNIX-like substance) and it's in 
 * C99 (albeit with slightly different semantics) but it isn't in C89.   
 * This tweaks allows us to use snprintf on the holdout.
 */
#if __WIN32__
#  define snprintf _snprintf
#endif

/*
 * Common definitions.   There should be no protocol or file-specific
 * data in this file.
 */
#define BASE_STRUCT(memberp, struct_type, member_name) \
   ((struct_type *)((char *)(memberp) - offsetof(struct_type, member_name)))


/*
 * Define globally on which kind of data gpsbabel is working.
 * Important for "file types" that are essentially a communication
 * protocol for a receiver, like the Magellan serial data.
 */
typedef enum {
	trkdata = 1 ,
	wptdata,
	rtedata
} gpsdata_type;

#define NOTHINGMASK		0
#define WPTDATAMASK		1
#define TRKDATAMASK		2
#define	RTEDATAMASK		4

/* mask objective testing */
#define	doing_nothing (global_opts.masked_objective == NOTHINGMASK)
#define	doing_wpts ((global_opts.masked_objective & WPTDATAMASK) == WPTDATAMASK)
#define	doing_trks ((global_opts.masked_objective & TRKDATAMASK) == TRKDATAMASK)
#define	doing_rtes ((global_opts.masked_objective & RTEDATAMASK) == RTEDATAMASK)

typedef struct {
	int synthesize_shortnames;
	int debug_level;
	gpsdata_type objective;
	unsigned int	masked_objective;
	int verbose_status;	/* set by GUI wrappers for status */
	int no_smart_icons;	
} global_options;

extern global_options global_opts;

/*
 * Extended data if waypoint happens to represent a geocache.  This is 
 * totally voluntary data...
 */

typedef enum {
	gt_unknown = 0 ,
	gt_traditional,
	gt_multi,
	gt_virtual,
	gt_letterbox,
	gt_event,
	gt_suprise,
	gt_webcam
} geocache_type;

typedef enum {
	gc_unknown = 0,
	gc_micro,
	gc_other,
	gc_regular,
	gc_large,
	gc_virtual
} geocache_container;

typedef struct {
	int is_html;
	char *utfstring;
} utf_string;

typedef struct {
	geocache_type type;
	geocache_container container;
	int diff; /* (multiplied by ten internally) */
	int terr; /* (likewise) */
	time_t exported;
	char *hint; /* all these UTF8, XML entities removed, May be not HTML. */
	utf_string desc_short;
	utf_string desc_long; 
} geocache_data ;

typedef struct xml_tag {
	char *tagname;
	char *cdata;
	int cdatalen;
	char *parentcdata;
	int parentcdatalen;
	char **attributes;
	struct xml_tag *parent;
	struct xml_tag *sibling;
	struct xml_tag *child;
} xml_tag ;

/*
 * This is a waypoint, as stored in the GPSR.   It tries to not 
 * cater to any specific model or protocol.  Anything that needs to
 * be truncated, edited, or otherwise trimmed should be done on the
 * way to the target.
 */
typedef struct {
	queue Q;			/* Master waypoint q.  Not for use
					   by modules. */

	double latitude;		/* Degrees */
	double longitude; 		/* Degrees */
	double altitude; 		/* Meters. */

	/* 
	 * The "thickness" of a waypoint; adds an element of 3D.  Can be
	 * used to construct rudimentary polygons for, say, airspace 
	 * definitions.   The units are meters.
	 */
	double depth;

	/*
	 * An alarm trigger value that can be considered to be a circle
	 * surrounding a waypoint (or cylinder if depth is also defined).
	 * The units are meters.
	 */
	double proximity;

	/* shortname is a waypoint name as stored in receiver.  It should
	 * strive to be, well, short, and unique.   Enforcing length and
	 * character restrictions is the job of the output.   A typical
	 * minimum length for shortname is 6 characters for NMEA units,
	 * 8 for Magellan and 10 for Vista.   These are only guidelines.
	 */
	char *shortname;	 
	/*
	 * description is typically a human readable description of the 
	 * waypoint.   It may be used as a comment field in some receivers.
	 * These are probably under 40 bytes, but that's only a guideline.
	 */
	char *description;
	/*
	 * notes are relatively long - over 100 characters - prose associated
	 * with the above.   Unlike shortname and description, these are never
	 * used to compute anything else and are strictly "passed through".
	 * Few formats support this.
	 */
	char *notes;
	char *url;
	char *url_link_text;
	int icon_descr_is_dynamic;
	const char *icon_descr;
	time_t creation_time;	/* standardized in UTC/GMT */
	int centiseconds;	/* Optional hundredths of a second. */
	geocache_data gc_data;
	xml_tag *gpx_extras;
	void *extra_data;	/* Extra data added by, say, a filter. */
} waypoint;

typedef struct {
	queue Q;		/* Link onto parent list. */
	queue waypoint_list;	/* List of child waypoints */
	char *rte_name;
	char *rte_desc;
	int rte_num;
	int rte_waypt_ct;		/* # waypoints in waypoint list */
} route_head;

/*
 *  Bounding box information.
 */
typedef struct {
	double max_lat;
	double max_lon;
	double min_lat;
	double min_lon;
} bounds;

typedef void (*ff_init) (char const *);
typedef void (*ff_deinit) (void);
typedef void (*ff_read) (void);
typedef void (*ff_write) (void);

#ifndef DEBUG_MEM
char * get_option(const char *iarglist, const char *argname);
#else
#define DEBUG_PARAMS const char *file, const int line
char *GET_OPTION(const char *iarglist, const char *argname, DEBUG_PARAMS);
#define get_option(iarglist, argname) GET_OPTION(iarglist, argname, __FILE__, __LINE__)
#endif

typedef void (*filter_init) (char const *);
typedef void (*filter_process) (void);
typedef void (*filter_deinit) (void);

typedef void (*waypt_cb) (const waypoint *);
typedef void (*route_hdr)(const route_head *);
typedef void (*route_trl)(const route_head *);
void waypt_add (waypoint *);
waypoint * waypt_dupe (const waypoint *);
waypoint * waypt_new(void);
void waypt_del (waypoint *);
void waypt_free (waypoint *);
void waypt_disp_all(waypt_cb);
void waypt_compute_bounds(bounds *);
void waypt_flush(queue *);
void waypt_flush_all(void);
unsigned int waypt_count(void);
void free_gpx_extras (xml_tag * tag);
void xcsv_read_internal_style(const char *style_buf);
waypoint * find_waypt_by_name(const char *name);

route_head *route_head_alloc(void);
void route_add (waypoint *);
void route_add_wpt(route_head *rte, waypoint *wpt);
void route_del_wpt(route_head *rte, waypoint *wpt);
void route_add_head(route_head *rte);
void track_add_head(route_head *rte);
void route_disp_all(route_hdr, route_trl, waypt_cb);
void track_disp_all(route_hdr, route_trl, waypt_cb);
void route_free (route_head *);
void route_flush( queue *);
void route_flush_all(void);
unsigned int route_waypt_count(void);
unsigned int route_count(void);
unsigned int track_count(void);

/*
 * All shortname functions take a shortname handle as the first arg.
 * This is an opaque pointer.  Callers must not fondle the contents of it.
 */
#ifndef DEBUG_MEM 
char *mkshort (void *, const char *);
void *mkshort_new_handle(void);
#else
char *MKSHORT(void *, const char *, DEBUG_PARAMS);
void *MKSHORT_NEW_HANDLE(DEBUG_PARAMS);
#define mkshort( a, b) MKSHORT(a,b,__FILE__, __LINE__)
#define mkshort_new_handle() MKSHORT_NEW_HANDLE(__FILE__,__LINE__)
#endif
void mkshort_del_handle(void *h);
void setshort_length(void *, int n);
void setshort_badchars(void *, const char *);
void setshort_mustupper(void *, int n);
void setshort_mustuniq(void *, int n);
void setshort_whitespace_ok(void *, int n);

/*
 *  Vmem flags values.
 */
#define VMFL_NOZERO (1 << 0)
typedef struct vmem {
	void *mem;		/* visible memory object */
	size_t size; 		/* allocated size of object */
} vmem_t;
vmem_t 	vmem_alloc(size_t, int flags);
void 	vmem_free(vmem_t*);
void 	vmem_realloc(vmem_t*, size_t);


#define ARGTYPE_UNKNOWN  0
#define ARGTYPE_INT      0x00000001
#define ARGTYPE_FLOAT    0x00000002
#define ARGTYPE_STRING   0x00000003
#define ARGTYPE_BOOL     0x00000004
#define ARGTYPE_FILE     0x00000005
#define ARGTYPE_OUTFILE  0x00000006
#define ARGTYPE_REQUIRED 0x40000000

#define ARGTYPE_TYPEMASK 0x00000fff
#define ARGTYPE_FLAGMASK 0xfffff000

typedef struct arglist {
	char *argstring;
	char **argval;
	char *helpstring;
	long argtype;
} arglist_t;

typedef enum {
	ff_type_file = 1,	/* normal format: useful to a GUI. */
	ff_type_internal,	/* fmt not useful with default options */
	ff_type_serial,		/* format describes a serial protoco (GUI can display port names) */
} ff_type;

/*
 *  Describe the file format to the caller.
 */
typedef struct ff_vecs {
	ff_type type;
	ff_init rd_init;
	ff_init wr_init;
	ff_deinit rd_deinit;
	ff_deinit wr_deinit;
	ff_read read;
	ff_write write;
	arglist_t *args;
} ff_vecs_t;

typedef struct style_vecs {
	const char *name;
	const char *style_buf;
} style_vecs_t;
extern style_vecs_t style_list[];

typedef struct filter_vecs {
	filter_init f_init;
	filter_process f_process;
	filter_deinit f_deinit;
	arglist_t *args;
} filter_vecs_t;

void waypt_init(void);
void route_init(void);
void waypt_disp(const waypoint *);
void fatal(const char *, ...)
#if __GNUC__
	__attribute__ ((__format__ (__printf__, 1, 2)))
#endif
	;
void warning(const char *, ...)
#if __GNUC__
	__attribute__ ((__format__ (__printf__, 1, 2)))
#endif
	;
ff_vecs_t *find_vec(char *, char **);
void disp_vecs(void);
void disp_formats(int version);
void printposn(double c, int is_lat);

filter_vecs_t * find_filter_vec(char *, char **);
void free_filter_vec(filter_vecs_t *);
void disp_filters(int version);
void disp_filter_vecs(void);

#ifndef DEBUG_MEM
void *xcalloc(size_t nmemb, size_t size);
void *xmalloc(size_t size);
void *xrealloc(void *p, size_t s);
void xfree(void *mem);
char *xstrdup(const char *s);
char *xstrndup(const char *s, size_t n);
char *xstrndupt(const char *s, size_t n);
char *xstrappend(char *src, const char *new);
#define xxcalloc(nmemb, size, file, line) xcalloc(nmemb, size)
#define xxmalloc(size, file, line) xmalloc(size)
#define xxrealloc(p, s, file, line) xrealloc(p,s)
#define xxfree(mem, file, line) xfree(mem)
#define xxstrdup(s, file, line) xstrdup(s)
#define xxstrappend(src, new, file, line) xstrappend(src, new)
#else /* DEBUG_MEM */
void *XCALLOC(size_t nmemb, size_t size, DEBUG_PARAMS );
void *XMALLOC(size_t size, DEBUG_PARAMS );
void *XREALLOC(void *p, size_t s, DEBUG_PARAMS );
void XFREE(void *mem, DEBUG_PARAMS );
char *XSTRDUP(const char *s, DEBUG_PARAMS );
char *XSTRNDUP(const char *src, size_t size, DEBUG_PARAMS );
char *XSTRNDUPT(const char *src, size_t size, DEBUG_PARAMS );
char *XSTRAPPEND(char *src, const char *new, DEBUG_PARAMS );
void debug_mem_open();
void debug_mem_output( char *format, ... );
void debug_mem_close();
#define xcalloc(nmemb, size) XCALLOC(nmemb, size, __FILE__, __LINE__)
#define xmalloc(size) XMALLOC(size, __FILE__, __LINE__)
#define xrealloc(p, s) XREALLOC(p,s,__FILE__,__LINE__)
#define xfree(mem) XFREE(mem, __FILE__, __LINE__)
#define xstrdup(s) XSTRDUP(s, __FILE__, __LINE__)
#define xstrndup(s, z) XSTRNDUP(s, z, __FILE__, __LINE__)
#define xstrndupt(s, z) XSTRNDUPT(s, z, __FILE__, __LINE__)
#define xstrappend(src,new) XSTRAPPEND(src, new, __FILE__, __LINE__)
#define xxcalloc XCALLOC
#define xxmalloc XMALLOC
#define xxrealloc XREALLOC
#define xxfree XFREE
#define xxstrdup XSTRDUP
#define xxstrndupt XSTRNDUPT
#define xxstrappend XSTRAPPEND
#endif /* DEBUG_MEM */

FILE *xfopen(const char *fname, const char *type, const char *errtxt);

int case_ignore_strcmp(const char *s1, const char *s2);

char *strsub(char *s, char *search, char *replace);
void rtrim(char *s);
signed int get_tz_offset(void);
time_t current_time(void);
signed int month_lookup(const char *m);
const char *get_cache_icon(const waypoint *waypointp);
char * xml_entitize(const char * str);
char * html_entitize(const char * str);
char * strip_html(const utf_string*);
char * strip_nastyhtml(const char * in);
char * str_utf8_to_cp1252( const char * str );
char * str_utf8_to_ascii( const char * str );

char * rot13( const char *str );

/*
 * PalmOS records like fixed-point numbers, which should be rounded
 * to deal with possible floating-point representation errors.
 */

signed int si_round( double d );

/* 
 * Data types for Palm/OS files.
 */
typedef struct {
	unsigned char data[4];
} pdb_32;

typedef struct {
	unsigned char data[2];
} pdb_16;

typedef struct {
	unsigned char data[8];
} pdb_double;

/*
 * Protypes for Endianness helpers.
 */

signed int be_read16(void *p);
signed int be_read32(void *p);
signed int le_read16(void *p);
signed int le_read32(void *p);
void le_read64(void *dest, const void *src);
void be_write16(void *pp, unsigned i);
void be_write32(void *pp, unsigned i);
void le_write16(void *pp, unsigned i);
void le_write32(void *pp, unsigned i);
double pdb_read_double(void *p);
void pdb_write_double(void *pp, double d);

/*
 * Prototypes for generic conversion routines (util.c).
 */

double ddmm2degrees(double ddmm_val);
double degrees2ddmm(double deg_val);

/*
 * A constant for unknown altitude.   It's tempting to just use zero
 * but that's not very nice for the folks near sea level.
 */
#define unknown_alt -99999999.0

#endif /* gpsbabel_defs_h_included */
