/* 
    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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
#if !NO_EXPAT
#include <expat.h>
static XML_Parser psr;
#endif

static waypoint *wpt_tmp;

FILE *fd;
FILE *ofd;

#define MYNAME "navicache"
#define MY_CBUF 4096

#if NO_EXPAT
void
nav_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded GPX support becuase expat was not installed.\n");
}

void
nav_read(void)
{
}
#else

struct
nc_type_mapping{
	geocache_type type;
	const char *name;
} nc_type_map[] = {
	{ gt_unknown, "unknown" },
	{ gt_traditional, "normal" },
	{ gt_multi, "Multi-part" },
	{ gt_virtual, "Virtual" },
	{ gt_event, "event" }
};

struct
nc_container_mapping{
	geocache_container type;
	const char *name;
} nc_container_map[] = {
	{ gc_other, "Unknown" },
	{ gc_micro, "Micro" },
	{ gc_regular, "Normal" },
	{ gc_large, "Large" },
	{ gc_virtual, "Virtual" }
};

static
geocache_type
nc_mktype(const char *t)
{
	int i;
	int sz = sizeof(nc_type_map) / sizeof(nc_type_map[0]);

	for (i = 0; i < sz; i++) {
		if (0 == case_ignore_strcmp(t, nc_type_map[i].name)) {
			return nc_type_map[i].type;
		}
	}
	return gt_unknown;
}

static
geocache_container
nc_mkcont(const char *t)
{
	int i;
	int sz = sizeof(nc_container_map) / sizeof(nc_container_map[0]);

	for (i = 0; i < sz; i++) {
		if (0 == case_ignore_strcmp(t, nc_container_map[i].name)) {
			return nc_container_map[i].type;
		}
	}
	return gc_unknown;
}

static void
nav_start(void *data, const char *el, const char **attr)
{
	if (0 == strcmp(el, "CacheDetails")) {
		wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
		const char **ap;
		for (ap = attr; *ap; ap+=2) {
			if (0 == strcmp(ap[0], "cache_id")) {
				wpt_tmp->shortname = xstrdup(ap[1]);
			} else
			if (0 == strcmp(ap[0], "name")) {
				wpt_tmp->description = xstrdup(ap[1]);
			} else
			if (0 == strcmp(ap[0], "latitude")) {
				sscanf(ap[1], "%lf", 
				&wpt_tmp->position.latitude.degrees);
			} else
			if (0 == strcmp(ap[0], "longitude")) {
				sscanf(ap[1], "%lf", 
				&wpt_tmp->position.longitude.degrees);
			} else
			if (0 == strcmp(ap[0], "longitude")) {
				sscanf(ap[1], "%lf", 
				&wpt_tmp->position.longitude.degrees);
			} else
			if (0 == strcmp(ap[0], "difficulty")) {
				float x;
				sscanf(ap[1], "%f", &x);
				wpt_tmp->gc_data.diff = x * 10;
			} else
			if (0 == strcmp(ap[0], "terrain")) {
				float x;
				sscanf(ap[1], "%f", &x);
				wpt_tmp->gc_data.terr = x * 10;
			} else
			if (0 == strcmp(ap[0], "cache_type")) {
	                        wpt_tmp->gc_data.type = nc_mktype(ap[1]);
			} else
			if (0 == strcmp(ap[0], "cache_size")) {
	                        wpt_tmp->gc_data.container = nc_mkcont(ap[1]);
			} 
		}
		waypt_add(wpt_tmp);
	}
}

static void
nav_end(void *data, const char *el)
{
}

void
nav_rd_init(const char *fname)
{
	fd = fopen(fname, "r");
	if (fd == NULL) {
		fatal(MYNAME ":Cannot open %s for reading\n", fname);
	}

	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ":Cannot create XML parser\n");
	}

	XML_SetElementHandler(psr, nav_start, nav_end);
}

void
nav_read(void)
{
	int len;
	char buf[MY_CBUF];
	
	while ((len = fread(buf, 1, sizeof(buf), fd))) {
		if (!XML_Parse(psr, buf, len, feof(fd))) {
			fatal(MYNAME ":Parse error at %d: %s\n", 
				XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}

	XML_ParserFree(psr);
}

#endif

void
nav_rd_deinit(void)
{
	fclose(fd);
}

void
nav_wr_init(const char *fname)
{
	fatal(MYNAME ": Does not support writing Navicache files.\n");
	ofd = fopen(fname, "w");
	if (ofd == NULL) {
		fatal(MYNAME ":Cannot open '%s' for writing\n", fname);
	}
}

void
nav_wr_deinit(void)
{
	fclose(ofd);
}

static void
nav_waypt_pr(const waypoint *waypointp)
{
}

void
nav_write(void)
{
}

ff_vecs_t navicache_vecs = {
	nav_rd_init,	
	nav_wr_init,	
	nav_rd_deinit,
	nav_wr_deinit,
	nav_read,
	nav_write,
	NULL
};
