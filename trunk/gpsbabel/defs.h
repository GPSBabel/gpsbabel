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

#include <time.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "queue.h"

/*
 * Common definitions.   There should be no protocol or file-specific
 * data in this file.
 */

/*
 * A coordinate in space.
 */
typedef struct {
	double degrees;
} coord;


/*
 * An altitude is essentially a coordinate along only the Z axis.
 */

typedef struct {
	double altitude_meters;
} altitude;


/*
 * A triplet of the coordinates along the three axes describes
 * a position.
 */
typedef struct {
	coord latitude;
	coord longitude;
	altitude altitude;
} position;


/*
 * This is a waypoint, as stored in the GPSR.   It tries to not 
 * cater to any specific model or protocol.  Anything that needs to
 * be truncated, edited, or otherwise trimmed should be done on the
 * way to the target.
 */
typedef struct {
	queue Q;
	position position;
	time_t time_created; 
	char *shortname;
	char *description;
	char *url;
	char *url_link_text;
	const char *icon_descr;
	time_t creation_time;
} waypoint;

typedef void (*ff_init) (char const *);
typedef void (*ff_deinit) (void);
typedef void (*ff_read) (void);
typedef void (*ff_write) (void);

typedef void (*waypt_cb) (waypoint *);
void waypt_add (waypoint *);
void route_add (waypoint *);
void waypt_disp_all(waypt_cb);
unsigned int waypt_count(void);
void fprintdms(FILE *, coord *, int);

typedef struct ff_vecs {
	ff_init rd_init;
	ff_init wr_init;
	ff_deinit rd_deinit;
	ff_deinit wr_deinit;
	ff_read read;
	ff_write write;
} ff_vecs_t;

void waypt_init(void);
void route_init(void);
void waypt_disp(waypoint *);
void fatal(const char *, ...);
ff_vecs_t *find_vec(char *);

void printposn(coord *c, int is_lat);

/* 
 * Data types for Palm/OS files.
 */
typedef struct {
	unsigned char data[4];
} pdb_32;

typedef struct {
	unsigned char data[2];
} pdb_16;

/*
 * Protypes for Palm/OS helpers.
 */

int pdb_read2(pdb_16 *p);
int pdb_read4(pdb_32 *p);
void pdb_write2(pdb_16 *pp, unsigned i);
void pdb_write4(pdb_32 *pp, unsigned i);
