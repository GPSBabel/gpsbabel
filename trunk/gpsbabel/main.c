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


#include "defs.h"
#include <ctype.h>

global_options global_opts;

static void
usage(const char *pname)
{
	printf("GPSBabel Version %s.  http://gpsbabel.sourceforge.net\n\n",
			VERSION );
	printf(
"Usage:\n"
"	%s [options] -i INTYPE -f INFILE -o OUTTYPE -F OUTFILE\n"
"	%s [options] -i INTYPE -o OUTTYPE INFILE [OUTFILE]\n"
"\n"
"	Converts GPS route and waypoint data from one format type to another.\n"
"	The input type and filename are specified with the -i INTYPE\n"
"	and -f INFILE options. The output type and filename are specified\n"
"	with the -o OUTTYPE and -F OUTFILE options.\n"
"\n"
"	In the second form of the command, INFILE and OUTFILE are the\n"
"	first and second positional (non-option) arguments.\n"
"\n"
"Options:\n"
"	-s		Synthesize shortnames\n"
"	-r		Process route information\n"
"	-t		Process track information\n"
"	-w		Process waypoint information [default]\n"
"	-x filtername	Invoke filter\n"
"	-D level	Set debug level [%d]\n"
"\n"
"File Types (-i and -o options):\n"
	, pname
	, pname
	, global_opts.debug_level
	);

	disp_vecs();
	printf("\nSupported data filters:\n");
	disp_filter_vecs();
}



int
main(int argc, char *argv[])
{
	int c;
	int argn;
	ff_vecs_t *ivecs = NULL;
	ff_vecs_t *ovecs = NULL;
	filter_vecs_t *fvecs = NULL;
	char *fname = NULL;
	char *ofname = NULL;
	char *ivec_opts = NULL;
	char *ovec_opts = NULL;
	char *fvec_opts = NULL;
	int opt_version = 0;

	global_opts.objective = wptdata;

#ifdef DEBUG_MEM
	debug_mem_open();
	debug_mem_output( "command line: " );
	for ( argn = 1; argn < argc; argn++ ) {
		debug_mem_output( "%s ", argv[argn] );
	}
	debug_mem_output( "\n" );
#endif
	
	waypt_init();
	route_init();

	/*
	 * Open-code getopts since POSIX-impaired OSes don't have one.
	 */
	for (argn = 1; argn < argc; argn++) {
		char *optarg;

		if (argv[argn][0] != '-') {
			break;
		}
		if (argv[argn][1] == '-') {
			break;
		}

		if (argv[argn][1] == '?' || argv[argn][1] == 'h') {
			usage(argv[0]);
			exit(0);
		}

		c = argv[argn][1];

		if (argv[argn][2]) {
			opt_version = atoi(&argv[argn][2]);
		}

		switch (c) {
			case 'i': 
				optarg = argv[argn][2]
					? argv[argn]+2 : argv[++argn];
				ivecs = find_vec(optarg, &ivec_opts);
				break;
			case 'o':
				optarg = argv[argn][2]
					? argv[argn]+2 : argv[++argn];
				ovecs = find_vec(optarg, &ovec_opts);
				break;
			case 'f':
				optarg = argv[argn][2]
					? argv[argn]+2 : argv[++argn];
				fname = optarg;
				if (ivecs == NULL) {
					fatal ("No valid input type specified\n");
				}
				ivecs->rd_init(fname);
				ivecs->read();
				ivecs->rd_deinit();
				break;
			case 'F':
				optarg = argv[argn][2]
					? argv[argn]+2 : argv[++argn];
				ofname = optarg;
				if (ovecs) {
					ovecs->wr_init(ofname);
					ovecs->write();
					ovecs->wr_deinit();
				}
				break;
			case 's':
				global_opts.synthesize_shortnames = 1;
				break;
			case 't':
				global_opts.objective = trkdata;
				break;
			case 'w':
				global_opts.objective = wptdata;
				break;
			case 'r':
				global_opts.objective = rtedata;
				break;
 			case 'x':
				optarg = argv[argn][2]
					? argv[argn]+2 : argv[++argn];
 				fvecs = find_filter_vec(optarg, &fvec_opts);

 				if (fvecs) {
 					fvecs->f_init(fvec_opts);
 					fvecs->f_process();
 					fvecs->f_deinit();
					free_filter_vec(fvecs);
 				}  else {
					fatal("Unknown filter '%s'\n",optarg);
				}
 				break;
			case 'D':
				optarg = argv[argn][2]
					? argv[argn]+2 : argv[++argn];
				global_opts.debug_level = atoi(optarg);
				break;
				/*
				 * DOS-derived systems will need to escape
				 * this as -^^.
				 */
			case '^':
				disp_formats(opt_version);
				exit(0);
 			case '%':
 				disp_filters(opt_version);
  				exit(0);
			case 'h':
			case '?':
				usage(argv[0]);
				exit(0);
		}
	}

	/*
	 * Allow input and output files to be specified positionally
	 * as well.  This is the typical command line format.
	 */
	argc -= argn;
	argv += argn;
	if (argc > 2) {
		fatal ("Extra arguments on command line\n");
	}
	else if (argc && ivecs) {
		ivecs->rd_init(argv[0]);
		ivecs->read();
		ivecs->rd_deinit();
		if (argc == 2 && ovecs) {
			ovecs->wr_init(argv[1]);
			ovecs->write();
			ovecs->wr_deinit();
		}
	}
	else if (argc) {
		usage(argv[0]);
		exit(0);
	}

	if (ovecs == NULL)
		waypt_disp_all(waypt_disp);

	waypt_flush_all();
	route_flush_all();

#ifdef DEBUG_MEM
	debug_mem_close();
#endif
	
	exit(0);
}
