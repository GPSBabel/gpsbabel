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

global_options global_opts;

void
usage(const char *pname)
{
	printf("Usage: %s [-s] -i <INPUT_FILE_TYPE> -f <INPUT_FILE> -o <OUT FTYPE> -F <OUTPUT_FILE>\n", pname);
	printf("Supported file types:\n");
	disp_vecs();
}



int
main(int argc, char *argv[])
{
	int c;
	int argn;
	ff_vecs_t *ivecs = NULL;
	ff_vecs_t *ovecs = NULL;
	char *fname = NULL;
	char *ofname = NULL;

	waypt_init();
	route_init();

	/*
	 * Open-code getopts since POSIX-impaired OSes don't have one.
	 */
	for (argn = 1; argn < argc; argn++) {
		char *optarg;

		if (argv[argn][0] != '-') {
			fatal ("argument '%s' not understood",argv[argn]);
		}

		if (argv[argn][1] == '?' || argv[argn][1] == 'h') {
			usage(argv[0]);
			exit(0);
		}

		c = argv[argn][1];
		optarg = argv[argn+1];

		switch (c) {
			case 'i': 
				ivecs = find_vec(optarg);
				argn++;
				break;
			case 'o':
				ovecs = find_vec(optarg);
				argn++;
				break;
			case 'f':
				fname = optarg;
				argn++;
				if (ivecs == NULL) {
					fatal ("No valid input type specified");
				}
				ivecs->rd_init(fname);
				ivecs->read();
				break;
			case 'F':
				ofname = optarg;
				argn++;
				if (ovecs) {
					ovecs->wr_init(ofname);
					ovecs->write();
				}
				break;
			case 's':
				global_opts.synthesize_shortnames = 1;
				break;
			case 'D':
				global_opts.debug_level = atoi(optarg);
				argn++;
				break;
			case 'h':
			case '?':
				usage(argv[0]);
				exit(0);
		}
	}

	if (ovecs == NULL)
		waypt_disp_all(waypt_disp);

	exit(0);
}
