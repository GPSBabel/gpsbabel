/*
    Copyright (C) 2002-2005 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <QtCore/QTextCodec>

#include "defs.h"
#include "filterdefs.h"
#include "cet.h"
#include "cet_util.h"
#include "csv_util.h"
#include "inifile.h"
#include "session.h"
#include "src/core/usasciicodec.h"
#include <ctype.h>
#include <locale.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#define MYNAME "main"

void signal_handler(int sig);

typedef struct arg_stack_s {
  int argn;
  int argc;
  char** argv;
  struct arg_stack_s* prev;
} arg_stack_t;

static arg_stack_t
* push_args(arg_stack_t* stack, const int argn, const int argc, char* argv[])
{
  arg_stack_t* res = (arg_stack_t*) xmalloc(sizeof(*res));

  res->prev = stack;
  res->argn = argn;
  res->argc = argc;
  res->argv = (char**)argv;

  return res;
}

static arg_stack_t
* pop_args(arg_stack_t* stack, int* argn, int* argc, char** argv[])
{
  arg_stack_t* res;
  char** argv2 = *argv;
  int i;

  if (stack == NULL) {
    fatal("main: Invalid point in time to call 'pop_args'\n");
  }

  for (i = 0; i < *argc; i++) {
    xfree(argv2[i]);
  }
  xfree(*argv);

  *argn = stack->argn;
  *argc = stack->argc;
  *argv = stack->argv;

  res = stack->prev;
  xfree(stack);

  return res;
}

static void
load_args(const char* filename, int* argc, char** argv[])
{
  gbfile* fin;
  char* str, *line = NULL;
  int argc2;
  char** argv2;

  fin = gbfopen(filename, "r", "main");
  while ((str = gbfgetstr(fin))) {
    str = lrtrim(str);
    if ((*str == '\0') || (*str == '#')) {
      continue;
    }

    if (line == NULL) {
      line = xstrdup(str);
    } else {
      char* tmp;
      xasprintf(&tmp, "%s %s", line, str);
      xfree(line);
      line = tmp;
    }
  }
  gbfclose(fin);

  argv2 = (char**) xmalloc(2 * sizeof(*argv2));
  argv2[0] = xstrdup(*argv[0]);
  argc2 = 1;

  str = csv_lineparse(line, " ", "\"", 0);
  while (str != NULL) {
    argv2 = (char**) xrealloc(argv2, (argc2 + 2) * sizeof(*argv2));
    argv2[argc2] = xstrdup(str);
    argc2++;
    str = csv_lineparse(NULL, " ", "\"", 0);
  }
  xfree(line);

  argv2[argc2] = NULL;

  *argc = argc2;
  *argv = argv2;
}

static void
usage(const char* pname, int shorter)
{
  printf("GPSBabel Version %s.  http://www.gpsbabel.org\n\n",
         gpsbabel_version);
  printf(
    "Usage:\n"
    "    %s [options] -i INTYPE -f INFILE [filter] -o OUTTYPE -F OUTFILE\n"
    "    %s [options] -i INTYPE -o OUTTYPE INFILE [filter] OUTFILE\n"
    "\n"
    "    Converts GPS route and waypoint data from one format type to another.\n"
    "    The input type and filename are specified with the -i INTYPE\n"
    "    and -f INFILE options. The output type and filename are specified\n"
    "    with the -o OUTTYPE and -F OUTFILE options.\n"
    "    If '-' is used for INFILE or OUTFILE, stdin or stdout will be used.\n"
    "\n"
    "    In the second form of the command, INFILE and OUTFILE are the\n"
    "    first and second positional (non-option) arguments.\n"
    "\n"
    "    INTYPE and OUTTYPE must be one of the supported file types and\n"
    "    may include options valid for that file type.  For example:\n"
    "      'gpx', 'gpx,snlen=10' and 'ozi,snlen=10,snwhite=1'\n"
    "    (without the quotes) are all valid file type specifications.\n"
    "\n"
    "Options:\n"
    "    -p               Preferences file (gpsbabel.ini)\n"
    "    -s               Synthesize shortnames\n"
    "    -r               Process route information\n"
    "    -t               Process track information\n"
    "    -T               Process realtime tracking information\n"
    "    -w               Process waypoint information [default]\n"
    "    -b               Process command file (batch mode)\n"
    "    -c               Character set for next operation\n"
    "    -N               No smart icons on output\n"
    "    -x filtername    Invoke filter (placed between inputs and output) \n"
    "    -D level         Set debug level [%d]\n"
    "    -l               Print GPSBabel builtin character sets and exit\n"
    "    -h, -?           Print detailed help and exit\n"
    "    -V               Print GPSBabel version and exit\n"
    "\n"
    , pname
    , pname
    , global_opts.debug_level
  );
  if (shorter) {
    printf("\n\n[Press enter]");
    fgetc(stdin);
  } else {
    printf("File Types (-i and -o options):\n");
    disp_vecs();
    printf("\nSupported data filters:\n");
    disp_filter_vecs();
  }
}

static void
spec_usage(const char* vec)
{
  printf("\n");
  disp_vec(vec);
  disp_filter_vec(vec);
  printf("\n");
}

static void
print_extended_info(void)
{
  printf(

#if !ZLIB_INHIBITED	/* Note polarity inverted here */
    "ZLIB_ENABLED "
#endif

#if FILTERS_ENABLED
    "FILTERS_ENABLED "
#endif

#if CSVFMTS_ENABLED
    "CSVFMTS_ENABLED "
#endif

#if PDBFMTS_ENABLED
    "PDBFMTS_ENABLED "
#endif

#if SHAPELIB_ENABLED
    "SHAPELIB_ENABLED "
#endif

#if defined CET_WANTED
    "CET_ENABLED "
#endif
    "\n");
}

int
main(int argc, char* argv[])
{
  int c;
  int argn;
  ff_vecs_t* ivecs = NULL;
  ff_vecs_t* ovecs = NULL;
  filter_vecs_t* fvecs = NULL;
  char* fname = NULL;
  char* ofname = NULL;
  const char* ivec_opts = NULL;
  const char* ovec_opts = NULL;
  char* fvec_opts = NULL;
  int opt_version = 0;
  int did_something = 0;
  const char* prog_name = argv[0]; /* argv is modified during processing */
  queue* wpt_head_bak, *rte_head_bak, *trk_head_bak;	/* #ifdef UTF8_SUPPORT */
  signed int wpt_ct_bak, rte_ct_bak, trk_ct_bak;	/* #ifdef UTF8_SUPPORT */
  arg_stack_t* arg_stack = NULL;
  (void) new gpsbabel::UsAsciiCodec(); /* make sure a US-ASCII codec is available */

#if (QT_VERSION < QT_VERSION_CHECK(5, 0, 0))
  // Qt 5.0 uses QString::fromUtf8 to convert from character pointers
  // and QBytreArrays to QStrings while previous version of Qt used
  // QString::fromAscii.  QString::fromAscii used the codec set
  // by QTextCode::setCodecForCStrings.
  // This makes the converstion consistent between Qt4 and Qt5.
  QTextCodec::setCodecForCStrings(QTextCodec::codecForName("UTF-8"));
#endif

  // The first invocation of QTextCodec::codecForLocale() may result in LC_ALL being set to the native environment
  // as opposed to the initial default "C" locale.
  // This was demonstrated with Qt5 on Mac OS X.
#ifdef DEBUG_LOCALE
  printf("%s\n",setlocale(LC_ALL, NULL));
#endif
  (void) QTextCodec::codecForLocale();
#ifdef DEBUG_LOCALE
  printf("%s\n",setlocale(LC_ALL, NULL));
#endif
  // As recommended in QCoreApplication reset the locale to the default.
  // Note the documentation says to set LC_NUMERIC, but QCoreApplicationPrivate::initLocale()
  // actually sets LC_ALL.
  // Perhaps we should restore LC_ALL instead of only LC_NUMERIC.
  if (strcmp(setlocale(LC_NUMERIC,0), "C") != 0) {
#ifdef DEBUG_LOCALE
    printf("Resetting LC_NUMERIC\n");
#endif
    setlocale(LC_NUMERIC,"C");
#ifdef DEBUG_LOCALE
    printf("%s\n",setlocale(LC_ALL, NULL));
#endif
  }

  global_opts.objective = wptdata;
  global_opts.masked_objective = NOTHINGMASK;	/* this makes the default mask behaviour slightly different */
  global_opts.charset = NULL;
  global_opts.charset_name = NULL;
  global_opts.inifile = NULL;

  gpsbabel_now = time(NULL);			/* gpsbabel startup-time */
  gpsbabel_time = current_time().toTime_t();			/* same like gpsbabel_now, but freezed to zero during testo */

#ifdef DEBUG_MEM
  debug_mem_open();
  debug_mem_output("command line: ");
  for (argn = 1; argn < argc; argn++) {
    debug_mem_output("%s ", argv[argn]);
  }
  debug_mem_output("\n");
#endif

  if (gpsbabel_time != 0) {	/* within testo ? */
    global_opts.inifile = inifile_init(NULL, MYNAME);
  }

  init_vecs();
  init_filter_vecs();
  cet_register();
  session_init();
  waypt_init();
  route_init();

  if (argc < 2) {
    usage(argv[0],1);
    exit(0);
  }

  /*
   * Open-code getopts since POSIX-impaired OSes don't have one.
   */
  argn = 1;
  while (argn < argc) {
    char* optarg;

    if (argv[argn][0] != '-') {
      break;
    }
    if (argv[argn][1] == '-') {
      break;
    }

    if (argv[argn][1] == 'V') {
      printf("\nGPSBabel Version %s\n\n", gpsbabel_version);
      if (argv[argn][2] == 'V') {
        print_extended_info();
      }
      exit(0);
    }

    if (argv[argn][1] == '?' || argv[argn][1] == 'h') {
      if (argn < argc-1) {
        spec_usage(argv[argn+1]);
      } else {
        usage(argv[0],0);
      }
      exit(0);
    }

    c = argv[argn][1];

    if (argv[argn][2]) {
      opt_version = atoi(&argv[argn][2]);
    }

    switch (c) {
    //case 'c':
    //  optarg = argv[argn][2] ? argv[argn]+2 : argv[++argn];
    //  cet_convert_init(optarg, 1);
    //  break;
    case 'i':
      optarg = argv[argn][2]
               ? argv[argn]+2 : argv[++argn];
      ivecs = find_vec(optarg, &ivec_opts);
      if (ivecs == NULL) {
        fatal("Input type '%s' not recognized\n", optarg);
      }
      break;
    case 'o':
      if (ivecs == NULL) {
        warning("-o appeared before -i.   This is probably not what you want to do.\n");
      }
      optarg = argv[argn][2]
               ? argv[argn]+2 : argv[++argn];
      ovecs = find_vec(optarg, &ovec_opts);
      if (ovecs == NULL) {
        fatal("Output type '%s' not recognized\n", optarg);
      }
      break;
    case 'f':
      optarg = argv[argn][2]
               ? argv[argn]+2 : argv[++argn];
      fname = optarg;
      if (fname == NULL) {
        fatal("No file or device name specified.\n");
      }
      if (ivecs == NULL) {
        fatal("No valid input type specified\n");
      }
      if (ivecs->rd_init == NULL) {
        fatal("Format does not support reading.\n");
      }
      if (global_opts.masked_objective & POSNDATAMASK) {
        did_something = 1;
        break;
      }
      /* simulates the default behaviour of waypoints */
      if (doing_nothing) {
        global_opts.masked_objective |= WPTDATAMASK;
      }

      cet_convert_init(ivecs->encode, ivecs->fixed_encode);	/* init by module vec */

      start_session(ivecs->name, fname);
      ivecs->rd_init(fname);
      ivecs->read();
      ivecs->rd_deinit();

      cet_convert_strings(global_opts.charset, NULL, NULL);
      cet_convert_deinit();

      did_something = 1;
      break;
    case 'F':
      optarg = argv[argn][2]
               ? argv[argn]+2 : argv[++argn];
      ofname = optarg;
      if (ofname == NULL) {
        fatal("No output file or device name specified.\n");
      }
      if (ovecs && (!(global_opts.masked_objective & POSNDATAMASK))) {
        /* simulates the default behaviour of waypoints */
        if (doing_nothing) {
          global_opts.masked_objective |= WPTDATAMASK;
        }
        if (ovecs->wr_init == NULL) {
          fatal("Format does not support writing.\n");
        }

        cet_convert_init(ovecs->encode, ovecs->fixed_encode);

        wpt_ct_bak = -1;
        rte_ct_bak = -1;
        trk_ct_bak = -1;
        rte_head_bak = trk_head_bak = NULL;

        ovecs->wr_init(ofname);

        if (global_opts.charset != &cet_cs_vec_utf8) {
          /*
           * Push and pop verbose_status so
                          		 * we don't get dual progress bars
           * when doing characterset
           * transformation.
           */
          int saved_status = global_opts.verbose_status;
          global_opts.verbose_status = 0;
          waypt_backup(&wpt_ct_bak, &wpt_head_bak);
          route_backup(&rte_ct_bak, &rte_head_bak);
          track_backup(&trk_ct_bak, &trk_head_bak);

          cet_convert_strings(NULL, global_opts.charset, NULL);
          global_opts.verbose_status = saved_status;
        }

        ovecs->write();
        ovecs->wr_deinit();

        cet_convert_deinit();

        if (wpt_ct_bak != -1) {
          waypt_restore(wpt_ct_bak, wpt_head_bak);
        }
        if (rte_ct_bak != -1) {
          route_restore(rte_head_bak);
          xfree(rte_head_bak);
        }
        if (trk_ct_bak != -1) {
          track_restore(trk_head_bak);
          xfree(trk_head_bak);
        }
      }
      break;
    case 's':
      global_opts.synthesize_shortnames = 1;
      break;
    case 't':
      global_opts.objective = trkdata;
      global_opts.masked_objective |= TRKDATAMASK;
      break;
    case 'w':
      global_opts.objective = wptdata;
      global_opts.masked_objective |= WPTDATAMASK;
      break;
    case 'r':
      global_opts.objective = rtedata;
      global_opts.masked_objective |= RTEDATAMASK;
      break;
    case 'T':
      global_opts.objective = posndata;
      global_opts.masked_objective |= POSNDATAMASK;
      break;
    case 'N':
#if 0
      /* This option is silently eaten for compatibilty.  -N is now the
       * default.  If you want the old behaviour, -S allows you to individually
       * turn them on.  The -N option will be removed in 2008.
       */

      switch (argv[argn][2]) {
      case 'i':
        global_opts.no_smart_icons = 1;
        break;
      case 'n':
        global_opts.no_smart_names = 1;
        break;
      default:
        global_opts.no_smart_names = 1;
        global_opts.no_smart_icons = 1;
        break;
      }
#endif

      break;
    case 'S':
      switch (argv[argn][2]) {
      case 'i':
        global_opts.smart_icons = 1;
        break;
      case 'n':
        global_opts.smart_names = 1;
        break;
      default:
        global_opts.smart_icons = 1;
        global_opts.smart_names = 1;
        break;
      }
      break;
    case 'x':
      optarg = argv[argn][2]
               ? argv[argn]+2 : argv[++argn];
      fvecs = find_filter_vec(optarg, &fvec_opts);

      if (fvecs) {
        if (fvecs->f_init) {
          fvecs->f_init(fvec_opts);
        }
        fvecs->f_process();
        if (fvecs->f_deinit) {
          fvecs->f_deinit();
        }
        free_filter_vec(fvecs);
      }  else {
        fatal("Unknown filter '%s'\n",optarg);
      }
      break;
    case 'D':
      optarg = argv[argn][2]
               ? argv[argn]+2 : argv[++argn];
      global_opts.debug_level = atoi(optarg);
      /*
       * When debugging, announce version.
       */
      if (global_opts.debug_level > 0)  {
        warning("GPSBabel Version: %s \n", gpsbabel_version);
      }

      break;
    /*
     * Undocumented '-vs' option for GUI wrappers.
     */
    case 'v':
      switch (argv[argn][2]) {
      case 's':
        global_opts.verbose_status = 1;
        break;
      case 'S':
        global_opts.verbose_status = 2;
        break;
      }
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
      usage(argv[0],0);
      exit(0);
    case 'p':
      optarg = argv[argn][2] ? argv[argn]+2 : argv[++argn];
      inifile_done(global_opts.inifile);
      if (!optarg || strcmp(optarg, "") == 0) {	/* from GUI to preserve inconsistent options */
        global_opts.inifile = NULL;
      } else {
        global_opts.inifile = inifile_init(optarg, MYNAME);
      }
      break;
    case 'b':
      optarg = argv[argn][2] ? argv[argn]+2 : argv[++argn];
      arg_stack = push_args(arg_stack, argn, argc, argv);
      load_args(optarg, &argc, &argv);
      if (argc == 0) {
        arg_stack = pop_args(arg_stack, &argn, &argc, &argv);
      } else {
        argn = 0;
      }
      break;

    default:
      fatal("Unknown option '%s'.\n", argv[argn]);
      break;
    }

    if ((argn+1 >= argc) && (arg_stack != NULL)) {
      arg_stack = pop_args(arg_stack, &argn, &argc, &argv);
    }
    argn++;
  }

  /*
   * Allow input and output files to be specified positionally
   * as well.  This is the typical command line format.
   */
  argc -= argn;
  argv += argn;
  if (argc > 2) {
    fatal("Extra arguments on command line\n");
  } else if (argc && ivecs) {
    did_something = 1;
    /* simulates the default behaviour of waypoints */
    if (doing_nothing) {
      global_opts.masked_objective |= WPTDATAMASK;
    }

    cet_convert_init(ivecs->encode, 1);

    start_session(ivecs->name, argv[0]);
    if (ivecs->rd_init == NULL) {
      fatal("Format does not support reading.\n");
    }
    ivecs->rd_init(argv[0]);
    ivecs->read();
    ivecs->rd_deinit();

    cet_convert_strings(global_opts.charset, NULL, NULL);
    cet_convert_deinit();

    if (argc == 2 && ovecs) {
      cet_convert_init(ovecs->encode, 1);
      cet_convert_strings(NULL, global_opts.charset, NULL);

      if (ovecs->wr_init == NULL) {
        fatal("Format does not support writing.\n");
      }

      ovecs->wr_init(argv[1]);
      ovecs->write();
      ovecs->wr_deinit();

      cet_convert_deinit();
    }
  } else if (argc) {
    usage(prog_name,0);
    exit(0);
  }
  if (ovecs == NULL) {
    /*
     * Push and pop verbose_status so we don't get dual
     * progress bars when doing characterset transformation.
     */
    int saved_status = global_opts.verbose_status;
    global_opts.verbose_status = 0;
    cet_convert_init(CET_CHARSET_ASCII, 1);
    cet_convert_strings(NULL, global_opts.charset, NULL);
    waypt_disp_all(waypt_disp);
    global_opts.verbose_status = saved_status;
  }

  /*
   * This is very unlike the rest of our command sequence.
   * If we're doing realtime position tracking, we enforce that
   * we're not doing anything else and we just bounce between
   * the special "read position" and "write position" vectors
   * in our most recent vecs.
   */
  if (global_opts.masked_objective & POSNDATAMASK) {

    if (!ivecs) {
      fatal("Realtime tracking (-T) requires an input type (-t)i such as Garmin or NMEA.\n");
    }

    if (!ivecs->position_ops.rd_position) {
      fatal("Realtime tracking (-T) is not suppored by this input type.\n");
    }


    if (ivecs->position_ops.rd_init) {
      if (!fname) {
        fatal("An input file (-f) must be specified.\n");
      }
      start_session(ivecs->name, fname);
      ivecs->position_ops.rd_init(fname);
    }

    if (global_opts.masked_objective & ~POSNDATAMASK) {
      fatal("Realtime tracking (-T) is exclusive of other modes.\n");
    }

    if (ovecs) {
      if (!ovecs->position_ops.wr_position) {
        fatal("This output format does not support output of realtime positioning.\n");
      }
    }

    if (signal(SIGINT, signal_handler) == SIG_ERR) {
      fatal("Couldn't install the exit signal handler.\n");
    }

    if (ovecs && ovecs->position_ops.wr_init) {
      ovecs->position_ops.wr_init(ofname);
    }

    tracking_status.request_terminate = 0;
    while (!tracking_status.request_terminate) {
      Waypoint* wpt;

      wpt = ivecs->position_ops.rd_position(&tracking_status);

      if (tracking_status.request_terminate) {
        if (wpt) {
          delete wpt;
        }
        break;
      }
      if (wpt) {
        if (ovecs) {
//					ovecs->position_ops.wr_init(ofname);
          ovecs->position_ops.wr_position(wpt);
//					ovecs->position_ops.wr_deinit();
        } else {
          /* Just print to screen */
          waypt_disp(wpt);
        }
        delete wpt;
      }
    }
    if (ivecs->position_ops.rd_deinit) {
      ivecs->position_ops.rd_deinit();
    }
    if (ovecs && ovecs->position_ops.wr_deinit) {
      ovecs->position_ops.wr_deinit();
    }
    exit(0);
  }


  if (!did_something) {
    fatal("Nothing to do!  Use '%s -h' for command-line options.\n", prog_name);
  }

  cet_deregister();
  waypt_flush_all();
  route_flush_all();
  session_exit();
  exit_vecs();
  exit_filter_vecs();
  inifile_done(global_opts.inifile);

#ifdef DEBUG_MEM
  debug_mem_close();
#endif
  exit(0);
}

void signal_handler(int sig)
{
  (void)sig;
  tracking_status.request_terminate = 1;
}
