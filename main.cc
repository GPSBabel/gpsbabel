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

#include <QtCore/QCoreApplication>
#include <QtCore/QStack>
#include <QtCore/QString>
#include <QtCore/QTextCodec>
#include <QtCore/QTextStream>

#include "cet.h"
#include "cet_util.h"
#include "csv_util.h"
#include "defs.h"
#include "filterdefs.h"
#include "inifile.h"
#include "session.h"
#include "src/core/file.h"
#include "src/core/usasciicodec.h"
#include <cctype>
#include <clocale>
#include <cstdio>
#include <cstdlib>
#include <csignal>
#ifdef AFL_INPUT_FUZZING
#include "argv-fuzz-inl.h"
#endif

#define MYNAME "main"

void signal_handler(int sig);

class QargStackElement
{
public:
  int argn;
  QStringList qargs;

public:
  QargStackElement()
  = default;

  QargStackElement(int p_argn, QStringList p_qargs)
  {
    argn = p_argn;
    qargs = p_qargs;
  }
};

static QStringList
load_args(const QString& filename, const QString& arg0)
{
  QString line;
  QString str;
  QStringList qargs(arg0);

// Use a QTextStream to read the batch file.
// By default that will use codecForLocale().
// By default Automatic Unicode detection is enabled.
  gpsbabel::File file(filename);
  file.open(QFile::ReadOnly);
  QTextStream stream(&file);
  while (!(str = stream.readLine()).isNull()) {
    str = str.trimmed();
    if ((str.isEmpty()) || (str.at(0).toLatin1() == '#')) {
      continue;
    }
    if (line.isEmpty()) {
      line = str;
    } else {
      line.append(' ');
      line.append(str);
    }
  }
  file.close();

  // We use csv_lineparse to protect quoted strings, otherwise
  // we could just split on blank and eliminate the round trip
  // to 8 bit characters and back.
  // TODO: move csv processing to Qt, eliminating the need to go
  // back to 8 bit encoding, which is shaky for encoding like utf8
  // that have multibyte characters.
  char* cbuff = xstrdup(CSTR(line));

  char* cstr = csv_lineparse(cbuff, " ", "\"", 0);
  while (cstr != NULL) {
    qargs.append(QString::fromUtf8(cstr));
    cstr = csv_lineparse(NULL, " ", "\"", 0);
  }

  xfree(cbuff);
  return (qargs);
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
    "    -N               No smart icons on output\n"
    "    -x filtername    Invoke filter (placed between inputs and output) \n"
    "    -D level         Set debug level [%d]\n"
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
print_extended_info()
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

#if SHAPELIB_ENABLED
    "SHAPELIB_ENABLED "
#endif

    "\n");
}

int
main(int argc, char* argv[])
{
#ifdef AFL_INPUT_FUZZING
  AFL_INIT_ARGV();
#endif
  int c;
  int argn;
  ff_vecs_t* ivecs = NULL;
  ff_vecs_t* ovecs = NULL;
  filter_vecs_t* fvecs = NULL;
  QString fname;
  QString ofname;
  const char* ivec_opts = NULL;
  const char* ovec_opts = NULL;
  const char* fvec_opts = NULL;
  int opt_version = 0;
  int did_something = 0;
  const char* prog_name = argv[0]; /* argv is modified during processing */
  queue* wpt_head_bak, *rte_head_bak, *trk_head_bak;	/* #ifdef UTF8_SUPPORT */
  signed int wpt_ct_bak, rte_ct_bak, trk_ct_bak;	/* #ifdef UTF8_SUPPORT */
  QStack<QargStackElement> qargs_stack = QStack<QargStackElement>();

  // Create a QCoreApplication object to handle application initialization.
  // In addition to being useful for argument decoding, the creation of a
  // QCoreApplication object gets Qt initialized, especially locale related
  // QTextCodec stuff.
  // For example, this will get the QTextCodec::codecForLocale set
  // correctly.
  QCoreApplication app(argc, argv);
  // Use QCoreApplication::arguments() to process the command line.
  QStringList qargs = QCoreApplication::arguments();

  (void) new gpsbabel::UsAsciiCodec(); /* make sure a US-ASCII codec is available */

// MIN_QT_VERSION in configure.ac should correspond to the QT_VERSION_CHECK arguments in main.cc and gui/main.cc
#if (QT_VERSION < QT_VERSION_CHECK(5, 7, 0))
#error This version of Qt is not supported.
#endif

  // The first invocation of QTextCodec::codecForLocale() may result in LC_ALL being set to the native environment
  // as opposed to the initial default "C" locale.
  // This was demonstrated with Qt5 on Mac OS X.
  // TODO: This need to invoke QTextCodec::codecForLocale() may be taken care of
  // by creating a QCoreApplication.
#ifdef DEBUG_LOCALE
  printf("Initial locale: %s\n",setlocale(LC_ALL, NULL));
#endif
  (void) QTextCodec::codecForLocale();
#ifdef DEBUG_LOCALE
  printf("Locale after codedForLocale: %s\n",setlocale(LC_ALL, NULL));
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
    printf("LC_ALL: %s\n",setlocale(LC_ALL, NULL));
#endif
  }
  /* reset LC_TIME for strftime */
  if (strcmp(setlocale(LC_TIME,0), "C") != 0) {
#ifdef DEBUG_LOCALE
    printf("Resetting LC_TIME\n");
#endif
    setlocale(LC_TIME,"C");
#ifdef DEBUG_LOCALE
    printf("LC_ALL: %s\n",setlocale(LC_ALL, NULL));
#endif
  }

  global_opts.objective = wptdata;
  global_opts.masked_objective = NOTHINGMASK;	/* this makes the default mask behaviour slightly different */
  global_opts.charset_name.clear();
  global_opts.inifile = NULL;

  gpsbabel_now = time(NULL);			/* gpsbabel startup-time */
  gpsbabel_time = current_time().toTime_t();			/* same like gpsbabel_now, but freezed to zero during testo */

#ifdef DEBUG_MEM
  debug_mem_open();
  debug_mem_output("command line: ");
  for (int i = 1; i < qargs.size(); i++) {
    debug_mem_output("%s ", qPrintable(qargs.at(i)));
  }
  debug_mem_output("\n");
#endif

  if (gpsbabel_time != 0) {	/* within testo ? */
    global_opts.inifile = inifile_init(QString(), MYNAME);
  }

  init_vecs();
  init_filter_vecs();
  cet_register();
  session_init();
  waypt_init();
  route_init();

  if (qargs.size() < 2) {
    usage(prog_name,1);
    exit(0);
  }

  /*
   * Open-code getopts since POSIX-impaired OSes don't have one.
   */
  argn = 1;
  while (argn < qargs.size()) {
    QString optarg;

//  we must check the length for afl input fuzzing to work.
//    if (qargs.at(argn).at(0).toLatin1() != '-') {
    if (qargs.at(argn).size() > 0 && qargs.at(argn).at(0).toLatin1() != '-') {
      break;
    }
    if (qargs.at(argn).size() > 1 && qargs.at(argn).at(1).toLatin1() == '-') {
      break;
    }

    if (qargs.at(argn).size() > 1 && qargs.at(argn).at(1).toLatin1() == 'V') {
      printf("\nGPSBabel Version %s\n\n", gpsbabel_version);
      if (qargs.at(argn).size() > 2 && qargs.at(argn).at(2).toLatin1() == 'V') {
        print_extended_info();
      }
      exit(0);
    }

    if (qargs.at(argn).size() > 1 && (qargs.at(argn).at(1).toLatin1() == '?' || qargs.at(argn).at(1).toLatin1() == 'h')) {
      if (argn < qargs.size()-1) {
        spec_usage(qPrintable(qargs.at(argn+1)));
      } else {
        usage(prog_name,0);
      }
      exit(0);
    }

    c = qargs.at(argn).size() > 1 ? qargs.at(argn).at(1).toLatin1() : '\0';

    if (qargs.at(argn).size() > 2) {
      opt_version = qargs.at(argn).at(2).digitValue();
    }

    switch (c) {
    case 'i':
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      ivecs = find_vec(CSTR(optarg), &ivec_opts);
      if (ivecs == NULL) {
        fatal("Input type '%s' not recognized\n", qPrintable(optarg));
      }
      break;
    case 'o':
      if (ivecs == NULL) {
        warning("-o appeared before -i.   This is probably not what you want to do.\n");
      }
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      ovecs = find_vec(CSTR(optarg), &ovec_opts);
      if (ovecs == NULL) {
        fatal("Output type '%s' not recognized\n", qPrintable(optarg));
      }
      break;
    case 'f':
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      fname = optarg;
      if (fname.isEmpty()) {
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

      start_session(ivecs->name, CSTR(fname));
      ivecs->rd_init(fname);
      ivecs->read();
      ivecs->rd_deinit();

      cet_convert_strings(global_opts.charset, NULL, NULL);
      cet_convert_deinit();

      did_something = 1;
      break;
    case 'F':
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      ofname = optarg;
      if (ofname.isEmpty()) {
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
    case 'S':
      switch (qargs.at(argn).size() > 2 ? qargs.at(argn).at(2).toLatin1() : '\0') {
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
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      fvecs = find_filter_vec(CSTR(optarg), &fvec_opts);

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
        fatal("Unknown filter '%s'\n",qPrintable(optarg));
      }
      break;
    case 'D':
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      global_opts.debug_level = optarg.toInt();
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
      switch (qargs.at(argn).size() > 2 ? qargs.at(argn).at(2).toLatin1() : '\0') {
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
      usage(prog_name,0);
      exit(0);
    case 'p':
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      inifile_done(global_opts.inifile);
      if (optarg.isEmpty()) {	/* from GUI to preserve inconsistent options */
        global_opts.inifile = NULL;
      } else {
        global_opts.inifile = inifile_init(optarg, MYNAME);
      }
      break;
    case 'b':
      optarg = qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(++argn) ? qargs.at(argn) : QString();
      qargs_stack.push(QargStackElement(argn, qargs));
      qargs = load_args(optarg, qargs.at(0));
      if (qargs.size() == 0) {
        QargStackElement ele = qargs_stack.pop();
        argn = ele.argn;
        qargs = ele.qargs;
      } else {
        argn = 0;
      }
      break;

    default:
      fatal("Unknown option '%s'.\n", qPrintable(qargs.at(argn)));
      break;
    }

    while ((argn+1 >= qargs.size()) && (!qargs_stack.isEmpty())) {
      QargStackElement ele = qargs_stack.pop();
      argn = ele.argn;
      qargs = ele.qargs;
    }
    argn++;
  }

  /*
   * Allow input and output files to be specified positionally
   * as well.  This is the typical command line format.
   */
  for (int i = 0; i < argn; i++) {
    qargs.removeFirst();
  }
  if (qargs.size() > 2) {
    fatal("Extra arguments on command line\n");
  } else if (qargs.size() && ivecs) {
    did_something = 1;
    /* simulates the default behaviour of waypoints */
    if (doing_nothing) {
      global_opts.masked_objective |= WPTDATAMASK;
    }

    cet_convert_init(ivecs->encode, 1);

    start_session(ivecs->name, CSTR(qargs.at(0)));
    if (ivecs->rd_init == NULL) {
      fatal("Format does not support reading.\n");
    }
    ivecs->rd_init(qargs.at(0));
    ivecs->read();
    ivecs->rd_deinit();

    cet_convert_strings(global_opts.charset, NULL, NULL);
    cet_convert_deinit();

    if (qargs.size() == 2 && ovecs) {
      cet_convert_init(ovecs->encode, 1);
      cet_convert_strings(NULL, global_opts.charset, NULL);

      if (ovecs->wr_init == NULL) {
        fatal("Format does not support writing.\n");
      }

      ovecs->wr_init(qargs.at(1));
      ovecs->write();
      ovecs->wr_deinit();

      cet_convert_deinit();
    }
  } else if (qargs.size()) {
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
      if (fname.isEmpty()) {
        fatal("An input file (-f) must be specified.\n");
      }
      start_session(ivecs->name, CSTR(fname));
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
