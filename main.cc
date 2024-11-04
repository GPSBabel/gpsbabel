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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <cassert>                    // for assert
#include <clocale>                    // for setlocale, LC_NUMERIC, LC_TIME
#include <csignal>                    // for signal, SIGINT, SIG_ERR
#include <cstdio>                     // for printf, fflush, fgetc, fprintf, stderr, stdin, stdout
#include <cstring>                    // for strcmp

#include <QCoreApplication>           // for QCoreApplication
#include <QDateTime>                  // for QDateTime
#include <QDebug>                     // for QDebug
#include <QElapsedTimer>              // for QElapsedTimer
#include <QFile>                      // for QFile
#include <QIODevice>                  // for QIODevice::ReadOnly
#include <QLocale>                    // for QLocale
#include <QMessageLogContext>         // for QMessageLogContext
#include <QStack>                     // for QStack
#include <QString>                    // for QString
#include <QStringList>                // for QStringList
#include <QSysInfo>                   // for QSysInfo
#include <QTextCodec>                 // for QTextCodec
#include <QTextStream>                // for QTextStream
#include <QtConfig>                   // for QT_VERSION_STR
#include <QtGlobal>                   // for qPrintable, qVersion, QT_VERSION, QT_VERSION_CHECK

#ifdef AFL_INPUT_FUZZING
#include "argv-fuzz-inl.h"
#endif

#include "defs.h"
#include "csv_util.h"                 // for csv_linesplit
#include "filter.h"                   // for Filter
#include "filter_vecs.h"              // for FilterVecs
#include "format.h"                   // for Format
#include "gbversion.h"                // for VERSION_SHA
#include "inifile.h"                  // for inifile_done, inifile_init
#include "jeeps/gpsmath.h"            // for GPS_Lookup_Datum_Index
#include "mkshort.h"                  // for MakeShort
#include "session.h"                  // for start_session, session_exit, session_init
#include "src/core/datetime.h"        // for DateTime
#include "src/core/file.h"            // for File
#include "src/core/usasciicodec.h"    // for UsAsciiCodec
#include "vecs.h"                     // for Vecs

static constexpr bool DEBUG_LOCALE = false;

// be careful not to advance argn passed the end of the list, i.e. ensure argn < qargs.size()
#define FETCH_OPTARG qargs.at(argn).size() > 2 ? QString(qargs.at(argn)).remove(0,2) : qargs.size()>(argn+1) ? qargs.at(++argn) : QString()

static QElapsedTimer timer;

class QargStackElement
{
public:
  int argn{0};
  QStringList qargs;

public:
  QargStackElement() = default;

  QargStackElement(int p_argn, const QStringList& p_qargs) :
    argn{p_argn}, qargs{p_qargs}
  {}
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

  const QStringList values = csv_linesplit(line, " ", "\"", 0);
  qargs.append(values);

  return (qargs);
}

static void
usage(const char* pname, bool verbose)
{
  printf("GPSBabel Version %s.  https://www.gpsbabel.org\n\n",
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
    "    -x filtername    Invoke filter (placed between inputs and output)\n"
    "    -D level         Set debug level [%d]\n"
    "    -h, -?           Print detailed help and exit\n"
    "    -V               Print GPSBabel version and exit\n"
    "\n"
    , pname
    , pname
    , global_opts.debug_level
  );
  if (!verbose) {
    printf("\n\n[Press enter]");
    fgetc(stdin);
  } else {
    printf("File Types (-i and -o options):\n");
    Vecs::Instance().disp_vec();
    printf("\nSupported data filters:\n");
    FilterVecs::Instance().disp_filter_vec();
  }
}

static void
spec_usage(const QString& vec)
{
  printf("\n");
  Vecs::Instance().disp_vec(vec);
  FilterVecs::Instance().disp_filter_vec(vec);
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

#if SHAPELIB_ENABLED
    "SHAPELIB_ENABLED "
#endif

    "\n");
}

static void setMessagePattern(const QString& id = QString())
{
  if (id.isEmpty()) {
    qSetMessagePattern("%{if-category}%{category}: %{endif}main: %{message}");
  } else {
    qSetMessagePattern(QStringLiteral("%{if-category}%{category}: %{endif}%1: %{message}").arg(id));
  }
}

static void MessageHandler(QtMsgType type, const QMessageLogContext& context, const QString& msg)
{
  QString message = qFormatLogMessage(type, context, msg);
  /* flush any buffered standard output */
  fflush(stdout);
  fprintf(stderr, "%s\n", qPrintable(message));
  fflush(stderr);
}

static void
signal_handler(int sig)
{
  (void)sig;
  tracking_status.request_terminate = 1;
}

class FallbackOutput
{
public:

  void waypt_disp(const Waypoint* wpt)
  {
    if (wpt->GetCreationTime().isValid()) {
      printf("%s ", qPrintable(wpt->creation_time.toString()));
    }
    printposn(wpt->latitude, true);
    printposn(wpt->longitude, false);
    if (!wpt->description.isEmpty()) {
      printf("%s/%s",
             global_opts.synthesize_shortnames ?
             qPrintable(mkshort_handle.mkshort(wpt->description)) :
             qPrintable(wpt->shortname),
             qPrintable(wpt->description));
    }

    if (wpt->altitude != unknown_alt) {
      printf(" %f", wpt->altitude);
    }
    printf("\n");
  }

private:
  MakeShort mkshort_handle;
};

static void
run_reader(Vecs::fmtinfo_t& ivecs, const QString& fname)
{
  if (global_opts.debug_level > 0)  {
    timer.start();
  }
  start_session(ivecs.fmtname, fname);
  setMessagePattern(ivecs.fmtname);
  if (ivecs.isDynamic()) {
    ivecs.fmt = ivecs.factory(fname);
    Vecs::init_vec(ivecs.fmt, ivecs.fmtname);
    Vecs::prepare_format(ivecs);

    ivecs->rd_init(fname);
    ivecs->read();
    ivecs->rd_deinit();

    Vecs::exit_vec(ivecs.fmt);
    delete ivecs.fmt;
    ivecs.fmt = nullptr;
  } else {
    /* reinitialize xcsv in case two formats that use xcsv were given */
    Vecs::prepare_format(ivecs);

    ivecs->rd_init(fname);
    ivecs->read();
    ivecs->rd_deinit();
  }
  setMessagePattern();
  if (global_opts.debug_level > 0)  {
    qDebug().noquote() << QStringLiteral("reader %1 took %2 seconds.")
                        .arg(ivecs.fmtname, QString::number(timer.elapsed()/1000.0, 'f', 3));
  }
}

static void
run_writer(Vecs::fmtinfo_t& ovecs, const QString& ofname)
{
  if (global_opts.debug_level > 0)  {
    timer.start();
  }
  setMessagePattern(ovecs.fmtname);
  if (ovecs.isDynamic()) {
    ovecs.fmt = ovecs.factory(ofname);
    Vecs::init_vec(ovecs.fmt, ovecs.fmtname);
    Vecs::prepare_format(ovecs);

    ovecs->wr_init(ofname);
    ovecs->write();
    ovecs->wr_deinit();

    Vecs::exit_vec(ovecs.fmt);
    delete ovecs.fmt;
    ovecs.fmt = nullptr;
  } else {
    /* reinitialize xcsv in case two formats that use xcsv were given */
    Vecs::prepare_format(ovecs);

    ovecs->wr_init(ofname);
    ovecs->write();
    ovecs->wr_deinit();
  }
  setMessagePattern();
  if (global_opts.debug_level > 0)  {
    qDebug().noquote() << QStringLiteral("writer %1 took %2 seconds.")
                        .arg(ovecs.fmtname, QString::number(timer.elapsed()/1000.0, 'f', 3));
  }
}

static int
run(const char* prog_name)
{
  int argn;
  Vecs::fmtinfo_t ivecs;
  Vecs::fmtinfo_t ovecs;
  FilterVecs::fltinfo_t filter;
  QString fname;
  QString ofname;
  int opt_version = 0;
  bool did_something = false;
  QStack<QargStackElement> qargs_stack;
  FallbackOutput fbOutput;


  // Use QCoreApplication::arguments() to process the command line.
  QStringList qargs = QCoreApplication::arguments();

  if (qargs.size() < 2) {
    usage(prog_name, false);
    return 0;
  }

  /*
   * Open-code getopts since POSIX-impaired OSes don't have one.
   */
  argn = 1;
  while (argn < qargs.size()) {
    QString argument;

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
      return 0;
    }

    if (qargs.at(argn).size() > 1 && (qargs.at(argn).at(1).toLatin1() == '?' || qargs.at(argn).at(1).toLatin1() == 'h')) {
      if (argn < qargs.size()-1) {
        spec_usage(qargs.at(argn+1));
      } else {
        usage(prog_name, true);
      }
      return 0;
    }

    int c = qargs.at(argn).size() > 1 ? qargs.at(argn).at(1).toLatin1() : '\0';

    if (qargs.at(argn).size() > 2) {
      opt_version = qargs.at(argn).at(2).digitValue();
    }

    switch (c) {
    case 'i':
      argument = FETCH_OPTARG;
      ivecs = Vecs::Instance().find_vec(argument);
      if (!ivecs) {
        gbFatal("Input type '%s' not recognized\n", gbLogCStr(argument));
      }
      break;
    case 'o':
      if (!ivecs) {
        gbWarning("-o appeared before -i.   This is probably not what you want to do.\n");
      }
      argument = FETCH_OPTARG;
      ovecs = Vecs::Instance().find_vec(argument);
      if (!ovecs) {
        gbFatal("Output type '%s' not recognized\n", gbLogCStr(argument));
      }
      break;
    case 'f':
      argument = FETCH_OPTARG;
      fname = argument;
      if (fname.isEmpty()) {
        gbFatal("No file or device name specified.\n");
      }
      if (!ivecs) {
        gbFatal("No valid input type specified\n");
      }
      if (global_opts.masked_objective & POSNDATAMASK) {
        did_something = true;
        break;
      }
      /* simulates the default behaviour of waypoints */
      if (doing_nothing) {
        global_opts.masked_objective |= WPTDATAMASK;
      }

      run_reader(ivecs, fname);

      did_something = true;
      break;
    case 'F':
      argument = FETCH_OPTARG;
      ofname = argument;
      if (ofname.isEmpty()) {
        gbFatal("No output file or device name specified.\n");
      }
      if (ovecs && (!(global_opts.masked_objective & POSNDATAMASK))) {
        /* simulates the default behaviour of waypoints */
        if (doing_nothing) {
          global_opts.masked_objective |= WPTDATAMASK;
        }

        run_writer(ovecs, ofname);

      }
      break;
    case 's':
      global_opts.synthesize_shortnames = true;
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
        global_opts.smart_icons = true;
        break;
      case 'n':
        global_opts.smart_names = true;
        break;
      default:
        global_opts.smart_icons = true;
        global_opts.smart_names = true;
        break;
      }
      break;
    case 'x':
      argument = FETCH_OPTARG;
      filter = FilterVecs::Instance().find_filter_vec(argument);

      if (filter) {
        if (global_opts.debug_level > 0)  {
          timer.start();
        }
        setMessagePattern(filter.fltname);
        if (filter.isDynamic()) {
          filter.flt = filter.factory();
          FilterVecs::init_filter_vec(filter.flt, filter.fltname);
          FilterVecs::prepare_filter(filter);

          filter->init();
          filter->process();
          filter->deinit();
          FilterVecs::free_filter_vec(filter.flt);

          FilterVecs::exit_filter_vec(filter.flt);
          delete filter.flt;
          filter.flt = nullptr;
        } else {
          FilterVecs::prepare_filter(filter);
          filter->init();
          filter->process();
          filter->deinit();
          FilterVecs::free_filter_vec(filter.flt);
        }
        setMessagePattern();
        if (global_opts.debug_level > 0)  {
          qDebug().noquote() << QStringLiteral("filter %1 took %2 seconds.")
                              .arg(filter.fltname, QString::number(timer.elapsed()/1000.0, 'f', 3));
        }
      }  else {
        gbFatal("Unknown filter '%s'\n",gbLogCStr(argument));
      }
      break;
    case 'D':
      argument = FETCH_OPTARG;
      {
        bool ok;
        global_opts.debug_level = argument.toInt(&ok);
        if (!ok) {
          gbFatal("the -D option requires an integer value to specify the debug level, i.e. -D level\n");
        }
      }
      /*
       * When debugging, announce version.
       */
      if (global_opts.debug_level > 0)  {
        gbInfo("GPSBabel Version: %s\n", gpsbabel_version);
        if(sizeof(kVersionSHA) > 1) {
          gbInfo("Repository SHA: %s\n", kVersionSHA);
        }
        if(sizeof(kVersionDate) > 1) {
          QDateTime date = QDateTime::fromString(kVersionDate, Qt::ISODate);
          if (date.isValid()) {
            gbInfo("Date: %s\n", gbLogCStr(date.toUTC().toString(Qt::ISODate)));
          }
        }
        gbInfo("Compiled with Qt %s for architecture %s\n",
                QT_VERSION_STR,
                gbLogCStr(QSysInfo::buildAbi()));
        gbInfo("Running with Qt %s on %s, %s\n", qVersion(),
                gbLogCStr(QSysInfo::prettyProductName()),
                gbLogCStr(QSysInfo::currentCpuArchitecture()));
        gbInfo("QLocale::system() is %s\n", gbLogCStr(QLocale::system().name()));
        gbInfo("QLocale() is %s\n", gbLogCStr(QLocale().name()));
        QTextCodec* defaultcodec = QTextCodec::codecForLocale();
        gbInfo("QTextCodec::codecForLocale() is %s, mib %d\n",
                defaultcodec->name().constData(),defaultcodec->mibEnum());
      }
      break;

    /*
     * Undocumented '-@' option for test.
     */
    case '@': {
      bool format_ok = Vecs::Instance().validate_formats();
      bool filter_ok = FilterVecs::Instance().validate_filters();
      return (format_ok && filter_ok)? 0 : 1;
    }

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
      Vecs::Instance().disp_formats(opt_version);
      return 0;
    case '%':
      FilterVecs::Instance().disp_filters(opt_version);
      return 0;
    case 'h':
    case '?':
      usage(prog_name, true);
      return 0;
    case 'p':
      argument = FETCH_OPTARG;
      inifile_done(global_opts.inifile);
      if (argument.isEmpty()) {	/* from GUI to preserve inconsistent options */
        global_opts.inifile = nullptr;
      } else {
        global_opts.inifile = inifile_init(argument);
      }
      break;
    case 'b':
      argument = FETCH_OPTARG;
      qargs_stack.push(QargStackElement(argn, qargs));
      qargs = load_args(argument, qargs.at(0));
      if (qargs.empty()) {
        QargStackElement ele = qargs_stack.pop();
        argn = ele.argn;
        qargs = ele.qargs;
      } else {
        argn = 0;
      }
      break;

    default:
      gbFatal("Unknown option '%s'.\n", gbLogCStr(qargs.at(argn)));
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
    gbFatal("Extra arguments on command line\n");
  } else if ((!qargs.isEmpty()) && ivecs) {
    did_something = true;
    /* simulates the default behaviour of waypoints */
    if (doing_nothing) {
      global_opts.masked_objective |= WPTDATAMASK;
    }

    run_reader(ivecs, qargs.at(0));

    if (qargs.size() == 2 && ovecs) {

      run_writer(ovecs, qargs.at(1));

    }
  } else if (!qargs.isEmpty()) {
    usage(prog_name, true);
    return 0;
  }
  if (!ovecs) {
    auto waypt_disp_lambda = [&fbOutput](const Waypoint* wpt)->void {
      fbOutput.waypt_disp(wpt);
    };
    waypt_disp_all(waypt_disp_lambda);
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
      gbFatal("Realtime tracking (-T) requires an input type (-t)i such as Garmin or NMEA.\n");
    }

    if (fname.isEmpty()) {
      gbFatal("An input file (-f) must be specified.\n");
    }

    if (ivecs.isDynamic()) {
      setMessagePattern(ivecs.fmtname);
      ivecs.fmt = ivecs.factory(fname);
      Vecs::init_vec(ivecs.fmt, ivecs.fmtname);
      setMessagePattern();
    }
    if (ovecs && ovecs.isDynamic()) {
      setMessagePattern(ovecs.fmtname);
      ovecs.fmt = ovecs.factory(ofname);
      Vecs::init_vec(ovecs.fmt, ovecs.fmtname);
      setMessagePattern();
    }

    start_session(ivecs.fmtname, fname);
    setMessagePattern(ivecs.fmtname);
    Vecs::prepare_format(ivecs);
    ivecs->rd_position_init(fname);
    setMessagePattern();

    if (global_opts.masked_objective & ~POSNDATAMASK) {
      gbFatal("Realtime tracking (-T) is exclusive of other modes.\n");
    }

    if (signal(SIGINT, signal_handler) == SIG_ERR) {
      gbFatal("Couldn't install the exit signal handler.\n");
    }

    if (ovecs) {
      setMessagePattern(ovecs.fmtname);
      Vecs::prepare_format(ovecs);
      ovecs->wr_position_init(ofname);
      setMessagePattern();
    }

    tracking_status.request_terminate = 0;
    while (!tracking_status.request_terminate) {
      setMessagePattern(ivecs.fmtname);
      Waypoint* wpt = ivecs->rd_position(&tracking_status);
      setMessagePattern();

      if (tracking_status.request_terminate) {
        delete wpt;
        break;
      }
      if (wpt) {
        setMessagePattern(ovecs.fmtname);
        if (ovecs) {
//          ovecs->wr_position_init(ofname);
          ovecs->wr_position(wpt);
//          ovecs->wr_position_deinit();
        } else {
          /* Just print to screen */
          fbOutput.waypt_disp(wpt);
        }
        delete wpt;
        setMessagePattern();
      }
    }
    setMessagePattern(ivecs.fmtname);
    Vecs::prepare_format(ivecs);
    ivecs->rd_position_deinit();
    setMessagePattern();
    if (ovecs) {
      setMessagePattern(ovecs.fmtname);
      Vecs::prepare_format(ovecs);
      ovecs->wr_position_deinit();
      setMessagePattern();
    }

    if (ovecs && ovecs.isDynamic()) {
      Vecs::exit_vec(ovecs.fmt);
      delete ovecs.fmt;
      ovecs.fmt = nullptr;
    }
    if (ivecs.isDynamic()) {
      Vecs::exit_vec(ivecs.fmt);
      delete ivecs.fmt;
      ivecs.fmt = nullptr;
    }
    return 0;
  }


  if (!did_something) {
    gbFatal("Nothing to do!  Use '%s -h' for command-line options.\n", prog_name);
  }

  return 0;
}

int
main(int argc, char* argv[])
{
#ifdef AFL_INPUT_FUZZING
  AFL_INIT_ARGV();
#endif
  int rc = 0;
  const char* prog_name = argv[0]; /* may not match QCoreApplication::arguments().at(0)! */

// MIN_QT_VERSION in GPSBabel.pro should correspond to the QT_VERSION_CHECK
// arguments in main.cc and gui/main.cc and the version check in
// CMakeLists.txt, gui/CMakeLists.txt.
#if (QT_VERSION < QT_VERSION_CHECK(6, 2, 0))
#error This version of Qt is not supported.
#endif

#if defined(_MSC_VER) && (_MSC_VER < 1920) /* Visual Studio 2017 or earlier */
#error Visual Studio 2017 and earlier are not supported. Please use Visual Studio 2019 or 2022.
#endif

  if constexpr (DEBUG_LOCALE) {
    gbDebug("Initial locale: %s\n",setlocale(LC_ALL, nullptr));
  }

  // Create a QCoreApplication object to handle application initialization.
  // In addition to being useful for argument decoding, the creation of a
  // QCoreApplication object gets Qt initialized, especially locale related
  // QTextCodec stuff.
  // For example, this will get the QTextCodec::codecForLocale set
  // correctly.
  QCoreApplication app(argc, argv);

  // The first invocation of QTextCodec::codecForLocale() or
  // construction of QCoreApplication object
  // may result in LC_ALL being set to the native environment
  // as opposed to the initial default "C" locale.
  // This was demonstrated with Qt5 on Mac OS X.
  if constexpr (DEBUG_LOCALE) {
    gbDebug("Locale after initial setup: %s\n",setlocale(LC_ALL, nullptr));
  }
  // As recommended in QCoreApplication reset the locale to the default.
  // Note the documentation says to set LC_NUMERIC, but QCoreApplicationPrivate::initLocale()
  // actually sets LC_ALL.
  // Perhaps we should restore LC_ALL instead of only LC_NUMERIC.
  if (strcmp(setlocale(LC_NUMERIC,nullptr), "C") != 0) {
    if constexpr (DEBUG_LOCALE) {
      gbDebug("Resetting LC_NUMERIC\n");
    }
    setlocale(LC_NUMERIC,"C");
    if constexpr (DEBUG_LOCALE) {
      gbDebug("LC_ALL: %s\n",setlocale(LC_ALL, nullptr));
    }
  }
  /* reset LC_TIME for strftime */
  if (strcmp(setlocale(LC_TIME,nullptr), "C") != 0) {
    if constexpr (DEBUG_LOCALE) {
      gbDebug("Resetting LC_TIME\n");
    }
    setlocale(LC_TIME,"C");
    if constexpr (DEBUG_LOCALE) {
      gbDebug("LC_ALL: %s\n",setlocale(LC_ALL, nullptr));
    }
  }
  qInstallMessageHandler(MessageHandler);
  setMessagePattern();

  (void) new gpsbabel::UsAsciiCodec(); /* make sure a US-ASCII codec is available */

  global_opts.objective = wptdata;
  global_opts.masked_objective = NOTHINGMASK;	/* this makes the default mask behaviour slightly different */
  global_opts.inifile = nullptr;

  gpsbabel_time = current_time().toTime_t();			/* frozen in testmode */

  if (!gpsbabel_testmode()) {	/* within testo ? */
    global_opts.inifile = inifile_init(QString());
  }

  assert(GPS_Lookup_Datum_Index("OSGB36") == kDatumOSGB36);
  assert(GPS_Lookup_Datum_Index("WGS 84") == kDatumWGS84);

  Vecs::Instance().init_vecs();
  FilterVecs::Instance().init_filter_vecs();
  session_init();
  waypt_init();
  route_init();

  rc = run(prog_name);

  route_deinit();
  waypt_deinit();
  session_exit();
  FilterVecs::Instance().exit_filter_vecs();
  Vecs::Instance().exit_vecs();
  inifile_done(global_opts.inifile);

  return rc;
}
