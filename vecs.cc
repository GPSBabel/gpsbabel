/*
    Describe vectors containing file operations.

    Copyright (C) 2002, 2004, 2005, 2006, 2007 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "vecs.h"

#include <QByteArray>          // for QByteArray
#include <QChar>               // for QChar
#include <QDebug>              // for QDebug
#include <QDir>                // for QDir, QDir::Files, QDir::Name
#include <QFileInfo>           // for QFileInfo
#include <QFileInfoList>       // for QFileInfoList
#include <QString>             // for QString
#include <QStringList>         // for QStringList
#include <QVector>             // for QVector
#include <Qt>                  // for CaseInsensitive
#include <QtGlobal>            // for qPrintable

#include <algorithm>           // for sort
#include <cassert>             // for assert
#include <cstdio>              // for printf, putchar, sscanf
#include <type_traits>         // for is_base_of
#include <utility>             // for as_const

#include "defs.h"              // for arglist_t, CSTR, fatal, ff_cap, ARGTYPE_TYPEMASK, ff_type, ARGTYPE_BOOL, case_ignore_strcmp, gpsdata_type, warning, ff_cap_array, global_options, global_opts, ARGTYPE_FLOAT, ARGTYPE_HIDDEN, ARGTYPE_INT, ARGTYPE_REQUIRED
#include "dg-100.h"            // for Dg100FileFormat, Dg100SerialFormat, Dg200FileFormat, Dg200SerialFormat
#include "exif.h"              // for ExifFormat
#include "format.h"            // for Format
#include "garmin.h"            // for GarminFormat
#include "garmin_fit.h"        // for GarminFitFormat
#include "garmin_gpi.h"        // for GarminGPIFormat
#include "garmin_txt.h"        // for GarminTxtFormat
#include "garmin_xt.h"         // for GarminXTFormat
#include "gbversion.h"         // for WEB_DOC_DIR
#include "gdb.h"               // for GdbFormat
#include "geojson.h"           // for GeoJsonFormat
#include "globalsat_sport.h"   // for GlobalsatSportFormat
#include "geo.h"               // for GeoFormat
#include "gpx.h"               // for GpxFormat
#include "gtm.h"               // for GtmFormat
#include "gtrnctr.h"           // for GtrnctrFormat
#include "html.h"              // for HtmlFormat
#include "humminbird.h"        // for HumminbirdFormat, HumminbirdHTFormat
#include "igc.h"               // for IgcFormat
#include "inifile.h"           // for inifile_readstr
#include "kml.h"               // for KmlFormat
#include "lowranceusr.h"       // for LowranceusrFormat
#include "mtk_logger.h"        // for MtkFormat, MtkM241Format, MtkFileFormat, MtkM241FileFormat
#include "nmea.h"              // for NmeaFormat
#include "option.h"            // for Option, OptionBool
#include "osm.h"               // for OsmFormat
#include "ozi.h"               // for OziFormat
#include "qstarz_bl_1000.h"    // for QstarzBL1000Format
#include "random.h"            // for RandomFormat
#include "shape.h"             // for ShapeFormat
#include "skytraq.h"           // for MinihomerFormat, SkytraqFormat, SkytraqfileFormat
#include "src/core/logging.h"  // for Warning, FatalMsg
#include "subrip.h"            // for SubripFormat
#include "text.h"              // for TextFormat
#include "tpg.h"               // for TpgFormat
#include "tpo.h"               // for Tpo2Format, Tpo3Format
#include "unicsv.h"            // for UnicsvFormat
#include "v900.h"              // for V900Format
#include "vcf.h"               // for VcfFormat
#include "xcsv.h"              // for XcsvStyle, XcsvFormat
#include "googletakeout.h"     // for GoogleTakeoutFormat


#define MYNAME "vecs"

template <typename T>
Format* fmtfactory(const QString& filename)
{
  static_assert(std::is_base_of<Format, T>::value, "T must be derived from Format");
  return new T(filename);
}

struct Vecs::Impl {
  /*
   * Having these LegacyFormat instances be non-static data members
   * prevents the static initialization order fiasco because
   * the static vec that is used to construct a legacy format
   * instance is guaranteed to have already constructed when an instance
   * of this class is constructed.
   */
  GpxFormat gpx_fmt;
  GarminFormat garmin_fmt;
  GdbFormat gdb_fmt;
  NmeaFormat nmea_fmt;
  OziFormat ozi_fmt;
  KmlFormat kml_fmt;
  LowranceusrFormat lowranceusr_fmt;
  Tpo2Format tpo2_fmt;
  Tpo3Format tpo3_fmt;
#if SHAPELIB_ENABLED
  ShapeFormat shape_fmt;
#endif
  TextFormat text_fmt;
  HtmlFormat html_fmt;
  IgcFormat igc_fmt;
  MtkFormat mtk_fmt;
  MtkFileFormat mtk_ffmt;
  MtkM241Format mtk_m241_fmt;
  MtkM241FileFormat mtk_m241_ffmt;
  UnicsvFormat unicsv_fmt;
  GtmFormat gtm_fmt;
  GarminTxtFormat garmin_txt_fmt;
  GtrnctrFormat gtc_fmt;
  GarminGPIFormat garmin_gpi_fmt;
  RandomFormat random_fmt;
  Dg100SerialFormat dg100_fmt;
  Dg100FileFormat dg100_ffmt;
  Dg200SerialFormat dg200_fmt;
  Dg200FileFormat dg200_ffmt;
  OsmFormat osm_fmt;
  ExifFormat exif_fmt;
  HumminbirdFormat humminbird_fmt;
  HumminbirdHTFormat humminbird_ht_fmt;
  SkytraqFormat skytraq_fmt;
  SkytraqfileFormat skytraq_ffmt;
  MinihomerFormat miniHomer_fmt;
  SubripFormat subrip_fmt;
  GarminXTFormat format_garmin_xt_fmt;
  GarminFitFormat format_fit_fmt;
  GeoJsonFormat geojson_fmt;
  GlobalsatSportFormat globalsat_sport_fmt;

  const QVector<vecs_t> vec_list {
    /* XCSV must be the first entry in this table. */
    {
      nullptr,
      "xcsv",
      "? Character Separated Values",
      nullptr,
      nullptr,
      &fmtfactory<XcsvFormat>
    },
    {
      nullptr,
      "geo",
      "Geocaching.com .loc",
      "loc",
      nullptr,
      &fmtfactory<GeoFormat>
    },
    {
      &gpx_fmt,
      "gpx",
      "GPX XML",
      "gpx",
      nullptr,
    },
    {
      &garmin_fmt,
      "garmin",
      "Garmin serial/USB protocol",
      nullptr,
      nullptr,
    },
    {
      &gdb_fmt,
      "gdb",
      "Garmin MapSource - gdb",
      "gdb",
      nullptr,
    },
    {
      &nmea_fmt,
      "nmea",
      "NMEA 0183 sentences",
      nullptr,
      nullptr,
    },
    {
      &ozi_fmt,
      "ozi",
      "OziExplorer",
      nullptr,
      nullptr,
    },
    {
      &kml_fmt,
      "kml",
      "Google Earth (Keyhole) Markup Language",
      "kml",
      nullptr,
    },
    {
      &lowranceusr_fmt,
      "lowranceusr",
      "Lowrance USR",
      "usr",
      nullptr,
    },
    {
      nullptr,
      "tpg",
      "National Geographic Topo .tpg (waypoints)",
      "tpg",
      nullptr,
      &fmtfactory<TpgFormat>
    },
    {
      &tpo2_fmt,
      "tpo2",
      "National Geographic Topo 2.x .tpo",
      "tpo",
      nullptr,
    },
    {
      &tpo3_fmt,
      "tpo3",
      "National Geographic Topo 3.x/4.x .tpo",
      "tpo",
      nullptr,
    },
#if SHAPELIB_ENABLED
    {
      &shape_fmt,
      "shape",
      "ESRI shapefile",
      "shp",
      nullptr,
    },
#endif
    {
      &text_fmt,
      "text",
      "Textual Output",
      "txt",
      nullptr,
    },
    {
      &html_fmt,
      "html",
      "HTML Output",
      "html",
      nullptr,
    },
    {
      &igc_fmt,
      "igc",
      "FAI/IGC Flight Recorder Data Format",
      nullptr,
      nullptr,
    },
    {
      &mtk_fmt,
      "mtk",
      "MTK Logger (iBlue 747,Qstarz BT-1000,...) download",
      nullptr,
      nullptr,
    },
    {
      &mtk_ffmt,
      "mtk-bin",
      "MTK Logger (iBlue 747,...) Binary File Format",
      "bin",
      nullptr,
    },
    {
      &mtk_m241_fmt,
      "m241",
      "Holux M-241 (MTK based) download",
      nullptr,
      nullptr,
    },
    {
      &mtk_m241_ffmt,
      "m241-bin",
      "Holux M-241 (MTK based) Binary File Format",
      "bin",
      nullptr,
    },
    {
      nullptr,
      "vcard",
      "Vcard Output (for iPod)",
      "vcf",
      nullptr,
      &fmtfactory<VcfFormat>
    },
    {
      &unicsv_fmt,
      "unicsv",
      "Universal csv with field structure in first line",
      nullptr,
      nullptr,
    },
    {
      &gtm_fmt,
      "gtm",
      "GPS TrackMaker",
      "gtm",
      nullptr,
    },
    {
      &garmin_txt_fmt,
      "garmin_txt",
      "Garmin MapSource - txt (tab delimited)",
      "txt",
      nullptr,
    },
    {
      &gtc_fmt,
      "gtrnctr",
      "Garmin Training Center (.tcx/.crs/.hst/.xml)",
      "tcx/crs/hst/xml",
      nullptr,
    },
    {
      &garmin_gpi_fmt,
      "garmin_gpi",
      "Garmin Points of Interest (.gpi)",
      "gpi",
      nullptr,
    },
    {
      &random_fmt,
      "random",
      "Internal GPS data generator",
      nullptr,
      nullptr,
    },
    {
      &dg100_fmt,
      "dg-100",
      "GlobalSat DG-100/BT-335 Download",
      nullptr,
      nullptr,
    },
    {
      &dg100_ffmt,
      "dg-100-bin",
      "GlobalSat DG-100/BT-335 Binary File",
      nullptr,
      nullptr,
    },
    {
      &dg200_fmt,
      "dg-200",
      "GlobalSat DG-200 Download",
      nullptr,
      nullptr,
    },
    {
      &dg200_ffmt,
      "dg-200-bin",
      "GlobalSat DG-200 Binary File",
      nullptr,
      nullptr,
    },
    {
      &osm_fmt,
      "osm",
      "OpenStreetMap data files",
      "osm",
      nullptr,
    },
    {
      &exif_fmt,
      "exif",
      "Embedded Exif-GPS data (.jpg)",
      "jpg",
      nullptr,
    },
    {
      &humminbird_fmt,
      "humminbird",
      "Humminbird waypoints and routes (.hwr)",
      "hwr",
      nullptr,
    },
    {
      &humminbird_ht_fmt,
      "humminbird_ht",
      "Humminbird tracks (.ht)",
      "ht",
      nullptr,
    },
    {
      nullptr,
      "v900",
      "Columbus/Visiontac V900 files (.csv)",
      nullptr,
      nullptr,
      &fmtfactory<V900Format>
    },
    {
      &skytraq_fmt,
      "skytraq",
      "SkyTraq Venus based loggers (download)",
      nullptr,
      nullptr,
    },
    {
      &skytraq_ffmt,
      "skytraq-bin",
      "SkyTraq Venus based loggers Binary File Format",
      "bin",
      nullptr,
    },
    {
      &miniHomer_fmt,
      "miniHomer",
      "MiniHomer, a skyTraq Venus 6 based logger (download tracks, waypoints and get/set POI)",
      nullptr,
      nullptr,
    },
    {
      &subrip_fmt,
      "subrip",
      "SubRip subtitles for video mapping (.srt)",
      "srt",
      nullptr,
    },
    {
      &format_garmin_xt_fmt,
      "garmin_xt",
      "Mobile Garmin XT Track files",
      nullptr,
      nullptr,
    },
    {
      &format_fit_fmt,
      "garmin_fit",
      "Flexible and Interoperable Data Transfer (FIT) Activity file",
      "fit",
      nullptr,
    },
    {
      &geojson_fmt,
      "geojson",
      "GeoJson",
      "json",
      nullptr,
    },
    {
      &globalsat_sport_fmt,
      "globalsat",
      "GlobalSat GH625XT GPS training watch",
      nullptr,
      nullptr,
    },
    {
      nullptr,
      "qstarz_bl-1000",
      "Qstarz BL-1000",
      nullptr,
      nullptr,
      &fmtfactory<QstarzBL1000Format>
    },
    {
      nullptr,
      "googletakeout",
      "Google Takeout Location History",
      "json",
      nullptr,
      &fmtfactory<GoogleTakeoutFormat>
    }
  };
};

Vecs& Vecs::Instance()
{
  static Impl impl;
  static Vecs instance(&impl);
  return instance;
}

/*
 * When we modify an element on the list we need to be careful
 * that we are not modifying a Qt COW copy.
 * Qt has an undocumented but public member function isDetached().
 * If the list is detached it implies it is not shared, then functions
 * then might detach, like the iterator begin which is implicitly used
 * in the range based for loop, won't cause a copy to be created.
 * We can make sure this is true for at least our regression cases
 * with assertions.
 * There is an odd situation that an empty QVector is not detached,
 * so we have to exclude this from the check.
 * The possibility of detachment is also why the type of element
 * on the list must be default constructable. This is why we have
 * to supply a default for any const members of arglist_t.  Without
 * the default the default constructor would be implicitly deleted.
 */

void Vecs::init_vec(Format* fmt)
{
  QVector<arglist_t>* args = fmt->get_args();
  if (args && !args->isEmpty()) {
    assert(args->isDetached());
    for (auto& arg : *args) {
      if (arg.argval != nullptr) {
        arg.argval->reset();
      }
    }
  }
}

void Vecs::init_vecs()
{
  for (const auto& vec : d_ptr_->vec_list) {
    if (vec.vec != nullptr) {
      init_vec(vec.vec);
    }
  }
  style_list = create_style_vec();
}

int Vecs::integer_base(uint32_t argtype)
{
  int base;
  switch (argtype & ARGTYPE_BASEMASK) {
  case ARGTYPE_BASE_AUTO:
    base = 0;
    break;
  case ARGTYPE_BASE_16:
    base = 16;
    break;
  case ARGTYPE_BASE_10:
  default:
    base = 10;
  }
  return base;

}

bool Vecs::trailing_data_allowed(uint32_t argtype)
{
  return (argtype & ARGTYPE_ALLOW_TRAILING_DATA) == ARGTYPE_ALLOW_TRAILING_DATA;
}

bool Vecs::is_integer(const QString& val, const QString& id, uint32_t argtype)
{
  bool ok;
  int base = integer_base(argtype);
  QString end;
  QString* endp = trailing_data_allowed(argtype) ? &end : nullptr;
  (void) parse_integer(val, id, &ok, endp, base);
  return ok;
}

int Vecs::convert_integer(const QString& val, const QString& id, QString* end, int base)
{
  // Fatal on conversion error
  constexpr bool* dieonerror = nullptr;
  return parse_integer(val, id, dieonerror, end, base);
}

bool Vecs::is_float(const QString& val, const QString& id, uint32_t argtype)
{
  bool ok;
  QString end;
  QString* endp = trailing_data_allowed(argtype) ? &end : nullptr;
  (void) parse_double(val, id, &ok, endp);
  return ok;
}

double Vecs::convert_float(const QString& val, const QString& id, QString* end)
{
  // Fatal on conversion error
  constexpr bool* dieonerror = nullptr;
  return parse_double(val, id, dieonerror, end);
}

bool Vecs::is_bool(const QString& val)
{
  return val.startsWith('y', Qt::CaseInsensitive) ||
         val.startsWith('n', Qt::CaseInsensitive) ||
         (!val.isEmpty() && val.at(0).isDigit());
}

void Vecs::free_options(QVector<arglist_t>* args)
{
  if (args && !args->isEmpty()) {
    assert(args->isDetached());
    for (auto& arg : *args) {
      if (arg.argval != nullptr) {
        arg.argval->reset();
      }
    }
  }
}

void Vecs::exit_vec(Format* fmt)
{
  (fmt->exit)();
  free_options(fmt->get_args());
}

void Vecs::exit_vecs()
{
  for (const auto& vec : d_ptr_->vec_list) {
    if (vec.vec != nullptr) {
      exit_vec(vec.vec);
    }
  }
  style_list.clear();
  style_list.squeeze();
}

void Vecs::assign_option(const QString& module, arglist_t& arg, const QString& val)
{
  if (arg.argval == nullptr) {
    fatal("%s: No local variable defined for option \"%s\"!\n", qPrintable(module), qPrintable(arg.argstring));
  }

  arg.argval->reset();

  if (val.isNull()) {
    return;
  }

  QString rval(val);
  QString id = QStringLiteral("%1(%2)").arg(module, arg.argstring);
  QString end;
  QString* endp = trailing_data_allowed(arg.argtype)? &end: nullptr;

  switch (arg.argtype & ARGTYPE_TYPEMASK) {
  case ARGTYPE_INT: {
    int result;
    if (val.isEmpty()) {
      rval = '0';
      result = 0;
    } else {
      // will fatal on conversion error
      result = convert_integer(val, id, endp, integer_base(arg.argtype));
    }
    auto* int_option = dynamic_cast<OptionInt*>(arg.argval);
    // TODO: enforce ARTYPE_INT <=> OptionInt
    if (int_option != nullptr) {
      int_option->set_result(result, end);
    }
  }
  break;
  case ARGTYPE_FLOAT: {
    double result;
    if (val.isEmpty()) {
      rval = '0';
      result = 0.0;
    } else {
      // will fatal on conversion error
      result = convert_float(val, id, endp);
    }
    auto* double_option = dynamic_cast<OptionDouble*>(arg.argval);
    // ARGTYPE_FLOAT <=> OptionDouble enforced in validate_arg
    if (double_option != nullptr) {
      double_option->set_result(result, end);
    }
  }
  break;
  case ARGTYPE_BOOL:
    if (val.isEmpty()) {
      rval = '1';
    } else {
      if (val.startsWith('y', Qt::CaseInsensitive)) {
        rval = '1';
      } else if (val.startsWith('n', Qt::CaseInsensitive)) {
        rval = '0';
      } else {
        // This works for decimal digits in the BMP (and thus represented by one QChar).
        if (val.at(0).isDigit()) {
          if (0 == val.at(0).digitValue()) {
            rval = '0';
          } else {
            rval = '1';
          }
        } else {
          warning("%s :Invalid logical value \"%s\" for option %s!\n", qPrintable(module), qPrintable(val), qPrintable(arg.argstring));
          rval = '0';
        }
      }
    }
    break;
  }

  arg.argval->set(rval);
  arg.argval->set_id(id);
}

void Vecs::disp_vec_options(const QString& vecname, const QVector<arglist_t>* args)
{
  if (args) {
    for (const auto& arg : *args) {
      if ((arg.argval != nullptr) && !arg.argval->isEmpty()) {
        printf("options: module/option=value: %s/%s=\"%s\"",
               qPrintable(vecname), qPrintable(arg.argstring), qPrintable(arg.argval->get()));
        if (case_ignore_strcmp(arg.defaultvalue, arg.argval->get()) == 0) {
          printf(" (=default)");
        }
        printf("\n");
      }
    }
  }
}

void Vecs::validate_options(const QStringList& options, const QVector<arglist_t>* args, const QString& name)
{
  for (const auto& option : options) {
    const QString option_name = option.left(option.indexOf('='));
    bool valid = false;
    if (args) {
      for (const auto& arg : *args) {
        if (option_name.compare(arg.argstring, Qt::CaseInsensitive) == 0) {
          valid = true;
          break;
        }
      }
    }
    if (!valid) {
      warning("'%s' is an unknown option to %s.\n", qPrintable(option_name), qPrintable(name));
    }
  }
}

void Vecs::prepare_format(const fmtinfo_t& fmtdata)
{
  QVector<arglist_t>* args = fmtdata->get_args();

  validate_options(fmtdata.options, args, fmtdata.fmtname);

  if (args && !args->isEmpty()) {
    assert(args->isDetached());
    for (auto& arg : *args) {
      if (!fmtdata.options.isEmpty()) {
        const QString opt = get_option(fmtdata.options, arg.argstring);
        if (!opt.isNull()) {
          assign_option(fmtdata.fmtname, arg, opt);
          continue;
        }
      }
      QString qopt = inifile_readstr(global_opts.inifile, fmtdata.fmtname, arg.argstring);
      if (qopt.isNull()) {
        qopt = inifile_readstr(global_opts.inifile, "Common format settings", arg.argstring);
      }
      if (qopt.isNull()) {
        assign_option(fmtdata.fmtname, arg, arg.defaultvalue);
      } else {
        assign_option(fmtdata.fmtname, arg, qopt);
      }
    }
  }

  if (global_opts.debug_level >= 1) {
    disp_vec_options(fmtdata.fmtname, args);
  }

  /*
   * For style based formats let xcsv know the style file.  Otherwise
   * make sure xcsv knows no style file is in use. This covers the case
   * that we are processing xcsv,style= and it was preceeded by an xcsv
   * format that utilized an internal style file.
   */
  auto* xcsvfmt = dynamic_cast<XcsvFormat*>(fmtdata.fmt);
  if (xcsvfmt != nullptr) {
    xcsvfmt->xcsv_setup_internal_style(fmtdata.style_filename);
  }
}

Vecs::fmtinfo_t Vecs::find_vec(const QString& fmtargstring)
{
  QStringList options = fmtargstring.split(',');
  if (options.isEmpty()) {
    fatal("A format name is required.\n");
  }
  const QString fmtname = options.takeFirst();

  for (const auto& vec : d_ptr_->vec_list) {
    if (fmtname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }

    return {vec.vec, vec.name, nullptr, options, vec.factory};
  }

  /*
   * Didn't find it in the table of "real" file types, so plan B
   * is to search the list of xcsv styles.
   */
  for (const auto& svec : std::as_const(style_list)) {
    if (fmtname.compare(svec.name,  Qt::CaseInsensitive) != 0) {
      continue;
    }

    return {d_ptr_->vec_list.at(0).vec, svec.name, svec.style_filename, options, d_ptr_->vec_list.at(0).factory};
  }

  /*
   * Not found.
   */
  return {};
}

/*
 * Find and return a specific argument in an arg list.
 * Modelled approximately after getenv.
 */
QString Vecs::get_option(const QStringList& options, const QString& argname)
{
  QString rval; // null

  for (const auto& option : options) {
    int split = option.indexOf('=');
    const QString option_name = option.left(split);
    if (option_name.compare(argname, Qt::CaseInsensitive) == 0) {
      /*
       * If we have something of the form "foo=bar"
       * return "bar".   Otherwise, we assume we have
       * simply "foo" so we return that.
       */
      if (split >= 0) { // we found an '='s.
        rval = option.mid(split + 1); // not null, possibly empty
        assert(!rval.isNull());
        break;
      } else {
        rval = ""; // not null, empty
        assert(!rval.isNull());
        break;
      }
    }
  }
  return rval;
}

QVector<Vecs::style_vec_t> Vecs::create_style_vec()
{
  QString styledir(":/style");
  QDir dir(styledir);
  if (!dir.isReadable()) {
    fatal(FatalMsg() << "style directory" << QFileInfo(styledir).absoluteFilePath() << "not readable.");
  }

  dir.setNameFilters(QStringList("*.style"));
  dir.setFilter(QDir::Files);
  dir.setSorting(QDir::Name);
  QFileInfoList fileinfolist = dir.entryInfoList();
  QVector<style_vec_t> slist;
  for (const auto& fileinfo : fileinfolist) {
    if (!fileinfo.isReadable()) {
      fatal(FatalMsg() << "Cannot open style file" << fileinfo.absoluteFilePath() << ".");
    }

    style_vec_t entry;
    entry.name = fileinfo.baseName();
    entry.style_filename = fileinfo.filePath();
    slist.append(entry);
  }
  return slist;
}

/*
 * Gather information relevant to serialization from the
 * vecs and style lists.  Sort and return the information.
 */
QVector<Vecs::vecinfo_t> Vecs::sort_and_unify_vecs() const
{
  QVector<vecinfo_t> svp;
  svp.reserve(d_ptr_->vec_list.size() + style_list.size());

  /* Gather relevant information for normal formats. */
  for (const auto& vec : d_ptr_->vec_list) {
    Format* fmt = (vec.factory != nullptr)? vec.factory("") : vec.vec;
    vecinfo_t info;
    info.name = vec.name;
    info.desc = vec.desc;
    info.extensions = vec.extensions;
    if (vec.parent.isEmpty()) {
      info.parent = vec.name;
    } else {
      info.parent = vec.parent;
    }
    info.type = fmt->get_type();
    info.cap = fmt->get_cap();
    const QVector<arglist_t>* args = fmt->get_args();
    if (args != nullptr) {
      for (const auto& arg : *args) {
        info.arginfo.append(arginfo_t(arg));
      }
    }
    svp.append(info);
    if (vec.factory != nullptr) {
      delete fmt;
    }
  }

  /* The style formats are based on the xcsv format,
   * Make sure we know which entry in the vector list that is.
   */
  assert(d_ptr_->vec_list.at(0).name.compare("xcsv", Qt::CaseInsensitive) == 0);

  Format* xcsvfmt = (d_ptr_->vec_list.at(0).factory != nullptr)? d_ptr_->vec_list.at(0).factory("") : d_ptr_->vec_list.at(0).vec;
  /* The style formats use a modified xcsv argument list that doesn't include
   * the option to set the style file.  Make sure we know which entry in
   * the argument list that is.
   */
  assert(case_ignore_strcmp(xcsvfmt->get_args()->at(0).helpstring,
                            "Full path to XCSV style file") == 0);

  /* Gather the relevant info for the style based formats. */
  for (const auto& svec : style_list) {
    XcsvStyle style = XcsvStyle::xcsv_read_style(svec.style_filename);
    vecinfo_t info;
    info.name = svec.name;
    info.desc = style.description;
    info.extensions = style.extension;
    info.parent = "xcsv";
    info.type = style.type;
    info.cap.fill(ff_cap_none, 3);
    switch (style.datatype) {
    case unknown_gpsdata:
    case wptdata:
      info.cap[ff_cap_rw_wpt] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    case trkdata:
      info.cap[ff_cap_rw_trk] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    case rtedata:
      info.cap[ff_cap_rw_rte] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    default:
      ;
    }
    /* Skip over the first help entry of the xcsv args.
     * We don't want to expose the
     * 'Full path to XCSV style file' argument to any
     * GUIs for an internal format.
     */
    const QVector<arglist_t>* args = xcsvfmt->get_args();
    if (args != nullptr) {
      bool first = true;
      for (const auto& arg : *args) {
        if (!first) {
          info.arginfo.append(arginfo_t(arg));
        }
        first = false;
      }
    }
    svp.append(info);
  }
  if (d_ptr_->vec_list.at(0).factory != nullptr) {
    delete xcsvfmt;
  }

  /*
   *  Display the available formats in a format that's easy for humans to
   *  parse for help on available command line options.
   */
  auto alpha = [](const vecinfo_t& a, const vecinfo_t& b)->bool {
    return QString::compare(a.desc, b.desc, Qt::CaseInsensitive) < 0;
  };

  /* Now that we have everything in an array, alphabetize them */
  std::sort(svp.begin(), svp.end(), alpha);

  return svp;
}

void Vecs::disp_vec(const QString& vecname) const
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    /*
     * Display info for all non-internal formats is vecname is empty,
     * otherwise just display info for the matching format.
     */
    if (vecname.isEmpty()? (vec.type == ff_type_internal) :
        (vecname.compare(vec.name, Qt::CaseInsensitive) != 0)) {
      continue;
    }

    printf("	%-20.20s  %-.50s\n", qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arginfo_t> args = vec.arginfo;
    for (const auto& arg : args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("	  %-18.18s    %s%-.50s%s\n",
               qPrintable(arg.argstring),
               (arg.argtype & ARGTYPE_TYPEMASK) ==
               ARGTYPE_BOOL ? "(0/1) " : "",
               qPrintable(arg.helpstring),
               (arg.argtype & ARGTYPE_REQUIRED) ? " (required)" : "");
      }
    }
  }
}

/*
 * Additional information for V1.
 * Output format type at front of line.
 */
void Vecs::disp_v1(ff_type t)
{
  const char* tstring;

  switch (t) {
  case ff_type_file:
    tstring = "file";
    break;
  case ff_type_serial:
    tstring = "serial";
    break;
  case ff_type_internal:
    tstring = "internal";
    break;
  default:
    tstring = "unknown";
    break;
  }
  printf("%s\t", tstring);
}

void Vecs::disp_v2(const vecinfo_t& v)
{
  for (const auto& i : v.cap) {
    putchar((i & ff_cap_read) ? 'r' : '-');
    putchar((i & ff_cap_write) ? 'w' : '-');
  }
  putchar('\t');
}

const char* Vecs::name_option(uint32_t type)
{
  const char* at[] = {
    "unknown",
    "integer",
    "float",
    "string",
    "boolean",
    "file",
    "outfile"
  };

  if ((type & ARGTYPE_TYPEMASK) <= 6) {
    return at[type & ARGTYPE_TYPEMASK];
  }
  return at[0];
}

void Vecs::disp_help_url(const vecinfo_t& vec, const QString& argstring)
{
  printf("\t" WEB_DOC_DIR "/fmt_%s.html", CSTR(vec.name));
  if (!argstring.isEmpty()) {
    printf("#fmt_%s_o_%s", CSTR(vec.name), CSTR(argstring));
  }
  printf("\n");
}


void Vecs::disp_v3(const vecinfo_t& vec)
{
  disp_help_url(vec, nullptr);
  const QVector<arginfo_t> args = vec.arginfo;
  for (const auto& arg : args) {
    if (!(arg.argtype & ARGTYPE_HIDDEN)) {
      printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
             CSTR(vec.name),
             CSTR(arg.argstring),
             CSTR(arg.helpstring),
             name_option(arg.argtype),
             CSTR(arg.defaultvalue),
             CSTR(arg.minvalue),
             CSTR(arg.maxvalue));
    }
    disp_help_url(vec, arg.argstring);
    printf("\n");
  }
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void Vecs::disp_formats(int version) const
{
  const auto svp = sort_and_unify_vecs();
  switch (version) {
  case 0:
  case 1:
  case 2:
  case 3:
    for (const auto& vec : svp) {
      /* Version 1 displays type at front of all types.
       * Version 0 skips internal types.
       */
      if (version > 0) {
        disp_v1(vec.type);
      } else {
        if (vec.type == ff_type_internal) {
          continue;
        }
      }
      if (version >= 2) {
        disp_v2(vec);
      }
      printf("%s\t%s\t%s%s%s\n", CSTR(vec.name),
             !vec.extensions.isEmpty() ? CSTR(vec.extensions) : "",
             CSTR(vec.desc),
             version >= 3 ? "\t" : "",
             version >= 3 ? CSTR(vec.parent) : "");
      if (version >= 3) {
        disp_v3(vec);
      }
    }
    break;
  default:
    ;
  }
}

//#define FIND_ALL_NULLPTR_ARGUMENTS
//#define FIND_ALL_EMPTY_ARGUMENT_LISTS

bool Vecs::validate_args(const QString& name, const QVector<arglist_t>* args)
{
  bool ok = true;

#ifdef FIND_ALL_NULLPTR_ARGUMENTS
  if (args == nullptr) {
    Warning() << name << "Is passing nullptr for arguments.";
  }
#endif

  if (args != nullptr) {
#ifdef FIND_ALL_EMPTY_ARGUMENT_LISTS
    if (args->isEmpty()) {
      Warning() << name << "It isn't necessary to use an empty argument list, you can pass nullptr.";
    }
#endif
    for (const auto& arg : *args) {
      QString id = QStringLiteral("%1(%2)").arg(name, arg.argstring);
      if (arg.argval == nullptr) {
        Warning() << name << "option" << arg.argstring << "does not point to an Option instance.";
        ok = false;
      }
      if ((arg.argtype & ARGTYPE_TYPEMASK) == ARGTYPE_INT) {
        if (!arg.defaultvalue.isNull() && !is_integer(arg.defaultvalue, id, arg.argtype)) {
          Warning() << name << "Int option" << arg.argstring << "default value" << arg.defaultvalue << "is not an integer.";
          ok = false;
        }
        if (!arg.minvalue.isNull() && !is_integer(arg.minvalue, id, arg.argtype)) {
          Warning() << name << "Int option" << arg.argstring << "minimum value" << arg.minvalue << "is not an integer.";
          ok = false;
        }
        if (!arg.maxvalue.isNull() && !is_integer(arg.maxvalue, id, arg.argtype)) {
          Warning() << name << "Int option" << arg.argstring << "maximum value" << arg.maxvalue << "is not an integer.";
          ok = false;
        }
        // ARGTYPE_INT => OptionInt
        if (const auto* opt = dynamic_cast<const OptionInt*>(arg.argval); opt == nullptr) {
          Warning() << name << "Int option" << arg.argstring << "argval is not of class OptionInt";
          ok = false;
        }
      } else if ((arg.argtype & ARGTYPE_TYPEMASK) == ARGTYPE_FLOAT) {
        if (!arg.defaultvalue.isNull() && !is_float(arg.defaultvalue, id, arg.argtype)) {
          Warning() << name << "Float option" << arg.argstring << "default value" << arg.defaultvalue << "is not an float.";
          ok = false;
        }
        if (!arg.minvalue.isNull() && !is_float(arg.minvalue, id, arg.argtype)) {
          Warning() << name << "Float option" << arg.argstring << "minimum value" << arg.minvalue << "is not an float.";
          ok = false;
        }
        if (!arg.maxvalue.isNull() && !is_float(arg.maxvalue, id, arg.argtype)) {
          Warning() << name << "Float option" << arg.argstring << "maximum value" << arg.maxvalue << "is not an float.";
          ok = false;
        }
        // ARGTYPE_FLOAT => OptionDouble
        if (const auto* opt = dynamic_cast<const OptionDouble*>(arg.argval); opt == nullptr) {
          Warning() << name << "Float option" << arg.argstring << "argval is not of class OptionDouble";
          ok = false;
        }
      } else if ((arg.argtype & ARGTYPE_TYPEMASK) == ARGTYPE_BOOL) {
        if (!arg.defaultvalue.isNull() && !is_bool(arg.defaultvalue)) {
          Warning() << name << "Bool option" << arg.argstring << "default value" << arg.defaultvalue << "is not an bool.";
          ok = false;
        }
        if (!arg.minvalue.isNull() && !is_bool(arg.minvalue)) {
          Warning() << name << "Bool option" << arg.argstring << "minimum value" << arg.minvalue << "is not an bool.";
          ok = false;
        }
        if (!arg.maxvalue.isNull() && !is_bool(arg.maxvalue)) {
          Warning() << name << "Bool option" << arg.argstring << "maximum value" << arg.maxvalue << "is not an bool.";
          ok = false;
        }
        // ARGTYPE_BOOL => OptionBool
        if (const auto* opt = dynamic_cast<const OptionBool*>(arg.argval); opt == nullptr) {
          Warning() << name << "Bool option" << arg.argstring << "argval is not of class OptionBool";
          ok = false;
        }
      }
      // OptionInt => ARGTYPE_INT
      if (const auto* opt = dynamic_cast<const OptionInt*>(arg.argval); opt != nullptr) {
        if ((arg.argtype & ARGTYPE_TYPEMASK) != ARGTYPE_INT) {
          Warning() << name << "OptionInt" << arg.argstring << "is of not of type ARGTYPE_INT";
        }
      }
      // OptionDouble => ARGTYPE_DOUBLE
      if (const auto* opt = dynamic_cast<const OptionDouble*>(arg.argval); opt != nullptr) {
        if ((arg.argtype & ARGTYPE_TYPEMASK) != ARGTYPE_FLOAT) {
          Warning() << name << "OptionDouble" << arg.argstring << "is of not of type ARGTYPE_FLOAT";
        }
      }
      // OptionBool => ARGTYPE_BOOL
      if (const auto* opt = dynamic_cast<const OptionBool*>(arg.argval); opt != nullptr) {
        if ((arg.argtype & ARGTYPE_TYPEMASK) != ARGTYPE_BOOL) {
          Warning() << name << "OptionBool" << arg.argstring << "is of not of type ARGTYPE_BOOL";
        }
      }
    }
  }

  return ok;
}

bool Vecs::validate_vec(const vecs_t& vec)
{
  Format* fmt = (vec.factory != nullptr)? vec.factory("") : vec.vec;
  bool ok = validate_args(vec.name, fmt->get_args());
  if (vec.factory != nullptr) {
    delete fmt;
  }

  return ok;
}

bool Vecs::validate_formats() const
{
  bool ok = true;

  for (const auto& vec : d_ptr_->vec_list) {
    ok = validate_vec(vec) && ok;
  }

  return ok;
}
