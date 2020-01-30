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

#include <QtCore/QByteArray>    // for QByteArray
#include <QtCore/QString>       // for QString
#include <QtCore/QStringList>   // for QStringList
#include <QtCore/QVector>       // for QVector<>::iterator, QVector
#include <QtCore/Qt>            // for CaseInsensitive
#include <QtCore/QtGlobal>      // for qPrintable

#include <algorithm>            // for sort
#include <cassert>              // for assert
#include <cctype>               // for isdigit
#include <cstdio>               // for printf, putchar, sscanf, size_t

#include "defs.h"
#include "vecs.h"
#include "format.h"
#include "gbversion.h"          // for WEB_DOC_DIR
#include "inifile.h"            // for inifile_readstr
#include "legacyformat.h"
#include "src/core/logging.h"   // for Warning
#include "xcsv.h"               // for xcsv_setup_internal_style, XcsvStyle, xcsv_read_internal_style


#define MYNAME "vecs"

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

void Vecs::init_vecs()
{
  for (const auto& vec : vec_list) {
    QVector<arglist_t>* args = vec.vec->get_args();
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        arg.argvalptr = nullptr;
        if (arg.argval) {
          *arg.argval = nullptr;
        }
      }
    }
  }
}

int Vecs::is_integer(const char* c)
{
  return isdigit(c[0]) || ((c[0] == '+' || c[0] == '-') && isdigit(c[1]));
}

void Vecs::exit_vecs()
{
  for (const auto& vec : vec_list) {
    (vec.vec->exit)();
    QVector<arglist_t>* args = vec.vec->get_args();
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        if (arg.argvalptr) {
          xfree(arg.argvalptr);
          *arg.argval = arg.argvalptr = nullptr;
        }
      }
    }
  }
}

void Vecs::assign_option(const QString& module, arglist_t* arg, const char* val)
{
  const char* c;

  if (arg->argval == nullptr) {
    fatal("%s: No local variable defined for option \"%s\"!", qPrintable(module), arg->argstring);
  }

  if (arg->argvalptr != nullptr) {
    xfree(arg->argvalptr);
    arg->argvalptr = nullptr;
  }
  if (arg->argval) {
    *arg->argval = nullptr;
  }

  if (val == nullptr) {
    return;
  }

  // Fixme - this is probably somewhere between wrong and less than great.  If you have an option "foo"
  // and want to set it to the value "foo", this code will prevent that from happening, but we seem to have
  // code all over the place that relies on this. :-/
  if (case_ignore_strcmp(val, arg->argstring) == 0) {
    c = "";
  } else {
    c = val;
  }

  switch (arg->argtype & ARGTYPE_TYPEMASK) {
  case ARGTYPE_INT:
    if (*c == '\0') {
      c = "0";
    } else {
      int test;
      is_fatal(1 != sscanf(c, "%d", &test),
               "%s: Invalid parameter value %s for option %s", qPrintable(module), val, arg->argstring);
    }
    break;
  case ARGTYPE_FLOAT:
    if (*c == '\0') {
      c = "0";
    } else {
      double test;
      is_fatal(1 != sscanf(c, "%lf", &test),
               "%s: Invalid parameter value %s for option %s", qPrintable(module), val, arg->argstring);
    }
    break;
  case ARGTYPE_BOOL:
    if (*c == '\0') {
      c = "1";
    } else {
      switch (*c) {
      case 'Y':
      case 'y':
        c = "1";
        break;
      case 'N':
      case 'n':
        c = "0";
        break;
      default:
        if (isdigit(*c)) {
          if (*c == '0') {
            c = "0";
          } else {
            c = "1";
          }
        } else {
          warning(MYNAME ": Invalid logical value '%s' (%s)!\n", c, qPrintable(module));
          c = "0";
        }
        break;
      }
    }
    break;
  }

  /* for bool options without default: don't set argval if "FALSE" */

  if (((arg->argtype & ARGTYPE_TYPEMASK) == ARGTYPE_BOOL) &&
      (*c == '0') && (arg->defaultvalue == nullptr)) {
    return;
  }
  *arg->argval = arg->argvalptr = xstrdup(c);
}

void Vecs::disp_vec_options(const QString& vecname, const QVector<arglist_t>* args)
{
  if (args) {
    for (const auto& arg : *args) {
      if (*arg.argval && arg.argval) {
        printf("options: module/option=value: %s/%s=\"%s\"",
               qPrintable(vecname), arg.argstring, *arg.argval);
        if (arg.defaultvalue && (case_ignore_strcmp(arg.defaultvalue, *arg.argval) == 0)) {
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

Format* Vecs::find_vec(const QString& vecname)
{
  QStringList options = vecname.split(',');
  if (options.isEmpty()) {
    fatal("A format name is required.\n");
  }
  const QString svecname = options.takeFirst();

  for (const auto& vec : vec_list) {
    if (svecname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }

    QVector<arglist_t>* args = vec.vec->get_args();

    validate_options(options, args, vec.name);

    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        if (!options.isEmpty()) {
          const QString opt = get_option(options, arg.argstring);
          if (!opt.isNull()) {
            assign_option(vec.name, &arg, CSTR(opt));
            continue;
          }
        }
        QString qopt = inifile_readstr(global_opts.inifile, vec.name, arg.argstring);
        if (qopt.isNull()) {
          qopt = inifile_readstr(global_opts.inifile, "Common format settings", arg.argstring);
        }
        if (qopt.isNull()) {
          assign_option(vec.name, &arg, arg.defaultvalue);
        } else {
          assign_option(vec.name, &arg, CSTR(qopt));
        }
      }
    }

    if (global_opts.debug_level >= 1) {
      disp_vec_options(vec.name, args);
    }

#if CSVFMTS_ENABLED
    /*
     * If this happens to be xcsv,style= and it was preceeded by an xcsv
     * format that utilized an internal style file, then we need to let
     * xcsv know the internal style file is no longer in play.
     */
     xcsv_fmt.xcsv_setup_internal_style(nullptr);
#endif // CSVFMTS_ENABLED
    vec.vec->set_name(vec.name);	/* needed for session information */
    return vec.vec;

  }

  /*
   * Didn't find it in the table of "real" file types, so plan B
   * is to search the list of xcsv styles.
   */
  for (const auto& svec : style_list) {
    if (svecname.compare(svec.name,  Qt::CaseInsensitive) != 0) {
      continue;
    }

    QVector<arglist_t>* xcsv_args = vec_list.at(0).vec->get_args();

    validate_options(options, xcsv_args, svec.name);

    if (xcsv_args && !xcsv_args->isEmpty()) {
      assert(xcsv_args->isDetached());
      for (auto& arg : *xcsv_args) {
        if (!options.isEmpty()) {
          const QString opt = get_option(options, arg.argstring);
          if (!opt.isNull()) {
            assign_option(svec.name, &arg, CSTR(opt));
            continue;
          }
        }
        QString qopt = inifile_readstr(global_opts.inifile, svec.name, arg.argstring);
        if (qopt.isNull()) {
          qopt = inifile_readstr(global_opts.inifile, "Common format settings", arg.argstring);
        }
        if (qopt.isNull()) {
          assign_option(svec.name, &arg, arg.defaultvalue);
        } else {
          assign_option(svec.name, &arg, CSTR(qopt));
        }
      }
    }

    if (global_opts.debug_level >= 1) {
      disp_vec_options(svec.name, xcsv_args);
    }
#if CSVFMTS_ENABLED
    xcsv_fmt.xcsv_setup_internal_style(svec.style_buf);
#endif // CSVFMTS_ENABLED

    vec_list[0].vec->set_name(svec.name);	/* needed for session information */
    return vec_list[0].vec;
  }

  /*
   * Not found.
   */
  return nullptr;
}

/*
 * Find and return a specific argument in an arg list.
 * Modelled approximately after getenv.
 */
QString Vecs::get_option(const QStringList& options, const char* argname)
{
  QString rval;

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
        rval = option_name; // not null, possibly empty.
        assert(!rval.isNull());
        break;
      }
    }
  }
  return rval;
}

/*
 * Gather information relevant to serialization from the
 * vecs and style lists.  Sort and return the information.
 */
QVector<Vecs::vecinfo_t> Vecs::sort_and_unify_vecs() const
{
  QVector<vecinfo_t> svp;
  svp.reserve(vec_list.size() + style_list.size());

  /* Gather relevant information for normal formats. */
  for (const auto& vec : vec_list) {
    vecinfo_t info;
    info.name = vec.name;
    info.desc = vec.desc;
    info.extensions = vec.extensions;
    if (vec.parent.isEmpty()) {
      info.parent = vec.name;
    } else {
      info.parent = vec.parent;
    }
    info.type = vec.vec->get_type();
    info.cap = vec.vec->get_cap();
    const QVector<arglist_t>* args = vec.vec->get_args();
    if (args != nullptr) {
      for (const auto& arg : *args) {
        info.arginfo.append(arginfo_t(arg));
      }
    }
    svp.append(info);
  }

#if CSVFMTS_ENABLED
  /* The style formats are based on the xcsv format,
   * Make sure we know which entry in the vector list that is.
   */
  assert(vec_list.at(0).name.compare("xcsv", Qt::CaseInsensitive) == 0);
  /* The style formats use a modified xcsv argument list that doesn't include
   * the option to set the style file.  Make sure we know which entry in
   * the argument list that is.
   */
  assert(case_ignore_strcmp(vec_list.at(0).vec->get_args()->at(0).helpstring,
                            "Full path to XCSV style file") == 0);

  /* Gather the relevant info for the style based formats. */
  for (const auto& svec : style_list) {
    XcsvStyle style = XcsvStyle::xcsv_read_internal_style(svec.style_buf);
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
    const QVector<arglist_t>* args = vec_list.at(0).vec->get_args();
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
#endif // CSVFMTS_ENABLED

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

#define VEC_FMT "	%-20.20s  %-.50s\n"

void Vecs::disp_vecs() const
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    if (vec.type == ff_type_internal)  {
      continue;
    }
    printf(VEC_FMT, qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arginfo_t> args = vec.arginfo;
    for (const auto& arg : args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("	  %-18.18s    %s%-.50s %s\n",
               qPrintable(arg.argstring),
               (arg.argtype & ARGTYPE_TYPEMASK) ==
               ARGTYPE_BOOL ? "(0/1) " : "",
               qPrintable(arg.helpstring),
               (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

void Vecs::disp_vec(const QString& vecname) const
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    if (vecname.compare(vec.name, Qt::CaseInsensitive) != 0)  {
      continue;
    }

    printf(VEC_FMT, qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arginfo_t> args = vec.arginfo;
    for (const auto& arg : args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("	  %-18.18s    %s%-.50s %s\n",
               qPrintable(arg.argstring),
               (arg.argtype & ARGTYPE_TYPEMASK) ==
               ARGTYPE_BOOL ? "(0/1) " : "",
               qPrintable(arg.helpstring),
               (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
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
  for (auto& i : v.cap) {
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
             arg.defaultvalue.isEmpty() ? "" : CSTR(arg.defaultvalue),
             arg.minvalue.isEmpty() ? "" : CSTR(arg.minvalue),
             arg.maxvalue.isEmpty() ? "" : CSTR(arg.maxvalue));
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
      if (arg.argtype == ARGTYPE_INT) {
        if (arg.defaultvalue &&
            ! is_integer(arg.defaultvalue)) {
          Warning() << name << "Int option" << arg.argstring << "default value" << arg.defaultvalue << "is not an integer.";
          ok = false;
        }
        if (arg.minvalue &&
            ! is_integer(arg.minvalue)) {
          Warning() << name << "Int option" << arg.argstring << "minimum value" << arg.minvalue << "is not an integer.";
          ok = false;
        }
        if (arg.maxvalue &&
            ! is_integer(arg.maxvalue)) {
          Warning() << name << "Int option" << arg.argstring << "maximum value" << arg.maxvalue << "is not an integer.";
          ok = false;
        }
      }
    }
  }

  return ok;
}

bool Vecs::validate_vec(const vecs_t& vec)
{
  bool ok = validate_args(vec.name, vec.vec->get_args());

  return ok;
}

bool Vecs::validate_formats() const
{
  bool ok = true;

  for (const auto& vec : vec_list) {
    ok = validate_vec(vec) && ok;
  }

  return ok;
}
