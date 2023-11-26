/*
    Header for our common utilities for XML-based formats.

    Copyright (C) 2004, 2005, 2006, 2007 Robert Lipe, robertlipe@usa.net

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

#ifndef XMLGENERIC_H_INCLUDED_
#define XMLGENERIC_H_INCLUDED_

#include <cassert>               // for assert

#include <QByteArray>            // for QByteArray
#include <QHash>                 // for QHash
#include <QList>                 // for QList
#include <QRegularExpression>    // for QRegularExpression
#include <QString>               // for QString
#include <QStringView>           // for QStringView
#include <QTextCodec>            // for QTextCodec
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <QXmlStreamReader>      // for QXmlStreamReader


enum class xg_cb_type {
  cb_unknown = 0,
  cb_start,
  cb_cdata,
  cb_end,
};

/*
 *  xml_init will build and own a table of XgFunctor and/or
 *  XgFunctionPtrCallback entries from a list
 *  of non-static member functions and/or function pointers.
 *
 *  QList<XmlGenericReader::xg_fmt_map_entry<SomeFormat>> some_map = {
 *    {&SomeFormat::memberfn, cb_start, "/Placemark"},
 *    {staticfn, cb_cdata, "/Placemark/coord"},
 *  };
 *
 *  The this pointer from the Format instance must be passed if any
 *  of the callbacks are member functions, otherwise nullptr can be passed
 *  as this.
 *
 *  xml_init(fname, this, some_map, encoding, ignorelist, skiplist);
 *
 */
class XmlGenericReader
{
public:
  /* Types */

  // formats pass a list containing member function pointers and/or function pointers.
  template<class MyFormat>
  struct xg_fmt_map_entry {
    // Constructor from a Member Function Pointer
    using XgMfpCb = void (MyFormat::*)(const QString&, const QXmlStreamAttributes*);
    xg_fmt_map_entry(XgMfpCb mfp, xg_cb_type ty, const char* tp) : tag_mfp_cb(mfp), cb_type(ty), tag_pattern(tp) {}
    // Constructor from a Function Pointer.
    using XgFpCb = void (const QString&, const QXmlStreamAttributes*);
    xg_fmt_map_entry(XgFpCb fp, xg_cb_type ty, const char* tp) : tag_fp_cb(fp), cb_type(ty), tag_pattern(tp) {}

    /* Data Members */

    XgMfpCb tag_mfp_cb{nullptr};
    XgFpCb* tag_fp_cb{nullptr};
    xg_cb_type cb_type{xg_cb_type::cb_unknown};
    const char* tag_pattern{nullptr};
  };

  /* Special Member Functions */

  XmlGenericReader() = default;
  XmlGenericReader(const XmlGenericReader&) = delete;
  XmlGenericReader& operator=(const XmlGenericReader&) = delete;
  XmlGenericReader(XmlGenericReader&&) = delete;
  XmlGenericReader& operator=(XmlGenericReader&&) = delete;
  ~XmlGenericReader();

  /* Member Functions */

  template<class MyFormat>
  void xml_init(const QString& fname, MyFormat* instance, const QList<xg_fmt_map_entry<MyFormat>>& tbl,
                const char* encoding = nullptr,
                const char* const* ignorelist = nullptr,
                const char* const* skiplist = nullptr)
  {
    xg_tag_tbl = build_xg_tag_map(instance, tbl);

    xml_common_init(fname, encoding, ignorelist, skiplist);
  }

  void xml_deinit();
  void xml_read();
  void xml_readstring(const char* str);
  void xml_readprefixstring(const char* str);
  void xml_readunicode(const QString& str);

private:
  /* Types */

  class XgCallbackBase
  {
  public:
    XgCallbackBase() = default;
    virtual ~XgCallbackBase() = default;
    XgCallbackBase(const XgCallbackBase&) = delete;
    XgCallbackBase& operator=(const XgCallbackBase&) = delete;
    XgCallbackBase(XgCallbackBase&&) = delete;
    XgCallbackBase& operator=(XgCallbackBase&&) = delete;

    virtual void operator()(const QString& string, const QXmlStreamAttributes* attrs) const = 0;
  };

  template<class XgFormat>
  class XgFunctor : public XgCallbackBase
  {
  public:
    using XgCb = void (XgFormat::*)(const QString&, const QXmlStreamAttributes*);
    XgFunctor(XgFormat* obj, XgCb cb) : that_(obj), cb_(cb) {}
    void operator()(const QString& string, const QXmlStreamAttributes* attrs) const override
    {
      (that_->*cb_)(string, attrs);
    }

  private:
    XgFormat* that_;
    XgCb cb_;
  };

  class XgFunctionPtrCallback : public XgCallbackBase
  {
  public:
    using XgCb = void (const QString&, const QXmlStreamAttributes*);
    explicit XgFunctionPtrCallback(XgCb cb) : cb_(cb) {}
    void operator()(const QString& string, const QXmlStreamAttributes* attrs) const override
    {
      (*cb_)(string, attrs);
    }

  private:
    XgCb* cb_;
  };

  // xml processing uses a list of xg_tag_map_entries.
  struct xg_tag_map_entry {
    XgCallbackBase* tag_cb{nullptr};
    xg_cb_type cb_type{xg_cb_type::cb_unknown};
    QRegularExpression tag_re;
  };

  enum class xg_shortcut {
    sc_none = 0,
    sc_skip,
    sc_ignore
  };

  /* Member Functions */

  XgCallbackBase* xml_tbl_lookup(const QString& tag, xg_cb_type cb_type);
  void xml_common_init(const QString& fname, const char* encoding,
                       const char* const* ignorelist, const char* const* skiplist);
  xg_shortcut xml_shortcut(QStringView name);
  void xml_run_parser(QXmlStreamReader& reader);

  // translate xg_fmt_map_entries to xg_tag_map_entries.
  template<class MyFormat>
  static QList<xg_tag_map_entry>* build_xg_tag_map(MyFormat* instance, const QList<xg_fmt_map_entry<MyFormat>>& map)
  {
    auto* tag_tbl = new QList<xg_tag_map_entry>;
    for (const auto& entry : map) {
      xg_tag_map_entry tme;
      if (entry.tag_mfp_cb != nullptr) {
        tme.tag_cb = new XgFunctor<MyFormat>(instance, entry.tag_mfp_cb);
      } else {
        tme.tag_cb = new XgFunctionPtrCallback(entry.tag_fp_cb);
      }
      QRegularExpression re(QRegularExpression::anchoredPattern(entry.tag_pattern));
      assert(re.isValid());
      tme.cb_type = entry.cb_type;
      tme.tag_re = re;
      tag_tbl->append(tme);
    }
    return tag_tbl;
  }

  /* Data Members */

  const QList<xg_tag_map_entry>* xg_tag_tbl{nullptr};
  QHash<QString, xg_shortcut>* xg_shortcut_taglist{nullptr};

  QString rd_fname;
  QByteArray reader_data;
  QTextCodec* codec{nullptr};  // Qt has no vanilla ASCII encoding =(

};

#endif  // XMLGENERIC_H_INCLUDED_
