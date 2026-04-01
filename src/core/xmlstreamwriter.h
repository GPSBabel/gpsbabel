/*
    Copyright (C) 2013 Robert Lipe, gpsbabel.org

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

#ifndef XMLSTREAMWRITER_H
#define XMLSTREAMWRITER_H

#include <QList>             // for QList
#include <QString>           // for QString
#include <QXmlStreamWriter>  // for QXmlStreamWriter
#include <utility>

namespace gpsbabel
{

class XmlStreamWriter : public QXmlStreamWriter
{
public:
  using QXmlStreamWriter::QXmlStreamWriter;

  /* Member Functions */

  void stackAttribute(const QString& name, const QString& value);
  void stackEndElement();
  void stackNamespace(const QString& namespaceUri, const QString& prefix);
  void stackOptionalStartElement(const QString& name);
  void stackOptionalTextElement(const QString& name, const QString& text);
  void stackStartElement(const QString& name);
  void stackTextElement(const QString& name, const QString& text);

  void writeOptionalTextElement(const QString& qualifiedName, const QString& text);

private:
  /* Types */

  enum class xml_wrt_cmd_t {
    start_element,
    attribute,
    name_space,
    text_element,
    end_element
  };

  struct xml_command {
    explicit xml_command(xml_wrt_cmd_t t,
                         QString n = QString(),
                         QString v = QString())
      : type(t), name(std::move(n)), value(std::move(v)) {}

    xml_wrt_cmd_t type;
    QString name;
    QString value;
  };

  using xml_stack_t = QList<xml_command>;

  struct xml_stack_list_entry_t {
    void append(const xml_stack_list_entry_t& other)
    {
      stack.append(other.stack);
      element_count += other.element_count;
    }

    void append(const xml_command& cmd)
    {
      stack.append(cmd);
      switch (cmd.type) {
      case xml_wrt_cmd_t::start_element:
        ++element_count;
        ++element_depth;
        break;
      case xml_wrt_cmd_t::text_element:
        ++element_count;
        break;
      case xml_wrt_cmd_t::end_element:
        --element_depth;
        break;
      case xml_wrt_cmd_t::attribute:
      case xml_wrt_cmd_t::name_space:
        break;
      }
    }

    xml_stack_t stack;
    int element_count{0};
    int element_depth{0};
  };

  /* Member Functions */

  xml_stack_list_entry_t& activeStack();

  /* Data Members */

  QList<xml_stack_list_entry_t> stack_list;

};

} // namespace gpsbabel

#endif // XMLSTREAMWRITER_H
