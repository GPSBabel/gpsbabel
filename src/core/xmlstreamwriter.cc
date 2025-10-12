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

#include "src/core/xmlstreamwriter.h"

#include <QString>                  // for QString
#include <QXmlStreamWriter>         // for QXmlStreamWriter
#include <QtGlobal>                 // for QT_VERSION, QT_VERSION_CHECK

#include "defs.h"

// As this code began in C, we have several hundred places that write
// c strings.  Add a test that the string contains anything useful
// before serializing an empty tag.
// We rely on Qt to strip out characters that are illegal in xml.  These can
// creep into our structures from other formats where they are legal.

// Verify Qt is new enough to strip out illegal characters.
// This fix went into Qt 5.11.0. See
// https://github.com/GPSBabel/gpsbabel/issues/637
// https://bugreports.qt.io/browse/QTBUG-63150
// https://github.com/qt/qtbase/commit/3b5b8f1d4ab8092e5dd337b7b4e32d85fda2e0b7
#if (QT_VERSION < QT_VERSION_CHECK(5, 11, 0))
#error We rely on the fix for QTBUG-63150 introduced in Qt 5.11.0.
#endif


namespace gpsbabel
{
XmlStreamWriter::xml_stack_list_entry_t& XmlStreamWriter::activeStack()
{
  if (stack_list.isEmpty()) {
    gbFatal("xmlstreamwriter: programming error: the stack* functions are used incorrectly.\n");
  }
  return stack_list.last();
}

void XmlStreamWriter::stackAttribute(const QString& name, const QString& value)
{
  activeStack().append(xml_command(xml_wrt_cmd_t::attribute, name, value));
}

void XmlStreamWriter::stackEndElement()
{
  xml_stack_list_entry_t& active_stack = activeStack();
  active_stack.append(xml_command(xml_wrt_cmd_t::end_element));

  // Has the active_stack OptionalStartElement been paired with an EndElement?
  if (active_stack.element_depth == 0) { // yes
    const xml_stack_list_entry_t completed_stack = stack_list.takeLast();
    // Does the completed_stack OptionalStartElement have any content?
    if (completed_stack.element_count > 1) { // yes
      // Is this the initial OptionalStartElement?
      if (!stack_list.isEmpty()) { // no.  append stack contents to parent.
        stack_list.last().append(completed_stack);
      } else { // yes.  write the stack contents.
        for (const auto& command: completed_stack.stack) {
          switch (command.type) {
          case xml_wrt_cmd_t::start_element:
            QXmlStreamWriter::writeStartElement(command.name);
            break;
          case xml_wrt_cmd_t::attribute:
            QXmlStreamWriter::writeAttribute(command.name, command.value);
            break;
          case xml_wrt_cmd_t::name_space:
            QXmlStreamWriter::writeNamespace(command.name, command.value);
            break;
          case xml_wrt_cmd_t::text_element:
            QXmlStreamWriter::writeTextElement(command.name, command.value);
            break;
          case xml_wrt_cmd_t::end_element:
            QXmlStreamWriter::writeEndElement();
            break;
          }
        }
      }
    } // else {no.  empty OptionalStartElement is discarded.}
  }
}

void XmlStreamWriter::stackNamespace(const QString& namespaceUri, const QString& prefix)
{
  activeStack().append(xml_command(xml_wrt_cmd_t::name_space, namespaceUri, prefix));
}

/*
 * Start an element that will be written if and only if it has children.
 *
 * Usage:
 * 1. stackOptionalStartElement must be the first stack*() method called.
 * 2. stackOptionalStartElement must be paired with a subsequent
 *    stackEndElement.
 * 3. write*() methods should not be called until the initial optional start
 *    element has paired with a subsequent stackEndElement.
 */
void XmlStreamWriter::stackOptionalStartElement(const QString& name)
{
  stack_list.append(xml_stack_list_entry_t());
  stackStartElement(name);
}

void XmlStreamWriter::stackOptionalTextElement(const QString& name, const QString& text)
{
  if (!text.isEmpty()) {
    stackTextElement(name, text);
  }
}

void XmlStreamWriter::stackStartElement(const QString& name)
{
  activeStack().append(xml_command(xml_wrt_cmd_t::start_element, name));
}

void XmlStreamWriter::stackTextElement(const QString& name, const QString& text)
{
  activeStack().append(xml_command(xml_wrt_cmd_t::text_element, name, text));
}

// Don't emit the element if there's nothing interesting in it.
void XmlStreamWriter::writeOptionalTextElement(const QString& qualifiedName, const QString& text)
{
  if (!text.isEmpty()) {
    QXmlStreamWriter::writeTextElement(qualifiedName, text);
  }
}

} // namespace gpsbabel
