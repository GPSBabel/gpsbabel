#include <QtCore/QList>
#include <QtWidgets/QApplication>

#include "format.h"
#include "formatload.h"

int main(int argc, char** argv)
{
  QApplication app(argc, argv);
  QList<Format> formatList;
  return FormatLoad().getFormats(formatList) ? 0 : 1;
}
