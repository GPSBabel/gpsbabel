#include <QApplication>
#include <QFile>
#include <QList>
#include <QTextStream>

#include "format.h"
#include "formatload.h"

QTextStream* generate_output_stream;

int main(int argc, char** argv)
{
  QApplication app(argc, argv);
  QStringList qargs = QApplication::arguments();

  QList<Format> formatList;

  if (qargs.size() != 2) {
    qFatal("Usage: %s output_file_name", qPrintable(qargs.at(0)));
  }

  QFile generate_output_file(qargs.at(1));
  bool status = generate_output_file.open(QIODevice::WriteOnly);
  if (!status) {
    qFatal("Could not open %s for write!", qPrintable(qargs.at(1)));
  }
  generate_output_stream = new QTextStream(&generate_output_file);

  bool fmtstatus = FormatLoad().getFormats(formatList);

  generate_output_stream->flush();
  generate_output_file.close();
  delete generate_output_stream;

  return fmtstatus ? 0 : 1;
}
