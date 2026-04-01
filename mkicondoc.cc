// Display all the Garmin icons that we know about so we can copy/paste
// into our doc, xmldoc/chapters/garmin_icons.xml.

#include <algorithm>             // for sort
#include <cstdio>                // for printf

#include <QString>               // for QString, operator<
#include <QVector>               // for QVector<>::iterator, QVector

#include "garmin_icon_tables.h"  // for icon_mapping_t, garmin_icon_table, garmin_smart_icon_table


int main()
{
  QVector<icon_mapping_t> table;
  table.append(garmin_icon_table);
  table.append(garmin_smart_icon_table);

  auto sort_lambda = [](const icon_mapping_t& a, const icon_mapping_t& b)->bool {
    return a.icon < b.icon;
  };
  std::sort(table.begin(), table.end(), sort_lambda);

  for (const auto& entry : table) {
    printf("    <member>%s</member>\n", qPrintable(entry.icon));
  }

  return 0;
}
