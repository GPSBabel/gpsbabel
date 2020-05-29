// Display all the Garmin icons that we know about so we can copy/paste
// into our doc, xmldoc/chapters/garmin_icons.xml.
// cd mkicondoc && qmake && make

#include <algorithm>             // for sort
#include <cstdio>                // for printf
#include <cstring>               // for strcmp

#include <QtCore/QVector>        // for QVector<>::iterator, QVector

#include "garmin_icon_tables.h"  // for garmin_icon_table, garmin_smart_icon_table
#include "garmin_tables.h"       // for icon_mapping_t


int main()
{
  QVector<icon_mapping_t> table;
  for (const icon_mapping_t* entry = garmin_icon_table; entry->icon; entry++) {
    table.append(*entry);
  }
  for (const icon_mapping_t* entry = garmin_smart_icon_table; entry->icon; entry++) {
    table.append(*entry);
  }
  
  auto sort_lambda = [](const icon_mapping_t& a, const icon_mapping_t& b)->bool {
    return strcmp(a.icon, b.icon) < 0;
  };
  std::sort(table.begin(), table.end(), sort_lambda);

  for (const auto& entry : table) {
    printf("    <member>%s</member>\n",entry.icon);
  }

  return 0;
}
