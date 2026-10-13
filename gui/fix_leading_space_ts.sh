#!/bin/bash -e
for file in gpsbabelfe*.ts; do

  sed -i -e '/filename="trackui.ui" line="235"/{:find2;n;/<source>/!b find2;s|<source> |<source>|;:find3;n;/<translation/!b find3;s|\(<translation[^>]*>\) |\1|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="274"/{:find2;n;/<source>/!b find2;s|<source> |<source>|;:find3;n;/<translation/!b find3;s|\(<translation[^>]*>\) |\1|;}' "$file"
done
