#!/bin/bash -e
for file in gpsbabelfe*.ts; do
#  sed -i -e '/INITIAL_PATTERN/{:find2;n;/SECOND_PATTERN/!b find2;s|SECOND_OLD|SECOND_NEW|;:find3;n;/THIRD_PATTERN/!b find3;s|THIRD_OLD|THIRD_NEW|;}' "$file"

  sed -i -e '/filename="advui.ui" line="20"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"

  sed -i -e '/filename="filterui.ui" line="17"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"

  sed -i -e '/filename="mainwinui.ui" line="21"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="129"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="207"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="427"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="460"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="545"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="623"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="mainwinui.ui" line="647"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"

  sed -i -e '/filename="miscfltui.ui" line="34"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="miscfltui.ui" line="46"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="miscfltui.ui" line="53"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="miscfltui.ui" line="215"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"

  sed -i -e '/filename="rttrkui.ui" line="89"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"

  sed -i -e '/filename="trackui.ui" line="17"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="45"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="57"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="67"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="204"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="207"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="232"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="235"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="247"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="254"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="257"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="274"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="305"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="348"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="443"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="446"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="trackui.ui" line="461"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"

  sed -i -e '/filename="wayptsui.ui" line="25"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="wayptsui.ui" line="40"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="wayptsui.ui" line="133"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="wayptsui.ui" line="168"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
  sed -i -e '/filename="wayptsui.ui" line="208"/{:find2;n;/<\/source>/!b find2;s| </source>|</source>|;:find3;n;/<\/translation>/!b find3;s| </translation>|</translation>|;}' "$file"
done
