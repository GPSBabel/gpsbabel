#!/bin/bash -e
# this generates the lists in org.gpsbabel.cli.metainfo.xml

cat <<"EOF"
<?xml version="1.0" encoding="UTF-8"?>
<!-- reference: https://freedesktop.org/software/appstream/docs/index.html -->
<component type="console-application">
  <id>org.gpsbabel.cli</id>
  <metadata_license>MIT</metadata_license>
  <project_license>GPL-2.0</project_license>
  <name>gpsbabel</name>
  <summary>GPS file conversion plus transfer to/from GPS units</summary>
  <description>
    <p>GPSBabel converts waypoints, tracks, and routes from one common
    mapping format to another or a serial upload or download to a GPS
    unit such as those from Garmin.</p>

    <p>GPSBabel supports several data formats and will be useful for
    tasks such as geocaching, mapping, and converting from one GPS
    unit to another.  Among the interesting formats it supports are
    the GPS Exchange Format (GPX), Keyhole Markup Language (KML), several
    GPS devices via a serial link, and various Geocaching data formats.
    GPSBabel also supports various filters to manipulate the data.</p>

    <p>GPSBabel supports the following formats:</p>

    <ul>
EOF

# generate list items for the formats
gpsbabel -^1 | grep -v internal | cut -f 2,4 | sed  -e 's/	/: /' -e 's/^/      <li>/' -e 's/$/<\/li>/' | sort -k 1

cat <<"EOF"
    </ul>

    <p>GPSBabel supports the following filters:</p>
    <ul>
EOF

# generate list items for the filters
gpsbabel -%1 | grep -v option | sed -e 's/ +/ /' | cut -f 1,2 | sed  -e 's/	/: /' -e 's/^/      <li>/' -e 's/$/<\/li>/' | sort -k 1

cat <<"EOF"
    </ul>
  </description>
  <url type="homepage">https://www.gpsbabel.org</url>
  <url type="vcs-browser">https://github.com/GPSBabel/gpsbabel</url>
  <provides>
    <binary>gpsbabel</binary>
    <modalias>usb:v091Ep0003d*</modalias>
  </provides>
</component>
EOF
