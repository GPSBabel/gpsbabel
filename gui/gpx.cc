// -*- C++ -*-
// $Id: gpx.cpp,v 1.2 2009-08-28 17:08:55 robertl Exp $
//------------------------------------------------------------------------
//
//  Copyright (C) 2009  S. Khai Mong <khai@mangrai.com>.
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------

#include "gpx.h"
#include <QFile>                 // for QFile
#include <QIODevice>             // for QIODevice
#include <QStringView>           // for QStringView, operator==
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <QXmlStreamReader>      // for QXmlStreamReader


static QDateTime decodeDateTime(const QString& s)
{
  QDateTime utc = QDateTime::fromString(s, u"yyyy-MM-dd'T'HH:mm:ss'Z'");
  return utc;
}

static bool trackIsEmpty(const GpxTrack& trk)
{
  int count = 0;
  for (const auto& segment : trk.getTrackSegments()) {
    count += segment.getTrackPoints().size();
  }
  return count <= 1;
}

class GpxHandler
{
public:
  GpxHandler()

  {
    state = e_noop;
  }

  enum elementState {e_noop, e_wpt, e_trk,
                     e_trkpt, e_trkseg, e_rte, e_rtept
                    };
  QString textChars;
  GpxWaypoint currentWpt;
  QList <GpxWaypoint> wptList;

  QList <GpxTrack> trkList;
  GpxTrack currentTrk;
  GpxTrackPoint currentTrkPt;
  GpxTrackSegment currentTrkSeg;

  QList <GpxRoute> rteList;
  GpxRoute currentRte;
  GpxRoutePoint currentRtePt;

  elementState state;
  QList <elementState> stateStack;

  void startElement(QStringView localName,
                    const QXmlStreamAttributes& atts)
  {
    if (localName == u"wpt") {
      currentWpt = GpxWaypoint();
      double lat = atts.value("lat").toDouble();
      double lon = atts.value("lon").toDouble();
      currentWpt.setLocation(LatLng(lat, lon));
      stateStack << state;
      state = e_wpt;
    }

    else if (localName == u"trk") {
      stateStack << state;
      state = e_trk;
      currentTrk.clear();
    }

    else if (localName == u"trkseg") {
      stateStack << state;
      state = e_trkseg;
      currentTrkSeg.clear();
    }

    else if (localName == u"trkpt") {
      currentTrkPt = GpxTrackPoint();
      double lat = atts.value("lat").toDouble();
      double lon = atts.value("lon").toDouble();
      currentTrkPt.setLocation(LatLng(lat, lon));
      stateStack << state;
      state = e_trkpt;
    }

    else if (localName == u"rte") {
      stateStack << state;
      state = e_rte;
      currentRte.clear();
    }

    else if (localName == u"rtept") {
      currentRtePt = GpxRoutePoint();
      double lat = atts.value("lat").toDouble();
      double lon = atts.value("lon").toDouble();
      currentRtePt.setLocation(LatLng(lat, lon));
      stateStack << state;
      state = e_rtept;
    }


    else if (state == e_wpt ||
             state == e_trkpt || state == e_trkseg || state == e_trk ||
             state == e_rte || state == e_rtept) {
    } else {
      //fprintf(stderr, "localName:  %s     name:  %s\n",
      //localName.toStdString().c_str(), qName.toStdString().c_str());
    }
  }

  void endElement(QStringView localName)
  {
    if (localName == u"wpt") {
      state = stateStack.takeLast();
      wptList << currentWpt;
    } else if (localName == u"ele" && state == e_wpt) {
      currentWpt.setElevation(textChars.toDouble());
    } else if (localName == u"name" && state == e_wpt) {
      currentWpt.setName(textChars);
    } else if (localName == u"cmt" && state == e_wpt) {
      currentWpt.setComment(textChars);
    } else if (localName == u"desc" && state == e_wpt) {
      currentWpt.setDescription(textChars);
    } else if (localName == u"sym" && state == e_wpt) {
      currentWpt.setSymbol(textChars);
    }

    else if (localName == u"trkpt") {
      state = stateStack.takeLast();
      currentTrkSeg.addPoint(currentTrkPt);
    } else if (localName == u"ele" && state == e_trkpt) {
      currentTrkPt.setElevation(textChars.toDouble());
    } else if (localName == u"time" && state == e_trkpt) {
      currentTrkPt.setDateTime(decodeDateTime(textChars));
    }

    else if (localName == u"trkseg") {
      state = stateStack.takeLast();
      currentTrk.addSegment(currentTrkSeg);
    }

    else if (localName == u"trk") {
      state = stateStack.takeLast();
      if (!trackIsEmpty(currentTrk)) {
        trkList << currentTrk;
      }
    }

    else if (localName == u"name" && state == e_trk) {
      currentTrk.setName(textChars);
    } else if (localName == u"number" && state == e_trk) {
      currentTrk.setNumber(textChars.toInt());
    }

    else if (localName == u"rte") {
      state = stateStack.takeLast();
      if (currentRte.getRoutePoints().size() >= 2) {
        rteList << currentRte;
      }
    }

    else if (localName == u"rtept") {
      state = stateStack.takeLast();
      currentRte.addPoint(currentRtePt);
    }

    else if (localName == u"name" && state == e_rtept) {
      currentRtePt.setName(textChars);
    }

    else if (localName == u"name" && state == e_rte) {
      currentRte.setName(textChars);
    }

    else {
      //fprintf(stderr, "end ---- localName:  %s     name:  %s\n\n",
      //localName.toStdString().c_str(), qName.toStdString().c_str());
    }
  }

  void characters(const QString& x)
  {
    textChars = x;
  }
};


//------------------------------------------------------------------------

bool Gpx::read(const QString& fileName)
{
  QFile file(fileName);
  if (!file.open(QIODevice::ReadOnly)) {
    return false;
  }

  QXmlStreamReader reader(&file);
  GpxHandler gpxHandler;

  for (bool atEnd = false; !reader.atEnd() && !atEnd;)  {
    reader.readNext();
    // do processing
    switch (reader.tokenType()) {
    case QXmlStreamReader::StartElement:
      gpxHandler.startElement(reader.name(), reader.attributes());
      break;

    case QXmlStreamReader::EndElement:
      gpxHandler.endElement(reader.name());
      break;

    case QXmlStreamReader::Characters:
      gpxHandler.characters(reader.text().toString());
      break;

    case QXmlStreamReader::EndDocument:
    case QXmlStreamReader::Invalid:
      atEnd = true;
      break;

    default:
      break;
    }
  }

  if (!reader.hasError()) {
    wayPoints = gpxHandler.wptList;
    tracks = gpxHandler.trkList;
    routes = gpxHandler.rteList;
    return true;
  }
  return false;

}
