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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------


#include <QFile>
#include <QXmlInputSource>
#include <QXmlSimpleReader>
#include <QXmlDefaultHandler>
#include "gpx.h"


static QDateTime decodeDateTime(const QString& s)
{
  QDateTime utc = QDateTime::fromString(s, "yyyy-MM-dd'T'HH:mm:ss'Z'");
  return utc;
}

static bool trackIsEmpty(const GpxTrack& trk)
{
  int count = 0;
  for (int i=0; i< trk.getTrackSegments().size(); i++) {
    for (int j=0; j<trk.getTrackSegments()[i].getTrackPoints().size(); j++) {
      count++;
    }
  }
  return count <=2 ;
}

class GpxHandler: public QXmlDefaultHandler
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

  bool startElement(const QString& /*namespaceURI*/,
                    const QString& localName, const QString& /*qName*/,
                    const QXmlAttributes& atts) override
  {
    if (localName == "wpt") {
      currentWpt = GpxWaypoint();
      double lat = atts.value("lat").toDouble();;
      double lon = atts.value("lon").toDouble();;
      currentWpt.setLocation(LatLng(lat, lon));
      stateStack << state;
      state = e_wpt;
    }

    else if (localName == "trk") {
      stateStack << state;
      state = e_trk;
      currentTrk.clear();
    }

    else if (localName == "trkseg") {
      stateStack << state;
      state = e_trkseg;
      currentTrkSeg.clear();
    }

    else if (localName == "trkpt") {
      currentTrkPt = GpxTrackPoint();
      double lat = atts.value("lat").toDouble();;
      double lon = atts.value("lon").toDouble();;
      currentTrkPt.setLocation(LatLng(lat, lon));
      stateStack << state;
      state = e_trkpt;
    }

    else if (localName == "rte") {
      stateStack << state;
      state = e_rte;
      currentRte.clear();
    }

    else if (localName == "rtept") {
      currentRtePt = GpxRoutePoint();
      double lat = atts.value("lat").toDouble();;
      double lon = atts.value("lon").toDouble();;
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
    return true;
  };

  bool endElement(const QString& /*namespaceURI*/,
                  const QString& localName,
                  const QString& /*qName*/) override
  {
    if (localName == "wpt") {
      state = stateStack.takeLast();
      wptList << currentWpt;
    } else if (localName == "ele" && state == e_wpt) {
      currentWpt.setElevation(textChars.toDouble());
    } else if (localName == "name" && state == e_wpt) {
      currentWpt.setName(textChars);
    } else if (localName == "cmt" && state == e_wpt) {
      currentWpt.setComment(textChars);
    } else if (localName == "desc" && state == e_wpt) {
      currentWpt.setDescription(textChars);
    } else if (localName == "sym" && state == e_wpt) {
      currentWpt.setSymbol(textChars);
    }

    else if (localName == "trkpt") {
      state = stateStack.takeLast();
      currentTrkSeg.addPoint(currentTrkPt);
    } else if (localName == "ele" && state == e_trkpt) {
      currentTrkPt.setElevation(textChars.toDouble());
    } else if (localName == "time" && state == e_trkpt) {
      currentTrkPt.setDateTime(decodeDateTime(textChars));
    }

    else if (localName == "trkseg") {
      state = stateStack.takeLast();
      currentTrk.addSegment(currentTrkSeg);
    }

    else if (localName == "trk") {
      state = stateStack.takeLast();
      if (!trackIsEmpty(currentTrk)) {
        trkList << currentTrk;
      }
    }

    else if (localName == "name" && state == e_trk) {
      currentTrk.setName(textChars);
    } else if (localName == "number" && state == e_trk) {
      currentTrk.setNumber(textChars.toInt());
    }

    else if (localName == "rte") {
      state = stateStack.takeLast();
      if (currentRte.getRoutePoints().size()>=2) {
        rteList << currentRte;
      }
    }

    else if (localName == "rtept") {
      state = stateStack.takeLast();
      currentRte.addPoint(currentRtePt);
    }

    else if (localName == "name" && state == e_rtept) {
      currentRtePt.setName(textChars);
    }

    else if (localName == "name" && state == e_rte) {
      currentRte.setName(textChars);
    }

    else {
      //fprintf(stderr, "end ---- localName:  %s     name:  %s\n\n",
      //localName.toStdString().c_str(), qName.toStdString().c_str());
    }
    return true;
  };

  bool characters(const QString& x) override
  {
    textChars = x;
    return true;
  };
};


//------------------------------------------------------------------------

bool Gpx::read(const QString& fileName)
{
  QFile file(fileName);
  if (!file.open(QIODevice::ReadOnly)) {
    return false;
  }

  QXmlInputSource xmlIn(&file);

  QXmlSimpleReader reader;
  GpxHandler gpxHandler;
  reader.setContentHandler(&gpxHandler);

  if (reader.parse(xmlIn)) {
    wayPoints = gpxHandler.wptList;
    tracks = gpxHandler.trkList;
    routes = gpxHandler.rteList;
    return true;
  }
  return false;

}
