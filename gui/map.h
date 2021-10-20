// -*- C++ -*-
// $Id: map.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#ifndef MAP_H
#define MAP_H

#include <QByteArray>             // for QByteArray
#include <QElapsedTimer>          // for QElapsedTimer
#include <QFile>                  // for QFile
#include <QList>                  // for QList
#include <QNetworkAccessManager>  // for QNetworkAccessManager
#include <QObject>                // for QObject, Q_OBJECT, emit, signals, slots
#include <QPlainTextEdit>         // for QPlainTextEdit
#include <QResizeEvent>           // for QResizeEvent
#include <QString>                // for QString
#include <QStringList>            // for QStringList
#include <QTextStream>            // for QTextStream
#include <QWebEngineView>         // for QWebEngineView
#include <QWidget>                // for QWidget

#include "gpx.h"                  // for Gpx, GpxRoute, GpxTrack, GpxWaypoint
#include "latlng.h"               // for LatLng


//#define DEBUG_JS_GENERATION


class MarkerClicker: public QObject
{
  Q_OBJECT

public:
  MarkerClicker(QObject* parent): QObject(parent) {}

public slots:
  void clickedX(int t, int i)
  {
    emit markerClicked(t, i);
  }
  void logTimeX(const QString& s)
  {
    emit logTime(s);
  }

signals:
  void markerClicked(int t, int i);
  void logTime(const QString& s);
};



class Map : public QWebEngineView
{
  Q_OBJECT
public:
  Map(QWidget* parent,
      const Gpx&  gpx_, QPlainTextEdit* textEdit_);
  ~Map();

public slots:
  void showGpxData();

  void showTracks(const QList<GpxTrack>& tracks);
  void hideAllTracks();
  void setTrackVisibility(int i, bool show);

  void showWaypoints(const QList<GpxWaypoint>& waypoints);
  void hideAllWaypoints();
  void setWaypointVisibility(int i, bool show);

  void showRoutes(const QList<GpxRoute>& routes);
  void hideAllRoutes();
  void setRouteVisibility(int i, bool show);

  void loadFinishedX(bool);
  void markerClicked(int t, int i);
  void panTo(const LatLng& loc);
  void setWaypointColorRed(int i);
  void setWaypointColorBlue(int i);
  void frameTrack(int i);
  void frameRoute(int i);

  void logTime(const QString&);

private:
  QByteArray encodeKey(const QByteArray& key);
  QByteArray decodeKey(const QByteArray& key);

signals:
  void waypointClicked(int i);
  void trackClicked(int i);
  void routeClicked(int i);

private:
#ifdef DEBUG_JS_GENERATION
  QFile* dbgdata_;
  QTextStream* dbgout_;
#endif
  QNetworkAccessManager* manager_;
  const Gpx& gpx_;
  bool mapPresent_;
  bool busyCursor_;
  QElapsedTimer stopWatch_;
  QPlainTextEdit* textEdit_;

  void evaluateJS(const QString& s, bool update = true);
  void evaluateJS(const QStringList& s, bool update = true);


protected:
  virtual void resizeEvent(QResizeEvent* event);

};


#endif // HEADER_H
