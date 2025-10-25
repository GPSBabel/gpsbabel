//
//  Copyright (C) 2025  Robert Lipe
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
// -*- C++ -*-
#ifndef LEAFLETMAP_H
#define LEAFLETMAP_H

#include <QByteArray>
#include <QElapsedTimer>
#include <QObject>
#include <QPlainTextEdit>
#include <QResizeEvent>
#include <QString>
#include <QStringList>
#include <QWebEnginePage>
#include <QWebEngineView>
#include <QWidget>
#include "gpx.h"
#include "latlng.h"
#include "webenginepage.h"


class LeafletMap : public QWebEngineView
{
  Q_OBJECT

public:
  LeafletMap(QWidget* parent,
             const Gpx& gpx, const QString& geojsonData, QPlainTextEdit* logSink);
  ~LeafletMap();

  void setWaypointVisibility(int i, bool show);
  void setTrackVisibility(int i, bool show);
  void setRouteVisibility(int i, bool show);
  void setAllWaypointsVisibility(bool show);
  void setAllTracksVisibility(bool show);
  void setAllRoutesVisibility(bool show);
  void panTo(const LatLng& loc);
  void frameTrack(int i);
  void frameRoute(int i);
  void setWaypointColorRed(int i);
  void setWaypointColorBlue(int i);
  void resetBounds();

signals:
  void waypointClicked(int i);
  void trackClicked(int i);
  void routeClicked(int i);
  void routePointClicked(int i);
  void mapRendered();
  void routePointSelected(int index);

public slots:
  void handleRoutePointClicked(int index);

private slots:
  void loadFinishedX(bool f);
  void markerClicked(int type, int index);
  void logTime(const QString& s);

private:
  void showGpxData();
  const Gpx& gpx_;
  const QString& geojsonData_;
  bool mapPresent_{false};
  bool busyCursor_{false};
  QElapsedTimer stopWatch_;
  QPlainTextEdit* textEdit_{nullptr};
  QString mapContainerId_;

  void evaluateJS(const QString& s, bool update = true);
  void evaluateJS(const QStringList& s, bool update = true);

protected:
  void resizeEvent(QResizeEvent* event) override;
};
#endif // LEAFLETMAP_H
