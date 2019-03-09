// -*- C++ -*-
// $Id: map.cpp,v 1.2 2009-08-28 17:08:55 robertl Exp $
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
#include "map.h"

#include <QNetworkRequest>
#include <QMessageBox>
#include <QNetworkAccessManager>
#if HAVE_WEBENGINE
#include <QWebEngineView>
#include <QWebEnginePage>
#include <QWebChannel>
#else
#include <QWebView>
#include <QWebFrame>
#include <QWebPage>
#endif
#include <QApplication>
#include <QCursor>
#include <QFile>
#include <QTextStream>

#include <math.h>
#include <string>
#include <vector>
#include "appname.h"

using std::string;
using std::vector;

//------------------------------------------------------------------------
static QString stripDoubleQuotes(const QString& s)
{
  QString out;
  foreach (QChar c, s) {
    if (c != QChar('"')) {
      out += c;
    }
  }
  return out;
}

//------------------------------------------------------------------------
Map::Map(QWidget* parent,
         const Gpx&  gpx, QPlainTextEdit* te):
#if HAVE_WEBENGINE
  QWebEngineView(parent),
#else
  QWebView(parent),
#endif
  gpx_(gpx),
  mapPresent_(false),
  busyCursor_(false),
  textEdit_(te)
{
  busyCursor_ = true;
  stopWatch_.start();
  QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
  manager_ = new QNetworkAccessManager(this);
  connect(this,SIGNAL(loadFinished(bool)),
          this,SLOT(loadFinishedX(bool)));
  this->logTime("Start map constuctor");

#if HAVE_WEBENGINE
  MarkerClicker* mclicker = new MarkerClicker(this);
  QWebChannel* channel = new QWebChannel(this->page());
  this->page()->setWebChannel(channel);
  // Note: A current limitation is that objects must be registered before any client is initialized.
  channel->registerObject(QStringLiteral("mclicker"), mclicker);
  connect(mclicker, SIGNAL(markerClicked(int,int)), this, SLOT(markerClicked(int,int)));
  connect(mclicker, SIGNAL(logTime(QString)), this, SLOT(logTime(QString)));
#endif

  QString baseFile =  QApplication::applicationDirPath() + "/gmapbase.html";
  if (!QFile(baseFile).exists()) {
    QMessageBox::critical(nullptr, appName,
                          tr("Missing \"gmapbase.html\" file.  Check installation"));
  } else {
    QString urlStr = "file:///" + baseFile;
    this->load(QUrl(urlStr));
  }

#ifdef DEBUG_JS_GENERATION
  dbgdata_ = new QFile("mapdebug.js");
  if (dbgdata_->open(QFile::WriteOnly | QIODevice::Truncate)) {
    dbgout_ = new QTextStream(dbgdata_);
  }
#endif

}

//------------------------------------------------------------------------
Map::~Map()
{
  if (busyCursor_) {
    QApplication::restoreOverrideCursor();
  }
#ifdef DEBUG_JS_GENERATION
  if (dbgout_) {
    delete dbgout_;
    dbgout_ = NULL;
  }
  if (dbgdata_) {
    delete dbgdata_;
    dbgdata_ = NULL;
  }
#endif
}
//------------------------------------------------------------------------
void Map::loadFinishedX(bool f)
{
  this->logTime("Done initial page load");
  if (!f) {
    QMessageBox::critical(nullptr, appName,
                          tr("Failed to load Google maps base page"));
  } else {
    QApplication::processEvents();
    showGpxData();
  }
  QApplication::restoreOverrideCursor();
  busyCursor_ = false;
}

//------------------------------------------------------------------------
static QString fmtLatLng(const LatLng& l)
{
  return  QString("{lat: %1, lng: %3}").arg(l.lat(), 0, 'f', 5) .arg(l.lng(), 0, 'f', 5);
}

//------------------------------------------------------------------------
static QString makePath(const vector <LatLng>& pts)
{
  // maps v3 Polylines do not use encoded paths.
  QString path;
  int lncount = 0;
  bool someoutput = false;
  foreach (const LatLng ll, pts) {
    if (lncount == 0) {
      if (someoutput) {
        path.append(QChar(','));
      }
      path.append(QLatin1String("\n            "));
    } else if (lncount == 1) {
      path.append(QLatin1String(", "));
    }
    path.append(fmtLatLng(ll));
    someoutput = true;
    lncount = (lncount + 1) % 2;
  }
  return path;
}

//------------------------------------------------------------------------
void Map::showGpxData()
{

#if !defined(HAVE_WEBENGINE)
  // Historically this was done here in showGpxData.
  MarkerClicker* mclicker = new MarkerClicker(this);
  this->page()->mainFrame()->addToJavaScriptWindowObject("mclicker", mclicker);
  connect(mclicker, SIGNAL(markerClicked(int,int)), this, SLOT(markerClicked(int,int)));
  connect(mclicker, SIGNAL(logTime(QString)), this, SLOT(logTime(QString)));
#endif

  this->logTime("Start defining JS string");
  QStringList scriptStr;
  scriptStr
      << "mclicker.logTimeX(\"Start JS execution\");"
      << "var map = new google.maps.Map(document.getElementById(\"map\"));"
      << "var bounds = new google.maps.LatLngBounds();"
      << "var waypts = [];"
      << "var rtes = [];"
      << "var trks = [];"
      << "var idx;"
      << "mclicker.logTimeX(\"Done prelim JS definition\");"
      ;

  mapPresent_ = true;

  // Waypoints.
  int num=0;
  foreach (const  GpxWaypoint& pt, gpx_.getWaypoints()) {
    scriptStr
        << QString("waypts[%1] = new google.maps.Marker({map: map, position: %2, "
                   "title: \"%3\", icon: blueIcon});")
        .arg(num)
        .arg(fmtLatLng(pt.getLocation()), stripDoubleQuotes(pt.getName()));
    num++;
  }

  scriptStr
      << "for (idx = 0; idx < waypts.length; idx += 1) {"
      << "    bounds.extend(waypts[idx].getPosition());"
      << "    attachHandler(waypts[idx], new MarkerHandler(0, idx));"
      << "}"
      << "mclicker.logTimeX(\"Done waypoints definition\");"
      ;

  // Tracks
  num = 0;
  foreach (const GpxTrack& trk, gpx_.getTracks()) {
    vector <LatLng> pts;
    QString path;
    foreach (const GpxTrackSegment seg, trk.getTrackSegments()) {
      foreach (const GpxTrackPoint pt, seg.getTrackPoints()) {
        pts.push_back(pt.getLocation());
      }
    }
    path = makePath(pts);

    scriptStr
        << QString("trks[%1] = new RTPolyline(\n"
                   "    map,\n"
                   "    new google.maps.Polyline({\n        map: map,\n        strokeColor: \"#0000E0\",\n        strokeWeight: 2,\n        strokeOpacity: 0.6,\n        path: [%2\n        ]\n    }),\n"
                   "    new google.maps.LatLng(%3),\n"
                   "    new google.maps.LatLng(%4),\n"
                   "    \"%5\",\n"
                   "    new MarkerHandler(1, %1)\n);"
                  ).arg(num).arg(path, fmtLatLng(pts.front()), fmtLatLng(pts.back()), stripDoubleQuotes(trk.getName()))
        << QString("bounds.union(trks[%1].getBounds());").arg(num)
        ;
    num++;
  }

  scriptStr
      << "mclicker.logTimeX(\"Done track definition\");"
      ;

  // Routes
  num = 0;
  foreach (const GpxRoute& rte, gpx_.getRoutes()) {
    vector <LatLng> pts;
    QString path;
    foreach (const GpxRoutePoint& pt, rte.getRoutePoints()) {
      pts.push_back(pt.getLocation());
    }
    path = makePath(pts);

    scriptStr
        << QString("rtes[%1] = new RTPolyline(\n"
                   "    map,\n"
                   "    new google.maps.Polyline({\n        map: map,\n        strokeColor: \"#8000B0\",\n        strokeWeight: 2,\n        strokeOpacity: 0.6,\n        path: [%2\n        ]\n    }),\n"
                   "    new google.maps.LatLng(%3),\n"
                   "    new google.maps.LatLng(%4),\n"
                   "    \"%5\",\n"
                   "    new MarkerHandler(2, %1)\n);"
                  ).arg(num).arg(path, fmtLatLng(pts.front()), fmtLatLng(pts.back()), stripDoubleQuotes(rte.getName()))
        << QString("bounds.union(rtes[%1].getBounds());").arg(num)
        ;
    num++;
  }

  scriptStr
      << "mclicker.logTimeX(\"Done route definition\");"
      ;

  scriptStr
      << "map.setCenter(bounds.getCenter());"
      << "map.fitBounds(bounds);"
      << "mclicker.logTimeX(\"Done setCenter\");"
      ;

  this->logTime("Done defining JS string");
  evaluateJS(scriptStr);
  this->logTime("Done JS evaluation");
}

//------------------------------------------------------------------------
void Map::markerClicked(int t, int i)
{
  if (t == 0) {
    emit waypointClicked(i);
  } else if (t == 1) {
    emit trackClicked(i);
  } else if (t == 2) {
    emit routeClicked(i);
  }

}

//------------------------------------------------------------------------
void Map::logTime(const QString& s)
{
  //  fprintf(stderr, "Log: %s:  %d ms\n", s.toStdString().c_str(), stopWatch.elapsed());
  if (textEdit_ != nullptr) {
    textEdit_->appendPlainText(QString("%1: %2 ms").arg(s).arg(stopWatch_.elapsed()));
  }
  stopWatch_.start();
}
//------------------------------------------------------------------------
void Map::showTracks(const QList<GpxTrack>& tracks)
{
  QStringList scriptStr;
  int i=0;
  foreach (const GpxTrack& trk, tracks) {
    scriptStr << QString("trks[%1].%2();").arg(i).arg(trk.getVisible()?"show":"hide");
    i++;
  }
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
void Map::hideAllTracks()
{
  QStringList scriptStr;
  scriptStr
      << "for (idx = 0; idx < trks.length; idx += 1) {"
      << "    trks[idx].hide();"
      << "}"
      ;
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
// TACKY: we assume the waypoints list and JS waypts[] are parallel.
void Map::showWaypoints(const QList<GpxWaypoint>& waypoints)
{
  QStringList scriptStr;
  int i=0;
  foreach (const GpxWaypoint& pt, waypoints) {
    scriptStr << QString("waypts[%1].setVisible(%2);").arg(i++).arg(pt.getVisible()?"true":"false");
  }
  evaluateJS(scriptStr);
}
//------------------------------------------------------------------------
void Map::hideAllWaypoints()
{
  QStringList scriptStr;
  scriptStr
      << "for (idx = 0; idx < waypts.length; idx += 1) {"
      << "    waypts[idx].setVisible(false);"
      << "}"
      ;
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
void Map::showRoutes(const QList<GpxRoute>& routes)
{
  QStringList scriptStr;
  int i=0;
  foreach (const GpxRoute& rt, routes) {
    scriptStr << QString("rtes[%1].%2();").arg(i).arg(rt.getVisible()?"show":"hide");
    i++;
  }
  evaluateJS(scriptStr);
}
//------------------------------------------------------------------------
void Map::hideAllRoutes()
{
  QStringList scriptStr;
  scriptStr
      << "for (idx = 0; idx < rtes.length; idx += 1) {"
      << "    rtes[idx].hide();"
      << "}"
      ;
  evaluateJS(scriptStr);
}
//------------------------------------------------------------------------
void Map::setWaypointVisibility(int i, bool show)
{
  evaluateJS(QString("waypts[%1].setVisible(%2);\n")
             .arg(i).arg(show?"true": "false"));
}

//------------------------------------------------------------------------
void Map::setTrackVisibility(int i, bool show)
{
  QString x = show?"show": "hide";
  QStringList scriptStr;
  scriptStr
      << QString("trks[%1].%2();").arg(i).arg(x)
      ;
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
void Map::setRouteVisibility(int i, bool show)
{
  QString x = show?"show": "hide";
  QStringList scriptStr;
  scriptStr
      << QString("rtes[%1].%2();").arg(i).arg(x)
      ;
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
void Map::panTo(const LatLng& loc)
{
  evaluateJS(QString("map.panTo(new google.maps.LatLng(%1));").arg(fmtLatLng(loc)));
}

//------------------------------------------------------------------------
void Map::resizeEvent(QResizeEvent* ev)
{
#if HAVE_WEBENGINE
  QWebEngineView::resizeEvent(ev);
#else
  QWebView::resizeEvent(ev);
#endif
  if (mapPresent_) {
    evaluateJS(QString("map.checkResize();"));
  }
}

//------------------------------------------------------------------------
void Map::setWaypointColorRed(int i)
{
  evaluateJS(QString("waypts[%1].setIcon(redIcon);").arg(i));
}

//------------------------------------------------------------------------
void Map::setWaypointColorBlue(int i)
{
  evaluateJS(QString("waypts[%1].setIcon(blueIcon);").arg(i));
}

//------------------------------------------------------------------------
void Map::frameTrack(int i)
{
  QStringList scriptStr;

  scriptStr
      << QString("map.setCenter(trks[%1].getBounds().getCenter());").arg(i)
      << QString("map.fitBounds(trks[%1].getBounds());").arg(i)

      ;
  evaluateJS(scriptStr);
}


//------------------------------------------------------------------------
void Map::frameRoute(int i)
{
  QStringList scriptStr;
  scriptStr
      << QString("map.setCenter(rtes[%1].getBounds().getCenter());").arg(i)
      << QString("map.fitBounds(rtes[%1].getBounds());").arg(i)
      ;
  evaluateJS(scriptStr);
}


//------------------------------------------------------------------------
void Map::evaluateJS(const QString& s, bool upd)
{
#ifdef DEBUG_JS_GENERATION
  *dbgout_ << s;
  *dbgout_ << '\n';
  dbgout_->flush();
#endif
#if HAVE_WEBENGINE
  this->page()->runJavaScript(s);
#else
  this->page()->mainFrame()->evaluateJavaScript(s);
#endif
  if (upd) {
    this->update();
  }
}

//------------------------------------------------------------------------
void Map::evaluateJS(const QStringList& s, bool upd)
{
  evaluateJS(s.join("\n"), upd);
}
