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
#include <QWebChannel>
#else
#include <QWebFrame>
#include <QWebPage>
#endif
#include <QApplication>
#include <QCursor>
#include <QFile>

#include <math.h>
#include "appname.h"
#include "dpencode.h"

//------------------------------------------------------------------------
static QString stripDoubleQuotes(const QString s) {
  QString out;
  foreach (QChar c, s) {
    if (c != QChar('"'))
      out += c;
  }
  return out;
}

//------------------------------------------------------------------------
Map::Map(QWidget *parent,
	 const Gpx  &gpx, QPlainTextEdit *te):
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
  this->logTimeX("Start map constuctor");
  QString baseFile =  QApplication::applicationDirPath() + "/gmapbase.html";
  if (!QFile(baseFile).exists()) {
    QMessageBox::critical(0, appName,
			  tr("Missing \"gmapbase.html\" file.  Check installation"));
  }
  else {
    QString urlStr = "file:///" + baseFile;
    load(QUrl(urlStr));
  }
}

//------------------------------------------------------------------------
Map::~Map()
{
  if (busyCursor_)
    QApplication::restoreOverrideCursor();
}
//------------------------------------------------------------------------
void Map::loadFinishedX(bool f)
{
  this->logTimeX("Done initial page load");
  if (!f)
    QMessageBox::critical(0, appName,
			  tr("Failed to load Google maps base page"));
  else {
    QApplication::processEvents();
    showGpxData();
  }
  QApplication::restoreOverrideCursor();
  busyCursor_ = false;
}

//------------------------------------------------------------------------

static QStringList makeLiteralVar(const QString &name, const string &s)
{
  QStringList out;
  out << QString("var %1 = ").arg(name);

  QString ws = "\"";
  for (unsigned int i=0; i<s.length(); i++) {
    if (s[i] =='\\') {
      ws += s[i];
    }
    ws += s[i];
    if (ws.length() > 5120) {
      ws += "\" + ";
      out << ws;
      ws = "\"";
    }
  }
  ws += "\";";
  out << ws;
  return out;
}

//------------------------------------------------------------------------
static QString fmtLatLng(const LatLng &l) {
  return  QString("%1, %3").arg(l.lat(), 0, 'f', 5) .arg(l.lng(), 0, 'f', 5);
}

//------------------------------------------------------------------------
void Map::showGpxData()
{
  MarkerClicker *mclicker = new MarkerClicker(this);
#if HAVE_WEBENGINE
  QWebChannel* channel = new QWebChannel(this);
  this->page()->setWebChannel(channel);
  channel->registerObject(QStringLiteral("mclicker"), mclicker);
//  this->addToJavaScriptWindowObject("mclicker", mclicker);
#else
  this->page()->mainFrame()->addToJavaScriptWindowObject("mclicker", mclicker);
#endif
  connect(mclicker, SIGNAL(markerClicked(int, int )), this, SLOT(markerClicked(int, int)));
  connect(mclicker, SIGNAL(logTime(const QString &)), this, SLOT(logTimeX(const QString &)));

  // It is appreciably faster to do the encoding on the C++ side.
  int numLevels = 18;
  double zoomFactor = 2;
  PolylineEncoder encoder(numLevels, zoomFactor, 0.00001);


  this->logTimeX("Start defining JS string");
  QStringList scriptStr;
  scriptStr
    << "mclicker.logTime(\"Start JS execution\");"
    << "var map = new GMap2(document.getElementById(\"map\"));"
    << "var bounds = new GLatLngBounds;"
    << "var waypts = [];"
    << "var rtes = [];"
    << "var trks = [];"
    << "map.enableScrollWheelZoom();"
    << "map.enableContinuousZoom();"
    << "map.addControl(new GLargeMapControl());"
    << "map.addControl(new GScaleControl());"
    << "map.addControl(new GMapTypeControl());"
    << "var pn = map.getPane(G_MAP_MARKER_PANE);"
    << "pn.style.KhtmlUserSelect='none';"
    << "pn.style.KhtmlUserDrag='none';"
    << "mclicker.logTime(\"Done prelim JS definition\");"
    << QString("var zoomFactor = %1;").arg(zoomFactor)
    << QString("var numLevels = %1;").arg(numLevels)
    ;

  mapPresent_ = true;

  // Waypoints.
  int num=0;
  foreach (const  GpxWaypoint &pt, gpx_.getWaypoints() ) {
    scriptStr
      << QString("waypts[%1] = new GMarker(new GLatLng(%2), "
		 "{title:\"%3\",icon:blueIcon});")
      .arg(num)
      .arg(fmtLatLng(pt.getLocation()))
      .arg(stripDoubleQuotes(pt.getName()));
    num++;
  }

  scriptStr
    << "for( var i=0; i<waypts.length; ++i ) {"
    << "   bounds.extend(waypts[i].getPoint());"
    << "   var ftemp = new MarkerHandler(0, i);"
    << "   GEvent.bind(waypts[i], \"click\", ftemp, ftemp.clicked);"
    << "   map.addOverlay(waypts[i]);"
    << "}"
    << "mclicker.logTime(\"Done waypoints definition\");"
    ;

  // Tracks
  num = 0;
  foreach (const GpxTrack &trk, gpx_.getTracks()) {
    vector <LatLng> epts;
    foreach (const GpxTrackSegment seg, trk.getTrackSegments()) {
      foreach (const GpxTrackPoint pt, seg.getTrackPoints()) {
	epts.push_back(pt.getLocation());
      }
    }
    string encPts, encLevels;
    encoder.dpEncode(encPts, encLevels, epts);

    scriptStr
      << QString("var startPt = new GLatLng(%1);").arg(fmtLatLng(epts[0]))
      << QString("var endPt = new GLatLng(%1);").arg(fmtLatLng(epts[epts.size()-1]))
      << QString("var idx = %1;").arg(num)
      << QString("var nm = \"%1\";").arg(stripDoubleQuotes(trk.getName()))
      << makeLiteralVar("encpts", encPts)
      << makeLiteralVar("enclvs", encLevels)

      << "var trk   = GPolyline.fromEncoded({color:\"#0000E0\", weight:2, opacity:0.6,"
      <<                   "points:encpts, zoomFactor:zoomFactor, levels:enclvs, numLevels:numLevels});"
      << "trks[idx] =  new RTPolyline(trk, startPt, endPt, new MarkerHandler(1, idx));"
      ;
    num++;
  }

  scriptStr
    << "for( var i=0; i<trks.length; ++i ) {"
    << "   var trkbound = trks[i].getBounds();"
    << "   bounds.extend(trkbound.getSouthWest());"
    << "   bounds.extend(trkbound.getNorthEast());"
    << "}"
    << "mclicker.logTime(\"Done track definition\");"
    ;

  // Routes
  num = 0;
  foreach (const GpxRoute &rte, gpx_.getRoutes()) {
    vector <LatLng> epts;
    foreach (const GpxRoutePoint &pt, rte.getRoutePoints()) {
      epts.push_back(pt.getLocation());
    }
    string encPts, encLevels;
    encoder.dpEncode(encPts, encLevels, epts);
    scriptStr
      << QString("var startPt = new GLatLng(%1);").arg(fmtLatLng(epts[0]))
      << QString("var endPt = new GLatLng(%1);").arg(fmtLatLng(epts[epts.size()-1]))
      << QString("var idx = %1;").arg(num)
      << QString("var nm = \"%1\";").arg(stripDoubleQuotes(rte.getName()))
      << makeLiteralVar("encpts", encPts)
      << makeLiteralVar("enclvs", encLevels)
      << "var rte = GPolyline.fromEncoded({color:\"#8000B0\", weight:2, opacity:0.6,"
      << "                       points:encpts, zoomFactor:zoomFactor, levels:enclvs, numLevels:numLevels});"
      << "rtes[idx] = new RTPolyline(rte, startPt, endPt, new MarkerHandler(2, idx));"
      ;
    num++;
  }

  scriptStr
    << "for( var i=0; i<rtes.length; ++i ) {"
    << "   var rtebound = rtes[i].getBounds();"
    << "   bounds.extend(rtebound.getSouthWest());"
    << "   bounds.extend(rtebound.getNorthEast());"
    << "}"
    << "mclicker.logTime(\"Done route definition\");"
    ;

  scriptStr
    << "map.setCenter(bounds.getCenter(), map.getBoundsZoomLevel(bounds));"
    << "mclicker.logTime(\"done setCenter\");"
    ;

  this->logTimeX("Done defining JS string");
  evaluateJS(scriptStr);
  this->logTimeX("Done JS evaluation");
}

//------------------------------------------------------------------------
void Map::markerClicked(int t, int i){
  if (t == 0)
    emit waypointClicked(i);
  else if (t == 1)
    emit trackClicked(i);
  else if (t == 2)
    emit routeClicked(i);

}

//------------------------------------------------------------------------
void Map::logTimeX(const QString &s)
{
  //  fprintf(stderr, "Log: %s:  %d ms\n", s.toStdString().c_str(), stopWatch.elapsed());
  if (textEdit_) {
    textEdit_->appendPlainText(QString("%1: %2 ms").arg(s).arg(stopWatch_.elapsed()));
  }
  stopWatch_.start();
}
//------------------------------------------------------------------------
void Map::showTracks(const QList<GpxTrack> &tracks)
{
  QStringList scriptStr;
  int i=0;
  foreach(const GpxTrack &trk, tracks) {
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
    << "for( var i=0; i<trks.length; ++i ) {"
    << "   trks[i].hide();"
    << "}"
    ;
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
void Map::showWaypoints(const QList<GpxWaypoint> &waypoints)
{
  QStringList scriptStr;
  int i=0;
  foreach(const GpxWaypoint &pt, waypoints) {
    scriptStr << QString("waypts[%1].%2();").arg(i++).arg(pt.getVisible()?"show":"hide");
  }
  evaluateJS(scriptStr);
}
//------------------------------------------------------------------------
void Map::hideAllWaypoints()
{
  QStringList scriptStr;
  scriptStr
    << "for( var i=0; i<waypts.length; ++i ) {"
    << "   waypts[i].hide();"
    << "}"
    ;
  evaluateJS(scriptStr);
}

//------------------------------------------------------------------------
void Map::showRoutes(const QList<GpxRoute> &routes)
{
  QStringList scriptStr;
  int i=0;
  foreach(const GpxRoute &rt, routes) {
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
    << "for( var i=0; i<rtes.length; ++i ) {"
    << "   rtes[i].hide();"
    << "}"
    ;
  evaluateJS(scriptStr);
}
//------------------------------------------------------------------------
void Map::setWaypointVisibility(int i, bool show)
{
  evaluateJS(QString("waypts[%1].%2();\n")
	     .arg(i).arg(show?"show": "hide"));
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
void Map::panTo(const LatLng &loc)
{
  evaluateJS(QString("map.panTo(new GLatLng(%1));").arg(fmtLatLng(loc)));
}

//------------------------------------------------------------------------
void Map::resizeEvent ( QResizeEvent * ev)
{
#if HAVE_WEBENGINE
  QWebEngineView::resizeEvent(ev);
#else
  QWebView::resizeEvent(ev);
#endif
  if (mapPresent_)
    evaluateJS(QString("map.checkResize();"));
}

//------------------------------------------------------------------------
void Map::setWaypointColorRed(int i)
{
  evaluateJS(QString("waypts[%1].setImage(redIcon.image)").arg(i));
}

//------------------------------------------------------------------------
void Map::setWaypointColorBlue(int i)
{
  evaluateJS(QString("waypts[%1].setImage(blueIcon.image)").arg(i));
}

//------------------------------------------------------------------------
void Map::frameTrack(int i)
{
  QStringList scriptStr;
  scriptStr
    << QString("var trkbound = trks[%1].getBounds();").arg(i)
    << "map.setCenter(trkbound.getCenter(), map.getBoundsZoomLevel(trkbound));"
    ;
  evaluateJS(scriptStr);
}


//------------------------------------------------------------------------
void Map::frameRoute(int i)
{
  QStringList scriptStr;
  scriptStr
    << QString("var rtebound = rtes[%1].getBounds();").arg(i)
    << "map.setCenter(rtebound.getCenter(), map.getBoundsZoomLevel(rtebound));"
    ;
  evaluateJS(scriptStr);
}


//------------------------------------------------------------------------
void Map::evaluateJS(const QString &s, bool upd)
{
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
void Map::evaluateJS(const QStringList &s, bool upd)
{
  evaluateJS(s.join("\n"), upd);
}
