// -*- C++ -*-
// $Id: gpx.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#ifndef GPX_H
#define GPX_H

#include <QString>
#include <QDateTime>
#include "latlng.h"

//------------------------------------------------------------------------
class GpxItem {
 public:
 GpxItem(): visible(true) {};
 GpxItem(bool visible ): visible(visible) {};
  
  void setVisible(bool b) { visible = b; };
  bool getVisible() const { return visible;};
  
 protected:
    bool visible;
};

//------------------------------------------------------------------------
class GpxRoutePoint: public GpxItem
{
 public:
  GpxRoutePoint(): GpxItem(), location(LatLng()), name(QString())
    {
    };
  void setLocation(const LatLng& pt) { location = pt; };
  LatLng getLocation() const { return location; };

  void setName(const QString &s) { name = s; };
  QString getName () const { return name; };

 private:
  LatLng location;
  QString name;
};

//------------------------------------------------------------------------
class GpxRoute: public GpxItem
{
 public:
  GpxRoute(): GpxItem(),name(QString()), cachedLength(-1) {};

 GpxRoute(const GpxRoute &c)
   :GpxItem(c.visible),
    name(c.name), cachedLength(c.cachedLength)
      {
	routePoints.clear();
	foreach (GpxRoutePoint sg, c.routePoints)
	  routePoints << sg;
      }
  GpxRoute & operator = (const GpxRoute &c) 
    {
      visible = c.visible;
      name = c.name; 
      cachedLength = c.cachedLength;
      routePoints.clear();
      foreach (GpxRoutePoint sg, c.routePoints) {
	routePoints << sg;
      }
      return *this;
    }

  double length() const
  {
    if (cachedLength >=0.0)
      return cachedLength;
    LatLng prevPt;
    bool first = true;
    double dist = 0.0;
    foreach (GpxRoutePoint pt, routePoints) {
      if (first) {
	prevPt = pt.getLocation();
	first = false;
      }
      else {
	LatLng thisPt = pt.getLocation();
	dist += prevPt.haversineDistance(thisPt);
	prevPt = thisPt;
      }
    }
    double *dptr = (double *)(&cachedLength); // big cheat
    *dptr = dist;
    return cachedLength;
  }
  
  void setName(const QString &s) { name = s; };
  QString getName() const { return name; };

  void clear() { routePoints.clear(); };
  void addPoint(const GpxRoutePoint &pt) {
    routePoints << pt;
  }
  const QList<GpxRoutePoint> & getRoutePoints() const { return routePoints; };

 private:
  QString name;
  QList <GpxRoutePoint> routePoints;
  double cachedLength;
};

//------------------------------------------------------------------------
class GpxTrackPoint: public GpxItem
{
 public:
 GpxTrackPoint(): GpxItem(), location(LatLng()), elevation(0), dateTime(QDateTime())
    {
    };
  void setLocation(const LatLng& pt) { location = pt; };
  LatLng getLocation() const { return location; };

  void setElevation(double e) { elevation = e; };
  double getElevation () const { return elevation; };

  void setDateTime(const QDateTime &dt) { dateTime = dt; };
  QDateTime getDateTime() const { return dateTime; };

 private:
  LatLng location;
  double  elevation;
  QDateTime dateTime;
};

//------------------------------------------------------------------------
class GpxTrackSegment: public GpxItem
{
 public:
  GpxTrackSegment() {};
 GpxTrackSegment(const GpxTrackSegment &c): GpxItem(c.visible) {
    trackPoints.clear();
    foreach (GpxTrackPoint pt, c.trackPoints)
      trackPoints << pt;
  }
  GpxTrackSegment & operator = (const GpxTrackSegment &c) {
    visible = c.visible;
    trackPoints.clear();
    foreach (GpxTrackPoint pt, c.trackPoints)
      trackPoints << pt;
    return *this;
  }
  void addPoint(const GpxTrackPoint & pt) {
    trackPoints << pt;
  }
  void clear() { trackPoints.clear(); };
  const QList<GpxTrackPoint> & getTrackPoints() const { return trackPoints; };

 private:
  QList <GpxTrackPoint> trackPoints;
};
//------------------------------------------------------------------------
class GpxTrack: public GpxItem
{
 public:
  GpxTrack(): GpxItem(), number(1), name(QString()), comment(QString()), description(QString()), cachedLength(-1.0) {};

 GpxTrack(const GpxTrack &c)
   :GpxItem(c.visible),
    number(c.number), 
    name(c.name), 
    comment(c.comment), 
    description (c.description),
    cachedLength(c.cachedLength)
      {
	trackSegments.clear();
	foreach (GpxTrackSegment sg, c.trackSegments)
	  trackSegments << sg;
      }
  GpxTrack & operator = (const GpxTrack &c) 
    {
      visible = c.visible;
      number = c.number;
      name = c.name; 
      comment = c.comment;
      description = c.description;
      cachedLength = c.cachedLength;
      trackSegments.clear();
      foreach (GpxTrackSegment sg, c.trackSegments) {
	trackSegments << sg;
      }
      return *this;
    }

  void setNumber(int n) { number = n; };
  int getNumber() const { return number; };

  void setName(const QString &s) { name = s; };
  QString getName() const { return name; };

  void setComment(const QString &s) { comment = s; };
  QString getComment() const { return comment; };

  void setDescription(const QString &s) { description = s; };
  QString getDescription() const { return description; };

  void clear() { trackSegments.clear(); };
  void addSegment(const GpxTrackSegment &seg) {
    trackSegments << seg;
  }
  const QList<GpxTrackSegment> & getTrackSegments() const { return trackSegments; };

  double length() const
  {
    if (cachedLength >=0.0)
      return cachedLength;
    LatLng prevPt;
    bool first = true;
    double dist = 0.0;
    foreach (GpxTrackSegment seg, trackSegments) {
      foreach (GpxTrackPoint pt, seg.getTrackPoints()) {
	if (first) {
	  prevPt = pt.getLocation();
	  first = false;
	}
	else {
	  LatLng thisPt = pt.getLocation();
	  dist += prevPt.haversineDistance(thisPt);
	  prevPt = thisPt;
	}
      }
    }
    double *dptr = (double *)(&cachedLength); // big cheat
    *dptr = dist;
    return cachedLength;
  }

 private:
  int     number;
  QString name;
  QString comment;
  QString description;
  QList <GpxTrackSegment> trackSegments;
  double cachedLength;
};

//------------------------------------------------------------------------
class GpxWaypoint: public GpxItem
{
 public:
 GpxWaypoint(): GpxItem(), 
    location_(LatLng(0, 0)),
    elevation_ (-1.0E-100),
    name_(QString()),
    comment_(QString()),
    description_(QString()),
    symbol_(QString())
      {};
  
  void setLocation(const LatLng& pt) { location_ = pt; };
  LatLng getLocation() const { return location_; };

  void setElevation(double e) { elevation_ = e; };
  double getElevation () const { return elevation_; };

  void setName(const QString &s) { name_ = s; };
  QString getName() const { return name_; };

  void setComment(const QString &s) { comment_ = s; };
  QString getComment() const { return comment_; };

  void setDescription(const QString &s) { description_ = s; };
  QString getDescription() const { return description_; };

  void setSymbol(const QString &s) { symbol_ = s; };
  QString getSymbol() const { return symbol_; };

 private:
  LatLng location_;
  double  elevation_;
  QString name_;
  QString comment_;
  QString description_;
  QString symbol_;
};

//------------------------------------------------------------------------
class Gpx {
public:
  Gpx() {};
  bool read(const QString & fileName);
  
  QList <GpxWaypoint> &getWaypoints()   { return wayPoints; }; // nonconst
  QList <GpxTrack>    &getTracks()      { return tracks; };
  QList <GpxRoute>    &getRoutes()      { return routes; };

  const QList <GpxWaypoint> &getWaypoints() const { return wayPoints; };
  const QList <GpxTrack> &getTracks()       const { return tracks; };
  const QList <GpxRoute> &getRoutes()       const { return routes; };

 private:
  QList <GpxWaypoint> wayPoints;
  QList <GpxTrack>    tracks;
  QList <GpxRoute>    routes;
};


#endif
