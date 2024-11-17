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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------
#ifndef GPX_H
#define GPX_H

#include <QDateTime>         // for QDateTime
#include <QList>             // for QList
#include <QString>           // for QString
#include "latlng.h"          // for LatLng


//------------------------------------------------------------------------
class GpxRoutePoint
{
public:
  void setLocation(const LatLng& pt)
  {
    location = pt;
  }

  LatLng getLocation() const
  {
    return location;
  }

  void setName(const QString& s)
  {
    name = s;
  }

  QString getName() const
  {
    return name;
  }

private:
  LatLng location;
  QString name;
};

//------------------------------------------------------------------------
class GpxRoute
{
public:
  double length() const
  {
    if (cachedLength >=0.0) {
      return cachedLength;
    }
    LatLng prevPt;
    bool first = true;
    double dist = 0.0;
    for (const GpxRoutePoint& pt : routePoints) {
      if (first) {
        prevPt = pt.getLocation();
        first = false;
      } else {
        LatLng thisPt = pt.getLocation();
        dist += prevPt.haversineDistance(thisPt);
        prevPt = thisPt;
      }
    }
    *const_cast<double*>(&cachedLength) = dist;  // big cheat
    return cachedLength;
  }

  void setName(const QString& s)
  {
    name = s;
  }

  QString getName() const
  {
    return name;
  }

  void clear()
  {
    routePoints.clear();
  }

  void addPoint(const GpxRoutePoint& pt)
  {
    routePoints << pt;
  }
  const QList<GpxRoutePoint>& getRoutePoints() const
  {
    return routePoints;
  }

private:
  QString name;
  QList <GpxRoutePoint> routePoints;
  double cachedLength{-1.0};
};

//------------------------------------------------------------------------
class GpxTrackPoint
{
public:
  void setLocation(const LatLng& pt)
  {
    location = pt;
  }

  LatLng getLocation() const
  {
    return location;
  }

  void setElevation(double e)
  {
    elevation = e;
  }

  double getElevation() const
  {
    return elevation;
  }

  void setDateTime(const QDateTime& dt)
  {
    dateTime = dt;
  }

  QDateTime getDateTime() const
  {
    return dateTime;
  }

private:
  LatLng location;
  double  elevation{0.0};
  QDateTime dateTime;
};

//------------------------------------------------------------------------
class GpxTrackSegment
{
public:
  void addPoint(const GpxTrackPoint& pt)
  {
    trackPoints << pt;
  }
  void clear()
  {
    trackPoints.clear();
  }

  const QList<GpxTrackPoint>& getTrackPoints() const
  {
    return trackPoints;
  }

private:
  QList <GpxTrackPoint> trackPoints;
};
//------------------------------------------------------------------------
class GpxTrack
{
public:
  void setNumber(int n)
  {
    number = n;
  }

  int getNumber() const
  {
    return number;
  }

  void setName(const QString& s)
  {
    name = s;
  }

  QString getName() const
  {
    return name;
  }

  void setComment(const QString& s)
  {
    comment = s;
  }

  QString getComment() const
  {
    return comment;
  }

  void setDescription(const QString& s)
  {
    description = s;
  }

  QString getDescription() const
  {
    return description;
  }

  void clear()
  {
    trackSegments.clear();
  }

  void addSegment(const GpxTrackSegment& seg)
  {
    trackSegments << seg;
  }
  const QList<GpxTrackSegment>& getTrackSegments() const
  {
    return trackSegments;
  }

  double length() const
  {
    if (cachedLength >=0.0) {
      return cachedLength;
    }
    LatLng prevPt;
    bool first = true;
    double dist = 0.0;
    for (const GpxTrackSegment& seg : trackSegments) {
      for (const GpxTrackPoint& pt : seg.getTrackPoints()) {
        if (first) {
          prevPt = pt.getLocation();
          first = false;
        } else {
          LatLng thisPt = pt.getLocation();
          dist += prevPt.haversineDistance(thisPt);
          prevPt = thisPt;
        }
      }
    }
    *const_cast<double*>(&cachedLength) = dist;  // big cheat
    return cachedLength;
  }

private:
  int     number{1};
  QString name;
  QString comment;
  QString description;
  QList <GpxTrackSegment> trackSegments;
  double cachedLength{-1.0};
};

//------------------------------------------------------------------------
class GpxWaypoint
{
public:
  void setLocation(const LatLng& pt)
  {
    location_ = pt;
  }

  LatLng getLocation() const
  {
    return location_;
  }

  void setElevation(double e)
  {
    elevation_ = e;
  }

  double getElevation() const
  {
    return elevation_;
  }

  void setName(const QString& s)
  {
    name_ = s;
  }

  QString getName() const
  {
    return name_;
  }

  void setComment(const QString& s)
  {
    comment_ = s;
  }

  QString getComment() const
  {
    return comment_;
  }

  void setDescription(const QString& s)
  {
    description_ = s;
  }

  QString getDescription() const
  {
    return description_;
  }

  void setSymbol(const QString& s)
  {
    symbol_ = s;
  }

  QString getSymbol() const
  {
    return symbol_;
  }

private:
  LatLng location_;
  double  elevation_{-1.0E-100};
  QString name_;
  QString comment_;
  QString description_;
  QString symbol_;
};

//------------------------------------------------------------------------
class Gpx
{
public:
  bool read(const QString& fileName);

  QList <GpxWaypoint>& getWaypoints()
  {
    return wayPoints;
  } // nonconst

  QList <GpxTrack>&    getTracks()
  {
    return tracks;
  }

  QList <GpxRoute>&    getRoutes()
  {
    return routes;
  }

  const QList <GpxWaypoint>& getWaypoints() const
  {
    return wayPoints;
  }

  const QList <GpxTrack>& getTracks()       const
  {
    return tracks;
  }

  const QList <GpxRoute>& getRoutes()       const
  {
    return routes;
  }

private:
  QList <GpxWaypoint> wayPoints;
  QList <GpxTrack>    tracks;
  QList <GpxRoute>    routes;
};


#endif
