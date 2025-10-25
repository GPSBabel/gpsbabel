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
#ifndef MARKERCLICKER_H
#define MARKERCLICKER_H

#include <QObject> // for QObject, emit, signals, slots
#include <QString> // for QString

class MarkerClicker: public QObject
{
  Q_OBJECT

public:
  MarkerClicker(QObject* parent): QObject(parent) {}

public slots:
  void clickedX(int t, int i)
  {
    if (t == 3) {
      emit routePointClicked(i);
    } else {
      emit markerClicked(t, i);
    }
  }
  void logTimeX(const QString& s)
  {
    emit logTime(s);
  }
  void loadedX()
  {
    emit loadFinished(true);
  }

signals:
  void markerClicked(int t, int i);
  void logTime(const QString& s);
  void loadFinished(bool b);
  void routePointClicked(int i);
};

#endif // MARKERCLICKER_H