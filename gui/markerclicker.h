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
    emit markerClicked(t, i);
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
};

#endif // MARKERCLICKER_H