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
  void overlayToggledX(const QString& name, bool visible)
  {
    emit overlayToggled(name, visible);
  }

signals:
  void markerClicked(int t, int i);
  void logTime(const QString& s);
  void loadFinished(bool b);
  void overlayToggled(const QString& name, bool visible);
};

#endif // MARKERCLICKER_H