// -*- C++ -*-
#ifndef LEAFLETMAP_H
#define LEAFLETMAP_H

#include <QByteArray>
#include <QElapsedTimer>
#include <QNetworkAccessManager>
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
             const Gpx& gpx, const QString& geojsonData, QPlainTextEdit* te);
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
  void overlayToggled(const QString& name, bool visible);
  void mapRendered();

private slots:
  void loadFinishedX(bool f);
  void markerClicked(int t, int i);
  void logTime(const QString& s);

private:
  void showGpxData();
  QNetworkAccessManager* manager_{nullptr};
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
