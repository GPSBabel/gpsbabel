// -*- C++ -*

#include <QApplication>
#include <QChar>
#include <QCursor>
#include <QDebug>
#include <QDesktopServices>
#include <QFile>
#include <QIODevice>
#include <QLatin1String>
#include <QList>
#include <QMessageBox>
#include <QNetworkAccessManager>
#include <QStringLiteral>
#include <QUrl>
#include <QWebChannel>
#include <QWebEnginePage>
#include <QWebEngineSettings>
#include <QWebEngineView>
#include <Qt>

#include <string>
#include <vector>

#include "appname.h"
#include "gpx.h"
#include "leafletmap.h"
#include "latlng.h"
#include "markerclicker.h"
#include "webenginepage.h"

using std::string;
using std::vector;

//------------------------------------------------------------------------
static QString
stripDoubleQuotes(const QString& s)
{
  QString out = s;
  return out.remove('"');
}

//------------------------------------------------------------------------
LeafletMap::LeafletMap(QWidget* parent,
                       const Gpx& gpx, const QString& geojsonData, QPlainTextEdit* te) :
  QWebEngineView(parent),
  gpx_(gpx),
  geojsonData_(geojsonData),
  textEdit_(te)
{
  busyCursor_ = true;
  stopWatch_.start();
  QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
  manager_ = new QNetworkAccessManager(this);
  this->logTime("Start map constructor");

  auto* page = new WebEnginePageWithLogging(this);
  page->setLogSink(textEdit_);
  this->setPage(page);

  auto* mclicker = new MarkerClicker(this);
  auto* channel = new QWebChannel(this->page());
  this->page()->setWebChannel(channel);
  channel->registerObject(QStringLiteral("mclicker"), mclicker);
  connect(mclicker, &MarkerClicker::loadFinished, this, &LeafletMap::loadFinishedX);
  connect(mclicker, &MarkerClicker::markerClicked, this, &LeafletMap::markerClicked);
  connect(mclicker, &MarkerClicker::logTime, this, &LeafletMap::logTime);
  connect(mclicker, &MarkerClicker::overlayToggled, this, &LeafletMap::overlayToggled);

  QString baseFile = QApplication::applicationDirPath() + "/leafletbase.html";
  QString fileName;
  QUrl baseUrl;
  if (QFile(baseFile).exists()) {
    fileName = baseFile;
    baseUrl = QUrl::fromLocalFile(baseFile);
  } else if (QFile(":/leafletbase.html").exists()) {
    fileName = ":/leafletbase.html";
    baseUrl = QUrl("qrc:///leafletbase.html");
  }

  this->settings()->setAttribute(QWebEngineSettings::LocalContentCanAccessRemoteUrls, true);

  if (!fileName.isEmpty()) {
    QFile htmlFile(fileName);
    if (htmlFile.open(QIODevice::ReadOnly)) {
      QByteArray content = htmlFile.readAll();
      htmlFile.close();
      this->setContent(content, "text/html;charset=UTF-8", baseUrl);
    } else {
      QMessageBox::critical(nullptr, appName,
                            tr("Error opening \"leafletbase.html\" file.  Check installation"));
    }
  } else {
    QMessageBox::critical(nullptr, appName,
                          tr("Missing \"leafletbase.html\" file.  Check installation"));
  }
}

//------------------------------------------------------------------------
LeafletMap::~LeafletMap()
{
  if (busyCursor_) {
    QApplication::restoreOverrideCursor();
  }
}
//------------------------------------------------------------------------
void
LeafletMap::loadFinishedX(bool f)
{
  this->logTime("Done initial page load");
  if (!f) {
    QMessageBox::critical(nullptr, appName,
                          tr("Failed to load Leaflet maps base page"));
  } else {
    QApplication::processEvents();
    showGpxData();
  }
  QApplication::restoreOverrideCursor();
  busyCursor_ = false;
}


//------------------------------------------------------------------------
static QString
makePath(const vector <LatLng>& pts)
{
  QString path;
  bool first = true;
  for (const auto& ll : pts) {
    if (!first) {
      path.append(", ");
    }
    path.append(fmtLatLng(ll));
    first = false;
  }
  return path;
}

//------------------------------------------------------------------------
void
LeafletMap::showGpxData()
{
  this->logTime("Start defining JS string");
  QStringList scriptStr;
  scriptStr
      << "mclicker.logTimeX(\"Start JS execution\");"
      << "var bounds = L.latLngBounds();"
      << "mclicker.logTimeX(\"Done prelim JS definition\");"
      ;

  mapPresent_ = true;

  scriptStr << "renderGeoJson(" + geojsonData_ + ");";

  scriptStr
      << "if (bounds.isValid()) {"
      << "    if (bounds.getNorthEast().equals(bounds.getSouthWest())) {"
      << "        map.setView(bounds.getCenter(), 18);"
      << "        mclicker.logTimeX('JS: setView on single point. Zoom: 18');"
      << "    } else {"
      << "        map.fitBounds(bounds);"
      << "        mclicker.logTimeX('JS: fitBounds called. Zoom: ' + map.getZoom());"
      << "    }"
      << "}"
      << "mclicker.logTimeX(\"Done setView\");"
      ;

  this->logTime("Done defining JS string");
  evaluateJS(scriptStr);
  this->logTime("Done JS evaluation");
  emit mapRendered();
}
//------------------------------------------------------------------------
void
LeafletMap::markerClicked(int t, int i)
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
void
LeafletMap::logTime(const QString& s)
{
  if (textEdit_ != nullptr) {
    textEdit_->appendPlainText(QString("%1: %2 ms").arg(s).arg(stopWatch_.elapsed()));
  }
  stopWatch_.start();
}
//------------------------------------------------------------------------
void
LeafletMap::setWaypointVisibility(int i, bool show)
{
  qDebug() << "LeafletMap::setWaypointVisibility called for index:" << i << "show:" << show;
  evaluateJS(QString("if (%2) { if (waypointLayers[%1]) { waypointLayerGroup.addLayer(waypointLayers[%1]); } } else { if (waypointLayers[%1]) { waypointLayerGroup.removeLayer(waypointLayers[%1]); } }")
             .arg(i).arg(show?"true": "false"));
}

//------------------------------------------------------------------------
void
LeafletMap::setTrackVisibility(int i, bool show)
{
  qDebug() << "LeafletMap::setTrackVisibility called for index:" << i << "show:" << show;
  evaluateJS(QString("if (%2) { if (trackLayers[%1]) { trackLayerGroup.addLayer(trackLayers[%1]); } } else { if (trackLayers[%1]) { trackLayerGroup.removeLayer(trackLayers[%1]); } }")
             .arg(i).arg(show ? "true" : "false"));
}

//------------------------------------------------------------------------
void
LeafletMap::setRouteVisibility(int i, bool show)
{
  qDebug() << "LeafletMap::setRouteVisibility called for index:" << i << "show:" << show;
  evaluateJS(QString("console.log('JS: setRouteVisibility called for index %1, show %2'); if (%2) rtes[%1].show(); else rtes[%1].hide();")
             .arg(i).arg(show ? "true" : "false"));
}

//------------------------------------------------------------------------
void
LeafletMap::setAllWaypointsVisibility(bool show)
{
  qDebug() << "LeafletMap::setAllWaypointsVisibility called with:" << show;
  QString jsString = QString("setAllWaypointsVisibility(%1);")
                     .arg(show ? "true" : "false");
  qDebug() << "Executing JS:" << jsString;
  evaluateJS(jsString);
}

//------------------------------------------------------------------------
void
LeafletMap::setAllTracksVisibility(bool show)
{
  qDebug() << "LeafletMap::setAllTracksVisibility called with:" << show;
  QString jsString = QString("setAllTracksVisibility(%1);")
                     .arg(show ? "true" : "false");
  qDebug() << "Executing JS:" << jsString;
  evaluateJS(jsString);
}

//------------------------------------------------------------------------
void
LeafletMap::setAllRoutesVisibility(bool show)
{
  qDebug() << "LeafletMap::setAllRoutesVisibility called with:" << show;
  evaluateJS(QString("console.log('JS: setAllRoutesVisibility called with: %1'); if (%1) map.addLayer(rteGroup); else map.removeLayer(rteGroup);")
             .arg(show ? "true" : "false"));
}

//------------------------------------------------------------------------
void
LeafletMap::resetBounds()
{
  evaluateJS(QStringList{
    QString("if (bounds.isValid()) {\n    map.fitBounds(bounds);\n}"),
  });
}

//------------------------------------------------------------------------
void
LeafletMap::panTo(const LatLng& loc)
{
  evaluateJS(QString("map.panTo(%1);").arg(fmtLatLng(loc)));
}

//------------------------------------------------------------------------
void
LeafletMap::resizeEvent(QResizeEvent* ev)
{
  QWebEngineView::resizeEvent(ev);
  if (mapPresent_) {
    evaluateJS(QString("map.invalidateSize();"));
  }
}

//------------------------------------------------------------------------
void
LeafletMap::setWaypointColorRed(int i)
{
  evaluateJS(QString("if (waypts[%1]) { waypts[%1].setIcon(redIcon); }").arg(i));
}

//------------------------------------------------------------------------
void
LeafletMap::setWaypointColorBlue(int i)
{
  evaluateJS(QString("if (waypts[%1]) { waypts[%1].setIcon(blueIcon); }").arg(i));
}

//------------------------------------------------------------------------
void
LeafletMap::frameTrack(int i)
{
  evaluateJS(QStringList{
    QString("if (trackLayers[%1] && trackLayers[%1].getBounds().isValid()) {\n").arg(i),
    QString("    map.fitBounds(trackLayers[%1].getBounds());\n").arg(i),
    "}",
  });
}

//------------------------------------------------------------------------
void
LeafletMap::frameRoute(int i)
{
  evaluateJS(QStringList{
    QString("if (rteGroup.getLayers()[%1] && rteGroup.getLayers()[%1].getBounds().isValid()) {\n").arg(i),
    QString("    map.fitBounds(rteGroup.getLayers()[%1].getBounds());\n").arg(i),
    "}",
  });
}

void
LeafletMap::evaluateJS(const QString& s, bool upd)
{
#ifdef DEBUG_JS_GENERATION
  *dbgout_ << s << '\n';
  dbgout_->flush();
#endif
  this->page()->runJavaScript(s);
  if (upd) {
    this->update();
  }
}

void
LeafletMap::evaluateJS(const QStringList& s, bool upd)
{
  evaluateJS(s.join('\n'), upd);
}
