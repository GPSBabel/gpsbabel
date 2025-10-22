// -*- C++ -*-
#ifndef LEAFLETDLG_H
#define LEAFLETDLG_H

#include <QDialog>
#include <QItemSelection>
#include <QModelIndex>
#include <QObject>
#include <QPlainTextEdit>
#include <QPoint>
#include <QStandardItem>
#include <QStandardItemModel>
#include <QString>
#include <QWidget>
#include "gpx.h"
#include "leafletmap.h"
#include "ui_leafletdlg.h"         // for Ui_LeafletMapDlg

enum ItemDataRole {
    OriginalIndexRole = Qt::UserRole + 1
};

class LeafletMapDialog: public QDialog
{
  Q_OBJECT

public:
  LeafletMapDialog(QWidget* parent, const Gpx& mapData, const QString& geojsonData, QPlainTextEdit* te);



private:
  static constexpr bool debug_ = false;
  Ui::LeafletMapDlg ui_;
  LeafletMap* mapWidget_;
  QStandardItemModel* model_;
  QStandardItem* wptItem_;
  QStandardItem* trkItem_;
  QStandardItem* rteItem_;
  const Gpx& gpx_;
  bool itemChangedActive_{false};

private slots:
  void itemChangedX(QStandardItem* it);
  void treeDoubleClicked(const QModelIndex& idx);
  void itemClickedX(const QStandardItem* it);
  void selectionChangedX(const QItemSelection& sel, const QItemSelection& desel);
  void showContextMenu(const QPoint& pt);

protected:
  QString formatLength(double l);
  void appendWaypointInfo(QStandardItem* it, const GpxWaypoint& wpt);
  void appendTrackInfo(QStandardItem* it, const GpxTrack& trk);
  void appendRouteInfo(QStandardItem* it, const GpxRoute& rte);
  void showHideChild(const QStandardItem* child);
  void showHideChildren(const QStandardItem* top);
  void expandCollapseAll(QStandardItem* top, bool exp);
  void showHideAll(QStandardItem* top, bool ck);
  void showOnlyThis(QStandardItem* top, int menuRow);
  void showTopContextMenu(const QStringList& text, QStandardItem* top, const QPoint& pt);
  void showChildContextMenu(const QString& text, const QStandardItem* child, const QPoint& pt);
  void trace(const QString& label, const QStandardItem* it);
  void onMapLoadedAndRendered();
};
#endif