// -*- C++ -*-
// $Id: filterwidgets.h,v 1.2 2009-11-02 20:38:02 robertl Exp $
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
#ifndef FILTERWIDGETS_H
#define FILTERWIDGETS_H


#include "ui_trackui.h"
#include "ui_wayptsui.h"
#include "ui_rttrkui.h"
#include "ui_miscfltui.h"
#include "filterdata.h"

class CheckEnabler;
class FilterOption;
//------------------------------------------------------------------------
class CheckEnabler: public QObject
{
  Q_OBJECT
    public:
  CheckEnabler(QObject *parent, QAbstractButton *ck, QWidget *w): QObject(parent), checkBox(ck)
  {
    widgetList << w;
    connect(ck, SIGNAL(clicked()), this, SLOT(checkStatusChanged()));
    checkStatusChanged();
    fixWhatsThis();
  }
  CheckEnabler(QObject *parent, QAbstractButton *ck, QList<QWidget *> &wl): 
    QObject(parent), checkBox(ck)
  {
    widgetList = wl;
    connect(ck, SIGNAL(clicked()), this, SLOT(checkStatusChanged()));
    checkStatusChanged();
    fixWhatsThis();
  }

public slots:
  void checkStatusChanged()
  {
    bool b = checkBox->isChecked();
    for (int i=0; i<widgetList.size(); i++) {
      widgetList[i]->setEnabled(b);
    }
  }
  
private:
  QAbstractButton *checkBox;
  QList<QWidget*> widgetList;
  void fixWhatsThis()
  {
    QString wts = checkBox->whatsThis();
    if (wts.length() != 0) {
      for (int i=0; i<widgetList.size(); i++) {
	QString s = widgetList[i]->whatsThis();
	if (s.length() == 0)
	  widgetList[i]->setWhatsThis(wts);
      }
    }
    QString wtf = checkBox->toolTip();
    if (wtf.length() != 0) {
      for (int i=0; i<widgetList.size(); i++) {
	QString s = widgetList[i]->toolTip();
	if (s.length() == 0)
	  widgetList[i]->setToolTip(wtf);
      }
    }
  }

};

//------------------------------------------------------------------------
class FilterOption
{
 public:
  FilterOption() {};
  virtual ~FilterOption() {};
  virtual void setWidgetValue() = 0;
  virtual void getWidgetValue() = 0;
};

//------------------------------------------------------------------------
class BoolFilterOption: public FilterOption
{
 public:
 BoolFilterOption(bool &b, QAbstractButton *ck): FilterOption(), b(b), checkBox(ck)
  {
  }
  void setWidgetValue() {checkBox->setChecked(b); }
  void getWidgetValue() {b = checkBox->isChecked();  }
    
 private:
  bool &b;
  QAbstractButton *checkBox;
};

//------------------------------------------------------------------------
class IntSpinFilterOption: public FilterOption
{
 public:
  IntSpinFilterOption(int &val, QSpinBox *sb, int bottom = -100, int top = 100): FilterOption(), val(val), spinBox(sb)
  {
    sb->setRange(bottom, top);
  }
  void setWidgetValue() {spinBox->setValue(val); }
  void getWidgetValue() {val = spinBox->value();  }
    
 private:
  int &val;
  QSpinBox *spinBox;
};

//------------------------------------------------------------------------
class StringFilterOption: public FilterOption
{
 public:
 StringFilterOption(QString &val, QLineEdit *le): FilterOption(), val(val), lineEdit(le)
  {
  }
  void setWidgetValue() {lineEdit->setText(val); }
  void getWidgetValue() {val = lineEdit->text();  }
    
 private:
  QString &val;
  QLineEdit *lineEdit;
};

//------------------------------------------------------------------------
class DoubleFilterOption: public FilterOption
{
 public:
  DoubleFilterOption(double &val, QLineEdit *le,
		     double minVal = -1.E308, 
		     double maxVal = 1.0E308,
		     int decimals = -1, 
		     char format = 'g'
		     ): FilterOption(), val(val), lineEdit(le), minVal(minVal),
			maxVal(maxVal), decimals(decimals), format(format)
  {
    le->setValidator(new QDoubleValidator(minVal, maxVal, decimals, le));
  }
  void setWidgetValue() {
    lineEdit->setText(QString("%1").arg(val, 0, format, decimals));
  }
  void getWidgetValue() {
    val = lineEdit->text().toDouble(); 
    val = qMin(val, maxVal);
    val = qMax(val, minVal);
  }
    
 private:
  double &val;
  QLineEdit *lineEdit;
  double minVal, maxVal;
  int decimals;
  char format;
};

//------------------------------------------------------------------------
class DateTimeFilterOption: public FilterOption
{
 public:
 DateTimeFilterOption(QDateTime &val, QDateTimeEdit *w): FilterOption(), val(val), w(w)
  {
  }
  void setWidgetValue() {w->setDateTime(val); }
  void getWidgetValue() {val = w->dateTime();  }
    
 private:
  QDateTime &val;
  QDateTimeEdit *w;
};

//------------------------------------------------------------------------
class ComboFilterOption: public FilterOption
{
 public:
 ComboFilterOption(int &val, QComboBox *w): FilterOption(), val(val), w(w)
  {
  }
  void setWidgetValue() {w->setCurrentIndex(val); }
  void getWidgetValue() {val = w->currentIndex();  }
    
 private:
  int &val;
  QComboBox *w;
};


//------------------------------------------------------------------------
class FilterWidget: public QWidget
{
public:
  FilterWidget(QWidget *parent) : QWidget(parent) {}
  ~FilterWidget() {
    for (int i=0; i<fopts.size(); i++)
      delete fopts[i];
  }

  void getWidgetValues() {
    for (int i=0; i<fopts.size(); i++) {
      fopts[i]->getWidgetValue();
    }
  }
  void setWidgetValues() {
    for (int i=0; i<fopts.size(); i++)
      fopts[i]->setWidgetValue();
  }
  void addCheckEnabler(QAbstractButton *ck, QWidget *w) {
    enbls << new CheckEnabler(this, ck, w);
  }
  void addCheckEnabler(QAbstractButton *ck, QList<QWidget *> &wl)
  {
    enbls << new CheckEnabler(this, ck, wl);
  }
  virtual void checkChecks(){
    for (int i=0; i<enbls.size(); i++)
      enbls[i]->checkStatusChanged();
  }

protected:
  QList <FilterOption*> fopts;
  QList <CheckEnabler *> enbls;
};

//------------------------------------------------------------------------

class TrackWidget: public FilterWidget
{
Q_OBJECT
 public:
  TrackWidget(QWidget *parent, TrackFilterData &tf);

  virtual void checkChecks(){
    otherCheckX();
    FilterWidget::checkChecks();
  }

 private:
  Ui_TrackWidget ui;
  TrackFilterData &tfd;

  private slots:
  void mergeCheckX();
  void otherCheckX();
  void splitDateX();
  void splitTimeX();
  void splitDistanceX();
  void packCheckX();
};

//------------------------------------------------------------------------
class WayPtsWidget: public FilterWidget
{
Q_OBJECT
 public:
  WayPtsWidget(QWidget *parent, WayPtsFilterData &wf);

 private:
  Ui_WayPtsWidget ui;
  WayPtsFilterData &wfd;
  
private slots:
  void locationsCkX();
  void shortNamesCkX();
};

//------------------------------------------------------------------------
class RtTrkWidget: public FilterWidget
{
Q_OBJECT
 public:
  RtTrkWidget(QWidget *parent, RtTrkFilterData &wf);

 private:
  Ui_RtTrkWidget ui;
  RtTrkFilterData &rfd;
};
//------------------------------------------------------------------------
class MiscFltWidget: public FilterWidget
{
Q_OBJECT
 public:
  MiscFltWidget(QWidget *, MiscFltFilterData &);

 private:
  Ui_MiscFltWidget ui;
  MiscFltFilterData &mfd;
};

#endif
