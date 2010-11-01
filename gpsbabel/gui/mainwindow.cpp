// -*- C++ -*-
// $Id: mainwindow.cpp,v 1.27 2010-11-01 03:30:42 robertl Exp $
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
#include <QDesktopServices>
#include <QFileDialog>
#include <QMessageBox>
#include <QMimeData>
#include <QProcess>
#include <QSettings>
#include <QTemporaryFile>
#include <QTextStream>

#include "mainwindow.h"
#include "../gbversion.h"
#include "aboutdlg.h"
#include "advdlg.h"
#include "appname.h"
#include "babeldata.h"
#include "donate.h"
#include "filterdlg.h"
#include "formatload.h"
#include "gmapdlg.h"
#include "help.h"
#include "optionsdlg.h"
#include "preferences.h"
#include "processwait.h"
#include "version_mismatch.h"
#include "upgrade.h"

const int BabelData::noType = -1;
const int BabelData::fileType = 0;
const int BabelData::deviceType = 1;
//------------------------------------------------------------------------
QString MainWindow::findBabelVersion()
{
  QProcess babel;
  babel.start("gpsbabel", QStringList() << "-V");
  if (!babel.waitForStarted())
    return QString();
  babel.closeWriteChannel();
  if (!babel.waitForFinished())
    return QString();

  QString str = babel.readAll();
  is_beta = str.contains("-beta");
  str.replace("Version",  "");
  str.replace("GPSBabel",  "");
  str.replace(QRegExp("^[\\s]*"),  "");
  str.replace(QRegExp("[\\s]+$"),  "");
  str = str.simplified();
  return str;
}

//------------------------------------------------------------------------
// Decides whether available beta upgrades are suggested to user for download.
bool MainWindow::allowBetaUpgrades()
{
  // If this is a beta version (which means the user consciously downloaded
  // it and decided to be on the beta track or the user has ticked the 
  // 'suggest beta upgrade' box, allow betas to be suggested for installation.
  return is_beta || bd.allowBetaUpgrades;
}

//------------------------------------------------------------------------
static QStringList getCharSets()
{
  QProcess babel;
  babel.start("gpsbabel", QStringList() << "-l");
  if (!babel.waitForStarted())
    return QStringList();
  babel.closeWriteChannel();
  if (!babel.waitForFinished())
    return QStringList();

  QStringList strList;
  QTextStream tstream(babel.readAll());
  while(!tstream.atEnd()) {
    QString l = tstream.readLine();
    if (QRegExp("^\\*").indexIn(l) == 0) {
      l.replace(QRegExp("^[\\*\\s]*"),  "");
      l.replace(QRegExp("[\\s]+$"),  "");
      strList << l;
    }
  }
  return strList;
}

//------------------------------------------------------------------------
static QString MakeOptions(const QList<FormatOption>& options)
{
  QString str;
  for (int i=0; i<options.size(); i++) {
    if (options[i].getSelected()) {
      str += ",";
      str += options[i].getName();
      if (options[i].getType() == FormatOption::OPTbool) {
        str += "=1";
      } else {
        str += "=" + options[i].getValue().toString();
      }
    } else {
      if (options[i].getType() == FormatOption::OPTbool) {
        str += "," + options[i].getName() + "=0";
      }
    }
  }
  return str;
}

//------------------------------------------------------------------------
static QString MakeOptionsNoLeadingComma(const QList<FormatOption>& options)
{
  QString str = MakeOptions(options);
  return (str.length()) ? str.mid(1) : str;

}

//------------------------------------------------------------------------
MainWindow::MainWindow(QWidget* parent): QMainWindow(parent)
{
  loadFormats();
  ui.setupUi(this);
  setWindowTitle(appName);
  babelVersion = findBabelVersion();
  fmtChgInterlock = false;
  loadDeviceNameCombos();
  loadCharSetCombos();

  connect(ui.inputFileOptBtn,        SIGNAL(clicked()), this, SLOT(inputFileOptBtnClicked()));
  connect(ui.inputDeviceOptBtn,      SIGNAL(clicked()), this, SLOT(inputDeviceOptBtnClicked()));
  connect(ui.inputFileNameBrowseBtn, SIGNAL(clicked()), this, SLOT(browseInputFile()));

  ui.outputFileOptBtn->setAutoExclusive(false);
  ui.outputDeviceOptBtn->setAutoExclusive(false);
  connect(ui.outputFileOptBtn,        SIGNAL(clicked()), this, SLOT(outputFileOptBtnClicked()));
  connect(ui.outputDeviceOptBtn,      SIGNAL(clicked()), this, SLOT(outputDeviceOptBtnClicked()));
  connect(ui.outputFileNameBrowseBtn, SIGNAL(clicked()), this, SLOT(browseOutputFile()));

  connect(ui.actionQuit, SIGNAL(triggered()), this, SLOT(closeActionX()));
  connect(ui.actionHelp, SIGNAL(triggered()), this, SLOT(helpActionX()));
  connect(ui.actionAbout, SIGNAL(triggered()), this, SLOT(aboutActionX()));
  connect(ui.actionVisit_Website, SIGNAL(triggered()), this, SLOT(visitWebsiteActionX()));
  connect(ui.actionMake_a_Donation, SIGNAL(triggered()), this, SLOT(donateActionX()));
  connect(ui.actionUpgradeCheck, SIGNAL(triggered()), this, SLOT(upgradeCheckActionX()));
  connect(ui.actionPreferences, SIGNAL(triggered()), this, SLOT(preferencesActionX()));

  connect(ui.inputFormatCombo,  SIGNAL(currentIndexChanged(int)),
	  this,                 SLOT(inputFormatChanged(int)));
  connect(ui.outputFormatCombo, SIGNAL(currentIndexChanged(int)),
	  this,                 SLOT(outputFormatChanged(int)));
  connect(ui.inputOptionsBtn,   SIGNAL(clicked()),
	  this,                 SLOT(inputOptionButtonClicked()));
  connect(ui.outputOptionsBtn , SIGNAL(clicked()),
	  this,                 SLOT(outputOptionButtonClicked()));
  connect(ui.moreOptionButton , SIGNAL(clicked()),
	  this,                 SLOT(moreOptionButtonClicked()));

  connect(ui.buttonBox->button(QDialogButtonBox::Apply), SIGNAL(clicked()), this, SLOT(applyActionX()));
  connect(ui.buttonBox, SIGNAL(rejected()), this, SLOT(closeActionX()));
  connect(ui.xlateFiltersBtn, SIGNAL(clicked()), this, SLOT(filtersClicked()));

  connect(ui.inputFileNameText, SIGNAL(textEdited(QString)), this, SLOT(inputFileNameEdited()));
  connect(ui.outputFileNameText, SIGNAL(textEdited(QString)), this, SLOT(outputFileNameEdited()));


  ui.buttonBox->button(QDialogButtonBox::Apply)->setIcon(QIcon(":images/runit.png"));
  ui.buttonBox->button(QDialogButtonBox::Close)->setIcon(QIcon(":images/exit.png"));


  ui.inputOptionsText->setReadOnly(true);
  ui.outputOptionsText->setReadOnly(true);
#if 0
  // 02/28/10  - let's try letting people edit these outside the browse.
  ui.inputFileNameText->setReadOnly(true);
  ui.outputFileNameText->setReadOnly(true);
#else
  setAcceptDrops(true);
#endif
  ui.wayPtLabel->setText("");
  ui.trackLabel->setText("");
  ui.routeLabel->setText("");
  lights[0] = QPixmap::fromImage(QImage(":images/00.png").scaledToHeight(20, Qt::SmoothTransformation));
  lights[1] = QPixmap::fromImage(QImage(":images/01.png").scaledToHeight(20, Qt::SmoothTransformation));
  lights[2] = QPixmap::fromImage(QImage(":images/10.png").scaledToHeight(20, Qt::SmoothTransformation));
  lights[3] = QPixmap::fromImage(QImage(":images/11.png").scaledToHeight(20, Qt::SmoothTransformation));

  ui.outputWindow->setReadOnly(true);


  //--- Restore from registry
  restoreSettings();

  upgrade = new UpgradeCheck(parent, formatList, bd);
  if (bd.startupVersionCheck) {
    upgrade->checkForUpgrade(babelVersion, bd.upgradeCheckTime, 
                             allowBetaUpgrades());
  }

  if (!bd.ignoreVersionMismatch && babelVersion != VERSION) {
    VersionMismatch vm(0, babelVersion, QString(VERSION));

    vm.exec();
    bd.ignoreVersionMismatch = vm.neverAgain();
  }
}

//------------------------------------------------------------------------
MainWindow::~MainWindow()
{
  if (upgrade)
    delete upgrade;
}

//------------------------------------------------------------------------
void MainWindow::loadInputDeviceNameCombo(const QString &format)
{
  ui.inputDeviceNameCombo->clear();
  // Later, we can probe the system for multiple USB devices and populate 
  // here.
  if (formatSupportsUSB(format))
    ui.inputDeviceNameCombo->addItem("usb:");
  if (formatSupportsSerial(format))
    osLoadDeviceNameCombos(ui.inputDeviceNameCombo);
  // If only one choice, just disable it.
  ui.inputDeviceNameCombo->setEnabled(ui.inputDeviceNameCombo->count() > 1);
}

//------------------------------------------------------------------------
void MainWindow::loadOutputDeviceNameCombo(const QString &format)
{
  ui.outputDeviceNameCombo->clear();
  // Later, we can probe the system for multiple USB devices and populate 
  // here.
  if (formatSupportsUSB(format))
    ui.outputDeviceNameCombo->addItem("usb:");
  if (formatSupportsSerial(format))
    osLoadDeviceNameCombos(ui.outputDeviceNameCombo);
  // If only one choice, just disable it.
  ui.outputDeviceNameCombo->setEnabled(ui.outputDeviceNameCombo->count() > 1);
}

//------------------------------------------------------------------------
void MainWindow::loadDeviceNameCombos()
{
  loadInputDeviceNameCombo("");
  loadOutputDeviceNameCombo("");
}
//------------------------------------------------------------------------
void MainWindow::loadCharSetCombos()
{
  charSets = getCharSets();

  ui.inputCharSetCombo->clear();
  ui.outputCharSetCombo->clear();
  ui.inputCharSetCombo->addItem(tr("default"), QVariant(-1));
  ui.outputCharSetCombo->addItem(tr("default"), QVariant(-1));
  for (int i=0; i<charSets.size(); i++) {
    ui.inputCharSetCombo->addItem(charSets[i], QVariant(i));
    ui.outputCharSetCombo->addItem(charSets[i], QVariant(i));
  }
}
//------------------------------------------------------------------------
void MainWindow::checkCharSetCombos()
{
  ui.inputCharSetCombo->setEnabled(bd.enableCharSetXform);
  ui.outputCharSetCombo->setEnabled(bd.enableCharSetXform);
  ui.inputCharSetCombo->setVisible(bd.enableCharSetXform);
  ui.outputCharSetCombo->setVisible(bd.enableCharSetXform);
}
//------------------------------------------------------------------------
void MainWindow::inputFileOptBtnClicked()
{
  fmtChgInterlock = true;
  QString fmt = bd.inputFileFormat;
  ui.inputStackedWidget->setCurrentWidget(ui.inputFilePage);
  QList<int>indices = inputFileFormatIndices();
  ui.inputFormatCombo->clear();
  for (int i=0; i<indices.size(); i++) {
    int k = indices[i];
    if (!formatList[k].isHidden())
      ui.inputFormatCombo->addItem(formatList[k].getDescription(), QVariant(k));
  }
  setComboToFormat(ui.inputFormatCombo, fmt, true);
  fmtChgInterlock = false;
}

//------------------------------------------------------------------------
void MainWindow::inputDeviceOptBtnClicked()
{
  fmtChgInterlock = true;
  QString fmt = bd.inputDeviceFormat;
  ui.inputStackedWidget->setCurrentWidget(ui.inputDevicePage);
  QList<int>indices = inputDeviceFormatIndices();
  ui.inputFormatCombo->clear();
  for (int i=0; i<indices.size(); i++) {
    int k = indices[i];
    if (!formatList[k].isHidden())
      ui.inputFormatCombo->addItem(formatList[k].getDescription(), QVariant(k));
  }
  setComboToFormat(ui.inputFormatCombo, fmt, false);
  fmtChgInterlock = false;
}

//------------------------------------------------------------------------
void MainWindow:: outputFileOptBtnClicked()
{
  fmtChgInterlock = true;
  if (ui.outputFileOptBtn->isChecked()) {
    ui.outputFilePage->setEnabled(true);
    ui.outputDeviceOptBtn->setChecked(false);
    QString fmt = bd.outputFileFormat;
    ui.outputStackedWidget->setCurrentWidget(ui.outputFilePage);
    QList<int>indices = outputFileFormatIndices();
    ui.outputFormatCombo->clear();
    for (int i=0; i<indices.size(); i++) {
      int k = indices[i];
      ui.outputFormatCombo->addItem(formatList[k].getDescription(), QVariant(k));
    }
    setComboToFormat(ui.outputFormatCombo, fmt, true);
  }
  else {
    ui.outputStackedWidget->setCurrentWidget(ui.outputFilePage);
    ui.outputFilePage->setEnabled(false);
  }
  fmtChgInterlock = false;
}

//------------------------------------------------------------------------
void MainWindow:: outputDeviceOptBtnClicked()
{
  fmtChgInterlock = true;
  if (ui.outputDeviceOptBtn->isChecked()) {
    ui.outputDevicePage->setEnabled(true);
    ui.outputFileOptBtn->setChecked(false);
    QString fmt = bd.outputDeviceFormat;
    ui.outputStackedWidget->setCurrentWidget(ui.outputDevicePage);
    QList<int>indices = outputDeviceFormatIndices();
    ui.outputFormatCombo->clear();
    for (int i=0; i<indices.size(); i++) {
      int k = indices[i];
      ui.outputFormatCombo->addItem(formatList[k].getDescription(), QVariant(k));
    }
    setComboToFormat(ui.outputFormatCombo, fmt, false);
  }
  else {
    ui.outputStackedWidget->setCurrentWidget(ui.outputDevicePage);
    ui.outputDevicePage->setEnabled(false);
  }
  fmtChgInterlock = false;
}
void MainWindow::inputFileNameEdited()
{
  bd.inputFileNames.clear();
  bd.inputFileNames << ui.inputFileNameText->text();
}

void MainWindow::outputFileNameEdited()
{
  bd.outputFileName = ui.outputFileNameText->text();

}

//------------------------------------------------------------------------
QString MainWindow::filterForFormat(int idx)
{
  QString str = formatList[idx].getDescription();
  str.replace(QRegExp("\\("), "[");
  str.replace(QRegExp("\\)"), "]");
  QStringList extensions = formatList[idx].getExtensions();

  // If we don't have any meaningful extensions available for this format,
  // don't be clever here; just fall through to "All files" case.
  if (extensions.size() > 0 && !extensions[0].isEmpty()) {
    str += " (";
    for (int i=0; i<extensions.size(); i++) {
      if (i!= 0)
        str += " ";
      str += "*." + extensions[i];
    }
    str += ");;";
  }
  str += "All Files (*.*)";
  return str;
}
//------------------------------------------------------------------------
QString MainWindow::ensureExtensionPresent(const QString &name, int idx)
{
  QString outname = name;
  if (QFileInfo(name).suffix().length() == 0) {
    QStringList extensions = formatList[idx].getExtensions();
    if (extensions.size() > 0)
      outname += "." + extensions[0];
  }
  return outname;
}

//------------------------------------------------------------------------
bool MainWindow::filterForFormatIncludes(int idx, const QString &fmt)
{
  QStringList extensions = formatList[idx].getExtensions();
  for (int i=0; i<extensions.size(); i++) {
    if (fmt == extensions[i])
      return true;
  }
  return false;
}

//------------------------------------------------------------------------
int MainWindow::currentComboFormatIndex(QComboBox *comboBox)
{
  int idx = comboBox->currentIndex();
  if (idx<0 || idx >= comboBox->count()) {
    //    QMessageBox::critical(0, appName, "*** Internal Error -- current combo index is invalid!");
    return 0;
  }
  return comboBox->itemData(idx).toInt();
}
//------------------------------------------------------------------------
void MainWindow::browseInputFile()
{
  QString startFile = bd.inputFileNames.size() ? bd.inputFileNames[0] : bd.inputBrowse;
  int idx = currentComboFormatIndex(ui.inputFormatCombo);
  QFileInfo finfo(startFile);
  if (!finfo.isDir() && (!filterForFormatIncludes(idx, finfo.suffix()))) {
    startFile = finfo.dir().absolutePath();
  }

  QStringList userList =
    QFileDialog::getOpenFileNames(0, tr("Select one or more input files"),
				  startFile,
				  filterForFormat(idx));
  if (userList.size()) {
    bd.inputBrowse = userList[0];
    bd.inputFileNames = userList;
    QString str;
    for (int i=0; i<bd.inputFileNames.size(); i++) {
      if (i != 0)
        str += ", ";
      str += "\"" + bd.inputFileNames[i] + "\"";
    }
    ui.inputFileNameText->setText(str);
  }
}

//------------------------------------------------------------------------
void MainWindow::browseOutputFile()
{
  int idx = currentComboFormatIndex(ui.outputFormatCombo);
  QString startFile = bd.outputFileName.length() == 0 ? bd.outputBrowse : bd.outputFileName;
  QFileInfo finfo(startFile);
  if (!finfo.isDir() && (!filterForFormatIncludes(idx, finfo.suffix()))) {
    startFile = finfo.dir().absolutePath();
  }

  QString str =
    QFileDialog::getSaveFileName(0, tr("Output File Name"),
				 startFile,
				 filterForFormat(idx));
  if (str.length() != 0) {
    str = ensureExtensionPresent(str, idx);
    bd.outputBrowse = str;
    bd.outputFileName = str;
    ui.outputFileNameText->setText(str);
  }
}

//------------------------------------------------------------------------
QList<int> MainWindow::inputFileFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList.size(); i++) {
    if (formatList[i].isReadSomething() && formatList[i].isFileFormat())
      indices<<i;
  }
  return indices;
}

//------------------------------------------------------------------------
QList<int> MainWindow::inputDeviceFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList.size(); i++) {
    if (formatList[i].isReadSomething() && formatList[i].isDeviceFormat())
      indices<<i;
  }
  return indices;
}

//------------------------------------------------------------------------
QList<int> MainWindow::outputFileFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList.size(); i++) {
    if (formatList[i].isWriteSomething() && formatList[i].isFileFormat())
      indices<<i;
  }
  return indices;
}

//------------------------------------------------------------------------
QList<int> MainWindow::outputDeviceFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList.size(); i++) {
    if (formatList[i].isWriteSomething() && formatList[i].isDeviceFormat())
      indices<<i;
  }
  return indices;
}

//------------------------------------------------------------------------
void MainWindow::loadFormats()
{
  if (!FormatLoad().getFormats(formatList)) {
    QMessageBox::information(0, QString(appName),
			     tr("Error reading format configuration.  "
				"Check that the backend program \"gpsbabel\" is properly installed "
				"and is in the current PATH\n\n"
				"This program cannot continue."));
    exit(1);
  }
  if (inputFileFormatIndices().size() == 0 ||
      inputDeviceFormatIndices().size() == 0 ||
      outputFileFormatIndices().size() == 0 ||
      outputDeviceFormatIndices().size() == 0) {
    QMessageBox::information(0, QString(appName),
			     tr("Some file/device formats were not found during initialization.  "
				"Check that the backend program \"gpsbabel\" is properly installed "
				"and is in the current PATH\n\n"
				"This program cannot continue."));
    exit(1);
  }
}
//------------------------------------------------------------------------
static int iconIndex(bool a, bool b)
{
  return ((a?1:0)*2) + (b?1:0);
}

//------------------------------------------------------------------------
void MainWindow::setIndicatorLights(QLabel *label, const QString &type, int code)
{
  label->setPixmap(lights[code]);
  QString s;
  switch (code)
    {
    default:
    case 0:
      s = tr("Input and output formats do not support %1").arg(type);
      break;
    case 1:
      s = tr("Input does not support %1; output format supports %2").arg(type).arg(type);
      break;
    case 2:
      s = tr("Input format supports %1; output format does not support %2").arg(type).arg(type);
      break;
    case 3:
      s = tr("Both input and output formats support %1").arg(type);
      break;
    }
  label->setToolTip(s);
}

//------------------------------------------------------------------------
void MainWindow::crossCheckInOutFormats()
{
  if (ui.inputFormatCombo->count() == 0 ||
      ui.outputFormatCombo->count() == 0) {
    // During format/device switch this is true
    return;
  }
  Format ifmt = formatList[currentComboFormatIndex(ui.inputFormatCombo)];
  Format ofmt = formatList[currentComboFormatIndex(ui.outputFormatCombo)];

  ui.xlateWayPtsCk->setEnabled(ifmt.isReadWaypoints() && ofmt.isWriteWaypoints());
  ui.xlateTracksCk->setEnabled(ifmt.isReadTracks()    && ofmt.isWriteTracks());
  ui.xlateRoutesCk->setEnabled(ifmt.isReadRoutes()    && ofmt.isWriteRoutes());

  setIndicatorLights(ui.wayPtLabel, tr("waypoints"), iconIndex(ifmt.isReadWaypoints(), ofmt.isWriteWaypoints()));
  setIndicatorLights(ui.trackLabel, tr("tracks"), iconIndex(ifmt.isReadTracks(), ofmt.isWriteTracks()));
  setIndicatorLights(ui.routeLabel, tr("routes"), iconIndex(ifmt.isReadRoutes(), ofmt.isWriteRoutes()));
}

//------------------------------------------------------------------------
void MainWindow::displayOptionsText(QLineEdit *le, QComboBox *combo, bool isInput)
{
  int fidx = combo->itemData(combo->currentIndex()).toInt();
  if (isInput)
    le->setText(MakeOptionsNoLeadingComma(formatList[fidx].getInputOptions()));
  else
    le->setText(MakeOptionsNoLeadingComma(formatList[fidx].getOutputOptions()));

}

//------------------------------------------------------------------------
void MainWindow::setComboToFormat(QComboBox *comboBox, const QString &name, bool isFile)
{
  int fidx = -1;
  for (int i=0; i<formatList.size(); i++) {
    if (formatList[i].getName() == name &&
	formatList[i].isFileFormat() == isFile) {
      fidx = i;
      break;
    }
  }
  if (fidx >=0) {
    for (int i=0; i<comboBox->count(); i++) {
      if (comboBox->itemData(i).toInt() == fidx) {
	comboBox->setCurrentIndex(i);
	break;
      }
    }
  }
}

//------------------------------------------------------------------------
bool MainWindow::formatSupportsUSB(const QString &format)
{
    return (format == "garmin" || format == "delbin");
}

//------------------------------------------------------------------------
bool MainWindow::formatSupportsSerial(const QString &format)
{
    return (format != "delbin");
}

//------------------------------------------------------------------------
void MainWindow::inputFormatChanged(int comboIdx)
{
  if (fmtChgInterlock)
    return;
  int fidx = ui.inputFormatCombo->itemData(comboIdx).toInt();
  ui.inputOptionsBtn->setEnabled(formatList[fidx].getInputOptions().size()>0);
  displayOptionsText(ui.inputOptionsText,  ui.inputFormatCombo, true);
  crossCheckInOutFormats();

  if (ui.inputFileOptBtn->isChecked())
    bd.inputFileFormat =formatList[fidx].getName();
  else
    bd.inputDeviceFormat = formatList[fidx].getName();

  loadInputDeviceNameCombo(formatList[fidx].getName());
}

//------------------------------------------------------------------------
void MainWindow::outputFormatChanged(int comboIdx)
{
  if (fmtChgInterlock)
    return;
  int fidx = ui.outputFormatCombo->itemData(comboIdx).toInt();
  ui.outputOptionsBtn->setEnabled(formatList[fidx].getOutputOptions().size()>0);
  displayOptionsText(ui.outputOptionsText,  ui.outputFormatCombo, false);
  crossCheckInOutFormats();

  if (ui.outputFileOptBtn->isChecked())
    bd.outputFileFormat =formatList[fidx].getName();
  else if (ui.outputDeviceOptBtn->isChecked())
    bd.outputDeviceFormat = formatList[fidx].getName();

  loadOutputDeviceNameCombo(formatList[fidx].getName());
}

//------------------------------------------------------------------------
void MainWindow::inputOptionButtonClicked()
{
  int fidx = currentComboFormatIndex(ui.inputFormatCombo);
  if (formatList[fidx].getInputOptionsRef()->size() == 0) {
    QMessageBox::information
      (0, appName,
       tr("There are no input options for format \"%1\"").arg(formatList[fidx].getDescription()));
  }
  else {
    OptionsDlg optionDlg(0,
			 formatList[fidx].getName(),
			 formatList[fidx].getInputOptionsRef(),
			 formatList[fidx].getHtml());
    optionDlg.setWindowTitle(QString(appName) + " - " + tr("Options for %1").arg(formatList[fidx].getName()));
    optionDlg.exec();
    displayOptionsText(ui.inputOptionsText,  ui.inputFormatCombo, true);
  }
}

//------------------------------------------------------------------------
void MainWindow::outputOptionButtonClicked()
{
  int fidx = currentComboFormatIndex(ui.outputFormatCombo);
  if (formatList[fidx].getOutputOptionsRef()->size() == 0) {
    QMessageBox::information
      (0, appName,
       tr("There are no output options for format \"%1\"").arg(formatList[fidx].getDescription()));
  }
  else {
    OptionsDlg optionDlg(0, 
			 formatList[fidx].getName(), 
			 formatList[fidx].getOutputOptionsRef(),
			 formatList[fidx].getHtml());
    optionDlg.setWindowTitle(QString(appName) + " - " + tr("Options for %1").arg(formatList[fidx].getName()));
    optionDlg.exec();
    displayOptionsText(ui.outputOptionsText,  ui.outputFormatCombo, false);
  }
}



//------------------------------------------------------------------------
bool MainWindow::isOkToGo()
{
  if (!((ui.xlateWayPtsCk->isChecked() && ui.xlateWayPtsCk->isEnabled()) ||
	(ui.xlateRoutesCk->isChecked() && ui.xlateRoutesCk->isEnabled()) ||
	(ui.xlateTracksCk->isChecked() && ui.xlateTracksCk->isEnabled()))) {
    QMessageBox::information(0, QString(appName), tr("No valid waypoints/routes/tracks translation specified"));
    return false;
  }

  // Paper over what didn't happen in inputBrowse() if the user edited
  // the filename fields directly.
  if ((bd.inputType == BabelData::fileType) &&
      (bd.inputFileNames.size() == 0) &&
      (!ui.inputFileNameText->text().isEmpty())) {
    bd.inputFileNames << ui.inputFileNameText->text();
  }
  if ((bd.outputType == BabelData::fileType) &&
      (bd.outputFileName.size() == 0) &&
      (!ui.outputFileNameText->text().isEmpty())) {
    bd.outputFileName = ui.outputFileNameText->text();
  }

  if ((bd.inputType == BabelData::fileType) &&
      (bd.inputFileNames.size() == 0)) {
    QMessageBox::information(0, QString(appName), tr("No input file specified"));
    return false;
  }

  if (bd.outputType == BabelData::noType && bd.previewGmap == true) {
  }
  if (bd.outputType == BabelData::noType && bd.previewGmap == false) {
    QMessageBox::information(0, QString(appName), tr("No valid output specified"));
    return false;
  }
  else if (bd.outputType == BabelData::fileType &&
	   bd.outputFileName.length() == 0) {
    QMessageBox::information(0, QString(appName), tr("No output file specified"));
    return false;
  }
  return true;
}

//------------------------------------------------------------------------
bool MainWindow::runGpsbabel(const QStringList &args, QString &errorString,
			  QString &outputString)
{
  QProcess *proc = new QProcess(0);
  QString name = "gpsbabel";
  proc->start(name, args);
  ProcessWaitDialog *waitDlg = new ProcessWaitDialog(0, proc);

  if (proc->state() == QProcess::NotRunning) {
    errorString = QString(tr("Process \"%1\" did not start")).arg(name);
    return false;
  }

  waitDlg->show();
  waitDlg->exec();
  int exitCode = -1;
  bool retStatus = false;
  if (waitDlg->getExitedNormally()) {
    exitCode = waitDlg->getExitCode();
    if (exitCode == 0)
      retStatus = true;
    else  {
      errorString =
	QString(tr("Process exited unsucessfully with code %1"))
	.arg(exitCode);
      retStatus = false;
    }
  }
  else {
    retStatus = false;
    errorString = waitDlg->getErrorString();
  }
  outputString = waitDlg->getOutputString();
  delete proc;
  delete waitDlg;
  return retStatus;
}

//------------------------------------------------------------------------
int MainWindow::formatIndexFromName(bool isFile, const QString &nm)
{
  for (int i= 0; i<formatList.size(); i++) {
    if (nm == formatList[i].getName() && formatList[i].isFileFormat() == isFile)
      return i;
  }
  return 0;
}

//------------------------------------------------------------------------
QString MainWindow::charSetFromCombo(QComboBox *combo)
{
  int i = combo->itemData((combo->currentIndex())).toInt();
  return (i >=0 ) ? charSets[i] : QString();
}

//------------------------------------------------------------------------
void MainWindow::setComboToCharSet(QComboBox *combo, const QString &cset)
{
  for (int i=0; i<charSets.size(); i++) {
    if (charSets[i] == cset) {
      combo->setCurrentIndex(i+1); // first index is default;
    }
  }
}
//------------------------------------------------------------------------
void MainWindow::applyActionX()
{
  getWidgetValues();
  if (!isOkToGo())
    return;

  QStringList args;

  if (bd.debugLevel >=0)     args << QString("-D%1").arg(bd.debugLevel);
  if (bd.synthShortNames)    args << "-s";

  // Input char set if specified
  if (bd.enableCharSetXform && bd.inputCharSet != QString())
    args << "-c" << bd.inputCharSet;

  Format ifmt = formatList[currentComboFormatIndex(ui.inputFormatCombo)];
  Format ofmt = formatList[currentComboFormatIndex(ui.outputFormatCombo)];

  if (bd.xlateWayPts && ifmt.isReadWaypoints() && ofmt.isWriteWaypoints())
    args << "-w";
  if (bd.xlateRoutes && ifmt.isReadRoutes()    && ofmt.isWriteRoutes())
    args << "-r";
  if (bd.xlateTracks && ifmt.isReadTracks()    && ofmt.isWriteTracks())
    args << "-t";

  // Input type, with options
  bool iisFile = (bd.inputType == BabelData::fileType);
  int fidx = formatIndexFromName(iisFile, iisFile ?
				 bd.inputFileFormat : bd.inputDeviceFormat);
  args << "-i";
  args << (formatList[fidx].getName() + MakeOptions(formatList[fidx].getInputOptions()));

  // Input file(s) or device
  int read_use_count = 0;
  if (bd.inputType == BabelData::fileType) {
    for (int i=0; i<bd.inputFileNames.size(); i++) {
      args << "-f" << bd.inputFileNames[i];
      read_use_count++;
    }
  }
  else {
    args << "-f" << bd.inputDeviceName;
    read_use_count++;
  }
  formatList[fidx].bumpReadUseCount(read_use_count);

  // --- Filters!
  args << filterData.getAllFilterStrings();

  // Output char set if specified
  if (bd.enableCharSetXform && bd.outputCharSet != QString())
    args << "-c" << bd.outputCharSet;

  // Output type, with options
  if (bd.outputType != BabelData::noType) {
    bool outIsFile = (bd.outputType == BabelData::fileType);
    fidx = formatIndexFromName(outIsFile, (outIsFile ?
					   bd.outputFileFormat : bd.outputDeviceFormat));
    args << "-o";
    args << (formatList[fidx].getName() + MakeOptions(formatList[fidx].getOutputOptions()));

    // output file or device option
    if (outIsFile) {
      if (bd.outputFileName != "")
	      args << "-F" << bd.outputFileName;
    }
    else if (bd.outputType == BabelData::deviceType) {
      args << "-F" << bd.outputDeviceName;
    }
    // GUI only ever writes a single file at a time.
    formatList[fidx].bumpWriteUseCount(1);
  }

  // Now output for preview in google maps
  QString tempName;
  if (bd.previewGmap) {
    QTemporaryFile ftemp;
    ftemp.open();
    tempName = ftemp.fileName();
    ftemp.close();

    // Ideally, expost this in the UI.  For now, just split the track
    // if we've no recorded fixes for > 5 mins and we've moved > 300 meters.
    //args << "-x";
    //args << "track,pack,sdistance=0.3k,split=5m";

    args << "-o";
    args << "gpx";
    args << "-F" << tempName;
  }

  ui.outputWindow->clear();
  ui.outputWindow->appendPlainText("gpsbabel " + args.join(" "));

  QString errorString, outputString;
  QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
  bool x = runGpsbabel(args, errorString, outputString);
  QApplication::restoreOverrideCursor();

  ui.outputWindow->appendPlainText(outputString);
  if (x) {
    ui.outputWindow->appendPlainText(tr("Translation successful"));
    if (bd.previewGmap) {
      this->hide();
      GMapDialog dlg(0, tempName, bd.debugLevel >=1 ? ui.outputWindow : 0);
      dlg.show();
      dlg.exec();
      this->show();
    }
  }
  else
    ui.outputWindow->appendPlainText(tr("Error running gpsbabel: %1\n").arg(errorString));
}

//------------------------------------------------------------------------
void MainWindow::closeActionX()
{
  QDateTime wt= upgrade->getUpgradeWarningTime();
  if (wt.isValid()) {
    bd.upgradeCheckTime = wt;
  }
  bd.runCount++;

  QDateTime now = QDateTime::currentDateTime();
  if((bd.runCount > 5) && (bd.donateSplashed.daysTo(now) > 30)) {
    Donate donate(0);
    if (bd.donateSplashed.date() == QDate(2010,1,1))
      donate.showNever(false);
    donate.exec();
    bd.donateSplashed = now;
  }
  saveSettings();
  delete upgrade;
  upgrade = 0;
  qApp->exit(0);
}

//------------------------------------------------------------------------
void MainWindow::closeEvent(QCloseEvent*)
{
  closeActionX();
}

//------------------------------------------------------------------------
void MainWindow::donateActionX()
{
  QDesktopServices::openUrl(QString("http://www.gpsbabel.org/contribute.html?gbversion=" VERSION));
}

//------------------------------------------------------------------------
void MainWindow::visitWebsiteActionX()
{
  QDesktopServices::openUrl(QString("http://www.gpsbabel.org"));
}

//------------------------------------------------------------------------
void MainWindow::dragEnterEvent(QDragEnterEvent *event)
{
  event->acceptProposedAction();
}

void MainWindow::dropEvent(QDropEvent *event)
{
  foreach (QString format, event->mimeData()->formats()) {
    if (format == "text/uri-list") {
      QList<QUrl> urlList = event->mimeData()->urls();
      bd.inputFileNames.clear();
      for (int i = 0; i < urlList.size(); ++i) {
        QFileInfo file_info(urlList.at(i).path());
        QString name = file_info.filePath();
        QString ext = file_info.suffix();

        QString fmt = getFormatNameForExtension(ext);
        setComboToFormat(ui.inputFormatCombo, fmt, true);
        ui.inputFileNameText->setText(name);
        bd.inputFileNames << ui.inputFileNameText->text();
        event->acceptProposedAction();
      }
    }
  }
}
//------------------------------------------------------------------------
void MainWindow::setComboToDevice(QComboBox *comboBox, const QString &name)
{
  for (int i=0; i<comboBox->count(); i++) {
    if (comboBox->itemText(i) == name) {
      comboBox->setCurrentIndex(i);
      break;
    }
  }
}

//------------------------------------------------------------------------
void MainWindow::saveSettings()
{
  getWidgetValues();

  QSettings settings;
  bd.saveSettings(settings);
  for (int i=0; i<formatList.size(); i++)
    formatList[i].saveSettings(settings);
  for (int i=0; i<filterData.filters.size(); i++)
    filterData.filters[i]->saveSettings(settings);
}

//------------------------------------------------------------------------
void MainWindow::restoreSettings()
{
  QSettings settings;
  bd.restoreSettings(settings);
  for (int i=0; i<formatList.size(); i++)
    formatList[i].restoreSettings(settings);

  for (int i=0; i<filterData.filters.size(); i++)
    filterData.filters[i]->restoreSettings(settings);

  setWidgetValues();
}

//------------------------------------------------------------------------
void MainWindow::resetFormatDefaults()
{
  int ret = QMessageBox::warning
    (this, QString(appName),
     tr("Are you sure you want to reset all format options to default values?"),
     QMessageBox::Yes | QMessageBox::No);
  if (ret == QMessageBox::Yes) {
    for (int i=0; i<formatList.size(); i++)
      formatList[i].setToDefault();
    displayOptionsText(ui.inputOptionsText,  ui.inputFormatCombo, true);
    displayOptionsText(ui.outputOptionsText,  ui.outputFormatCombo, false);
  }
}

//------------------------------------------------------------------------
void MainWindow::moreOptionButtonClicked()
{
  AdvDlg advDlg(0, bd.synthShortNames,
		bd.enableCharSetXform, bd.previewGmap, bd.debugLevel);
  connect(advDlg.formatButton(), SIGNAL(clicked()),
	  this, SLOT(resetFormatDefaults()));
  advDlg.exec();
  checkCharSetCombos();
}
//------------------------------------------------------------------------
void MainWindow::aboutActionX()
{
  AboutDlg aboutDlg(0, babelVersion, QString(appName) + QString(" " VERSION));
  aboutDlg.setWindowTitle(tr("About %1").arg(appName));
  aboutDlg.exec();
}

//------------------------------------------------------------------------
void MainWindow::upgradeCheckActionX()
{
    upgrade->checkForUpgrade(babelVersion, 
                            QDateTime(QDate(2000, 1, 1), QTime(0, 0)), 
                            allowBetaUpgrades());
}

//------------------------------------------------------------------------
void MainWindow::preferencesActionX()
{
  Preferences preferences(0, formatList, bd);
  preferences.exec();

  // We may have changed the list of displayed formats.  Resynchronize.
  setWidgetValues();
}


//------------------------------------------------------------------------
void MainWindow::helpActionX()
{
  ShowHelp("index.html");
}
//------------------------------------------------------------------------
void MainWindow::filtersClicked()
{
  FilterDialog dlg(0, filterData);
  dlg.runDialog();
  updateFilterStatus();
}


//------------------------------------------------------------------------
void MainWindow::updateFilterStatus()
{
  bool filterActive = filterData.getAllFilterStrings().size();
  ui.filterStatus->setEnabled(filterActive);
  if (filterActive)
    ui.filterStatus->setToolTip(tr("One or more data filters are active"));
  else {
    ui.filterStatus->setToolTip(tr("No data filters are active"));
  }
}
//------------------------------------------------------------------------
void MainWindow::setWidgetValues()
{
  if (bd.inputType == BabelData::fileType) {
    ui.inputFileOptBtn->setChecked(true);
    inputFileOptBtnClicked();
    setComboToFormat(ui.inputFormatCombo, bd.inputFileFormat, true);
    ui.inputStackedWidget->setCurrentWidget(ui.inputFilePage);
  }
  else {
    ui.inputDeviceOptBtn->setChecked(true);
    inputDeviceOptBtnClicked();
    setComboToFormat(ui.inputFormatCombo, bd.inputDeviceFormat, false);
    loadInputDeviceNameCombo(bd.inputDeviceFormat);
    ui.inputStackedWidget->setCurrentWidget(ui.inputDevicePage);
  }
  setComboToDevice(ui.inputDeviceNameCombo, bd.inputDeviceName);
  setComboToCharSet(ui.inputCharSetCombo, bd.inputCharSet);

  if (bd.outputType == BabelData::fileType) {
    ui.outputFileOptBtn->setChecked(true);
    outputFileOptBtnClicked();
    setComboToFormat(ui.outputFormatCombo, bd.outputFileFormat, true);
    ui.outputStackedWidget->setCurrentWidget(ui.outputFilePage);
  }
  else if (bd.outputType == BabelData::deviceType) {
    ui.outputDeviceOptBtn->setChecked(true);
    outputDeviceOptBtnClicked();
    setComboToFormat(ui.outputFormatCombo, bd.outputDeviceFormat, false);
    loadOutputDeviceNameCombo(bd.outputDeviceFormat);
    ui.outputStackedWidget->setCurrentWidget(ui.outputDevicePage);
  }
  else {
    ui.outputFileOptBtn->setChecked(false);
    ui.outputDeviceOptBtn->setChecked(false);
    setComboToFormat(ui.outputFormatCombo, bd.outputFileFormat, true);
    ui.outputStackedWidget->setCurrentWidget(ui.outputFilePage);
    ui.outputFilePage->setDisabled(true);
  }

  setComboToDevice(ui.outputDeviceNameCombo, bd.outputDeviceName);
  setComboToCharSet(ui.outputCharSetCombo, bd.outputCharSet);

  ui.xlateWayPtsCk->setChecked(bd.xlateWayPts);
  ui.xlateTracksCk->setChecked(bd.xlateTracks);
  ui.xlateRoutesCk->setChecked(bd.xlateRoutes);

  crossCheckInOutFormats();
  displayOptionsText(ui.inputOptionsText,  ui.inputFormatCombo, true);
  displayOptionsText(ui.outputOptionsText,  ui.outputFormatCombo, false);
  checkCharSetCombos();
  updateFilterStatus();
}

//------------------------------------------------------------------------
void MainWindow::getWidgetValues()
{
  int comboIdx = ui.inputFormatCombo->currentIndex();
  int fidx = ui.inputFormatCombo->itemData(comboIdx).toInt();
  if (ui.inputFileOptBtn->isChecked()){
    bd.inputType = BabelData::fileType;
    bd.inputFileFormat =formatList[fidx].getName();
  }
  else {
    bd.inputType = BabelData::deviceType;
    bd.inputDeviceFormat =formatList[fidx].getName();
  }
  bd.inputDeviceName = ui.inputDeviceNameCombo->currentText();
  bd.inputCharSet = charSetFromCombo(ui.inputCharSetCombo);

  comboIdx = ui.outputFormatCombo->currentIndex();
  fidx = ui.outputFormatCombo->itemData(comboIdx).toInt();
  if (ui.outputFileOptBtn->isChecked()){
    bd.outputType = BabelData::fileType;
    bd.outputFileFormat =formatList[fidx].getName();
  }
  else if (ui.outputDeviceOptBtn->isChecked()){
    bd.outputType = BabelData::deviceType;
    bd.outputDeviceFormat =formatList[fidx].getName();
  }
  else {
    bd.outputType = BabelData::noType;
  }
  bd.outputDeviceName = ui.outputDeviceNameCombo->currentText();
  bd.outputCharSet = charSetFromCombo(ui.outputCharSetCombo);

  bd.xlateWayPts = ui.xlateWayPtsCk->isChecked();
  bd.xlateTracks = ui.xlateTracksCk->isChecked();
  bd.xlateRoutes = ui.xlateRoutesCk->isChecked();
}

// This could be made faster, but any attempt to do so would have to be
// careful about disabled formats.  As it was written to be handled by a
// drag response, performance is hardly critical.
// It's also kind of dumb to return the name which SetCombo then looks up,
// but there's not a 1:1 correlation between offsets in the combo box and
// in the list of formats.
QString MainWindow::getFormatNameForExtension(QString ext)
{
  for (int i = 0; i < formatList.size(); i++) {
    QStringList extensions = formatList[i].getExtensions();
    for (int j = 0; j < extensions.size(); ++j) {
      if (extensions[j] == ext) {
        return formatList[i].getName();;
      }
    }
  }
  return 0;
}
