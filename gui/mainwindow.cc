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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
#include "mainwindow.h"
#include <QAbstractButton>     // for QAbstractButton
#include <QAction>             // for QAction
#include <QApplication>        // for QApplication, qApp
#include <QByteArray>          // for QByteArray
#include <QCheckBox>           // for QCheckBox
#include <QCursor>             // for QCursor
#include <QDate>               // for QDate
#include <QDateTime>           // for QDateTime
#include <QDesktopServices>    // for QDesktopServices
#include <QDialogButtonBox>    // for QDialogButtonBox
#include <QDir>                // for QDir
#include <QEvent>              // for QEvent
#include <QFile>               // for QFile
#include <QFileDialog>         // for QFileDialog
#include <QFileInfo>           // for QFileInfo
#include <QGradientStop>       // for QBrush
#include <QImage>              // for QImage
#include <QLibraryInfo>        // for QLibraryInfo
#include <QLocale>             // for QLocale
#include <QMessageBox>         // for QMessageBox, operator|
#include <QMimeData>           // for QMimeData
#include <QPlainTextEdit>      // for QPlainTextEdit
#include <QProcess>            // for QProcess
#include <QPushButton>         // for QPushButton
#include <QRadioButton>        // for QRadioButton
#include <QSettings>           // for QSettings
#include <QStackedWidget>      // for QStackedWidget
#include <QString>             // for QString, operator+, operator==, operator!=
#include <QStringList>         // for QStringList
#include <QTemporaryFile>      // for QTemporaryFile
#include <QTextCharFormat>     // for QTextCharFormat
#include <QTime>               // for QTime, operator==
#include <QUrl>                // for QUrl
#include <QVariant>            // for QVariant, operator!=
#include <Qt>                  // for TransformationMode, DateFormat, CursorShape, GlobalColor
#include <QtGlobal>            // for QForeachContainer, qMakeForeachContainer, foreach
#include <cstdlib>             // for exit
#include "aboutdlg.h"          // for AboutDlg
#include "advdlg.h"            // for AdvDlg
#include "appname.h"           // for appName
#include "babeldata.h"         // for BabelData
#include "donate.h"            // for Donate
#include "filterdlg.h"         // for FilterDialog
#include "formatload.h"        // for FormatLoad
#include "gbversion.h"         // for VERSION, kVersionDate, kVersionSHA
#ifndef DISABLE_MAPPREVIEW
#include "gpx.h"               // for Gpx
#include "gmapdlg.h"           // for GMapDialog
#endif
#include "help.h"              // for ShowHelp
#include "optionsdlg.h"        // for OptionsDlg
#include "preferences.h"       // for Preferences
#include "runmachine.h"        // for RunMachine
#include "upgrade.h"           // for UpgradeCheck
#include "version_mismatch.h"  // for VersionMismatch



const int BabelData::noType_ = -1;
const int BabelData::fileType_ = 0;
const int BabelData::deviceType_ = 1;

//------------------------------------------------------------------------
QString MainWindow::findBabelVersion()
{
  QProcess babel;
  babel.start(QApplication::applicationDirPath() + "/gpsbabel", QStringList() << "-V");
  if (!babel.waitForStarted()) {
    return QString();
  }
  babel.closeWriteChannel();
  if (!babel.waitForFinished()) {
    return QString();
  }

  QString str = babel.readAll();
  isBeta_ = str.contains("-beta");
  str.replace("Version",  "");
  str.replace("GPSBabel",  "");
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
  return isBeta_ || babelData_.allowBetaUpgrades_;
}

//------------------------------------------------------------------------
static QString MakeOptions(const QList<FormatOption>& options)
{
  QString str;
  for (const auto& option : options) {
    QVariant default_value = option.getDefaultValue();
    if (option.getSelected()) {
      // For OPTbool, 'selected' is the key, not value.
      if (option.getType() == FormatOption::OPTbool) {
        // Only write "foo=1" if that's not already the default.
        if (default_value != "1") {
          str += "," + option.getName() + "=1";
        }
      } else {
        str += "," + option.getName() + "=" + option.getValue().toString();
      }
    } else {
      // For every boolean option not selected, explicitly
      // turn it off here, but only if the default isn't zero
      // or given.
      if (option.getType() == FormatOption::OPTbool &&
          default_value != "0" &&
          default_value != "") {
        str += "," + option.getName() + "=0";
      }
    }
  }
  return str;
}

//------------------------------------------------------------------------
static QString MakeOptionsNoLeadingComma(const QList<FormatOption>& options)
{
  QString str = MakeOptions(options);
  return (str.length()) != 0 ? str.mid(1) : str;

}

//------------------------------------------------------------------------
MainWindow::MainWindow(QWidget* parent): QMainWindow(parent)
{
  ui_.setupUi(this);
  setWindowTitle(appName);
  babelVersion_ = findBabelVersion();
  fmtChgInterlock_ = false;
  loadDeviceNameCombos();

  connect(ui_.inputFileOptBtn,        &QAbstractButton::clicked, this, &MainWindow::inputFileOptBtnClicked);
  connect(ui_.inputDeviceOptBtn,      &QAbstractButton::clicked, this, &MainWindow::inputDeviceOptBtnClicked);
  connect(ui_.inputFileNameBrowseBtn, &QAbstractButton::clicked, this, &MainWindow::browseInputFile);

  ui_.outputFileOptBtn->setAutoExclusive(false);
  ui_.outputDeviceOptBtn->setAutoExclusive(false);
  connect(ui_.outputFileOptBtn,        &QAbstractButton::clicked, this, &MainWindow::outputFileOptBtnClicked);
  connect(ui_.outputDeviceOptBtn,      &QAbstractButton::clicked, this, &MainWindow::outputDeviceOptBtnClicked);
  connect(ui_.outputFileNameBrowseBtn, &QAbstractButton::clicked, this, &MainWindow::browseOutputFile);

  connect(ui_.actionQuit, &QAction::triggered, this, &MainWindow::closeActionX);
  connect(ui_.actionHelp, &QAction::triggered, this, &MainWindow::helpActionX);
  connect(ui_.actionAbout, &QAction::triggered, this, &MainWindow::aboutActionX);
  connect(ui_.actionVisit_Website, &QAction::triggered, this, &MainWindow::visitWebsiteActionX);
  connect(ui_.actionMake_a_Donation, &QAction::triggered, this, &MainWindow::donateActionX);
  connect(ui_.actionUpgradeCheck, &QAction::triggered, this, &MainWindow::upgradeCheckActionX);
  connect(ui_.actionPreferences, &QAction::triggered, this, &MainWindow::preferencesActionX);

  connect(ui_.inputFormatCombo, &QComboBox::currentIndexChanged,
          this,                 &MainWindow::inputFormatChanged);
  connect(ui_.outputFormatCombo, &QComboBox::currentIndexChanged,
          this,                 &MainWindow::outputFormatChanged);
  connect(ui_.inputOptionsBtn,   &QAbstractButton::clicked,
          this,                 &MainWindow::inputOptionButtonClicked);
  connect(ui_.outputOptionsBtn, &QAbstractButton::clicked,
          this,                 &MainWindow::outputOptionButtonClicked);
  connect(ui_.moreOptionButton, &QAbstractButton::clicked,
          this,                 &MainWindow::moreOptionButtonClicked);

  connect(ui_.buttonBox, &QDialogButtonBox::accepted, this, &MainWindow::applyActionX);
  connect(ui_.buttonBox, &QDialogButtonBox::rejected, this, &MainWindow::closeActionX);
  connect(ui_.buttonBox, &QDialogButtonBox::helpRequested, this, &MainWindow::helpActionX);

  connect(ui_.xlateFiltersBtn, &QAbstractButton::clicked, this, &MainWindow::filtersClicked);

  connect(ui_.inputFileNameText, &QLineEdit::textEdited, this, &MainWindow::inputFileNameEdited);
  connect(ui_.outputFileNameText, &QLineEdit::textEdited, this, &MainWindow::outputFileNameEdited);

#if defined (Q_OS_WIN)
  // Windows users like the colored buttons.  They look out of place elsewhere.
  ui_.buttonBox->button(QDialogButtonBox::Ok)->setIcon(QIcon(":/images/runit.png"));
  ui_.buttonBox->button(QDialogButtonBox::Close)->setIcon(QIcon(":/images/exit.png"));
#endif

  ui_.inputOptionsText->setReadOnly(true);
  ui_.outputOptionsText->setReadOnly(true);
#if 0
  // 02/28/10  - let's try letting people edit these outside the browse.
  ui.inputFileNameText->setReadOnly(true);
  ui.outputFileNameText->setReadOnly(true);
#else
  setAcceptDrops(true);
#endif
  lights_[0] = QPixmap::fromImage(QImage(":/images/00.png").scaledToHeight(20, Qt::SmoothTransformation));
  lights_[1] = QPixmap::fromImage(QImage(":/images/01.png").scaledToHeight(20, Qt::SmoothTransformation));
  lights_[2] = QPixmap::fromImage(QImage(":/images/10.png").scaledToHeight(20, Qt::SmoothTransformation));
  lights_[3] = QPixmap::fromImage(QImage(":/images/11.png").scaledToHeight(20, Qt::SmoothTransformation));

  ui_.outputWindow->setReadOnly(true);

  // Start up in the current system language.
  loadLanguage(QLocale::system().name());
  loadFormats();

  //--- Restore from registry
  restoreSettings();

  upgrade = new UpgradeCheck(this, formatList_, babelData_);
  if (babelData_.startupVersionCheck_) {
    upgrade->checkForUpgrade(babelVersion_, babelData_.upgradeCheckTime_,
                             allowBetaUpgrades());
  }

  if (!babelData_.ignoreVersionMismatch_ && babelVersion_ != VERSION) {
    VersionMismatch vm(nullptr, babelVersion_, QString(VERSION));

    vm.exec();
    babelData_.ignoreVersionMismatch_ = vm.neverAgain();
  }
}

void MainWindow::switchTranslator(QTranslator& translator, const QString& filename)
{
  // remove the old translator
  qApp->removeTranslator(&translator);

  // Set a list of locations to search for the translation file.
  // 1. In the file system in the translations directory relative to the
  //    location of the executable.
  // 2. In the Qt resource system under the translations path.  This is useful
  //    if the resource was compiled into the executable.
  // 3. In the translations path for Qt.  This is useful to find translations
  //    included with Qt.
  const QStringList directories = {
    QApplication::applicationDirPath() + "/translations",
    ":/translations",
    QLibraryInfo::path(QLibraryInfo::TranslationsPath)
  };

  // Load the new translator.
  for (const auto& directory : directories) {
    if (translator.load(filename, directory)) {
      qApp->installTranslator(&translator);
      break;
    }
  }
}

void MainWindow::loadLanguage(const QString& rLanguage)
{
  if (currLang_ != rLanguage) {
    currLang_ = rLanguage;
    QLocale locale = QLocale(currLang_);
    QLocale::setDefault(locale);

    switchTranslator(translator_, QString("gpsbabelfe_%1.qm").arg(rLanguage));
    switchTranslator(translatorCore_, QString("gpsbabel_%1.qm").arg(rLanguage));
    switchTranslator(translatorQt_, QString("qt_%1.qm").arg(rLanguage));
  }
}

void MainWindow::changeEvent(QEvent* event)
{
  if (nullptr != event) {
    switch (event->type()) {
    // This event is sent if a translator is loaded.
    case QEvent::LanguageChange:
      ui_.retranslateUi(this);
      break;
    // This event is sent if the system language changes.
    case QEvent::LocaleChange: {
      QString locale = QLocale::system().name();
      locale.truncate(locale.lastIndexOf('_'));
      loadLanguage(locale);
    }
    break;
    default:
      break;
    }
  }

  QMainWindow::changeEvent(event);
}

//------------------------------------------------------------------------
void MainWindow::loadInputDeviceNameCombo(const QString& format)
{
  ui_.inputDeviceNameCombo->clear();
  // Later, we can probe the system for multiple USB devices and populate
  // here.
  if (formatSupportsUSB(format)) {
    ui_.inputDeviceNameCombo->addItem("usb:");
  }
  if (formatSupportsSerial(format)) {
    osLoadDeviceNameCombos(ui_.inputDeviceNameCombo);
  }
  // If only one choice, just disable it.
  ui_.inputDeviceNameCombo->setEnabled(ui_.inputDeviceNameCombo->count() > 1);
}

//------------------------------------------------------------------------
void MainWindow::loadOutputDeviceNameCombo(const QString& format)
{
  ui_.outputDeviceNameCombo->clear();
  // Later, we can probe the system for multiple USB devices and populate
  // here.
  if (formatSupportsUSB(format)) {
    ui_.outputDeviceNameCombo->addItem("usb:");
  }
  if (formatSupportsSerial(format)) {
    osLoadDeviceNameCombos(ui_.outputDeviceNameCombo);
  }
  // If only one choice, just disable it.
  ui_.outputDeviceNameCombo->setEnabled(ui_.outputDeviceNameCombo->count() > 1);
}

//------------------------------------------------------------------------
void MainWindow::loadDeviceNameCombos()
{
  loadInputDeviceNameCombo("");
  loadOutputDeviceNameCombo("");
}
//------------------------------------------------------------------------
void MainWindow::inputFileOptBtnClicked()
{
  fmtChgInterlock_ = true;
  QString fmt = babelData_.inputFileFormat_;
  ui_.inputStackedWidget->setCurrentWidget(ui_.inputFilePage);
  QList<int>indices = inputFileFormatIndices();
  ui_.inputFormatCombo->clear();
  for (int i=0; i<indices.size(); i++) {
    int k = indices[i];
    if (!formatList_[k].isHidden()) {
      ui_.inputFormatCombo->addItem(formatList_[k].getDescription(), QVariant(k));
    }
  }
  setComboToFormat(ui_.inputFormatCombo, fmt, true);
  fmtChgInterlock_ = false;
}

//------------------------------------------------------------------------
void MainWindow::inputDeviceOptBtnClicked()
{
  fmtChgInterlock_ = true;
  QString fmt = babelData_.inputDeviceFormat_;
  ui_.inputStackedWidget->setCurrentWidget(ui_.inputDevicePage);
  QList<int>indices = inputDeviceFormatIndices();
  ui_.inputFormatCombo->clear();
  for (int i=0; i<indices.size(); i++) {
    int k = indices[i];
    if (!formatList_[k].isHidden()) {
      ui_.inputFormatCombo->addItem(formatList_[k].getDescription(), QVariant(k));
    }
  }
  setComboToFormat(ui_.inputFormatCombo, fmt, false);
  fmtChgInterlock_ = false;
}

//------------------------------------------------------------------------
void MainWindow:: outputFileOptBtnClicked()
{
  fmtChgInterlock_ = true;
  if (ui_.outputFileOptBtn->isChecked()) {
    ui_.outputFilePage->setEnabled(true);
    ui_.outputDeviceOptBtn->setChecked(false);
    QString fmt = babelData_.outputFileFormat_;
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputFilePage);
    QList<int>indices = outputFileFormatIndices();
    ui_.outputFormatCombo->clear();
    for (int i=0; i<indices.size(); i++) {
      int k = indices[i];
      if (!formatList_[k].isHidden()) {
        ui_.outputFormatCombo->addItem(formatList_[k].getDescription(), QVariant(k));
      }
    }
    setComboToFormat(ui_.outputFormatCombo, fmt, true);
  } else {
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputFilePage);
    ui_.outputFilePage->setEnabled(false);
  }
  fmtChgInterlock_ = false;
}

//------------------------------------------------------------------------
void MainWindow:: outputDeviceOptBtnClicked()
{
  fmtChgInterlock_ = true;
  if (ui_.outputDeviceOptBtn->isChecked()) {
    ui_.outputDevicePage->setEnabled(true);
    ui_.outputFileOptBtn->setChecked(false);
    QString fmt = babelData_.outputDeviceFormat_;
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputDevicePage);
    QList<int>indices = outputDeviceFormatIndices();
    ui_.outputFormatCombo->clear();
    for (int i=0; i<indices.size(); i++) {
      int k = indices[i];
      if (!formatList_[k].isHidden()) {
        ui_.outputFormatCombo->addItem(formatList_[k].getDescription(), QVariant(k));
      }
    }
    setComboToFormat(ui_.outputFormatCombo, fmt, false);
  } else {
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputDevicePage);
    ui_.outputDevicePage->setEnabled(false);
  }
  fmtChgInterlock_ = false;
}
void MainWindow::inputFileNameEdited()
{
  babelData_.inputFileNames_.clear();
  babelData_.inputFileNames_ << ui_.inputFileNameText->text();
}

void MainWindow::outputFileNameEdited()
{
  babelData_.outputFileName_ = ui_.outputFileNameText->text();

}

//------------------------------------------------------------------------
QString MainWindow::filterForFormat(int idx)
{
  QString str = formatList_[idx].getDescription();
  str.replace('(', '[');
  str.replace(')', ']');
  QStringList extensions = formatList_[idx].getExtensions();

  // If we don't have any meaningful extensions available for this format,
  // don't be clever here; just fall through to "All files" case.
  if (!extensions.empty() && !extensions[0].isEmpty()) {
    str += " (";
    for (int i=0; i<extensions.size(); i++) {
      if (i!= 0) {
        str += " ";
      }
      str += "*." + extensions[i];
    }
    str += ");;";
  }
  str += "All Files (*.*)";
  return str;
}
//------------------------------------------------------------------------
QString MainWindow::ensureExtensionPresent(const QString& name, int idx)
{
  QString outname = name;
  if (QFileInfo(name).suffix().length() == 0) {
    QStringList extensions = formatList_[idx].getExtensions();
    if (!extensions.empty() && !extensions[0].isEmpty()) {
      outname += "." + extensions[0];
    }
  }
  return outname;
}

//------------------------------------------------------------------------
bool MainWindow::filterForFormatIncludes(int idx, const QString& fmt)
{
  QStringList extensions = formatList_[idx].getExtensions();
  for (int i=0; i<extensions.size(); i++) {
    if (fmt == extensions[i]) {
      return true;
    }
  }
  return false;
}

//------------------------------------------------------------------------
int MainWindow::currentComboFormatIndex(QComboBox* comboBox)
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
  QString startFile = !babelData_.inputFileNames_.empty() ? babelData_.inputFileNames_[0] : babelData_.inputBrowse_;
  int idx = currentComboFormatIndex(ui_.inputFormatCombo);
  QFileInfo finfo(startFile);
  if (!finfo.isDir() && (!filterForFormatIncludes(idx, finfo.suffix()))) {
    startFile = finfo.dir().absolutePath();
  }

  QStringList userList =
    QFileDialog::getOpenFileNames(nullptr, tr("Select one or more input files"),
                                  startFile,
                                  filterForFormat(idx));
  if (!userList.empty()) {
    babelData_.inputBrowse_ = userList[0];
    babelData_.inputFileNames_ = userList;
    QString str;
    for (int i=0; i<babelData_.inputFileNames_.size(); i++) {
      if (i != 0) {
        str += ", ";
      }
      str += "\"" + babelData_.inputFileNames_[i] + "\"";
    }
    ui_.inputFileNameText->setText(str);
  }
}

//------------------------------------------------------------------------
void MainWindow::browseOutputFile()
{
  int idx = currentComboFormatIndex(ui_.outputFormatCombo);
  QString startFile = babelData_.outputFileName_.length() == 0 ? babelData_.outputBrowse_ : babelData_.outputFileName_;
  QFileInfo finfo(startFile);
  if (!finfo.isDir() && (!filterForFormatIncludes(idx, finfo.suffix()))) {
    startFile = finfo.dir().absolutePath();
  }

  QString str =
    QFileDialog::getSaveFileName(nullptr, tr("Output File Name"),
                                 startFile,
                                 filterForFormat(idx));
  if (str.length() != 0) {
    str = ensureExtensionPresent(str, idx);
    babelData_.outputBrowse_ = str;
    babelData_.outputFileName_ = str;
    ui_.outputFileNameText->setText(str);
  }
}

//------------------------------------------------------------------------
QList<int> MainWindow::inputFileFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList_.size(); i++) {
    if (formatList_[i].isReadSomething() && formatList_[i].isFileFormat()) {
      indices<<i;
    }
  }
  return indices;
}

//------------------------------------------------------------------------
QList<int> MainWindow::inputDeviceFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList_.size(); i++) {
    if (formatList_[i].isReadSomething() && formatList_[i].isDeviceFormat()) {
      indices<<i;
    }
  }
  return indices;
}

//------------------------------------------------------------------------
QList<int> MainWindow::outputFileFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList_.size(); i++) {
    if (formatList_[i].isWriteSomething() && formatList_[i].isFileFormat()) {
      indices<<i;
    }
  }
  return indices;
}

//------------------------------------------------------------------------
QList<int> MainWindow::outputDeviceFormatIndices()
{
  QList<int>indices;
  for (int i=0; i<formatList_.size(); i++) {
    if (formatList_[i].isWriteSomething() && formatList_[i].isDeviceFormat()) {
      indices<<i;
    }
  }
  return indices;
}

//------------------------------------------------------------------------
void MainWindow::loadFormats()
{
  if (!FormatLoad().getFormats(formatList_)) {
    QMessageBox::information(nullptr, QString(appName),
                             tr("Error reading format configuration.  "
                                "Check that the backend program \"gpsbabel\" is properly installed "
                                "and is in the current PATH\n\n"
                                "This program cannot continue."));
    exit(1);
  }
  if (inputFileFormatIndices().empty() ||
      inputDeviceFormatIndices().empty() ||
      outputFileFormatIndices().empty() ||
      outputDeviceFormatIndices().empty()) {
    QMessageBox::information(nullptr, QString(appName),
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
void MainWindow::setIndicatorLights(QLabel* label, const QString& type, int code)
{
  label->setPixmap(lights_[code]);
  QString s;
  switch (code) {
  default:
  case 0:
    s = tr("Input and output formats do not support %1").arg(type);
    break;
  case 1:
    s = tr("Input does not support %1; output format supports %1").arg(type);
    break;
  case 2:
    s = tr("Input format supports %1; output format does not support %1").arg(type);
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
  if (ui_.inputFormatCombo->count() == 0 ||
      ui_.outputFormatCombo->count() == 0) {
    // During format/device switch this is true
    return;
  }
  Format ifmt = formatList_[currentComboFormatIndex(ui_.inputFormatCombo)];
  Format ofmt = formatList_[currentComboFormatIndex(ui_.outputFormatCombo)];

  ui_.xlateWayPtsCk->setEnabled(ifmt.isReadWaypoints() && ofmt.isWriteWaypoints());
  ui_.xlateTracksCk->setEnabled(ifmt.isReadTracks()    && ofmt.isWriteTracks());
  ui_.xlateRoutesCk->setEnabled(ifmt.isReadRoutes()    && ofmt.isWriteRoutes());

  setIndicatorLights(ui_.wayPtLabel, tr("waypoints"), iconIndex(ifmt.isReadWaypoints(), ofmt.isWriteWaypoints()));
  setIndicatorLights(ui_.trackLabel, tr("tracks"), iconIndex(ifmt.isReadTracks(), ofmt.isWriteTracks()));
  setIndicatorLights(ui_.routeLabel, tr("routes"), iconIndex(ifmt.isReadRoutes(), ofmt.isWriteRoutes()));
}

//------------------------------------------------------------------------
void MainWindow::displayOptionsText(QLineEdit* le, QComboBox* combo, bool isInput)
{
  int fidx = combo->itemData(combo->currentIndex()).toInt();
  if (isInput) {
    le->setText(MakeOptionsNoLeadingComma(formatList_[fidx].getInputOptions()));
  } else {
    le->setText(MakeOptionsNoLeadingComma(formatList_[fidx].getOutputOptions()));
  }

}

//------------------------------------------------------------------------
void MainWindow::setComboToFormat(QComboBox* comboBox, const QString& name, bool isFile)
{
  int fidx = -1;
  for (int i=0; i<formatList_.size(); i++) {
    if (formatList_[i].getName() == name &&
        formatList_[i].isFileFormat() == isFile) {
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
bool MainWindow::formatSupportsUSB(const QString& format)
{
  return (format == "garmin" || format == "delbin");
}

//------------------------------------------------------------------------
bool MainWindow::formatSupportsSerial(const QString& format)
{
  return (format != "delbin");
}

//------------------------------------------------------------------------
void MainWindow::inputFormatChanged(int comboIdx)
{
  if (fmtChgInterlock_) {
    return;
  }
  int fidx = ui_.inputFormatCombo->itemData(comboIdx).toInt();
  ui_.inputOptionsBtn->setEnabled(!formatList_[fidx].getInputOptions().empty());
  displayOptionsText(ui_.inputOptionsText,  ui_.inputFormatCombo, true);
  crossCheckInOutFormats();

  if (ui_.inputFileOptBtn->isChecked()) {
    babelData_.inputFileFormat_ =formatList_[fidx].getName();
  } else {
    babelData_.inputDeviceFormat_ = formatList_[fidx].getName();
  }

  loadInputDeviceNameCombo(formatList_[fidx].getName());
}

//------------------------------------------------------------------------
void MainWindow::outputFormatChanged(int comboIdx)
{
  if (fmtChgInterlock_) {
    return;
  }
  int fidx = ui_.outputFormatCombo->itemData(comboIdx).toInt();
  ui_.outputOptionsBtn->setEnabled(!formatList_[fidx].getOutputOptions().empty());
  displayOptionsText(ui_.outputOptionsText,  ui_.outputFormatCombo, false);
  crossCheckInOutFormats();

  if (ui_.outputFileOptBtn->isChecked()) {
    babelData_.outputFileFormat_ =formatList_[fidx].getName();
  } else if (ui_.outputDeviceOptBtn->isChecked()) {
    babelData_.outputDeviceFormat_ = formatList_[fidx].getName();
  }

  loadOutputDeviceNameCombo(formatList_[fidx].getName());
}

//------------------------------------------------------------------------
void MainWindow::inputOptionButtonClicked()
{
  int fidx = currentComboFormatIndex(ui_.inputFormatCombo);
  if (formatList_[fidx].getInputOptionsRef()->empty()) {
    QMessageBox::information
    (nullptr, appName,
     tr("There are no input options for format \"%1\"").arg(formatList_[fidx].getDescription()));
  } else {
    OptionsDlg optionDlg(nullptr,
                         formatList_[fidx].getName(),
                         formatList_[fidx].getInputOptionsRef(),
                         formatList_[fidx].getHtml());
    optionDlg.setWindowTitle(QString(appName) + " - " + tr("Options for %1").arg(formatList_[fidx].getName()));
    optionDlg.exec();
    displayOptionsText(ui_.inputOptionsText,  ui_.inputFormatCombo, true);
  }
}

//------------------------------------------------------------------------
void MainWindow::outputOptionButtonClicked()
{
  int fidx = currentComboFormatIndex(ui_.outputFormatCombo);
  if (formatList_[fidx].getOutputOptionsRef()->empty()) {
    QMessageBox::information
    (nullptr, appName,
     tr("There are no output options for format \"%1\"").arg(formatList_[fidx].getDescription()));
  } else {
    OptionsDlg optionDlg(nullptr,
                         formatList_[fidx].getName(),
                         formatList_[fidx].getOutputOptionsRef(),
                         formatList_[fidx].getHtml());
    optionDlg.setWindowTitle(QString(appName) + " - " + tr("Options for %1").arg(formatList_[fidx].getName()));
    optionDlg.exec();
    displayOptionsText(ui_.outputOptionsText,  ui_.outputFormatCombo, false);
  }
}



//------------------------------------------------------------------------
bool MainWindow::isOkToGo()
{
  if (!((ui_.xlateWayPtsCk->isChecked() && ui_.xlateWayPtsCk->isEnabled()) ||
        (ui_.xlateRoutesCk->isChecked() && ui_.xlateRoutesCk->isEnabled()) ||
        (ui_.xlateTracksCk->isChecked() && ui_.xlateTracksCk->isEnabled()))) {
    QMessageBox::information(nullptr, QString(appName), tr("No valid waypoints/routes/tracks translation specified"));
    return false;
  }

  // Paper over what didn't happen in inputBrowse() if the user edited
  // the filename fields directly.
  if ((babelData_.inputType_ == BabelData::fileType_) &&
      (babelData_.inputFileNames_.empty()) &&
      (!ui_.inputFileNameText->text().isEmpty())) {
    babelData_.inputFileNames_ << ui_.inputFileNameText->text();
  }
  if ((babelData_.outputType_ == BabelData::fileType_) &&
      (babelData_.outputFileName_.size() == 0) &&
      (!ui_.outputFileNameText->text().isEmpty())) {
    babelData_.outputFileName_ = ui_.outputFileNameText->text();
  }

  if ((babelData_.inputType_ == BabelData::fileType_) &&
      (babelData_.inputFileNames_.empty())) {
    QMessageBox::information(nullptr, QString(appName), tr("No input file specified"));
    return false;
  }

#ifndef DISABLE_MAPPREVIEW
  if (babelData_.outputType_ == BabelData::noType_ && !babelData_.previewGmap_) {
#else
  if (babelData_.outputType_ == BabelData::noType_) {
#endif
    QMessageBox::information(nullptr, QString(appName), tr("No valid output specified"));
    return false;
  }
  if (babelData_.outputType_ == BabelData::fileType_ &&
      babelData_.outputFileName_.length() == 0) {
    QMessageBox::information(nullptr, QString(appName), tr("No output file specified"));
    return false;
  }
  return true;
}

//------------------------------------------------------------------------
bool MainWindow::runGpsbabel(const QStringList& args, QString& errorString,
                             QString& outputString)
{
  QString name = "gpsbabel";
  QString program = QApplication::applicationDirPath() + '/' + name;
  RunMachine runMachine(this, program, args);
  int retStatus = runMachine.exec();

  errorString = runMachine.getErrorString();
  outputString = runMachine.getOutputString();
  return retStatus;
}

//------------------------------------------------------------------------
int MainWindow::formatIndexFromName(bool isFile, const QString& nm)
{
  for (int i= 0; i<formatList_.size(); i++) {
    if (nm == formatList_[i].getName() && formatList_[i].isFileFormat() == isFile) {
      return i;
    }
  }
  return 0;
}

//------------------------------------------------------------------------
void MainWindow::applyActionX()
{
  getWidgetValues();
  if (!isOkToGo()) {
    return;
  }

  QStringList args;

  if (babelData_.debugLevel_ >=0) {
    args << QString("-D%1").arg(babelData_.debugLevel_);
  }
  if (babelData_.synthShortNames_) {
    args << "-s";
  }

  Format ifmt = formatList_[currentComboFormatIndex(ui_.inputFormatCombo)];
  Format ofmt = formatList_[currentComboFormatIndex(ui_.outputFormatCombo)];

  if (babelData_.xlateWayPts_ && ifmt.isReadWaypoints() && ofmt.isWriteWaypoints()) {
    args << "-w";
  }
  if (babelData_.xlateRoutes_ && ifmt.isReadRoutes()    && ofmt.isWriteRoutes()) {
    args << "-r";
  }
  if (babelData_.xlateTracks_ && ifmt.isReadTracks()    && ofmt.isWriteTracks()) {
    args << "-t";
  }

  // Input type, with options
  bool iisFile = (babelData_.inputType_ == BabelData::fileType_);
  int fidx = formatIndexFromName(iisFile, iisFile ?
                                 babelData_.inputFileFormat_ : babelData_.inputDeviceFormat_);
  args << "-i";
  args << (formatList_[fidx].getName() + MakeOptions(formatList_[fidx].getInputOptions()));

  // Input file(s) or device
  int read_use_count = 0;
  if (babelData_.inputType_ == BabelData::fileType_) {
    for (int i=0; i<babelData_.inputFileNames_.size(); i++) {
      args << "-f" << babelData_.inputFileNames_[i];
      read_use_count++;
    }
  } else {
    args << "-f" << babelData_.inputDeviceName_;
    read_use_count++;
  }
  formatList_[fidx].bumpReadUseCount(read_use_count);

  // --- Filters!
  args << filterData_.getAllFilterStrings();

  // Output type, with options
  if (babelData_.outputType_ != BabelData::noType_) {
    bool outIsFile = (babelData_.outputType_ == BabelData::fileType_);
    fidx = formatIndexFromName(outIsFile, (outIsFile ?
                                           babelData_.outputFileFormat_ : babelData_.outputDeviceFormat_));
    args << "-o";
    args << (formatList_[fidx].getName() + MakeOptions(formatList_[fidx].getOutputOptions()));

    // output file or device option
    if (outIsFile) {
      if (babelData_.outputFileName_ != "") {
        args << "-F" << babelData_.outputFileName_;
      }
    } else if (babelData_.outputType_ == BabelData::deviceType_) {
      args << "-F" << babelData_.outputDeviceName_;
    }
    // GUI only ever writes a single file at a time.
    formatList_[fidx].bumpWriteUseCount(1);
  }

#ifndef DISABLE_MAPPREVIEW
  // Now output for preview in google maps
  QString tempName;
  if (babelData_.previewGmap_) {
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
#endif

  ui_.outputWindow->clear();
  ui_.outputWindow->appendPlainText("gpsbabel " + args.join(" "));

  QString errorString;
  QString outputString;
  QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
  bool x = runGpsbabel(args, errorString, outputString);
  QApplication::restoreOverrideCursor();

  ui_.outputWindow->appendPlainText(outputString);
  if (x) {
    ui_.outputWindow->appendPlainText(tr("Translation successful"));
#ifndef DISABLE_MAPPREVIEW
    if (babelData_.previewGmap_) {
      Gpx mapData;
      QString mapStatus = mapData.read(tempName);
      QFile(tempName).remove();
      if (!mapStatus.isNull()) {
        QTextCharFormat defaultFormat = ui_.outputWindow->currentCharFormat();
        QTextCharFormat errorFormat = defaultFormat;
        errorFormat.setForeground(Qt::red);
        errorFormat.setFontItalic(true);

        ui_.outputWindow->setCurrentCharFormat(errorFormat);
        ui_.outputWindow->appendPlainText(tr("Error preparing map: %1\n").arg(mapStatus));
        ui_.outputWindow->setCurrentCharFormat(defaultFormat);
      } else {
        this->hide();
        GMapDialog dlg(nullptr, mapData, babelData_.debugLevel_ >=1 ? ui_.outputWindow : nullptr);
        dlg.show();
        dlg.exec();
        this->show();
      }
    }
#endif
  } else {
    QTextCharFormat defaultFormat = ui_.outputWindow->currentCharFormat();
    QTextCharFormat errorFormat = defaultFormat;
    errorFormat.setForeground(Qt::red);
    errorFormat.setFontItalic(true);

    ui_.outputWindow->setCurrentCharFormat(errorFormat);
    ui_.outputWindow->appendPlainText(tr("Error running gpsbabel: %1\n").arg(errorString));
    ui_.outputWindow->setCurrentCharFormat(defaultFormat);
  }
}

//------------------------------------------------------------------------
void MainWindow::closeActionX()
{
  QDateTime wt= upgrade->getUpgradeWarningTime();
  if (wt.isValid()) {
    babelData_.upgradeCheckTime_ = wt;
  }
  babelData_.runCount_++;

  QDateTime now = QDateTime::currentDateTime();
  if (!babelData_.disableDonateDialog_ &&
      ((babelData_.runCount_ == 1) ||
       ((babelData_.runCount_ > 5) && (babelData_.donateSplashed_.daysTo(now) > 30)))) {
    Donate donate(nullptr);
    if (babelData_.donateSplashed_.date() == QDate(2010,1,1)) {
      donate.showNever(false);
    }
    donate.exec();
    babelData_.disableDonateDialog_ = donate.neverAgain();
    babelData_.donateSplashed_ = now;
  }
  saveSettings();
  qApp->exit(0);
}

//------------------------------------------------------------------------
void MainWindow::closeEvent(QCloseEvent* /*event*/)
{
  closeActionX();
}

//------------------------------------------------------------------------
void MainWindow::donateActionX()
{
  QDesktopServices::openUrl(QString("https://www.gpsbabel.org/contribute.html?gbversion=" VERSION));
}

//------------------------------------------------------------------------
void MainWindow::visitWebsiteActionX()
{
  QDesktopServices::openUrl(QString("https://www.gpsbabel.org"));
}

//------------------------------------------------------------------------
void MainWindow::dragEnterEvent(QDragEnterEvent* event)
{
  event->acceptProposedAction();
}

void MainWindow::dropEvent(QDropEvent* event)
{
  foreach (QString format, event->mimeData()->formats()) {
    if (format == "text/uri-list") {
      QList<QUrl> urlList = event->mimeData()->urls();
      babelData_.inputFileNames_.clear();
      for (int i = 0; i < urlList.size(); ++i) {
        QFileInfo file_info(urlList.at(i).toLocalFile());
        QString name = file_info.filePath();
        QString ext = file_info.suffix();

        QString fmt = getFormatNameForExtension(ext);
        setComboToFormat(ui_.inputFormatCombo, fmt, true);
        ui_.inputFileNameText->setText(name);
        babelData_.inputFileNames_ << ui_.inputFileNameText->text();
        event->acceptProposedAction();
      }
    }
  }
}
//------------------------------------------------------------------------
void MainWindow::setComboToDevice(QComboBox* comboBox, const QString& name)
{
  for (int i=0; i<comboBox->count(); i++) {
    QString value = comboBox->itemData(i).isValid()?
      comboBox->itemData(i).toString() : comboBox->itemText(i);
    if (value == name) {
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
  babelData_.saveSettings(settings);
  for (int i=0; i<formatList_.size(); i++) {
    formatList_[i].saveSettings(settings);
  }
  for (int i=0; i<filterData_.filters.size(); i++) {
    filterData_.filters[i]->saveSettings(settings);
  }
}

//------------------------------------------------------------------------
void MainWindow::restoreSettings()
{
  QSettings settings;
  babelData_.restoreSettings(settings);
  for (int i=0; i<formatList_.size(); i++) {
    formatList_[i].restoreSettings(settings);
  }

  for (int i=0; i<filterData_.filters.size(); i++) {
    filterData_.filters[i]->restoreSettings(settings);
  }

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
    for (int i=0; i<formatList_.size(); i++) {
      formatList_[i].setToDefault();
    }
    displayOptionsText(ui_.inputOptionsText,  ui_.inputFormatCombo, true);
    displayOptionsText(ui_.outputOptionsText,  ui_.outputFormatCombo, false);
  }
}

//------------------------------------------------------------------------
void MainWindow::moreOptionButtonClicked()
{
  AdvDlg advDlg(nullptr, babelData_.synthShortNames_,
                babelData_.previewGmap_, babelData_.debugLevel_);
  connect(advDlg.formatButton(), &QAbstractButton::clicked,
          this, &MainWindow::resetFormatDefaults);
  advDlg.exec();
}
//------------------------------------------------------------------------
void MainWindow::aboutActionX()
{
  QDateTime date = QDateTime::fromString(kVersionDate, Qt::ISODate);
  QString utcdate;
  if (date.isValid()) {
    utcdate = date.toUTC().toString(Qt::ISODate);
  }
  AboutDlg aboutDlg(nullptr, babelVersion_, QString(appName) + QString(" " VERSION), kVersionSHA, utcdate, babelData_.installationUuid_);
  aboutDlg.setWindowTitle(tr("About %1").arg(appName));
  aboutDlg.exec();
}

//------------------------------------------------------------------------
void MainWindow::upgradeCheckActionX()
{
  upgrade->checkForUpgrade(babelVersion_,
                           QDateTime(QDate(2000, 1, 1), QTime(0, 0)),
                           allowBetaUpgrades());
}

//------------------------------------------------------------------------
void MainWindow::preferencesActionX()
{
  Preferences preferences(nullptr, formatList_, babelData_);
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
  FilterDialog dlg(nullptr, filterData_);
  dlg.runDialog();
  updateFilterStatus();
}


//------------------------------------------------------------------------
void MainWindow::updateFilterStatus()
{
  bool filterActive = !filterData_.getAllFilterStrings().empty();
  ui_.filterStatus->setEnabled(filterActive);
  if (filterActive) {
    ui_.filterStatus->setToolTip(tr("One or more data filters are active"));
  } else {
    ui_.filterStatus->setToolTip(tr("No data filters are active"));
  }
}
//------------------------------------------------------------------------
void MainWindow::setWidgetValues()
{
  if (babelData_.inputType_ == BabelData::fileType_) {
    ui_.inputFileOptBtn->setChecked(true);
    inputFileOptBtnClicked();
    setComboToFormat(ui_.inputFormatCombo, babelData_.inputFileFormat_, true);
    ui_.inputStackedWidget->setCurrentWidget(ui_.inputFilePage);
  } else {
    ui_.inputDeviceOptBtn->setChecked(true);
    inputDeviceOptBtnClicked();
    setComboToFormat(ui_.inputFormatCombo, babelData_.inputDeviceFormat_, false);
    loadInputDeviceNameCombo(babelData_.inputDeviceFormat_);
    ui_.inputStackedWidget->setCurrentWidget(ui_.inputDevicePage);
  }
  setComboToDevice(ui_.inputDeviceNameCombo, babelData_.inputDeviceName_);

  if (babelData_.outputType_ == BabelData::fileType_) {
    ui_.outputFileOptBtn->setChecked(true);
    outputFileOptBtnClicked();
    setComboToFormat(ui_.outputFormatCombo, babelData_.outputFileFormat_, true);
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputFilePage);
  } else if (babelData_.outputType_ == BabelData::deviceType_) {
    ui_.outputDeviceOptBtn->setChecked(true);
    outputDeviceOptBtnClicked();
    setComboToFormat(ui_.outputFormatCombo, babelData_.outputDeviceFormat_, false);
    loadOutputDeviceNameCombo(babelData_.outputDeviceFormat_);
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputDevicePage);
  } else {
    ui_.outputFileOptBtn->setChecked(false);
    ui_.outputDeviceOptBtn->setChecked(false);
    setComboToFormat(ui_.outputFormatCombo, babelData_.outputFileFormat_, true);
    ui_.outputStackedWidget->setCurrentWidget(ui_.outputFilePage);
    ui_.outputFilePage->setDisabled(true);
  }

  setComboToDevice(ui_.outputDeviceNameCombo, babelData_.outputDeviceName_);

  ui_.xlateWayPtsCk->setChecked(babelData_.xlateWayPts_);
  ui_.xlateTracksCk->setChecked(babelData_.xlateTracks_);
  ui_.xlateRoutesCk->setChecked(babelData_.xlateRoutes_);

  crossCheckInOutFormats();
  displayOptionsText(ui_.inputOptionsText,  ui_.inputFormatCombo, true);
  displayOptionsText(ui_.outputOptionsText,  ui_.outputFormatCombo, false);
  updateFilterStatus();
}

//------------------------------------------------------------------------
void MainWindow::getWidgetValues()
{
  int comboIdx = ui_.inputFormatCombo->currentIndex();
  int fidx = ui_.inputFormatCombo->itemData(comboIdx).toInt();
  if (ui_.inputFileOptBtn->isChecked()) {
    babelData_.inputType_ = BabelData::fileType_;
    babelData_.inputFileFormat_ =formatList_[fidx].getName();
  } else {
    babelData_.inputType_ = BabelData::deviceType_;
    babelData_.inputDeviceFormat_ =formatList_[fidx].getName();
  }
  babelData_.inputDeviceName_ = ui_.inputDeviceNameCombo->currentData().isValid()?
    ui_.inputDeviceNameCombo->currentData().toString() : ui_.inputDeviceNameCombo->currentText();

  comboIdx = ui_.outputFormatCombo->currentIndex();
  fidx = ui_.outputFormatCombo->itemData(comboIdx).toInt();
  if (ui_.outputFileOptBtn->isChecked()) {
    babelData_.outputType_ = BabelData::fileType_;
    babelData_.outputFileFormat_ =formatList_[fidx].getName();
  } else if (ui_.outputDeviceOptBtn->isChecked()) {
    babelData_.outputType_ = BabelData::deviceType_;
    babelData_.outputDeviceFormat_ =formatList_[fidx].getName();
  } else {
    babelData_.outputType_ = BabelData::noType_;
  }
  babelData_.outputDeviceName_ = ui_.outputDeviceNameCombo->currentData().isValid()?
    ui_.outputDeviceNameCombo->currentData().toString() : ui_.outputDeviceNameCombo->currentText();

  babelData_.xlateWayPts_ = ui_.xlateWayPtsCk->isChecked();
  babelData_.xlateTracks_ = ui_.xlateTracksCk->isChecked();
  babelData_.xlateRoutes_ = ui_.xlateRoutesCk->isChecked();
}

// This could be made faster, but any attempt to do so would have to be
// careful about disabled formats.  As it was written to be handled by a
// drag response, performance is hardly critical.
// It's also kind of dumb to return the name which SetCombo then looks up,
// but there's not a 1:1 correlation between offsets in the combo box and
// in the list of formats.
QString MainWindow::getFormatNameForExtension(const QString& ext)
{
  for (int i = 0; i < formatList_.size(); i++) {
    QStringList extensions = formatList_[i].getExtensions();
    for (int j = 0; j < extensions.size(); ++j) {
      if (extensions[j] == ext) {
        return formatList_[i].getName();
      }
    }
  }
  return nullptr;
}
