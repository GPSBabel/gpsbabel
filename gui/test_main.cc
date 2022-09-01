#include "test_main.h"

#include <QAbstractButton>      // for QAbstractButton
#include <QApplication>         // for QApplication
#include <QByteArray>           // for QByteArray
#include <QCheckBox>            // for QCheckBox
#include <QComboBox>            // for QComboBox
#include <QDateTime>            // for QDateTime
#include <QDateTimeEdit>        // for QDateTimeEdit
#include <QDebug>               // for QDebug, operator<<
#include <QDialog>              // for QDialog
#include <QDialogButtonBox>     // for QDialogButtonBox, QDialogButtonBox::Ok, QDialogButtonBox::Close
#include <QLineEdit>            // for QLineEdit
#include <QList>                // for QList, QList<>::const_iterator
#include <QListWidget>          // for QListWidget
#include <QListWidgetItem>      // for QListWidgetItem
#include <QMessageBox>          // for QMessageBox, QMessageBox::Yes
#include <QPlainTextEdit>       // for QPlainTextEdit
#include <QPushButton>          // for QPushButton
#include <QRadioButton>         // for QRadioButton
#include <QRect>                // for QRect
#include <QRegularExpression>   // for QRegularExpression
#include <QSettings>            // for QSettings
#include <QStackedWidget>       // for QStackedWidget
#include <QTimer>               // for QTimer
#include <Qt>                   // for LeftButton, Checked, Unchecked, ControlModifier, ISODate, Key_Home, Key_K, MatchExactly, NoModifier
#include <QtGlobal>             // for qDebug, qPrintable
#include <QtTest>               // for QVERIFY, QFAIL, QVERIFY2, QCOMPARE, QTEST_MAIN

#include <utility>              // for move

#include "mainwindow.h"         // for MainWindow

test1::test1()
{
  qputenv("QTEST_FUNCTION_TIMEOUT", "120000");

  Q_INIT_RESOURCE(app);
#ifdef HAVE_EMBEDDED_MAP
  Q_INIT_RESOURCE(map);
#endif
#ifdef HAVE_EMBEDDED_TRANSLATIONS
  Q_INIT_RESOURCE(translations);
#endif

  // isolate Qt Settings storage from real application.
  QApplication::setOrganizationName("GPSBabel");
  QApplication::setOrganizationDomain("gpsbabel.org");
  QApplication::setApplicationName("GPSBabel-TEST");
}

test1::~test1()
{

}

void test1::initTestCase()
{
#if 1
  QSettings settings;
  settings.clear();
#endif
}

void test1::cleanupTestCase()
{
#if 0
  QSettings settings;
  settings.clear();
#endif
}

void test1::dialogcb()
{
  (this->*dialogHandler_)(status_);
}

void test1::runDialog(dialogStatus* status, dialog_cb dialogHandler, QWidget* button)
{
  (*status).errorCode = 0;
  status_ = status;
  dialogHandler_ = dialogHandler;
  QTimer::singleShot(100, this, &test1::dialogcb);
  if (button != nullptr) {
    QTest::mouseClick(button, Qt::LeftButton);
  }
}

void test1::test_case1()
{
  dialogStatus status;

  MainWindow mainWindow(nullptr);
  mainWindow.show();

  //mainWindow.dumpObjectTree();

  auto* actionAbout = mainWindow.findChild<QAction*>("actionAbout");
  QVERIFY(actionAbout != nullptr);
  runDialog(&status, &test1::AboutDialogTester, nullptr);
  // FIXME: not keyboard/mouse simulation
  actionAbout->trigger();
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));

  auto* actionPreferences = mainWindow.findChild<QAction*>("actionPreferences");
  QVERIFY(actionPreferences != nullptr);
  runDialog(&status, &test1::PreferencesDialogTester, nullptr);
  // FIXME: not keyboard/mouse simulation
  actionPreferences->trigger();
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));

  auto* inputFileOptBtn = mainWindow.findChild<QRadioButton*>("inputFileOptBtn");
  QVERIFY(inputFileOptBtn != nullptr);
  inputFileOptBtn->click();
  QVERIFY(inputFileOptBtn->isChecked());

  auto* inputFormatCombo = mainWindow.findChild<QComboBox*>("inputFormatCombo");
  QVERIFY(inputFormatCombo != nullptr);
  int iidx = inputFormatCombo->findText("GPX XML");
  QVERIFY(iidx >= 0);
  // FIXME: not keyboard/mouse simulation
  inputFormatCombo->setCurrentIndex(iidx);

  auto* inputFileNameText = mainWindow.findChild<QLineEdit*>("inputFileNameText");
  QVERIFY(inputFileNameText != nullptr);
  inputFileNameText->insert("reference/bounds-test.gpx");

  auto* xlateWayPtsCk = mainWindow.findChild<QCheckBox*>("xlateWayPtsCk");
  QVERIFY(xlateWayPtsCk != nullptr);
  xlateWayPtsCk->setCheckState(Qt::Checked);

  auto* xlateRoutesCk = mainWindow.findChild<QCheckBox*>("xlateRoutesCk");
  QVERIFY(xlateRoutesCk != nullptr);
  xlateRoutesCk->setCheckState(Qt::Unchecked);

  auto* xlateTracksCk = mainWindow.findChild<QCheckBox*>("xlateTracksCk");
  QVERIFY(xlateTracksCk != nullptr);
  xlateTracksCk->setCheckState(Qt::Unchecked);

  auto* xlateFiltersBtn = mainWindow.findChild<QPushButton*>("xlateFiltersBtn");
  QVERIFY(xlateFiltersBtn != nullptr);
  runDialog(&status, &test1::FilterDialogTester, xlateFiltersBtn);
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));

  auto* moreOptionButton = mainWindow.findChild<QPushButton*>("moreOptionButton");
  QVERIFY(moreOptionButton != nullptr);
  runDialog(&status, &test1::AdvDialogTester, moreOptionButton);
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));

  // The output file/device buttons can be set to check/unchecked, unchecked/unchecked,
  // or unchecked/checked.
  auto* outputFileOptBtn = mainWindow.findChild<QRadioButton*>("outputFileOptBtn");
  QVERIFY(outputFileOptBtn != nullptr);
  auto* outputDeviceOptBtn = mainWindow.findChild<QRadioButton*>("outputDeviceOptBtn");
  QVERIFY(outputDeviceOptBtn != nullptr);
  outputFileOptBtn->click();
  outputFileOptBtn->click();
  QVERIFY(outputFileOptBtn->isChecked());

  auto* outputFormatCombo = mainWindow.findChild<QComboBox*>("outputFormatCombo");
  QVERIFY(outputFormatCombo != nullptr);
  int oidx = outputFormatCombo->findText("Google Earth (Keyhole) Markup Language");
  QVERIFY(oidx >= 0);
  // FIXME: not keyboard/mouse simulation
  outputFormatCombo->setCurrentIndex(oidx);

  auto* outputFileNameText = mainWindow.findChild<QLineEdit*>("outputFileNameText");
  QVERIFY(outputFileNameText != nullptr);
  outputFileNameText->insert("junk");

  auto* outputOptionsBtn = mainWindow.findChild<QPushButton*>("outputOptionsBtn");
  QVERIFY(outputOptionsBtn != nullptr);
  runDialog(&status, &test1::OptionsDialogTester, outputOptionsBtn);
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));

  auto* buttonBox = mainWindow.findChild<QDialogButtonBox*>("buttonBox");
  QVERIFY(buttonBox != nullptr);
  auto* mainwindowOK = buttonBox->button(QDialogButtonBox::Ok);
  QVERIFY(mainwindowOK != nullptr);
  runDialog(&status, &test1::GMapDialogTester, mainwindowOK);
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));

  auto* outputWindow = mainWindow.findChild<QPlainTextEdit*>("outputWindow");
  QVERIFY(outputWindow != nullptr);
  QTest::qWait(2000);
  qDebug() << outputWindow->toPlainText();
  QString output = outputWindow->toPlainText().replace(QRegularExpression("\\S*GPSBabel-TEST\\......."), "GPSBabel-TEST.XXXXXX");
  QCOMPARE(output,
           QString("gpsbabel -w -i gpx -f reference/bounds-test.gpx -x sort,shortname -x track,start=20220517020304 -x duplicate,shortname -x simplify,count=100 -o kml,prec=3 -F junk -o gpx -F GPSBabel-TEST.XXXXXX\n\nTranslation successful"));

  auto* mainwindowClose = buttonBox->button(QDialogButtonBox::Close);
  QVERIFY(mainwindowClose != nullptr);
  runDialog(&status, &test1::DonateDialogTester, mainwindowClose);
  QVERIFY2(status.errorCode == 0, qPrintable(status.message));
}

QWidget* test1::selectFilterWidget(QWidget* filterDialog, const QString& dialogName, const QString& widgetName)
{
  auto* filterList = filterDialog->findChild<QListWidget*>("filterList");
  if (filterList == nullptr) {
    return nullptr;
  }

  auto itemlist = filterList->findItems(dialogName, Qt::MatchExactly);
  if (itemlist.count() != 1) {
    return nullptr;
  }

  auto* item = itemlist.at(0);
  // FIXME: not keyboard/mouse simulation
  // It isn't clear how to simulate clicking to make the item checked.
  item->setCheckState(Qt::Checked);
  QRect rect = filterList->visualItemRect(item);
  QTest::mouseClick(filterList->viewport(), Qt::LeftButton, Qt::NoModifier, rect.center());

  auto* widgetStack = filterDialog->findChild<QStackedWidget*>("widgetStack");
  if (widgetStack == nullptr) {
    return nullptr;
  }

  auto* currentWidget = widgetStack->currentWidget();
  if (currentWidget == nullptr || (currentWidget->objectName() != widgetName)) {
    return nullptr;
  }

  return currentWidget;
}

// QVERIFY and many other Q test macros can only be called from a test function
// invoked by the test framework.
#define DIALOGVERIFY(status, widget, condition, msg) \
  do { \
    if (static_cast<bool>(condition)) { \
      (*status).errorCode = 0; \
    } else { \
      (*status).errorCode = 1; \
      (*status).message = QString(msg); \
      if (widget != nullptr) { \
         qobject_cast<QDialog*>(widget)->done(-1); \
      } \
      return; \
    } \
  } while (false)

void test1::AboutDialogTester(dialogStatus* status)
{
  qDebug() << "About Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("AboutDlg")) {
      //DIALOGVERIFY(status, widget, false, "AboutDlg: test failure");
      //widget->dumpObjectTree();
      auto* buttonBox = widget->findChild<QDialogButtonBox*>("buttonBox");
      DIALOGVERIFY(status, widget, buttonBox != nullptr, "AboutDlg: can't find buttonBox");
      auto* optOK = buttonBox->button(QDialogButtonBox::Ok);
      DIALOGVERIFY(status, widget, optOK != nullptr, "AboutDlg: can't find OK button");
      QTest::mouseClick(optOK, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected AboutDlg, but someting else is the Modal Widget");
    }
  }
  qDebug() << "About Dialog Tester Exiting";
}

void test1::AdvDialogTester(dialogStatus* status)
{
  qDebug() << "Adv Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("AdvDlg")) {
      //widget->dumpObjectTree();
      auto* previewGmap = widget->findChild<QCheckBox*>("previewGmap");
      DIALOGVERIFY(status, widget, previewGmap != nullptr, "AdvDlg: can't find previewGmap");
      QTest::mouseClick(previewGmap, Qt::LeftButton);

      auto* formatButton = widget->findChild<QPushButton*>("formatButton");
      DIALOGVERIFY(status, widget, formatButton != nullptr, "AdvDlg: can't find formatButton");
      dialogStatus msgStatus;
      runDialog(&msgStatus, &test1::QMessageBoxDialogTester, formatButton);
      DIALOGVERIFY(status, widget, msgStatus.errorCode == 0, qPrintable(msgStatus.message));

      auto* advButtonBox = widget->findChild<QDialogButtonBox*>("buttonBox");
      DIALOGVERIFY(status, widget, advButtonBox != nullptr, "AdvDlg: can't find buttonBox");
      auto* advOK = advButtonBox->button(QDialogButtonBox::Ok);
      DIALOGVERIFY(status, widget, advOK != nullptr, "AdvDlg: can't find OK button");
      QTest::mouseClick(advOK, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected AdvDlg, but someting else is the Modal Widget");
    }
  }
  qDebug() << "Adv Dialog Tester Exiting";
}

void test1::DonateDialogTester(dialogStatus* status)
{
  qDebug() << "Donate Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("Donate")) {
      //widget->dumpObjectTree();
      auto* dismissButton = widget->findChild<QPushButton*>("dismissButton");
      DIALOGVERIFY(status, widget, dismissButton != nullptr, "Donate: can't find dismissButton");
      QTest::mouseClick(dismissButton, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected Donate, but someting else is the Modal Widget");
    }
  }
  qDebug() << "Donate Dialog Tester Exiting";
}

void test1::FilterDialogTester(dialogStatus* status)
{
  qDebug() << "Filter Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("FilterDialog")) {
      //widget->dumpObjectTree();
      auto* reset = widget->findChild<QPushButton*>("resetButton");
      DIALOGVERIFY(status, widget, reset != nullptr, "FilterDialog: can't find resetButton");
      //DIALOGVERIFY(status, widget, false, "test death");
      dialogStatus msgStatus;
      runDialog(&msgStatus, &test1::QMessageBoxDialogTester, reset);
      DIALOGVERIFY(status, widget, msgStatus.errorCode == 0, qPrintable(msgStatus.message));

      QWidget* currentWidget = selectFilterWidget(widget, "Miscellaneous", "MiscFltWidget");
      DIALOGVERIFY(status, widget, currentWidget != nullptr, "FilterDialog: can't find MiscFltWidget in Miscellaneous dialog.");

      auto* sortWptCheck = currentWidget->findChild<QCheckBox*>("sortWptCheck");
      DIALOGVERIFY(status, widget, sortWptCheck != nullptr, "FilterDialog: cant find sortWptCheck");
      QTest::mouseClick(sortWptCheck, Qt::LeftButton);

      auto* sortWptBy = currentWidget->findChild<QComboBox*>("sortWptBy");
      DIALOGVERIFY(status, widget, sortWptBy != nullptr, "FilterDialog: can't find sortWptBy");
      int cbidx = sortWptBy->findText("Name");
      DIALOGVERIFY(status, widget, cbidx >= 0, "");
      sortWptBy->setCurrentIndex(cbidx);

      QTest::qWait(2000);

      currentWidget = selectFilterWidget(widget, "Waypoints", "WayPtsWidget");
      DIALOGVERIFY(status, widget, currentWidget != nullptr, "FilterDialog: can't find WaypPtsWidget in Waypoints dialog.");

      auto* duplicatesCheck = currentWidget->findChild<QCheckBox*>("duplicatesCheck");
      DIALOGVERIFY(status, widget, duplicatesCheck != nullptr, "FilterDialog: can't find duplicatesCheck");
      QTest::mouseClick(duplicatesCheck, Qt::LeftButton);

      QTest::qWait(2000);

      currentWidget = selectFilterWidget(widget, "Routes & Tracks", "RtTrkWidget");
      DIALOGVERIFY(status, widget, currentWidget != nullptr, "FilterDialog: can't find RtTrkWidget in Routes & Tracks dialog.");

      auto* simplifyCheck = currentWidget->findChild<QCheckBox*>("simplifyCheck");
      DIALOGVERIFY(status, widget, simplifyCheck != nullptr, "FilterDialog: can't find simplifyCheck");
      QTest::mouseClick(simplifyCheck, Qt::LeftButton);

      QTest::qWait(2000);

      currentWidget = selectFilterWidget(widget, "Tracks", "TrackWidget");
      DIALOGVERIFY(status, widget, currentWidget != nullptr, "FilterDialog: can't find TrackWidget in Tracks dialog.");

      auto* startCheck = currentWidget->findChild<QCheckBox*>("startCheck");
      DIALOGVERIFY(status, widget, startCheck != nullptr, "FilterDialog: can't find startCheck");
      auto* stopCheck = currentWidget->findChild<QCheckBox*>("stopCheck");
      DIALOGVERIFY(status, widget, stopCheck != nullptr, "FilterDialog: can't find stopCheck");

      QTest::mouseClick(startCheck, Qt::LeftButton);

      auto* utc = currentWidget->findChild<QRadioButton*>("utc");
      DIALOGVERIFY(status, widget, utc != nullptr, "FilterDialog: can't find utc");
      auto* localTime = currentWidget->findChild<QRadioButton*>("localTime");
      DIALOGVERIFY(status, widget, localTime != nullptr, "FilterDialog: can't find localTime");
      auto* startEdit = currentWidget->findChild<QDateTimeEdit*>("startEdit");
      DIALOGVERIFY(status, widget, startEdit != nullptr, "FilterDialog: can't find startEdit");
      auto* stopEdit = currentWidget->findChild<QDateTimeEdit*>("stopEdit");
      DIALOGVERIFY(status, widget, stopEdit != nullptr, "FilterDialog: can't find stopEdit");

#if 0
      // It seems to require searching to find the clickable area of a radio button.
      QTest::mouseClick(utc, Qt::LeftButton, Qt::KeyboardModifiers(), QPoint(0, utc->height()/2));
#else
      utc->click();
#endif
      DIALOGVERIFY(status, widget, utc->isChecked(), "FilterDialog: difficulty checking utc");
      DIALOGVERIFY(status, widget, !localTime->isChecked(), "FilterDialog: difficulty checking utc");
      DIALOGVERIFY(status, widget, startEdit->timeSpec() == Qt::UTC, "FilterDialog: difficulty checking utc");
      DIALOGVERIFY(status, widget, stopEdit->timeSpec() == Qt::UTC, "FilterDialog: difficulty checking utc");
      QTest::qWait(2000);

#if 0
      // It seems to require searching to find the clickable area of a radio button.
      QTest::mouseClick(localTime, Qt::LeftButton, Qt::KeyboardModifiers(), QPoint(0, localTime->height()/2));
#else
      localTime->click();
#endif
      DIALOGVERIFY(status, widget, !utc->isChecked(), "FilterDialog: difficulty checking localTime");
      DIALOGVERIFY(status, widget, localTime->isChecked(), "FilterDialog: difficulty checking localTime");
      DIALOGVERIFY(status, widget, startEdit->timeSpec() == Qt::LocalTime, "FilterDialog: difficulty checking localTime");
      DIALOGVERIFY(status, widget, stopEdit->timeSpec() == Qt::LocalTime, "FilterDialog: difficulty checking localTime");
      QTest::qWait(2000);

      // FIXME: not keyboard/mouse simulation
      // In Qt 5 "When setting this property the timespec of the QDateTimeEdit remains the same and the timespec of the new QDateTime is ignored."
      // In Qt 6 "When setting this property, the new QDateTime is converted to the timespec of the QDateTimeEdit, which thus remains unchanged."
      // To cover up this change in behavior convert to the QDateTimeEdit timeSpec manually.
      auto startDT = QDateTime::fromString("2022-05-17T02:03:04Z", Qt::ISODate).toTimeSpec(startEdit->timeSpec());
      startEdit->setDateTime(startDT);
      DIALOGVERIFY(status, widget, startEdit->timeSpec() == Qt::LocalTime, "FilterDialog: unexpected timeSpec of startEdit");
      DIALOGVERIFY(status, widget, startEdit->dateTime() == startDT.toTimeSpec(Qt::LocalTime), "FilterDialog: unexpected startTime set");
      QTest::qWait(2000);

      auto* filterButtonBox = widget->findChild<QDialogButtonBox*>("buttonBox");
      DIALOGVERIFY(status, widget, filterButtonBox != nullptr, "FilterDialog: can't find buttonBox");
      auto* filterOK = filterButtonBox->button(QDialogButtonBox::Ok);
      DIALOGVERIFY(status, widget, filterOK != nullptr, "FilterDialog: can't find OK button");
      QTest::mouseClick(filterOK, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected FilterDialog, but someting else is the Modal Widget");
    }
  }
  qDebug() << "Filter Dialog Tester Exiting";
}

void test1::GMapDialogTester(dialogStatus* status)
{
  qDebug() << "GMap Dialog Tester";
  QTest::qWait(2000);
  // Why can't we use activeModalWidget()?
  // Gnome application "is ready" notification?
  qDebug() << QApplication::activeModalWidget();
  const auto topLevelWidgets = QApplication::topLevelWidgets();
  QWidget* gmapwidget = nullptr;
  for (auto* widget : topLevelWidgets) {
    qDebug() << "top level widget" << widget;
    if (widget->inherits("GMapDialog")) {
      gmapwidget = widget;
      break;
    }
  }
  // We may have to wait for the CLI to run (ProcessWaitDialog).
  if (gmapwidget == nullptr) {
    qDebug() << "Waiting for GMapDialog";
    QTimer::singleShot(200, this, &test1::dialogcb);
    return;
  }
  auto* gmapButtonBox = gmapwidget->findChild<QDialogButtonBox*>("buttonBox");
  DIALOGVERIFY(status, gmapwidget, gmapButtonBox != nullptr, "GMapDialog: can't find buttonBox");
  auto* gmapClose = gmapButtonBox->button(QDialogButtonBox::Close);
  DIALOGVERIFY(status, gmapwidget, gmapClose != nullptr, "GMapDialog: can't find Close button");
  QTest::mouseClick(gmapClose, Qt::LeftButton);
  qDebug() << "GMap Dialog Tester Exiting";
}

void test1::OptionsDialogTester(dialogStatus* status)
{
  qDebug() << "Options Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("OptionsDlg")) {
      //widget->dumpObjectTree();
      auto* precOpt = widget->findChild<QCheckBox*>("kml_prec");
      DIALOGVERIFY(status, widget, precOpt != nullptr, "OptionsDlg: can't find kml_prec QCheckBox");
      QTest::mouseClick(precOpt, Qt::LeftButton);
      auto* precValue = widget->findChild<QLineEdit*>("kml_prec");
      DIALOGVERIFY(status, widget, precValue != nullptr, "OptionsDlg: can't find kml_prec QLineEdit");
#ifdef Q_OS_MACOS
      QTest::keyClick(precValue, Qt::Key_A, Qt::MetaModifier); // select all
      QTest::keyClick(precValue, Qt::Key_Delete);
#else
      QTest::keyClick(precValue, Qt::Key_Home); // move to beginning of line
      QTest::keyClick(precValue, Qt::Key_K, Qt::ControlModifier); // delete to end of line
#endif
      QTest::keyClicks(precValue, "3");
      QTest::qWait(1000);

      auto* optButtonBox = widget->findChild<QDialogButtonBox*>("buttonBox");
      DIALOGVERIFY(status, widget, optButtonBox != nullptr, "OptionsDlg: can't find buttonBox");
      auto* optOK = optButtonBox->button(QDialogButtonBox::Ok);
      DIALOGVERIFY(status, widget, optOK != nullptr, "OptionsDlg: can't find OK button");
      QTest::mouseClick(optOK, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected OptionsDlg, but someting else is the Modal Widget");
    }
  }
  qDebug() << "Options Dialog Tester Exiting";
}

void test1::QMessageBoxDialogTester(dialogStatus* status)
{
  qDebug() << "QMessageBox Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("QMessageBox")) {
      //widget->dumpObjectTree();
      QMessageBox* mb = qobject_cast<QMessageBox*>(widget);
      auto* yes = mb->button(QMessageBox::Yes);
      DIALOGVERIFY(status, widget, yes != nullptr, "QMessageBoxDialog: can't find Yes button");
      QTest::mouseClick(yes, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected QMessagBox, but someting else is the Modal Widget");
    }
  }
  qDebug() << "QMessageBox Dialog Tester Exiting";
}

void test1::PreferencesDialogTester(dialogStatus* status)
{
  qDebug() << "Preferences Dialog Tester";
  QTest::qWait(1000);
  auto* widget = QApplication::activeModalWidget();
  if (widget != nullptr) {
    if (widget->inherits("Preferences")) {
      //widget->dumpObjectTree();
      bool tabWidgetFound = false;
      auto* tabWidget = widget->findChild<QTabWidget*>("tabWidget");
      DIALOGVERIFY(status, widget, tabWidget != nullptr, "PreferencesDialog; can't find QTabWidget");
      for (int idx = 0; idx < tabWidget->count(); ++idx) {
        if (tabWidget->tabText(idx) == "Formats") {
          tabWidget->setCurrentIndex(idx);
          tabWidgetFound = true;
          break;
        }
      }
      DIALOGVERIFY(status, widget, tabWidgetFound, "PreferencesDialog: can't find Formats tab");
      QTest::qWait(1000);

      auto* enableAllButton = tabWidget->findChild<QPushButton*>("enableAllButton");
      DIALOGVERIFY(status, widget, enableAllButton != nullptr, "PreferencesDialog: can't find enableAllButton");
      QTest::mouseClick(enableAllButton, Qt::LeftButton);

      auto* buttonBox = widget->findChild<QDialogButtonBox*>("buttonBox");
      DIALOGVERIFY(status, widget, buttonBox != nullptr, "PreferncesDialog: can't find buttonBox");
      auto* optOK = buttonBox->button(QDialogButtonBox::Ok);
      DIALOGVERIFY(status, widget, optOK != nullptr, "PreferencesDialog: can't find OK button");
      QTest::mouseClick(optOK, Qt::LeftButton);
    } else {
      DIALOGVERIFY(status, widget, false, "Expected Preferences, but someting else is the Modal Widget");
    }
  }
  qDebug() << "Preferences Dialog Tester Exiting";
}
QTEST_MAIN(test1)

//#include "main.moc"
