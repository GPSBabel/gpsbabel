#include <QObject>  // for QObject, Q_OBJECT, slots
#include <QString>  // for QString
#include <QWidget>  // for QWidget


class test1 : public QObject
{
  Q_OBJECT

public:
  /* Types */

  struct dialogStatus {
    int errorCode;
    QString message;
  };

  using dialog_cb = void (test1::*)(dialogStatus*);

  /* Special Member Functions */

  test1();
  ~test1();

private:
  /* Member Functions */

  // don't declare these slots so they won't be run as tests.
  static QWidget* selectFilterWidget(QWidget* filterDialog, const QString& dialogName, const QString& widgetName);
  void dialogcb();
  void runDialog(dialogStatus*, dialog_cb, QWidget*);

  void AboutDialogTester(dialogStatus* status);
  void AdvDialogTester(dialogStatus* status);
  void DonateDialogTester(dialogStatus* status);
  void FilterDialogTester(dialogStatus* status);
  void GMapDialogTester(dialogStatus* status);
  void OptionsDialogTester(dialogStatus* status);
  void QMessageBoxDialogTester(dialogStatus* status);
  void PreferencesDialogTester(dialogStatus* status);

private slots:
  /* Member Functions */

  void initTestCase();
  void cleanupTestCase();
  void test_case1();

private:
  dialogStatus* status_{nullptr};
  dialog_cb dialogHandler_{nullptr};
};
