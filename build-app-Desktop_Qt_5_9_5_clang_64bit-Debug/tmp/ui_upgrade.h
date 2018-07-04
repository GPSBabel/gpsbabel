/********************************************************************************
** Form generated from reading UI file 'upgrade.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_UPGRADE_H
#define UI_UPGRADE_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTextBrowser>

QT_BEGIN_NAMESPACE

class Ui_Upgrade
{
public:
    QDialogButtonBox *buttonBox;
    QTextBrowser *textBrowser;

    void setupUi(QDialog *Upgrade)
    {
        if (Upgrade->objectName().isEmpty())
            Upgrade->setObjectName(QStringLiteral("Upgrade"));
        Upgrade->resize(400, 300);
        buttonBox = new QDialogButtonBox(Upgrade);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(30, 240, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        textBrowser = new QTextBrowser(Upgrade);
        textBrowser->setObjectName(QStringLiteral("textBrowser"));
        textBrowser->setGeometry(QRect(10, 10, 381, 211));

        retranslateUi(Upgrade);
        QObject::connect(buttonBox, SIGNAL(accepted()), Upgrade, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), Upgrade, SLOT(reject()));

        QMetaObject::connectSlotsByName(Upgrade);
    } // setupUi

    void retranslateUi(QDialog *Upgrade)
    {
        Upgrade->setWindowTitle(QApplication::translate("Upgrade", "Dialog", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class Upgrade: public Ui_Upgrade {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_UPGRADE_H
