/********************************************************************************
** Form generated from reading UI file 'donate.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DONATE_H
#define UI_DONATE_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Donate
{
public:
    QWidget *layoutWidget;
    QVBoxLayout *verticalLayout;
    QLabel *textLine1;
    QLabel *textLine2;
    QCheckBox *neverAgain;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *dismissButton;
    QPushButton *contributeButton;

    void setupUi(QDialog *Donate)
    {
        if (Donate->objectName().isEmpty())
            Donate->setObjectName(QStringLiteral("Donate"));
        Donate->resize(351, 300);
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(Donate->sizePolicy().hasHeightForWidth());
        Donate->setSizePolicy(sizePolicy);
        Donate->setMinimumSize(QSize(0, 0));
        Donate->setMaximumSize(QSize(400, 300));
        layoutWidget = new QWidget(Donate);
        layoutWidget->setObjectName(QStringLiteral("layoutWidget"));
        layoutWidget->setGeometry(QRect(10, 13, 312, 249));
        verticalLayout = new QVBoxLayout(layoutWidget);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        textLine1 = new QLabel(layoutWidget);
        textLine1->setObjectName(QStringLiteral("textLine1"));
        textLine1->setWordWrap(true);

        verticalLayout->addWidget(textLine1);

        textLine2 = new QLabel(layoutWidget);
        textLine2->setObjectName(QStringLiteral("textLine2"));
        textLine2->setWordWrap(true);

        verticalLayout->addWidget(textLine2);

        neverAgain = new QCheckBox(layoutWidget);
        neverAgain->setObjectName(QStringLiteral("neverAgain"));

        verticalLayout->addWidget(neverAgain);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        dismissButton = new QPushButton(layoutWidget);
        dismissButton->setObjectName(QStringLiteral("dismissButton"));

        horizontalLayout->addWidget(dismissButton);

        contributeButton = new QPushButton(layoutWidget);
        contributeButton->setObjectName(QStringLiteral("contributeButton"));
        contributeButton->setAutoDefault(false);

        horizontalLayout->addWidget(contributeButton);


        verticalLayout->addLayout(horizontalLayout);


        retranslateUi(Donate);
        QObject::connect(dismissButton, SIGNAL(clicked()), Donate, SLOT(close()));

        contributeButton->setDefault(true);


        QMetaObject::connectSlotsByName(Donate);
    } // setupUi

    void retranslateUi(QDialog *Donate)
    {
        Donate->setWindowTitle(QApplication::translate("Donate", "Support GPSBabel", Q_NULLPTR));
        textLine1->setText(QApplication::translate("Donate", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">GPSBabel is free software built and supported by volunteers. It consumes vast amounts of time to create and support the software as well as money for mapping programs, GPS receivers, and development fixtures. Please see how you can <a href=\"https://www.gpsbabel.org\"><span style=\" text-decoration: underline; color:#0000ff;\">contribute time or via PayPal (no account needed).</span></a></p></body></html>", Q_NULLPTR));
        textLine2->setText(QApplication::translate("Donate", "<p>Of course, if you've already contributed  to the project or you just can't help the project, please check the box below to never see this message again.</p>", Q_NULLPTR));
        neverAgain->setText(QApplication::translate("Donate", "Never show this message again.", Q_NULLPTR));
        dismissButton->setText(QApplication::translate("Donate", "No, Thanks", Q_NULLPTR));
        contributeButton->setText(QApplication::translate("Donate", "Contribute", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class Donate: public Ui_Donate {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DONATE_H
