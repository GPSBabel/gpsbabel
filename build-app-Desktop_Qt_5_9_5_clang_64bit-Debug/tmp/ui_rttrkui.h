/********************************************************************************
** Form generated from reading UI file 'rttrkui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_RTTRKUI_H
#define UI_RTTRKUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_RtTrkWidget
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *label;
    QHBoxLayout *horizontalLayout;
    QCheckBox *simplifyCheck;
    QSpacerItem *horizontalSpacer;
    QLabel *limitToLabel;
    QSpinBox *limitToSpin;
    QLabel *pointLabel;
    QSpacerItem *horizontalSpacer_2;
    QCheckBox *reverseCheck;
    QSpacerItem *verticalSpacer;

    void setupUi(QWidget *RtTrkWidget)
    {
        if (RtTrkWidget->objectName().isEmpty())
            RtTrkWidget->setObjectName(QStringLiteral("RtTrkWidget"));
        RtTrkWidget->resize(305, 114);
        verticalLayout = new QVBoxLayout(RtTrkWidget);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        label = new QLabel(RtTrkWidget);
        label->setObjectName(QStringLiteral("label"));
        QFont font;
        font.setPointSize(11);
        font.setBold(true);
        font.setWeight(75);
        label->setFont(font);

        verticalLayout->addWidget(label);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        simplifyCheck = new QCheckBox(RtTrkWidget);
        simplifyCheck->setObjectName(QStringLiteral("simplifyCheck"));

        horizontalLayout->addWidget(simplifyCheck);

        horizontalSpacer = new QSpacerItem(20, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        limitToLabel = new QLabel(RtTrkWidget);
        limitToLabel->setObjectName(QStringLiteral("limitToLabel"));

        horizontalLayout->addWidget(limitToLabel);

        limitToSpin = new QSpinBox(RtTrkWidget);
        limitToSpin->setObjectName(QStringLiteral("limitToSpin"));

        horizontalLayout->addWidget(limitToSpin);

        pointLabel = new QLabel(RtTrkWidget);
        pointLabel->setObjectName(QStringLiteral("pointLabel"));

        horizontalLayout->addWidget(pointLabel);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout);

        reverseCheck = new QCheckBox(RtTrkWidget);
        reverseCheck->setObjectName(QStringLiteral("reverseCheck"));

        verticalLayout->addWidget(reverseCheck);

        verticalSpacer = new QSpacerItem(20, 19, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);


        retranslateUi(RtTrkWidget);

        QMetaObject::connectSlotsByName(RtTrkWidget);
    } // setupUi

    void retranslateUi(QWidget *RtTrkWidget)
    {
        RtTrkWidget->setWindowTitle(QApplication::translate("RtTrkWidget", "Form", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        RtTrkWidget->setToolTip(QApplication::translate("RtTrkWidget", "Simplify routes and tracks by removing points", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        RtTrkWidget->setWhatsThis(QApplication::translate("RtTrkWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'DejaVu Sans'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">The Simplify filter is used to simplify routes and tracks for use with formats that limit the number of points they can contain or just to reduce the complexity of a route. </p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">The filter attempts to remove points from each route until the number of points or the error is within the given bounds, while also attempting to preserve the shape of the original route as much as possible. </p>\n"
"<p style=\" margin-top:12px; margin-bottom"
                        ":12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">The quality of the results will vary depending on the density of points in the original route and the length of the original route. </p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"></p></body></html>", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        label->setText(QApplication::translate("RtTrkWidget", "Routes & Tracks", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        simplifyCheck->setToolTip(QApplication::translate("RtTrkWidget", "Simplify route by removing points", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        simplifyCheck->setWhatsThis(QApplication::translate("RtTrkWidget", " The Simplify filter is used to simplify routes and tracks for use with formats that limit the number of points they can contain or just to reduce the complexity of a route.\n"
"\n"
"The filter attempts to remove points from each route until the number of points or the error is within the given bounds, while also attempting to preserve the shape of the original route as much as possible.\n"
"\n"
"The quality of the results will vary depending on the density of points in the original route and the length of the original route. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        simplifyCheck->setText(QApplication::translate("RtTrkWidget", "Simplify", Q_NULLPTR));
        limitToLabel->setText(QApplication::translate("RtTrkWidget", "Limit To", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        limitToSpin->setToolTip(QApplication::translate("RtTrkWidget", "Maximum number points in track or route. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        pointLabel->setText(QApplication::translate("RtTrkWidget", "Points", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        reverseCheck->setToolTip(QApplication::translate("RtTrkWidget", "Reverse tracks and routes", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        reverseCheck->setWhatsThis(QApplication::translate("RtTrkWidget", "The reversal is performed in the laziest way possible. Timestamps are kept with the original waypoints so the resulting track or route will have the interesting characteristic that time runs backwards. This tends to make Magellan Mapsend, in particular, do a wierd thing and place each waypoint on a separate day. \n"
"Additionally, if you're using this to reverse a route that navigates, say, an exit ramp or a one way street, you will be in for unpleasant ride. application cares about timestamps ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        reverseCheck->setText(QApplication::translate("RtTrkWidget", "Reverse", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class RtTrkWidget: public Ui_RtTrkWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_RTTRKUI_H
