/********************************************************************************
** Form generated from reading UI file 'wayptsui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_WAYPTSUI_H
#define UI_WAYPTSUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_WayPtsWidget
{
public:
    QGridLayout *gridLayout_2;
    QGridLayout *gridLayout;
    QLabel *label;
    QCheckBox *duplicatesCheck;
    QCheckBox *shortNamesCheck;
    QCheckBox *locationsCheck;
    QCheckBox *positionCheck;
    QLineEdit *positionText;
    QComboBox *positionUnitCombo;
    QCheckBox *radiusCheck;
    QLineEdit *radiusText;
    QComboBox *radiusUnitCombo;
    QLabel *latLabel;
    QLineEdit *latText;
    QLabel *longLabel;
    QLineEdit *longText;
    QCheckBox *sortCheck;
    QSpacerItem *horizontalSpacer;
    QSpacerItem *verticalSpacer;

    void setupUi(QWidget *WayPtsWidget)
    {
        if (WayPtsWidget->objectName().isEmpty())
            WayPtsWidget->setObjectName(QStringLiteral("WayPtsWidget"));
        WayPtsWidget->resize(523, 195);
        gridLayout_2 = new QGridLayout(WayPtsWidget);
        gridLayout_2->setObjectName(QStringLiteral("gridLayout_2"));
        gridLayout = new QGridLayout();
        gridLayout->setObjectName(QStringLiteral("gridLayout"));
        label = new QLabel(WayPtsWidget);
        label->setObjectName(QStringLiteral("label"));
        QFont font;
        font.setPointSize(11);
        font.setBold(true);
        font.setWeight(75);
        label->setFont(font);

        gridLayout->addWidget(label, 0, 0, 1, 5);

        duplicatesCheck = new QCheckBox(WayPtsWidget);
        duplicatesCheck->setObjectName(QStringLiteral("duplicatesCheck"));

        gridLayout->addWidget(duplicatesCheck, 1, 0, 1, 1);

        shortNamesCheck = new QCheckBox(WayPtsWidget);
        shortNamesCheck->setObjectName(QStringLiteral("shortNamesCheck"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(shortNamesCheck->sizePolicy().hasHeightForWidth());
        shortNamesCheck->setSizePolicy(sizePolicy);

        gridLayout->addWidget(shortNamesCheck, 1, 1, 1, 1);

        locationsCheck = new QCheckBox(WayPtsWidget);
        locationsCheck->setObjectName(QStringLiteral("locationsCheck"));

        gridLayout->addWidget(locationsCheck, 1, 2, 1, 1);

        positionCheck = new QCheckBox(WayPtsWidget);
        positionCheck->setObjectName(QStringLiteral("positionCheck"));

        gridLayout->addWidget(positionCheck, 2, 0, 1, 1);

        positionText = new QLineEdit(WayPtsWidget);
        positionText->setObjectName(QStringLiteral("positionText"));
        positionText->setMaximumSize(QSize(80, 16777215));

        gridLayout->addWidget(positionText, 2, 1, 1, 1);

        positionUnitCombo = new QComboBox(WayPtsWidget);
        positionUnitCombo->setObjectName(QStringLiteral("positionUnitCombo"));

        gridLayout->addWidget(positionUnitCombo, 2, 2, 1, 1);

        radiusCheck = new QCheckBox(WayPtsWidget);
        radiusCheck->setObjectName(QStringLiteral("radiusCheck"));

        gridLayout->addWidget(radiusCheck, 3, 0, 1, 1);

        radiusText = new QLineEdit(WayPtsWidget);
        radiusText->setObjectName(QStringLiteral("radiusText"));
        radiusText->setMaximumSize(QSize(80, 16777215));

        gridLayout->addWidget(radiusText, 3, 1, 1, 1);

        radiusUnitCombo = new QComboBox(WayPtsWidget);
        radiusUnitCombo->setObjectName(QStringLiteral("radiusUnitCombo"));

        gridLayout->addWidget(radiusUnitCombo, 3, 2, 1, 1);

        latLabel = new QLabel(WayPtsWidget);
        latLabel->setObjectName(QStringLiteral("latLabel"));

        gridLayout->addWidget(latLabel, 3, 3, 1, 1);

        latText = new QLineEdit(WayPtsWidget);
        latText->setObjectName(QStringLiteral("latText"));
        latText->setMinimumSize(QSize(110, 0));

        gridLayout->addWidget(latText, 3, 4, 1, 1);

        longLabel = new QLabel(WayPtsWidget);
        longLabel->setObjectName(QStringLiteral("longLabel"));

        gridLayout->addWidget(longLabel, 4, 3, 1, 1);

        longText = new QLineEdit(WayPtsWidget);
        longText->setObjectName(QStringLiteral("longText"));
        longText->setMinimumSize(QSize(110, 0));

        gridLayout->addWidget(longText, 4, 4, 1, 1);

        sortCheck = new QCheckBox(WayPtsWidget);
        sortCheck->setObjectName(QStringLiteral("sortCheck"));

        gridLayout->addWidget(sortCheck, 5, 0, 1, 1);


        gridLayout_2->addLayout(gridLayout, 0, 0, 1, 1);

        horizontalSpacer = new QSpacerItem(80, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout_2->addItem(horizontalSpacer, 0, 1, 1, 1);

        verticalSpacer = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout_2->addItem(verticalSpacer, 1, 0, 1, 1);


        retranslateUi(WayPtsWidget);

        QMetaObject::connectSlotsByName(WayPtsWidget);
    } // setupUi

    void retranslateUi(QWidget *WayPtsWidget)
    {
        WayPtsWidget->setWindowTitle(QApplication::translate("WayPtsWidget", "Form", Q_NULLPTR));
        label->setText(QApplication::translate("WayPtsWidget", "Waypoints Filters", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        duplicatesCheck->setToolTip(QApplication::translate("WayPtsWidget", "Remove duplicates", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        duplicatesCheck->setWhatsThis(QApplication::translate("WayPtsWidget", "The duplicate filter is designed to remove duplicate points based on their short name (traditionally a waypoint's name on the GPS receiver), and/or their location (to a precision of 6 decimals). This filter supports two options that specify how duplicates will be recognized, shortname and location. Generally, at least one of these options is required. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        duplicatesCheck->setText(QApplication::translate("WayPtsWidget", "Duplicates", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        shortNamesCheck->setToolTip(QApplication::translate("WayPtsWidget", "Suppress duplicate waypoints based on name.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        shortNamesCheck->setWhatsThis(QApplication::translate("WayPtsWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'DejaVu Sans'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Suppress duplicate waypoints based on name. </p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">This option is the one most often used with the duplicate filter. This option instructs the duplicate filter to remove any waypoints that share a short name with a waypoint that has come before. This option might be used to remove duplicates if you are merging two datasets that were each created in part from a common ancestor dataset. </p></body></html>", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        shortNamesCheck->setText(QApplication::translate("WayPtsWidget", "Short Names", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        locationsCheck->setToolTip(QApplication::translate("WayPtsWidget", "Suppress duplicate waypoint based on coords. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        locationsCheck->setWhatsThis(QApplication::translate("WayPtsWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'DejaVu Sans'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Suppress duplicate waypoint based on coords. </p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">This option causes the duplicate filter to remove any additional waypoint that has the same coordinates (to six decimal degrees) as a waypoint that came before. This option may be used to remove duplicate waypoints if the names are not expected to be the same. It also might be used along with the <span style=\" font-family:'Courier New,courier';\">shortname</span> option to remove dup"
                        "licate waypoints if the names of several unrelated groups of waypoints might be the same. </p></body></html>", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        locationsCheck->setText(QApplication::translate("WayPtsWidget", "Locations", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        positionCheck->setToolTip(QApplication::translate("WayPtsWidget", "Remove points based on proximity", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        positionCheck->setWhatsThis(QApplication::translate("WayPtsWidget", "Maximum positional distance.\n"
"\n"
"This option specifies the minimum allowable distance between two points. If two points are closer than this distance, only one of them is kept. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        positionCheck->setText(QApplication::translate("WayPtsWidget", "Position", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        positionText->setToolTip(QApplication::translate("WayPtsWidget", "Maximum positional distance.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        positionUnitCombo->clear();
        positionUnitCombo->insertItems(0, QStringList()
         << QApplication::translate("WayPtsWidget", "Feet", Q_NULLPTR)
         << QApplication::translate("WayPtsWidget", "Meters", Q_NULLPTR)
        );
#ifndef QT_NO_TOOLTIP
        radiusCheck->setToolTip(QApplication::translate("WayPtsWidget", "Include points only within radius", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        radiusCheck->setWhatsThis(QApplication::translate("WayPtsWidget", "This filter includes or excludes waypoints based on their proximity to a central point. All waypoints more than the specified distance from the specified point will be removed from the dataset.\n"
"\n"
"By default, all remaining points are sorted so that points closer to the center appear earlier in the output file. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        radiusCheck->setText(QApplication::translate("WayPtsWidget", "Radius", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        radiusText->setToolTip(QApplication::translate("WayPtsWidget", "Maximum distance from center. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        radiusUnitCombo->clear();
        radiusUnitCombo->insertItems(0, QStringList()
         << QApplication::translate("WayPtsWidget", "Miles", Q_NULLPTR)
         << QApplication::translate("WayPtsWidget", "km", Q_NULLPTR)
        );
        latLabel->setText(QApplication::translate("WayPtsWidget", "Lat.", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        latText->setToolTip(QApplication::translate("WayPtsWidget", "Latitude of the central point in decimal degrees.  South latitudes should be expressed as a negative number.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        longLabel->setText(QApplication::translate("WayPtsWidget", "Long.", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        longText->setToolTip(QApplication::translate("WayPtsWidget", "Longitude of the central point in decimal degrees. West longitudes should be expressed as a negative number.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        sortCheck->setToolTip(QApplication::translate("WayPtsWidget", "This filter sorts waypoints into alphabetical order", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        sortCheck->setWhatsThis(QApplication::translate("WayPtsWidget", "This filter sorts waypoints into alphabetical order", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        sortCheck->setText(QApplication::translate("WayPtsWidget", "Sort", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class WayPtsWidget: public Ui_WayPtsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_WAYPTSUI_H
