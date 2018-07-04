/********************************************************************************
** Form generated from reading UI file 'trackui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_TRACKUI_H
#define UI_TRACKUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDateTimeEdit>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_TrackWidget
{
public:
    QGridLayout *gridLayout_2;
    QGridLayout *gridLayout;
    QLabel *label;
    QCheckBox *titleCheck;
    QLineEdit *titleText;
    QCheckBox *moveCheck;
    QHBoxLayout *horizontalLayout_5;
    QSpinBox *daysSpin;
    QLabel *daysLabel;
    QHBoxLayout *horizontalLayout_4;
    QSpinBox *hoursSpin;
    QLabel *hoursLabel;
    QHBoxLayout *horizontalLayout_2;
    QSpinBox *minsSpin;
    QLabel *minsLabel;
    QHBoxLayout *horizontalLayout;
    QSpinBox *secsSpin;
    QLabel *secsLabel;
    QCheckBox *startCheck;
    QDateTimeEdit *startEdit;
    QCheckBox *TZCheck;
    QCheckBox *stopCheck;
    QDateTimeEdit *stopEdit;
    QCheckBox *packCheck;
    QCheckBox *mergeCheck;
    QRadioButton *splitDateCheck;
    QRadioButton *splitTimeCheck;
    QHBoxLayout *horizontalLayout_6;
    QSpinBox *splitTimeSpin;
    QComboBox *splitTimeCombo;
    QRadioButton *splitDistanceCheck;
    QHBoxLayout *horizontalLayout_7;
    QSpinBox *splitDistSpin;
    QComboBox *splitDistCombo;
    QHBoxLayout *horizontalLayout_8;
    QCheckBox *GPSFixesCheck;
    QComboBox *GPSFixesCombo;
    QSpacerItem *horizontalSpacer_2;
    QCheckBox *speedCheck;
    QCheckBox *courseCheck;
    QSpacerItem *horizontalSpacer;
    QSpacerItem *verticalSpacer;

    void setupUi(QWidget *TrackWidget)
    {
        if (TrackWidget->objectName().isEmpty())
            TrackWidget->setObjectName(QStringLiteral("TrackWidget"));
        TrackWidget->resize(663, 248);
        gridLayout_2 = new QGridLayout(TrackWidget);
        gridLayout_2->setObjectName(QStringLiteral("gridLayout_2"));
        gridLayout = new QGridLayout();
        gridLayout->setObjectName(QStringLiteral("gridLayout"));
        label = new QLabel(TrackWidget);
        label->setObjectName(QStringLiteral("label"));
        QFont font;
        font.setPointSize(11);
        font.setBold(true);
        font.setWeight(75);
        label->setFont(font);

        gridLayout->addWidget(label, 0, 0, 1, 2);

        titleCheck = new QCheckBox(TrackWidget);
        titleCheck->setObjectName(QStringLiteral("titleCheck"));

        gridLayout->addWidget(titleCheck, 1, 0, 1, 1);

        titleText = new QLineEdit(TrackWidget);
        titleText->setObjectName(QStringLiteral("titleText"));

        gridLayout->addWidget(titleText, 1, 1, 1, 4);

        moveCheck = new QCheckBox(TrackWidget);
        moveCheck->setObjectName(QStringLiteral("moveCheck"));

        gridLayout->addWidget(moveCheck, 2, 0, 1, 1);

        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setObjectName(QStringLiteral("horizontalLayout_5"));
        daysSpin = new QSpinBox(TrackWidget);
        daysSpin->setObjectName(QStringLiteral("daysSpin"));

        horizontalLayout_5->addWidget(daysSpin);

        daysLabel = new QLabel(TrackWidget);
        daysLabel->setObjectName(QStringLiteral("daysLabel"));

        horizontalLayout_5->addWidget(daysLabel);


        gridLayout->addLayout(horizontalLayout_5, 2, 1, 1, 1);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setObjectName(QStringLiteral("horizontalLayout_4"));
        hoursSpin = new QSpinBox(TrackWidget);
        hoursSpin->setObjectName(QStringLiteral("hoursSpin"));

        horizontalLayout_4->addWidget(hoursSpin);

        hoursLabel = new QLabel(TrackWidget);
        hoursLabel->setObjectName(QStringLiteral("hoursLabel"));

        horizontalLayout_4->addWidget(hoursLabel);


        gridLayout->addLayout(horizontalLayout_4, 2, 3, 1, 1);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QStringLiteral("horizontalLayout_2"));
        minsSpin = new QSpinBox(TrackWidget);
        minsSpin->setObjectName(QStringLiteral("minsSpin"));

        horizontalLayout_2->addWidget(minsSpin);

        minsLabel = new QLabel(TrackWidget);
        minsLabel->setObjectName(QStringLiteral("minsLabel"));

        horizontalLayout_2->addWidget(minsLabel);


        gridLayout->addLayout(horizontalLayout_2, 2, 4, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        secsSpin = new QSpinBox(TrackWidget);
        secsSpin->setObjectName(QStringLiteral("secsSpin"));

        horizontalLayout->addWidget(secsSpin);

        secsLabel = new QLabel(TrackWidget);
        secsLabel->setObjectName(QStringLiteral("secsLabel"));

        horizontalLayout->addWidget(secsLabel);


        gridLayout->addLayout(horizontalLayout, 2, 5, 1, 1);

        startCheck = new QCheckBox(TrackWidget);
        startCheck->setObjectName(QStringLiteral("startCheck"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(startCheck->sizePolicy().hasHeightForWidth());
        startCheck->setSizePolicy(sizePolicy);

        gridLayout->addWidget(startCheck, 3, 0, 1, 1);

        startEdit = new QDateTimeEdit(TrackWidget);
        startEdit->setObjectName(QStringLiteral("startEdit"));

        gridLayout->addWidget(startEdit, 3, 1, 1, 2);

        TZCheck = new QCheckBox(TrackWidget);
        TZCheck->setObjectName(QStringLiteral("TZCheck"));

        gridLayout->addWidget(TZCheck, 3, 3, 2, 1);

        stopCheck = new QCheckBox(TrackWidget);
        stopCheck->setObjectName(QStringLiteral("stopCheck"));
        sizePolicy.setHeightForWidth(stopCheck->sizePolicy().hasHeightForWidth());
        stopCheck->setSizePolicy(sizePolicy);

        gridLayout->addWidget(stopCheck, 4, 0, 1, 1);

        stopEdit = new QDateTimeEdit(TrackWidget);
        stopEdit->setObjectName(QStringLiteral("stopEdit"));

        gridLayout->addWidget(stopEdit, 4, 1, 1, 2);

        packCheck = new QCheckBox(TrackWidget);
        packCheck->setObjectName(QStringLiteral("packCheck"));

        gridLayout->addWidget(packCheck, 5, 0, 1, 1);

        mergeCheck = new QCheckBox(TrackWidget);
        mergeCheck->setObjectName(QStringLiteral("mergeCheck"));

        gridLayout->addWidget(mergeCheck, 5, 1, 1, 1);

        splitDateCheck = new QRadioButton(TrackWidget);
        splitDateCheck->setObjectName(QStringLiteral("splitDateCheck"));

        gridLayout->addWidget(splitDateCheck, 6, 0, 1, 2);

        splitTimeCheck = new QRadioButton(TrackWidget);
        splitTimeCheck->setObjectName(QStringLiteral("splitTimeCheck"));
        splitTimeCheck->setAutoExclusive(false);

        gridLayout->addWidget(splitTimeCheck, 6, 2, 1, 1);

        horizontalLayout_6 = new QHBoxLayout();
        horizontalLayout_6->setObjectName(QStringLiteral("horizontalLayout_6"));
        splitTimeSpin = new QSpinBox(TrackWidget);
        splitTimeSpin->setObjectName(QStringLiteral("splitTimeSpin"));
        splitTimeSpin->setMaximum(500);

        horizontalLayout_6->addWidget(splitTimeSpin);

        splitTimeCombo = new QComboBox(TrackWidget);
        splitTimeCombo->setObjectName(QStringLiteral("splitTimeCombo"));

        horizontalLayout_6->addWidget(splitTimeCombo);


        gridLayout->addLayout(horizontalLayout_6, 6, 3, 1, 1);

        splitDistanceCheck = new QRadioButton(TrackWidget);
        splitDistanceCheck->setObjectName(QStringLiteral("splitDistanceCheck"));
        splitDistanceCheck->setAutoExclusive(false);

        gridLayout->addWidget(splitDistanceCheck, 6, 4, 1, 1);

        horizontalLayout_7 = new QHBoxLayout();
        horizontalLayout_7->setObjectName(QStringLiteral("horizontalLayout_7"));
        splitDistSpin = new QSpinBox(TrackWidget);
        splitDistSpin->setObjectName(QStringLiteral("splitDistSpin"));
        splitDistSpin->setMaximum(5280);

        horizontalLayout_7->addWidget(splitDistSpin);

        splitDistCombo = new QComboBox(TrackWidget);
        splitDistCombo->setObjectName(QStringLiteral("splitDistCombo"));

        horizontalLayout_7->addWidget(splitDistCombo);


        gridLayout->addLayout(horizontalLayout_7, 6, 5, 1, 1);

        horizontalLayout_8 = new QHBoxLayout();
        horizontalLayout_8->setObjectName(QStringLiteral("horizontalLayout_8"));
        GPSFixesCheck = new QCheckBox(TrackWidget);
        GPSFixesCheck->setObjectName(QStringLiteral("GPSFixesCheck"));
        sizePolicy.setHeightForWidth(GPSFixesCheck->sizePolicy().hasHeightForWidth());
        GPSFixesCheck->setSizePolicy(sizePolicy);

        horizontalLayout_8->addWidget(GPSFixesCheck);

        GPSFixesCombo = new QComboBox(TrackWidget);
        GPSFixesCombo->setObjectName(QStringLiteral("GPSFixesCombo"));

        horizontalLayout_8->addWidget(GPSFixesCombo);

        horizontalSpacer_2 = new QSpacerItem(2, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_8->addItem(horizontalSpacer_2);


        gridLayout->addLayout(horizontalLayout_8, 7, 0, 1, 2);

        speedCheck = new QCheckBox(TrackWidget);
        speedCheck->setObjectName(QStringLiteral("speedCheck"));

        gridLayout->addWidget(speedCheck, 7, 4, 1, 1);

        courseCheck = new QCheckBox(TrackWidget);
        courseCheck->setObjectName(QStringLiteral("courseCheck"));

        gridLayout->addWidget(courseCheck, 7, 5, 1, 1);


        gridLayout_2->addLayout(gridLayout, 0, 0, 1, 1);

        horizontalSpacer = new QSpacerItem(1, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout_2->addItem(horizontalSpacer, 0, 1, 1, 1);

        verticalSpacer = new QSpacerItem(10, 2, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout_2->addItem(verticalSpacer, 1, 0, 1, 1);


        retranslateUi(TrackWidget);

        QMetaObject::connectSlotsByName(TrackWidget);
    } // setupUi

    void retranslateUi(QWidget *TrackWidget)
    {
        TrackWidget->setWindowTitle(QApplication::translate("TrackWidget", "Track Filter Options", Q_NULLPTR));
#ifndef QT_NO_WHATSTHIS
        TrackWidget->setWhatsThis(QApplication::translate("TrackWidget", "This filter performs various operations on track data. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
#ifndef QT_NO_WHATSTHIS
        label->setWhatsThis(QString());
#endif // QT_NO_WHATSTHIS
        label->setText(QApplication::translate("TrackWidget", "Track Filters", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        titleCheck->setToolTip(QApplication::translate("TrackWidget", "Basic title for track", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        titleCheck->setWhatsThis(QApplication::translate("TrackWidget", "Basic title for new track(s). \n"
"This option specifies a title for tracks generated by the track filter. By default, the title of the new track is composed of the start time of the track appended to this value. \n"
"If this value contains a percent (%) character, it is treated as a format string for the POSIX strftime function, allowing custom time-based track names. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        titleCheck->setText(QApplication::translate("TrackWidget", "Title", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        titleText->setToolTip(QApplication::translate("TrackWidget", "the title of the new track is composed of the start time of the track appended to this value. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        moveCheck->setToolTip(QApplication::translate("TrackWidget", "Correct track point timestamps by specified amount", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        moveCheck->setWhatsThis(QApplication::translate("TrackWidget", "Correct trackpoint timestamps by a delta. \n"
"This option changes the time of all trackpoints. This might be useful if your track must be moved by one or more hours because of an incorrect time zone. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        moveCheck->setText(QApplication::translate("TrackWidget", "Move", Q_NULLPTR));
        daysLabel->setText(QApplication::translate("TrackWidget", "days", Q_NULLPTR));
        hoursLabel->setText(QApplication::translate("TrackWidget", "hours", Q_NULLPTR));
        minsLabel->setText(QApplication::translate("TrackWidget", "mins", Q_NULLPTR));
        secsLabel->setText(QApplication::translate("TrackWidget", "secs", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        startCheck->setToolTip(QApplication::translate("TrackWidget", "Use track pts. after this time. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        startCheck->setWhatsThis(QApplication::translate("TrackWidget", "Use only track points after this timestamp.\n"
"\n"
"This option is used along with the stop to discard trackpoints that were recorded outside of a specific period of time. This option specifies the beginning of the time period. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        startCheck->setText(QApplication::translate("TrackWidget", "Start", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        startEdit->setToolTip(QApplication::translate("TrackWidget", "Use track pts. after this time. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        TZCheck->setToolTip(QApplication::translate("TrackWidget", "If checked, time specified here is based on this computer's current time zone. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        TZCheck->setWhatsThis(QApplication::translate("TrackWidget", "If checked, the times specified here are based on the local computer's time zone.  Otherwise it is UTC.", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        TZCheck->setText(QApplication::translate("TrackWidget", "Local Time", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        stopCheck->setToolTip(QApplication::translate("TrackWidget", "Use track pts before this time. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        stopCheck->setWhatsThis(QApplication::translate("TrackWidget", " Use only track points before this timestamp.\n"
"\n"
"This option is used in conjunction with the start option to discard all trackpoints outside of a given period of time. This option defines the end of the time period. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        stopCheck->setText(QApplication::translate("TrackWidget", "Stop", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        stopEdit->setToolTip(QApplication::translate("TrackWidget", "Use track pts before this time. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        packCheck->setToolTip(QApplication::translate("TrackWidget", "Pack all tracks into one. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        packCheck->setWhatsThis(QApplication::translate("TrackWidget", "Pack all tracks into one.\n"
"\n"
"This option causes all tracks to be appended to one another to form a single track. This option does not work if any two tracks overlap in time; in that case, consider using the merge option.\n"
"\n"
"This option is most useful for rejoining tracks that might have been interrupted by an equipment malfunction or an overnight stop. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        packCheck->setText(QApplication::translate("TrackWidget", "Pack", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        mergeCheck->setToolTip(QApplication::translate("TrackWidget", "Merge multiple tracks for the same way.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mergeCheck->setWhatsThis(QApplication::translate("TrackWidget", " Merge multiple tracks for the same way.\n"
"\n"
"This option puts all track points from all tracks into a single track and sorts them by time stamp. Points with identical time stamps will be dropped. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        mergeCheck->setText(QApplication::translate("TrackWidget", "Merge", Q_NULLPTR));
        splitDateCheck->setText(QApplication::translate("TrackWidget", "Split by Date", Q_NULLPTR));
        splitTimeCheck->setText(QApplication::translate("TrackWidget", "Split by Time", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        splitTimeSpin->setToolTip(QApplication::translate("TrackWidget", "If nonzero, the track will be split if the time between two points is greater than this parameter.   If zero, the track will be split by date. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        splitTimeCombo->clear();
        splitTimeCombo->insertItems(0, QStringList()
         << QApplication::translate("TrackWidget", "mins", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "hrs", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "days", Q_NULLPTR)
        );
        splitDistanceCheck->setText(QApplication::translate("TrackWidget", "Split by Dist.", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        splitDistSpin->setToolTip(QApplication::translate("TrackWidget", "If nonzero, the input track will be split into several tracks if the distance between successive track points is greater than the distance given as a parameter. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        splitDistCombo->clear();
        splitDistCombo->insertItems(0, QStringList()
         << QApplication::translate("TrackWidget", "ft", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "m", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "km", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "mi", Q_NULLPTR)
        );
        GPSFixesCheck->setText(QApplication::translate("TrackWidget", "GPS Fixes", Q_NULLPTR));
        GPSFixesCombo->clear();
        GPSFixesCombo->insertItems(0, QStringList()
         << QApplication::translate("TrackWidget", "none", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "pps", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "dgps", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "3d", Q_NULLPTR)
         << QApplication::translate("TrackWidget", "2d", Q_NULLPTR)
        );
#ifndef QT_NO_TOOLTIP
        speedCheck->setToolTip(QApplication::translate("TrackWidget", "Synthesize speed. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        speedCheck->setWhatsThis(QApplication::translate("TrackWidget", "Synthesize speed.\n"
"\n"
"This option computes a value for the GPS speed at each trackpoint. This is most useful with trackpoints from formats that don't support speed information or for trackoints synthesized by the interpolate filter. The speed at each trackpoint is the average speed from the previous trackpoint (distance divided by time). The first trackpoint in each track is assigned a speed of \"unknown.\" ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        speedCheck->setText(QApplication::translate("TrackWidget", "Speed", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        courseCheck->setToolTip(QApplication::translate("TrackWidget", "Synthesize course.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        courseCheck->setWhatsThis(QApplication::translate("TrackWidget", "Synthesize course.\n"
"\n"
"This option computes (or recomputes) a value for the GPS heading at each trackpoint. This is most useful with trackpoints from formats that don't support heading information or for trackpoints synthesized by the interpolate filter. The heading at each trackpoint is simply the course from the previous trackpoint in the track. The first trackpoint in each track is arbitrarily assigned a heading of 0 degrees. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        courseCheck->setText(QApplication::translate("TrackWidget", "Course", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class TrackWidget: public Ui_TrackWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_TRACKUI_H
