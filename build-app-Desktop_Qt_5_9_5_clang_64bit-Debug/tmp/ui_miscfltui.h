/********************************************************************************
** Form generated from reading UI file 'miscfltui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MISCFLTUI_H
#define UI_MISCFLTUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MiscFltWidget
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *label;
    QGroupBox *groupBox;
    QHBoxLayout *horizontalLayout_2;
    QCheckBox *nukeRoutes;
    QCheckBox *nukeTracks;
    QCheckBox *nukeWaypoints;
    QHBoxLayout *horizontalLayout;
    QCheckBox *transformCheck;
    QComboBox *transformCombo;
    QCheckBox *deleteCheck;
    QSpacerItem *horizontalSpacer;
    QCheckBox *swapCheck;
    QSpacerItem *verticalSpacer;

    void setupUi(QWidget *MiscFltWidget)
    {
        if (MiscFltWidget->objectName().isEmpty())
            MiscFltWidget->setObjectName(QStringLiteral("MiscFltWidget"));
        MiscFltWidget->resize(303, 175);
        verticalLayout = new QVBoxLayout(MiscFltWidget);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        label = new QLabel(MiscFltWidget);
        label->setObjectName(QStringLiteral("label"));
        QFont font;
        font.setPointSize(11);
        font.setBold(true);
        font.setWeight(75);
        label->setFont(font);

        verticalLayout->addWidget(label);

        groupBox = new QGroupBox(MiscFltWidget);
        groupBox->setObjectName(QStringLiteral("groupBox"));
        horizontalLayout_2 = new QHBoxLayout(groupBox);
        horizontalLayout_2->setObjectName(QStringLiteral("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 4, -1, 4);
        nukeRoutes = new QCheckBox(groupBox);
        nukeRoutes->setObjectName(QStringLiteral("nukeRoutes"));

        horizontalLayout_2->addWidget(nukeRoutes);

        nukeTracks = new QCheckBox(groupBox);
        nukeTracks->setObjectName(QStringLiteral("nukeTracks"));

        horizontalLayout_2->addWidget(nukeTracks);

        nukeWaypoints = new QCheckBox(groupBox);
        nukeWaypoints->setObjectName(QStringLiteral("nukeWaypoints"));

        horizontalLayout_2->addWidget(nukeWaypoints);


        verticalLayout->addWidget(groupBox);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        transformCheck = new QCheckBox(MiscFltWidget);
        transformCheck->setObjectName(QStringLiteral("transformCheck"));

        horizontalLayout->addWidget(transformCheck);

        transformCombo = new QComboBox(MiscFltWidget);
        transformCombo->setObjectName(QStringLiteral("transformCombo"));

        horizontalLayout->addWidget(transformCombo);

        deleteCheck = new QCheckBox(MiscFltWidget);
        deleteCheck->setObjectName(QStringLiteral("deleteCheck"));

        horizontalLayout->addWidget(deleteCheck);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout);

        swapCheck = new QCheckBox(MiscFltWidget);
        swapCheck->setObjectName(QStringLiteral("swapCheck"));

        verticalLayout->addWidget(swapCheck);

        verticalSpacer = new QSpacerItem(20, 1, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);


        retranslateUi(MiscFltWidget);

        QMetaObject::connectSlotsByName(MiscFltWidget);
    } // setupUi

    void retranslateUi(QWidget *MiscFltWidget)
    {
        MiscFltWidget->setWindowTitle(QApplication::translate("MiscFltWidget", "Form", Q_NULLPTR));
        label->setText(QApplication::translate("MiscFltWidget", "Misc. Filters", Q_NULLPTR));
        groupBox->setTitle(QApplication::translate("MiscFltWidget", "Nuke (Remove) Data Types", Q_NULLPTR));
        nukeRoutes->setText(QApplication::translate("MiscFltWidget", "Routes", Q_NULLPTR));
        nukeTracks->setText(QApplication::translate("MiscFltWidget", "Tracks", Q_NULLPTR));
        nukeWaypoints->setText(QApplication::translate("MiscFltWidget", "Waypoints", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        transformCheck->setToolTip(QApplication::translate("MiscFltWidget", "Convert routes, waypoints and tracks to different types.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        transformCheck->setWhatsThis(QApplication::translate("MiscFltWidget", "This filter can be used to convert GPS data between different data types.\n"
"\n"
"Some GPS data formats support only some subset of waypoints, tracks, and routes. The transform filter allows you to convert between these types. For example, it can be used to convert a pile of waypoints (such as those from a CSV file) into a track or vice versa. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        transformCheck->setText(QApplication::translate("MiscFltWidget", "Transform", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        transformCombo->setToolTip(QApplication::translate("MiscFltWidget", "Type of transformation. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        deleteCheck->setToolTip(QApplication::translate("MiscFltWidget", "Delete original data after transform to prevent duplicated data. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        deleteCheck->setText(QApplication::translate("MiscFltWidget", "Delete", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        swapCheck->setToolTip(QApplication::translate("MiscFltWidget", "Swap Longitude and Latitudes for badly formatted data formats.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        swapCheck->setWhatsThis(QApplication::translate("MiscFltWidget", "Simple filter to swap the coordinate values (latitude and longitude) of all points. This can be helpful for wrong defined/coded data. Or if you think, you can use one of our xcsv formats, but latitude and longitude are in opposite order. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        swapCheck->setText(QApplication::translate("MiscFltWidget", "Swap Coordinates", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class MiscFltWidget: public Ui_MiscFltWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MISCFLTUI_H
