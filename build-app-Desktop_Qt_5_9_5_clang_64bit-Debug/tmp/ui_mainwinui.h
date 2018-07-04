/********************************************************************************
** Form generated from reading UI file 'mainwinui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINUI_H
#define UI_MAINWINUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *actionHelp;
    QAction *actionAbout;
    QAction *actionQuit;
    QAction *actionPreferences;
    QAction *actionUpgradeCheck;
    QAction *actionVisit_Website;
    QAction *actionMake_a_Donation;
    QWidget *centralwidget;
    QVBoxLayout *verticalLayout_5;
    QGroupBox *groupBox;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout_5;
    QRadioButton *inputFileOptBtn;
    QRadioButton *inputDeviceOptBtn;
    QSpacerItem *horizontalSpacer_2a;
    QLabel *label;
    QComboBox *inputFormatCombo;
    QStackedWidget *inputStackedWidget;
    QWidget *inputFilePage;
    QHBoxLayout *horizontalLayout_4;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *inputFileNameBrowseBtn;
    QLineEdit *inputFileNameText;
    QWidget *inputDevicePage;
    QVBoxLayout *verticalLayout_2;
    QHBoxLayout *horizontalLayout_3;
    QLabel *label_2;
    QComboBox *inputDeviceNameCombo;
    QSpacerItem *horizontalSpacer_3a;
    QHBoxLayout *horizontalLayout_6;
    QPushButton *inputOptionsBtn;
    QLineEdit *inputOptionsText;
    QGroupBox *groupBox_2;
    QHBoxLayout *horizontalLayout_7;
    QLabel *wayPtLabel;
    QCheckBox *xlateWayPtsCk;
    QFrame *line;
    QSpacerItem *horizontalSpacer_6;
    QLabel *routeLabel;
    QCheckBox *xlateRoutesCk;
    QFrame *line_2;
    QSpacerItem *horizontalSpacer_7;
    QLabel *trackLabel;
    QCheckBox *xlateTracksCk;
    QSpacerItem *horizontalSpacer_4;
    QLabel *filterStatus;
    QPushButton *xlateFiltersBtn;
    QSpacerItem *horizontalSpacer_8;
    QSpacerItem *horizontalSpacer_5;
    QPushButton *moreOptionButton;
    QGroupBox *groupBox_3;
    QVBoxLayout *verticalLayout_3;
    QHBoxLayout *horizontalLayout_8;
    QRadioButton *outputFileOptBtn;
    QRadioButton *outputDeviceOptBtn;
    QSpacerItem *horizontalSpacer_2b;
    QLabel *label_4;
    QComboBox *outputFormatCombo;
    QStackedWidget *outputStackedWidget;
    QWidget *outputFilePage;
    QHBoxLayout *horizontalLayout_9;
    QHBoxLayout *horizontalLayout_10;
    QPushButton *outputFileNameBrowseBtn;
    QLineEdit *outputFileNameText;
    QWidget *outputDevicePage;
    QVBoxLayout *verticalLayout_4;
    QHBoxLayout *horizontalLayout_11;
    QLabel *label_6;
    QComboBox *outputDeviceNameCombo;
    QSpacerItem *horizontalSpacer_3b;
    QHBoxLayout *horizontalLayout_12;
    QPushButton *outputOptionsBtn;
    QLineEdit *outputOptionsText;
    QPlainTextEdit *outputWindow;
    QDialogButtonBox *buttonBox;
    QMenuBar *menubar;
    QMenu *menuFile;
    QMenu *menuHelp;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QStringLiteral("MainWindow"));
        MainWindow->resize(675, 582);
        actionHelp = new QAction(MainWindow);
        actionHelp->setObjectName(QStringLiteral("actionHelp"));
        actionAbout = new QAction(MainWindow);
        actionAbout->setObjectName(QStringLiteral("actionAbout"));
        actionQuit = new QAction(MainWindow);
        actionQuit->setObjectName(QStringLiteral("actionQuit"));
        actionPreferences = new QAction(MainWindow);
        actionPreferences->setObjectName(QStringLiteral("actionPreferences"));
        actionUpgradeCheck = new QAction(MainWindow);
        actionUpgradeCheck->setObjectName(QStringLiteral("actionUpgradeCheck"));
        actionVisit_Website = new QAction(MainWindow);
        actionVisit_Website->setObjectName(QStringLiteral("actionVisit_Website"));
        actionMake_a_Donation = new QAction(MainWindow);
        actionMake_a_Donation->setObjectName(QStringLiteral("actionMake_a_Donation"));
        centralwidget = new QWidget(MainWindow);
        centralwidget->setObjectName(QStringLiteral("centralwidget"));
        verticalLayout_5 = new QVBoxLayout(centralwidget);
        verticalLayout_5->setObjectName(QStringLiteral("verticalLayout_5"));
        groupBox = new QGroupBox(centralwidget);
        groupBox->setObjectName(QStringLiteral("groupBox"));
        verticalLayout = new QVBoxLayout(groupBox);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        verticalLayout->setContentsMargins(-1, 4, -1, 4);
        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setObjectName(QStringLiteral("horizontalLayout_5"));
        inputFileOptBtn = new QRadioButton(groupBox);
        inputFileOptBtn->setObjectName(QStringLiteral("inputFileOptBtn"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(inputFileOptBtn->sizePolicy().hasHeightForWidth());
        inputFileOptBtn->setSizePolicy(sizePolicy);

        horizontalLayout_5->addWidget(inputFileOptBtn);

        inputDeviceOptBtn = new QRadioButton(groupBox);
        inputDeviceOptBtn->setObjectName(QStringLiteral("inputDeviceOptBtn"));
        sizePolicy.setHeightForWidth(inputDeviceOptBtn->sizePolicy().hasHeightForWidth());
        inputDeviceOptBtn->setSizePolicy(sizePolicy);

        horizontalLayout_5->addWidget(inputDeviceOptBtn);

        horizontalSpacer_2a = new QSpacerItem(18, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer_2a);

        label = new QLabel(groupBox);
        label->setObjectName(QStringLiteral("label"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label->sizePolicy().hasHeightForWidth());
        label->setSizePolicy(sizePolicy1);

        horizontalLayout_5->addWidget(label);

        inputFormatCombo = new QComboBox(groupBox);
        inputFormatCombo->setObjectName(QStringLiteral("inputFormatCombo"));

        horizontalLayout_5->addWidget(inputFormatCombo);


        verticalLayout->addLayout(horizontalLayout_5);

        inputStackedWidget = new QStackedWidget(groupBox);
        inputStackedWidget->setObjectName(QStringLiteral("inputStackedWidget"));
        inputStackedWidget->setLineWidth(0);
        inputFilePage = new QWidget();
        inputFilePage->setObjectName(QStringLiteral("inputFilePage"));
        horizontalLayout_4 = new QHBoxLayout(inputFilePage);
        horizontalLayout_4->setContentsMargins(0, 0, 0, 0);
        horizontalLayout_4->setObjectName(QStringLiteral("horizontalLayout_4"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QStringLiteral("horizontalLayout_2"));
        inputFileNameBrowseBtn = new QPushButton(inputFilePage);
        inputFileNameBrowseBtn->setObjectName(QStringLiteral("inputFileNameBrowseBtn"));
        QIcon icon;
        icon.addFile(QStringLiteral(":/images/open.png"), QSize(), QIcon::Normal, QIcon::Off);
        inputFileNameBrowseBtn->setIcon(icon);

        horizontalLayout_2->addWidget(inputFileNameBrowseBtn);

        inputFileNameText = new QLineEdit(inputFilePage);
        inputFileNameText->setObjectName(QStringLiteral("inputFileNameText"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(10);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(inputFileNameText->sizePolicy().hasHeightForWidth());
        inputFileNameText->setSizePolicy(sizePolicy2);

        horizontalLayout_2->addWidget(inputFileNameText);


        horizontalLayout_4->addLayout(horizontalLayout_2);

        inputStackedWidget->addWidget(inputFilePage);
        inputDevicePage = new QWidget();
        inputDevicePage->setObjectName(QStringLiteral("inputDevicePage"));
        verticalLayout_2 = new QVBoxLayout(inputDevicePage);
        verticalLayout_2->setContentsMargins(0, 0, 0, 0);
        verticalLayout_2->setObjectName(QStringLiteral("verticalLayout_2"));
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setObjectName(QStringLiteral("horizontalLayout_3"));
        label_2 = new QLabel(inputDevicePage);
        label_2->setObjectName(QStringLiteral("label_2"));

        horizontalLayout_3->addWidget(label_2);

        inputDeviceNameCombo = new QComboBox(inputDevicePage);
        inputDeviceNameCombo->setObjectName(QStringLiteral("inputDeviceNameCombo"));
        inputDeviceNameCombo->setSizeAdjustPolicy(QComboBox::AdjustToContents);

        horizontalLayout_3->addWidget(inputDeviceNameCombo);

        horizontalSpacer_3a = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_3a);


        verticalLayout_2->addLayout(horizontalLayout_3);

        inputStackedWidget->addWidget(inputDevicePage);

        verticalLayout->addWidget(inputStackedWidget);

        horizontalLayout_6 = new QHBoxLayout();
        horizontalLayout_6->setObjectName(QStringLiteral("horizontalLayout_6"));
        inputOptionsBtn = new QPushButton(groupBox);
        inputOptionsBtn->setObjectName(QStringLiteral("inputOptionsBtn"));

        horizontalLayout_6->addWidget(inputOptionsBtn);

        inputOptionsText = new QLineEdit(groupBox);
        inputOptionsText->setObjectName(QStringLiteral("inputOptionsText"));
        inputOptionsText->setEnabled(true);
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(inputOptionsText->sizePolicy().hasHeightForWidth());
        inputOptionsText->setSizePolicy(sizePolicy3);

        horizontalLayout_6->addWidget(inputOptionsText);


        verticalLayout->addLayout(horizontalLayout_6);


        verticalLayout_5->addWidget(groupBox);

        groupBox_2 = new QGroupBox(centralwidget);
        groupBox_2->setObjectName(QStringLiteral("groupBox_2"));
        horizontalLayout_7 = new QHBoxLayout(groupBox_2);
        horizontalLayout_7->setContentsMargins(4, 4, 4, 4);
        horizontalLayout_7->setObjectName(QStringLiteral("horizontalLayout_7"));
        wayPtLabel = new QLabel(groupBox_2);
        wayPtLabel->setObjectName(QStringLiteral("wayPtLabel"));
        wayPtLabel->setScaledContents(false);

        horizontalLayout_7->addWidget(wayPtLabel);

        xlateWayPtsCk = new QCheckBox(groupBox_2);
        xlateWayPtsCk->setObjectName(QStringLiteral("xlateWayPtsCk"));

        horizontalLayout_7->addWidget(xlateWayPtsCk);

        line = new QFrame(groupBox_2);
        line->setObjectName(QStringLiteral("line"));
        line->setFrameShape(QFrame::VLine);
        line->setFrameShadow(QFrame::Sunken);

        horizontalLayout_7->addWidget(line);

        horizontalSpacer_6 = new QSpacerItem(15, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_7->addItem(horizontalSpacer_6);

        routeLabel = new QLabel(groupBox_2);
        routeLabel->setObjectName(QStringLiteral("routeLabel"));

        horizontalLayout_7->addWidget(routeLabel);

        xlateRoutesCk = new QCheckBox(groupBox_2);
        xlateRoutesCk->setObjectName(QStringLiteral("xlateRoutesCk"));

        horizontalLayout_7->addWidget(xlateRoutesCk);

        line_2 = new QFrame(groupBox_2);
        line_2->setObjectName(QStringLiteral("line_2"));
        line_2->setFrameShape(QFrame::VLine);
        line_2->setFrameShadow(QFrame::Sunken);

        horizontalLayout_7->addWidget(line_2);

        horizontalSpacer_7 = new QSpacerItem(15, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_7->addItem(horizontalSpacer_7);

        trackLabel = new QLabel(groupBox_2);
        trackLabel->setObjectName(QStringLiteral("trackLabel"));

        horizontalLayout_7->addWidget(trackLabel);

        xlateTracksCk = new QCheckBox(groupBox_2);
        xlateTracksCk->setObjectName(QStringLiteral("xlateTracksCk"));

        horizontalLayout_7->addWidget(xlateTracksCk);

        horizontalSpacer_4 = new QSpacerItem(18, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_7->addItem(horizontalSpacer_4);

        filterStatus = new QLabel(groupBox_2);
        filterStatus->setObjectName(QStringLiteral("filterStatus"));
        filterStatus->setPixmap(QPixmap(QString::fromUtf8(":/images/ok20.png")));
        filterStatus->setScaledContents(false);

        horizontalLayout_7->addWidget(filterStatus);

        xlateFiltersBtn = new QPushButton(groupBox_2);
        xlateFiltersBtn->setObjectName(QStringLiteral("xlateFiltersBtn"));

        horizontalLayout_7->addWidget(xlateFiltersBtn);

        horizontalSpacer_8 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_7->addItem(horizontalSpacer_8);

        horizontalSpacer_5 = new QSpacerItem(244, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_7->addItem(horizontalSpacer_5);

        moreOptionButton = new QPushButton(groupBox_2);
        moreOptionButton->setObjectName(QStringLiteral("moreOptionButton"));

        horizontalLayout_7->addWidget(moreOptionButton);


        verticalLayout_5->addWidget(groupBox_2);

        groupBox_3 = new QGroupBox(centralwidget);
        groupBox_3->setObjectName(QStringLiteral("groupBox_3"));
        verticalLayout_3 = new QVBoxLayout(groupBox_3);
        verticalLayout_3->setObjectName(QStringLiteral("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(-1, 4, -1, 4);
        horizontalLayout_8 = new QHBoxLayout();
        horizontalLayout_8->setObjectName(QStringLiteral("horizontalLayout_8"));
        outputFileOptBtn = new QRadioButton(groupBox_3);
        outputFileOptBtn->setObjectName(QStringLiteral("outputFileOptBtn"));
        sizePolicy.setHeightForWidth(outputFileOptBtn->sizePolicy().hasHeightForWidth());
        outputFileOptBtn->setSizePolicy(sizePolicy);

        horizontalLayout_8->addWidget(outputFileOptBtn);

        outputDeviceOptBtn = new QRadioButton(groupBox_3);
        outputDeviceOptBtn->setObjectName(QStringLiteral("outputDeviceOptBtn"));
        sizePolicy.setHeightForWidth(outputDeviceOptBtn->sizePolicy().hasHeightForWidth());
        outputDeviceOptBtn->setSizePolicy(sizePolicy);

        horizontalLayout_8->addWidget(outputDeviceOptBtn);

        horizontalSpacer_2b = new QSpacerItem(18, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_8->addItem(horizontalSpacer_2b);

        label_4 = new QLabel(groupBox_3);
        label_4->setObjectName(QStringLiteral("label_4"));
        sizePolicy1.setHeightForWidth(label_4->sizePolicy().hasHeightForWidth());
        label_4->setSizePolicy(sizePolicy1);

        horizontalLayout_8->addWidget(label_4);

        outputFormatCombo = new QComboBox(groupBox_3);
        outputFormatCombo->setObjectName(QStringLiteral("outputFormatCombo"));

        horizontalLayout_8->addWidget(outputFormatCombo);


        verticalLayout_3->addLayout(horizontalLayout_8);

        outputStackedWidget = new QStackedWidget(groupBox_3);
        outputStackedWidget->setObjectName(QStringLiteral("outputStackedWidget"));
        outputFilePage = new QWidget();
        outputFilePage->setObjectName(QStringLiteral("outputFilePage"));
        horizontalLayout_9 = new QHBoxLayout(outputFilePage);
        horizontalLayout_9->setContentsMargins(0, 0, 0, 0);
        horizontalLayout_9->setObjectName(QStringLiteral("horizontalLayout_9"));
        horizontalLayout_10 = new QHBoxLayout();
        horizontalLayout_10->setObjectName(QStringLiteral("horizontalLayout_10"));
        outputFileNameBrowseBtn = new QPushButton(outputFilePage);
        outputFileNameBrowseBtn->setObjectName(QStringLiteral("outputFileNameBrowseBtn"));
        QIcon icon1;
        icon1.addFile(QStringLiteral(":/images/save.png"), QSize(), QIcon::Normal, QIcon::Off);
        outputFileNameBrowseBtn->setIcon(icon1);

        horizontalLayout_10->addWidget(outputFileNameBrowseBtn);

        outputFileNameText = new QLineEdit(outputFilePage);
        outputFileNameText->setObjectName(QStringLiteral("outputFileNameText"));
        sizePolicy2.setHeightForWidth(outputFileNameText->sizePolicy().hasHeightForWidth());
        outputFileNameText->setSizePolicy(sizePolicy2);

        horizontalLayout_10->addWidget(outputFileNameText);


        horizontalLayout_9->addLayout(horizontalLayout_10);

        outputStackedWidget->addWidget(outputFilePage);
        outputDevicePage = new QWidget();
        outputDevicePage->setObjectName(QStringLiteral("outputDevicePage"));
        verticalLayout_4 = new QVBoxLayout(outputDevicePage);
        verticalLayout_4->setContentsMargins(0, 0, 0, 0);
        verticalLayout_4->setObjectName(QStringLiteral("verticalLayout_4"));
        horizontalLayout_11 = new QHBoxLayout();
        horizontalLayout_11->setObjectName(QStringLiteral("horizontalLayout_11"));
        label_6 = new QLabel(outputDevicePage);
        label_6->setObjectName(QStringLiteral("label_6"));

        horizontalLayout_11->addWidget(label_6);

        outputDeviceNameCombo = new QComboBox(outputDevicePage);
        outputDeviceNameCombo->setObjectName(QStringLiteral("outputDeviceNameCombo"));
        outputDeviceNameCombo->setSizeAdjustPolicy(QComboBox::AdjustToContents);

        horizontalLayout_11->addWidget(outputDeviceNameCombo);

        horizontalSpacer_3b = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_11->addItem(horizontalSpacer_3b);


        verticalLayout_4->addLayout(horizontalLayout_11);

        outputStackedWidget->addWidget(outputDevicePage);

        verticalLayout_3->addWidget(outputStackedWidget);

        horizontalLayout_12 = new QHBoxLayout();
        horizontalLayout_12->setObjectName(QStringLiteral("horizontalLayout_12"));
        outputOptionsBtn = new QPushButton(groupBox_3);
        outputOptionsBtn->setObjectName(QStringLiteral("outputOptionsBtn"));

        horizontalLayout_12->addWidget(outputOptionsBtn);

        outputOptionsText = new QLineEdit(groupBox_3);
        outputOptionsText->setObjectName(QStringLiteral("outputOptionsText"));

        horizontalLayout_12->addWidget(outputOptionsText);


        verticalLayout_3->addLayout(horizontalLayout_12);


        verticalLayout_5->addWidget(groupBox_3);

        outputWindow = new QPlainTextEdit(centralwidget);
        outputWindow->setObjectName(QStringLiteral("outputWindow"));
        QSizePolicy sizePolicy4(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy4.setHorizontalStretch(10);
        sizePolicy4.setVerticalStretch(10);
        sizePolicy4.setHeightForWidth(outputWindow->sizePolicy().hasHeightForWidth());
        outputWindow->setSizePolicy(sizePolicy4);

        verticalLayout_5->addWidget(outputWindow);

        buttonBox = new QDialogButtonBox(centralwidget);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setStandardButtons(QDialogButtonBox::Close|QDialogButtonBox::Help|QDialogButtonBox::Ok);

        verticalLayout_5->addWidget(buttonBox);

        MainWindow->setCentralWidget(centralwidget);
        menubar = new QMenuBar(MainWindow);
        menubar->setObjectName(QStringLiteral("menubar"));
        menubar->setGeometry(QRect(0, 0, 675, 22));
        menuFile = new QMenu(menubar);
        menuFile->setObjectName(QStringLiteral("menuFile"));
        menuHelp = new QMenu(menubar);
        menuHelp->setObjectName(QStringLiteral("menuHelp"));
        MainWindow->setMenuBar(menubar);
        statusbar = new QStatusBar(MainWindow);
        statusbar->setObjectName(QStringLiteral("statusbar"));
        MainWindow->setStatusBar(statusbar);

        menubar->addAction(menuFile->menuAction());
        menubar->addAction(menuHelp->menuAction());
        menuFile->addAction(actionPreferences);
        menuFile->addAction(actionQuit);
        menuHelp->addAction(actionHelp);
        menuHelp->addSeparator();
        menuHelp->addAction(actionAbout);
        menuHelp->addAction(actionUpgradeCheck);
        menuHelp->addSeparator();
        menuHelp->addAction(actionVisit_Website);
        menuHelp->addAction(actionMake_a_Donation);

        retranslateUi(MainWindow);

        inputStackedWidget->setCurrentIndex(1);
        outputStackedWidget->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "GPSBabel", Q_NULLPTR));
        actionHelp->setText(QApplication::translate("MainWindow", "GPSBabel Help", Q_NULLPTR));
        actionAbout->setText(QApplication::translate("MainWindow", "About GPSBabel", Q_NULLPTR));
        actionQuit->setText(QApplication::translate("MainWindow", "Quit", Q_NULLPTR));
        actionPreferences->setText(QApplication::translate("MainWindow", "Preferences...", Q_NULLPTR));
        actionUpgradeCheck->setText(QApplication::translate("MainWindow", "Check for Upgrade", Q_NULLPTR));
        actionVisit_Website->setText(QApplication::translate("MainWindow", "Visit Website...", Q_NULLPTR));
        actionMake_a_Donation->setText(QApplication::translate("MainWindow", "Make a Donation...", Q_NULLPTR));
        groupBox->setTitle(QApplication::translate("MainWindow", "Input ", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        inputFileOptBtn->setToolTip(QApplication::translate("MainWindow", "If selected, input is from a file.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        inputFileOptBtn->setText(QApplication::translate("MainWindow", "File", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        inputDeviceOptBtn->setToolTip(QApplication::translate("MainWindow", "If selected, input is from a device or GPS unit", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        inputDeviceOptBtn->setText(QApplication::translate("MainWindow", "Device", Q_NULLPTR));
        label->setText(QApplication::translate("MainWindow", "Format", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        inputFormatCombo->setToolTip(QApplication::translate("MainWindow", "Input data format", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        inputFileNameBrowseBtn->setToolTip(QApplication::translate("MainWindow", "Browse for one or more input files. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        inputFileNameBrowseBtn->setText(QApplication::translate("MainWindow", "File Name(s)", Q_NULLPTR));
        label_2->setText(QApplication::translate("MainWindow", "Device Name:", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        inputDeviceNameCombo->setToolTip(QApplication::translate("MainWindow", "Name of port to which input device is connected", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        inputOptionsBtn->setToolTip(QApplication::translate("MainWindow", "Options for the selected input format. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        inputOptionsBtn->setText(QApplication::translate("MainWindow", "Options", Q_NULLPTR));
        groupBox_2->setTitle(QApplication::translate("MainWindow", "Translation Options", Q_NULLPTR));
        wayPtLabel->setText(QApplication::translate("MainWindow", "-", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        xlateWayPtsCk->setToolTip(QApplication::translate("MainWindow", "If selected, translate waypoints.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        xlateWayPtsCk->setText(QApplication::translate("MainWindow", "Waypoints", Q_NULLPTR));
        routeLabel->setText(QApplication::translate("MainWindow", "-", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        xlateRoutesCk->setToolTip(QApplication::translate("MainWindow", "If selected, translate routes.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        xlateRoutesCk->setText(QApplication::translate("MainWindow", "Routes", Q_NULLPTR));
        trackLabel->setText(QApplication::translate("MainWindow", "-", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        xlateTracksCk->setToolTip(QApplication::translate("MainWindow", "If selected, translate tracks.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        xlateTracksCk->setText(QApplication::translate("MainWindow", "Tracks", Q_NULLPTR));
        filterStatus->setText(QString());
#ifndef QT_NO_TOOLTIP
        xlateFiltersBtn->setToolTip(QApplication::translate("MainWindow", "Data Filters between input and output", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        xlateFiltersBtn->setText(QApplication::translate("MainWindow", "Filters", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        moreOptionButton->setToolTip(QApplication::translate("MainWindow", "More translation options. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        moreOptionButton->setText(QApplication::translate("MainWindow", "More Options", Q_NULLPTR));
        groupBox_3->setTitle(QApplication::translate("MainWindow", "Output", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        outputFileOptBtn->setToolTip(QApplication::translate("MainWindow", "If selected, output is to a file. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        outputFileOptBtn->setText(QApplication::translate("MainWindow", "File", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        outputDeviceOptBtn->setToolTip(QApplication::translate("MainWindow", "If selected, output is to a device or GPS unit", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        outputDeviceOptBtn->setText(QApplication::translate("MainWindow", "Device", Q_NULLPTR));
        label_4->setText(QApplication::translate("MainWindow", "Format", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        outputFormatCombo->setToolTip(QApplication::translate("MainWindow", "Output data format.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        outputFileNameBrowseBtn->setToolTip(QApplication::translate("MainWindow", "Browse for an output file name. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        outputFileNameBrowseBtn->setText(QApplication::translate("MainWindow", "File Name", Q_NULLPTR));
        label_6->setText(QApplication::translate("MainWindow", "Device Name:", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        outputDeviceNameCombo->setToolTip(QApplication::translate("MainWindow", "Name of port to which output device is connected", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        outputOptionsBtn->setToolTip(QApplication::translate("MainWindow", "Options for the selected output format. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        outputOptionsBtn->setText(QApplication::translate("MainWindow", "Options", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        outputWindow->setToolTip(QApplication::translate("MainWindow", "Output of GPSBabel translation process. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        buttonBox->setToolTip(QString());
#endif // QT_NO_TOOLTIP
        menuFile->setTitle(QApplication::translate("MainWindow", "File", Q_NULLPTR));
        menuHelp->setTitle(QApplication::translate("MainWindow", "Help", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINUI_H
