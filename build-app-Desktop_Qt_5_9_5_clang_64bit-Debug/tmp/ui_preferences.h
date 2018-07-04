/********************************************************************************
** Form generated from reading UI file 'preferences.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PREFERENCES_H
#define UI_PREFERENCES_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Preferences
{
public:
    QFormLayout *formLayout;
    QTabWidget *tabWidget;
    QWidget *general_tab;
    QWidget *layoutWidget;
    QVBoxLayout *verticalLayout_4;
    QCheckBox *startupCheck;
    QCheckBox *reportStatisticsCheck;
    QCheckBox *ignoreVersionMismatchCheck;
    QWidget *format_tab;
    QHBoxLayout *horizontalLayout;
    QVBoxLayout *verticalLayout;
    QLabel *label;
    QListWidget *enabledFormatsList;
    QVBoxLayout *verticalLayout_3;
    QPushButton *enableAllButton;
    QPushButton *disableAllButton;
    QDialogButtonBox *buttonBox;
    QSpacerItem *horizontalSpacer;

    void setupUi(QDialog *Preferences)
    {
        if (Preferences->objectName().isEmpty())
            Preferences->setObjectName(QStringLiteral("Preferences"));
        Preferences->resize(494, 327);
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(Preferences->sizePolicy().hasHeightForWidth());
        Preferences->setSizePolicy(sizePolicy);
        formLayout = new QFormLayout(Preferences);
        formLayout->setObjectName(QStringLiteral("formLayout"));
        tabWidget = new QTabWidget(Preferences);
        tabWidget->setObjectName(QStringLiteral("tabWidget"));
        general_tab = new QWidget();
        general_tab->setObjectName(QStringLiteral("general_tab"));
        layoutWidget = new QWidget(general_tab);
        layoutWidget->setObjectName(QStringLiteral("layoutWidget"));
        layoutWidget->setGeometry(QRect(23, 18, 387, 62));
        verticalLayout_4 = new QVBoxLayout(layoutWidget);
        verticalLayout_4->setObjectName(QStringLiteral("verticalLayout_4"));
        verticalLayout_4->setContentsMargins(0, 0, 0, 0);
        startupCheck = new QCheckBox(layoutWidget);
        startupCheck->setObjectName(QStringLiteral("startupCheck"));

        verticalLayout_4->addWidget(startupCheck);

        reportStatisticsCheck = new QCheckBox(layoutWidget);
        reportStatisticsCheck->setObjectName(QStringLiteral("reportStatisticsCheck"));

        verticalLayout_4->addWidget(reportStatisticsCheck);

        ignoreVersionMismatchCheck = new QCheckBox(layoutWidget);
        ignoreVersionMismatchCheck->setObjectName(QStringLiteral("ignoreVersionMismatchCheck"));

        verticalLayout_4->addWidget(ignoreVersionMismatchCheck);

        tabWidget->addTab(general_tab, QString());
        format_tab = new QWidget();
        format_tab->setObjectName(QStringLiteral("format_tab"));
        horizontalLayout = new QHBoxLayout(format_tab);
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        label = new QLabel(format_tab);
        label->setObjectName(QStringLiteral("label"));

        verticalLayout->addWidget(label);

        enabledFormatsList = new QListWidget(format_tab);
        enabledFormatsList->setObjectName(QStringLiteral("enabledFormatsList"));
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(enabledFormatsList->sizePolicy().hasHeightForWidth());
        enabledFormatsList->setSizePolicy(sizePolicy1);
        enabledFormatsList->setSortingEnabled(false);

        verticalLayout->addWidget(enabledFormatsList);


        horizontalLayout->addLayout(verticalLayout);

        verticalLayout_3 = new QVBoxLayout();
        verticalLayout_3->setObjectName(QStringLiteral("verticalLayout_3"));
        enableAllButton = new QPushButton(format_tab);
        enableAllButton->setObjectName(QStringLiteral("enableAllButton"));

        verticalLayout_3->addWidget(enableAllButton);

        disableAllButton = new QPushButton(format_tab);
        disableAllButton->setObjectName(QStringLiteral("disableAllButton"));

        verticalLayout_3->addWidget(disableAllButton);


        horizontalLayout->addLayout(verticalLayout_3);

        tabWidget->addTab(format_tab, QString());

        formLayout->setWidget(0, QFormLayout::LabelRole, tabWidget);

        buttonBox = new QDialogButtonBox(Preferences);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        formLayout->setWidget(1, QFormLayout::LabelRole, buttonBox);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        formLayout->setItem(0, QFormLayout::FieldRole, horizontalSpacer);


        retranslateUi(Preferences);

        tabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(Preferences);
    } // setupUi

    void retranslateUi(QDialog *Preferences)
    {
        Preferences->setWindowTitle(QApplication::translate("Preferences", "GPSBabel Preferences", Q_NULLPTR));
        startupCheck->setText(QApplication::translate("Preferences", "Check for newer version on start.", Q_NULLPTR));
        reportStatisticsCheck->setText(QApplication::translate("Preferences", "Anonymously report usage data.", Q_NULLPTR));
        ignoreVersionMismatchCheck->setText(QApplication::translate("Preferences", "Ignore mismatch between command line and GUI version.", Q_NULLPTR));
        tabWidget->setTabText(tabWidget->indexOf(general_tab), QApplication::translate("Preferences", "General", Q_NULLPTR));
        label->setText(QApplication::translate("Preferences", "Enabled Formats", Q_NULLPTR));
        enableAllButton->setText(QApplication::translate("Preferences", "Enable All", Q_NULLPTR));
        disableAllButton->setText(QApplication::translate("Preferences", "Disable All", Q_NULLPTR));
        tabWidget->setTabText(tabWidget->indexOf(format_tab), QApplication::translate("Preferences", "Formats", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class Preferences: public Ui_Preferences {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PREFERENCES_H
