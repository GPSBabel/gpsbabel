/********************************************************************************
** Form generated from reading UI file 'version_mismatch.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_VERSION_MISMATCH_H
#define UI_VERSION_MISMATCH_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_VersionMismatch
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *label;
    QHBoxLayout *horizontalLayout;
    QLabel *label_2;
    QLabel *ClVersion;
    QSpacerItem *horizontalSpacer;
    QHBoxLayout *horizontalLayout_2;
    QLabel *label_3;
    QLabel *GuiVersion;
    QSpacerItem *horizontalSpacer_2;
    QHBoxLayout *horizontalLayout_3;
    QCheckBox *neverAgain;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *VersionMismatch)
    {
        if (VersionMismatch->objectName().isEmpty())
            VersionMismatch->setObjectName(QStringLiteral("VersionMismatch"));
        VersionMismatch->resize(346, 144);
        QSizePolicy sizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(VersionMismatch->sizePolicy().hasHeightForWidth());
        VersionMismatch->setSizePolicy(sizePolicy);
        VersionMismatch->setMinimumSize(QSize(0, 0));
        VersionMismatch->setMaximumSize(QSize(400, 300));
        verticalLayout = new QVBoxLayout(VersionMismatch);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        label = new QLabel(VersionMismatch);
        label->setObjectName(QStringLiteral("label"));

        verticalLayout->addWidget(label);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        label_2 = new QLabel(VersionMismatch);
        label_2->setObjectName(QStringLiteral("label_2"));

        horizontalLayout->addWidget(label_2);

        ClVersion = new QLabel(VersionMismatch);
        ClVersion->setObjectName(QStringLiteral("ClVersion"));
#ifndef QT_NO_TOOLTIP
        ClVersion->setToolTip(QStringLiteral(""));
#endif // QT_NO_TOOLTIP

        horizontalLayout->addWidget(ClVersion);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QStringLiteral("horizontalLayout_2"));
        label_3 = new QLabel(VersionMismatch);
        label_3->setObjectName(QStringLiteral("label_3"));

        horizontalLayout_2->addWidget(label_3);

        GuiVersion = new QLabel(VersionMismatch);
        GuiVersion->setObjectName(QStringLiteral("GuiVersion"));

        horizontalLayout_2->addWidget(GuiVersion);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout_2);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setObjectName(QStringLiteral("horizontalLayout_3"));
        neverAgain = new QCheckBox(VersionMismatch);
        neverAgain->setObjectName(QStringLiteral("neverAgain"));

        horizontalLayout_3->addWidget(neverAgain);

        buttonBox = new QDialogButtonBox(VersionMismatch);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Ignore);

        horizontalLayout_3->addWidget(buttonBox);


        verticalLayout->addLayout(horizontalLayout_3);


        retranslateUi(VersionMismatch);
        QObject::connect(buttonBox, SIGNAL(accepted()), VersionMismatch, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), VersionMismatch, SLOT(reject()));

        QMetaObject::connectSlotsByName(VersionMismatch);
    } // setupUi

    void retranslateUi(QDialog *VersionMismatch)
    {
        VersionMismatch->setWindowTitle(QApplication::translate("VersionMismatch", "GPSBabel Version Mismatch", Q_NULLPTR));
        label->setText(QApplication::translate("VersionMismatch", "<b>A version mismatch has been detected.</b>", Q_NULLPTR));
        label_2->setText(QApplication::translate("VersionMismatch", "GPSBabel command line version:", Q_NULLPTR));
        ClVersion->setText(QString());
        label_3->setText(QApplication::translate("VersionMismatch", "GPSBabel GUI version:", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        GuiVersion->setToolTip(QString());
#endif // QT_NO_TOOLTIP
        GuiVersion->setText(QString());
        neverAgain->setText(QApplication::translate("VersionMismatch", "Never show this message again.", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class VersionMismatch: public Ui_VersionMismatch {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_VERSION_MISMATCH_H
