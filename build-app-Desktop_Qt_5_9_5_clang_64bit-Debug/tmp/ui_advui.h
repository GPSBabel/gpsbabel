/********************************************************************************
** Form generated from reading UI file 'advui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ADVUI_H
#define UI_ADVUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_AdvUi
{
public:
    QVBoxLayout *verticalLayout;
    QCheckBox *synthShortNames;
    QCheckBox *enableCharSetXform;
    QCheckBox *previewGmap;
    QHBoxLayout *horizontalLayout;
    QComboBox *debugCombo;
    QLabel *label;
    QSpacerItem *horizontalSpacer;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *formatButton;
    QSpacerItem *horizontalSpacer_2;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *AdvUi)
    {
        if (AdvUi->objectName().isEmpty())
            AdvUi->setObjectName(QStringLiteral("AdvUi"));
        AdvUi->resize(280, 185);
        verticalLayout = new QVBoxLayout(AdvUi);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        synthShortNames = new QCheckBox(AdvUi);
        synthShortNames->setObjectName(QStringLiteral("synthShortNames"));

        verticalLayout->addWidget(synthShortNames);

        enableCharSetXform = new QCheckBox(AdvUi);
        enableCharSetXform->setObjectName(QStringLiteral("enableCharSetXform"));
        enableCharSetXform->setEnabled(true);

        verticalLayout->addWidget(enableCharSetXform);

        previewGmap = new QCheckBox(AdvUi);
        previewGmap->setObjectName(QStringLiteral("previewGmap"));

        verticalLayout->addWidget(previewGmap);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        debugCombo = new QComboBox(AdvUi);
        debugCombo->setObjectName(QStringLiteral("debugCombo"));

        horizontalLayout->addWidget(debugCombo);

        label = new QLabel(AdvUi);
        label->setObjectName(QStringLiteral("label"));

        horizontalLayout->addWidget(label);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QStringLiteral("horizontalLayout_2"));
        formatButton = new QPushButton(AdvUi);
        formatButton->setObjectName(QStringLiteral("formatButton"));

        horizontalLayout_2->addWidget(formatButton);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout_2);

        buttonBox = new QDialogButtonBox(AdvUi);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonBox);


        retranslateUi(AdvUi);

        QMetaObject::connectSlotsByName(AdvUi);
    } // setupUi

    void retranslateUi(QDialog *AdvUi)
    {
        AdvUi->setWindowTitle(QApplication::translate("AdvUi", "Global Options", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        synthShortNames->setToolTip(QApplication::translate("AdvUi", "Create smart shortened names. ", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        synthShortNames->setText(QApplication::translate("AdvUi", "Synthesize short names", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        enableCharSetXform->setToolTip(QApplication::translate("AdvUi", "Convert character set encoding between input and output", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        enableCharSetXform->setText(QApplication::translate("AdvUi", "Enable character set transformation", Q_NULLPTR));
        previewGmap->setText(QApplication::translate("AdvUi", "Preview in Google Maps", Q_NULLPTR));
        debugCombo->clear();
        debugCombo->insertItems(0, QStringList()
         << QApplication::translate("AdvUi", "None", Q_NULLPTR)
         << QApplication::translate("AdvUi", "0", Q_NULLPTR)
         << QApplication::translate("AdvUi", "1", Q_NULLPTR)
         << QApplication::translate("AdvUi", "2", Q_NULLPTR)
         << QApplication::translate("AdvUi", "3", Q_NULLPTR)
         << QApplication::translate("AdvUi", "4", Q_NULLPTR)
         << QApplication::translate("AdvUi", "5", Q_NULLPTR)
         << QApplication::translate("AdvUi", "6", Q_NULLPTR)
         << QApplication::translate("AdvUi", "7", Q_NULLPTR)
         << QApplication::translate("AdvUi", "8", Q_NULLPTR)
         << QApplication::translate("AdvUi", "9", Q_NULLPTR)
        );
#ifndef QT_NO_TOOLTIP
        debugCombo->setToolTip(QApplication::translate("AdvUi", "Debugging diagnostics.  \n"
"Higher number provides more deitaled diagnostics.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        label->setText(QApplication::translate("AdvUi", "Debugging Diagnostics", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        formatButton->setToolTip(QApplication::translate("AdvUi", "Set all format input/output options to default values", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        formatButton->setText(QApplication::translate("AdvUi", "Default Format Options", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class AdvUi: public Ui_AdvUi {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ADVUI_H
