/********************************************************************************
** Form generated from reading UI file 'filterui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_FILTERUI_H
#define UI_FILTERUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_FilterDlg
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QListWidget *filterList;
    QGridLayout *gridLayout;
    QFrame *frame;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *helpButton;
    QPushButton *resetButton;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *FilterDlg)
    {
        if (FilterDlg->objectName().isEmpty())
            FilterDlg->setObjectName(QStringLiteral("FilterDlg"));
        FilterDlg->resize(892, 260);
        verticalLayout = new QVBoxLayout(FilterDlg);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        filterList = new QListWidget(FilterDlg);
        filterList->setObjectName(QStringLiteral("filterList"));
        filterList->setMinimumSize(QSize(140, 0));

        horizontalLayout->addWidget(filterList);

        gridLayout = new QGridLayout();
        gridLayout->setObjectName(QStringLiteral("gridLayout"));
        frame = new QFrame(FilterDlg);
        frame->setObjectName(QStringLiteral("frame"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(100);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(frame->sizePolicy().hasHeightForWidth());
        frame->setSizePolicy(sizePolicy);
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);

        gridLayout->addWidget(frame, 0, 0, 1, 1);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(verticalSpacer, 1, 0, 1, 1);


        horizontalLayout->addLayout(gridLayout);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QStringLiteral("horizontalLayout_2"));
        helpButton = new QPushButton(FilterDlg);
        helpButton->setObjectName(QStringLiteral("helpButton"));
        QIcon icon;
        icon.addFile(QStringLiteral(":/images/help.png"), QSize(), QIcon::Normal, QIcon::Off);
        helpButton->setIcon(icon);

        horizontalLayout_2->addWidget(helpButton);

        resetButton = new QPushButton(FilterDlg);
        resetButton->setObjectName(QStringLiteral("resetButton"));
        QIcon icon1;
        icon1.addFile(QStringLiteral(":/images/reload.png"), QSize(), QIcon::Normal, QIcon::Off);
        resetButton->setIcon(icon1);

        horizontalLayout_2->addWidget(resetButton);

        buttonBox = new QDialogButtonBox(FilterDlg);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        horizontalLayout_2->addWidget(buttonBox);


        verticalLayout->addLayout(horizontalLayout_2);


        retranslateUi(FilterDlg);
        QObject::connect(buttonBox, SIGNAL(accepted()), FilterDlg, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), FilterDlg, SLOT(reject()));

        QMetaObject::connectSlotsByName(FilterDlg);
    } // setupUi

    void retranslateUi(QDialog *FilterDlg)
    {
        FilterDlg->setWindowTitle(QApplication::translate("FilterDlg", "Data Filters", Q_NULLPTR));
#ifndef QT_NO_WHATSTHIS
        FilterDlg->setWhatsThis(QApplication::translate("FilterDlg", "Data filters process and transform the data between input and output files or devices. ", Q_NULLPTR));
#endif // QT_NO_WHATSTHIS
        helpButton->setText(QApplication::translate("FilterDlg", "Help", Q_NULLPTR));
        resetButton->setText(QApplication::translate("FilterDlg", "Reset", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class FilterDlg: public Ui_FilterDlg {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_FILTERUI_H
