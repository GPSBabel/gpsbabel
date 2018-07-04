/********************************************************************************
** Form generated from reading UI file 'gmapui.ui'
**
** Created by: Qt User Interface Compiler version 5.9.5
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_GMAPUI_H
#define UI_GMAPUI_H

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
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QSplitter>
#include <QtWidgets/QTreeView>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_GMapDlg
{
public:
    QGridLayout *gridLayout;
    QVBoxLayout *verticalLayout_2;
    QSplitter *splitter;
    QWidget *widget;
    QVBoxLayout *verticalLayout;
    QTreeView *treeView;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *copyButton;
    QSpacerItem *horizontalSpacer_2;
    QFrame *frame;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *GMapDlg)
    {
        if (GMapDlg->objectName().isEmpty())
            GMapDlg->setObjectName(QStringLiteral("GMapDlg"));
        GMapDlg->resize(879, 668);
        gridLayout = new QGridLayout(GMapDlg);
        gridLayout->setObjectName(QStringLiteral("gridLayout"));
        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setObjectName(QStringLiteral("verticalLayout_2"));
        splitter = new QSplitter(GMapDlg);
        splitter->setObjectName(QStringLiteral("splitter"));
        splitter->setOrientation(Qt::Horizontal);
        widget = new QWidget(splitter);
        widget->setObjectName(QStringLiteral("widget"));
        verticalLayout = new QVBoxLayout(widget);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        treeView = new QTreeView(widget);
        treeView->setObjectName(QStringLiteral("treeView"));

        verticalLayout->addWidget(treeView);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        copyButton = new QPushButton(widget);
        copyButton->setObjectName(QStringLiteral("copyButton"));

        horizontalLayout->addWidget(copyButton);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout);

        splitter->addWidget(widget);
        frame = new QFrame(splitter);
        frame->setObjectName(QStringLiteral("frame"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(10);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(frame->sizePolicy().hasHeightForWidth());
        frame->setSizePolicy(sizePolicy);
        frame->setMinimumSize(QSize(20, 20));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        splitter->addWidget(frame);

        verticalLayout_2->addWidget(splitter);

        buttonBox = new QDialogButtonBox(GMapDlg);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Close);

        verticalLayout_2->addWidget(buttonBox);


        gridLayout->addLayout(verticalLayout_2, 0, 0, 1, 1);


        retranslateUi(GMapDlg);
        QObject::connect(buttonBox, SIGNAL(accepted()), GMapDlg, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), GMapDlg, SLOT(reject()));

        QMetaObject::connectSlotsByName(GMapDlg);
    } // setupUi

    void retranslateUi(QDialog *GMapDlg)
    {
        GMapDlg->setWindowTitle(QApplication::translate("GMapDlg", "Dialog", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        copyButton->setToolTip(QApplication::translate("GMapDlg", "Copy to Clipboard", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        copyButton->setText(QApplication::translate("GMapDlg", "Copy", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class GMapDlg: public Ui_GMapDlg {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_GMAPUI_H
