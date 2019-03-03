// -*- C++ -*-
// $Id: optionsdlg.cpp,v 1.5 2010-03-01 04:22:28 robertl Exp $
//------------------------------------------------------------------------
//
//  Copyright (C) 2009  S. Khai Mong <khai@mangrai.com>.
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------

#include <QVBoxLayout>
#include <QDoubleValidator>
#include <QIntValidator>
#include <QPushButton>
#include <QFileDialog>
#include <QIcon>
#include <QSpinBox>
#include "optionsdlg.h"
#include "help.h"

//------------------------------------------------------------------------
static void SetSizeStuff(QWidget* w)
{
  QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
  sizePolicy.setHorizontalStretch(0);
  sizePolicy.setVerticalStretch(0);
  sizePolicy.setHeightForWidth(w->sizePolicy().hasHeightForWidth());
  w->setSizePolicy(sizePolicy);
}

//------------------------------------------------------------------------
FileDlgManager::FileDlgManager(QObject* parent,
                               QLineEdit* le,
                               QToolButton* tb, bool isInFile):
  QObject(parent), le(le), tb(tb), isInFile(isInFile)
{
  connect(tb, SIGNAL(clicked()), this, SLOT(buttonClicked()));
}

//------------------------------------------------------------------------
QVariant getOptionValue(QList<FormatOption> opts, int k)
{
  if (opts[k].getValue().toString() != "") {
    return opts[k].getValue();
  }
  return opts[k].getDefaultValue();

}

//------------------------------------------------------------------------
FileDlgManager::~FileDlgManager()
  = default;

//------------------------------------------------------------------------
void FileDlgManager::buttonClicked()
{
  QString str;
  if (isInFile) {
    str = QFileDialog::getOpenFileName(nullptr, tr("Select input file"),
                                       le->text(),
                                       "All Files (*.*)");
  } else {
    str = QFileDialog::getSaveFileName(nullptr, tr("Select output file"),
                                       le->text(),
                                       "All Files (*.*)");
  }
  if (str != "") {
    le->setText(str);
  }
}

//------------------------------------------------------------------------
OptionsDlg::OptionsDlg(QWidget* parent,  const QString& fmtName, QList<FormatOption>* opts,
                       const QString& htmlArg):
  QDialog(parent),
  fmtName_(fmtName),
  options_(*opts),
  html_(htmlArg)
{
  if (htmlArg.isEmpty()) {
    html_ = "fmt_" + fmtName + ".html";
  }
  QVBoxLayout* verticalLayout = new QVBoxLayout(this);
  for (int k=0; k<options_.size(); k++) {
    QHBoxLayout* horizontalLayout = new QHBoxLayout();

    QCheckBox* checkBox = new QCheckBox(this);
    checkBox->setText(options_[k].getDescription());
    horizontalLayout->addWidget(checkBox);
    checkBox->setChecked(options_[k].getSelected());
    //checkBox->setWhatsThis(options[k].getHtml());

    QSpacerItem* horizontalSpacer = new QSpacerItem(0, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    horizontalLayout->addItem(horizontalSpacer);

    QWidget* w = nullptr;
    switch (options_[k].getType()) {
    case FormatOption::OPTstring: {
      QLineEdit* lineEdit = new QLineEdit(this);
      SetSizeStuff(lineEdit);
      lineEdit->setText(getOptionValue(options_, k).toString());
      w = lineEdit;
      horizontalLayout->addWidget(lineEdit);
    }
    break;

    case FormatOption::OPTinFile:
    case FormatOption::OPToutFile: {
      bool inFile = options_[k].getType() == FormatOption::OPTinFile;
      QLineEdit* lineEdit = new QLineEdit(this);
      QToolButton* button = new QToolButton(this);
      lineEdit->setText(getOptionValue(options_, k).toString());
      button->setIcon(QIcon(inFile ? ":images/file.png" : ":images/save.png"));
      w = lineEdit;
      horizontalSpacer->changeSize(5, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);
      horizontalLayout->addWidget(lineEdit);
      horizontalLayout->addWidget(button);
      (void) new FileDlgManager(this, lineEdit, button, inFile);
    }
    break;

    case FormatOption::OPTbool:
      // If it was selected before, select it again.
      checkBox->setChecked(options_[k].getSelected());
      w = nullptr;
      break;

    case FormatOption::OPTfloat: {
      QLineEdit* lineEdit = new QLineEdit(this);
      SetSizeStuff(lineEdit);
      lineEdit->setText(getOptionValue(options_, k).toString());
      w = lineEdit;
      double minVal = options_[k].getMinValue().toDouble();
      double maxVal = options_[k].getMaxValue().toDouble();
      if (minVal < maxVal) {
        QDoubleValidator* v = new QDoubleValidator(this);
        v->setRange(minVal, maxVal);
        lineEdit->setValidator(v);
      }
      horizontalLayout->addWidget(lineEdit);
    }
    break;

    case FormatOption::OPTint: {
      QLineEdit* lineEdit = new QLineEdit(this);
      SetSizeStuff(lineEdit);
      w = lineEdit;
      int minVal = options_[k].getMinValue().toInt();
      int maxVal = options_[k].getMaxValue().toInt();
      if (minVal < maxVal) {
        QIntValidator* iv = new QIntValidator(this);
        iv->setRange(minVal, maxVal);
        lineEdit->setValidator(iv);
      }
      lineEdit->setText(getOptionValue(options_, k).toString());
      horizontalLayout->addWidget(lineEdit);
    }
    break;

    case FormatOption::OPTboundedInt: {
      QSpinBox* spinBox = new QSpinBox(this);
      spinBox->setRange(options_[k].getMinValue().toInt(),
                        options_[k].getMaxValue().toInt());
      spinBox->setValue(getOptionValue(options_, k).toInt());
      SetSizeStuff(spinBox);
      w = spinBox;
      horizontalLayout->addWidget(spinBox);
    }
    break;
    }
    checkBoxes_.push_back(checkBox);
    fields_.push_back(w);
#if LATER
    // 2013-12-30 robertlipe - because we can't pass arguments
    // to slots, this requires QSignalMapper or the new lambda
    // functions added in Qt5.  Not worth it right now, but
    // an idea worth picking up later.
    QPushButton* help = new QPushButton(tr("Help"), this);
    help->setIcon(QIcon(":/images/help.png"));
    help->setProperty("page", options[k].getHtml();)
    connect(help, SIGNAL(clicked()), this,  SLOT(helpClicked()));
    horizontalLayout->addWidget(help);
#endif

    verticalLayout->addLayout(horizontalLayout);
  }
  QPushButton* helpButton = new QPushButton(this);
  helpButton->setIcon(QIcon(":/images/help.png"));
  helpButton->setText(tr("Help"));

  QHBoxLayout* lay = new QHBoxLayout();
  lay->addWidget(helpButton);

  buttonBox_ = new QDialogButtonBox(this);
  buttonBox_->setOrientation(Qt::Horizontal);
  buttonBox_->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
  lay->addWidget(buttonBox_);
  verticalLayout->addLayout(lay);
  buttonBox_->button(QDialogButtonBox::Ok)->setIcon(QIcon(":images/ok"));
  buttonBox_->button(QDialogButtonBox::Cancel)->setIcon(QIcon(":images/cancel"));

  connect(buttonBox_, SIGNAL(accepted()), this, SLOT(acceptClicked()));
  connect(buttonBox_, SIGNAL(rejected()), this, SLOT(rejectClicked()));
  connect(helpButton, SIGNAL(clicked()), this,  SLOT(helpClicked()));
}

//------------------------------------------------------------------------
void OptionsDlg::acceptClicked()
{
  for (int k=0; k<options_.size(); k++) {
    options_[k].setSelected(checkBoxes_[k]->isChecked());
    if (fields_[k] != nullptr) {
      if (options_[k].getType() == FormatOption::OPTboundedInt) {
        int value = static_cast<QSpinBox*>(fields_[k])->value();
        value = qMax(qMin(value, options_[k].getMaxValue().toInt()),options_[k].getMinValue().toInt());
        options_[k].setValue(QVariant(value));
      } else if (options_[k].getType() == FormatOption::OPTint) {
        int value = static_cast<QLineEdit*>(fields_[k])->text().toInt();
        value = qMax(qMin(value, options_[k].getMaxValue().toInt()),options_[k].getMinValue().toInt());
        options_[k].setValue(QVariant(value));
      } else if (options_[k].getType() == FormatOption::OPTfloat) {
        double value = static_cast<QLineEdit*>(fields_[k])->text().toDouble();
        value = qMax(qMin(value, options_[k].getMaxValue().toDouble()),options_[k].getMinValue().toDouble());
        options_[k].setValue(QVariant(value));
      } else {
        options_[k].setValue(static_cast<QLineEdit*>(fields_[k])->text());
      }
    }
  }
  accept();
}

//------------------------------------------------------------------------
void OptionsDlg::rejectClicked()
{
  reject();
}

//------------------------------------------------------------------------
void OptionsDlg::helpClicked()
{
  ShowHelp(html_);
}
