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
static void SetSizeStuff(QWidget *w)
{
  QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
  sizePolicy.setHorizontalStretch(0);
  sizePolicy.setVerticalStretch(0);
  sizePolicy.setHeightForWidth(w->sizePolicy().hasHeightForWidth());
  w->setSizePolicy(sizePolicy);
}

//------------------------------------------------------------------------
FileDlgManager::FileDlgManager(QObject*parent,
			       QLineEdit *le,
			       QToolButton *tb, bool isInFile):
  QObject(parent), le(le), tb(tb), isInFile(isInFile)
{
  connect(tb, SIGNAL(clicked()), this, SLOT(buttonClicked()));
}

//------------------------------------------------------------------------
QVariant getOptionValue(QList<FormatOption> opts, int k) {
  if (opts[k].getValue().toString() != "")
    return opts[k].getValue();
  else
    return opts[k].getDefaultValue();
}

//------------------------------------------------------------------------
FileDlgManager::~FileDlgManager()
{
}
//------------------------------------------------------------------------
void FileDlgManager::buttonClicked()
{
  QString str;
  if (isInFile) {
    str = QFileDialog::getOpenFileName(0, tr("Select input file"),
				       le->text(),
				       "All Files (*.*)");
  }
  else {
    str = QFileDialog::getSaveFileName(0, tr("Select output file"),
				       le->text(),
				       "All Files (*.*)");
  }
  if (str != "")
    le->setText(str);
}

//------------------------------------------------------------------------
OptionsDlg::OptionsDlg(QWidget*parent,  const QString &fmtName, QList<FormatOption> *opts,
		       const QString &html):
  QDialog(parent),
  fmtName(fmtName),
  options(*opts),
  html(html)
{

  QVBoxLayout *verticalLayout = new QVBoxLayout(this);
  for (int k=0; k<options.size(); k++) {
    QHBoxLayout *horizontalLayout = new QHBoxLayout();

    QCheckBox *checkBox = new QCheckBox(this);
    checkBox->setText(tr(options[k].getDescription().toAscii().data()));
    horizontalLayout->addWidget(checkBox);
    checkBox->setChecked(options[k].getSelected());
    //checkBox->setWhatsThis(options[k].getHtml());

    QSpacerItem *horizontalSpacer = new QSpacerItem(0, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    horizontalLayout->addItem(horizontalSpacer);

    QWidget *w = 0;
    switch (options[k].getType())
      {
      case FormatOption::OPTstring:
	{
	  QLineEdit *lineEdit = new QLineEdit(this);
	  SetSizeStuff(lineEdit);
	  lineEdit->setText(getOptionValue(options, k).toString());
	  w = lineEdit;
	  horizontalLayout->addWidget(lineEdit);
	}
	break;

      case FormatOption::OPTinFile:
      case FormatOption::OPToutFile:
	{
	  bool inFile = options[k].getType() == FormatOption::OPTinFile;
	  QLineEdit *lineEdit = new QLineEdit(this);
	  QToolButton *button = new QToolButton(this);
	  lineEdit->setText(getOptionValue(options, k).toString());
	  button->setIcon(QIcon(inFile ? ":images/file.png" : ":images/save.png" ));
	  w = lineEdit;
	  horizontalSpacer->changeSize(5, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);
	  horizontalLayout->addWidget(lineEdit);
	  horizontalLayout->addWidget(button);
	  (void) new FileDlgManager(this, lineEdit, button, inFile);
	}
	break;

      case FormatOption::OPTbool:
        // This is quirky.  It means that GPSBabel's bool options that default
        // to true get turned on here, even if user turned them off on last
        // exit.
	checkBox->setChecked(getOptionValue(options,k).toBool());
	w = 0;
	break;

      case FormatOption::OPTfloat:
	{
	  QLineEdit *lineEdit = new QLineEdit(this);
	  SetSizeStuff(lineEdit);
	  lineEdit->setText(getOptionValue(options, k).toString());
	  w = lineEdit;
	  QDoubleValidator *v = new QDoubleValidator(this);
	  v->setRange(options[k].getMinValue().toDouble(),
		      options[k].getMaxValue().toDouble());
	  lineEdit->setValidator(v);
	  horizontalLayout->addWidget(lineEdit);
	}
	break;

      case FormatOption::OPTint:
	{
	  QLineEdit *lineEdit = new QLineEdit(this);
	  SetSizeStuff(lineEdit);
	  w = lineEdit;
	  QIntValidator *iv = new QIntValidator(this);
	  iv->setRange(options[k].getMinValue().toInt(),
		       options[k].getMaxValue().toInt());
	  lineEdit->setValidator(iv);
	  lineEdit->setText(getOptionValue(options, k).toString());
	  horizontalLayout->addWidget(lineEdit);
	}
	break;
	
      case FormatOption::OPTboundedInt:
	{
	  QSpinBox *spinBox = new QSpinBox(this);
	  spinBox->setRange(options[k].getMinValue().toInt(),
			    options[k].getMaxValue().toInt());
	  spinBox->setValue(getOptionValue(options, k).toInt());
	  SetSizeStuff(spinBox);
	  w = spinBox;
	  horizontalLayout->addWidget(spinBox);
	}
	break;
      }
    checkBoxes.push_back(checkBox);
    fields.push_back(w);

    verticalLayout->addLayout(horizontalLayout);
  }
  QPushButton *helpButton = new QPushButton(this);
  helpButton->setIcon(QIcon(":/images/help.png"));
  helpButton->setText(tr("Help"));

  QHBoxLayout *lay = new QHBoxLayout();
  lay->addWidget(helpButton);

  buttonBox = new QDialogButtonBox(this);
  buttonBox->setOrientation(Qt::Horizontal);
  buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
  lay->addWidget(buttonBox);
  verticalLayout->addLayout(lay);
  buttonBox->button(QDialogButtonBox::Ok)->setIcon(QIcon(":images/ok"));
  buttonBox->button(QDialogButtonBox::Cancel)->setIcon(QIcon(":images/cancel"));

  connect(buttonBox, SIGNAL(accepted()), this, SLOT(acceptClicked()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(rejectClicked()));
  connect(helpButton, SIGNAL(clicked()), this,  SLOT(helpClicked()));
}

//------------------------------------------------------------------------
void OptionsDlg::acceptClicked()
{
  for (int k=0; k<options.size(); k++) {
    options[k].setSelected(checkBoxes[k]->isChecked());
    if (fields[k]) {
      if (options[k].getType() == FormatOption::OPTboundedInt) {
	int value = static_cast<QSpinBox*>(fields[k])->value();
	value = qMax(qMin(value, options[k].getMaxValue().toInt()),options[k].getMinValue().toInt());
	options[k].setValue(QVariant(value));
      }
      else if (options[k].getType() == FormatOption::OPTint) {
	int value = static_cast<QLineEdit*>(fields[k])->text().toInt();
	value = qMax(qMin(value, options[k].getMaxValue().toInt()),options[k].getMinValue().toInt());
	options[k].setValue(QVariant(value));
      }
      else if (options[k].getType() == FormatOption::OPTfloat) {
	double value = static_cast<QLineEdit*>(fields[k])->text().toDouble();
	value = qMax(qMin(value, options[k].getMaxValue().toDouble()),options[k].getMinValue().toDouble());
	options[k].setValue(QVariant(value));
      }
      else
	options[k].setValue(static_cast<QLineEdit*>(fields[k])->text());
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
  ShowHelp(html);
}
