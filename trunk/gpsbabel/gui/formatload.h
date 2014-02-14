// -*- c++ -*-
// $Id: formatload.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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


#ifndef FORMATLOAD_H
#define FORMATLOAD_H

#include "format.h"

class FormatLoad {
 public:
  FormatLoad() : currentLine_(0) {};
  ~FormatLoad() {};
  
  bool getFormats(QList<Format> &formatList);
 private:
  QStringList lines_;
  int currentLine_;
  bool skipToValidLine();
  bool processFormat(Format &format);
};

#endif
