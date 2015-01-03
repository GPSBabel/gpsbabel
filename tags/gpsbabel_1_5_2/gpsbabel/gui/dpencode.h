// -*- C++ -*-
// $Id: dpencode.h,v 1.2 2009-09-08 16:06:32 robertl Exp $
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
#ifndef DPENCODE_H
#define DPENCODE_H

#include <vector>
#include <string>
#include "latlng.h"
using std::vector;
using std::string;


class PolylineEncoder {
public:
  PolylineEncoder(int numLevels=19, double zoomFactor=2.0, double verySmall = 0.00001);
  void dpEncode(string &encPts, string &encLevels, const vector<LatLng> &points);
  
private:

  int computeLevel(double dd);
  double distance(const LatLng &, const LatLng &, const LatLng &);
  void encodeLevels(string &, const vector<LatLng> &points, const vector<double>&dists);
  void createEncodings(string &encoded_points, 
		       const vector <LatLng> &points, 
		       const vector <double> &dists);

  int numLevels;
  double verySmall;
  vector <double> zoomLevelBreaks;
};

#endif
