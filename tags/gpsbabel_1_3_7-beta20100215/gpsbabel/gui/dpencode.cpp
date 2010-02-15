// -*- C++ -*-
// $Id: dpencode.cpp,v 1.3 2009-09-08 16:06:32 robertl Exp $
//------------------------------------------------------------------------
//
//  Original in JavaScript: 
//  PolylineEncoder.js copyright Mark McClure  April/May 2007
//
//  Translated to C++
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
#include <stack>
#include <math.h>
#include "dpencode.h"

using std::vector;
using std::stack;
using std::string;



//------------------------------------------------------------------------
PolylineEncoder::PolylineEncoder(int numLevels, double zoomFactor, double vs):
  numLevels(numLevels), zoomFactor(zoomFactor), verySmall(vs)
{
  if (verySmall <=0.0)
    verySmall = 1.0E-5;
  for(int i = 0; i < numLevels; i++)
    zoomLevelBreaks.push_back(verySmall*pow(zoomFactor, numLevels-i-1));
}

//------------------------------------------------------------------------
class IntervalPair
{
public:
  IntervalPair(int i0, int i1): i0(i0), i1(i1) {
  }
  int  i0, i1;
};

//------------------------------------------------------------------------
static double hypotenuse(double a, double b){
  return sqrt(a*a + b*b);
}
//------------------------------------------------------------------------
static double hdist(const LatLng &a, const LatLng &b) {
  return hypotenuse(a.lat()-b.lat(), a.lng()-b.lng());
}

//------------------------------------------------------------------------
static string encodeNumber(int num) 
{
  string encodeString = "";
  while (num >= 0x20) {
    encodeString += char((0x20 | (num & 0x1f)) + 63);
    num = num >> 5;
  }
  encodeString += char(num + 63);
  return encodeString;
}

//------------------------------------------------------------------------
// This one is Google's verbatim.
static string encodeSignedNumber (int num) 
{
  int sgn_num = num << 1;
  if (num < 0) {
    sgn_num = ~(sgn_num);
  }
  return(encodeNumber(sgn_num));
}

//------------------------------------------------------------------------
int roundToInt(double x) {
  return (x>0.0) ? int(x+0.5) : int(x-0.5);
}
//------------------------------------------------------------------------
void PolylineEncoder::createEncodings(string &encoded_points,
				      const vector <LatLng> &points,
				      const vector <double> &dists)
{
  encoded_points = "";;
  int plat = 0;
  int plng = 0;
  for(unsigned int i = 0; i < points.size(); i++) {
    if(dists[i] >= 0.0 || i == 0 || i == points.size()-1) {
      int late5 = roundToInt(points[i].lat() * 1e5);
      int lnge5 = roundToInt(points[i].lng() * 1e5);
      int dlat = late5 - plat;
      int dlng = lnge5 - plng;
      encoded_points += encodeSignedNumber(dlat) + encodeSignedNumber(dlng);
      plat = late5;
      plng = lnge5;
    }
  }
}

//------------------------------------------------------------------------
// This computes the appropriate zoom level of a point in terms of it's
// distance from the relevant segment in the DP algorithm.  Could be done
// in terms of a logarithm, but this approach makes it a bit easier to
// ensure that the level is not too large.
int PolylineEncoder::computeLevel(double dd)
{
  int lev = 0;
  if(dd > verySmall) {
    while(dd < zoomLevelBreaks[lev]) {
      lev++;
    }
  }
  return lev;
}
//------------------------------------------------------------------------
// Now we can use the previous function to march down the list
// of points and encode the levels.  Like createEncodings, we
// ignore points whose distance (in dists) is undefined.
void PolylineEncoder::encodeLevels(string &encoded_levels, const vector<LatLng> &points, const vector<double>&dists)
{
  encoded_levels = "";
  encoded_levels += encodeNumber(numLevels-1);
  for (unsigned int i=1; i<points.size()-1; i++) {
    if(dists[i] >= 0.0) {
      encoded_levels += encodeNumber(numLevels-computeLevel(dists[i])-1);
    }
  }
  encoded_levels += encodeNumber(numLevels-1);
}


//------------------------------------------------------------------------
double PolylineEncoder::distance(const LatLng &p0, const LatLng &p1, const LatLng &p2) 
{
  double out = 0.0;
  if (p1.lat() == p2.lat() && p1.lng() == p2.lng()) {
    out = hdist(p2, p0);
  }
  else {
    double dlat = (p2.lat()-p1.lat());
    double dlng = (p2.lng()-p1.lng());
    double u = ((p0.lat()-p1.lat())*dlat+(p0.lng()-p1.lng())*dlng)/(dlat*dlat + dlng*dlng);
  
    if (u <= 0) {
      out = hdist(p0, p1);
    }
    else if(u >= 1) {
      out = hdist(p0, p2);
    }
    else {
      out = hdist(p0, LatLng(p1.lat() + u*dlat, p1.lng() + u*dlng));
    }
  }
  return out;
}

//------------------------------------------------------------------------
void PolylineEncoder::dpEncode(string &encPts, string &encLevels, const vector<LatLng> &points)
{
  if (points.size() < 2) {
    encPts = encLevels = "";  // no solution here.
    return;

  }
  stack <IntervalPair> stk;
  vector <double>  dists(points.size(), -1.0);
  
  stk.push(IntervalPair(0, int(points.size())-1));
  while (!stk.empty()) {
    
    IntervalPair current = stk.top();
    stk.pop();
    
    double maxDist = -1.0;
    int maxLoc = -1;
    for (int i=current.i0+1; i<current.i1; i++) {
      double dist = this->distance(points[i], points[current.i0], points[current.i1]);
      if(dist > maxDist) {
	maxDist = dist;
	maxLoc = i;
      }
    }
    if(maxDist > this->verySmall) {
      dists[maxLoc] = maxDist;
      stk.push(IntervalPair(current.i0, maxLoc));
      stk.push(IntervalPair(maxLoc, current.i1));
    }
  }
  createEncodings(encPts, points, dists);
  encodeLevels(encLevels, points, dists);
}
