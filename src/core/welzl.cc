/*
    Copyright (C) 2021 Robert Lipe, robertlipe+source@gpsbabel.org

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

    This solves the smallest circle problem using Welzl's algorithm.

    Welzl, Emo (1991), "Smallest enclosing disks (balls and ellipsoids)",
    in Maurer, H. (ed.), New Results and New Trends in Computer Science,
    Lecture Notes in Computer Science, 555, Springer-Verlag, pp. 359–370

 */

#include "src/core/welzl.h"

#include <algorithm>            // for shuffle
#include <cfloat>               // for DBL_EPSILON
#include <random>               // for random_device, mt19937

#include "defs.h"               // for fatal
#include "src/core/nvector.h"   // for NVector
#include "src/core/vector3d.h"  // for Vector3D


namespace gpsbabel
{

Circle Welzl::b_md(PointContainer R)
{
  NVector center;
  double radius;

  switch (R.size()) {

  case 0:
    center = NVector(0.0, 0.0);
    radius = 0.0;
    break;

  case 1:
    center = R.at(0);
    radius = 0.0;
    break;

  case 2:
#if 0
    double dp_a_b = NVector::dotProduct(R.at(0), R.at(1));
    if (dp_a_b <= (-1.0+8.0*DBL_EPSILON)) {
      // The points are so close to be exactly opposite each other that
      // there isn't a unique great circle between them.
      Vector3D result(nan(""), nan(""), nan(""));
      return result;
    }
#endif
    center = (R.at(0) + R.at(1)).normalize();
    radius = NVector::distance(center, R.at(0));
    //qDebug() << "center2 check" << NVector::distance(center, R.at(0)) << NVector::distance(center, R.at(1));
    break;

  case 3: {
    auto n_EA_E = R.at(0);
    auto n_EB_E = R.at(1);
    auto n_EC_E = R.at(2);

    // Form an orthonormal basis with two components being in the plane
    // of n_EA_E, n_EB_E and n_EC_E, and the third being perpendicular
    // to this plane.
    // Call this the W frame.
    // The columns of the rotation matrix from E to W are w1, w2 and w3.
    Vector3D w1 = (n_EA_E - n_EB_E).normalize();
    Vector3D w3 = NVector::crossProduct(w1, (n_EA_E - n_EC_E).normalize()).normalize();
    Vector3D w2 = NVector::crossProduct(w1, w3);
    // Rotate A to the W frame.
    Vector3D n_EA_W = Vector3D(w1*n_EA_E, w2*n_EA_E, w3*n_EA_E);
    // The z component of n_EA_W, n_EB_W, n_EC_W are all identical.
    // The point n_EX_W that is equidistant from n_EA_W, n_EB_W, n_EC_W is
    // either (0.0, 0.0, 1.0) or (0.0, 0.0, -1.0).
    NVector n_EX_W;
    if (n_EA_W.getz() >= 0) {
      n_EX_W = Vector3D(0.0, 0.0, 1.0);
    } else {
      n_EX_W = Vector3D(0.0, 0.0, -1.0);
    }
    // Translate n_EX_W back to the E frame.
    // We need to invert the matrix composed of the column vectors w1, w2, w3.
    // Since this matrix is orthogonal it's inverse equals it's transpose.
    Vector3D wt1 = Vector3D(w1.getx(), w2.getx(), w3.getx());
    Vector3D wt2 = Vector3D(w1.gety(), w2.gety(), w3.gety());
    Vector3D wt3 = Vector3D(w1.getz(), w2.getz(), w3.getz());
    Vector3D n_EX_E = Vector3D(wt1*n_EX_W, wt2*n_EX_W, wt3*n_EX_W);

    center = n_EX_E;
    radius = NVector::distance(center, R.at(0));

    //qDebug() << R.at(0) << R.at(1) << R.at(2);
    //qDebug() << center;
    //qDebug() << "center3 check" << NVector::distance(center, R.at(0)) << NVector::distance(center, R.at(1)) << NVector::distance(center, R.at(2));
    break;
  }

  default:
    fatal("welzl should not occur");
  }
  return Circle(center, radius);
};

bool Welzl::outside(Circle D, NVector n)
{
  return NVector::distance(D.center(), n) > D.radius();
}

PointContainer Welzl::unionof(PointContainer R, NVector p)
{
  bool contains = false;
  for (const auto& e : R) {
    double dp_e_p = NVector::dotProduct(e, p);
    if (dp_e_p >= (1.0-8.0*DBL_EPSILON)) {
      contains = true;
      break;
    }
  }
  auto Rprime(R);
  if (!contains) {
    Rprime.push_back(p);
  }
  return Rprime;
}

/* This is Welzl's algorithm */
Circle Welzl::b_mindisk(PointContainer P, PointContainer R)
{
  Circle D;

  if (P.empty() || (R.size() == 3)) {
    D = b_md(R);
  } else {
    auto p = P.back();
    auto Pprime(P);
    Pprime.pop_back();
    D = b_mindisk(Pprime, R);
    if (outside(D, p)) {
      D = b_mindisk(Pprime, unionof(R, p));
    }
  }
  return D;
}

// Return the center and radius of the smallest circle containing the points.
// This works across the antimeridian and at the poles.
Circle Welzl::welzl(PointContainer Points)
{
  // randomize order of Points
  std::random_device rd;
  std::mt19937 generator(rd());
  std::shuffle(Points.begin(), Points.end(), generator);
  Circle bound = b_mindisk(Points, PointContainer());
  //qDebug() << bound.center_.latitude() << bound.center_.longitude() << bound.radius_;
  return bound;
}

} // namespace gpsbabel
