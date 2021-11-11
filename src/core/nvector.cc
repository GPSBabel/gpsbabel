/*
    Copyright (C) 2018, 2021 Robert Lipe, gpsbabel.org

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

 */

// https://en.wikipedia.org/wiki/N-vector
// http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf

#include <cfloat>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <utility>

#include "defs.h"
#include "nvector.h"
#include "vector3d.h"

namespace gpsbabel
{

LatLon::LatLon(double latitude, double longitude)
{
  lat = latitude;
  lon = longitude;
}

NVector::NVector(double latitude_degrees, double longitude_degrees)
{
  // This implements equation 3.

  // The coordinate system choice matches
  // A_Nonsingular_Horizontal_Position_Representation.pdf
  // The E frame:
  // from the earths center
  // x points to North pole
  // y points towards latitude 0, longitude +90 (east)
  // z points to latitude 0, longitude +180, the antimeridian
  double latitude_radians = latitude_degrees * M_PI/180.0;
  double longitude_radians = longitude_degrees * M_PI/180.0;
  x = sin(latitude_radians);
  y = sin(longitude_radians)*cos(latitude_radians);
  z = -cos(longitude_radians)*cos(latitude_radians);
}

NVector::NVector(const Vector3D& v)
{
  x = v.getx();
  y = v.gety();
  z = v.getz();
}

std::pair<NVector, double> PVector::toNVectorAndHeight() const
{
  // This implements equation 23.
  constexpr double a = WGS84_SEMI_MAJOR_AXIS_METERS;
  constexpr double a2 = a * a;
  constexpr double e2 = WGS84_ECCENTRICITY_SQUARED;
  constexpr double e4 = e2 * e2;

  double px2 = getx() * getx();
  double py2 = gety() * gety();
  double pz2 = getz() * getz();

  double q = ((1.0-e2)/(a2)) * px2;
  double p = (py2 + pz2) / (a2);
  double r = (p+q-e4) / 6.0;
  double s = (e4*p*q) / (4.0*r*r*r);
  double t = cbrt(1.0 + s + sqrt(s*(2.0+s)));
  double u = r * (1.0 + t + (1.0/t));
  double v = sqrt(u*u + e4*q);
  double w = e2 * ((u+v-q)/(2.0*v));
  double k = sqrt(u+v+w*w) - w;
  double d = k*sqrt(py2 + pz2) / (k+e2);
  double sf = 1.0 / (sqrt(d*d + px2));
  double sf2 = k / (k+e2);

  double height = ((k+e2-1.0)/k) * sqrt(d*d+px2);
  double nx = sf * getx();
  double ny = sf * sf2 * gety();
  double nz = sf * sf2 * getz();

  return {Vector3D(nx, ny, nz), height};
}

double NVector::latitude() const
{
  // This implements equation 6.
  double latitude_radians = atan2(x, sqrt(y*y + z*z));
  double latitude_degrees = latitude_radians * 180.0/M_PI;
  return latitude_degrees;
}

double NVector::longitude() const
{
  // This implements equation 5.
  double longitude_radians = atan2(y, -z);
  double longitude_degrees = longitude_radians * 180.0/M_PI;
  return longitude_degrees;
}

// great circle distance in radians
double NVector::distance_radians(const NVector& n_EA_E, const NVector& n_EB_E)
{
  // This implements equation 16 using arctan for numerical accuracy.
  double result = atan2(crossProduct(n_EA_E, n_EB_E).norm(),
                        dotProduct(n_EA_E, n_EB_E));
  return result;
}

// great circle distance in meters
double NVector::distance(const NVector& n_EA_E, const NVector& n_EB_E)
{
  double result = distance_radians(n_EA_E, n_EB_E) * MEAN_EARTH_RADIUS_METERS;
  return result;
}

double NVector::distance(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees)
{
  NVector n_EA_E(latitude_a_degrees, longitude_a_degrees);
  NVector n_EB_E(latitude_b_degrees, longitude_b_degrees);
  double result = distance(n_EA_E, n_EB_E);
  return result;
}

double NVector::azimuth_radians(const NVector& n_EA_E, const NVector& n_EB_E, double height_a, double height_b)
{
  PVector p_EA_E(n_EA_E, height_a);
  PVector p_EB_E(n_EB_E, height_b);
  Vector3D p_AB_E = p_EB_E - p_EA_E;

  // equation 9 (normalized)
  Vector3D kaeast = crossProduct(Vector3D(1.0, 0.0, 0.0), n_EA_E).normalize();
  // equation 10 (normalized, n_EA_E and kaeast are perpendicular unit vectors)
  Vector3D kanorth = crossProduct(n_EA_E, kaeast);

  // equation 11
  // Ren = [kanorth(normalized) kaeast(normalized) -n_EA_E];
  // and a rotation from the E frame to the N frame (North, East, Down).
  // P_AB_N = R_EN' * p_AB_E
  double P_AB_N_x = kanorth*p_AB_E;
  double P_AB_N_y = kaeast*p_AB_E;
  // double P_AB_N_z = (-n_EA_E)*p_AB_E;

  double azimuth = atan2(P_AB_N_y, P_AB_N_x);
  return azimuth;
}

double NVector::azimuth(const NVector& n_EA_E, const NVector& n_EB_E, double height_a, double height_b)
{
  double azimuth = azimuth_radians(n_EA_E, n_EB_E, height_a, height_b);
  double azimuth_degrees = azimuth * 180.0/M_PI;
  return azimuth_degrees;
}

// returns values in the range [0.0,360)
double NVector::azimuth_true_degrees(const NVector& n_EA_E, const NVector& n_EB_E, double height_a, double height_b)
{
  double result = azimuth(n_EA_E, n_EB_E, height_a, height_b);
  if (result < 0.0) {
    result += 360.0;
  }
  if (result >= 360.0) {
    result = 0.0;
  }
  return result;
}

double NVector::azimuth(double latitude_a_degrees, double longitude_a_degrees,
                        double latitude_b_degrees, double longitude_b_degrees,
                        double height_a, double height_b)
{
  NVector n_EA_E(latitude_a_degrees, longitude_a_degrees);
  NVector n_EB_E(latitude_b_degrees, longitude_b_degrees);
  return azimuth(n_EA_E, n_EB_E, height_a, height_b);
}

// returns values in the range [0.0,360)
double NVector::azimuth_true_degrees(double latitude_a_degrees, double longitude_a_degrees,
                                     double latitude_b_degrees, double longitude_b_degrees,
                                     double height_a, double height_b)
{
  double result = azimuth(latitude_a_degrees, longitude_a_degrees,
                          latitude_b_degrees, longitude_b_degrees,
                          height_a, height_b);
  if (result < 0.0) {
    result += 360.0;
  }
  if (result >= 360.0) {
    result = 0.0;
  }
  return result;
}

#if 0
// This interpolation is nonlinear!
NVector NVector::linepart(const NVector& n_EA_E, const NVector& n_EB_E, double fraction)
{
  Vector3D n_ER_E = (n_EA_E + (n_EB_E - n_EA_E)*fraction).normalize();
  return n_ER_E;
}
#elif 0
NVector NVector::linepart(const NVector& n_EA_E, const NVector& n_EB_E, double fraction)
{
  // see example 8.
  double sab_rad = distance_radians(n_EA_E, n_EB_E);
  double sar_rad = fraction*sab_rad;
  double az_rad = azimuth_radians(n_EA_E, n_EB_E);

  // equation 9 (normalized)
  Vector3D kaeast = crossProduct(Vector3D(1.0, 0.0, 0.0), n_EA_E).normalize();
  // equation 10 (normalized, n_EA_E and kaeast are perpendicular unit vectors)
  Vector3D kanorth = crossProduct(n_EA_E, kaeast);

  Vector3D d_EA_E = kanorth*cos(az_rad) + kaeast*sin(az_rad);
  Vector3D n_ER_E = n_EA_E*cos(sar_rad) + d_EA_E*sin(sar_rad);
  return n_ER_E;
}
#else
NVector NVector::linepart(const NVector& n_EA_E, const NVector& n_EB_E, double fraction)
{
  double dp_a_b = dotProduct(n_EA_E, n_EB_E);
  if (dp_a_b >= (1.0-8.0*DBL_EPSILON)) {
    // The points are so close we will have trouble constructing a basis.
    // Since they are so close the non-linearities in direct n vector
    // interpolation are negligible.
    Vector3D result = (n_EA_E + (n_EB_E-n_EA_E)*fraction).normalize();
    return result;
  }
  if (dp_a_b <= (-1.0+8.0*DBL_EPSILON)) {
    // The points are so close to be exactly opposite each other that
    // there isn't a unique great circle between them.
    // Unless fraction is 1.0 or 0.0 there isn't a unique answer.
    Vector3D result(nan(""), nan(""), nan(""));
    return result;
  }
  // Form an orthonormal basis with one component perpendicular to the great
  // circle containing A and B, and one component being A.
  // Call this the W frame.
  // The columns of the rotation matrix from E to W are w1, w2 and w3.
  Vector3D w1 = n_EA_E;
  Vector3D w2 = crossProduct(n_EB_E, n_EA_E).normalize();
  Vector3D w3 = crossProduct(n_EA_E,w2);
  // Rotate A and B to the W frame.
  // Vector3D n_EA_W = Vector3D(w1*n_EA_E, w2*n_EA_E, w3*n_EA_E);
  Vector3D n_EB_W = Vector3D(w1*n_EB_E, w2*n_EB_E, w3*n_EB_E);
  // By construction n_EA_W.y is (1, 0, 0),
  // n_EB_W.y is zero,
  // and both n_EA_W and n_EB_W are unit vectors.
  // The information is all in the angle between them,
  // which is all in n_EB_W.z and n_EB_W.x.
  double theta = atan2(n_EB_W.getz(), n_EB_W.getx());
  // We define the interpolated point as the point on the great circle
  // between A and B whose great circle distance from A is the given fraction
  // of the great circle distance between A and B.  For a spheroid the
  // distance is proportional to the angle between the vectors.
  // The interpolated point is thus:
  Vector3D n_EX_W = Vector3D(cos(fraction*theta), 0.0, sin(fraction*theta));
  // Translate the interpolated point back to the E frame.
  // We need to invert the matrix composed of the column vectors w1, w2, w3.
  // Since this matrix is orthogonal it's inverse equals it's transpose.
  Vector3D wt1 = Vector3D(w1.getx(), w2.getx(), w3.getx());
  Vector3D wt2 = Vector3D(w1.gety(), w2.gety(), w3.gety());
  Vector3D wt3 = Vector3D(w1.getz(), w2.getz(), w3.getz());
  Vector3D n_EX_E = Vector3D(wt1*n_EX_W, wt2*n_EX_W, wt3*n_EX_W);
  return n_EX_E;
}
#endif

LatLon NVector::linepart(double latitude_a_degrees, double longitude_a_degrees,
                         double latitude_b_degrees, double longitude_b_degrees,
                         double fraction)
{
  NVector n_EA_E(latitude_a_degrees, longitude_a_degrees);
  NVector n_EB_E(latitude_b_degrees, longitude_b_degrees);
  NVector n_ER_E = linepart(n_EA_E, n_EB_E, fraction);
  LatLon result(n_ER_E.latitude(), n_ER_E.longitude());
  return result;
}

// Find the point of closest approach to Y on the great circle arc AB.
// The great circle arc AB is the shortest of the two possibilities.
#if 0
// This implementation works and passes regression.
// However, it seems harder to understand than the next implementation.
NVector NVector::crossTrackProjection(const NVector& n_EA_E, const NVector& n_EB_E, const NVector& n_EY_E)
{
  // Compute the normal to the great circle defined by A and B:
  Vector3D a_cross_b = crossProduct(n_EA_E, n_EB_E);
  // Because a_cross_b is normal to the plane defined by n_EA_E and n_EB_E,
  // any great circle defined by the point defined by a_cross_b and
  // any other point, e.g. Y,
  // will cross the great circle including the points A and B perpendicularly.
  // Thus, if we find the intersection of the two great circles we will
  // have found the projection of Y onto the great circle defined
  // by A and B.
  Vector3D x = a_cross_b;
  // Compute the normal to the great circle defined by Y and X:
  Vector3D x_cross_y = crossProduct(x, n_EY_E);
  // A candidate projection from X onto the great circle defined by A and B is
  // c_candidate, the other possibility is -c_candidate.
  Vector3D c_candidate = crossProduct(a_cross_b, x_cross_y).normalize();
  // pick the candidate closest to Y.
  Vector3D c = dotProduct(c_candidate, n_EY_E) >= 0.0 ? c_candidate : -c_candidate;
  // find the midpoint between A and B.
  Vector3D m = (n_EA_E + n_EB_E).normalize();
  // Now we have a point C on the great circle defined by A and B,
  // and the midpoint M of this arc.
  // If C is on the arc defined by A and B then the point of closest
  // approach to Y is C.  But if C is not on the arc, then the point
  // of closest approach is either A or B, whichever is close to C.
  // Note that A, B, C, M all are all on the great circle defined by A and B, so
  // the dot product of any of these two unit vectors monotonically decreases
  // the farther apart they are on the great circle.
  Vector3D result;
  if (dotProduct(n_EA_E, m) < dotProduct(c, m)) {
    result = c;
  } else if (dotProduct(n_EA_E, c) < dotProduct(n_EB_E, c)) {
    result = n_EB_E;
  } else {
    result = n_EA_E;
  }
  return result;
}
#else
NVector NVector::crossTrackProjection(const NVector& n_EA_E, const NVector& n_EB_E, const NVector& n_EY_E)
{
  // Form an orthonormal basis with one component perpendicular to the great
  // circle containing A and B, and one component being A.
  // Call this the W frame.
  // The columns of the rotation matrix from E to W are w1, w2 and w3.
  Vector3D w1 = n_EA_E;
  Vector3D w2 = crossProduct(n_EB_E, n_EA_E).normalize();
  Vector3D w3 = crossProduct(n_EA_E,w2);
  // Rotate Y to the W frame.
  Vector3D n_EY_W = Vector3D(w1*n_EY_E, w2*n_EY_E, w3*n_EY_E);
  // By construction n_EA_W.y is (1, 0, 0),
  // n_EB_W.y is zero,
  // and both n_EA_W and n_EB_W are unit vectors.
  // The projection of Y onto the great circle defined by A and B
  // is just the n_EY_W with the y component set to zero.
  Vector3D n_EC_W = Vector3D(n_EY_W.getx(), 0.0, n_EY_W.getz()).normalize();
  // Translate the projected point back to the E frame.
  // We need to invert the matrix composed of the column vectors w1, w2, w3.
  // Since this matrix is orthogonal it's inverse equals it's transpose.
  Vector3D wt1 = Vector3D(w1.getx(), w2.getx(), w3.getx());
  Vector3D wt2 = Vector3D(w1.gety(), w2.gety(), w3.gety());
  Vector3D wt3 = Vector3D(w1.getz(), w2.getz(), w3.getz());
  Vector3D n_EC_E = Vector3D(wt1*n_EC_W, wt2*n_EC_W, wt3*n_EC_W);
  // find the midpoint between A and B.
  Vector3D n_EM_E = (n_EA_E + n_EB_E).normalize();
  // Now we have a point C on the great circle defined by A and B,
  // and the midpoint M of this arc.
  // If C is on the arc defined by A and B then the point of closest
  // approach to Y is C.  But if C is not on the arc, then the point
  // of closest approach is either A or B, whichever is close to C.
  // Note that A, B, C, M all are all on the great circle defined by A and B, so
  // the dot product of any of these two unit vectors monotonically decreases
  // the farther apart they are on the great circle.
  Vector3D result;
  if (dotProduct(n_EA_E, n_EM_E) < dotProduct(n_EC_E, n_EM_E)) {
    result = n_EC_E;
  } else if (dotProduct(n_EA_E, n_EC_E) < dotProduct(n_EB_E, n_EC_E)) {
    result = n_EB_E;
  } else {
    result = n_EA_E;
  }
  return result;
}
#endif

LatLon NVector::crossTrackProjection(double latitude_a_degrees, double longitude_a_degrees,
                                     double latitude_b_degrees, double longitude_b_degrees,
                                     double latitude_x_degrees, double longitude_x_degrees)
{
  NVector n_EA_E(latitude_a_degrees, longitude_a_degrees);
  NVector n_EB_E(latitude_b_degrees, longitude_b_degrees);
  NVector n_EX_E(latitude_x_degrees, longitude_x_degrees);
  NVector n_EC_E = crossTrackProjection(n_EA_E, n_EB_E, n_EX_E);
  LatLon result(NVector(n_EC_E).latitude(), NVector(n_EC_E).longitude());
  return result;
}

// Find the minimum distance to point X when traveling from A to B.
#if 0
// This doesn't work if we are more than 90 degrees away
double NVector::crossTrackDistance(const NVector& n_EA_E, const NVector& n_EB_E, const NVector& n_EX_E)
{
  Vector3D c_E = crossProduct(n_EA_E, n_EB_E).normalize();
  double result = fabs((atan2(crossProduct(c_E, n_EX_E).norm(),
                              dotProduct(c_E, n_EX_E)) - M_PI/2.0)) * MEAN_EARTH_RADIUS_METERS;
  return result;
}
#else
double NVector::crossTrackDistance(const NVector& n_EA_E, const NVector& n_EB_E, const NVector& n_EX_E)
{
  NVector n_EP_E = crossTrackProjection(n_EA_E, n_EB_E, n_EX_E);
  double result = distance(n_EP_E, n_EX_E);
  return result;
}
#endif

double NVector::crossTrackDistance(double latitude_a_degrees, double longitude_a_degrees,
                                   double latitude_b_degrees, double longitude_b_degrees,
                                   double latitude_x_degrees, double longitude_x_degrees)
{
  NVector n_EA_E(latitude_a_degrees, longitude_a_degrees);
  NVector n_EB_E(latitude_b_degrees, longitude_b_degrees);
  NVector n_EX_E(latitude_x_degrees, longitude_x_degrees);
  double result = crossTrackDistance(n_EA_E, n_EB_E, n_EX_E);
  return result;
}

PVector::PVector(const NVector& n_EA_E, double h=0.0)
{
  // This implements equation 22.

  // a semi-major axis, b semi-minor axis, f flattening
  // a/b = 1/(1-f)
  // aspect ratio = b/a
  constexpr double b = WGS84_SEMI_MINOR_AXIS_METERS;
  constexpr double asq_over_bsq = 1.0 / (WGS84_ASPECT_RATIO * WGS84_ASPECT_RATIO);

  double denom = sqrt(n_EA_E.getx()*n_EA_E.getx() + n_EA_E.gety()*n_EA_E.gety()*asq_over_bsq + n_EA_E.getz()*n_EA_E.getz()*asq_over_bsq);
  x = (b/denom)*n_EA_E.getx() + h*n_EA_E.getx();
  y = (b/denom)*asq_over_bsq*n_EA_E.gety() + h*n_EA_E.gety();
  z = (b/denom)*asq_over_bsq*n_EA_E.getz() + h*n_EA_E.getz();
}

PVector::PVector(const Vector3D& v)
{
  x = v.getx();
  y = v.gety();
  z = v.getz();
}

} // namespace gpsbabel
