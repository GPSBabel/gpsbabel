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

#ifndef NVECTOR_H
#define NVECTOR_H

#include "vector3d.h"

namespace gpsbabel
{

#define COMPARE_BEARING_TO_GRTCIRC
#ifdef COMPARE_BEARING_TO_GRTCIRC
constexpr double MEAN_EARTH_RADIUS_METERS = 6378137.0;
#else
constexpr double MEAN_EARTH_RADIUS_METERS = 6371000.0;
#endif
constexpr double WGS84_SEMI_MAJOR_AXIS_METERS = 6378137.0; // a
#ifdef COMPARE_BEARING_TO_GRTCIRC
constexpr double WGS84_FLATTENING = 0.0;
#else
constexpr double WGS84_FLATTENING = 1.0/298.257223563; // (a-b)/a
#endif
constexpr double WGS84_ASPECT_RATIO = 1.0 - WGS84_FLATTENING; // b/a
constexpr double WGS84_SEMI_MINOR_AXIS_METERS = WGS84_SEMI_MAJOR_AXIS_METERS * WGS84_ASPECT_RATIO; // b
constexpr double WGS84_ECCENTRICITY_SQUARED = 1.0 - (WGS84_ASPECT_RATIO * WGS84_ASPECT_RATIO);

class PVector;

class LatLon
{
public:
  LatLon(double latitude, double longitude);

  double lat;
  double lon;
};

class NVector : public Vector3D
{

public:
  NVector() = default;
  NVector(double latitude_degrees, double longitude_degrees);
  NVector(const Vector3D& v);

  [[nodiscard]] double latitude() const;
  [[nodiscard]] double longitude() const;

  static double distance_radians(const NVector& n_EA_E, const NVector& n_EB_E);
  static double distance(const NVector& n_EA_E, const  NVector& n_EB_E);
  static double distance(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees);
  static double azimuth_radians(const NVector& n_EA_E, const NVector& n_EB_E, double height_a = 0.0, double height_b = 0.0);
  static double azimuth(const NVector& n_EA_E, const NVector& n_EB_E, double height_a = 0.0, double height_b = 0.0);
  static double azimuth(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees, double height_a = 0.0, double height_b = 0.0);
  static double azimuth_true_degrees(const NVector& n_EA_E, const NVector& n_EB_E, double height_a = 0.0, double height_b = 0.0);
  static double azimuth_true_degrees(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees, double height_a = 0.0, double height_b = 0.0);
  static NVector linepart(const NVector& n_EA_E, const NVector& n_EB_E, double fraction);
  static LatLon linepart(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees, double fraction);
  static NVector crossTrackProjection(const NVector& n_EA_E, const NVector& n_EB_E, const NVector& n_EY_E);
  static LatLon crossTrackProjection(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees, double latitude_x_degrees, double longitude_x_degrees);
  static double crossTrackDistance(const NVector& n_EA_E, const NVector& n_EB_E, const NVector& n_EX_E);
  static double crossTrackDistance(double latitude_a_degrees, double longitude_a_degrees, double latitude_b_degrees, double longitude_b_degrees, double latitude_x_degrees, double longitude_x_degrees);
};

class PVector : public Vector3D
{
public:
  PVector() = default;
  PVector(const NVector& n_EA_E, double h);
  PVector(const Vector3D& v);

  [[nodiscard]] std::pair<NVector, double> toNVectorAndHeight() const;
};

} // namespace gpsbabel
#endif // NVECTOR_H
