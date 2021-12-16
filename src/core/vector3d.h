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

#ifndef VECTOR3D_H
#define VECTOR3D_H

#include <QtCore/QDebug>

#include <iostream>

namespace gpsbabel
{

class Vector3D
{
public:
  Vector3D() = default;
  Vector3D(double xi, double yi, double zi) : x_(xi), y_(yi), z_(zi) {}

  [[nodiscard]] double norm() const;
  [[nodiscard]] double getx() const {return x_;}
  [[nodiscard]] double gety() const {return y_;}
  [[nodiscard]] double getz() const {return z_;}
  Vector3D& normalize();

  Vector3D& operator+=(const Vector3D& rhs);
  Vector3D& operator-=(const Vector3D& rhs);
  Vector3D& operator*=(double rhs);
  Vector3D& operator/=(double rhs);
  Vector3D operator+ (const Vector3D& rhs) const;
  Vector3D operator- (const Vector3D& rhs) const;
  Vector3D operator* (double rhs) const;
  Vector3D operator/ (double rhs) const;
  Vector3D operator- () const;
  double operator* (const Vector3D& b) const;

  static Vector3D crossProduct(const Vector3D& b, const Vector3D& c);
  static double dotProduct(const Vector3D& a, const Vector3D& b);

protected:
  double x_{};
  double y_{};
  double z_{};
};
Vector3D operator* (double lhs, const Vector3D& rhs);
std::ostream& operator<< (std::ostream& os, const Vector3D& v);
QDebug operator<<(QDebug debug, const Vector3D& v);

} // namespace gpsbabel
#endif // VECTOR3D_H
