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

#include <cmath>
#include <cstdio>
#include "vector3d.h"

namespace gpsbabel
{

Vector3D::Vector3D(double xi, double yi, double zi)
{
  x_ = xi;
  y_ = yi;
  z_ = zi;
}

double Vector3D::norm() const
{
  double norm = sqrt(x_*x_ + y_*y_ + z_*z_);
  return norm;
}

Vector3D& Vector3D::normalize()
{
  *this = *this/this->norm();
  return *this;
}

Vector3D& Vector3D::operator+=(const Vector3D& rhs)
{
  x_ += rhs.x_;
  y_ += rhs.y_;
  z_ += rhs.z_;
  return *this;
}

Vector3D& Vector3D::operator-=(const Vector3D& rhs)
{
  x_ -= rhs.x_;
  y_ -= rhs.y_;
  z_ -= rhs.z_;
  return *this;
}

Vector3D& Vector3D::operator*=(double rhs)
{
  x_ *= rhs;
  y_ *= rhs;
  z_ *= rhs;
  return *this;
}

Vector3D& Vector3D::operator/=(double rhs)
{
  x_ /= rhs;
  y_ /= rhs;
  z_ /= rhs;
  return *this;
}

Vector3D Vector3D::operator+ (const Vector3D& rhs) const
{
  Vector3D result = *this;
  result += rhs;
  return result;
}

Vector3D Vector3D::operator- (const Vector3D& rhs) const
{
  Vector3D result = *this;
  result -= rhs;
  return result;
}

Vector3D Vector3D::operator* (double rhs) const
{
  Vector3D result = *this;
  result *= rhs;
  return result;
}

Vector3D Vector3D::operator/ (double rhs) const
{
  Vector3D result = *this;
  result /= rhs;
  return result;
}

Vector3D Vector3D::operator- () const
{
  Vector3D result(-x_,-y_,-z_);
  return result;
}

double Vector3D::operator* (const Vector3D& b) const
{
  double result = x_*b.x_ + y_*b.y_ + z_*b.z_;
  return result;
}

double Vector3D::dotProduct(const Vector3D& a, const Vector3D& b)
{
  double dotproduct = a.x_*b.x_ + a.y_*b.y_ + a.z_*b.z_;
  return dotproduct;
}

Vector3D Vector3D::crossProduct(const Vector3D& b, const Vector3D& c)
{
  // a = b x c
  Vector3D a;
  a.x_ = b.y_*c.z_ - b.z_*c.y_;
  a.y_ = b.z_*c.x_ - b.x_*c.z_;
  a.z_ = b.x_*c.y_ - b.y_*c.x_;
  return a;
}

Vector3D operator* (double lhs, const Vector3D& rhs)
{
  Vector3D result = rhs*lhs;
  return result;
}

std::ostream& operator<< (std::ostream& os, const Vector3D& v)
{
  os << '(' << v.getx() << ", " << v.gety() << ", " << v.getz() << ')';
  return os;
}

QDebug operator<< (QDebug debug, const Vector3D& v)
{
  QDebugStateSaver saver(debug);
  debug.nospace() << '(' << v.getx() << ", " << v.gety() << ", " << v.getz() << ')';
  return debug;
}

} // namespace gpsbabel
