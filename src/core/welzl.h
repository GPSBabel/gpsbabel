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

#ifndef SRC_CORE_WELZL_H
#define SRC_CORE_WELZL_H

#include <QVector>
#include "src/core/nvector.h"


namespace gpsbabel
{

class Circle
{
public:
  Circle() = default;
  Circle(NVector center, double radius) : center_(center), radius_(radius) {}

  NVector center() const {return center_;}
  double radius() const {return radius_;}

private:
  NVector center_;
  double radius_;
};

class Welzl
{
public:
  static Circle welzl(QVector<NVector>);

private:
  static Circle b_md(QVector<NVector> R);
  static bool outside(Circle D, NVector n);
  static QVector<NVector> unionof(QVector<NVector> R, NVector p);
  static Circle b_mindisk(QVector<NVector> P, QVector<NVector> R);
};

} // namespace gpsbabel

#endif  // SRC_CORE_WELZL_H
