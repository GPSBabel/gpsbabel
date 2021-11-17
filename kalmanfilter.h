/*
    Kalman filter

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

 */

#ifndef KALMANFILTER_H_INCLUDED_
#define KALMANFILTER_H_INCLUDED_

#include <QtCore/QVector>  // for QVector

#include "defs.h"    // for ARGTYPE_BOOL, ARG_NOMINMAX, ARGTYPE_BEGIN_EXCL
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

/* 2 Dimension */
typedef struct {
    double x[2];     /* state: [0]-angle [1]-difference of angle, 2x1 */
    double A[2][2];  /* X(n)=A*X(n-1)+U(n),U(n)~N(0,q), 2x2 */
    double H[2];     /* Z(n)=H*X(n)+W(n),W(n)~N(0,r), 1x2   */
    double q[2];     /* process(predict) noise covariance,2x1 [q0,0; 0,q1] */
    double r;        /* measure noise variance */
    double p[2][2];  /* estimated error covariance,2x2 [p0 p1; p2 p3] */
    double gain[2];  /* 2x1 */
} kalman2_state;

typedef struct
{
    kalman2_state longitude_filter;
    kalman2_state latitude_filter;
    bool is_first;
} gps_filter_t;

class KalmanFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;
  void deinit() override;

private:
  void kalman_point_cb(const Waypoint* ref);
  gps_filter_t* gps_filter_;

  QVector<arglist_t> args = {
  };

};
#endif // FILTERS_ENABLED
#endif // KALMENFILTER_H_INCLUDED_
