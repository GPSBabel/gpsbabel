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

//
// Largely taken from
// https://github.com/liaoyinan/gps_kalman/blob/master/gps_kalman.c
// though that basic code appears all over GitHub.
//
// This implementation very much preserves the C style of the original,
// with no non-trivial work at making the meat of it C++.
//

#include <math.h>

#include "defs.h"
#include "filter.h"
#include "kalmanfilter.h"

#if FILTERS_ENABLED

#define MYNAME "Kalman filter"

static void kalman2_init(kalman2_state *state, const double init_x[2], double init_p[2][2])
{
    state->x[0]    = init_x[0];
    state->x[1]    = init_x[1];
    state->p[0][0] = init_p[0][0];
    state->p[0][1] = init_p[0][1];
    state->p[1][0] = init_p[1][0];
    state->p[1][1] = init_p[1][1];
    //state->A       = {{1, 0.1}, {0, 1}};
    state->A[0][0] = 1;
    state->A[0][1] = 0.1;
    state->A[1][0] = 0;
    state->A[1][1] = 1;
    //state->H       = {1,0};
    state->H[0]    = 1;
    state->H[1]    = 0;
    //state->q       = {{10e-6,0}, {0,10e-6}};
    state->q[0]    = 10e-7;
    state->q[1]    = 10e-7; /* estimated error covariance */
    state->r       = 10e-7; /* measure noise covariance */
}

static double kalman2_filter(kalman2_state *state, double z_measure)
{
    /* Step1: Predict */
    state->x[0] = state->A[0][0] * state->x[0] + state->A[0][1] * state->x[1];
    state->x[1] = state->A[1][0] * state->x[0] + state->A[1][1] * state->x[1];
    /* p(n|n-1)=A^2*p(n-1|n-1)+q */
    state->p[0][0] = state->A[0][0] * state->p[0][0] + state->A[0][1] * state->p[1][0] + state->q[0];
    state->p[0][1] = state->A[0][0] * state->p[0][1] + state->A[1][1] * state->p[1][1];
    state->p[1][0] = state->A[1][0] * state->p[0][0] + state->A[0][1] * state->p[1][0];
    state->p[1][1] = state->A[1][0] * state->p[0][1] + state->A[1][1] * state->p[1][1] + state->q[1];

    /* Step2: Measurement */
    /* gain = p * H^T * [r + H * p * H^T]^(-1), H^T means transpose. */
    double temp0 = state->p[0][0] * state->H[0] + state->p[0][1] * state->H[1];
    double temp1 = state->p[1][0] * state->H[0] + state->p[1][1] * state->H[1];
    double temp  = state->r + state->H[0] * temp0 + state->H[1] * temp1;
    state->gain[0] = temp0 / temp;
    state->gain[1] = temp1 / temp;
    /* x(n|n) = x(n|n-1) + gain(n) * [z_measure - H(n)*x(n|n-1)]*/
    temp = state->H[0] * state->x[0] + state->H[1] * state->x[1];
    state->x[0] = state->x[0] + state->gain[0] * (z_measure - temp);
    state->x[1] = state->x[1] + state->gain[1] * (z_measure - temp);

    /* Update @p: p(n|n) = [I - gain * H] * p(n|n-1) */
    state->p[0][0] = (1 - state->gain[0] * state->H[0]) * state->p[0][0];
    state->p[0][1] = (1 - state->gain[0] * state->H[1]) * state->p[0][1];
    state->p[1][0] = (1 - state->gain[1] * state->H[0]) * state->p[1][0];
    state->p[1][1] = (1 - state->gain[1] * state->H[1]) * state->p[1][1];

    return state->x[0];
}

static void kalman2_set_param(kalman2_state *state, const double q[2], double r)
{
    state->q[0] = q[0];
    state->q[1] = q[1];
    state->r = r;
}

gps_filter_t *gps_init(void)
{
    gps_filter_t *gps_kalman_filter = (gps_filter_t *) malloc(sizeof(gps_filter_t));
    gps_kalman_filter->is_first = true;
    return gps_kalman_filter;
}

bool gps_filter(gps_filter_t *gps_kalman_filter, double in_longitude, double in_latitude, double *out_longitude,
                double *out_latitude)
{
    if (gps_kalman_filter == NULL)
    {
        *out_longitude = in_longitude;
        *out_latitude = in_latitude;
        return false;
    }
    if (gps_kalman_filter->is_first)
    {
        double init_longitude[2] = {in_longitude, 0};
        double init_longitude_p[2][2] = {{0, 0},
                                         {0, 0}};
        kalman2_init(&gps_kalman_filter->longitude_filter, init_longitude, init_longitude_p);

        double init_latitude[2] = {in_latitude, 0};
        double init_latitude_p[2][2] = {{0, 0},
                                        {0, 0}};
        kalman2_init(&gps_kalman_filter->latitude_filter, init_latitude, init_latitude_p);
        gps_kalman_filter->is_first = false;
    }
#define ORIGINAL 0
#if ORIGINAL
    double process_noise_err[2] = {0.00001, 0.00001};
#else
    double process_noise_err[2] = {0.00003, 0.00003};
#endif
    double measurement_err = (in_longitude - gps_kalman_filter->longitude_filter.x[0]) *
                             (in_longitude - gps_kalman_filter->longitude_filter.x[0]);
    measurement_err += (in_latitude - gps_kalman_filter->latitude_filter.x[0]) *
                       (in_latitude - gps_kalman_filter->latitude_filter.x[0]);
#if ORIGINAL
    //measurement_err *= 1300;
#else
    measurement_err *= 1.00;
#endif
    kalman2_set_param(&gps_kalman_filter->longitude_filter, process_noise_err, measurement_err);
    kalman2_set_param(&gps_kalman_filter->latitude_filter, process_noise_err, measurement_err);

    *out_longitude = kalman2_filter(&gps_kalman_filter->longitude_filter, in_longitude);
    *out_latitude = kalman2_filter(&gps_kalman_filter->latitude_filter, in_latitude);

    return true;
}

void gps_de_init(gps_filter_t *gps_kalman_filter)
{
    if (gps_kalman_filter != NULL)
    {
        free(gps_kalman_filter);
        gps_kalman_filter = NULL;
    }
}


void KalmanFilter::process()
{
  WayptFunctor<KalmanFilter> kalman_point_cb_f(this, &KalmanFilter::kalman_point_cb);

  track_disp_all(nullptr, nullptr, kalman_point_cb_f);

#if 0
  for (auto* track : qAsConst(track_list)) {
    foreach (Waypoint* wpt, track->waypoint_list) {
//      wpt->creation_time = wpt->creation_time.addSecs(delta);
    }
  }
#endif
}
route_head* trk_head;

void KalmanFilter::init()
{
qDebug() << "Init";
  gps_filter_ = gps_init();
  trk_head = new route_head;
trk_head->line_color.bbggrr = color_to_bbggrr("tomato");
trk_head->rte_name = "filtered";
}

void KalmanFilter::deinit()
{
qDebug() << "Deinit";
  gps_de_init(gps_filter_);
  track_add_head(trk_head);
//  delete trk_head;
}

void KalmanFilter::kalman_point_cb(const Waypoint* ref) {
  auto* wpt = const_cast<Waypoint*>(ref);
  double out_lon, out_lat;
qDebug() << wpt->latitude << wpt->longitude;
#if 0
  out_lat = wpt->latitude;
  out_lon = wpt->longitude;
#else
  gps_filter(gps_filter_, wpt->longitude, wpt->latitude, &out_lon, &out_lat);
qDebug() << "XX" << out_lat << out_lon;
#endif

// Just duplicate the track instead of "fixing" it in place so we can easily
// do A/B testing via checking layers in Earth.
// make && ./gpsbabel -i mapsend -f reference/track/mapsend.trk -x kalman -o kml -F /tmp/blah.kml && open /tmp/blah.kml

auto trk_pt = new Waypoint;
qDebug() << "N " << wpt->shortname;
trk_pt->latitude = out_lat;
trk_pt->longitude = out_lon;
trk_pt->creation_time = wpt->creation_time;
track_add_wpt(trk_head, trk_pt);
}

#endif // FILTERS_ENABLED
