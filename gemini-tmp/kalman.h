/*
    Copyright (C) 2021, 2025 Robert Lipe, gpsbabel.org

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
#ifndef KALMAN_H_INCLUDED_
#define KALMAN_H_INCLUDED_

#include "filter.h"
#include "src/core/matrix.h"
#include "src/core/nvector.h"
#include <QtCore/QVector>
#include <deque>
#include <vector>
#include "option.h" // Added for OptionDouble

struct KalmanExtraData {
    bool is_zinger_deletion = false;
};

#if FILTERS_ENABLED

class Kalman : public Filter {
 public:
  Kalman();
  ~Kalman() override = default;

  void process() override;
  void init() override;
  void deinit() override;
  QVector<arglist_t>* get_args() override;

 private:
  void kalman_point_cb(Waypoint* wpt);

  bool is_initialized_;
  bool initial_velocity_estimated_;
  QDateTime last_timestamp_;
  gpsbabel::NVector last_nvector_;

  // Kalman filter matrices
  Matrix x_;  // State vector [x, y, z, vel_x, vel_y, vel_z]
  Matrix P_;  // Covariance matrix
  Matrix F_;  // State transition matrix
  Matrix H_;  // Measurement matrix
  Matrix R_;  // Measurement noise covariance matrix
  Matrix Q_;  // Process noise covariance matrix

  // Configurable parameters
  OptionDouble gap_factor_option_;
  OptionDouble r_scale_option_;
  OptionDouble q_scale_pos_option_;
  OptionDouble q_scale_vel_option_;
  OptionDouble max_speed_option_;
  OptionDouble interp_max_dt_option_;
  OptionDouble interp_min_multiplier_option_;
  OptionString profile_option_;

  // Member variables to store the resolved option values
  double r_scale_;
  double q_scale_pos_;
  double q_scale_vel_;
  double interp_max_dt_;
  double interp_min_multiplier_;
  double gap_factor_;
  double max_speed_;

  QVector<arglist_t> args = {
    {"gap_factor", &gap_factor_option_, "Factor to determine if a time gap is significant enough to trigger gap recovery",
      "10.0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"r_scale", &r_scale_option_, "Measurement noise covariance scaling factor",
      "1.0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"q_scale_pos", &q_scale_pos_option_, "Process noise covariance scaling factor for position",
      "1.0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"q_scale_vel", &q_scale_vel_option_, "Process noise covariance scaling factor for velocity",
      "1.0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"max_speed", &max_speed_option_, "Maximum reasonable speed in m/s for pre-filtering",
      "15.0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"interp_max_dt", &interp_max_dt_option_, "Maximum time gap (seconds) to attempt interpolation",
      "300.0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"interp_min_multiplier", &interp_min_multiplier_option_, "Multiplier for median_dt to consider interpolation",
      "1.5", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"profile", &profile_option_, "Predefined profile for Kalman filter tuning (auto, walking, running, cycling, driving, flying)",
      "auto", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

};

#endif // FILTERS_ENABLED
#endif // KALMAN_H_INCLUDED_
