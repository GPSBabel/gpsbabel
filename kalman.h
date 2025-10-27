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

#include <QDateTime>           // for QDateTime
#include <QList>               // for QList
#include <QString>             // for QString
#include <QVector>             // for QVector

#include "defs.h"              // for arglist_t, ARG_NOMINMAX, ARGTYPE_FLOAT, ARGTYPE_STRING, Waypoint
#include "filter.h"            // for Filter
#include "option.h"            // for OptionDouble, OptionString
#include "src/core/matrix.h"   // for Matrix
#include "src/core/nvector.h"  // for NVector

#if FILTERS_ENABLED

class Kalman : public Filter {
 public:
  /* Types */

  enum class PreFilterState { NORMAL, RECOVERY, FIRST_GOOD_SEEN_IN_RECOVERY };

  /* Constants */

  static constexpr int debugLevelInfo = 1;
  static constexpr int debugLevelDebug = 3;
  static constexpr int debugLevelTrace = 5;
  static constexpr int debugLevelVerboseTrace = 7;

  /* Member Functions */

  void process() override;
  QVector<arglist_t>* get_args() override;

 private:
  /* Types */

  struct KalmanExtraData {
    bool is_zinger_deletion = false;
  };

  /* Constants */

  // Constants for Kalman filter tuning

  // A high initial uncertainty allows the filter to converge quickly to the measured state.
  static constexpr double INITIAL_UNCERTAINTY = 1000.0;

  // Scales the measurement noise covariance (R). A larger value makes the filter
  // trust the raw measurements less and produces a smoother output.
  static constexpr double MEASUREMENT_NOISE_SCALE = 0.1;

  // Scales the process noise on position (in Q). This represents the expected
  // variance of the acceleration. A larger value allows for faster position changes.
  static constexpr double POSITION_PROCESS_NOISE_SCALE = 0.001;

  // Scales the process noise on velocity (in Q). This represents the expected
  // variance of the jerk (rate of change of acceleration). A larger value allows
  // for faster velocity changes.
  static constexpr double VELOCITY_PROCESS_NOISE_SCALE = 0.1;

  static constexpr double COORDINATE_PRECISION_FACTOR = 1e7;
  static constexpr int STATE_SIZE = 6;   // [x,y,z,vx,vy,vz]
  static constexpr int MEAS_SIZE  = 3;   // [x,y,z] measurements

  /* Member Functions */

  void kalman_point_cb(Waypoint* wpt);
  static double median(std::vector<double>& samples);

  /* Data Members */

  bool is_initialized_{false};
  bool initial_velocity_estimated_{false};
  QDateTime last_timestamp_;
  gpsbabel::NVector last_nvector_;

  // Kalman filter matrices
  Matrix x_{6, 1};  // State vector [x, y, z, vel_x, vel_y, vel_z]
  Matrix P_{6, 6};  // Covariance matrix
  Matrix F_{6, 6};  // State transition matrix
  Matrix H_{3, 6};  // Measurement matrix
  Matrix R_{3, 3};  // Measurement noise covariance matrix
  Matrix Q_{STATE_SIZE, STATE_SIZE};  // Process noise covariance matrix

  // Configurable parameters
  OptionDouble gap_factor_option_;
  OptionDouble r_scale_option_;
  OptionDouble q_scale_pos_option_;
  OptionDouble q_scale_vel_option_;
  OptionDouble max_speed_option_;
  OptionString profile_option_;
  OptionDouble interp_max_dt_option_;
  OptionDouble interp_min_multiplier_option_;

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
      "10.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {"r_scale", &r_scale_option_, "Measurement noise covariance scaling factor",
      "1.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {"q_scale_pos", &q_scale_pos_option_, "Process noise covariance scaling factor for position",
      "1.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {"q_scale_vel", &q_scale_vel_option_, "Process noise covariance scaling factor for velocity",
      "1.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {"max_speed", &max_speed_option_, "Maximum reasonable speed in m/s for pre-filtering",
      "15.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {"profile", &profile_option_, "Predefined profile for Kalman filter tuning (auto, walking, running, cycling, driving, flying)",
      "auto", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {"interp_max_dt", &interp_max_dt_option_, "Maximum time gap (seconds) for interpolation",
      "300.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {"interp_min_multiplier", &interp_min_multiplier_option_, "Minimum multiplier for median dt to consider interpolation",
      "1.5", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
  };

};

#endif // FILTERS_ENABLED
#endif // KALMAN_H_INCLUDED_
