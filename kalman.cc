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

#include "kalman.h"

#include <algorithm>            // for max, nth_element
#include <cmath>                // for round, floor
#include <iterator>             // for prev
#include <utility>              // for as_const
#include <vector>               // for vector

#include <QDebug>               // for QDebug
#include <QHash>                // for QHash
#include <QTextStream>          // for qSetRealNumberPrecision, fixed
#include <QtGlobal>             // for qDebug, qRound64, qint64

#include "defs.h"               // Waypoint, WaypointList, route_head, wp_flags, RouteList, global_options, global_opts, gbFatal, track_add_wpt, unknown_alt, arglist_t
#include "option.h"             // for OptionDouble, OptionString
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for FatalMsg
#include "src/core/nvector.h"   // for NVector
#include "src/core/vector3d.h"  // for Vector3D

// Constants

// static constexpr int debugLevelInfo = 1;
// static constexpr int debugLevelDebug = 2;
// static constexpr int debugLevelInterpolate = 7;

extern RouteList* global_track_list;

#if FILTERS_ENABLED

QVector<arglist_t>* Kalman::get_args() {
    return &args;
}

void Kalman::process() {
    struct profile_params_t {
        double max_speed;     // meters/second
        double r_scale;
        double q_scale_pos;
        double q_scale_vel;
        double interp_max_dt; // seconds
        double interp_min_multiplier;
    };
    static const QHash<QString, profile_params_t> profile_params = {
    //    profile     max_speed r_scale q_scale_pos q_scal_vel interp_max_dt interp_min_multiplier
        {"walking", {       2.0,  100.0,       0.01,     0.001,         60.0,                  2.0}},
        {"running", {      10.0,   10.0,        0.1,      0.01,        120.0,                  1.8}},
        {"cycling", {      30.0,    1.0,        1.0,       0.1,        300.0,                  1.5}},
        {"driving", {      50.0,    0.1,       10.0,       1.0,        600.0,                  1.2}},
        {"flying",  {     300.0,   0.01,      100.0,      10.0,       3600.0,                  1.1}}
    };

    extern RouteList* global_track_list;
    if (!global_track_list || global_track_list->empty()) return;

    // Tunable thresholds
    max_speed_ = max_speed_option_.get_result();   // m/s (~54 km/h, fast cycling)
    r_scale_ = r_scale_option_.get_result();
    q_scale_pos_ = q_scale_pos_option_.get_result();
    q_scale_vel_ = q_scale_vel_option_.get_result();
    gap_factor_ = gap_factor_option_.get_result();
    interp_max_dt_ = interp_max_dt_option_.get_result();
    interp_min_multiplier_ = interp_min_multiplier_option_.get_result();

    for (const auto& rte : std::as_const(*global_track_list)) {
        // Per-track state initialization
        is_initialized_ = false;
        initial_velocity_estimated_ = false;
        last_timestamp_ = QDateTime();
        last_nvector_ = gpsbabel::NVector();
        x_ = Matrix(STATE_SIZE, 1);
        P_ = Matrix::identity(STATE_SIZE) * INITIAL_UNCERTAINTY;
        F_ = Matrix::identity(STATE_SIZE);
        H_ = Matrix(MEAS_SIZE, STATE_SIZE);
        H_(0, 0) = 1.0;
        H_(1, 1) = 1.0;
        H_(2, 2) = 1.0;
        // Q_ matrix initialization moved to kalman_point_cb()
        // Q_ = Matrix::identity(STATE_SIZE);
        // Q_(0, 0) = 1e-3 * q_scale_pos;
        // Q_(1, 1) = 1e-3 * q_scale_pos;
        // Q_(2, 2) = 1e-3 * q_scale_pos;
        // Q_(3, 3) = 1e-1 * q_scale_vel;
        // Q_(4, 4) = 1e-1 * q_scale_vel;
        // Q_(5, 5) = 1e-1 * q_scale_vel;

        // Calculate track statistics for auto-profile inference
        std::vector<double> speed_samples_for_stats;
        QDateTime prev_time_for_stats;
        gpsbabel::NVector prev_nvector_for_stats;
        bool first_point_for_stats = true;

        for (const auto& wpt_stats : std::as_const(rte->waypoint_list)) {
            const QDateTime cur_time_for_stats = wpt_stats->GetCreationTime();
            const gpsbabel::NVector cur_nvector_for_stats(wpt_stats->latitude, wpt_stats->longitude);

            // FIXME: Enforce dt > 0 and all points have time?
            if (!first_point_for_stats) {
                const double dt = prev_time_for_stats.msecsTo(cur_time_for_stats) / 1000.0;
                if (dt > 0.0) {
                    const double dist = gpsbabel::NVector::euclideanDistance(prev_nvector_for_stats, cur_nvector_for_stats);
                    const double speed = dist / dt;
                    speed_samples_for_stats.push_back(speed);
                }
            } else {
                first_point_for_stats = false;
            }
            prev_time_for_stats = cur_time_for_stats;
            prev_nvector_for_stats = cur_nvector_for_stats;
        }
        double median_speed = speed_samples_for_stats.empty()? 0.0 : median(speed_samples_for_stats);

        // Auto-profile inference logic (moved here)
        QString current_profile = profile_option_.get(); // Use a new variable to avoid confusion
        if (current_profile == "auto") {
            if (median_speed <= 2.0) { // Walking speed
                current_profile = "walking";
            } else if (median_speed <= 5.0) { // Running speed
                current_profile = "running";
            } else if (median_speed <= 20.0) { // Cycling speed
                current_profile = "cycling";
            } else if (median_speed <= 50.0) { // Driving speed
                current_profile = "driving";
            } else { // Flying speed
                current_profile = "flying";
            }
        }

        if (profile_params.contains(current_profile)) {
            auto params = profile_params.value(current_profile);
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = params.max_speed;
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = params.r_scale;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = params.q_scale_pos;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = params.q_scale_vel;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = params.interp_max_dt;
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = params.interp_min_multiplier;
            }
        } else {
             gbFatal(FatalMsg() << "profile" << current_profile << "is not recognized.");
        }

        if (global_opts.debug_level >= 1) {
            qDebug().nospace() << "Using profile " << current_profile
                               << " with max_speed " << max_speed_ << (max_speed_option_.isDefaulted()? "":"*")
                               << ", r_scale " << r_scale_ << (r_scale_option_.isDefaulted()? "":"*")
                               << ", q_scale_pos " << q_scale_pos_ << (q_scale_pos_option_.isDefaulted()? "":"*")
                               << ", q_scal_vel " << q_scale_vel_ << (q_scale_vel_option_.isDefaulted()? "":"*")
                               << ", interp_max_dt " << interp_max_dt_ << (interp_max_dt_option_.isDefaulted()? "":"*")
                               << ", interp_min_multiplier " << interp_min_multiplier_ << (interp_min_multiplier_option_.isDefaulted()? "":"*")
                               << " (* default overridden)";
            qDebug().nospace() << "Using gap_factor " << gap_factor_ << (gap_factor_option_.isDefaulted()? "":"*") << " (* default overridden)";
        }

        R_ = Matrix::identity(MEAS_SIZE) * MEASUREMENT_NOISE_SCALE * r_scale_;

        // ---------------------------
        // 1) First pass: detect spikes and gaps, mark culprit waypoint(s)
        // ---------------------------
        enum class PreFilterState { NORMAL, RECOVERY, FIRST_GOOD_SEEN_IN_RECOVERY };
        auto state_name_lambda = [](PreFilterState state)->QString {
            if (state == PreFilterState::NORMAL) {
                return "NORMAL";
            } else if (state == PreFilterState::RECOVERY) {
                return "RECOVERY";
            } else if (state == PreFilterState::FIRST_GOOD_SEEN_IN_RECOVERY) {
                return "FIRST GOOD SEEN IN RECOVERY";
            } else {
                return "UNKNOWN STATE";
            }
        };
        PreFilterState state = PreFilterState::NORMAL;
        Waypoint* last_accepted_wpt = nullptr;

        for (auto it = rte->waypoint_list.begin(); it != rte->waypoint_list.end(); ++it) {
            auto* const current_wpt = *it;

            // Ensure KalmanExtraData is attached
            if (current_wpt->extra_data == nullptr) {
                current_wpt->extra_data = new KalmanExtraData();
            }
            auto* extra_data = static_cast<KalmanExtraData*>(current_wpt->extra_data);
            // Initialize or reset zinger flag for all points initially
            extra_data->is_zinger_deletion = false;

            if (last_accepted_wpt == nullptr) {
                last_accepted_wpt = current_wpt;
                state = PreFilterState::NORMAL;
                continue;
            }

            if (state == PreFilterState::NORMAL) {
                const double dt = last_accepted_wpt->GetCreationTime().msecsTo(current_wpt->GetCreationTime()) / 1000.0;
                // FIXME: dt limit violated by subsecond sampling
                const double speed = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(last_accepted_wpt->latitude, last_accepted_wpt->longitude),
                                               gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(MIN_DT, dt);

                if (dt >= gap_factor_ || speed > max_speed_) {
                    current_wpt->wpt_flags.marked_for_deletion = true;
                    extra_data->is_zinger_deletion = true; // Mark as zinger deletion
                    if (global_opts.debug_level >= 5) {
                        auto dbg = qDebug();
                        dbg << "[DEL0] deleted point at" << current_wpt->GetCreationTime().toString() << current_wpt->shortname
                            << Qt::fixed << qSetRealNumberPrecision(7)
                            << "lat:" << current_wpt->latitude << "lon:" << current_wpt->longitude << state_name_lambda(state);
                        dbg.nospace() << qSetRealNumberPrecision(4);
                        if (dt >= gap_factor_) {
                            dbg << "delta t (" << dt << ") >= gap factor (" << gap_factor_ << ") ";
                        }
                        if (speed > max_speed_) {
                            dbg << "speed (" << speed << ") > max speed (" << max_speed_ << ") ";
                        }
                    }
                    state = PreFilterState::RECOVERY;
                    // last_accepted_wpt remains unchanged as the anchor
                } else {
                    last_accepted_wpt = current_wpt;
                }
            } else { // RECOVERY or FIRST_GOOD_SEEN_IN_RECOVERY
                // FIXME: Should an isolated single "Spike: sudden, unrealistic movement" result in 3 points being tossed?
                const auto* const prev_wpt_in_list = *std::prev(it);
                const double dt_consecutive = prev_wpt_in_list->GetCreationTime().msecsTo(current_wpt->GetCreationTime()) / 1000.0;
                // FIXME: dt limit violated by subsecond sampling
                const double speed_consecutive = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(prev_wpt_in_list->latitude, prev_wpt_in_list->longitude),
                                                           gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(MIN_DT, dt_consecutive);

                // Recalculate dt from anchor for speed_from_anchor
                const double dt_from_anchor = last_accepted_wpt->GetCreationTime().msecsTo(current_wpt->GetCreationTime()) / 1000.0;
                // FIXME: dt limit violated by subsecond sampling
                const double speed_from_anchor = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(last_accepted_wpt->latitude, last_accepted_wpt->longitude),
                                                    gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(MIN_DT, dt_from_anchor);
                if (dt_consecutive > gap_factor_ || speed_consecutive > max_speed_ || speed_from_anchor > max_speed_) {
                    current_wpt->wpt_flags.marked_for_deletion = true;
                    extra_data->is_zinger_deletion = true; // Mark as zinger deletion
                        if (global_opts.debug_level >= 5) {
                            auto dbg = qDebug();
                            dbg << "[DEL1] deleted point at" << current_wpt->GetCreationTime().toString() << current_wpt->shortname
                                << Qt::fixed << qSetRealNumberPrecision(7)
                                << "lat:" << current_wpt->latitude << "lon:" << current_wpt->longitude << state_name_lambda(state);
                            dbg.nospace() << qSetRealNumberPrecision(4);
                            if (dt_consecutive > gap_factor_) {
                                dbg << "delta t consecutive (" << dt_consecutive << ") > gap factor (" << gap_factor_ << ") ";
                            }
                            if (speed_consecutive > max_speed_) {
                                dbg << "speed consecutive (" << speed_consecutive << ") > max speed (" << max_speed_ << ") ";
                            }
                            if (speed_from_anchor > max_speed_) {
                                dbg << "speed from anchor (" << speed_from_anchor << ") > max speed (" << max_speed_ << ") ";
                            }
                        }
                    state = PreFilterState::RECOVERY;
                } else {
                    if (state == PreFilterState::RECOVERY) {
                        current_wpt->wpt_flags.marked_for_deletion = true;
                        extra_data->is_zinger_deletion = true; // Mark as zinger deletion
                        if (global_opts.debug_level >= 5) {
                            qDebug() << "[DEL2] deleted point at" << current_wpt->GetCreationTime().toString() << current_wpt->shortname
                                     << Qt::fixed << qSetRealNumberPrecision(7)
                                     << "lat:" << current_wpt->latitude << "lon:" << current_wpt->longitude << state_name_lambda(state);
                        }
                        last_accepted_wpt = current_wpt;
                        state = PreFilterState::FIRST_GOOD_SEEN_IN_RECOVERY;
                    } else { // FIRST_GOOD_SEEN_IN_RECOVERY
                        last_accepted_wpt = current_wpt;
                        state = PreFilterState::NORMAL;
                    }
                }
            }
        }



        // ---------------------------
        // 2) Second pass: compute median dt and interpolate moderate gaps
        // ---------------------------
        // Collect dt samples from non-deleted points for median_dt calculation
        std::vector<double> dt_samples;
        Waypoint* last_valid_for_dt = nullptr;
        for (const auto& current_wpt : std::as_const(rte->waypoint_list)) {
            if (!current_wpt->wpt_flags.marked_for_deletion) {
                if (last_valid_for_dt) {
                    double dt = last_valid_for_dt->GetCreationTime().msecsTo(current_wpt->GetCreationTime());
                    if (dt > 0) dt_samples.push_back(dt);
                }
                last_valid_for_dt = current_wpt;
            }
        }

        double median_dt = dt_samples.empty()? 1000.0 : median(dt_samples);

        // Interpolate moderate gaps and build new route
        // Steal all the waypoints.
        WaypointList orig_wpt_list;
        track_swap_wpts(rte, orig_wpt_list);
        Waypoint* last_kept_for_interp = nullptr;

        int i = -1;
        // And add them back, with interpolated points interspersed.
        for (const auto& current_original_wpt : std::as_const(orig_wpt_list)) {
            i++;

            if (current_original_wpt->wpt_flags.marked_for_deletion) {
                auto extra_data = static_cast<KalmanExtraData*>(current_original_wpt->extra_data);
                // FIXME: marked_for_deletion <==> is_zinger deletion.
                // Interpolate to estimate missing data, or alternatively just
                // let the kalman filter predict across the gap.
                if (extra_data && extra_data->is_zinger_deletion) {
                    // This is a zinger deletion, do not interpolate across it.
                    // Reset last_kept_for_interp to effectively start a new segment.
                    last_kept_for_interp = nullptr;
                } else {
                    // This is a marked point but NOT a zinger.
                    // We keep last_kept_for_interp so that the next non-deleted
                    // point can be interpolated against the previous one.
                }
            } else {
                // This is a good point, consider interpolation if there was a previous kept point
                if (last_kept_for_interp) {
                    const qint64 gap = last_kept_for_interp->GetCreationTime().msecsTo(current_original_wpt->GetCreationTime());

                    if (gap >= interp_min_multiplier_ * median_dt && gap <= interp_max_dt_ * 1000.0) {
                        const int n_insert = static_cast<int>(std::floor(gap / median_dt)) - 1;
                        if (n_insert > 0) {
                            const double last_alt = last_kept_for_interp->altitude;
                            const double current_alt = current_original_wpt->altitude;
                            const bool can_interp_alt = (last_alt != unknown_alt && current_alt != unknown_alt);

                            for (int k = 1; k <= n_insert; ++k) {
                                const double frac = double(k) / (n_insert + 1);

                                // Linear interpolation in NVector space
                                const gpsbabel::NVector na(last_kept_for_interp->latitude, last_kept_for_interp->longitude);
                                const gpsbabel::NVector nb(current_original_wpt->latitude, current_original_wpt->longitude);
                                const gpsbabel::NVector interpolated_nvector = gpsbabel::NVector::linepart(na, nb, frac);

                                const QDateTime gen_time = last_kept_for_interp->GetCreationTime().addMSecs(qRound64(frac * gap));

                                auto* const new_wpt = new Waypoint();
                                new_wpt->latitude = interpolated_nvector.latitude();
                                new_wpt->longitude = interpolated_nvector.longitude();
                                if (can_interp_alt) {
                                  new_wpt->altitude = last_alt + frac * (current_alt - last_alt);
                                }
                                new_wpt->SetCreationTime(gen_time);
                                new_wpt->shortname = "interpolated" + QString::number(i) + "-" + QString::number(k);
                                new_wpt->extra_data = new KalmanExtraData(); // Default to false

                                track_add_wpt(rte, new_wpt);
                                if (global_opts.debug_level >= 5) {
                                    qDebug() << "[GEN] interpolated point at" << gen_time.toString() << new_wpt->shortname
                                             << Qt::fixed << qSetRealNumberPrecision(7)
                                             << "lat:" << new_wpt->latitude << "lon:" << new_wpt->longitude;
                                }
                            }
                        }
                    }
                }
                last_kept_for_interp = current_original_wpt;
            }
            track_add_wpt(rte, current_original_wpt); // Add back original point, even if marked for deletion.
        }

        // Clean up the extra_data that we allocated.
        for (const auto& wpt : std::as_const(rte->waypoint_list)) {
            if (wpt->extra_data) {
                delete static_cast<KalmanExtraData*>(wpt->extra_data);
                wpt->extra_data = nullptr;
            }
        }

        // Delete any waypoints marked for deletion.
        track_del_marked_wpts(rte);

        // Now apply Kalman filter to the cleaned and interpolated data
        for (const auto& wpt : std::as_const(rte->waypoint_list)) {
            kalman_point_cb(wpt);
        }

    }
}

void Kalman::kalman_point_cb(Waypoint* wpt) {
    // FIXME: Quote reference
    const gpsbabel::NVector current_nvector(wpt->latitude, wpt->longitude);
    const QDateTime current_timestamp = wpt->GetCreationTime();

    if (!is_initialized_) {
        // First point: store it and wait for the second point to estimate initial velocity
        last_nvector_ = current_nvector;
        last_timestamp_ = current_timestamp;
        is_initialized_ = true;
        return;
    }

    const double dt = last_timestamp_.msecsTo(current_timestamp) / 1000.0;

    // If dt is zero, skip this point to avoid division by zero and infinite velocity.
    if (dt < MIN_DT) {
        return;
    }

    // Initialize Q_ (process noise covariance) adaptively with dt
    Q_ = Matrix::identity(STATE_SIZE);
    Q_(0, 0) = POSITION_PROCESS_NOISE_SCALE * q_scale_pos_ * dt * dt; // Position uncertainty grows with dt^2
    Q_(1, 1) = POSITION_PROCESS_NOISE_SCALE * q_scale_pos_ * dt * dt;
    Q_(2, 2) = POSITION_PROCESS_NOISE_SCALE * q_scale_pos_ * dt * dt;
    Q_(3, 3) = VELOCITY_PROCESS_NOISE_SCALE * q_scale_vel_ * dt;     // Velocity uncertainty grows with dt
    Q_(4, 4) = VELOCITY_PROCESS_NOISE_SCALE * q_scale_vel_ * dt;
    Q_(5, 5) = VELOCITY_PROCESS_NOISE_SCALE * q_scale_vel_ * dt;

    if (!initial_velocity_estimated_) {
        // Second point: estimate initial velocity and initialize state and covariance
        const gpsbabel::Vector3D delta_nvector = current_nvector - last_nvector_;

        x_(0, 0) = last_nvector_.getx();
        x_(1, 0) = last_nvector_.gety();
        x_(2, 0) = last_nvector_.getz();
        x_(3, 0) = delta_nvector.getx() / dt;
        x_(4, 0) = delta_nvector.gety() / dt;
        x_(5, 0) = delta_nvector.getz() / dt;

        // Initialize P_ with some uncertainty for position and velocity
        P_ = Matrix::identity(6) * INITIAL_UNCERTAINTY;

        initial_velocity_estimated_ = true;
    } else {
        // Subsequent points: perform predict and update steps
        // Update state transition matrix
        F_(0, 3) = dt;
        F_(1, 4) = dt;
        F_(2, 5) = dt;

        // Predict
        x_ = F_ * x_;
        P_ = F_ * P_ * F_.transpose() + Q_;
    }

    // Update
    Matrix z(3, 1);
    z(0, 0) = current_nvector.getx();
    z(1, 0) = current_nvector.gety();
    z(2, 0) = current_nvector.getz();

    // Chi-squared threshold for 3 degrees of freedom, p=0.01 (approx 11.34)
    const double CHI_SQUARED_THRESHOLD = 11.34;

    const Matrix y = z - (H_ * x_);

    const Matrix S = H_ * P_ * H_.transpose() + R_;

    const Matrix S_inv = S.inverse();

    const Matrix d_squared_matrix = y.transpose() * S_inv * y;
    const double d_squared = d_squared_matrix(0, 0);

    if (d_squared < CHI_SQUARED_THRESHOLD) {
      const Matrix K = P_ * H_.transpose() * S_inv;

      x_ = x_ + (K * y);
      P_ = (Matrix::identity(6) - (K * H_)) * P_;
    }

    gpsbabel::Vector3D filtered_position(x_(0, 0), x_(1, 0), x_(2, 0));
    filtered_position.normalize(); // Ensure it's a unit vector
    const gpsbabel::NVector filtered_nvector(filtered_position);

    // FIXME: Quit adding quantization noise to the filter output.
    wpt->latitude= std::round(filtered_nvector.latitude() * COORDINATE_PRECISION_FACTOR) / COORDINATE_PRECISION_FACTOR;
    wpt->longitude = std::round(filtered_nvector.longitude() * COORDINATE_PRECISION_FACTOR) / COORDINATE_PRECISION_FACTOR;

    // Update for next iteration
    last_timestamp_ = current_timestamp;
    last_nvector_ = current_nvector;
}

double Kalman::median(std::vector<double>& samples)
{
    /* Note nth_element modifies the samples vector! */
    if (!samples.empty()) {
        auto n = samples.size();
        if (n % 2 == 0) {
            // Even number of samples, average middle two elements.
            std::nth_element(samples.begin(), samples.begin() + n / 2, samples.end());
            auto last = samples[n / 2];
            std::nth_element(samples.begin(), samples.begin() + n / 2 - 1, samples.end());
            auto first = samples[n / 2 - 1];
            return (first + last) / 2.0;

        } else {
            // Odd number of samples, return middle element.
            std::nth_element(samples.begin(), samples.begin() + n / 2, samples.end());
            return samples[n / 2];
        }
    }
    gbFatal("Attempt to compute median without any samples.");
}
#endif // FILTERS_ENABLED
