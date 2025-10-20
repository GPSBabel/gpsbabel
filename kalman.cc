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
#include <QTextStream>          // for qSetRealNumberPrecision, fixed
#include <QtGlobal>             // for qDebug, qRound64, qint64

#include "defs.h"               // for Waypoint, WaypointList, route_head, wp_flags, RouteList, global_options, global_opts, track_add_wpt, gbFatal, unknown_alt, arglist_t
#include "option.h"             // for OptionDouble, OptionString
#include "src/core/datetime.h"  // for DateTime
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
    extern RouteList* global_track_list;
    if (!global_track_list || global_track_list->empty()) return;

    // Tunable thresholds
    max_speed_ = max_speed_option_.get_result();   // m/s (~54 km/h, fast cycling)
    r_scale_ = r_scale_option_.get_result();
    q_scale_pos_ = q_scale_pos_option_.get_result();
    q_scale_vel_ = q_scale_vel_option_.get_result();
    if (gap_factor_option_.has_value()) {
      gap_factor_ = gap_factor_option_.get_result();
    } else {
      gap_factor_ = 5.0;
    }
    interp_max_dt_ = interp_max_dt_option_.get_result();
    interp_min_multiplier_ = interp_min_multiplier_option_.get_result();

    for (const auto& route_it : std::as_const(*global_track_list)) {
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

        WaypointList* wpt_list = &(route_it->waypoint_list);

        // Calculate track statistics for auto-profile inference
        std::vector<double> speed_samples_for_stats;
        QDateTime prev_time_for_stats;
        gpsbabel::NVector prev_nvector_for_stats;
        bool first_point_for_stats = true;

        for (const auto& wpt_stats : std::as_const(*wpt_list)) {
            const QDateTime cur_time_for_stats = wpt_stats->GetCreationTime();
            const gpsbabel::NVector cur_nvector_for_stats(wpt_stats->latitude, wpt_stats->longitude);

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

        // Apply profile settings based on current_profile
        if (current_profile == "walking") {
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = 2.0; // e.g., 7.2 km/h
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = 100.0;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = 0.01;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = 0.001;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = 60.0; // 1 minute
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = 2.0;
            }
        } else if (current_profile == "running") { // New running profile
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = 10.0; // e.g., 18 km/h
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = 10.0;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = 0.1;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = 0.01;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = 120.0; // 2 minutes
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = 1.8;
            }
        } else if (current_profile == "cycling") {
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = 30.0; // e.g., 108 km/h
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = 1.0;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = 1.0;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = 0.1;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = 300.0; // 5 minutes
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = 1.5;
            }
        } else if (current_profile == "driving") {
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = 50.0; // e.g., 180 km/h
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = 0.1;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = 10.0;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = 1.0;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = 600.0; // 10 minutes
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = 1.2;
            }
        } else if (current_profile == "flying") {
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = 300.0; // e.g., 1080 km/h
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = 0.01;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = 100.0;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = 10.0;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = 3600.0; // 1 hour
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = 1.1;
            }
        } else { // Default to cycling if no profile or unknown profile
            if (max_speed_option_.isDefaulted()) {
                max_speed_ = 15.0; // e.g., 54 km/h
            }
            if (r_scale_option_.isDefaulted()) {
                r_scale_ = 1.0;
            }
            if (q_scale_pos_option_.isDefaulted()) {
                q_scale_pos_ = 1.0;
            }
            if (q_scale_vel_option_.isDefaulted()) {
                q_scale_vel_ = 0.1;
            }
            if (interp_max_dt_option_.isDefaulted()) {
                interp_max_dt_ = 300.0; // 5 minutes
            }
            if (interp_min_multiplier_option_.isDefaulted()) {
                interp_min_multiplier_ = 1.5;
            }
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

        for (auto it = wpt_list->begin(); it != wpt_list->end(); ++it) {
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
                const double speed = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(last_accepted_wpt->latitude, last_accepted_wpt->longitude),
                                               gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(1.0, dt);

                if (dt >= gap_factor_ || speed > max_speed_) {
                    current_wpt->wpt_flags.marked_for_deletion = true;
                    extra_data->is_zinger_deletion = true; // Mark as zinger deletion
                    if (global_opts.debug_level >= 5) {
                        auto dbg = qDebug();
                        dbg << "[ZNG0] deleted point at" << current_wpt->GetCreationTime().toString() << current_wpt->shortname
                            << Qt::fixed << qSetRealNumberPrecision(7)
                            << "lat:" << current_wpt->latitude << "lon:" << current_wpt->longitude << state_name_lambda(state);
                        dbg.nospace() << qSetRealNumberPrecision(4);
                        if (dt >= gap_factor_) {
                            dbg << "delta t (" << dt << ") >= gap factor (" << gap_factor_ << ")";
                        }
                        if (speed > max_speed_) {
                            dbg << "speed (" << speed << ") > max speed (" << max_speed_ << ")";
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
                const double speed_consecutive = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(prev_wpt_in_list->latitude, prev_wpt_in_list->longitude),
                                                           gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(1.0, dt_consecutive);

                // Recalculate dt from anchor for speed_from_anchor
                const double dt_from_anchor = last_accepted_wpt->GetCreationTime().msecsTo(current_wpt->GetCreationTime()) / 1000.0;
                const double speed_from_anchor = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(last_accepted_wpt->latitude, last_accepted_wpt->longitude),
                                                    gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(1.0, dt_from_anchor);
                if (dt_consecutive > gap_factor_ || speed_consecutive > max_speed_ || speed_from_anchor > max_speed_) {
                    current_wpt->wpt_flags.marked_for_deletion = true;
                    extra_data->is_zinger_deletion = true; // Mark as zinger deletion
                        if (global_opts.debug_level >= 5) {
                            auto dbg = qDebug();
                            dbg << "[ZNG1] deleted point at" << current_wpt->GetCreationTime().toString() << current_wpt->shortname
                                << Qt::fixed << qSetRealNumberPrecision(7)
                                << "lat:" << current_wpt->latitude << "lon:" << current_wpt->longitude << state_name_lambda(state);
                            dbg.nospace() << qSetRealNumberPrecision(4);
                            if (dt_consecutive > gap_factor_) {
                                dbg << "delta t consecutive (" << dt_consecutive << ") > gap factor (" << gap_factor_ << ")";
                            }
                            if (speed_consecutive > max_speed_) {
                                dbg << "speed consecutive (" << speed_consecutive << ") > max speed (" << max_speed_ << ")";
                            }
                            if (speed_from_anchor > max_speed_) {
                                dbg << "speed from anchor (" << speed_from_anchor << ") > max speed (" << max_speed_ << ")";
                            }
                        }
                    state = PreFilterState::RECOVERY;
                } else {
                    if (state == PreFilterState::RECOVERY) {
                        current_wpt->wpt_flags.marked_for_deletion = true;
                        extra_data->is_zinger_deletion = true; // Mark as zinger deletion
                        if (global_opts.debug_level >= 5) {
                            qDebug() << "[ZNG2] deleted point at" << current_wpt->GetCreationTime().toString() << current_wpt->shortname
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
        for (const auto& current_wpt : std::as_const(*wpt_list)) {
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
        route_head* new_route_head = new route_head(); // Temporary route to build the new sequence
        Waypoint* last_kept_for_interp = nullptr;

        int i = -1;
        // Iterate through the original wpt_list
        for (const auto& current_original_wpt : std::as_const(*wpt_list)) {
            i++;

            if (current_original_wpt->wpt_flags.marked_for_deletion) {
                auto extra_data = static_cast<KalmanExtraData*>(current_original_wpt->extra_data);
                // FIXME: marked_for_deletion <==> is_zinger deletion.
                // FIXME: We should interpolate across zingers to estimate actual data, or alternatively
                //        modify the kalman filter to estimate missing data.
                if (extra_data && extra_data->is_zinger_deletion) {
                    // This is a zinger deletion, do not interpolate across it.
                    // Reset last_kept_for_interp to effectively start a new segment.
                    last_kept_for_interp = nullptr;
                } else {
                    // This is a marked point but NOT a zinger.
                    // We simply skip adding it to new_route_head, but keep last_kept_for_interp
                    // so that the next non-deleted point can be interpolated against the previous one.
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

                                track_add_wpt(new_route_head, new_wpt);
                                if (global_opts.debug_level >= 5) {
                                    qDebug() << "[GEN] interpolated point at" << gen_time.toString() << new_wpt->shortname
                                             << Qt::fixed << qSetRealNumberPrecision(7)
                                             << "lat:" << new_wpt->latitude << "lon:" << new_wpt->longitude;
                                }
                            }
                        }
                    }
                }
                track_add_wpt(new_route_head, current_original_wpt); // Add current good point
                last_kept_for_interp = current_original_wpt;
            }
        }


        // Swap the lists. The original route now owns the new interpolated list.
        // The old wpt_list (containing all original points, including marked ones)
        // is now held by new_route_head->waypoint_list.
        wpt_list->swap(new_route_head->waypoint_list);

        // Now, iterate through the old list (which was the original wpt_list)
        // and delete waypoints that were marked for deletion and their extra_data.
        // FIXME: use track_del_marked_wpt to forward any track segment markers.
        //        this requires changes to interpolation so the marked points are added to new_route_head.
        for (auto& wpt_ptr : new_route_head->waypoint_list) {
            Waypoint* old_wpt = wpt_ptr;
            if (old_wpt->wpt_flags.marked_for_deletion) {
                if (old_wpt->extra_data) {
                    delete static_cast<KalmanExtraData*>(old_wpt->extra_data);
                    old_wpt->extra_data = nullptr;
                }
                delete old_wpt;
            } else {
                // For points that were not marked for deletion, they are now owned by the new wpt_list.
                // We must set their pointer to nullptr in the old list to prevent double deletion.
                wpt_ptr = nullptr;
            }
        }
        // Clear the old list (now held by new_route_head) to prevent its destructor from deleting Waypoints
        // that are now owned by the main wpt_list.
        WaypointList empty_list_for_cleanup;
        new_route_head->waypoint_list.swap(empty_list_for_cleanup);
        delete new_route_head; // Clean up temporary route_head object

        // Now apply Kalman filter to the cleaned and interpolated data
        for (const auto& wpt_it : std::as_const(*wpt_list)) {
            kalman_point_cb(wpt_it);
        }

        // Clean up the extra_data that we allocated.
        for (const auto& wpt_it : std::as_const(*wpt_list)) {
            if (wpt_it->extra_data) {
                delete static_cast<KalmanExtraData*>(wpt_it->extra_data);
                wpt_it->extra_data = nullptr;
            }
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
