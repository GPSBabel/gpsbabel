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
#include "defs.h"
#include "option.h"
#include "session.h"
#include "src/core/logging.h"
#include "src/core/nvector.h"
#include "src/core/vector3d.h"
#include <QDebug>
#include <deque>
#include <numeric>

// Constants
static constexpr int STATE_SIZE = 6;   // [x,y,z,vx,vy,vz]
static constexpr int MEAS_SIZE  = 3;   // [x,y,z] measurements

static constexpr int debugLevelInfo = 1;
static constexpr int debugLevelDebug = 2;
static constexpr int debugLevelInterpolate = 7;


extern RouteList* global_track_list;

#if FILTERS_ENABLED

Kalman::Kalman()
  : is_initialized_(false),
    initial_velocity_estimated_(false),
    last_timestamp_(),
    last_nvector_(),

    x_(6, 1),
    P_(6, 6),
    F_(6, 6),
    H_(3, 6),
    R_(3, 3),
    Q_(STATE_SIZE, STATE_SIZE),
    r_scale_option_(),
    q_scale_pos_option_(),
    q_scale_vel_option_()
{
//	global_opts.debug_level = debugLevelInfo; // TODO: get from options
	    global_opts.debug_level = std::max(global_opts.debug_level, debugLevelInfo); // TODO: get from options
}

void Kalman::init() {
  // All initialization is now done per-track in process().
}

void Kalman::deinit() {
	gbDebug(debugLevelInfo) << "Kalman::deinit()";
  // Nothing to do.
}

QVector<arglist_t>* Kalman::get_args() {
    return &args;
}

void Kalman::process() {
    extern RouteList* global_track_list;
    if (!global_track_list || global_track_list->empty()) return;

    // Tunable thresholds
    r_scale_ = r_scale_option_.get_result();
    q_scale_pos_ = q_scale_pos_option_.get_result();
    q_scale_vel_ = q_scale_vel_option_.get_result();
    interp_max_dt_ = interp_max_dt_option_.get_result();
    interp_min_multiplier_ = interp_min_multiplier_option_.get_result();
    gap_factor_ = gap_factor_option_.get_result();

    // constexpr double INTERP_MAX_DT = 300.0;         // don't try to interpolate gaps > this (seconds)
    // constexpr double INTERP_MIN_MULTIPLIER = 1.5;   // gap must be >= multiplier * median_dt to consider interpolation

    for (RouteList::iterator route_it = global_track_list->begin();
         route_it != global_track_list->end(); ++route_it) {

        // Per-track state initialization
        is_initialized_ = false;
        initial_velocity_estimated_ = false;
        last_timestamp_ = QDateTime();
        last_nvector_ = gpsbabel::NVector();
        x_ = Matrix(STATE_SIZE, 1);
        P_ = Matrix::identity(STATE_SIZE) * 1000.0;
        F_ = Matrix::identity(STATE_SIZE);
        H_ = Matrix(MEAS_SIZE, STATE_SIZE);
        H_(0, 0) = 1.0;
        H_(1, 1) = 1.0;
        H_(2, 2) = 1.0;
        R_ = Matrix::identity(MEAS_SIZE) * 1e-1 * r_scale_;
        // Q_ matrix initialization moved to kalman_point_cb()
        // Q_ = Matrix::identity(STATE_SIZE);
        // Q_(0, 0) = 1e-3 * q_scale_pos;
        // Q_(1, 1) = 1e-3 * q_scale_pos;
        // Q_(2, 2) = 1e-3 * q_scale_pos;
        // Q_(3, 3) = 1e-1 * q_scale_vel;
        // Q_(4, 4) = 1e-1 * q_scale_vel;
        // Q_(5, 5) = 1e-1 * q_scale_vel;

        WaypointList* wpt_list = &((*route_it)->waypoint_list);


        // Calculate track statistics for auto-profile inference
        double total_speed = 0.0;
        double max_speed_observed = 0.0;
        std::vector<double> dt_samples_for_stats;
        QDateTime prev_time_for_stats;
        gpsbabel::NVector prev_nvector_for_stats;
        bool first_point_for_stats = true;

        for (auto it_stats = wpt_list->begin(); it_stats != wpt_list->end(); ++it_stats) {
            Waypoint* wpt_stats = *it_stats;
            QDateTime cur_time_for_stats = wpt_stats->GetCreationTime();
            gpsbabel::NVector cur_nvector_for_stats(wpt_stats->latitude, wpt_stats->longitude);

            if (!first_point_for_stats) {
                double dt_stats = prev_time_for_stats.secsTo(cur_time_for_stats);
                if (dt_stats > 0) {
                    double dist_stats = gpsbabel::NVector::euclideanDistance(prev_nvector_for_stats, cur_nvector_for_stats);
                    double speed_stats = dist_stats / dt_stats;
                    gbDebug(debugLevelDebug) << "[TRACK_STATS] dt_stats:" << dt_stats << ", speed_stats:" << speed_stats;
                    total_speed += speed_stats;
                    if (speed_stats > max_speed_observed) {
                        max_speed_observed = speed_stats;
                    }
                    dt_samples_for_stats.push_back(dt_stats);
                }
            } else {
                first_point_for_stats = false;
            }
            prev_time_for_stats = cur_time_for_stats;
            prev_nvector_for_stats = cur_nvector_for_stats;
        }

        double average_speed = dt_samples_for_stats.empty() ? 0.0 : total_speed / dt_samples_for_stats.size();
        double median_dt_for_stats = 1.0;
        if (!dt_samples_for_stats.empty()) {
            std::sort(dt_samples_for_stats.begin(), dt_samples_for_stats.end());
            size_t mid = dt_samples_for_stats.size() / 2;
            median_dt_for_stats = (dt_samples_for_stats.size() % 2 == 0) ?
                                  (dt_samples_for_stats[mid-1] + dt_samples_for_stats[mid])/2.0 : dt_samples_for_stats[mid];
            if (median_dt_for_stats < 1e-6) median_dt_for_stats = 1.0;
        }

        gbDebug(debugLevelInfo) << "[INFO] Track Stats - Avg Speed:" << average_speed << "m/s, Max Speed:" << max_speed_observed << "m/s, Median dt:" << median_dt_for_stats << "s";

        // Auto-profile inference logic (moved here)
        QString current_profile = profile_option_.get(); // Use a new variable to avoid confusion
        if (!max_speed_option_.has_value() && current_profile == "auto") {
            if (average_speed <= 2.0) { // Walking speed
                current_profile = "walking";
            } else if (average_speed <= 5.0) { // Running speed
                current_profile = "running";
            } else if (average_speed <= 15.0) { // Cycling speed
                current_profile = "cycling";
            } else if (average_speed <= 50.0) { // Driving speed
                current_profile = "driving";
            } else { // Flying speed
                current_profile = "flying";
            }
            gbDebug(debugLevelInfo) << "[INFO] Auto-inferred profile:" << current_profile;
        }
        gbDebug(debugLevelDebug) << "[DEBUG] Profile after inference:" << current_profile; // Added debug line

        // Apply profile settings based on current_profile
        if (current_profile == "walking") {
            if (!max_speed_option_.has_value()) max_speed_ = 2.0; // e.g., 7.2 km/h
            r_scale_ = 100.0;
            q_scale_pos_ = 0.01;
            q_scale_vel_ = 0.001;
            gap_factor_ = 5.0;
            interp_max_dt_ = 120.0; // Shorter gaps for walking
            interp_min_multiplier_ = 2.0;
        } else if (current_profile == "running") { // New running profile
            if (!max_speed_option_.has_value()) max_speed_ = 15.0; // e.g., 18 km/h
            r_scale_ = 10.0;
            q_scale_pos_ = 0.1;
            q_scale_vel_ = 0.01;
            gap_factor_ = 10.0;
            interp_max_dt_ = 180.0; // Moderate gaps for running
            interp_min_multiplier_ = 1.8;
        } else if (current_profile == "cycling") {
            if (!max_speed_option_.has_value()) max_speed_ = 15.0; // e.g., 54 km/h
            r_scale_ = 1.0;
            q_scale_pos_ = 1.0;
            q_scale_vel_ = 0.1;
            gap_factor_ = 15.0;
            interp_max_dt_ = 300.0; // Standard gaps for cycling
            interp_min_multiplier_ = 1.5;
        } else if (current_profile == "driving") {
            if (!max_speed_option_.has_value()) max_speed_ = 50.0; // e.g., 180 km/h
            r_scale_ = 0.1;
            q_scale_pos_ = 10.0;
            q_scale_vel_ = 1.0;
            gap_factor_ = 20.0;
            interp_max_dt_ = 600.0; // Longer gaps for driving (tunnels)
            interp_min_multiplier_ = 1.2;
        } else if (current_profile == "flying") {
            if (!max_speed_option_.has_value()) max_speed_ = 300.0; // e.g., 1080 km/h
            r_scale_ = 0.01;
            q_scale_pos_ = 100.0;
            q_scale_vel_ = 10.0;
            gap_factor_ = 30.0;
            interp_max_dt_ = 1200.0; // Very long gaps for flying
            interp_min_multiplier_ = 1.1;
        } else { // Default to cycling if no profile or unknown profile
            if (!max_speed_option_.has_value()) max_speed_ = 15.0; // e.g., 54 km/h
            r_scale_ = 1.0;
            q_scale_pos_ = 1.0;
            q_scale_vel_ = 0.1;
            gap_factor_ = 15.0;
            interp_max_dt_ = 300.0;
            interp_min_multiplier_ = 1.5;
            gbDebug(debugLevelInfo) << "[INFO] Unknown profile or no profile specified, defaulting to cycling.";
        }

        // Apply user-provided max_speed if available
        if (max_speed_option_.has_value()) {
            max_speed_ = max_speed_option_.get_result();
        }

        // Apply user-provided max_speed if available
        if (max_speed_option_.has_value()) {
            max_speed_ = max_speed_option_.get_result();
        }

        // ---------------------------
        // 1) First pass: detect spikes and gaps, mark culprit waypoint(s)
        // ---------------------------
        Waypoint* last_good_wpt = nullptr;

        for (auto it = wpt_list->begin(); it != wpt_list->end(); ++it) {
            Waypoint* current_wpt = *it;

            // Ensure KalmanExtraData is attached
            if (current_wpt->extra_data == nullptr) {
                current_wpt->extra_data = new KalmanExtraData();
            }
            // Initialize or reset zinger flag for all points initially
            ((KalmanExtraData*)current_wpt->extra_data)->is_zinger_deletion = false;

            if (last_good_wpt == nullptr) {
                last_good_wpt = current_wpt;
                continue;
            }

            double dt = last_good_wpt->GetCreationTime().secsTo(current_wpt->GetCreationTime());
            double speed = gpsbabel::NVector::euclideanDistance(gpsbabel::NVector(last_good_wpt->latitude, last_good_wpt->longitude),
                                           gpsbabel::NVector(current_wpt->latitude, current_wpt->longitude)) / std::max(1.0, dt);

            qDebug() << "[PREFILTER - QDEBUG] dt:" << dt << ", speed:" << speed << ", gap_factor_:" << gap_factor_ << ", max_speed_:" << max_speed_ << ", (dt > gap_factor_ || speed > max_speed_) :" << (dt > gap_factor_ || speed > max_speed_);

            if (dt > gap_factor_ || speed > max_speed_) {
                current_wpt->wpt_flags.marked_for_deletion = true;
                ((KalmanExtraData*)current_wpt->extra_data)->is_zinger_deletion = true; // Mark as zinger deletion
                // last_good_wpt remains unchanged as the anchor
            } else {
                last_good_wpt = current_wpt;
            }
        }



        // ---------------------------
        // 2) Second pass: compute median dt and interpolate moderate gaps
        // ---------------------------
        // Collect dt samples from non-deleted points for median_dt calculation
        std::vector<double> dt_samples;
        Waypoint* last_valid_for_dt = nullptr;
        for (auto it = wpt_list->begin(); it != wpt_list->end(); ++it) {
            Waypoint* current_wpt = *it;
            if (!current_wpt->wpt_flags.marked_for_deletion) {
                if (last_valid_for_dt) {
                    double dt = last_valid_for_dt->GetCreationTime().secsTo(current_wpt->GetCreationTime());
                    if (dt > 0) dt_samples.push_back(dt);
                }
                last_valid_for_dt = current_wpt;
            }
        }

        double median_dt = 1.0;
        if (!dt_samples.empty()) {
            std::sort(dt_samples.begin(), dt_samples.end());
            size_t mid = dt_samples.size() / 2;
            median_dt = (dt_samples.size() % 2 == 0) ?
                        (dt_samples[mid-1] + dt_samples[mid])/2.0 : dt_samples[mid];
            if (median_dt < 1e-6) median_dt = 1.0;
        }

        gbDebug(debugLevelInfo) << "[INFO] route median_dt=" << median_dt;

        // Interpolate moderate gaps and build new route
        route_head* new_route_head = new route_head(); // Temporary route to build the new sequence
        Waypoint* last_kept_for_interp = nullptr;

        // Handle the first non-deleted point
        for (auto it = wpt_list->begin(); it != wpt_list->end(); ++it) {
            Waypoint* current_original_wpt = *it;
            if (!current_original_wpt->wpt_flags.marked_for_deletion) {
                track_add_wpt(new_route_head, current_original_wpt);
                last_kept_for_interp = current_original_wpt;
                break; // Found the first non-deleted point, exit loop
            }
        }

        if (!last_kept_for_interp) {
            // No valid points to keep after pre-filtering, continue to next route
            delete new_route_head;
            continue;
        }

        // Iterate through the rest of the original wpt_list
        for (auto it = wpt_list->begin(); it != wpt_list->end(); ++it) {
            Waypoint* current_original_wpt = *it;

            if (current_original_wpt == last_kept_for_interp) {
                continue; // Skip the first point, already handled
            }

            if (current_original_wpt->wpt_flags.marked_for_deletion) {
                KalmanExtraData* extra_data = (KalmanExtraData*)current_original_wpt->extra_data;
                if (extra_data && extra_data->is_zinger_deletion) {
                    // This is a zinger deletion, do not interpolate across it.
                    // Reset last_kept_for_interp to effectively start a new segment.
                    last_kept_for_interp = nullptr;
                    gbDebug(debugLevelInterpolate) << "[INTERP] Skipping zinger deletion at" << current_original_wpt->GetCreationTime().toString();
                } else {
                    // This is a marked point but NOT a zinger.
                    // We simply skip adding it to new_route_head, but keep last_kept_for_interp
                    // so that the next non-deleted point can be interpolated against the previous one.
                    gbDebug(debugLevelInterpolate) << "[INTERP] Skipping non-zinger marked point at" << current_original_wpt->GetCreationTime().toString();
                }
            } else {
                // This is a good point, consider interpolation if there was a previous kept point
                if (last_kept_for_interp) {
                    double gap = std::abs(last_kept_for_interp->GetCreationTime().secsTo(current_original_wpt->GetCreationTime()));

                    if (gap >= interp_min_multiplier_ * median_dt && gap <= interp_max_dt_) {
                        int n_insert = static_cast<int>(std::floor(gap / median_dt)) - 1;
                        if (n_insert > 0) {
                            for (int k = 1; k <= n_insert; ++k) {
                                double frac = double(k) / (n_insert + 1);

                                // Linear interpolation in NVector space
                                gpsbabel::NVector na(last_kept_for_interp->latitude, last_kept_for_interp->longitude);
                                gpsbabel::NVector nb(current_original_wpt->latitude, current_original_wpt->longitude);
                                gpsbabel::NVector interpolated_nvector = gpsbabel::NVector::linepart(na, nb, frac);

                                QDateTime gen_time = last_kept_for_interp->GetCreationTime().addSecs(qRound(frac * gap));

                                Waypoint* new_wpt = new Waypoint();
                                new_wpt->latitude = interpolated_nvector.latitude();
                                new_wpt->longitude = interpolated_nvector.longitude();
                                new_wpt->SetCreationTime(gen_time);
                                new_wpt->shortname = "interpolated" + QString::number(std::distance(wpt_list->begin(), it)) + "-" + QString::number(k);
                                new_wpt->extra_data = new KalmanExtraData(); // Default to false

                                track_add_wpt(new_route_head, new_wpt);
                                gbDebug(debugLevelInterpolate) << "[GEN] interpolated point at" << gen_time.toString() << new_wpt->shortname
                                         << "lat:" << new_wpt->latitude << "lon:" << new_wpt->longitude;
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
        for (WaypointList::iterator it = new_route_head->waypoint_list.begin(); it != new_route_head->waypoint_list.end(); ++it) {
            Waypoint* old_wpt = *it;
            if (old_wpt->wpt_flags.marked_for_deletion) {
                if (old_wpt->extra_data) {
                    delete (KalmanExtraData*)old_wpt->extra_data;
                    old_wpt->extra_data = nullptr;
                }
                delete old_wpt;
            } else {
                // For points that were not marked for deletion, they are now owned by the new wpt_list.
                // We must set their pointer to nullptr in the old list to prevent double deletion.
                *it = nullptr;
            }
        }
        // Clear the old list (now held by new_route_head) to prevent its destructor from deleting Waypoints
        // that are now owned by the main wpt_list.
        WaypointList empty_list_for_cleanup;
        new_route_head->waypoint_list.swap(empty_list_for_cleanup);
        delete new_route_head; // Clean up temporary route_head object

        // Now apply Kalman filter to the cleaned and interpolated data
        for (WaypointList::iterator wpt_it = wpt_list->begin(); wpt_it != wpt_list->end(); ++wpt_it) {
            Waypoint* current_wpt = *wpt_it;
            if (!current_wpt->wpt_flags.marked_for_deletion) { // Skip marked for deletion
                kalman_point_cb(current_wpt);
            }
        }
    }
}

void Kalman::kalman_point_cb(Waypoint* wpt) {
    gpsbabel::NVector current_nvector(wpt->latitude, wpt->longitude);
    QDateTime current_timestamp = wpt->GetCreationTime();
    gbDebug(debugLevelDebug) << "kalman_point_cb: Wpt:" << wpt->latitude << wpt->longitude << current_timestamp.toString() << "Initialized:" << is_initialized_ << "Vel Estimated:" << initial_velocity_estimated_;

    if (!is_initialized_) {
        // First point: store it and wait for the second point to estimate initial velocity
        last_nvector_ = current_nvector;
        last_timestamp_ = current_timestamp;
        is_initialized_ = true;
        gbDebug(debugLevelDebug) << "kalman_point_cb: Initializing first point.";
        return;
    }
  
    double dt = last_timestamp_.secsTo(current_timestamp);
    gbDebug(debugLevelDebug) << "kalman_point_cb: dt:" << dt;
 
    // If dt is zero, skip this point to avoid division by zero and infinite velocity.
    if (dt == 0) {
        gbDebug(debugLevelDebug) << "kalman_point_cb: dt is zero, skipping point.";
        return;
    }

    // Initialize Q_ (process noise covariance) adaptively with dt
    Q_ = Matrix::identity(STATE_SIZE);
    Q_(0, 0) = 1e-3 * q_scale_pos_ * dt * dt; // Position uncertainty grows with dt^2
    Q_(1, 1) = 1e-3 * q_scale_pos_ * dt * dt;
    Q_(2, 2) = 1e-3 * q_scale_pos_ * dt * dt;
    Q_(3, 3) = 1e-1 * q_scale_vel_ * dt;     // Velocity uncertainty grows with dt
    Q_(4, 4) = 1e-1 * q_scale_vel_ * dt;
    Q_(5, 5) = 1e-1 * q_scale_vel_ * dt;
    gbDebug(debugLevelDebug) << "kalman_point_cb: Q_ initialized.";

    if (!initial_velocity_estimated_) {
        // Second point: estimate initial velocity and initialize state and covariance
        gpsbabel::Vector3D delta_nvector = current_nvector - last_nvector_;

        x_(0, 0) = last_nvector_.getx();
        x_(1, 0) = last_nvector_.gety();
        x_(2, 0) = last_nvector_.getz();
        x_(3, 0) = delta_nvector.getx() / dt;
        x_(4, 0) = delta_nvector.gety() / dt;
        x_(5, 0) = delta_nvector.getz() / dt;

        // Initialize P_ with some uncertainty for position and velocity
        P_ = Matrix::identity(6) * 1000.0;
        gbDebug(debugLevelDebug) << "kalman_point_cb: Initializing second point. x_:" << x_(0,0) << x_(1,0) << x_(2,0) << x_(3,0) << x_(4,0) << x_(5,0);

        initial_velocity_estimated_ = true;
    } else {
        // Subsequent points: perform predict and update steps
        // Update state transition matrix
        F_(0, 3) = dt;
        F_(1, 4) = dt;
        F_(2, 5) = dt;

        gbDebug(debugLevelDebug) << "kalman_point_cb: Before Prediction. x_:" << x_(0,0) << x_(1,0) << x_(2,0) << x_(3,0) << x_(4,0) << x_(5,0);
        // Predict
        x_ = F_ * x_;
        P_ = F_ * P_ * F_.transpose() + Q_;
        gbDebug(debugLevelDebug) << "kalman_point_cb: After Prediction. x_:" << x_(0,0) << x_(1,0) << x_(2,0) << x_(3,0) << x_(4,0) << x_(5,0);
    }

    // Update
    Matrix z(3, 1);
    z(0, 0) = current_nvector.getx();
    z(1, 0) = current_nvector.gety();
    z(2, 0) = current_nvector.getz();

    // Chi-squared threshold for 3 degrees of freedom, p=0.01 (approx 11.34)
    const double CHI_SQUARED_THRESHOLD = 11.34;

    Matrix y = z - (H_ * x_);

    Matrix S = H_ * P_ * H_.transpose() + R_;

    Matrix S_inv = S.inverse();

    Matrix d_squared_matrix = y.transpose() * S_inv * y;
    double d_squared = d_squared_matrix(0, 0);
    gbDebug(debugLevelDebug) << "kalman_point_cb: d_squared:" << d_squared << "Threshold:" << CHI_SQUARED_THRESHOLD;

    if (d_squared < CHI_SQUARED_THRESHOLD) {
      Matrix K = P_ * H_.transpose() * S_inv;

      x_ = x_ + (K * y);
      P_ = (Matrix::identity(6) - (K * H_)) * P_;
      gbDebug(debugLevelDebug) << "kalman_point_cb: After Update (accepted). x_:" << x_(0,0) << x_(1,0) << x_(2,0) << x_(3,0) << x_(4,0) << x_(5,0);
    } else {
        gbDebug(debugLevelDebug) << "kalman_point_cb: Update rejected (d_squared > threshold).";
    }

    gpsbabel::Vector3D filtered_position(x_(0, 0), x_(1, 0), x_(2, 0));
    filtered_position.normalize(); // Ensure it's a unit vector
    gpsbabel::NVector filtered_nvector(filtered_position);

    double new_lat = std::round(filtered_nvector.latitude() * 1e7) / 1e7;
    double new_lon = std::round(filtered_nvector.longitude() * 1e7) / 1e7;
    gbDebug(debugLevelDebug) << "kalman_point_cb: Filtered Lat/Lon:" << new_lat << new_lon;

    if (wpt->latitude != new_lat || wpt->longitude != new_lon) {
        wpt->latitude = new_lat;
        wpt->longitude = new_lon;
    }

    // Update for next iteration
    last_timestamp_ = current_timestamp;
    last_nvector_ = current_nvector;
}

#endif // FILTERS_ENABLED
