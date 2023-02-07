
/*
 * Interesting traits of the device from the *.ini files.
 */
struct mag_info {
  char* geo_path;
  char* track_path;
  char* waypoint_path;
};

mag_info* explorist_ini_get(const char** directory_list);
void explorist_ini_done(mag_info* info);
