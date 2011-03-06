#include "defs.h"
#include "inifile.h"
#include "explorist_ini.h"

static inifile_t *inifile;
static const char myname[] = "explorist";

const char *
explorist_read_value(const char* section, const char *key) {
  return inifile_readstr(inifile, section, key);
}

mag_info *
explorist_ini_get() {
  mag_info *info = xmalloc(sizeof(mag_info));
  char *s;

  inifile = inifile_init("Geocaches.ini", myname);
  s = xstrdup(inifile_readstr(inifile, "Geocaches", "GeocachesPath"));
  s = gstrsub(s, "\\", "/");
  info->geo_path = s;
  inifile_done(inifile);

  inifile = inifile_init("Tracks.ini", myname);
  s = xstrdup(inifile_readstr(inifile, "Tracks", "TracksExportPath"));
  s = gstrsub(s, "\\", "/");
  info->track_path = xstrappend(s, "/tracks.gpx");;
  inifile_done(inifile);

  inifile = inifile_init("Waypoints.ini", myname);
  s = xstrdup(inifile_readstr(inifile, "Waypoints", "WaypointsPath"));
  s = gstrsub(s, "\\", "/");
  info->waypoint_path = xstrappend(s, "/newwaypoints.gpx");
  inifile_done(inifile);

  return info;
}

void
explorist_ini_done(mag_info *info) {
  xfree(info->geo_path);
  xfree(info->track_path);
  xfree(info->waypoint_path);
  xfree(info);
}
