#include "defs.h"
#include "inifile.h"
#include "explorist_ini.h"

static inifile_t *inifile;
static const char myname[] = "explorist";

const char *
explorist_read_value(const char* section, const char *key) {
  return inifile_readstr(inifile, section, key);
}

static mag_info *
explorist_ini_try(const char *path) {
  mag_info *info = NULL;
  char *inipath;
  char *s;

  xasprintf(&inipath, "%s/%s", path, "APP/Atlas.ini");
  inifile = inifile_init(inipath, myname);
  if (!inifile) {
    xfree (inipath);
    return NULL;
  }

  info = xmalloc(sizeof(mag_info));
  info->geo_path = NULL;
  info->track_path = NULL;
  info->waypoint_path = NULL;

  s = xstrdup(inifile_readstr(inifile,  "UGDS", "WpFolder"));
  if (s) {
    s = gstrsub(s, "\\", "/");
    xasprintf(&info->waypoint_path, "%s/%s", path, s);
  }
  s = xstrdup(inifile_readstr(inifile,  "UGDS", "GcFolder"));
  if (s) {
    s = gstrsub(s, "\\", "/");
    xasprintf(&info->geo_path, "%s/%s", path, s);
  }
  s = xstrdup(inifile_readstr(inifile,  "UGDS", "TrkFolder"));
  if (s) {
    s = gstrsub(s, "\\", "/");
    xasprintf(&info->track_path, "%s/%s", path, s);
  }

  inifile_done(inifile);
  xfree (inipath);
  return info;
}

mag_info *
explorist_ini_get(const char **dirlist) {
  mag_info *r = NULL;
  while (dirlist && *dirlist) {
    r = explorist_ini_try(*dirlist);
    if (r) return r;
  }
  return r;
}

void
explorist_ini_done(mag_info *info) {
  xfree(info->geo_path);
  xfree(info->track_path);
  xfree(info->waypoint_path);
  xfree(info);
}
