#include "defs.h"
#include "explorist_ini.h"
#include "inifile.h"

static inifile_t* inifile;
static const char myname[] = "explorist";

#ifdef DEAD_CODE_IS_REBORN
static const char*
explorist_read_value(const char* section, const char* key)
{
  return inifile_readstr(inifile, section, key);
}
#endif

static mag_info*
explorist_ini_try(const char* path)
{
  char* inipath;

  xasprintf(&inipath, "%s/%s", path, "APP/Atlas.ini");
  inifile = inifile_init(QString::fromUtf8(inipath), myname);
  if (!inifile) {
    xfree(inipath);
    return nullptr;
  }

  mag_info* info = (mag_info*) xmalloc(sizeof(mag_info));
  info->geo_path = nullptr;
  info->track_path = nullptr;
  info->waypoint_path = nullptr;

  char* s = xstrdup(inifile_readstr(inifile,  "UGDS", "WpFolder"));
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
  xfree(inipath);
  return info;
}

mag_info*
explorist_ini_get(const char** dirlist)
{
  mag_info* r = nullptr;
  while (dirlist && *dirlist) {
    r = explorist_ini_try(*dirlist);
    if (r) {
      return r;
    }
  }
  return r;
}

void
explorist_ini_done(mag_info* info)
{
  xfree(info->geo_path);
  xfree(info->track_path);
  xfree(info->waypoint_path);
  xfree(info);
}
