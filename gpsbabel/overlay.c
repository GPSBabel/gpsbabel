/*

    Geogrid-Viewer overlay file  Version 0.9.3

    A detail description of the ASCII-overlay-fomat you can find in the
    helpfile of Geogrid-Viewer.

    Latest changes at 11.01.2005 by Fredie Kern

    Copyright (C) 2005 Fredie Kern, f.kern@xdesy.de


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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
#include "defs.h"
#include "grtcirc.h"

static short_handle mkshort_handle;

#define MYNAME         "overlay"
#define PARAMETER_FILE "overlay.def"

#undef  MAPNAME
#define MAPNAME "Bundesrepublik 1:1 Mio"
#undef  MAPNAME
#define MAPNAME "Top. Karte 1:50.000 Nieders."

static gbfile* fpout;
static gbfile* fpin;
static int govl_cnt;
static double govl_sum_e=0.0;
static double govl_sum_n=0.0;
static double govl_sumcnt=0.0;
static int    govl_symbol_cnt=0;
static int    govl_group_cnt=0;
/*
static double govl_last_east=0.0;
static double govl_last_north=0.0;
*/

static int     govl_col=1;
static char*   govl_col_s = NULL;
static int     govl_size=101;
static char*   govl_size_s = NULL;
static double  govl_dir=0.0;

static char* govl_mapname = NULL;
static int   govl_zoomfc = 100;
static char* govl_zoomfc_s = NULL;
static int   govl_dimmfc = 100;
static char* govl_dimmfc_s = NULL;


static int     govl_txtcol=1;
static int     govl_txtsize=120;
static int     govl_font=1;
static int     govl_txttrans=0;

static char* govl_txtcol_s = NULL;
static char* govl_txtsize_s = NULL;
static char* govl_font_s = NULL;
static char* govl_txttrans_s = NULL;

static char* govl_file_s = NULL;

static arglist_t ovl_args[] = {
  {
    "col", &govl_col_s, "color index [1-9] for routes",
    NULL, ARGTYPE_INT, "1", "9"
  },
  {
    "size", &govl_size_s, "size index [101-] for routes",
    NULL, ARGTYPE_INT, "101", NULL
  },
  {
    "mapname", &govl_mapname, "name of map",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "zoomfc", &govl_zoomfc_s, "zoom factor of map in %",
    NULL, ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "dimmfc", &govl_dimmfc_s, "dimmer factor of map in %",
    NULL, ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "txtcol", &govl_txtcol_s, "color index [1-9] for waypoint names",
    NULL, ARGTYPE_INT, "1", "9"
  },
  {
    "txtsize", &govl_txtsize_s, "text size [101-] for waypoint names",
    NULL, ARGTYPE_INT, "101", NULL
  },
  {
    "font", &govl_font_s, "font index [1-] for waypoint names",
    NULL, ARGTYPE_INT, "1", NULL
  },
  {
    "txttrans", &govl_txttrans_s, "set text background to transparent",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "file", &govl_file_s, "use file of parameters (parameters on command line overwrites file parameters)",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};


static char* Keywords[]= {
  "Typ",
  "Group",
  "Col",
  "Zoom",
  "Size",
  "Art",
  "Punkte",
  "Path",
  "Dir",
  "Font",
  "Area",
  "Text",
  "Width",
  "Height",
  "Trans",
  "TransByte",
  NULL
};

#define KEY_TYP        0
#define KEY_GROUP      1
#define KEY_COL        2
#define KEY_ZOOM       3
#define KEY_SIZE       4
#define KEY_ART        5
#define KEY_PUNKTE     6
#define KEY_PATH       7
#define KEY_DIR        8
#define KEY_FONT       9
#define KEY_AREA      10
#define KEY_TEXT      11
#define KEY_WIDTH     12
#define KEY_HEIGHT    13
#define KEY_TRANS     14
#define KEY_TRANSBYTE 15

static int isKeyword(char* str,char** keys)
{
  int i;

  i = 0;
  while (keys[i]!=NULL && strcmp(str,keys[i])) {
    i++;
  }
  return(keys[i]==NULL ? -1 : i);
}

/*----------------------------------------------*/

static
void ovl_rd_init(char const* fname)
{
  fpin = gbfopen(fname, "r", MYNAME);
}

#define SECTION_NONE    0
#define SECTION_SYMBOL  1
#define SECTION_PUNKTE  2
#define SECTION_OVERLAY 3

#define MAXLINE 512

static  struct _group {
  int  group;
  char* name;
}* groups;
static  int    groups_cnt;

static void ovl_add_group(int aktgrp,char* akttxt)
{
  int i;

  i = 0;
  while (i<groups_cnt && groups[i].group!=aktgrp) {
    i++;
  }
  if (i==groups_cnt) {
    groups = (struct _group*) xrealloc(groups,(groups_cnt+1)*sizeof(struct _group));
    groups[i].group = aktgrp;
    groups[i].name = NULL;
    groups_cnt++;
  }
  groups[i].name = (char*) xrealloc(groups[i].name,(strlen(akttxt)+1)*sizeof(char));
  strcpy(groups[i].name,akttxt);
}

/*
  The name of route is stored in a 'Text'-symbol with identical 'Group'-number.
*/
static void route_add_name(const route_head* hd)
{
  int grp;
  int i;
  char name[MAXLINE];
  route_head* route;

  route = (route_head*) hd;
  grp = atoi(route->rte_name);
  i = 0;
  while (i<groups_cnt && groups[i].group!=grp) {
    i++;
  }
  if (i==groups_cnt) { // not found
    sprintf(name,"undef(%d)",grp); /* pseudo name*/
    sprintf(name,"?%d",grp);
  } else {
    strcpy(name,groups[i].name);
  }
  route->rte_name = (char*) xrealloc(route->rte_name,(strlen(name)+1)*sizeof(char));
  strcpy(route->rte_name,name);
}

static void ovl_read(void)
{
  char*    line;
  int     isSection;
  int     aktTyp,aktCol,aktSize,aktArt,aktGroup;
  int     aktArea,aktWidth,aktHeight,aktTrans,aktTransByte,aktDir;
  double  aktX,aktY;
  char*   aktPath;
  char*   aktText;
  char*   pstr;
  int     keyw,i;
  double  rwert;
  route_head* route_head = NULL;
  waypoint*   wpt;
  int      sym_cnt;
  int lineno = 0;

  groups = NULL;
  groups_cnt = 0;
  aktTyp = aktCol = aktSize = aktArt = aktGroup = -1;
  aktArea = aktWidth = aktHeight = aktTrans = aktTransByte = aktDir = -1;
  aktX = aktY = 0.0;
  aktText = NULL;
  aktPath = NULL;
  sym_cnt = 0;
  isSection = SECTION_NONE;
  while ((line = gbfgetstr(fpin))) {
    if ((lineno == 0) && fpin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }
    lineno++;
    line = lrtrim(line);
    if ((pstr = strstr(line,"[Symbol "))!= NULL) {
      sym_cnt++;
      isSection = SECTION_SYMBOL;
    } else if ((pstr = strstr(line,"[Overlay]"))!= NULL) {
      isSection = SECTION_OVERLAY;
    } else if (isSection==SECTION_SYMBOL) {
      pstr = strtok(line,"=");
      if (pstr!=NULL) {
        keyw = isKeyword(pstr,Keywords);
        pstr = strtok(NULL,"\n");
        if (pstr!=NULL) {
          switch (keyw) {
          case KEY_TYP     :
            aktTyp = atoi(pstr);
            break;
          case KEY_GROUP   :
            aktGroup = atoi(pstr);
            ovl_add_group(aktGroup,"?"); /* 'Group' without relation to 'Text'-Symbol */
            switch (aktTyp) {
            case 3: // Linie
              route_head = route_head_alloc();
              route_head->rte_num = sym_cnt;
              route_head->rte_name = xstrdup(pstr); /* use group-number for the moment */
              route_add_head(route_head);
              break;
            }
            break;
          case KEY_COL     :
            aktCol = atoi(pstr);
            break;
          case KEY_ZOOM    :
            break;
          case KEY_SIZE    :
            aktSize = atoi(pstr);
            break;
          case KEY_ART     :
            aktArt = atoi(pstr);
            break;
          case KEY_AREA     :
            aktArea = atoi(pstr);
            if (aktTyp==5 || aktTyp==5 || aktTyp==7) {
              isSection = SECTION_PUNKTE;  // Rechteck, Kreis, Dreieck
            }
            break;
          case KEY_PUNKTE  :
            isSection = SECTION_PUNKTE; // Linie, Fläche
            break;
#ifdef WITH_BITMAP
          case KEY_PATH     :
            aktPath = xstrdup(pstr);
            isSection = SECTION_PUNKTE; // Bitmap
            break;
          case KEY_TRANS     :
            aktTrans = atoi(pstr);
            break;
          case KEY_TRANSBYTE     :
            aktTransByte = atoi(pstr);
            break;
#endif
          case KEY_TEXT     :
            aktText = xstrdup(pstr);
            /* The last 'Text'-symbol wins as a information block for
               waypoint/route description.
               Infos from previous symbols get overwrited.
            */
            ovl_add_group(aktGroup,aktText);
            break;
          case KEY_WIDTH     :
            aktWidth = atoi(pstr);
            break;
          case KEY_HEIGHT     :
            aktHeight = atoi(pstr);
            break;
          case KEY_DIR     :
            aktDir = atoi(pstr);
            if (aktTyp==2) {
              isSection = SECTION_PUNKTE;  // Text
            }
            break;
          }
        }
      }
    } else if (isSection==SECTION_PUNKTE) {
      pstr = strtok(line,"=");
      if (strstr(pstr,"XKoord")!=NULL || strstr(pstr,"YKoord")!=NULL) {
        if ((pstr = strtok(NULL,"\n"))!=NULL) {
          rwert = atof(pstr);
          if (line[0]=='X') {
            aktX = rwert;
          } else if (line[0]=='Y') {
            aktY = rwert;
            switch (aktTyp) {
#ifdef WITH_BITMAP
            case 1: // Bitmap
              wpt = waypt_new();
              wpt->latitude = aktY;
              wpt->longitude = aktX;
              wpt->altitude = 0.0;
              wpt->shortname = strdup(aktPath);
              waypt_add(wpt);
              break;
#endif
            case 2: // Text
              isSection = SECTION_SYMBOL;
              break;
            case 3: // Linie
              wpt = waypt_new();
              wpt->latitude = aktY;
              wpt->longitude = aktX;
              wpt->altitude = 0.0;
              route_add_wpt(route_head, wpt);
              break;
            case 4: // Fläche
              break;
            case 5: // Rechteck
              break;
            case 6: // Kreis
              break;
            case 7: // Dreieck
              break;
            }
          }
        }
      }
    } else if (isSection==SECTION_OVERLAY) {
      isSection = SECTION_NONE;
    }
  }
  route_disp_all(route_add_name,NULL,NULL);
  if (aktText!=NULL) {
    xfree(aktText);
  }
  if (aktPath!=NULL) {
    xfree(aktPath);
  }
  for (i=0; i<groups_cnt; i++) {
    if (groups[i].name!=NULL) {
      xfree(groups[i].name);
    }
  }
  xfree(groups);
}

static void ovl_rd_deinit(void)
{
  gbfclose(fpin);
}

/*------------------------------------------*/
void ovl_read_parameter(const char* fname)
{
  gbfile*    fpin;
  arglist_t* p;
  char*      str;
  char*      pstr;

  fpin = gbfopen(fname, "r", MYNAME);
  if (fpin!=NULL) {
    while ((str = gbfgetstr(fpin))) {
      str = lrtrim(str); // trim
      if (str[0]!=';') {
        p = ovl_args;
        pstr = strtok(str,"=");
        if (pstr!=NULL) {
          while (p->argstring!=NULL) {
            if (strcmp(pstr,p->argstring)==0) {
              pstr = strtok(NULL,"\n");
              if (p->argtype==ARGTYPE_BOOL) {
                *(p->argval) = atoi(pstr) ? xstrdup(pstr) : NULL;
              } else {
                *(p->argval) = xstrdup(pstr);
              }
              break;
            }
            p++;
          }
        }
      }
    }
    gbfclose(fpin);
  }
}

static void ovl_wr_init(const char* fname)
{
  fpout = gbfopen(fname, "w", MYNAME);
  govl_sum_n = 0.0;
  govl_sum_e = 0.0;
  govl_sumcnt = 0.0;
  govl_symbol_cnt = 0;


  ovl_read_parameter(govl_file_s!=NULL ? govl_file_s : PARAMETER_FILE);

  if (govl_col_s!=NULL) {
    govl_col = atoi(govl_col_s);
  }
  if (govl_size_s!=NULL) {
    govl_size = atoi(govl_size_s);
  }
  if (govl_mapname==NULL) {
    govl_mapname = xstrdup(MAPNAME);
  }
  if (govl_zoomfc_s!=NULL) {
    govl_zoomfc = atoi(govl_zoomfc_s);
  }
  if (govl_dimmfc_s!=NULL) {
    govl_dimmfc = atoi(govl_dimmfc_s);
  }
  if (govl_txtcol_s!=NULL) {
    govl_txtcol = atoi(govl_txtcol_s);
  }
  if (govl_txtsize_s!=NULL) {
    govl_txtsize = atoi(govl_txtsize_s);
  }
  if (govl_font_s!=NULL) {
    govl_font = atoi(govl_font_s);
  }
  if (govl_txttrans_s!=NULL) {
    govl_txttrans = 1;
  }
}

static void ovl_wr_deinit(void)
{
  gbfprintf(fpout,"[Overlay]\n");
  gbfprintf(fpout,"Symbols=%d\n",govl_symbol_cnt);
  gbfprintf(fpout,"[MapLage]\n");
  gbfprintf(fpout,"MapName=%s\n",govl_mapname);
  gbfprintf(fpout,"DimmFc=%d\n",govl_dimmfc);
  gbfprintf(fpout,"ZoomFc=%d\n",govl_zoomfc);
  if (govl_symbol_cnt) {
    gbfprintf(fpout,"CenterLat=%.8lf\n",govl_sum_n/govl_sumcnt); // precision 8 = better than 1mm
    gbfprintf(fpout,"CenterLong=%.8lf\n",govl_sum_e/govl_sumcnt);
  } else {
    gbfprintf(fpout,"CenterLong=10.52374295\n"); // Braunschweiger Löwe, Mittelpunkt der Welt :-)
    gbfprintf(fpout,"CenterLat=52.26474445\n");
  }
  gbfprintf(fpout,"RefOn=0\n");

  gbfclose(fpout);
}

static void symbol_init(const route_head* hd)
{
  gbfprintf(fpout,"[Symbol %d]\n",govl_symbol_cnt+1);
  gbfprintf(fpout,"Typ=3\n");                            // Linie
  gbfprintf(fpout,"Group=%d\n"   ,govl_group_cnt+1+1);   // group==1 : not a group
  gbfprintf(fpout,"Col=%d\n"     ,govl_col);
  gbfprintf(fpout,"Zoom=2\n");
  gbfprintf(fpout,"Size=%d\n"    ,govl_size);
  gbfprintf(fpout,"Art=1\n");
  gbfprintf(fpout,"Punkte=%d\n"  ,hd->rte_waypt_ct);
  govl_cnt = 0;
  govl_symbol_cnt++;
  govl_group_cnt++;
}

static void symbol_text(double east,double north,char* text,int group)
{
  gbfprintf(fpout,"[Symbol %d]\n",govl_symbol_cnt+1);
  gbfprintf(fpout,"Typ=2\n");                           // Text
  gbfprintf(fpout,"Group=%d\n",group+1);  // group==1 : not a group
  gbfprintf(fpout,"Col=%d\n",govl_txtcol);
  gbfprintf(fpout,"Area=%d\n",govl_txttrans ? 1 : 2); // =2 opak =1 transparent
  gbfprintf(fpout,"Zoom=%d\n",2);
  gbfprintf(fpout,"Size=%d\n",govl_txtsize);
  gbfprintf(fpout,"Font=%d\n",govl_font);
  gbfprintf(fpout,"Dir=%d\n",100+((int) govl_dir));
  gbfprintf(fpout,"XKoord=%.8lf\n",east);  // precision 8 = better than 1mm
  gbfprintf(fpout,"YKoord=%.8lf\n",north);
  gbfprintf(fpout,"Text=%s\n",text);
  govl_symbol_cnt++;
}

static void symbol_point(const waypoint* wpt)
{
  double east,north;

  east  = wpt->longitude;
  north = wpt->latitude;
  gbfprintf(fpout,"XKoord%d=%.8lf\n",govl_cnt,east);    // precision 8 = better than 1mm
  gbfprintf(fpout,"YKoord%d=%.8lf\n",govl_cnt,north);
  govl_cnt++;
  govl_sum_e += east;
  govl_sum_n += north;
  govl_sumcnt += 1.0;
  /*
    govl_last_east  = east;
    govl_last_north = north;
  */
}


static void symbol_deinit(const route_head* hd)
{
  queue* elem, *tmp;
  waypoint* waypointp;
  int i;
  double lat1,lon1,lat2,lon2;
  double lats,lons,late,lone;
  double dist,d,dd;

  lat1 = lon1 = lat2 = lon2 = 0.0;
  lats = lons = late = lone = 0.0;
  dist = 0.0;
  i = 0;
  QUEUE_FOR_EACH(&(hd->waypoint_list), elem, tmp) {
    waypointp = (waypoint*) elem;
    lat2 = RAD(waypointp->latitude);
    lon2 = RAD(waypointp->longitude);
    if (i) {
      d   = gcdist(lat1, lon1, lat2, lon2);
      dist += d;
    } else {
      lats = lat2; // start point
      lons = lon2;
    }
    lat1 = lat2;
    lon1 = lon2;
    i++;
  }
  late = lat2;  // end point
  lone = lon2;
  dd = 0;
  i = 0;
  elem = QUEUE_FIRST(&(hd->waypoint_list));
  while (elem!=&(hd->waypoint_list) && dd<dist/2.0) {
    waypointp = (waypoint*) elem;
    lat2 = RAD(waypointp->latitude);
    lon2 = RAD(waypointp->longitude);
    if (i) {
      d   = gcdist(lat1, lon1, lat2, lon2);
      dd += d;
    }
    lat1 = lat2;
    lon1 = lon2;
    elem = QUEUE_NEXT(elem);
    i++;
  }

  d = gcdist(lats,lons,late,lone);
//  d = acos( sin(lats)*sin(late)+cos(lats)*cos(late)*cos(lone-lons) );
  dd = acos((sin(late) - sin(lats)*cos(d))/(cos(lats)*sin(d)));
  if (lone<lons) {
    dd = -dd;  // correction because the ambiguity of acos function
  }
  dd = DEG(dd);              // azimuth
  dd = 360.0 - (dd + 270.0); // make it anticlockwise and start counting on x-axis
  dd = dd <   0.0 ? dd + 360.0 : dd; // normalizing
  dd = dd > 360.0 ? dd - 360.0 : dd; // normalizing

  /* name of route */
  /* plot text at the last point of route */
  govl_dir = dd;  // approximated text rotation, correct value must be the azimuth in UTM
  symbol_text(DEG(lon1),DEG(lat1),hd->rte_name,govl_group_cnt);
  govl_dir = 0.0; // restore
}

static void overlay_waypt_pr(const waypoint* waypointp)
{
  const char* oname;
  char* odesc;

  /*
   * Desparation time, try very hard to get a good shortname
  */
  odesc = waypointp->notes;
  if (!odesc) {
    odesc = waypointp->description;
  }
  if (!odesc) {
    odesc = waypointp->shortname;
  }
  oname = global_opts.synthesize_shortnames ?
          mkshort(mkshort_handle, odesc) :
          waypointp->shortname;

  gbfprintf(fpout,"[Symbol %d]\n",govl_symbol_cnt+1);
  gbfprintf(fpout,"Typ=1\n");
  gbfprintf(fpout,"Group=1\n");
  gbfprintf(fpout,"Width=100\n");
  gbfprintf(fpout,"Height=100\n");
  gbfprintf(fpout,"Dir=%d\n",100+((int) govl_dir));
  gbfprintf(fpout,"Zoom=2\n");
  gbfprintf(fpout,"Trans=2\n");
  gbfprintf(fpout,"TransByte=5\n");
  gbfprintf(fpout,"Path=%s\n","waypoint.bmp");
  gbfprintf(fpout,"XKoord=%.8lf\n",waypointp->longitude);
  gbfprintf(fpout,"YKoord=%.8lf\n",waypointp->latitude);
  govl_symbol_cnt++;
  govl_sum_e += waypointp->longitude;
  govl_sum_n += waypointp->latitude;
  govl_sumcnt += 1.0;

}

static void ovl_write(void)
{
  waypt_disp_all(overlay_waypt_pr);
  track_disp_all(symbol_init, symbol_deinit, symbol_point);
  route_disp_all(symbol_init, symbol_deinit, symbol_point);
  /*
    switch(global_opts.objective)
    {
      case wptdata:
        break;
      case trkdata:
        break;
    }
  */
}


ff_vecs_t overlay_vecs = {
  ff_type_internal,
  FF_CAP_RW_ALL,
  ovl_rd_init,
  ovl_wr_init,
  ovl_rd_deinit,
  ovl_wr_deinit,
  ovl_read,
  ovl_write,
  NULL,
  ovl_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
