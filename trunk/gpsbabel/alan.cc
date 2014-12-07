/*

    Read/write Alan Map500 Waypoints, Routes and Tracklogs.

    Provides "alanwpr" and "alantrl" formats for gpsbabel.
    Currently supports OS 2.xx only.

    Copyright (C) 2007  Gunar Megger, 0xff@quantentunnel.de
    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#define MYNAME "alan"

#define MAXWPT      1000		/* old 500 */

#define MAXRTE      50			/* old 20 */
#define MAXWPTINRTE 150			/* old 30 */

#define MAXTRK      8			/* old 5 */
#define MAXPTINTRK  2500

#define WPT_NAME_LEN    8
#define WPT_COMMENT_LEN 12

#define RTE_NAME_LEN    8
#define RTE_COMMENT_LEN 12

#define TRK_NAME_LEN    12
#define TRK_COMMENT_LEN 13

struct wpthdr {
  uint32_t id;
  int16_t num;
  int16_t next;
  int16_t idx[MAXWPT];
  uint8_t used[MAXWPT];
};

struct wpt {
  char name[WPT_NAME_LEN];
  char comment[WPT_COMMENT_LEN];
  struct {
    int32_t x;				/* degree * 36000 */
    int32_t y;				/* degree * 36000  */
  } pt;
  int32_t date;
  int32_t time;
  int16_t usecount;
  int8_t checked;
  int8_t reserved;
};

struct rtehdr {
  uint32_t id;
  int16_t num;
  int16_t next;
  int16_t idx[MAXRTE];
  uint8_t used[MAXRTE];
  int16_t rteno;
};

struct rte {
  char name[RTE_NAME_LEN];
  char comment[RTE_COMMENT_LEN];
  int16_t wptnum;
  int16_t wptidx[MAXWPTINRTE];
  int16_t reserved;
  int32_t date;
  int32_t time;
};

struct wprdata {
  struct wpthdr wpthdr;
  struct wpt    wpt[MAXWPT];
  struct rtehdr rtehdr;
  struct rte    rte[MAXRTE];
};

struct trkhdr {
  int16_t totalpt;
  int16_t next;
  char name[TRK_NAME_LEN];		/* 10, null terminated */
  char comment[TRK_COMMENT_LEN];	/* 12, null terminated */
  uint8_t reserved[3];
  uint32_t occupied;
  uint32_t show;
  uint32_t fill;
};

struct loghdr {
  uint32_t id;
  int16_t num;
  int16_t next;
  int32_t date;
  int32_t time;
  struct trkhdr trkhdr[MAXTRK];
};

struct trklog {
  struct {
    int32_t x;				/* degree * 36000 */
    int32_t y;				/* degree * 36000  */
  } pt[MAXPTINTRK];
  struct {
    int16_t speed;			/* km/h * 200 */
    int16_t height;			/* m * 5 */
  } sh[MAXPTINTRK];
};

struct trldata {
  struct loghdr loghdr;
  struct trklog trklog[MAXTRK];
};

#define WPT_HDR_ID 0x5C38A600
#define RTE_HDR_ID 0xD87F5900
#define TRL_HDR_ID 0x38CB1200

#define WPT_IDX_NONE -1			/* 0xffff */
#define WPT_USED      0xff
#define WPT_UNUSED    0
#define WPT_CHECKED   1
#define WPT_UNCHECKED 0

#define RTE_IDX_NONE   -1		/* 0xffff */
#define RTE_USED        0xff
#define RTE_UNUSED      0
#define RTE_RTENO_NONE -1

#define TRK_USED     1
#define TRK_UNUSED   0
#define TRK_SHOW     1
#define TRK_HIDE     0
#define TRK_FILL     1
#define TRK_WRAP     0

#define MAP500_PT_SCALE 36000.0
#define pt2deg(P) ((double)(P) / MAP500_PT_SCALE)
#define deg2pt(D) (int32_t)si_round((double)(D) * MAP500_PT_SCALE)

#define MAP500_ALTITUDE_SCALE   5.0
#define hgt2m(A) ((double)(A) / MAP500_ALTITUDE_SCALE)
#define m2hgt(A) (int16_t)si_round((double)(A) * MAP500_ALTITUDE_SCALE)

#define MAP500_SPEED_SCALE    720.0
#define sp2mps(S) ((double)(S) / MAP500_SPEED_SCALE)
#define mps2sp(S) (int16_t)si_round((double)(S) * MAP500_SPEED_SCALE)

#define  BYTEORDER_TEST  0x04030201	/* 32bit reference value */
enum {
  SWAP_NONE  = 0x1234,			/* map500 regular */
  SWAP_BYTES = 0x2143,			/* bytes swapped */
  SWAP_WORDS = 0x3412,			/* words swapped */
  SWAP_BOTH  = 0x4321			/* words + bytes swapped */
};

/**************************************************************************/

static gbfile* fin = NULL, *fout = NULL;
struct wprdata WPR;
struct trldata TRL;

static arglist_t wpr_args[] = {
  /*
  {"os3", &osversion, "Operating system version 3",
          NULL, ARGTYPE_BOOL, ARGNOMINMAX },
  */
  ARG_TERMINATOR
};
static arglist_t trl_args[] = {
  /*
  {"os3", &osversion, "Operating system version 3",
          NULL, ARGTYPE_BOOL, ARGNOMINMAX },
  */
  ARG_TERMINATOR
};

/**************************************************************************/
// FIXME: Why is this code doing its own byte order conversion?
static unsigned int byte_order(void)
{
  unsigned long test = BYTEORDER_TEST;
  unsigned char* ptr;
  unsigned int order;

  ptr = (unsigned char*)(&test);
  order = (ptr[0] << 12) | (ptr[1] << 8) | (ptr[2] << 4) | ptr[3];

  return order;
}

static void sw_bytes(void* word)
{
  uint8_t* p = (uint8_t*) word;
  uint16_t* r = (uint16_t*) word;

  *r = (uint16_t)(p[1] << 8 | p[0]);
}
static void sw_words(void* dword)
{
  uint16_t* p = (uint16_t*) dword;
  uint32_t* r = (uint32_t*) dword;

  *r = (uint32_t)(p[0] << 16 | p[1]);
}
static void rev_bytes(void* dword)
{
  uint8_t* p = (uint8_t*) dword;
  uint32_t* r = (uint32_t*) dword;

  *r = (uint32_t)(p[3] << 24 | p[2] << 16 | p[1] << 8 | p[0]);
}

static void swap_wpthdr(struct wpthdr* wpthdr,
                        void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  int i;

  if (swap32_func != NULL) {
    swap32_func(&wpthdr->id);
  }
  if (swap16_func != NULL) {
    swap16_func(&wpthdr->num);
    swap16_func(&wpthdr->next);
    for (i=0; i<MAXWPT; i++) {
      swap16_func(&wpthdr->idx[i]);
    }
  }
}

static void swap_wpt(struct wpt* wpt,
                     void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  if (swap16_func != NULL) {
    swap16_func(&wpt->usecount);
  }
  if (swap32_func != NULL) {
    swap32_func(&wpt->pt.x);
    swap32_func(&wpt->pt.y);
    swap32_func(&wpt->date);
    swap32_func(&wpt->time);
  }
}

static void swap_rtehdr(struct rtehdr* rtehdr,
                        void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  int i;

  if (swap16_func != NULL) {
    swap16_func(&rtehdr->num);
    swap16_func(&rtehdr->next);
    for (i=0; i<MAXRTE; i++) {
      swap16_func(&rtehdr->idx[i]);
    }
    swap16_func(&rtehdr->rteno);
  }
  if (swap32_func != NULL) {
    swap32_func(&rtehdr->id);
  }
}

static void swap_rte(struct rte* rte,
                     void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  int i;

  if (swap16_func != NULL) {
    swap16_func(&rte->wptnum);
    for (i=0; i<MAXWPTINRTE; i++) {
      swap16_func(&rte->wptidx[i]);
    }
    swap16_func(&rte->reserved);
  }
  if (swap32_func != NULL) {
    swap32_func(&rte->date);
    swap32_func(&rte->time);
  }
}

static void wpr_swap(struct wprdata* wprdata)
{
  void (*swap16_func)(void*);
  void (*swap32_func)(void*);
  int i;

  switch (byte_order()) {
  case SWAP_NONE:		   /* same byte oder, LITTLE_ENDIAN */
    return;
    break;
  case SWAP_BOTH:		   /* swap words and bytes, BIG_ENDIAN */
    swap16_func = sw_bytes;
    swap32_func = rev_bytes;
    break;
  case SWAP_WORDS:		   /* swap words, PDP_ENDIAN */
    swap16_func = NULL;
    swap32_func = sw_words;
    break;
  case SWAP_BYTES:		   /* swap bytes */
    swap16_func = sw_bytes;
    swap32_func = NULL;
    break;
  default:
    return;			   /* never reached */
  }

  swap_wpthdr(&(wprdata->wpthdr), swap16_func, swap32_func);
  for (i=0; i< MAXWPT; i++) {
    swap_wpt(&(wprdata->wpt[i]), swap16_func, swap32_func);
  }
  swap_rtehdr(&(wprdata->rtehdr), swap16_func, swap32_func);
  for (i=0; i<MAXRTE; i++) {
    swap_rte(&(wprdata->rte[i]), swap16_func, swap32_func);
  }
}

static void swap_trkhdr(struct trkhdr* trkhdr,
                        void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  if (swap16_func != NULL) {
    swap16_func(&(trkhdr->totalpt));
    swap16_func(&(trkhdr->next));
  }
  if (swap32_func != NULL) {
    swap32_func(&(trkhdr->occupied));
    swap32_func(&(trkhdr->show));
    swap32_func(&(trkhdr->fill));
  }
}

static void swap_loghdr(struct loghdr* loghdr,
                        void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  int i;

  if (swap16_func != NULL) {
    swap16_func(&(loghdr->num));
    swap16_func(&(loghdr->next));
  }
  if (swap32_func != NULL) {
    swap32_func(&(loghdr->id));
    swap32_func(&(loghdr->date));
    swap32_func(&(loghdr->time));
  }
  for (i=0; i<MAXTRK; i++) {
    swap_trkhdr(&(loghdr->trkhdr[i]), swap16_func, swap32_func);
  }
}

static void swap_trklog(struct trklog* trklog,
                        void (*swap16_func)(void*), void (*swap32_func)(void*))
{
  int i;

  if (swap16_func != NULL) {
    for (i=0; i<MAXPTINTRK; i++) {
      swap16_func(&(trklog->sh[i].speed));
      swap16_func(&(trklog->sh[i].height));
    }
  }
  if (swap32_func != NULL) {
    for (i=0; i<MAXPTINTRK; i++) {
      swap32_func(&(trklog->pt[i].x));
      swap32_func(&(trklog->pt[i].y));
    }
  }
}

static void trl_swap(struct trldata* trldata)
{
  void (*swap16_func)(void*);
  void (*swap32_func)(void*);
  int i;

  switch (byte_order()) {
  case SWAP_NONE:		   /* same byte oder, LITTLE_ENDIAN */
    return;
    break;
  case SWAP_BOTH:		   /* swap words and bytes, BIG_ENDIAN */
    swap16_func = sw_bytes;
    swap32_func = rev_bytes;
    break;
  case SWAP_WORDS:		   /* swap words, PDP_ENDIAN */
    swap16_func = NULL;
    swap32_func = sw_words;
    break;
  case SWAP_BYTES:		   /* swap bytes */
    swap16_func = sw_bytes;
    swap32_func = NULL;
    break;
  default:
    return;                        /* never reached */
  }

  swap_loghdr(&(trldata->loghdr), swap16_func, swap32_func);
  for (i=0; i<MAXTRK; i++) {
    swap_trklog(&(trldata->trklog[i]), swap16_func, swap32_func);
  }
}


/**************************************************************************/

static void str2lab(char* dest, const char* src, int len, const char* fmt,
                    int n)
{
  int i,j;

  j = 0;
  if (src != NULL) {
    for (i=0; i<len && src[i] != '\0'; i++) {
      if (isprint(src[i])) {
        dest[j++] = src[i];
      }
    }
  }
  if (j == 0 && fmt != NULL) {
    snprintf(dest, len, fmt, n);
    j = strlen(dest);
  }
  if (j < len) {
    memset(dest+j, ' ', len-j);
  }
}

static void str2lab(char* dest, const QString& src, int len, const char* fmt,
                    int n)
{
  str2lab(dest, CSTR(src), len, fmt, n);
}

static void pack_time(time_t t, int32_t* date, int32_t* time)
{
  struct tm* tm;

  tm = gmtime(&t);
  *date = tm->tm_mday | ((tm->tm_mon+1)<<8) | ((tm->tm_year+1900)<<16);
  *time = t % 86400;
}

static time_t unpack_time(int32_t date, int32_t time)
{
  time_t result;
  short year, month, day;
  static int m_to_d[12] =
  {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

  year  = (date >> 16) & 0xffff;
  month = (date >> 8) & 0xff;	/* 1-12 */
  day   = date & 0xff;          /* 1-31 */

  month -= 1;			/* fit struct tm */
  year += month / 12;

  if (month < 0) {
    year -= 1;
    month += 12;
  }
  result = (year - 1970) * 365 + m_to_d[month];
  if (month <= 1) {
    year -= 1;
  }
  result += (year - 1968) / 4;
  result -= (year - 1900) / 100;
  result += (year - 1600) / 400;
  result += day;
  result -= 1;
  result *= 86400;
  result += time;	     /* map500 time is inseconds of the day */

  return result;
}

/**************************************************************************/

static Waypoint* get_wpt(struct wprdata* wprdata, unsigned n)
{
  struct wpthdr* wpthdr;
  struct wpt* wpt;
  int j, idx;
  Waypoint* WP;

  wpthdr = &(wprdata->wpthdr);
  idx = wpthdr->idx[n];

  if (idx == WPT_IDX_NONE || wpthdr->used[idx] == WPT_UNUSED) {
    return NULL;
  }
  wpt = &(wprdata->wpt[idx]);

  WP = new Waypoint;
  WP->latitude  = -pt2deg(wpt->pt.y);
  WP->longitude =  pt2deg(wpt->pt.x);
  WP->SetCreationTime(unpack_time(wpt->date, wpt->time));
  for (j=WPT_NAME_LEN-1; j >= 0 && wpt->name[j] == ' '; j--) {};
  char *s = xstrndup(wpt->name,j+1);
  WP->shortname = s;
  xfree(s);
  for (j=WPT_COMMENT_LEN-1; j >= 0 && wpt->comment[j] == ' '; j--) {};
  if (j >= 0) {
    char *s = xstrndup(wpt->comment, j+1);
    WP->description = s;
    xfree(s);
  } else {
    WP->description = "";
  }
  WP->notes = "";

  return WP;
}

static void wpr_read(void)
{
  struct wprdata wprdata;
  struct rtehdr* rtehdr;
  struct rte* rte;
  int i, j, idx;
  Waypoint* WP;
  route_head* RT;

  if (gbfread(&wprdata, sizeof(struct wprdata), 1, fin) != 1) {
    fatal(MYNAME ": Read error on '%s'. Perhaps this isn't an alan file\n", fin->name);
  }
  wpr_swap(&wprdata);
  if (wprdata.wpthdr.id != WPT_HDR_ID ||
      wprdata.rtehdr.id != RTE_HDR_ID) {
    fatal(MYNAME ": %s is not in Alan .wpr format.\n", fin->name);
  }

  /* waypoints */
  for (i=0; i<MAXWPT; i++) {
    WP = get_wpt(&wprdata, i);
    if (WP != NULL) {
      waypt_add(WP);
    }
  }

  /* routes */
  rtehdr = &(wprdata.rtehdr);
  for (i=0; i<MAXRTE; i++) {
    idx = rtehdr->idx[i];
    if (idx == RTE_IDX_NONE || rtehdr->used[idx] == RTE_UNUSED) {
      continue;
    }
    rte = &(wprdata.rte[idx]);

    RT = route_head_alloc();
    RT->rte_num = i;
    for (j=RTE_NAME_LEN-1; j >= 0 && rte->name[j] == ' '; j--) {};
    char *s = xstrndup(rte->name,j+1);
    RT->rte_name = s;
    xfree(s);
    for (j=RTE_COMMENT_LEN-1; j >= 0 && rte->comment[j] == ' '; j--) {};
    if (j >= 0) {
      char *s = xstrndup(rte->comment,j+1);
      RT->rte_desc = s;
      xfree(s);
    } else {
      RT->rte_desc = "";
    }

    route_add_head(RT);

    /* route points */
    for (j=0; j<rte->wptnum; j++) {
      WP = get_wpt(&wprdata, rte->wptidx[j]);
      if (WP != NULL) {
        route_add_wpt(RT, WP);
      }
    }
  }
}

static void trl_read(void)
{
  struct trldata trldata;
  struct trkhdr* trkhdr;
  struct trklog* trklog;
  Waypoint* WP;
  route_head* TL;
  int i, j;

  for (i=0; i<MAXTRK; i+=2) {
    gbfseek(fin, 0x10000 * (i/2), SEEK_SET);
    if (gbfread(&(trldata.trklog[i]), sizeof(struct trklog), 2, fin) != 2) {
      fatal(MYNAME ": Read error on '%s'. Perhaps this isn't an alan file.\n", fin->name);
    }
  }
  gbfseek(fin, 0x10000 * MAXTRK/2, SEEK_SET);
  if (gbfread(&(trldata.loghdr), sizeof(struct loghdr), 1, fin) != 1) {
    fatal(MYNAME ": Read error on '%s'. Perhaps this isn't an alan file.\n", fin->name);
  }
  trl_swap(&trldata);
  if (trldata.loghdr.id != TRL_HDR_ID) {
    fatal(MYNAME ": %s is not in Alan .trl format.\n", fin->name);
  }

  for (i=0; i<MAXTRK; i++) {
    /* track header */
    trkhdr = &(trldata.loghdr.trkhdr[i]);
    if (trkhdr->occupied == TRK_UNUSED) {
      continue;
    }
    TL = route_head_alloc();
    for (j=TRK_NAME_LEN-1;
         j >= 0 && (trkhdr->name[j] == ' ' || trkhdr->name[j] == '\0');
         j--) {};
    char *s1 = xstrndup(trkhdr->name,j+1);
    TL->rte_name = s1;
    xfree(s1);
    /*  TL->rte_name[TRK_NAME_LEN+1] = 0; */	/* MAYBE BAD ADDRESS (Valgrind) */
    for (j=TRK_COMMENT_LEN-1;
         j >= 0 && (trkhdr->comment[j] == ' ' || trkhdr->comment[j] == '\0');
         j--) {};
    s1 = xstrndup(trkhdr->comment,j+1);
    TL->rte_desc = s1;
    xfree(s1);
    /*  TL->rte_desc[TRK_COMMENT_LEN+1] = 0; */	/* MAYBE BAD ADDRESS (Valgrind) */
    TL->rte_num = i;

    track_add_head(TL);

    /* track points */
    trklog = &(trldata.trklog[i]);
    for (j=0; j<trkhdr->totalpt; j++) {
      WP = new Waypoint;
      WP->latitude  = -pt2deg(trklog->pt[j].y);
      WP->longitude =  pt2deg(trklog->pt[j].x);
      WP->altitude  =  hgt2m(trklog->sh[j].height);
      if (trklog->sh[j].speed >= 0)
        WAYPT_SET(WP, speed, sp2mps(trklog->sh[j].speed))
        else {			/* bad speed < 0 - set to 0.0 */
          WAYPT_UNSET(WP, speed);
        }
      track_add_wpt(TL, WP);
    }
  }
}

/**************************************************************************/

static int find_wpt(struct wprdata* wprdata, const Waypoint* WP)
{
  struct wpt pattern, *wpt;
  int i, wpt_idx;

  str2lab(pattern.name, WP->shortname, WPT_NAME_LEN, NULL, 0);
  pattern.pt.x = deg2pt(WP->longitude);
  pattern.pt.y = deg2pt(-WP->latitude);

  wpt = wprdata->wpt;
  for (i=0; i<MAXWPT; i++) {
    wpt_idx = wprdata->wpthdr.idx[i];
    if (wpt_idx == WPT_IDX_NONE ||
        wprdata->wpthdr.used[wpt_idx] == WPT_UNUSED) {
      continue;
    }
    if (strncmp(wpt[wpt_idx].name, pattern.name, WPT_NAME_LEN) == 0 &&
        wpt[wpt_idx].pt.x == pattern.pt.x &&
        wpt[wpt_idx].pt.y == pattern.pt.y) {
      return i;
    }
  }

  return -1;
}

static int add_wpt(struct wprdata* wprdata, const Waypoint* WP,int isroute)
{
  struct wpthdr* wpthdr;
  int hdr_idx, wpt_idx;
  struct wpt* wpt;
  int i;

  wpthdr = &(wprdata->wpthdr);

  hdr_idx = find_wpt(wprdata, WP);
  if (hdr_idx >= 0) {
    /* duplicate waypoint */
    if (isroute) {
      wpt = &(wprdata->wpt[wpthdr->idx[hdr_idx]]);
      wpt->usecount ++;
    }
    return hdr_idx;
  }

  for (i=0; i<MAXWPT && wpthdr->idx[i] != WPT_IDX_NONE; i++) { }
  hdr_idx = i;
  for (i=0; i<MAXWPT && wpthdr->used[i] != WPT_UNUSED; i++) { }
  wpt_idx = i;
  if (wpthdr->num >= MAXWPT || hdr_idx >= MAXWPT || wpt_idx >= MAXWPT) {
    fatal(MYNAME ": Can't store more than %u waypoints\n", MAXWPT);
  }

  wpt = &(wprdata->wpt[wpt_idx]);
  str2lab(wpt->name, WP->shortname, WPT_NAME_LEN, "W%05d", wpt_idx);
  str2lab(wpt->comment, WP->description, WPT_COMMENT_LEN, NULL, 0);
  wpt->pt.x = deg2pt(WP->longitude);
  wpt->pt.y = deg2pt(-WP->latitude);
  wpt->usecount = isroute ? 1 : 0;
  wpt->checked  = isroute ? 0 : 1;
  wpt->reserved = 0;
  pack_time(WP->GetCreationTime().toTime_t(), &(wpt->date), &(wpt->time));

  wpthdr->idx[hdr_idx] = wpt_idx;
  wpthdr->used[wpt_idx] = WPT_USED;
  wpthdr->num++;
  wpthdr->next++;
  if (wpthdr->next >= MAXWPT) {	/* overrun */
    wpthdr->next = 0;
  }

  return hdr_idx;
}

static void wpr_waypoint(const Waypoint* WP)
{
  add_wpt(&WPR, WP, 0);
}

static void wpr_route_hdr(const route_head* RT)
{
  struct rtehdr* rtehdr;
  int hdr_idx, rte_idx;
  struct rte* rte;
  int i;

  rtehdr = &(WPR.rtehdr);
  for (i=0; i<MAXRTE && rtehdr->idx[i] != RTE_IDX_NONE; i++) { }
  hdr_idx = i;
  for (i=0; i<MAXRTE && rtehdr->used[i] != RTE_UNUSED; i++) { }
  rte_idx = i;
  if (rtehdr->num >= MAXRTE || hdr_idx >= MAXRTE || rte_idx >= MAXRTE) {
    fatal(MYNAME ": Can't store more than %u routes", MAXRTE);
  }

  rte = &(WPR.rte[rte_idx]);
  str2lab(rte->name, RT->rte_name, RTE_NAME_LEN, "R%03d", rte_idx);
  str2lab(rte->comment, RT->rte_desc, RTE_COMMENT_LEN, NULL, 0);
  pack_time(time(NULL), &(rte->date), &(rte->time));

  rtehdr->idx[hdr_idx] = rte_idx;
  rtehdr->used[rte_idx] = RTE_USED;
  rtehdr->num++;
  rtehdr->next++;
  if (rtehdr->next >= MAXRTE) {	/* overrun */
    rtehdr->next = 0;
  }

  /* if you want the new route to be active, uncomment the next line */
  /* rtehdr->rteno = rte_idx; */
}

static void wpr_route_wpt(const Waypoint* WP)
{
  struct rte* rte;
  int wpt_idx;

  rte = &(WPR.rte[WPR.rtehdr.num -1]);
  if (rte->wptnum >= MAXWPTINRTE) {
    fatal(MYNAME ": Can't store more than %u waypoints per route", MAXWPTINRTE);
  }

  wpt_idx = add_wpt(&WPR, WP, 1);

  rte->wptidx[rte->wptnum] = wpt_idx;
  rte->wptnum ++;
}

static void wpr_route_trl(const route_head* RT)
{
  /* should we do some final sanity checks? */
}

static void wpr_write(void)
{
  int i;

  WPR.wpthdr.id = WPT_HDR_ID;
  WPR.wpthdr.num = WPR.wpthdr.next = 0;
  for (i=0; i<MAXWPT; i++) {
    WPR.wpthdr.idx[i]  = WPT_IDX_NONE;
    WPR.wpthdr.used[i] = WPT_UNUSED;
  }
  memset(WPR.wpt, 0, MAXWPT * sizeof(struct wpt));
  WPR.rtehdr.id = RTE_HDR_ID;
  WPR.rtehdr.num = WPR.rtehdr.next = 0;
  for (i=0; i<MAXRTE; i++) {
    WPR.rtehdr.idx[i] = RTE_IDX_NONE;
    WPR.rtehdr.used[i] = RTE_UNUSED;
  }
  WPR.rtehdr.rteno = RTE_RTENO_NONE;
  memset(WPR.rte, 0, MAXRTE * sizeof(struct rte));

  waypt_disp_all(wpr_waypoint);
  route_disp_all(wpr_route_hdr, wpr_route_trl, wpr_route_wpt);

  wpr_swap(&WPR);
  if (gbfwrite(&WPR, sizeof(struct wprdata), 1, fout) != 1) {
    fatal(MYNAME ": Write error on %s\n", fout->name);
  }
}

/**************************************************************************/

static void trl_track_hdr(const route_head* TL)
{
  struct trkhdr* trkhdr;
  int idx, l;

  trkhdr = TRL.loghdr.trkhdr;

  for (idx=0; idx< MAXTRK && trkhdr[idx].occupied != TRK_UNUSED; idx++) {};
  if (idx >= MAXTRK) {
    fatal(MYNAME ": Can't store more than %u tracklogs", MAXTRK);
  }

  if (TL->rte_name != NULL) {
    strncpy(trkhdr[idx].name, CSTRc(TL->rte_name), TRK_NAME_LEN);
  }
  if (*(trkhdr[idx].name) == '\0') {
    sprintf(trkhdr[idx].name, "T%03d", idx);
  }
  trkhdr[idx].name[TRK_NAME_LEN-1] = '\0';

  if (TL->rte_desc != NULL) {
    strncpy(trkhdr[idx].comment, CSTRc(TL->rte_desc), TRK_COMMENT_LEN);
    l = strlen(CSTRc(TL->rte_desc));
    if (l < TRK_COMMENT_LEN-1) {
      memset(trkhdr[idx].comment + l, ' ', TRK_COMMENT_LEN - l);
    }
  }
  trkhdr[idx].comment[TRK_COMMENT_LEN-1] = '\0';

  trkhdr[idx].comment[TRK_COMMENT_LEN-1] = '\0';
  trkhdr[idx].occupied = TRK_USED;
  trkhdr[idx].totalpt = 0;
  trkhdr[idx].next = 0;

  TRL.loghdr.num = idx;
}

static void trl_track_wpt(const Waypoint* WP)
{
  struct trklog* trklog;
  struct trkhdr* trkhdr;
  int trk_idx, log_idx;

  trk_idx = TRL.loghdr.num;

  trkhdr = &(TRL.loghdr.trkhdr[trk_idx]);
  if (trkhdr->totalpt >= MAXPTINTRK) {
    fatal(MYNAME ": Can't store more than %u points per track", MAXPTINTRK);
  }
  log_idx = trkhdr->next;

  trklog = &(TRL.trklog[trk_idx]);
  trklog->pt[log_idx].x = deg2pt(WP->longitude);
  trklog->pt[log_idx].y = deg2pt(-WP->latitude);
  if WAYPT_HAS(WP, speed) {
    trklog->sh[log_idx].speed =  mps2sp(WP->speed);
  }
  if (WP->altitude != unknown_alt) {
    trklog->sh[log_idx].height = m2hgt(WP->altitude);
  }

  trkhdr->totalpt ++;
  trkhdr->next = trkhdr->totalpt;
}

static void trl_track_tlr(const route_head* TL)
{
  struct trkhdr* trkhdr;
  int trk_idx;

  trk_idx = TRL.loghdr.num;
  trkhdr = &(TRL.loghdr.trkhdr[trk_idx]);

  if (trkhdr->totalpt == 0) {
    trkhdr->occupied = TRK_UNUSED;
  }

  TRL.loghdr.num = -1;
}

static void trl_write(void)
{
  struct trkhdr* trkhdr;
  void* buf;
  int i;
  size_t fill;

  TRL.loghdr.id = TRL_HDR_ID;
  TRL.loghdr.num = TRL.loghdr.next = -1;
  TRL.loghdr.date = TRL.loghdr.time = 0;
  for (i=0; i<MAXTRK; i++) {
    trkhdr = &(TRL.loghdr.trkhdr[i]);
    trkhdr->totalpt = 0;
    trkhdr->next = 0;
    memset(trkhdr->name, 0, TRK_NAME_LEN);
    memset(trkhdr->comment, ' ', TRK_COMMENT_LEN);
    trkhdr->comment[TRK_COMMENT_LEN-1] = '\0';
    trkhdr->occupied = TRK_UNUSED;
    trkhdr->show = TRK_HIDE;
    trkhdr->fill = TRK_FILL;
  }
  memset(TRL.trklog, 0xff, sizeof(struct trklog) * MAXTRK);

  track_disp_all(trl_track_hdr, trl_track_tlr, trl_track_wpt);

  trl_swap(&TRL);

  fill =  0x10000 - 2 * sizeof(struct trklog);
  buf = xmalloc(fill);
  if (buf == NULL) {
    fatal(MYNAME ": Not enough memory\n");
  }
  memset(buf, 0xff, fill);

  for (i=0; i<MAXTRK; i+=2) {
    if (gbfwrite(&(TRL.trklog[i]), sizeof(struct trklog), 2, fout) != 2 ||
        gbfwrite(buf, fill, 1, fout) != 1) {
      fatal(MYNAME ": Write error on %s\n", fout->name);
    }
  }
  xfree(buf);

  fill = 0x1000 - sizeof(struct loghdr);
  buf = xmalloc(fill);
  if (buf == NULL) {
    fatal(MYNAME ": Not enough memory\n");
  }
  memset(buf, 0xff, fill);

  if (gbfwrite(&(TRL.loghdr), sizeof(struct loghdr), 1, fout) != 1 ||
      gbfwrite(buf, fill, 1, fout) != 1) {
    fatal(MYNAME ": Write error on %s\n", fout->name);
  }
  xfree(buf);
}

/**************************************************************************/

static void alan_rd_init(const char* fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
}

static void alan_rd_deinit(void)
{
  gbfclose(fin);
  fin = NULL;
}


static void alan_wr_init(const char* fname)
{
  fout = gbfopen(fname, "wb", MYNAME);
}

static void alan_wr_deinit(void)
{
  gbfclose(fout);
  fout = NULL;
}


static void alan_exit(void)
{
  return;
}

/**************************************************************************/

ff_vecs_t alanwpr_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* waypoints */,
    ff_cap_none              	/* tracks */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* routes */
  },
  alan_rd_init,
  alan_wr_init,
  alan_rd_deinit,
  alan_wr_deinit,
  wpr_read,
  wpr_write,
  alan_exit,
  wpr_args,
  CET_CHARSET_ASCII, 0 /* ascii is the expected character set */
  /* not fixed, can be changed through command
  line parameter */
};

ff_vecs_t alantrl_vecs = {
  ff_type_file,
  {
    ff_cap_none            	/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* tracks */,
    ff_cap_none           	/* routes */
  },
  alan_rd_init,
  alan_wr_init,
  alan_rd_deinit,
  alan_wr_deinit,
  trl_read,
  trl_write,
  alan_exit,
  trl_args,
  CET_CHARSET_ASCII, 0 /* ascii is the expected character set */
  /* not fixed, can be changed through command
  line parameter */
};
