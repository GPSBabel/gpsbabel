/*

	Support for Microsoft AutoRoute 2002 ".axe" files,

	Copyright (C) 2005,2007,2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "jeeps/gpsmath.h"
#include <ctype.h>

#define MYNAME "msroute"

#undef OLE_DEBUG

static gbfile *fin;

static arglist_t msroute_args[] = {
  ARG_TERMINATOR
};

/* MS-AutoRoute structures */

typedef struct msroute_head_s {
  gbuint32 U1; 		/* 58/02/00/00 */
  char masm[4];		/* "MASM " */
  gbuint32 U2;
  gbuint32 U3;
  gbint32 waypts;
  gbuint32 U5;
  gbuint32 U6;
  gbuint32 U7;
  gbuint32 U8;
  gbuint32 U9;
  gbuint32 U10;
  gbuint32 U11;
  gbuint32 U12;
  gbuint32 U13;
  gbuint32 U14;
  gbuint32 U15;
  gbuint32 U16;
//	short U17;
//	char U18;
} msroute_head_t;

#define MSROUTE_OBJ_NAME 	"Journey"

/* simple ole file reader */

#define OLE_MAX_NAME_LENGTH 32
#define OLE_HEAD_FAT1_CT (512-0x4c)/4

#define BLOCKS(a, b) (((a) + (b) - 1) / (b))

static const unsigned char ole_magic[8] = {
  0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1
};

/*
	The ole implementation looks like a FAT filesystem.
	Thatswhy i use in code fat1 as item for the "big blocks" or bbd
	and fat2 for "small blocks" (sbd).

	Remarks:

	* in the moment ole_size1 and sector_sz represents the same value
	* in OLE_DEBUG mode: successfully tested with 64MB++ standard MS doc's (PowerPoint, Word)
*/

typedef struct ole_head_s {
  char magic[8];
  char clsid[16];
  gbuint16 rev;			/* offset 0x18 */
  gbuint16 ver;			/* offset 0x1a */
  gbint16 byte_order;		/* offset 0x1c */
  gbuint16 fat1_size_shift;	/* offset 0x1e */
  gbuint16 fat2_size_shift;	/* offset 0x20 */
  gbuint16 U7;			/* offset 0x22 */
  gbuint32 U8;			/* offset 0x24 */
  gbuint32 U9;			/* offset 0x28 */
  gbint32 fat1_blocks;		/* offset 0x2c */
  gbint32 prop_start;		/* offset 0x30 */
  gbuint32 U12;			/* offset 0x34 */
  gbuint32 fat1_min_size;		/* offset 0x38 */
  gbint32 fat2_start;		/* offset 0x3c */
  gbint32 fat2_blocks;		/* offset 0x40 */
  gbint32 fat1_extra_start;	/* offset 0x44 */
  gbint32 fat1_extra_ct;		/* offset 0x48 */
  gbint32 fat1[OLE_HEAD_FAT1_CT];	/* offset 0x4c */
} ole_head_t;

typedef struct ole_prop_s {
  gbuint16 name[32];
  gbuint16 name_sz;		/* offset 0x40 */
  char ole_typ;			/* offset 0x42 */
  char U1;			/* offset 0x43 */
  gbuint32 previous;		/* offset 0x44 */
  gbuint32 next;			/* offset 0x48 */
  gbuint32 dir;			/* offset 0x4c */
  gbuint32 U5;			/* offset 0x50 */
  gbuint32 U6;			/* offset 0x54 */
  gbuint32 U7;			/* offset 0x58 */
  gbuint32 U8;			/* offset 0x5c */
  gbuint32 U9;			/* offset 0x60 */
  gbuint32 U10;			/* offset 0x64 */
  gbuint32 U11;			/* offset 0x68 */
  gbuint32 U12;			/* offset 0x6c */
  gbuint32 U13;			/* offset 0x70 */
  gbint32 first_sector;		/* offset 0x74 */
  gbint32 data_sz;		/* offset 0x78 */
  gbuint32 U16;			/* offset 0x7c */
} ole_prop_t;

#define DIR_ITEM_SIZE sizeof(ole_prop_t)

static int sector_sz = 512;

#ifndef min
#define min(a,b) ((a) < (b)) ? (a) : (b)
#endif
#ifndef max
#define max(a,b) ((a) > (b)) ? (a) : (b)
#endif

static gbint32 *ole_fat1 = NULL;
static gbint32 *ole_fat2 = NULL;
static int ole_fat1_ct;
static int ole_fat2_ct;
static int ole_size1;
static int ole_size2;
static int ole_size1_min = 4096;
static ole_prop_t *ole_dir = NULL;
static int ole_dir_ct;
static ole_prop_t *ole_root = NULL;
static char **ole_root_sec = NULL;
static int ole_root_sec_ct;

/* local helpers */

static void
le_read32_buff(int *buff, const int count)
{
  int i;
  for (i = 0; i < count; i++) {
    buff[i] = le_read32(&buff[i]);
  }
}

/* simple OLE file reader */

static void
ole_read_sector(const int sector, void *target, const char full)
{
  int res;

  res = gbfseek(fin, (sector + 1) * sector_sz, SEEK_SET);
  is_fatal((res != 0), MYNAME ": Could not seek file to sector %d!", sector + 1);
  res = gbfread(target, 1, sector_sz, fin);
  is_fatal(
    ((res < 0) || (full && (res < sector_sz))),
    MYNAME ": Read error (%d, sector %d) on file \"%s\"!", res, sector, fin->name);
}

static ole_prop_t *
ole_find_property(const char *property)
{
  int i;

  for (i = 0; i < ole_dir_ct; i++) {
    int len, test;
    char *str;
    ole_prop_t *item;

    item = &ole_dir[i];
    if ((item->ole_typ != 1) && (item->ole_typ != 2) && (item->ole_typ != 5)) {
      continue;
    }
    if ((item->data_sz <= 0) || (item->name_sz <= 0)) {
      continue;
    }

    len = min(OLE_MAX_NAME_LENGTH, item->name_sz / 2);
    str = cet_str_uni_to_utf8((short *)&item->name, len);
    test = case_ignore_strcmp(str, property);
    xfree(str);

    if (test == 0) {
      return item;
    }
  }
  is_fatal((1), MYNAME ": \"%s\" not in property catalog!", property);
  return 0;
}

static char *
ole_read_stream(const ole_prop_t *property)
{
  const char *action = "ole_read_stream";
  int len, sector, big, blocksize, offs, left;
  int i;
  int *fat;
  char *buff;

  len = property->data_sz;

  if (len >= ole_size1_min) {
    big = 1;
    blocksize = ole_size1;
    fat = ole_fat1;
  } else {
    big = 0;
    blocksize = ole_size2;
    fat = ole_fat2;
  }

  offs = 0;
  left = len;
  sector = property->first_sector;

  i = ((len + blocksize - 1) / blocksize) * blocksize;
  buff = xmalloc(i);	/* blocksize aligned */

  if (big != 0) {
    while (left > 0) {
      int bytes = (left <= blocksize) ? left : blocksize;
      ole_read_sector(sector, buff + offs, (bytes >= sector_sz));
      left -= bytes;
      offs += bytes;
      if (left > 0) {
        sector = fat[sector];
        is_fatal((sector < 0), MYNAME ": Broken stream (%s)!", action);
      }
    }
  } else {
    int chain = sector;
    int blocks = (len + blocksize - 1) / blocksize;
    int blocks_per_sector = sector_sz / blocksize;

    offs = 0;

    while (blocks-- > 0) {
      char *temp;
      int block_offs;

      is_fatal((chain < 0), MYNAME ": Broken stream (%s)!", action);

      sector = chain / blocks_per_sector;
      is_fatal((sector >= ole_root_sec_ct), MYNAME ": Broken stream (%s)!", action);

      temp = ole_root_sec[sector];
      is_fatal((temp == NULL), MYNAME ": Broken stream (%s)!", action);

      block_offs = (chain % blocks_per_sector) * blocksize;

      memcpy(buff + offs, temp + block_offs, blocksize);

      offs += blocksize;
      chain = fat[chain];
    }
  }
  return buff;
}


static char *
ole_read_property_stream(const char *property_name, int *data_sz)
{
  ole_prop_t *property;
  char *result;

  if ((property = ole_find_property(property_name)) == NULL) {
    return NULL;
  }

  result = ole_read_stream(property);
  if ((result != NULL) && (data_sz != NULL)) {
    *data_sz = property->data_sz;
  }

  return result;
}

#ifdef OLE_DEBUG

static void
ole_test_properties()
{
  int i;

  for (i = 0; i < ole_dir_ct; i++) {
    char *temp;
    char name[OLE_MAX_NAME_LENGTH + 1];
    ole_prop_t *p = &ole_dir[i];

    if ((p->ole_typ != 1) && (p->ole_typ != 2) && (p->ole_typ != 5)) {
      continue;
    }
    if ((p->data_sz <= 0) || (p->name_sz <= 0)) {
      continue;
    }

    temp = cet_str_uni_to_utf8(&p->name, min(p->name_sz / 2, OLE_MAX_NAME_LENGTH));
    strncpy(name, temp, sizeof(name));
    xfree(temp);

    printf(MYNAME ": ole_test_properties for \"%s\" (%d bytes):", name, p->data_sz);

    if ((case_ignore_strcmp(name, "Root Entry") == 0) ||
        (p->data_sz < ole_size1_min)) {
      printf(" skipped...\n");
      continue;
    } else {
      int sector = p->first_sector;
      int data_sz = p->data_sz;
      int block_size = ole_size1; 	/* sector_sz */

      printf("\n");

      while ((data_sz > 0) && (sector >= 0)) {
        int bytes = (data_sz > block_size) ? block_size : data_sz;
        int prev = sector;

        data_sz -= bytes;
        sector = ole_fat1[sector];
        if (sector == -3) {
          printf(MYNAME ": special block at %d\n", prev);
          if ((prev + 2) < ole_fat1_ct) {
            sector = ole_fat1[prev + 1];
          }
          printf(MYNAME "-new sector: %d\n", sector);
        }
      }
      is_fatal((data_sz != 0), MYNAME ": Error in fat1 chain, sector = %d, %d bytes (=%d blocks) left!",
               sector, data_sz, BLOCKS(data_sz, block_size));
    }
  }
}
#endif

static void
ole_init(void)
{
  ole_head_t head;
  int i, i_offs, sector, count, left;
  int fat1_extra[128];

  ole_fat1 = NULL;
  ole_fat2 = NULL;

  sector_sz = 512;	/* fixed for the moment */

  is_fatal((sizeof(head) != sector_sz),
           MYNAME ": (!) internal error - invalid header size (%lu)!",
           (unsigned long) sizeof(head));

  memset(&head, 0, sizeof(head));
  gbfread(&head, sizeof(head), 1, fin);

  is_fatal((strncmp(head.magic, (char *) ole_magic, sizeof(ole_magic)) != 0), MYNAME ": No MS document.");

  head.rev = le_read16(&head.rev);
  head.ver = le_read16(&head.ver);
  head.byte_order = le_read16(&head.byte_order);
  head.fat1_size_shift = le_read16(&head.fat1_size_shift);
  head.fat2_size_shift = le_read16(&head.fat2_size_shift);
  head.fat1_blocks = le_read32(&head.fat1_blocks);
  head.prop_start = le_read32(&head.prop_start);
  head.fat1_min_size = le_read32(&head.fat1_min_size);
  head.fat2_start = le_read32(&head.fat2_start);
  head.fat2_blocks = le_read32(&head.fat2_blocks);
  head.fat1_extra_start = le_read32(&head.fat1_extra_start);
  head.fat1_extra_ct = le_read32(&head.fat1_extra_ct);
  le_read32_buff(&head.fat1[0], OLE_HEAD_FAT1_CT);

  ole_size1 = (1 << head.fat1_size_shift);
  ole_size2 = (1 << head.fat2_size_shift);
  ole_size1_min = head.fat1_min_size;

#ifdef OLE_DEBUG
  printf(MYNAME "-head: (version.revision) = %d.%d\n", head.ver, head.rev);
  printf(MYNAME "-head: byte-order = %d\n", head.byte_order);
  printf(MYNAME "-head: big fat start sector =  %d (0x%x)\n", head.fat1[0], (head.fat1[0] + 1) * 512);
  printf(MYNAME "-head: big fat blocks = %d\n", head.fat1_blocks);
  printf(MYNAME "-head: big fat block size = %d\n", (1 << head.fat1_size_shift));
  printf(MYNAME "-head: small fat start sector = %d\n", head.fat2_start);
  printf(MYNAME "-head: small fat blocks = %d\n", head.fat2_blocks);
  printf(MYNAME "-head: small fat block size = %d\n", (1 << head.fat2_size_shift));
  printf(MYNAME "-head: big fat minimum length = %d\n", head.fat1_min_size);
  printf(MYNAME "-head: property catalog start sector = %d\n", head.prop_start);
  printf(MYNAME "-head: additional big fat blocks = %d\n", head.fat1_extra_ct);
  printf(MYNAME "-head: additional big fat start sector = %d (0x%x)\n", head.fat1_extra_start, (head.fat1_extra_start + 1) * 512);
#endif

  is_fatal((head.byte_order != -2), MYNAME ": Unsupported byte-order %d", head.byte_order);
#if 0
  sector_sz = ole_size1;	/* i'll implement this, if i get an MS-doc (ole) 	*/
  /* with "sector_sz" other than 512			*/
#else
  is_fatal((ole_size1 != 512), MYNAME ": Unsupported sector size %d", ole_size1);
#endif
  ole_fat1 = xmalloc(head.fat1_blocks * sector_sz);
  ole_fat1_ct = (head.fat1_blocks * sector_sz) / sizeof(gbint32);

#ifdef OLE_DEBUG
  printf(MYNAME "-big fat: %d maximum sectors, size in memory %d, max. datasize %d bytes\n",
         ole_fat1_ct, head.fat1_blocks * sector_sz, head.fat1_blocks * sector_sz * sector_sz / sizeof(gbint32));
#endif

  i_offs = 0;				/* load "big fat" into memory */
  left = head.fat1_blocks;
  count = (left > OLE_HEAD_FAT1_CT) ? OLE_HEAD_FAT1_CT : left;

  for (i = 0; i < count; i++) {
    sector = head.fat1[i];
    ole_read_sector(sector, &ole_fat1[i_offs], 1);
    i_offs += ole_size1 / 4;
  }

  left -= count;

  if (left > 0) {
    sector = head.fat1_extra_start;

    while ((left > 0) && (sector >= 0)) {
      ole_read_sector(sector, &fat1_extra, 1);
      le_read32_buff(&fat1_extra[0], 128);

      count = (left < 127) ? left : 127;
      for (i = 0; i < count; i++) {
        ole_read_sector(fat1_extra[i], &ole_fat1[i_offs], 1);
        i_offs += ole_size1 / 4;
      }
      left -= count;
      if (left > 0) {
        sector = fat1_extra[127];
      }
    }
    is_fatal((left > 0), MYNAME ": Broken stream!");
  }
  if (ole_fat1_ct > 0) {
    le_read32_buff(&ole_fat1[0], ole_fat1_ct);
  }


  /* load fat2 "small fat" into memory */

  sector = head.fat2_start;
  if (sector >= 0) {
    count = 0;
    do {
      if (ole_fat2 == NULL) {
        ole_fat2 = (int *)xmalloc((count + 1) * sector_sz);
      } else {
        ole_fat2 = (int *)xrealloc(ole_fat2, (count + 1) * sector_sz);
      }

      ole_read_sector(sector, (char *)ole_fat2 + (count * sector_sz), 1);
      sector = ole_fat1[sector];

      count++;
    } while (sector >= 0);

    ole_fat2_ct = (count * sector_sz) / sizeof(gbint32);
    if (ole_fat2_ct > 0) {
      le_read32_buff(&ole_fat2[0], ole_fat2_ct);
    }
  }

  /* load directory (property catalog) */

  sector = head.prop_start;
  is_fatal((sector < 0), MYNAME ": Invalid file (no property catalog)!");

  count = 0;
  while (sector >= 0) {
    if (ole_dir == NULL) {
      ole_dir = (void *)xmalloc((count + 1) * sector_sz);
    } else {
      ole_dir = (void *)xrealloc(ole_dir, (count + 1) * sector_sz);
    }

    ole_read_sector(sector, (char *)ole_dir + (count * sector_sz), 1);
    sector = ole_fat1[sector];

    count++;
  }
  ole_dir_ct = (count * sector_sz) / sizeof(ole_prop_t);

  /* fix endianess of property catalog */

  for (i = 0; i < ole_dir_ct; i++) {
    ole_prop_t *item = &ole_dir[i];

    item->first_sector = le_read32(&item->first_sector);
    item->data_sz = le_read32(&item->data_sz);
  }

  ole_root = ole_find_property("Root Entry");

  /* read fat2 data sectors given by "Root Entry" */

  ole_root_sec_ct = (ole_root->data_sz + (sector_sz - 1)) / sector_sz;
  ole_root_sec = xcalloc(ole_root_sec_ct + 1, sizeof(char *));

  i = 0;
  sector = ole_root->first_sector;
  while (sector >= 0) {
    char *temp;

    temp = ole_root_sec[i++] = xmalloc(sector_sz);

    ole_read_sector(sector, temp, 1);
    sector = ole_fat1[sector];
  }
#ifdef OLE_DEBUG
  ole_test_properties();
#endif
}

static void
ole_deinit(void)
{
  if (ole_root_sec != NULL) {
    int i;
    for (i = 0; i < ole_root_sec_ct; i++) {
      char *c;
      if ((c = ole_root_sec[i])) {
        xfree(c);
      }
    }
    xfree(ole_root_sec);
  }
  if (ole_fat1 != NULL) {
    xfree(ole_fat1);
  }
  if (ole_fat2 != NULL) {
    xfree(ole_fat2);
  }
  if (ole_dir != NULL) {
    xfree(ole_dir);
  }
}

/* global MS AutoRoute functions */

static void
msroute_read_journey(void)
{
  int bufsz = 0;
  char *buff;

  buff = ole_read_property_stream(MSROUTE_OBJ_NAME, &bufsz);

  if ((buff != NULL) && (bufsz > 0)) {
    msroute_head_t *head = (msroute_head_t *)buff;
    unsigned char *cin, *cend;
    int count = 0;
    route_head *route;
    waypoint *wpt;
    char version;

    is_fatal((strncmp(head->masm, "MASM", 4) != 0), MYNAME ": Invalid or unknown data!");

    version = buff[0x14];
    is_fatal((version < 1) || (version > 7), MYNAME ": Unsupported version %d!", version);

    cin = (unsigned char *)buff + 71; // (at least?) sizeof(msroute_head_t);
    cend = (unsigned char *)buff + bufsz;

    route = route_head_alloc();
    route_add_head(route);

    head->waypts = le_read32(&head->waypts);

    while (count < head->waypts) {
      int len;

      /* after version 6 we've seen data with different header length */
      /* now we try to find the next pair of names in buff */

      while (1) {
        len = *cin;
        if ((cin + 120) > cend) {
          cin = NULL;
          break;
        }
        if ((cin + len < cend) && 	/* within buff ? */
            (cin[len + 3] == 0xff) &&	/* 0xff before next length byte ? */
            (cin[len + 4] == len) && 	/* wide string of same length ? */
            (le_read16(cin + len + 1) == 0xfeff)) {
          break;
        }
        cin++;
      }
      if (cin == NULL) {
        break;
      }

      wpt = waypt_new();

      len = *cin++;			/* length of shortname */
      cin += len;
      cin += 3;			/* 0xfffeff */

      len = *cin++;
      wpt->shortname = cet_str_uni_to_utf8((const short *)cin, len);
      cin += (len * 2);		/* seek over wide string */
      cin += (5 * sizeof(gbint32));	/* five unknown DWORDs */

      /* offs 12 !!!! Latitude int32 LE	*/
      /* offs 16 !!!! Longitude int32 LE 	*/
      wpt->latitude = GPS_Math_Semi_To_Deg(le_read32(cin+12));
      wpt->longitude = GPS_Math_Semi_To_Deg(le_read32(cin+16));

      cin += (23 * sizeof(gbint32));
      cin += 3;

#ifdef OLE_DEBUG
      waypt_add(waypt_dupe(wpt));	/* put to wpt-list to see results if no output is specified */
#endif
      route_add_wpt(route, wpt);
      count++;
    }
  }

  if (buff != NULL) {
    xfree(buff);
  }
}

/* registered callbacks */

static void msroute_rd_init(const char *fname)
{
  fin = gbfopen(fname, "rb", MYNAME);

  ole_init();
}

static void msroute_rd_deinit(void)
{
  ole_deinit();

  gbfclose(fin);
}

static void msroute_read(void)
{
  msroute_read_journey();
}

ff_vecs_t msroute_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_none, ff_cap_read },
  msroute_rd_init,
  NULL,
  msroute_rd_deinit,
  NULL,
  msroute_read,
  NULL,
  NULL,
  msroute_args,
  CET_CHARSET_UTF8, 1		/* CET-REVIEW */
};
