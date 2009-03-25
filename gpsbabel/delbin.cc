extern "C" {
  #include "defs.h"
};
extern void delbin_find_gps(void);
extern int delbin_os_put_packet(int, void *);
#include "delbin.h"
#include <assert.h>
/*
char * read_shortstring(void *src, int *len) {
  int sz;
  char *s = (char *) src;

  sz = *s++;
  if (len) *len = sz;

  return xstrdup(s);
}

char * read_longstring(void *src, int *len) {
  gbuint16 sz = le_read16(src);
  char *s = (char *) src + 2;

  if (len) *len = sz;

  return xstrdup(s);
}
*/

int delbin_pkt_size(int message_id) {
  switch (message_id) {
//    case IN_GET_WAYPOINTS_MESSAGE:
//      return 0;
    case IN_GET_WAYPOINTS_MESSAGE:
      return sizeof(delbin_request_waypoint);
      break;
    case IN_GET_NUM_WAYPTS_MESSAGE:
    case OUT_REQUEST_VERSION:
      return 0;
    default: abort(); 
  }
//  switch (message_id) {
//    case OUT_REQUEST_VERSION: assert(sizeof(delbin_version_response) == 
//  }
};


signed short get_cs(unsigned char *buf, short len) 
{
  signed short i, cs, tdat;

  cs = 0;
  for (i = 0; i < len; i += 2) {
    if ((len -i) > 1) {
      tdat = *(unsigned char*) (buf + i + 1);
      tdat = tdat << 8;
      tdat |= *(unsigned char *)(buf + i);
      cs += tdat;
    } else {
      tdat = *(unsigned char *)(buf + i);
      cs += tdat;
    }
  }
  cs = (signed short)(0x10000 - (long) cs);
  return cs;
}





// Needed:
// Xmit packet.  Await ack.  Retry if needed.
// Build packet.  Compute checksums, etc.
// Get size of packet - switch on type, add var length strings.
// Debugging in packet builders
//
// Initialize unit: request caps, versions.
// send wpts: loop for 0xb014's
// get wpts:  send 0xb012.   loop for 0xb013's

#if 0

int Xmain(void) {
#if 0
  return sizeof(radians);
  radians r;
  r.fromDegrees(123.45);
  double d = r.toDegrees();
#endif

  delbin_request_waypoint req;
  req.type = 1;
  req.lat.fromDegrees(0xabcd);
  delbin_packet pkt(IN_GET_NUM_WAYPTS_MESSAGE, &req);
  void *buf  = pkt.serialize();
  int s = pkt.serialized_size();
  unsigned char *b = (unsigned char *)buf;
  for (int i = 0; i < s; i++) {
    fprintf(stderr, "%02x ", b[i]);
  }

  fprintf(stderr, "\n");
  delbin_packet pkt2(IN_GET_WAYPOINTS_MESSAGE, &req);
  void *buf2  = pkt2.serialize();
  s = pkt2.serialized_size();
  b = (unsigned char *)buf2;
  for (int i = 0; i < s; i++) {
    fprintf(stderr, "%02x ", b[i]);
  }

  delbin_find_gps();

}
#endif

static void
delbin_init(const char *fname)
{
  delbin_find_gps();
  delbin_packet pkt(OUT_REQUEST_VERSION);
//  pkt.delbin_put_packet();
  void *buf= pkt.serialize();
  int sz = pkt.serialized_size();
  delbin_os_put_packet(sz, buf);
}

static void
delbin_deinit(void)
{
}

static void
delbin_read(void)
{
  delbin_blah();
}

static void
delbin_write(void)
{
}


ff_vecs_t delbin_vecs = {
	ff_type_serial,
//	FF_CAP_RW_ALL,
          { (ff_cap)(ff_cap_read | ff_cap_write), (ff_cap)(ff_cap_read | ff_cap_write), (ff_cap)(ff_cap_read | ff_cap_write) },

	delbin_init,
	delbin_init,
	delbin_deinit,
	delbin_deinit,
	delbin_read,
	delbin_write,
	NULL,
	NULL,
	CET_CHARSET_ASCII, 0,
};
