extern "C" {
#include "defs.h"
#include "gbser.h"
}

#include "delbin.h"

static void *serial_handle;
#define MYNAME "delbin"
#define fname "com20:"

void
delbin_find_gps(void)
{
  if (serial_handle = gbser_init(fname), NULL == serial_handle) {
    fatal(MYNAME ": Can't open port '%s'\n", fname);
  }
  if (gbser_set_speed(serial_handle, 115200) != gbser_OK) {
    fatal(MYNAME ": Can't configure port '%s'\n", fname);
  }
}

int
delbin_os_put_packet(int, void *) {

}

// FIXME: This should go to common protocol code.
void 
delbin_get_pkt(unsigned char *buf, unsigned int sz) {
  if (sz < 12) {
    fatal(MYNAME ": runt packet of %d bytes.\n", sz);
  } 
  gbuint16 prolog, msg_id, data_sz, header_cksum, data_cksum, epilog;
  gbuint16 computed_header_cksum, computed_data_cksum;
  prolog = le_read16(&buf[0]);
  msg_id = le_read16(&buf[2]);
  data_sz = le_read16(&buf[4]);
  header_cksum = le_read16(&buf[6]);
  computed_header_cksum = get_cs(buf, 6);

  if (prolog != 0xfedb) {
    fatal(MYNAME ": packet had corrupt SOF of %04x.\n", prolog);
  }
  if (data_sz > sz - 12) {
    fatal(MYNAME ": packet had invalid size of %04x.\n", data_sz);
  }
  if (header_cksum != computed_header_cksum) {
    fatal(MYNAME ": packet had invalid header checksum.\n");
  }

  fprintf(stderr, "Header got: %x computed: %x\n", header_cksum, computed_header_cksum);

  // The Delorme specs is fuzzy on this.  'data size' in the header seems
  // to include not only the data, but the trailing four bytes.  Weirdly,
  // we're still off by two bytes that I don't see.
  data_cksum = le_read16(&buf[data_sz + 4]);

  // Weirdly, the header cs includes the epilogue, bu the data cs doesn't 
  // include the epilogue.
  computed_data_cksum = get_cs(buf + 8, data_sz -4);
  epilog = le_read16(&buf[data_sz + 6]);

  if (data_cksum != computed_data_cksum) {
    fatal(MYNAME ": packet had invalid data checksum.\n");
  }
  fprintf(stderr, "Data got: %x computed: %x\n", data_cksum, computed_data_cksum);

  if (epilog != 0xbcad) {
    fatal(MYNAME ": packet had corrupt EOF of %04x.\n", epilog);
  }

fprintf(stderr, "pro: %x %x %x \n", prolog, msg_id, data_sz);
fprintf(stderr, "epi: %x %x \n", data_cksum, epilog);

}



int
delbin_blah(void) {
  unsigned char buf[1000];

  int r = gbser_read_wait(serial_handle, &buf, sizeof(buf), 1000);
  fprintf(stderr, "Reading %p\n", serial_handle);
  delbin_get_pkt(buf, r);
  for (int i = 0; i < r; i++) {
if ((i & 15) == 0) fprintf(stderr, "\n%04x: ", i);
    fprintf(stderr, "%02x ", buf[i]);
  }


// This write doesn't seem to "stick" - we get no device response, 
// but I'm tired for the evening....
// 
// Maybe some clever way to share packet validation between reader and writer?
  unsigned char zark[] = {0xdb, 0xfe, 0x01, 0xb0, 0x04, 0x00, 0x20, 0x51, 0x00, 0x00, 0xad, 0xbc};
  int v = gbser_write(serial_handle, &zark, sizeof(zark));
fprintf(stderr, "\nWrite: %s for 0x%x bytes\n", v == gbser_OK ? "passed" : "failed", sizeof(zark));


#if 1
  r = gbser_read_wait(serial_handle, &buf, sizeof(buf), 1000);
  fprintf(stderr, "Response %p\n", serial_handle);
  delbin_get_pkt(buf, r);
  for (int i = 0; i < r; i++) {
if ((i & 15) == 0) fprintf(stderr, "\n%04x: ", i);
    fprintf(stderr, "%02x ", buf[i]);
  }

#endif

  return r;
}


