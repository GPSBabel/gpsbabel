#include <stdio.h>
#include <usb.h>

extern "C" {
#include "defs.h"
}
#include "delbin.h"

struct usb_dev_handle *handle;
int ep_in, ep_out;  /* Endpoints */



static struct usb_device *
delbin_find_dev(void) {
  struct usb_bus *busses, *bus;

  usb_init();
  usb_find_busses();
  usb_find_devices();
  busses = usb_get_busses();

  for (bus = busses; bus; bus = bus->next) {
    struct usb_device *dev;

    for (dev = bus->devices; dev; dev = dev->next) {
      if (dev->descriptor.idVendor == delbin_vid) {
        return dev;
      }
    }
  }
  return NULL;
}


void delbin_find_gps(void) {
  struct usb_device *dev;

  dev = delbin_find_dev();
  if (dev == NULL) {
    fatal("No Delorme device found");
  }

  handle = usb_open(dev);
  if (handle == NULL) {
    fatal("Unable to open Delorme device");
  }

  if (usb_set_configuration(handle, 1) < 0) {
//    fatal("Unable to set configuration on  Delorme device");
  }
    usb_set_debug(99);
#if 0
  if (usb_claim_interface(handle, 0) < 0) {
    fatal("Unable to claim interface on  Delorme device");
  }
#endif

  for (int i = 0; i < dev->config->interface->altsetting->bNumEndpoints; i++) {
    struct usb_endpoint_descriptor * ep;
    ep = &dev->config->interface->altsetting->endpoint[i];

   if(ep->bEndpointAddress & USB_ENDPOINT_DIR_MASK) 
     ep_in = ep->bEndpointAddress & USB_ENDPOINT_ADDRESS_MASK;
   else
     ep_out = ep->bEndpointAddress & USB_ENDPOINT_ADDRESS_MASK;

    fprintf(stderr, "Mask: %x ", ep->bmAttributes);
    fprintf(stderr, "Dir: %x ", ep->bmAttributes);
    fprintf(stderr, "Addr: %x\n", ep->bEndpointAddress);

  }
    fprintf(stderr, "in %d out %d\n", ep_in, ep_out);
    usb_set_debug(99);
}

int
delbin_os_put_packet(int size, void *buf) {
  unsigned char* b = (unsigned char *) buf;
  fprintf(stderr, "Sending (%d): ", size);
  for (int i = 0; i < size; i++) {
    fprintf(stderr, "%02x ", *b++);
  }
  fprintf(stderr, "\n");

  int result = usb_interrupt_write(handle, ep_out, (char *) buf, size, 5000);
  fprintf(stderr, "Handle %p Result: %d\n", handle, result);



  return 0;
}
