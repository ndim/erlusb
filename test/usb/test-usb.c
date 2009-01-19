#include <stdio.h>
#include <usb.h>


int main() /* int argc, char *argv[] */
{
  int bus_count = 0;
  int dev_count = 0;
  usb_init();

  if (1) {
    const int number_of_busses = usb_find_busses();
    const int number_of_devices = usb_find_devices();
    printf("USB buses: %d\n", number_of_busses);
    printf("USB devices: %d\n", number_of_devices);
  }

  if (1) {
    struct usb_bus *busses = usb_get_busses();
    struct usb_bus *bus;

    for (bus = busses; bus; bus = bus->next) {
      struct usb_device *dev;
      bus_count++;
      printf("BUS dirname: %s\n", bus->dirname);
      for (dev = bus->devices; dev; dev = dev->next) {
	unsigned int c;
	usb_dev_handle *dev_hdl = usb_open(dev);
	char buf[256];
	dev_count++;

	/* Check if this device is a printer */
	/* if (dev->descriptor.bDeviceClass == 7) {
	 * Open the device, claim the interface and do your processing *
	 * ...
	 * }
	 */
	/*
	printf("  %04x:%04x\n", 
	       dev->descriptor.idVendor, dev->descriptor.idProduct);
	*/
	printf("  DEVICE %04x:%04x\n", 
	       dev->descriptor.idVendor, dev->descriptor.idProduct);
	printf("    bLength=0x%02x\n", dev->descriptor.bLength);
	printf("    bDescriptorType=0x%02x\n", dev->descriptor.bDescriptorType);
	printf("    bcdUSB=0x%04x\n", dev->descriptor.bcdUSB);
	printf("    bDeviceClass=0x%02x\n", dev->descriptor.bDeviceClass);
	printf("    bDeviceSubClass=0x%02x\n", dev->descriptor.bDeviceSubClass);
	printf("    bDeviceProtocol=0x%02x\n", dev->descriptor.bDeviceProtocol);
	printf("    bMaxPacketSize0=0x%02x\n", dev->descriptor.bMaxPacketSize0);

	printf("    idVendor=0x%04x", dev->descriptor.idVendor);
	if (dev->descriptor.iManufacturer && 
	    (0 <= usb_get_string_simple(dev_hdl, dev->descriptor.iManufacturer, 
					buf, sizeof(buf)))) {
	  printf(" (%s)\n", buf);
	} else {
	  printf("\n");
	}

	printf("    idProduct=0x%04x", dev->descriptor.idProduct);
	if (dev->descriptor.iProduct && 
	    (0 <= usb_get_string_simple(dev_hdl, dev->descriptor.iProduct, 
					buf, sizeof(buf)))) {
	  printf(" (%s)\n", buf);
	} else {
	  printf("\n");
	}

	printf("    iSerialNumber=0x%02x", dev->descriptor.iSerialNumber);
	if (dev->descriptor.iSerialNumber && 
	    (0 <= usb_get_string_simple(dev_hdl, dev->descriptor.iSerialNumber, 
					buf, sizeof(buf)))) {
	  printf(" (%s)\n", buf);
	} else {
	  printf("\n");
	}

	printf("    bcdDevice=0x%04x\n", dev->descriptor.bcdDevice);
	printf("    iManufacturer=0x%02x\n", dev->descriptor.iManufacturer);
	printf("    iProduct=0x%02x\n", dev->descriptor.iProduct);

	/* Loop through all of the configurations */
	for (c = 0; c < dev->descriptor.bNumConfigurations; c++) {
	  int i;
	  /* Loop through all of the interfaces */
	  for (i = 0; i < dev->config[c].bNumInterfaces; i++) {
	    int a;
	    /* Loop through all of the alternate settings */
	    for (a = 0; a < dev->config[c].interface[i].num_altsetting; a++) {
	      /* Check if this interface is a printer */
	      if (dev->config[c].interface[i].altsetting[a].bInterfaceClass == 7) {
		/* Open the device, set the alternate setting, claim the interface and do your processing */
		/* do nothing */
	      }
	    } /* for a in altsettings */
	  } /* for i in interfaces */
	} /* for c in configurations */
	usb_close(dev_hdl);
      } /* for dev in */
    } /* for bus in */
  } /* if (1) */

  return(bus_count>0||dev_count>0)?0:1;
}
