%% External interface

-record(usb_device_descriptor,
	{bLength,
	 bDescriptorType,
	 bcdUSB,
	 bDeviceClass,
	 bDeviceSubClass,

	 bDeviceProtocol,
	 bMaxPacketSize0,
	 idVendor,
	 idProduct,
	 bcdDevice,

	 iManufacturer,
	 iProduct,
	 iSerialNumber,
	 bNumConfigurations
	}).
