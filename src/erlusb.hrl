% erlusb.hrl - Erlang interface to libusb
% Copyright (C) 2006 Hans Ulrich Niedermann <hun@n-dimensional.de>
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

-record(usb_endpoint_descriptor,
	{
	  bLength,
	  bDescriptorType,
	  bEndpointAddress,
	  bmAttributes,
	  wMaxPacketSize,
	  bInterval,
	  bRefresh,
	  bSynchAddress
	 }).

-record(usb_device_descriptor, {
	  bLength,
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
