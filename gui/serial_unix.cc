// $Id: serial_unix.cpp,v 1.2 2010-02-13 23:25:23 robertl Exp $
//------------------------------------------------------------------------
//
//  Copyright (C) 2009  S. Khai Mong <khai@mangrai.com>.
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
#include "mainwindow.h"
#if !defined (Q_OS_MAC) // FIXME: find a better way to hide this on Mac.

#ifdef HAVE_UDEV
#include <libudev.h>
#include <QDebug>

static QStringList dynamicDevices()
{
    struct udev *udev = udev_new();
    if(!udev) {
        qDebug() << "Can't create udev";
        return QStringList();
    }

    QSet<QString> devices;

    struct udev_enumerate *enumerate = udev_enumerate_new(udev);
    udev_enumerate_add_match_subsystem(enumerate, "tty");
    udev_enumerate_scan_devices(enumerate);

    struct udev_list_entry *device;
    udev_list_entry_foreach(device, udev_enumerate_get_list_entry(enumerate)) {
        const char *path = udev_list_entry_get_name(device);
        struct udev_device *dev = udev_device_new_from_syspath(udev, path);

        bool okMaj, okMin;
        int major = QString(udev_device_get_property_value(dev, "MAJOR")).toInt(&okMaj);
        int minor = QString(udev_device_get_property_value(dev, "MINOR")).toInt(&okMin);
        if(!okMaj || !okMin) {
             major = -1;
             minor = -1;
        }

        // see Documentation/devices.txt in the linux tree
        if( !( (major == 4 || major == 5) && 0 <= minor && minor <= 63 ) ) {
            devices << QString::fromUtf8(udev_device_get_devnode(dev));
            /*
            udev_device_get_sysattr_list_entry(dev);
            udev_device_get_tags_list_entry(dev);
            struct udev_list_entry *prop;
            qDebug() << "Device Node Path:" << udev_device_get_devnode(dev) << path;
            udev_list_entry_foreach(prop, udev_device_get_properties_list_entry(dev)) {
                qDebug() << "  " << udev_list_entry_get_name(prop)
                         << "=>" << udev_list_entry_get_value(prop);
            }
            */
        }
        udev_device_unref(dev);
    }
    udev_enumerate_unref(enumerate);
    udev_unref(udev);

    QStringList list = devices.toList();
    qSort(list);
    return list;
}
#else
static QStringList dynamicDevices()
{
    return QStringList();
}
#endif


static const char *deviceNames[] = {
  "/dev/ttyS0",
  "/dev/ttyS1",
  "/dev/ttyS2",
  "/dev/ttyS3",
  "/dev/ttyUSB0",
  "/dev/rfcomm0",
  0
};

void MainWindow::osLoadDeviceNameCombos(QComboBox *box)
{
  const QStringList devices = dynamicDevices();
  box->addItems(devices);

  for (int i=0; deviceNames[i]; i++) {
    if(!devices.contains(deviceNames[i])) {
      box->addItem(deviceNames[i]);
    }
  }
}

#endif
