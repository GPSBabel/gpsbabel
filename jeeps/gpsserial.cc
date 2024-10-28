/********************************************************************
** @source JEEPS serial port low level functions
**
** @author Copyright (C) 1999,2000 Alan Bleasby
** @version 1.0
** @modified December 28th 1999 Alan Bleasby. First version
** @modified June 29th 2000 Alan Bleasby. NMEA additions
** @modified Copyright (C) 2006 Robert Lipe
** @@
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
**
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
** Boston, MA  02110-1301, USA.
********************************************************************/
#include <cstdarg>              // for va_end, va_list, va_start
#include <cstdio>               // for fprintf, stderr

#include <QByteArray>           // for QByteArray
#include <QDebug>               // for QDebug, operator<<
#include <QIODeviceBase>        // for QIODeviceBase, QIODeviceBase::ReadWrite
#include <QSerialPort>          // for QSerialPort, QSerialPort::AllDirections
#include <QString>              // for QString
#include <QTextStreamFunction>  // for endl
#include <QThread>              // for QThread
#include <QtGlobal>             // for qPrintable, qint64

#include "jeeps/gps.h"
#include "jeeps/gpsserial.h"

int gps_baud_rate = DEFAULT_BAUD;

struct serial_data_handle {
  QSerialPort sp;
};

/*
 * Display an error from the serial subsystem.
 */
[[gnu::format(printf, 2, 3)]] static void GPS_Serial_Error(const serial_data_handle* h, const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  QString msg = QString::vasprintf(fmt, ap);
  msg.append(": ");
  msg.append(h->sp.errorString());

  GPS_Error("%s", qPrintable(msg));

  va_end(ap);
}

/* @func GPS_Serial_On *************************************************
**
** Open a serial port 8 data bits, 1 stop bit, 9600 baud
**
** @param [r] port [const char *] port e.g. COM4
** @param [w] dh [gpsdev** ] file descriptor
**
** @return [int32_t] false upon error
************************************************************************/
int32_t GPS_Serial_On(const char* port, gpsdevh** dh)
{
  auto* h = new serial_data_handle;
  GPS_Diag("Opening %s\n", port);
  h->sp.setPortName(port);
  bool ok = h->sp.open(QIODeviceBase::ReadWrite);
  if (!ok) {
    GPS_Serial_Error(h, "Cannot open serial port '%s'", port);
    gps_errno = SERIAL_ERROR;
    delete h;
    return 0;
  }
  ok = h->sp.setDataTerminalReady(true);
  if (!ok) {
    GPS_Serial_Error(h, "Couldn't set DTR");
    gps_errno = SERIAL_ERROR;
    delete h;
    return 0;
  }
  ok = h->sp.setRequestToSend(true);
  if (!ok) {
    GPS_Serial_Error(h, "Couldn't set RTS");
    gps_errno = SERIAL_ERROR;
    delete h;
    return 0;
  }
  if (global_opts.debug_level > 2) {
    QString debugstr;
    QDebug debug(&debugstr);
    debug << "  baudRate:" << h->sp.baudRate() << Qt::endl;
    debug << "  parity:" << h->sp.parity() << Qt::endl;
    debug << "  stopBits:" << h->sp.stopBits() << Qt::endl;
    debug << "  flow control:" << h->sp.flowControl() << Qt::endl;
    debug << "  break enabled:" << h->sp.isBreakEnabled() << Qt::endl;
    debug << "  pinout signals:" << h->sp.pinoutSignals() << Qt::endl;
    debug << "  read buffer size:" << h->sp.readBufferSize() << Qt::endl;
    debug << "  DTR:" << h->sp.isDataTerminalReady() << Qt::endl;
    debug << "  RTS:" << h->sp.isRequestToSend() << Qt::endl;
    debug << "  error status:" << h->sp.error() << Qt::endl;
    GPS_Diag("%s", qPrintable(debugstr));
  }

  *dh = reinterpret_cast<gpsdevh*>(h);
  return 1;
}

/* @func GPS_Serial_Off *************************************************
**
** Close serial port
**
** @param [r] dh [gpsdev* ] file descriptor
**
** @return [int32_t] false upon error
************************************************************************/
int32_t GPS_Serial_Off(gpsdevh* dh)
{
  auto* h = reinterpret_cast<serial_data_handle*>(dh);
  h->sp.close();
  delete h;
  return 1;
}

static int32_t GPS_Serial_Chars_Ready_After(gpsdevh* dh, int msec_timeout)
{
  auto* h = reinterpret_cast<serial_data_handle*>(dh);
  /* If no bytes are available call waitForRead()
   * in order to process IO in blocking mode.
   * This may result in bytes becoming available.
   * Don't process IO unless necessary as it negatively
   * impacts performance.
   */
  if (h->sp.bytesAvailable() <= 0) {
    bool ok = h->sp.waitForReadyRead(msec_timeout);
    if (!ok) {
      h->sp.clearError();
    }
  }
  return h->sp.bytesAvailable() > 0;
}

/* @func GPS_Serial_Chars_Ready *****************************************
**
** Query port to see if characters are waiting to be read
**
** @param [r] dh [gpsdev* ] file descriptor
**
** @return [int32_t] true if chars waiting
************************************************************************/
int32_t GPS_Serial_Chars_Ready(gpsdevh* dh)
{
  return GPS_Serial_Chars_Ready_After(dh, 1);
}

/* @func GPS_Serial_Wait ***********************************************
**
** Wait 80 milliseconds before testing for input. The GPS delay
** appears to be around 40-50 milliseconds. Doubling the value is to
** allow some leeway.
**
** @param [r] dh [gpsdev* ] file descriptor
**
** @return [int32_t] true if serial chars waiting
************************************************************************/
int32_t GPS_Serial_Wait(gpsdevh* dh)
{
  /* Wait a short time before testing if data is ready.
   * The GPS II, in particular, has a noticable time responding
   * with a response to the device inquiry and if we give up on this
   * too soon, we fail to read the response to the A001 packet and
   * blow our state machines when it starts streaming the capabiilties
   * response packet.
   */
  return GPS_Serial_Chars_Ready_After(dh, msecDELAY);
}

/* @func GPS_Serial_Flush ***********************************************
**
** Flush the serial lines
**
** @param [r] dh [gpsdev* ] file descriptor
**
** @return [int32_t] false upon error
************************************************************************/
int32_t GPS_Serial_Flush(gpsdevh* dh)
{
  auto* h = reinterpret_cast<serial_data_handle*>(dh);
  bool ok = h->sp.clear(QSerialPort::AllDirections);
  if (!ok) {
    GPS_Serial_Error(h, "SERIAL: flush error");
    gps_errno = SERIAL_ERROR;
  }

  return ok;
}

int32_t GPS_Serial_Write(gpsdevh* dh, const void* obuf, int size)
{
  auto* h = reinterpret_cast<serial_data_handle*>(dh);

  /*
   * Unbelievably, the Keyspan PDA serial driver 3.2, a "Windows
   * Certified driver", will crash the OS on a write of zero bytes.
   * We get such writes from upstream when there are zero payload
   * bytes.  SO we trap those here to stop Keyspan & Windows from
   * nuking the system.
   */
  if (size == 0) {
    return 0;
  }

  qint64 len = h->sp.write(static_cast<const char*>(obuf), size);

  if (len != size) {
    fatal("Write error.  Wrote %d of %d bytes.\n", static_cast<int>(len), size);
  }

  /* Call waitForBytesWritten to process IO in blocking mode.
   */
  bool ok = h->sp.waitForBytesWritten(10);
  if (!ok) {
    h->sp.clearError();
  }
  return len;
}

int32_t GPS_Serial_Read(gpsdevh* dh, void* ibuf, int size)
{
  auto* h = reinterpret_cast<serial_data_handle*>(dh);

  /* Process IO */
  (void) GPS_Serial_Chars_Ready(dh);

  qint64 cnt = h->sp.read(static_cast<char*>(ibuf), size);
  return cnt;
}

// Based on information by Kolesár András from
// http://www.manualslib.com/manual/413938/Garmin-Gps-18x.html?page=32
int32_t GPS_Serial_Set_Baud_Rate(gpsdevh* dh, int br)
{
  static UC data[4];
  GPS_Packet tra;
  GPS_Packet rec;
  auto* h = reinterpret_cast<serial_data_handle*>(dh);

  // Turn off all requests by transmitting packet
  GPS_Util_Put_Short(data, 0);
  GPS_Make_Packet(&tra, 0x1c, data, 2);
  if (!GPS_Write_Packet(dh,tra)) {
    return gps_errno;
  }
  if (!GPS_Get_Ack(dh, &tra, &rec)) {
    return gps_errno;
  }

  GPS_Util_Put_Int(data, br);
  GPS_Make_Packet(&tra, 0x30, data, 4);
  if (!GPS_Write_Packet(dh,tra)) {
    return gps_errno;
  }
  if (!GPS_Get_Ack(dh, &tra, &rec)) {
    return gps_errno;
  }

  // Receive IOP_BAUD_ACPT_DATA
  if (!GPS_Packet_Read(dh, &rec)) {
    return gps_errno;
  }

  // Acknowledge new speed
  if (!GPS_Send_Ack(dh, &tra, &rec)) {
    return gps_errno;
  }
  GPS_Device_Flush(dh);
  GPS_Device_Wait(dh);

  // Sleep for a small amount of time, about 100 milliseconds,
  // to make sure the packet was successfully transmitted to the GPS unit.
  QThread::usleep(100000);

  // Change port speed
  bool ok = h->sp.setBaudRate(br);
  if (!ok) {
    GPS_Serial_Error(h, "setBaudRate failed");
    h->sp.close();
    return 0;
  }

  GPS_Util_Put_Short(data, 0x3a);
  GPS_Make_Packet(&tra, 0x0a, data, 2);
  if (!GPS_Write_Packet(dh,tra)) {
    return gps_errno;
  }
  if (!GPS_Get_Ack(dh, &tra, &rec)) {
    return gps_errno;
  }

  GPS_Util_Put_Short(data, 0x3a);
  GPS_Make_Packet(&tra, 0x0a, data, 2);
  if (!GPS_Write_Packet(dh,tra)) {
    return gps_errno;
  }
  if (!GPS_Get_Ack(dh, &tra, &rec)) {
    return gps_errno;
  }

  if (global_opts.debug_level >= 1) {
    fprintf(stderr, "Serial port speed set to %d\n", br);
  }
  return 0;

}
