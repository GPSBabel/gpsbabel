/********************************************************************
** @source JEEPS packet construction, sending and ack functions
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0 
** @modified Dec 28 1999 Alan Bleasby. First version
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
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
********************************************************************/
#include "gps.h"
#include "gpsserial.h"

#include <stdio.h>
#include <errno.h>
#include <ctype.h>


/* @func GPS_Make_Packet ***********************************************
**
** Forms a complete packet to send 
**
** @param [w] packet [GPS_PPacket *] packet string
** @param [r] type [UC] packet type
** @param [r] data [UC *] data string
** @param [r] n [int16] number of bytes in data string
**
** @return [void]
************************************************************************/

void GPS_Serial_Make_Packet(GPS_PPacket *packet, UC type, UC *data, int16 n)
{
    UC *p;
    UC *q;
    
    int32 i;
    UC  chk=0;

    
    p = data;
    q = (*packet)->data;

    (*packet)->dle   = DLE;
    (*packet)->edle  = DLE;
    (*packet)->etx   = ETX;
    (*packet)->n     = (UC) n;
    (*packet)->type  = type;
    (*packet)->bytes = 0;

    chk -= type;

    if(n == DLE)
    {
	++(*packet)->bytes;
	*q++ = DLE;
    }
    
    
    chk -= (UC) n;
    
    for(i=0;i<n;++i)
    {
	if(*p == DLE)
	{
	    ++(*packet)->bytes;
	    *q++ = DLE;
	}
	chk -= *p;
	*q++ = *p++;
	++(*packet)->bytes;
    }

    if(chk == DLE)
    {
	*q++ = DLE;
	++(*packet)->bytes;
    }
    
    (*packet)->chk = chk;
    
    return;
}


void
Diag(void *buf, size_t sz)
{
	unsigned char *cbuf = (unsigned char *) buf;
	while (sz--) {
		GPS_Diag("%02x ", *cbuf++);
	}
}

void 
DiagS(void *buf, size_t sz)
{
	unsigned char *cbuf = (unsigned char *) buf;

	while (sz--) {
		unsigned char c = *cbuf++;
		GPS_Diag("%c", isalnum(c) ? c : '.');
	}
}

/* @func GPS_Write_Packet ***********************************************
**
** Forms a complete packet to send 
**
** @param [w] fd [int32] file descriptor
** @param [r] packet [GPS_PPacket] packet
**
** @return [int32] number of bytes in the packet
************************************************************************/

int32 GPS_Serial_Write_Packet(gpsdevh *fd, GPS_PPacket packet)
{
    size_t ret;
    const char *m1, *m2;
    

    GPS_Diag("Tx Data:");
    Diag(&packet->dle, 3);    
    if((ret=GPS_Serial_Write(fd,(const void *) &packet->dle,(size_t)3)) == -1)
    {
	perror("write");
	GPS_Error("SEND: Write to GPS failed");
	return 0;
    }
    if(ret!=3)
    {
	GPS_Error("SEND: Incomplete write to GPS");
	return 0;
    }

    Diag(packet->data, packet->bytes);
    if((ret=GPS_Serial_Write(fd,(const void *)packet->data,(size_t)packet->bytes)) == -1)
    {
	perror("write");
	GPS_Error("SEND: Write to GPS failed");
	return 0;
    }
    if(ret!=packet->bytes)
    {
	GPS_Error("SEND: Incomplete write to GPS");
	return 0;
    }


    Diag(&packet->chk, 3);

    GPS_Diag(": ");
    DiagS(packet->data, packet->bytes);
    DiagS(&packet->chk, 3);
    m1 = Get_Pkt_Type(packet->type, packet->data[0], &m2);
    GPS_Diag("(%-8s%s)\n", m1, m2 ? m2 : "");

    if((ret=GPS_Serial_Write(fd,(const void *)&packet->chk,(size_t)3)) == -1)
    {
	perror("write");
	GPS_Error("SEND: Write to GPS failed");
	return 0;
    }
    if(ret!=3)
    {
	GPS_Error("SEND: Incomplete write to GPS");
	return 0;
    }


    return 1;
}


/* @func GPS_Send_Ack ***********************************************
**
** Send an acknowledge packet
**
** @param [w] fd [int32] file descriptor
** @param [r] tra [GPS_PPacket *] packet to transmit
** @param [r] rec [GPS_PPacket *] last packet received
**
** @return [int32] success
************************************************************************/

int32 GPS_Serial_Send_Ack(gpsdevh *fd, GPS_PPacket *tra, GPS_PPacket *rec)
{
    UC data[2];

    GPS_Util_Put_Short(data,(US)(*rec)->type);
    GPS_Make_Packet(tra,LINK_ID[0].Pid_Ack_Byte,data,2);
    if(!GPS_Write_Packet(fd,*tra))
    {
	GPS_Error("Error acknowledging packet");
	gps_errno = SERIAL_ERROR;
	return 0;
    }

    return 1;
}
