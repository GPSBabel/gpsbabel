/*
 *  For portability any '32' type must be 32 bits
 *                  and '16' type must be 16 bits
 */

/* Since GPSBabel already has an integer size abstraction layer and
 * defs.h includes gbtypes.h before this file, just use that.
 */

#ifndef JEEPS_GPSPORT_H_INCLUDED_
#define JEEPS_GPSPORT_H_INCLUDED_

#include <cstdint>


typedef unsigned char UC;
typedef uint16_t      US;
typedef uint16_t      uint16;
typedef int16_t       int16;
typedef uint32_t      uint32;
typedef int32_t       int32;

#endif // JEEPS_GPSPORT_H_INCLUDED_
