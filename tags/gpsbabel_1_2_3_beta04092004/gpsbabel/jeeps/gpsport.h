/*
 *  For portability any '32' type must be 32 bits
 *                  and '16' type must be 16 bits
 */
typedef unsigned char UC;
typedef short int16;
typedef unsigned short uint16;
typedef uint16 US;

#if defined(__alpha)
typedef int int32;
typedef unsigned int uint32;
#else
typedef long int32;
typedef unsigned long uint32;
#endif
