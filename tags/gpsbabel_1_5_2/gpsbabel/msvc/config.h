#if 0
//This file is no longer used, the defines are set in the project files

#define __va_copy(ap1, ap2) ((ap1) = (ap2))

// This controls the capabilities of our Character Encoding Transformations.
// Undefne for minimal.
// Define to zero for the common UTF-8, ASCII and related sets.
// Define to one for everything we know.

/* 0 for most-used, 1 for all character sets */
#define CET_WANTED 1

/* 1 to enable as many formats as possible */
#define MAXIMAL_ENABLED 1

/* 1 to enable the CSV formats support */
#define CSVFMTS_ENABLED 1

/* 1 to enable all the filters. */
#define FILTERS_ENABLED 1

/* 1 to enable Palm PDB support */
#define PDBFMTS_ENABLED 1

/* 1 to enable shapefile support */
#define SHAPELIB_ENABLED 1

/* 1 to inhibit our use of zlib. */
#undef ZLIB_INHIBITED
#endif
