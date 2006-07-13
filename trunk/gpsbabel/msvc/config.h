#define HAVE_LIBEXPAT 1
#define __va_copy(ap1, ap2) ((ap1) = (ap2))

// This controls the capabilities of our Character Encoding Transformations.
// Undefne for minimal.
// Define to zero for the common UTF-8, ASCII and related sets.
// Define to one for everything we know.

#undef CET_WANTED     

/* 1 to enable the CSV formats support */
#undef CSVFMTS_ENABLED 

/* 1 to enable all the filters. */
#undef FILTERS_ENABLED 

/* 1 to enable Palm PDB support */
#undef  PDBFMTS_ENABLED 

/* 1 to enable shapefile support */
#undef SHAPELIB_ENABLED

