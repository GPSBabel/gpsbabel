static char csv[] = "
# gpsbabel XCSV style file
#
# Format: Delorme SA 9.0 CSV
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As defined in csv.c
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMASPACE
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	LAT_DECIMAL, \"\", \"%08.5f\"
IFIELD	LON_DECIMAL, \"\", \"%08.5f\"
IFIELD	DESCRIPTION, \"\", \"%s\"
";
static char custom[] = "
# gpsbabel XCSV style file
#
# Format: Custom \"Everything\" Style
# Author: Alex Mottram
#   Date: 11/24/2002
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMA
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

#
# HEADER STUFF:
#
PROLOGUE	Prologue Line 1
PROLOGUE	Prologue Line 2

#
# INDIVIDUAL DATA FIELDS:
#
IFIELD	CONSTANT, \"\", \"CONSTANT\"
IFIELD	INDEX, \"\", \"%d\"
IFIELD	LAT_DECIMAL, \"\", \"%f\"
IFIELD	LAT_DIR, \"\", \"%c\"
IFIELD	LON_DECIMAL, \"\", \"%f\"
IFIELD	LON_DIR, \"\", \"%c\"
IFIELD	ICON_DESCR, \"\", \"%s\"
IFIELD	SHORTNAME, \"\", \"%s\"
IFIELD	DESCRIPTION, \"\", \"%s\"
IFIELD	NOTES, \"\", \"%s\"
IFIELD	URL, \"\", \"%s\" 		
IFIELD	URL_LINK_TEXT, \"\", \"%s\"
IFIELD	ALT_METERS, \"\", \"%fM\"
IFIELD	ALT_FEET, \"\", \"%fF\"
IFIELD	LAT_DECIMALDIR, \"\", \"%f/%c\"
IFIELD	LON_DECIMALDIR, \"\", \"%f/%c\"
IFIELD	LAT_DIRDECIMAL, \"\", \"%c/%f\"
IFIELD	LON_DIRDECIMAL, \"\", \"%c/%f\"
IFIELD	LAT_INT32DEG, \"\", \"%ld\"
IFIELD	LON_INT32DEG, \"\", \"%ld\"
IFIELD	TIMET_TIME, \"\", \"%ld\"
IFIELD	EXCEL_TIME, \"\", \"%f\"

# EPILOGUE: 
EPILOGUE	Epilogue Line 1
EPILOGUE	Epilogue Line 2
";
static char dna[] = "
# gpsbabel XCSV style file
#
# Format: DNA Marker Format
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As defined in dna.c
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMA
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	INDEX, \"\", \"%d\"
IFIELD	LAT_DECIMAL, \"\", \"%08.5f\"
IFIELD	LON_DECIMAL, \"\", \"%08.5f\"
IFIELD	DESCRIPTION, \"\", \"%s\"

";
static char gpsdrive[] = "
# gpsbabel XCSV style file
#
# Format: GPSDrive
# Author: Alex Mottram
#   Date: 12/11/2002
#
# 
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		SPACE
RECORD_DELIMITER	NEWLINE
BADCHARS		,\"

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:

IFIELD	ANYNAME, \"\", \"%s\"
IFIELD	LAT_DECIMAL, \"\", \"%08.5f\"
IFIELD	LON_DECIMAL, \"\", \"%08.5f\"
";
static char gpsman[] = "
# gpsbabel XCSV style file
#
# Format: GPSMAN Format
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As defined in gpsman.c
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		TAB
RECORD_DELIMITER	NEWLINE
BADCHARS		TAB

PROLOGUE	!Format: DDD 1 WGS 84
PROLOGUE	!W:

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	SHORTNAME, \"\", \"%-8.8s\"
IFIELD	DESCRIPTION, \"\", \"%s\"
IFIELD	LAT_DIRDECIMAL, \"\", \"%c%f\"
IFIELD	LON_DIRDECIMAL, \"\", \"%c%f\"
IFIELD	IGNORE, \"\", \"%s\"

# gpsman.c likes mkshort len = 8, whitespace = 0.
";
static char mxf[] = "
# gpsbabel XCSV style file
#
# Format: Ozi Explorer
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As used in mxf.c
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMASPACE
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	LAT_DECIMAL, \"\", \"%08.5f\"
IFIELD	LON_DECIMAL, \"\", \"%08.5f\"
IFIELD	DESCRIPTION, \"\", \"%s\"
IFIELD	SHORTNAME, \"\", \"%s\"
IFIELD	IGNORE, \"\", \"%s\"
IFIELD	CONSTANT, \"ff0000\", \"%s\"	# COLOR
IFIELD	CONSTANT, \"47\", \"%s\"		# ICON

OFIELD	LAT_DECIMAL, \"\", \"%08.5f\"
OFIELD	LON_DECIMAL, \"\", \"%08.5f\"
OFIELD	DESCRIPTION, \"\", \"\"%s\"\"
OFIELD	SHORTNAME, \"\", \"%s\"
OFIELD	DESCRIPTION, \"\", \"%s\"
OFIELD	CONSTANT, \"ff0000\", \"%s\"	# COLOR
OFIELD	CONSTANT, \"47\", \"%s\"		# ICON
";
static char nima[] = "
# gpsbabel XCSV style file
#
# Format: NIMA/GNIS Geographic Names File
# Author: Alex Mottram
#   Date: 11/24/2002
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		TAB
RECORD_DELIMITER	NEWLINE
BADCHARS		TAB
PROLOGUE	RC	UFI	UNI	DD_LAT	DD_LONG	DMS_LAT	DMS_LONG	UTM	JOG	FC	DSG	PC	CC1	ADM1	ADM2	DIM	CC2	NT	LC	SHORT_FORM	GENERIC	SORT_NAME	FULL_NAME	FULL_NAME_ND	MODIFY_DATE

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	IGNORE, \"\", \"%s\"		# RC
IFIELD	IGNORE, \"\", \"%s\"		# UFI
IFIELD	IGNORE, \"\", \"%s\"		# UNI
IFIELD	LAT_DECIMAL, \"\", \"%f\"		# DD_LAT
IFIELD	LON_DECIMAL, \"\", \"%f\"		# DD_LON
IFIELD	IGNORE, \"\", \"%s\"		# DMS_LAT
IFIELD	IGNORE, \"\", \"%s\"		# DMS_LON
IFIELD	IGNORE, \"\", \"%s\"		# UTM
IFIELD	IGNORE, \"\", \"%s\"		# JOG
IFIELD	IGNORE, \"\", \"%s\"		# FC
IFIELD	IGNORE, \"\", \"%s\"		# DSG
IFIELD	IGNORE, \"\", \"%s\"		# PC
IFIELD	IGNORE, \"\", \"%s\"		# CC1
IFIELD	IGNORE, \"\", \"%s\"		# ADM1
IFIELD	IGNORE, \"\", \"%s\"		# ADM2
IFIELD	IGNORE, \"\", \"%s\"		# DIM
IFIELD	IGNORE, \"\", \"%s\"		# CC2
IFIELD	IGNORE, \"\", \"%s\"		# NT
IFIELD	IGNORE, \"\", \"%s\"		# LC
IFIELD	IGNORE, \"\", \"%s\"		# SHORT_FORM
IFIELD	IGNORE, \"\", \"%s\"		# GENERIC
IFIELD	SHORTNAME, \"\", \"%s\"		# SORT_NAME 
IFIELD	IGNORE, \"\", \"%s\"		# FULL_NAME (unicoded!)
IFIELD	DESCRIPTION, \"\", \"%s\"		# FULL_NAME_ND
IFIELD	IGNORE, \"\", \"%s\"		# MODIFY_DATE
";
static char ozi[] = "
# gpsbabel XCSV style file
#
# Format: Ozi Explorer
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As used in ozi.c
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMA
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

PROLOGUE	OziExplorer Waypoint File Version 1.1
PROLOGUE	WGS 84
PROLOGUE	Reserved 2
PROLOGUE	Reserved 3

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	INDEX, \"1\", \"%4d\"
IFIELD	SHORTNAME, \"\", \"%-14.14s\"
IFIELD	LAT_DECIMAL, \"\", \"%11.6f\"
IFIELD	LON_DECIMAL, \"\", \"%11.6f\"
IFIELD	EXCEL_TIME, \"\", \"%011.5f\"
IFIELD	CONSTANT, \"0\", \"%3s\"  		# icon 
IFIELD	CONSTANT, \"1\", \"%2s\"  		# 1 
IFIELD	CONSTANT, \"3\", \"%2s\"  		# display format opts 
IFIELD	CONSTANT, \"0\", \"%10s\"  		# foreground color 
IFIELD	CONSTANT, \"65535\", \"%10s\"  	# background color 
IFIELD	DESCRIPTION, \"\", \"%-40.40s\"
IFIELD	CONSTANT, \"0\", \"%2s\"  		# pointer direction 
IFIELD	CONSTANT, \"0\", \"%2s\"  		# garmin display flags 
IFIELD	CONSTANT, \"0\", \"%5s\"  		# proximity distance 
IFIELD	ALT_FEET, \"\", \"%7.0f\"
IFIELD	CONSTANT, \"6\", \"%2s\"  		# waypt name text size 
IFIELD	CONSTANT, \"0\", \"%2s\"  		# bold checkbox 
IFIELD	CONSTANT, \"17\", \"%2s\"  		# symbol size 
";
static char s_and_t[] = "
# gpsbabel XCSV style file
#
# Format: MS S&T 2002/2003
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As requested by Noel Shrum on the gpsbabel-code mailing list.
# Name,Latitude,Longitude,Name 2,URL,Type
# GCCBF,44.479133,-85.56515,High Rollaway by rjlint,http://www.geocaching.com/seek/cache_details.aspx?ID=3263,Traditional Cache
# GC110D,44.6522,-85.492483,Brown Bridge Pond Peek-a-Boo Cache by Big Bird,http://www.geocaching.com/seek/cache_details.aspx?ID=4365,Traditional Cache
# GC171C,44.70605,-85.62265,The Michigan Frog by RealDcoy & LRB,http://www.geocaching.com/seek/cache_details.aspx?ID=5916,Traditional Cache
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMA
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

PROLOGUE	Name,Latitude,Longitude,Name 2,URL,Type

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
# NOTE: MS S&T ONLY IMPORTS DATA, IT DOESN'T EXPORT THIS ANYWHERE SO WE CAN
#       HAVE OUR WAY WITH THE FORMATTING. 
#
IFIELD	SHORTNAME, \"\", \"%s\"		# Name
IFIELD	LAT_DECIMAL, \"\", \"%f\"		# Latitude
IFIELD	LON_DECIMAL, \"\", \"%f\"		# Longitude
IFIELD	DESCRIPTION, \"\", \"%s\"		# Name 2 (Big Description)
IFIELD	URL, \"\", \"%s\"			# URL
IFIELD	IGNORE, \"\", \"\"			# Holder for Geocache Type
";
static char tiger[] = "
# gpsbabel XCSV style file
#
# Format: Tiger Data Format
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As defined in tiger.c
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COLON
RECORD_DELIMITER	NEWLINE
BADCHARS		COLON

#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	LON_DECIMAL, \"\", \"%f\"
IFIELD	LAT_DECIMAL, \"\", \"%f\"
IFIELD	CONSTANT, \"redpin\", \"%s\"
IFIELD	DESCRIPTION, \"\", \"%s\"
";
static char xmap[] = "
# gpsbabel XCSV style file
#
# Format: Delorme Xmap Conduit
# Author: Alex Mottram
#   Date: 12/09/2002
#
# 
# As defined in csv.c/xmap
#
#
# FILE LAYOUT DEFINITIIONS:
#
FIELD_DELIMITER		COMMASPACE
RECORD_DELIMITER	NEWLINE
BADCHARS		COMMA

PROLOGUE	BEGIN SYMBOL
EPILOGUE	END
#
# INDIVIDUAL DATA FIELDS, IN ORDER OF APPEARANCE:
#
IFIELD	LAT_DECIMAL, \"\", \"%08.5f\"
IFIELD	LON_DECIMAL, \"\", \"%08.5f\"
IFIELD	DESCRIPTION, \"\", \"%s\"
";
#include "defs.h"
style_vecs_t style_list[] = {{"xmap", xmap} , {"tiger", tiger} , {"s_and_t", s_and_t} , {"ozi", ozi} , {"nima", nima} , {"mxf", mxf} , {"gpsman", gpsman} , {"gpsdrive", gpsdrive} , {"dna", dna} , {"custom", custom} , {"csv", csv} ,  {0,0}};
