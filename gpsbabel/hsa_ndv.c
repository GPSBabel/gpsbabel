/*
    Copyright (C) 2004 HSA Systems, Sven Dowideit <sven@hsa.com.au>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
#include "defs.h"
#include "cet_util.h"
#if HAVE_LIBEXPAT
#include <expat.h>
static XML_Parser psr;
#endif

static char *cdatastr;
static int in_Route = 0;
static int in_ChartWork = 0;
static int in_Object = 0;

static waypoint *wpt_tmp;
static char *routeName = "ROUTENAME";

#define REPLACEMENT_SIRIUS_ATTR_SEPARATOR	';'
#define ATTR_USRMRK							"usrmrk"
#define ATTR_OBJECTNAME						"OBJNAM"
#define ATTR_SHIPNAME						"shpnam"

static void readVersion4(gbfile* pFile);
static void getAttr(const char *data, const char *attr, char **val, char seperator);

static gbfile *fd;
static gbfile *ofd;

static
arglist_t hsa_ndv_args[] = {
  ARG_TERMINATOR
};

#define MYNAME "HsaNdv"
#define MY_CBUF 4096


#if ! HAVE_LIBEXPAT
static void
hsa_ndv_rd_init(const char *fname)
{
  fatal(MYNAME ": This build excluded HSA Endeavour support because expat was not installed.\n");
}

static void
hsa_ndv_read(void)
{
}
#else

static void
hsa_ndv_start(void *data, const XML_Char *xml_el, const XML_Char **attr)
{
  const char *el = xml_convert_to_char_string(xml_el);

//	printf("<%s>\n", el);
  if (strcmp(el, "Export") == 0) {
    //should only be one
  } else if (strcmp(el, "Route") == 0) {
    in_Route++;
  } else if (strcmp(el, "Chartwork") == 0) {
    in_ChartWork++;
  } else if (strcmp(el, "Object") == 0) {
    wpt_tmp = waypt_new();
    wpt_tmp->altitude = unknown_alt;
    in_Object++;
  }
  //reset data :)
  memset(cdatastr,0, MY_CBUF);
  xml_free_converted_string(el);
}

static void
hsa_ndv_end(void *data, const XML_Char *xml_el)
{
  const char *el = xml_convert_to_char_string(xml_el);
  if (in_Route) {
    if (strcmp(el, "Version") == 0) {
      //don't really care
    } else if (strcmp(el, "Name") == 0) {
      routeName = xstrdup(cdatastr);
    } else if (strcmp(el, "LastModified") == 0) {
      //don't really care
    }
    if (in_Object) {
      if (strcmp(el, "ClassName") == 0) {
      } else if (strcmp(el, "Attr") == 0) {
        getAttr(cdatastr, ATTR_OBJECTNAME, &wpt_tmp->shortname, REPLACEMENT_SIRIUS_ATTR_SEPARATOR);
        getAttr(cdatastr, ATTR_USRMRK, &wpt_tmp->description, REPLACEMENT_SIRIUS_ATTR_SEPARATOR);
      } else if (strcmp(el, "LegAttr") == 0) {
      } else if (strcmp(el, "NumberOfVertexs") == 0) {
      } else if (strcmp(el, "Latitude") == 0) {
        wpt_tmp->latitude = atof(cdatastr);
      } else if (strcmp(el, "Longitude") == 0) {
        wpt_tmp->longitude = atof(cdatastr);
      }
    }
  }

  if (in_ChartWork) {
    if (strcmp(el, "Version") == 0) {
      //don't really care
    }
    if (in_Object) {
      if (strcmp(el, "ClassName") == 0) {
//				className = xstrdup(cdatastr);
      } else if (strcmp(el, "Attr") == 0) {
        //getAttr(cdatastr, ATTR_OBJECTNAME, &wpt_tmp->shortname, REPLACEMENT_SIRIUS_ATTR_SEPARATOR);
        //getAttr(cdatastr, ATTR_SHIPNAME, &wpt_tmp->description, REPLACEMENT_SIRIUS_ATTR_SEPARATOR);
      } else if (strcmp(el, "NumberOfVertexs") == 0) {
      } else if (strcmp(el, "Latitude") == 0) {
        wpt_tmp->latitude = atof(cdatastr);
      } else if (strcmp(el, "Longitude") == 0) {
        wpt_tmp->longitude = atof(cdatastr);
      } else if (strcmp(el, "Time") == 0) {
        wpt_tmp->creation_time = atoi(cdatastr);
      }
    }
  }

  //ignore everything else for now..
  memset(cdatastr,0, MY_CBUF);

  if (strcmp(el, "Object") == 0) {
    if (in_Route) {
      waypt_add(wpt_tmp);
    } else if (in_ChartWork) {
      //TODO: not sure how i want to handle this..
    }
    in_Object--;
  } else if (strcmp(el, "Route") == 0) {
    in_Route--;
  } else if (strcmp(el, "Chartwork") == 0) {
    in_ChartWork--;
  }
  xml_free_converted_string(el);
}

static void
hsa_ndv_cdata(void *dta, const XML_Char *s, int len)
{
  char *estr;
  estr = cdatastr + strlen(cdatastr);
  memcpy(estr, s, len);
}

static void
hsa_ndv_rd_init(const char *fname)
{
  fd = gbfopen(fname, "r", MYNAME);

  psr = XML_ParserCreate(NULL);
  if (!psr) {
    fatal(MYNAME ":Cannot create XML parser\n");
  }

  XML_SetUnknownEncodingHandler(psr, cet_lib_expat_UnknownEncodingHandler, NULL);
  XML_SetElementHandler(psr, hsa_ndv_start, hsa_ndv_end);
  cdatastr = xcalloc(MY_CBUF,1);
  XML_SetCharacterDataHandler(psr, hsa_ndv_cdata);
}

static void
hsa_ndv_read(void)
{
  int len;
  char buf[MY_CBUF + 1];

  while ((len = gbfread(buf, 1, sizeof(buf) - 1, fd))) {
    char *bad;

    buf[len] = '\0';
    if (NULL != strstr(buf, "nver=1")) {
      //its the older format, not xml
      gbfseek(fd, 0, SEEK_SET);
      readVersion4(fd);
      break;
    }
    //grumble - have to remove \x1f's from sirius attributes
    bad = buf;
    while (NULL != (bad = strchr(bad, '\x1f'))) {
      *bad = REPLACEMENT_SIRIUS_ATTR_SEPARATOR;
    }
    if (!XML_Parse(psr, buf, len, gbfeof(fd))) {
      fatal(MYNAME ":Parse error at %d: %s\n",
            (int) XML_GetCurrentLineNumber(psr),
            XML_ErrorString(XML_GetErrorCode(psr)));
    }
  }

  XML_ParserFree(psr);
}

#endif

static void getAttr(const char *data, const char *attr, char **val, char seperator)
{
  char *start;
  if ((start = strstr(data, attr)) != NULL) {
    char *end;
    int len;

    end = strchr(start, seperator);
    if (end == NULL) {
      end = start + strlen(start);//assume we are teh last attr
    }

    len = end-start - strlen(attr);

    *val = xcalloc(len+1, 1);
    memcpy(*val, start+strlen(attr), len);
    (*val)[len] = '\0';
  } else {
    *val = xcalloc(1, 1);
    (*val)[0] = '\0';
  }
}
static void
hsa_ndv_rd_deinit(void)
{
  if (cdatastr) {
    xfree(cdatastr);
  }
  gbfclose(fd);
}

static void
hsa_ndv_wr_init(const char *fname)
{
  ofd = gbfopen(fname, "w", MYNAME);
}

static void
hsa_ndv_wr_deinit(void)
{
  gbfclose(ofd);
}

static int legNum = 0;

static void
hsa_ndv_waypt_pr(const waypoint *waypointp)
{

  gbfprintf(ofd, "\t\t<Object>\n");

  gbfprintf(ofd, "\t\t\t<ClassName>waypnt</ClassName>\n");
//ignore these for now, they are s57 specific
//	fprintf(ofd, "\t\t\t<FeatureNameAgency>0</FeatureNameAgency>\n");
//	fprintf(ofd, "\t\t\t<FeatureNameSubDiv>1</FeatureNameSubDiv>\n");
//	fprintf(ofd, "\t\t\t<FeatureNameNumber>1089009023</FeatureNameNumber>\n");
  gbfprintf(ofd, "\t\t\t<Attr><![CDATA[attr=grpnam%s\x1ftrnrad50\x1fOBJNAM%s\x1flegnum%i\x1fusrmrk%s\x1fselect2]]></Attr>\n", routeName, waypointp->shortname, legNum, waypointp->description);
  gbfprintf(ofd, "\t\t\t<LegAttr><![CDATA[attr=grpnam%s\x1f]]></LegAttr>\n", routeName);
  gbfprintf(ofd, "\t\t\t<NumberOfVertexs>1</NumberOfVertexs>\n");
  gbfprintf(ofd, "\t\t\t<Latitude>%lf</Latitude>\n", waypointp->latitude);
  gbfprintf(ofd, "\t\t\t<Longitude>%lf</Longitude>\n", waypointp->longitude);

  gbfprintf(ofd, "\t\t</Object>\n");

  legNum++;
}

static void
hsa_ndv_write(void)
{
  gbfprintf(ofd, "<?xml version=\"1.0\"?>\n");
  gbfprintf(ofd, "<Export>\n");
  gbfprintf(ofd, "\t<Route>\n");
  gbfprintf(ofd, "\t\t<Version>1.0000000</Version>\n");
  gbfprintf(ofd, "\t\t<Name>ROUTENAME</Name>\n");			/*TODO: used filename? */
  gbfprintf(ofd, "\t\t<LastModified>0</LastModified>\n");
  waypt_disp_all(hsa_ndv_waypt_pr);
  gbfprintf(ofd, "\t</Route>\n");

//later we'll import past tracks and chart objects?
//	fprintf(ofd, "\t<Chartwork>\n");
//	fprintf(ofd, "\t\t<Version>1.0000000</Version>\n");
//	track_disp_all(hsa_ndv_track_pr);
//	fprintf(ofd, "\t</Chartwork>\n");


  gbfprintf(ofd, "</Export>\n");
}

ff_vecs_t HsaEndeavourNavigator_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  hsa_ndv_rd_init,
  hsa_ndv_wr_init,
  hsa_ndv_rd_deinit,
  hsa_ndv_wr_deinit,
  hsa_ndv_read,
  hsa_ndv_write,
  NULL,
  hsa_ndv_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};

//////////////////////////////////////////////////////////////////////////
// older style Endeavour route export file
//read DEC2000 NDV export files

#define EF_RECORD_DELIMTER 0
#define ED_REC_NAME_SIZE 5
#define EF_NVER_REC "nver="
#define EF_LAT_REC "lati="
#define EF_LONG_REC "long="
#define EF_TIME_REC "time="
#define EF_ATTR_REC "attr="
#define EF_CLNM_REC "clnm="
#define INVALID_TIME -1L
#define SOUNDARRAY_CHAR 'S'

static int readRecord(gbfile* pFile, const char* pRecName, char *recData);
static int readPositionRecord(gbfile* pFile, double* lat, double* lng, long* timeStamp);

static void readVersion4(gbfile* pFile)
{
  while (TRUE) {
    char recData[256] = {0};
    // get the position
    double  lat2, lng2 = 0;

    // set the pointer to the time stamp depending
    // on whether we have a sounding array or not
    long ts1, ts2;
    long* pts1 = 0;
    long* pts2 = 0;

    int soundArray = FALSE;
    int numberOfVerticies = 0;
    char className[256];
    char attr[1024];
    int Vertex;

    memset(attr, 0, sizeof(attr));

    wpt_tmp = waypt_new();

    // read the first record
    if (!readRecord(pFile, EF_NVER_REC, recData))
      // no first record then finished
    {
      break;
    }

    // get the type
    sscanf((const char*)recData, "%d", &numberOfVerticies);

    // do we have a sounding array
    if (*((const char *)recData + strlen(recData) - 1) == SOUNDARRAY_CHAR) {
      soundArray = TRUE;
    }

    if (soundArray) {
      pts1 = &ts1;
      pts2 = &ts2;
    }

    // go through the vertices
    for (Vertex = 0; Vertex < numberOfVerticies; Vertex++) {
      // read vertex position
      if (!readPositionRecord(pFile, &lat2, &lng2, pts2)) {
        xfree(wpt_tmp);
        return;
      }

      wpt_tmp->longitude = lng2;
      wpt_tmp->latitude = lat2;
      break;//TODO: ignore more points for now
    }


    // read the class name
    if (!readRecord(pFile, EF_CLNM_REC, className)) {
      xfree(wpt_tmp);
      return;
    }

    // read the attributes name
    if (!readRecord(pFile, EF_ATTR_REC, attr)) {
      xfree(wpt_tmp);
      return;
    }
    getAttr(attr, ATTR_OBJECTNAME, &wpt_tmp->shortname, '\x1f');
    getAttr(attr, ATTR_USRMRK, &wpt_tmp->description, '\x1f');

    {
      char *bad;
      //remove \n and \x1f from description data
      while (NULL != (bad = strchr(wpt_tmp->description, '\x1f'))) {
        *bad = REPLACEMENT_SIRIUS_ATTR_SEPARATOR;
      }
      while (NULL != (bad = strchr(wpt_tmp->description, '\n'))) {
        *bad = ' ';
      }
      while (NULL != (bad = strchr(wpt_tmp->description, '\r'))) {
        *bad = ' ';
      }
    }

    waypt_add(wpt_tmp);
  }

  gbfclose(pFile);
  return;
}

// read a record to a file
static int readRecord(gbfile* pFile, const char* pRecName, char *recData)
{
  // get the rec name
  int len;
  char recName[ED_REC_NAME_SIZE+1];
  char arrRecData[256];

  for (len = 0; len < ED_REC_NAME_SIZE; len++) {
    int c = gbfgetc(pFile);

    // if we hit EOF failed
    if (c == EOF) {
      return FALSE;
    }

    recName[len] = c;
  }

  // if the record name is not the reqiured type then error
  if (strncmp(recName, pRecName, ED_REC_NAME_SIZE) != 0) {
    return FALSE;
  }

  // get the rec data
  for (len = 0; TRUE; len++) {
    int c = gbfgetc(pFile);

    // if we hit EOF failed
    if (c == EOF) {
      return FALSE;
    }

    // hit end of line
    if (c == EF_RECORD_DELIMTER) {
      break;
    }

    arrRecData[len] = c;
  }

  // get the rec data to a string
  strncpy(recData, arrRecData, len);

  return TRUE;
}

// read position
static int readPositionRecord(gbfile* pFile, double* lat, double* lng,
                              long* timeStamp)
{
  // read the lat record
  char recData[256] = {0};

  if (!readRecord(pFile, EF_LAT_REC, recData))
    // no lat record then finished
  {
    return FALSE;
  }

  // read the latitude
  sscanf((const char*)recData, "%lf", lat);

  // read the lng record
  if (!readRecord(pFile, EF_LONG_REC, recData))
    // no first record then finished
  {
    return FALSE;
  }

  // read the latitude
  sscanf((const char*)recData, "%lf", lng);

  // if we are to read a time record
  if (timeStamp) {
    // read the lng record
    if (!readRecord(pFile, EF_TIME_REC, recData))
      // no first record then finished
    {
      return FALSE;
    }

    // read the latitude
    sscanf((const char*)recData, "%ld", timeStamp);
  }

  return TRUE;
}
