#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsnmeafmt_h
#define gpsnmeafmt_h


#include "gps.h"

int32 GPS_NMEA_Apb_Scan(const char *s, GPS_PApb *thys);
int32 GPS_NMEA_Bod_Scan(const char *s, GPS_PBod *thys);
int32 GPS_NMEA_Bwc_Scan(const char *s, GPS_PBwc *thys);
int32 GPS_NMEA_Bwr_Scan(const char *s, GPS_PBwr *thys);
int32 GPS_NMEA_Dbt_Scan(const char *s, GPS_PDbt *thys);
int32 GPS_NMEA_Gga_Scan(const char *s, GPS_PGga *thys);
int32 GPS_NMEA_Gll_Scan(const char *s, GPS_PGll *thys);
int32 GPS_NMEA_Gsa_Scan(const char *s, GPS_PGsa *thys);
int32 GPS_NMEA_Gsv_Scan(const char *s, GPS_PGsv *thys);
int32 GPS_NMEA_Hdm_Scan(const char *s, GPS_PHdm *thys);
int32 GPS_NMEA_Hsc_Scan(const char *s, GPS_PHsc *thys);
int32 GPS_NMEA_Mtw_Scan(const char *s, GPS_PMtw *thys);
int32 GPS_NMEA_R00_Scan(const char *s, GPS_PR00 *thys);
int32 GPS_NMEA_Rmb_Scan(const char *s, GPS_PRmb *thys);
int32 GPS_NMEA_Rmc_Scan(const char *s, GPS_PRmc *thys);
int32 GPS_NMEA_Rte_Scan(const char *s, GPS_PRte *thys);
int32 GPS_NMEA_Vhw_Scan(const char *s, GPS_PVhw *thys);
int32 GPS_NMEA_Vwr_Scan(const char *s, GPS_PVwr *thys);
int32 GPS_NMEA_Vtg_Scan(const char *s, GPS_PVtg *thys);
int32 GPS_NMEA_Wpl_Scan(const char *s, GPS_PWpl *thys);
int32 GPS_NMEA_Xte_Scan(const char *s, GPS_PXte *thys);
int32 GPS_NMEA_Xtr_Scan(const char *s, GPS_PXtr *thys);
int32 GPS_NMEA_Rme_Scan(const char *s, GPS_PRme *thys);
int32 GPS_NMEA_Rmz_Scan(const char *s, GPS_PRmz *thys);
int32 GPS_NMEA_Rmm_Scan(const char *s, GPS_PRmm *thys);
int32 GPS_NMEA_Lib_Scan(const char *s, GPS_PLib *thys);


#endif

#ifdef __cplusplus
}
#endif
