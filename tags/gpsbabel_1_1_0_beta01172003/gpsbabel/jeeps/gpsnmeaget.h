#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsnmearead_h
#define gpsnmearead_h


#include "gps.h"

int32 GPS_NMEA_Get_Apb(GPS_PApb *thys);
int32 GPS_NMEA_Get_Bod(GPS_PBod *thys);
int32 GPS_NMEA_Get_Bwc(GPS_PBwc *thys);
int32 GPS_NMEA_Get_Bwr(GPS_PBwr *thys);
int32 GPS_NMEA_Get_Dbt(GPS_PDbt *thys);
int32 GPS_NMEA_Get_Gga(GPS_PGga *thys);
int32 GPS_NMEA_Get_Gll(GPS_PGll *thys);
int32 GPS_NMEA_Get_Gsa(GPS_PGsa *thys);
int32 GPS_NMEA_Get_Gsv(GPS_PGsv *thys);
int32 GPS_NMEA_Get_Hdm(GPS_PHdm *thys);
int32 GPS_NMEA_Get_Hsc(GPS_PHsc *thys);
int32 GPS_NMEA_Get_Mtw(GPS_PMtw *thys);
int32 GPS_NMEA_Get_R00(GPS_PR00 *thys);
int32 GPS_NMEA_Get_Rmb(GPS_PRmb *thys);
int32 GPS_NMEA_Get_Rmc(GPS_PRmc *thys);
int32 GPS_NMEA_Get_Rte(GPS_PRte *thys);
int32 GPS_NMEA_Get_Vhw(GPS_PVhw *thys);
int32 GPS_NMEA_Get_Vwr(GPS_PVwr *thys);
int32 GPS_NMEA_Get_Vtg(GPS_PVtg *thys);
int32 GPS_NMEA_Get_Wpl(GPS_PWpl *thys);
int32 GPS_NMEA_Get_Xte(GPS_PXte *thys);
int32 GPS_NMEA_Get_Xtr(GPS_PXtr *thys);
int32 GPS_NMEA_Get_Rme(GPS_PRme *thys);
int32 GPS_NMEA_Get_Rmz(GPS_PRmz *thys);
int32 GPS_NMEA_Get_Rmm(GPS_PRmm *thys);
int32 GPS_NMEA_Get_Lib(GPS_PLib *thys);

#endif

#ifdef __cplusplus
}
#endif
