#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsmem_h
#define gpsmem_h


#include "gps.h"

GPS_PPacket   GPS_Packet_New(void);
void          GPS_Packet_Del(GPS_PPacket *thys);    
GPS_PPvt_Data GPS_Pvt_New(void);
void          GPS_Pvt_Del(GPS_PPvt_Data *thys);
GPS_PAlmanac  GPS_Almanac_New(void);
void          GPS_Almanac_Del(GPS_PAlmanac *thys);
GPS_PTrack    GPS_Track_New(void);
void          GPS_Track_Del(GPS_PTrack *thys);
GPS_PWay      GPS_Way_New(void);
void          GPS_Way_Del(GPS_PWay *thys);


/*
 *  NMEA Section
 */
GPS_PGsv      GPS_Gsv_New(void);
void          GPS_Gsv_Del(GPS_PGsv *thys);
GPS_PRme      GPS_Rme_New(void);
void          GPS_Rme_Del(GPS_PRme *thys);
GPS_PGll      GPS_Gll_New(void);
void          GPS_Gll_Del(GPS_PGll *thys);
GPS_PRmz      GPS_Rmz_New(void);
void          GPS_Rmz_Del(GPS_PRmz *thys);
GPS_PRmm      GPS_Rmm_New(void);
void          GPS_Rmm_Del(GPS_PRmm *thys);
GPS_PBod      GPS_Bod_New(void);
void          GPS_Bod_Del(GPS_PBod *thys);
GPS_PRte      GPS_Rte_New(void);
void          GPS_Rte_Del(GPS_PRte *thys);
GPS_PRmc      GPS_Rmc_New(void);
void          GPS_Rmc_Del(GPS_PRmc *thys);
GPS_PRmb      GPS_Rmb_New(void);
void          GPS_Rmb_Del(GPS_PRmb *thys);
GPS_PGga      GPS_Gga_New(void);
void          GPS_Gga_Del(GPS_PGga *thys);
GPS_PGsa      GPS_Gsa_New(void);
void          GPS_Gsa_Del(GPS_PGsa *thys);
GPS_PApb      GPS_Apb_New(void);
void          GPS_Apb_Del(GPS_PApb *thys);
GPS_PBwc      GPS_Bwc_New(void);
void          GPS_Bwc_Del(GPS_PBwc *thys);
GPS_PBwr      GPS_Bwr_New(void);
void          GPS_Bwr_Del(GPS_PBwr *thys);
GPS_PDbt      GPS_Dbt_New(void);
void          GPS_Dbt_Del(GPS_PDbt *thys);
GPS_PHdm      GPS_Hdm_New(void);
void          GPS_Hdm_Del(GPS_PHdm *thys);
GPS_PHsc      GPS_Hsc_New(void);
void          GPS_Hsc_Del(GPS_PHsc *thys);
GPS_PMtw      GPS_Mtw_New(void);
void          GPS_Mtw_Del(GPS_PMtw *thys);
GPS_PR00      GPS_R00_New(void);
void          GPS_R00_Del(GPS_PR00 *thys);
GPS_PVhw      GPS_Vhw_New(void);
void          GPS_Vhw_Del(GPS_PVhw *thys);
GPS_PVwr      GPS_Vwr_New(void);
void          GPS_Vwr_Del(GPS_PVwr *thys);
GPS_PVtg      GPS_Vtg_New(void);
void          GPS_Vtg_Del(GPS_PVtg *thys);
GPS_PXte      GPS_Xte_New(void);
void          GPS_Xte_Del(GPS_PXte *thys);
GPS_PXtr      GPS_Xtr_New(void);
void          GPS_Xtr_Del(GPS_PXtr *thys);
GPS_PLib      GPS_Lib_New(void);
void          GPS_Lib_Del(GPS_PLib *thys);
GPS_PWpl      GPS_Wpl_New(void);
void          GPS_Wpl_Del(GPS_PWpl *thys);
GPS_PNmea     GPS_Nmea_New(void);
void          GPS_Nmea_Del(GPS_PNmea *thys);



#endif

#ifdef __cplusplus
}
#endif
