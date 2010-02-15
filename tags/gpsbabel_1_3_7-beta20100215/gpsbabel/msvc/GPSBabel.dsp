# Microsoft Developer Studio Project File - Name="GPSBabel" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=GPSBabel - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "GPSBabel.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "GPSBabel.mak" CFG="GPSBabel - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GPSBabel - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "GPSBabel - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /WX /GX /O2 /I "expat" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "__WIN32__" /D VERSION=\"1.2.6_beta06232005_msvc\" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib setupapi.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /WX /Gm /GX /ZI /Od /I "expat" /D "WIN32" /D "__WIN32__" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D VERSION=\"1.2.6_beta06232005_msvc\" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib setupapi.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "GPSBabel - Win32 Release"
# Name "GPSBabel - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "Jeeps"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\jeeps\gpsapp.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpscom.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsmath.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsmem.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsprot.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsread.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsrqst.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpssend.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsserial.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsusbread.c
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsusbsend.c
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsusbstub.c
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsusbwin.c
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsutil.c

!IF  "$(CFG)" == "GPSBabel - Win32 Release"

# PROP Intermediate_Dir "Release\Jeeps"

!ELSEIF  "$(CFG)" == "GPSBabel - Win32 Debug"

# PROP Intermediate_Dir "Debug\Jeeps"

!ENDIF 

# End Source File
# End Group
# Begin Group "ShapeLib"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\shapelib\dbfopen.c
# End Source File
# Begin Source File

SOURCE=..\shapelib\shpopen.c
# End Source File
# End Group
# Begin Source File

SOURCE=..\an1.c
# End Source File
# Begin Source File

SOURCE=..\arcdist.c
# End Source File
# Begin Source File

SOURCE=..\brauniger_iq.c
# End Source File
# Begin Source File

SOURCE=..\wbt-200.c
# End Source File
# Begin Source File

SOURCE=..\cetus.c
# End Source File
# Begin Source File

SOURCE=..\coastexp.c
# End Source File
# Begin Source File

SOURCE=..\copilot.c
# End Source File
# Begin Source File

SOURCE=..\csv_util.c
# End Source File
# Begin Source File

SOURCE=..\delgpl.c
# End Source File
# Begin Source File

SOURCE=..\duplicate.c
# End Source File
# Begin Source File

SOURCE=..\easygps.c
# End Source File
# Begin Source File

SOURCE=..\filter_vecs.c
# End Source File
# Begin Source File

SOURCE=..\garmin.c
# End Source File
# Begin Source File

SOURCE=..\garmin_tables.c
# End Source File
# Begin Source File

SOURCE=..\gcdb.c
# End Source File
# Begin Source File

SOURCE=..\geo.c
# End Source File
# Begin Source File

SOURCE=..\geoniche.c
# End Source File
# Begin Source File

SOURCE=..\glogbook.c
# End Source File
# Begin Source File

SOURCE=..\google.c
# End Source File
# Begin Source File

SOURCE=..\gpilots.c
# End Source File
# Begin Source File

SOURCE=..\gpspilot.c
# End Source File
# Begin Source File

SOURCE=..\gpsutil.c
# End Source File
# Begin Source File

SOURCE=..\gpx.c
# End Source File
# Begin Source File

SOURCE=..\grtcirc.c
# End Source File
# Begin Source File

SOURCE=..\hiketech.c
# End Source File
# Begin Source File

SOURCE=..\holux.c
# End Source File
# Begin Source File

SOURCE=..\hsa_ndv.c
# End Source File
# Begin Source File

SOURCE=..\html.c
# End Source File
# Begin Source File

SOURCE=..\igc.c
# End Source File
# Begin Source File

SOURCE=..\internal_styles.c
# End Source File
# Begin Source File

SOURCE=..\kml.c
# End Source File
# Begin Source File

SOURCE=..\lowranceusr.c
# End Source File
# Begin Source File

SOURCE=..\maggeo.c
# End Source File
# Begin Source File

SOURCE=..\magnav.c
# End Source File
# Begin Source File

SOURCE=..\magproto.c
# End Source File
# Begin Source File

SOURCE=..\main.c
# End Source File
# Begin Source File

SOURCE=..\mapopolis.c
# End Source File
# Begin Source File

SOURCE=..\mapsend.c
# End Source File
# Begin Source File

SOURCE=..\mapsource.c
# End Source File
# Begin Source File

SOURCE=..\mkshort.c
# End Source File
# Begin Source File

SOURCE=..\navicache.c
# End Source File
# Begin Source File

SOURCE=..\netstumbler.c
# End Source File
# Begin Source File

SOURCE=..\nmea.c
# End Source File
# Begin Source File

SOURCE=..\overlay.c
# End Source File
# Begin Source File

SOURCE=..\ozi.c
# End Source File
# Begin Source File

SOURCE=..\palmdoc.c
# End Source File
# Begin Source File

SOURCE=..\pathaway.c
# End Source File
# Begin Source File

SOURCE=..\pcx.c
# End Source File
# Begin Source File

SOURCE=..\polygon.c
# End Source File
# Begin Source File

SOURCE=..\position.c
# End Source File
# Begin Source File

SOURCE=..\psitrex.c
# End Source File
# Begin Source File

SOURCE=..\psp.c
# End Source File
# Begin Source File

SOURCE=..\queue.c
# End Source File
# Begin Source File

SOURCE=..\quovadis.c
# End Source File
# Begin Source File

SOURCE=..\reverse_route.c
# End Source File
# Begin Source File

SOURCE=..\route.c
# End Source File
# Begin Source File

SOURCE=..\saroute.c
# End Source File
# Begin Source File

SOURCE=..\shape.c
# End Source File
# Begin Source File

SOURCE=..\smplrout.c
# End Source File
# Begin Source File

SOURCE=..\sort.c
# End Source File
# Begin Source File

SOURCE=..\stackfilter.c
# End Source File
# Begin Source File

SOURCE=..\tef_xml.c
# End Source File
# Begin Source File

SOURCE=..\text.c
# End Source File
# Begin Source File

SOURCE=..\tiger.c
# End Source File
# Begin Source File

SOURCE=..\tmpro.c
# End Source File
# Begin Source File

SOURCE=..\tomtom.c
# End Source File
# Begin Source File

SOURCE=..\tpg.c
# End Source File
# Begin Source File

SOURCE=..\util.c
# End Source File
# Begin Source File

SOURCE=..\util_crc.c
# End Source File
# Begin Source File

SOURCE=..\uuid.c
# End Source File
# Begin Source File

SOURCE=..\vcf.c
# End Source File
# Begin Source File

SOURCE=..\vecs.c
# End Source File
# Begin Source File

SOURCE=..\vitosmt.c
# End Source File
# Begin Source File

SOURCE=..\vmem.c
# End Source File
# Begin Source File

SOURCE=..\waypt.c
# End Source File
# Begin Source File

SOURCE=..\xcsv.c
# End Source File
# Begin Source File

SOURCE=..\xmlgeneric.c
# End Source File
# Begin Source File

SOURCE=.\Expat\libexpat.lib
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Group "Jeeps-Headers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\jeeps\gps.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsapp.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpscom.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsdatum.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsfmt.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsinput.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsmath.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsmem.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsnmea.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsnmeafmt.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsnmeaget.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsport.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsproj.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsprot.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsread.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsrqst.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpssend.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsserial.h
# End Source File
# Begin Source File

SOURCE=..\jeeps\gpsutil.h
# End Source File
# End Group
# Begin Group "ShapeLib-Headers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\shapelib\shapefil.h
# End Source File
# End Group
# Begin Source File

SOURCE=..\csv_util.h
# End Source File
# Begin Source File

SOURCE=..\defs.h
# End Source File
# Begin Source File

SOURCE=..\garmin_tables.h
# End Source File
# Begin Source File

SOURCE=..\grtcirc.h
# End Source File
# Begin Source File

SOURCE=..\holux.h
# End Source File
# Begin Source File

SOURCE=..\magellan.h
# End Source File
# Begin Source File

SOURCE=..\mapsend.h
# End Source File
# Begin Source File

SOURCE=..\queue.h
# End Source File
# Begin Source File

SOURCE=..\quovadis.h
# End Source File
# Begin Source File

SOURCE=..\uuid.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
