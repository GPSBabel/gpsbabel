@echo off
REM
REM Simple Windows NT/2000/XP .cmd version of GPSBabel testo script
REM

SET TMPDIR=%TEMP%\WINTESTO
MKDIR %TMPDIR% 2>NUL:

GOTO :REALSTART

REM ==================================

:CommonCOMPARE
SET PARAM1=%2
SET PARAM2=%3
REM Test if param3 was a dir rather than a file, if so add a \* to make fc work
FOR %%A IN (%3) DO IF "d--------"=="%%~aA" SET PARAM2=%3\*
FOR /f "delims=" %%a IN ('fc %PARAM1% %PARAM2%') DO IF "x%%a"=="xFC: no differences encountered" GOTO :EOF
REM Show the first 5 lines of difference
fc %1 /LB5 %PARAM1% %PARAM2%
ECHO %* are not the same (first 5 differences above) - pausing. ^C to quit if required
PAUSE
GOTO :EOF

REM ==================================

:COMPARE
CALL :CommonCOMPARE /L %1 %2
GOTO :EOF

REM ==================================

:BINCOMPARE
CALL :CommonCOMPARE /B %1 %2
GOTO :EOF

REM ==================================

:SORTandCOMPARE
SORT <%1 >%TMPDIR%\s1
SORT <%2 >%TMPDIR%\s2
CALL :COMPARE %TMPDIR%\s1 %TMPDIR%\s2
GOTO :EOF

REM ==================================

:REALSTART


SET PNAME=.\gpsbabel
IF NOT EXIST %PNAME% ECHO Can't find %PNAME%&& GOTO :EOF






REM Geocaching .loc
DEL %TMPDIR%\gl.loc
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o geo -F %TMPDIR%\gl.loc
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gl.loc reference

REM GPSUtil
DEL %TMPDIR%\gu.wpt %TMPDIR%\1.gpx %TMPDIR%\2.gpx
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o gpsutil -F %TMPDIR%\gu.wpt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gu.wpt reference
@echo on
@echo Testing...
%PNAME% -i gpsutil -f %TMPDIR%\gu.wpt -o gpx -F %TMPDIR%\1.gpx
%PNAME% -i gpsutil -f reference\gu.wpt -o gpx -F %TMPDIR%\2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\1.gpx %TMPDIR%\2.gpx

REM GPSman 
DEL %TMPDIR%\gm.gm %TMPDIR%\gm.gm+
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o gpsman -F %TMPDIR%\gm.gm
%PNAME% -i gpsman -f %TMPDIR%\gm.gm -o gpsutil -F %TMPDIR%\gm.gm+
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gm.gm+ %TMPDIR%\gu.wpt

REM GPX
DEL %TMPDIR%\gl.gpx %TMPDIR%\gpx.gpx
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o gpx -F %TMPDIR%\gl.gpx
%PNAME% -i gpx -f %TMPDIR%\gl.gpx -o gpsutil -F %TMPDIR%\gpx.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gpx.gpx %TMPDIR%\gu.wpt

REM Magellan Mapsend
DEL %TMPDIR%\mm.mapsend %TMPDIR%\mm.gps
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o mapsend -F %TMPDIR%\mm.mapsend
%PNAME% -i mapsend -f %TMPDIR%\mm.mapsend -o gpsutil -F %TMPDIR%\mm.gps
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mm.gps %TMPDIR%\gu.wpt

REM Magellan serial
REM TODO

REM Tiger
REM This one is a little tacky, becuase it's a very lossy format.
REM so we simply test we can write it, and then read it and write it and
REM get an identical file back.
DEL %TMPDIR%\tiger
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o tiger -F %TMPDIR%\tiger
%PNAME% -i tiger -f %TMPDIR%\tiger -o tiger -F %TMPDIR%\tiger2
@echo off
@echo.
CALL :COMPARE %TMPDIR%\tiger %TMPDIR%\tiger2

REM CSV (Comma separated value) data.

@echo on
@echo Testing...
%PNAME%  -i geo -f geocaching.loc -o csv -F %TMPDIR%\csv.csv
%PNAME%  -i csv -f %TMPDIR%\csv.csv -o csv -F %TMPDIR%\csv2.csv
@echo off
@echo.
CALL :COMPARE %TMPDIR%\csv2.csv %TMPDIR%\csv.csv 

REM 
REM Delorme TopoUSA 4 is a CSV strain.  
REM 
DEL %TMPDIR%\xmap-1.gpx %TMPDIR%\xmap-2.gpx %TMPDIR%\xmap
@echo on
@echo Testing...
%PNAME% -i xmap -f reference\xmap -o xmap -F %TMPDIR%\xmap
%PNAME% -i xmap -f reference\xmap -o gpx -F %TMPDIR%\xmap-1.gpx
%PNAME% -i xmap -f %TMPDIR%\xmap -o gpx -F %TMPDIR%\xmap-2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\xmap-1.gpx %TMPDIR%\xmap-2.gpx
CALL :COMPARE reference\xmap %TMPDIR%\xmap

REM PCX (Garmin mapsource import) file format
DEL %TMPDIR%\mm.pcx %TMPDIR%\pcx.gps
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o pcx -F %TMPDIR%\mm.pcx
%PNAME% -i pcx -f %TMPDIR%\mm.pcx -o gpsutil -F %TMPDIR%\pcx.gps
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mm.gps %TMPDIR%\gu.wpt

REM Magellan file format
@echo on
@echo Testing...
%PNAME% -i magellan -f reference\magfile -o magellan -F %TMPDIR%\magfile
@echo off
@echo.
CALL :COMPARE %TMPDIR%\magfile reference\magfile

REM Navitrak DNA marker format
@echo on
@echo Testing...
%PNAME% -i dna -f reference\dnatest.txt -o dna -F %TMPDIR%\dnatest.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\dnatest.txt reference\dnatest.txt

REM PSP (PocketStreets 2002 Pushpin (.PSP)) file format. Use mxf as an 
REM intermediate format to avoid binary FP anomalies on compareerent platforms.
DEL %TMPDIR%\psp.mxf %TMPDIR%\mxf.psp
@echo on
@echo Testing...
%PNAME% -i psp -f reference\ps.psp -o mxf -F %TMPDIR%\psp.mxf
%PNAME% -i geo -f geocaching.loc -o mxf -F %TMPDIR%\mxf.psp
@echo off
@echo.
CALL :COMPARE %TMPDIR%\psp.mxf %TMPDIR%\mxf.psp
@echo on
@echo Testing...
%PNAME% -i psp -f reference\ps.psp -o gpx -F %TMPDIR%\psp1.gpx
%PNAME% -i psp -f reference\ps.psp -o psp -F %TMPDIR%\xxx.psp
%PNAME% -i psp -f %TMPDIR%\xxx.psp -o gpx -F %TMPDIR%\psp2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\psp1.gpx %TMPDIR%\psp2.gpx

REM MXF (Maptech Exchange Format) file format
DEL %TMPDIR%\mx.mxf %TMPDIR%\mxf.mxf
@echo on
@echo Testing...
%PNAME% -i mxf -f reference\mxf.mxf -o mxf -F %TMPDIR%\mx.mxf
%PNAME% -i mxf -f %TMPDIR%\mx.mxf -o mxf -F %TMPDIR%\mxf.mxf
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mxf.mxf reference

REM tmpro (TopoMapPro Places) file format
DEL %TMPDIR%\topomappro.txt %TMPDIR%\mxf.mxf
@echo on
@echo Testing...
%PNAME% -i tmpro -f reference\topomappro.txt -o tmpro -F %TMPDIR%\tmp.txt
%PNAME% -i tmpro -f %TMPDIR%\tmp.txt -o tmpro -F %TMPDIR%\topomappro.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\topomappro.txt reference

REM TPG (NG Topo!) file format
REM This is hard to test as the datum conversions create minute
REM inconsistencies in the coordinates.  So..  we test our i/o 
REM against a format that rounds higher than we care to compare
REM for binary data. 
DEL %TMPDIR%\topo.mxf %TMPDIR%\tpg.mxf %TMPDIR%\geo.tpg
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o tpg -F %TMPDIR%\geo.tpg
%PNAME% -i tpg -f %TMPDIR%\geo.tpg -o mxf -F %TMPDIR%\tpg.mxf
%PNAME% -i tpg -f reference\tpg.tpg -o mxf -F %TMPDIR%\topo.mxf
@echo off
@echo.
CALL :COMPARE %TMPDIR%\tpg.mxf %TMPDIR%\topo.mxf

REM OZI (OziExplorer 1.1) file format
DEL %TMPDIR%\oz.wpt %TMPDIR%\ozi.wpt
@echo on
@echo Testing...
%PNAME% -i ozi -f reference\ozi.wpt -o ozi -F %TMPDIR%\oz.wpt
%PNAME% -i ozi -f %TMPDIR%\oz.wpt -o ozi -F %TMPDIR%\ozi.wpt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ozi.wpt reference

REM Holux support is a little funky to test.  Becuase it loses precision,
REM if we convert it to another format, we lose accuracy (rounding) in the
REM coords, so converting it so something else and comparing it never works.
REM So we verify that we can read the reference and write it and get an
REM identical reference.
@echo on
@echo Testing...
%PNAME% -i holux -f reference\paris.wpo -o holux -F %TMPDIR%\paris.wpo
@echo off
@echo.
CALL :COMPARE reference\paris.wpo %TMPDIR%\paris.wpo

REM Magellan NAV Companion for PalmOS
REM This format is hard to test, because each record and the database itself
REM contains the time of creation, so two otherwise identical files won't
REM compare accurately.  In any case, the files are binary so compare wouldn't
REM like them.  So, we convert the reference file to gpsutil and the converted
REM file to gpsutil and make sure they're the same, and that they're the same
REM as one converted on a known-working installation.  Unfortunately, this does
REM not verify that the appinfo block was written correctly.  However, it does
REM successfully test for some endianness errors that might otherwise go 
REM unnoticed.
DEL %TMPDIR%\magnav.pdb %TMPDIR%\magnav.gpu %TMPDIR%\magnavt.gpu
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o magnav -F %TMPDIR%\magnav.pdb
%PNAME% -i magnav -f %TMPDIR%\magnav.pdb -o gpsutil -F %TMPDIR%\magnav.gpu
%PNAME% -i magnav -f reference\magnav.pdb -o gpsutil -F %TMPDIR%\magnavt.gpu
@echo off
@echo.
CALL :COMPARE %TMPDIR%\magnavt.gpu %TMPDIR%\magnav.gpu
CALL :COMPARE reference\gu.wpt %TMPDIR%\magnav.gpu

DEL %TMPDIR%\magnav.pdb
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o magnav -F %TMPDIR%\magnav.pdb
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\magnav.pdb reference\magnav.pdb



REM GPSPilot Tracker for PalmOS
REM This test is eerily similar to the NAV Companion test.  In fact, the 
REM converted reference file (magnavr.gpu) is identical.
DEL %TMPDIR%\gpspilot.pdb %TMPDIR%\gpspilot.gpu %TMPDIR%\gpspil_t.gpu
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o gpspilot -F %TMPDIR%\gpspilot.pdb
%PNAME% -i gpspilot -f %TMPDIR%\gpspilot.pdb -o gpsutil -F %TMPDIR%\gpspilot.gpu
%PNAME% -i gpspilot -f reference\gpspilot.pdb -o gpsutil -F %TMPDIR%\gpspil_t.gpu
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gpspil_t.gpu %TMPDIR%\gpspilot.gpu
CALL :COMPARE reference\gu.wpt %TMPDIR%\gpspilot.gpu

REM Cetus GPS for PalmOS
REM This test is also similar to the NAV Companion test.
DEL %TMPDIR%\cetus.pdb %TMPDIR%\cetus.gpu %TMPDIR%\cetust.gpu
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o cetus -F %TMPDIR%\cetus.pdb
%PNAME% -i cetus -f %TMPDIR%\cetus.pdb -o gpsutil -F %TMPDIR%\cetus.gpu
%PNAME% -i cetus -f reference\cetus.pdb -o gpsutil -F %TMPDIR%\cetust.gpu
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cetust.gpu %TMPDIR%\cetus.gpu
CALL :COMPARE reference\cetus.gpu %TMPDIR%\cetus.gpu

REM QuoVadis GPS for PalmOS
REM This test is derived from the Cetus test above.
DEL %TMPDIR%\quovadis.pdb %TMPDIR%\quovadis.gpu %TMPDIR%\quovadist.gpu
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o quovadis -F %TMPDIR%\quovadis.pdb
%PNAME% -i quovadis -f %TMPDIR%\quovadis.pdb -o gpsutil -F %TMPDIR%\quovadis.gpu
%PNAME% -i quovadis -f reference\quovadis.pdb -o gpsutil -F %TMPDIR%\quovadist.gpu
@echo off
@echo.
CALL :COMPARE %TMPDIR%\quovadist.gpu %TMPDIR%\quovadis.gpu
CALL :COMPARE reference\quovadis.gpu %TMPDIR%\quovadis.gpu

REM GpsDrive
DEL %TMPDIR%\gpsdrive.txt
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o gpsdrive -F %TMPDIR%\gpsdrive.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gpsdrive.txt reference
@echo on
@echo Testing...
%PNAME% -i gpsdrive -f reference\gpsdrive.txt -o gpsdrive -F %TMPDIR%\gpsdrive2.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gpsdrive2.txt reference\gpsdrive.txt

REM XMapHH Street Atlas USA file format
DEL %TMPDIR%\xmapwpt.wpt %TMPDIR%\xmapwpt.xmapwpt
@echo on
@echo Testing...
%PNAME% -i xmapwpt -f reference\xmapwpt.wpt -o xmapwpt -F %TMPDIR%\xmapwpt.xmapwpt
%PNAME% -i xmapwpt -f %TMPDIR%\xmapwpt.xmapwpt -o xmapwpt -F %TMPDIR%\xmapwpt.wpt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\xmapwpt.wpt reference

REM XCSV
REM Test that we can parse a style file, and read and write data in the 
REM same xcsv format (a complete test is virtually impossible).
ECHO RECORD_DELIMITER NEWLINE> %TMPDIR%\testo.style
ECHO FIELD_DELIMITER COMMA>> %TMPDIR%\testo.style
ECHO BADCHARS COMMA>> %TMPDIR%\testo.style
ECHO PROLOGUE Header>> %TMPDIR%\testo.style
ECHO EPILOGUE Footer>> %TMPDIR%\testo.style
ECHO IFIELD SHORTNAME,,%%s>> %TMPDIR%\testo.style
ECHO IFIELD LAT_DIRDECIMAL,,%%c%%lf>> %TMPDIR%\testo.style
ECHO IFIELD LON_DECIMALDIR,,%%lf%%c>> %TMPDIR%\testo.style
DEL %TMPDIR%\xcsv.geo %TMPDIR%\xcsv.xcsv
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o xcsv,style=%TMPDIR%\testo.style -F %TMPDIR%\xcsv.geo
%PNAME% -i xcsv,style=%TMPDIR%\testo.style -f %TMPDIR%\xcsv.geo -o xcsv,style=%TMPDIR%\testo.style -F %TMPDIR%\xcsv.xcsv
@echo off
@echo.
CALL :COMPARE %TMPDIR%\xcsv.geo %TMPDIR%\xcsv.xcsv

REM Garmin Mapsource This is a binary format with some undocumented
REM fields.  This test is therefore intentionally vague.  We read a file,
REM convert it to GPX, then write a file as MPS, then read it back and
REM write it as GPX and compare them.  Since we're writing both GPX files
REM ourselves from the same version, we're immune to changes in our own
REM GPX output.

@echo on
@echo Testing...
%PNAME% -i mapsource -f reference\mapsource.mps  -o gpx -F %TMPDIR%\ms1.gpx
%PNAME% -i mapsource -f reference\mapsource.mps  -o mapsource -F %TMPDIR%\ms.mps
%PNAME% -i mapsource -f %TMPDIR%\ms.mps -o gpx -F %TMPDIR%\ms2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ms1.gpx %TMPDIR%\ms2.gpx

REM 
REM MRCB mapsource track test
REM 
DEL %TMPDIR%\mps-track.mps
@echo on
@echo Testing...
%PNAME% -t -i mapsource -f reference\track\mps-track.mps -o mapsource,mpsverout=3 -F %TMPDIR%\mps-track.mps
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mps-track.mps reference\track
REM Now do a test of reading waypoints from a track-only file - should have an empty result
DEL %TMPDIR%\mps-track.mps
@echo on
@echo Testing...
%PNAME% -i mapsource -f reference\track\mps-track.mps -o mapsource,mpsverout=3 -F %TMPDIR%\mps-track.mps
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mps-track.mps reference\mps-empty.mps

REM 
REM MRCB mapsource route test
REM 
DEL %TMPDIR%\mps-route.mps
@echo on
@echo Testing...
%PNAME% -r -i mapsource -f reference\route\route.mps -o mapsource,mpsverout=4 -F %TMPDIR%\mps-route.mps
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mps-route.mps reference\route\route.mps

REM Now do a test of reading tracks from a route-only file - should have an empty result
DEL %TMPDIR%\mps-route.mps
@echo on
@echo Testing...
%PNAME% -t -i mapsource -f reference\route\route.mps -o mapsource,mpsverout=3 -F %TMPDIR%\mps-route.mps
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mps-route.mps reference\mps-empty.mps

REM 
REM Geocaching Database is a binary Palm format that, like the GPX variants
REM has a zillion "equivalent" encodings of any given record set.  So we
REM read the reference file, spin it to GPX and back to GCDB and then spin
REM that one to GPX.
REM 

@echo on
@echo Testing...
%PNAME% -i gcdb -f reference\GeocachingDB.PDB -o gpx -F %TMPDIR%\gcdb1.gpx -o gcdb -F %TMPDIR%\gcdb1.pdb
%PNAME% -i gpx -f %TMPDIR%\gcdb1.gpx -o gpx -F %TMPDIR%\gcdb2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gcdb1.gpx %TMPDIR%\gcdb1.gpx

REM 
REM Duplicate filter - Since filters have no format of their own, we use csv
REM as an intermediate format for testing the filter.
REM 
DEL %TMPDIR%\filterdupe.csv1 %TMPDIR%\filterdupe.csv2
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o csv -F %TMPDIR%\filterdupe.csv1
%PNAME% -i geo -f geocaching.loc -f geocaching.loc -x duplicate,shortname -o csv -F %TMPDIR%\filterdupe.csv2
@echo off
@echo.
CALL :SORTandCOMPARE %TMPDIR%\filterdupe.csv1 %TMPDIR%\filterdupe.csv2

REM 
REM Position filter -  Since very small distances are essentialy a duplicate 
REM position filter, we can test very similarly to the duplicate filter.
REM 
DEL %TMPDIR%\filterpos.csv1 %TMPDIR%\filterpos.csv2
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o csv -F %TMPDIR%\filterpos.csv1
%PNAME% -i geo -f geocaching.loc -f geocaching.loc -x position,distance=5f -o csv -F %TMPDIR%\filterpos.csv2
@echo off
@echo.
CALL :SORTandCOMPARE %TMPDIR%\filterpos.csv1 %TMPDIR%\filterpos.csv2

REM 
REM Radius filter
REM 
DEL %TMPDIR%\radius.csv
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -x radius,lat=35.9720,lon=-87.1347,distance=14.7 -o csv -F %TMPDIR%\radius.csv
@echo off
@echo.
CALL :COMPARE %TMPDIR%\radius.csv reference
REM 
REM magellan SD card waypoint / route format
REM 
DEL %TMPDIR%\magellan.rte
@echo on
@echo Testing...
%PNAME% -r -i magellan -f reference\route\magellan.rte -o magellan -F %TMPDIR%\magellan.rte
@echo off
@echo.
CALL :COMPARE %TMPDIR%\magellan.rte reference\route\magellan.rte

REM 
REM GPX routes -- since GPX contains a date stamp, tests will always
REM fail, so we use magellan as an interim format...
REM 
DEL %TMPDIR%\gpxroute.gpx %TMPDIR%\maggpx.rte
@echo on
@echo Testing...
%PNAME% -r -i gpx -f reference\route\route.gpx -o gpx -F %TMPDIR%\gpxroute.gpx
%PNAME% -r -i gpx -f %TMPDIR%\gpxroute.gpx -o magellan -F %TMPDIR%\maggpx.rte
@echo off
@echo.
CALL :COMPARE %TMPDIR%\maggpx.rte reference\route\magellan.rte

REM 
REM GPX tracks -- since GPX contains a date stamp, tests will always
REM fail, so we use magellan as an interim format...
REM 
DEL %TMPDIR%\gpxtrack.gpx %TMPDIR%\maggpx.trk
@echo on
@echo Testing...
%PNAME% -t -i gpx -f reference\track\tracks.gpx -o gpx -F %TMPDIR%\gpxtrack.gpx
%PNAME% -t -i magellan -f reference\track\meridian.trk -o gpx -F %TMPDIR%\maggpx.trk
@echo off
@echo.
CALL :COMPARE %TMPDIR%\maggpx.trk %TMPDIR%\gpxtrack.gpx

REM 
REM MAPSEND waypoint / route format
REM 
DEL %TMPDIR%\route.mapsend
@echo on
@echo Testing...
%PNAME% -r -i mapsend -f reference\route\route.mapsend -o mapsend -F %TMPDIR%\route.mapsend
@echo off
@echo.
CALL :COMPARE %TMPDIR%\route.mapsend reference\route
REM 
REM MAPSEND track format 
REM 
DEL %TMPDIR%\mapsend.trk
@echo on
@echo Testing...
%PNAME% -t -i mapsend -f reference\track\mapsend.trk -o mapsend -F %TMPDIR%\mapsend.trk
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mapsend.trk reference\track
REM 
REM copilot
REM 
DEL %TMPDIR%\copilot.pdb
@echo on
@echo Testing...
%PNAME% -i copilot -f reference\UKultralight.pdb -o copilot -F %TMPDIR%\cop.pdb
%PNAME% -i copilot -f reference\UKultralight.pdb -o gpx -F %TMPDIR%\cop1.gpx
%PNAME% -i copilot -f %TMPDIR%\cop.pdb -o gpx -F %TMPDIR%\cop2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cop1.gpx %TMPDIR%\cop2.gpx

REM 
REM EasyGPS.   Another binary format.
REM 
DEL %TMPDIR%\easy.loc
@echo on
@echo Testing...
%PNAME% -i easygps -f reference\easygps.loc -o easygps -F %TMPDIR%\ez.loc
%PNAME% -i easygps -f reference\easygps.loc -o gpx -F %TMPDIR%\ez1.gpx
%PNAME% -i easygps -f %TMPDIR%\ez.loc -o gpx -F %TMPDIR%\ez2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ez1.gpx %TMPDIR%\ez2.gpx

REM 
REM GPilotS.   A Palm format.  Another binary format that 
REM 
REM rm -f ${TMPDIR/gpilots.l
REM ${PNAME} -i easygps -f reference/gpilots.pdb -o gpx -F ${TMPDIR}/gp.gpx
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o gpilots -F %TMPDIR%\blah.pdb
%PNAME% -i gpilots -f %TMPDIR%\blah.pdb -o gpx -F %TMPDIR%\1.gpx
%PNAME% -i gpilots -f reference\gpilots.pdb -o gpx -F %TMPDIR%\2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\1.gpx %TMPDIR%\2.gpx
REM ${PNAME} -i easygps -f reference/gpilots.pdb -o gpx -F ${TMPDIR}/gp.gpx

REM 
REM Navicache.
@echo on
@echo Testing...
%PNAME% -i navicache -f reference\navicache.xml -o gpsutil -F %TMPDIR%\navi.wpt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\navi.wpt reference\navicache.ref
REM 

REM PsiTrex.  A text format that can't be handled by XCSV due to context of
REM data based on other data values in the file
REM Waypoints first
DEL %TMPDIR%\psit-ww.txt %TMPDIR%\psit-ww.mps
@echo on
@echo Testing...
%PNAME% -i psitrex -f reference\psitwpts.txt -o mapsource -F %TMPDIR%\psit-ww.mps
%PNAME% -i mapsource -f %TMPDIR%\psit-ww.mps -o psitrex -F %TMPDIR%\psit-ww.txt
@echo off
@echo.
CALL :COMPARE reference\psitwpts.txt %TMPDIR%\psit-ww.txt

REM Now test correct "empty" handling - ask for routes when there aren't any
REM Uses mapsource as the empty handling for this has already happened above
DEL %TMPDIR%\psit-wr.mps
@echo on
@echo Testing...
%PNAME% -r -i psitrex -f reference\psitwpts.txt -o mapsource,mpsverout=3 -F %TMPDIR%\psit-wr.mps
@echo off
@echo.
CALL :COMPARE reference\mps-empty.mps %TMPDIR%\psit-wr.mps

REM Routes next
DEL %TMPDIR%\psit-rr.txt %TMPDIR%\psit-rr.mps
@echo on
@echo Testing...
%PNAME% -r -i psitrex -f reference\route\psitrtes.txt -o mapsource -F %TMPDIR%\psit-rr.mps
%PNAME% -r -i mapsource -f %TMPDIR%\psit-rr.mps -o psitrex -F %TMPDIR%\psit-rr.txt
@echo off
@echo.
CALL :COMPARE reference\route\psitrtes.txt %TMPDIR%\psit-rr.txt

REM Now test correct "empty" handling - ask for tracks when there aren't any
REM Uses mapsource as the empty handling for this has already happened above
DEL %TMPDIR%\psit-rt.mps
@echo on
@echo Testing...
%PNAME% -t -i psitrex -f reference\route\psitrtes.txt -o mapsource,mpsverout=3 -F %TMPDIR%\psit-rt.mps
@echo off
@echo.
CALL :COMPARE reference\mps-empty.mps %TMPDIR%\psit-rt.mps

REM Tracks last
DEL %TMPDIR%\psit-tt.txt %TMPDIR%\psit-tt.mps
@echo on
@echo Testing...
%PNAME% -t -i psitrex -f reference\track\psittrks.txt -o mapsource -F %TMPDIR%\psit-tt.mps
%PNAME% -t -i mapsource -f %TMPDIR%\psit-tt.mps -o psitrex -F %TMPDIR%\psit-tt.txt
@echo off
@echo.
CALL :COMPARE reference\track\psittrks.txt %TMPDIR%\psit-tt.txt

REM Now test correct "empty" handling - ask for waypoints when there aren't any
REM Uses mapsource as the empty handling for this has already happened above
DEL %TMPDIR%\psit-tw.mps
@echo on
@echo Testing...
%PNAME% -i psitrex -f reference\track\psittrks.txt -o mapsource,mpsverout=3 -F %TMPDIR%\psit-tw.mps
@echo off
@echo.
CALL :COMPARE reference\mps-empty.mps %TMPDIR%\psit-tw.mps

REM 
REM Arc Distance filter
REM 
DEL %TMPDIR%\arcdist.txt
@echo on
@echo Testing...
%PNAME% -i xmap -f reference\arcdist_input.txt -x arc,file=reference\arcdist_arc.txt,distance=1 -o xmap -F %TMPDIR%\arcdist.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\arcdist.txt reference\arcdist_output.txt

REM 
REM Polygon filter
REM 
DEL %TMPDIR%\polygon.txt
@echo on
@echo Testing...
%PNAME% -i xmap -f reference\arcdist_input.txt -x polygon,file=reference\polygon_allencty.txt -o xmap -F %TMPDIR%\polygon.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\polygon.txt reference\polygon_output.txt

REM 
REM Simplify filter
REM 
DEL %TMPDIR%\simplify.txt
@echo on
@echo Testing...
%PNAME% -r -i gpx -f reference\route\route.gpx -x simplify,count=10 -o arc -F %TMPDIR%\simplify.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\simplify.txt reference\simplify_output.txt

REM 
REM Route reversal filter.   Do it twice and be sure we get what we
REM started with.
REM 
DEL %TMPDIR%\reverse1.arc %TMPDIR%\reverse2.arc %TMPDIR%\reference.arc
@echo on
@echo Testing...
%PNAME% -r -i gpx -f reference\route\route.gpx -o arc -F %TMPDIR%\reference.arc
%PNAME% -r -i gpx -f reference\route\route.gpx -x reverse -o arc -F %TMPDIR%\reverse1.arc
%PNAME% -r -i gpx -f reference\route\route.gpx -x reverse -x reverse -o arc -F %TMPDIR%\reverse2.arc
@echo off
@echo.
REM Verify the first and last are the same
CALL :COMPARE %TMPDIR%\reference.arc  %TMPDIR%\reverse2.arc
REM Verify the first and second are different.
REM ${DIFF}  ${TMPDIR}/reverse1.arc  ${TMPDIR}/reverse2.arc > /dev/null && {
REM 		echo ERROR Failed reversal test.
REM 		exit 1
REM }

REM parkrrrr: This isn't a straightforward compare; we *want* it to fail.
REM Obviously this test should just be rewritten with a new reference.
REM compare  ${TMPDIR}/reverse1.arc  ${TMPDIR}/reverse2.arc

REM 
REM Geoniche: No reference file was available, so we created one and just
REM test it against itself.
REM 
DEL %TMPDIR%\gn.pdb %TMPDIR%\1.gpx %TMPDIR%\2.gpx
@echo on
@echo Testing...
%PNAME% -i geoniche -f reference\geoniche.pdb -o geoniche -F %TMPDIR%\gn.pdb
%PNAME% -i geoniche -f reference\geoniche.pdb -o gpx -F %TMPDIR%\1.gpx
%PNAME% -i geoniche -f %TMPDIR%\gn.pdb -o gpx -F %TMPDIR%\2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\1.gpx %TMPDIR%\2.gpx

REM 
REM saroute covers *.anr, *.rte, and *.rtd, but I only have an .anr for testing.
REM Unfortunately for us, this is a read-only format for now.
REM 
@echo on
@echo Testing...
%PNAME% -t -i saroute -f reference\track\i65.anr -o gpx -F %TMPDIR%\gpl1.gpx
%PNAME% -t -i gpx -f reference\track\i65.anr.gpx -o gpx -F %TMPDIR%\gpl2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gpl1.gpx %TMPDIR%\gpl2.gpx

REM 
REM Delorme GPL file.   This is sort of a track format.
REM 
DEL %TMPDIR%\gpl1.gpx %TMPDIR%\gpl2.gpx %TMPDIR%\gpl1.gpl
@echo on
@echo Testing...
%PNAME% -t -i gpl -f reference\track\webpark1.gpl -o gpx -F %TMPDIR%\gpl1.gpx
%PNAME% -t -i gpl -f reference\track\webpark1.gpl -o gpl -F %TMPDIR%\gpl1.gpl
%PNAME% -t -i gpl -f %TMPDIR%\gpl1.gpl -o gpx -F %TMPDIR%\gpl2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gpl1.gpx %TMPDIR%\gpl2.gpx

REM 
REM NetStumbler Summary File (read-only)
REM 
DEL %TMPDIR%\netstumbler.mps
@echo on
@echo Testing...
%PNAME% -i netstumbler -f reference\netstumbler.txt -o mapsource -F %TMPDIR%\netstumbler.mps
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\netstumbler.mps reference\netstumbler.mps

REM 
REM IGC tests
REM 
DEL %TMPDIR%\igc*out
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\igc1.gpx -o igc -F %TMPDIR%\igc.out
@echo off
@echo.
FINDSTR /V "XXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
CALL :COMPARE %TMPDIR%\igc_sed.out reference\igc1_igc.out

@echo on
@echo Testing...
%PNAME% -i igc -f %TMPDIR%\igc.out -o gpx -F %TMPDIR%\igc.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\igc.gpx reference\igc1_gpx.out

@echo on
@echo Testing...
%PNAME% -i gpx -f %TMPDIR%\igc.gpx -o igc -F %TMPDIR%\igc.out
@echo off
@echo.
FINDSTR /V "XXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
CALL :COMPARE %TMPDIR%\igc_sed.out reference\igc1_igc.out

@echo on
@echo Testing...
%PNAME% -i gpx -f reference\igc1_baro.gpx -i igc -f reference\igc1_igc.out -o igc,timeadj=auto -F %TMPDIR%\igc.out
@echo off
@echo.
FINDSTR /V "XXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
CALL :COMPARE %TMPDIR%\igc_sed.out reference\igc1_3d.out


@echo on
@echo Testing...
%PNAME% -i igc -f reference\igc2.igc -o gpx -F %TMPDIR%\igc.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\igc.gpx reference\igc2_gpx.out

@echo on
@echo Testing...
%PNAME% -i gpx -f %TMPDIR%\igc.gpx -o igc -F %TMPDIR%\igc.out
@echo off
@echo.
FINDSTR /V "XXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
CALL :COMPARE %TMPDIR%\igc_sed.out reference\igc2_igc.out

@echo on
@echo Testing...
%PNAME% -i igc -f %TMPDIR%\igc.out -o gpx -F %TMPDIR%\igc.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\igc.gpx reference\igc2_gpx.out


REM 
REM XCSV "human readable" tests
REM 
DEL %TMPDIR%\humanread.out
@echo on
@echo Testing...
%PNAME% -i xcsv,style=reference\humanread.style -f reference\human.in -o arc -F %TMPDIR%\humanread.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\humanread.out reference\humanread.out

DEL %TMPDIR%\humanwrite.out
@echo on
@echo Testing...
%PNAME% -i xcsv,style=reference\humanread.style -f reference\human.in -o xcsv,style=reference\humanwrite.style -F %TMPDIR%\humanwrite.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\humanwrite.out reference\humanwrite.out

REM 
REM XCSV "path distance" test
REM 
DEL %TMPDIR%\pathdist.out
@echo on
@echo Testing...
%PNAME% -i magellan -f reference\dusky.trk -o xcsv,style=reference\gnuplot.style -F %TMPDIR%\pathdist.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\pathdist.out reference\dusky.gnuplot

REM hsandv
DEL %TMPDIR%\hsandv.exp %TMPDIR%\1.exp %TMPDIR%\1.exp %TMPDIR%\Glad_5.exp
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o hsandv -F %TMPDIR%\hsandv.exp
@echo off
@echo.
CALL :COMPARE %TMPDIR%\hsandv.exp reference
REM the hsandv format is too lossy to do this test :(
REM ${PNAME} -i hsandv -f ${TMPDIR}/hsandv.exp -o geo -F ${TMPDIR}/1.exp
REM ${PNAME} -i hsandv -f reference/hsandv.exp -o geo -F ${TMPDIR}/2.exp
REM compare ${TMPDIR}/1.exp ${TMPDIR}/2.exp
REM test conversion from v4 to v5 files
@echo on
@echo Testing...
%PNAME% -i hsandv -f reference\Glad_4.exp -o hsandv -F %TMPDIR%\Glad_5.exp
@echo off
@echo.
REM Can't compare directly because of potential FP rounding.
@echo on
@echo Testing...
%PNAME% -i hsandv -f %TMPDIR%\Glad_5.exp -o gpx -F %TMPDIR%\g5.gpx
%PNAME% -i hsandv -f reference\Glad_5.exp -o gpx -F %TMPDIR%\g5r.gpx
@echo off
@echo.
REM exit 1
REM compare  ${TMPDIR}/g5.gpx ${TMPDIR}/g5r.gpx

REM 
REM stack filter tests
REM These don't actually test for proper behavior, for now, but they do 
REM exercise all of the currently-extant filter code.
REM 

@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -x stack,push,copy,nowarn -x stack,push,copy -x stack,push -x stack,pop,replace -x stack,pop,append -x stack,push,copy -x stack,pop,discard -x stack,swap,depth=1 -o
@echo off
@echo.

