@echo off
REM
REM Simple Windows NT/2000/XP .cmd version of GPSBabel testo script
REM

SET GPSBABEL_FREEZE_TIME=y
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
if errorlevel 1 ECHO %* are not the same (first 5 differences above) - pausing. ^C to quit if required
if errorlevel 1 PAUSE
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


REM Turn on GNU libc instrumentation.

SET PNAME=.\gpsbabel
IF NOT EXIST %PNAME%.EXE ECHO Can't find %PNAME%&& GOTO :EOF





REM Some formats are just too boring to test.   The ones that
REM are xcsv include 
REM garmin301 
REM garmin_poi 
REM gpsdrivetrack
REM nima 
REM mapconverter
REM geonet
REM saplus
REM s_and_t
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

REM GTM
DEL %TMPDIR%\gl.gpx %TMPDIR%\gpx.gpx
@echo on
@echo Testing...
%PNAME% -i gtm -f reference\sample.gtm -o gpx -F %TMPDIR%\gtm1.gpx
%PNAME% -i gpx -f %TMPDIR%\gtm1.gpx -o gtm -F %TMPDIR%\gtm.gtm
%PNAME% -i gtm -f %TMPDIR%\gtm.gtm -o gpx -F %TMPDIR%\gtm2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gtm1.gpx %TMPDIR%\gtm2.gpx
CALL :COMPARE %TMPDIR%\gtm.gtm reference\sample.gtm

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
REM This one is a little tacky, because it's a very lossy format.
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

REM 
REM Lowrance USR.  Binary, and also slightly lossy because of the math to
REM convert lat/long.  It also doesn't support description, which makes it
REM awkward  to test.
REM 
DEL %TMPDIR%\lowrance1.usr
DEL %TMPDIR%\enchilada1.usr
DEL %TMPDIR%\enchilada.gpx
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o lowranceusr -F %TMPDIR%\lowrance1.usr
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\lowrance1.usr reference\lowrance.usr
@echo on
@echo Testing...
%PNAME% -i lowranceusr -f %TMPDIR%\lowrance1.usr -o lowranceusr -F %TMPDIR%\lowrance1.usr
@echo off
@echo.
REM And because of the FP rounding, we can't even read our file, write it back
REM and get the same data.  Sigh. 
REM bincompare reference/lowrance.usr  ${TMPDIR}/lowrance1.usr
@echo on
@echo Testing...
%PNAME% -i lowranceusr -f reference\all.usr -o gpx -F %TMPDIR%\enchilada.gpx
%PNAME% -i gpx -f %TMPDIR%\enchilada.gpx -o lowranceusr -F %TMPDIR%\enchilada1.usr
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\enchilada1.usr reference\enchilada.usr
REM Don't convert icons as waypts
@echo on
@echo Testing...
%PNAME% -i lowranceusr,ignoreicons -f reference\all.usr -o gpx -F %TMPDIR%\enchilada.gpx
%PNAME% -i gpx -f %TMPDIR%\enchilada.gpx -o lowranceusr -F %TMPDIR%\enchilada1.usr
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\enchilada1.usr reference\ignoreicons.usr

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
@echo on
@echo Testing...
%PNAME% -t -i gpx -f reference\track\tracks.gpx -o pcx -F %TMPDIR%\pcx.trk
%PNAME% -t -i pcx -f reference\track\pcx.trk -o pcx -F %TMPDIR%\pcx2.trk
@echo off
@echo.
CALL :COMPARE %TMPDIR%\pcx.trk %TMPDIR%\pcx2.trk 

REM 
REM Magellan file format
REM 
@echo on
@echo Testing...
%PNAME% -i magellan -f reference\magfile -o magellan -F %TMPDIR%\magfile
@echo off
@echo.
CALL :COMPARE %TMPDIR%\magfile reference\magfile

REM 
REM Magellanx is just like, but with longer names. (which this admittedly
REM doesn't actually exercise...)
REM 
@echo on
@echo Testing...
%PNAME% -i magellan -f reference\magfile -o magellanx -F %TMPDIR%\magfile2
@echo off
@echo.
CALL :COMPARE %TMPDIR%\magfile2 reference\magfile

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

REM TPO (NG Topo!) file format
REM This is hard to test as the datum conversions create minute
REM inconsistencies in the coordinates. We have four reference files:
REM tpo-sample1.tpo, tpo-sample1.gpx, tpo-sample2.gpx, and
REM tpo-sample2.tpo.  These are used to check the conversion to/from
REM TPO format.
DEL %TMPDIR%\tpo-sample1.gpx %TMPDIR%\tpo-sample2.tpo
@echo on
@echo Testing...
%PNAME% -t -i tpo -f reference\track\tpo-sample1.tpo -o gpx -F %TMPDIR%\tpo-sample1.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\tpo-sample1.gpx reference\track\tpo-sample1.gpx
REM ${PNAME} -t -i gpx -f reference/track/tpo-sample2.gpx -o tpo -F ${TMPDIR}/tpo-sample2.tpo
REM bincompare ${TMPDIR}/tpo-sample2.tpo reference/track/tpo-sample2.tpo

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
REM compare reference/paris.wpo ${TMPDIR}/paris.wpo

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
CALL :BINCOMPARE %TMPDIR%\route.mapsend reference\route\route.mapsend

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

REM 
REM CoastalExplorer..
@echo on
@echo Testing...
%PNAME% -r -i coastexp -f reference\coastexp.nob -o gpx -F %TMPDIR%\coastexp.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\coastexp.gpx reference\coastexp.ref
@echo on
@echo Testing...
%PNAME% -r -i gpx -f %TMPDIR%\coastexp.gpx -o coastexp -F %TMPDIR%\coastexp.nob
@echo off
@echo.
CALL :COMPARE %TMPDIR%\coastexp.nob reference\coastexp.ref2
@echo on
@echo Testing...
%PNAME% -w -i coastexp -f reference\coastexp.nob -o gpx -F %TMPDIR%\coastexp.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\coastexp.gpx reference\coastexp.ref3
@echo on
@echo Testing...
%PNAME% -w -i gpx -f %TMPDIR%\coastexp.gpx -o coastexp -F %TMPDIR%\coastexp.nob
@echo off
@echo.
CALL :COMPARE %TMPDIR%\coastexp.nob reference\coastexp.ref4
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
@echo on
@echo Testing...
%PNAME% -i geoniche -f reference\gn-targets.pdb -o gpx -F %TMPDIR%\gn-targets.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\gn-targets.gpx reference\gn-targets.gpx

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
FINDSTR /V /R /C:"^LXXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
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
FINDSTR /V /R /C:"^LXXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
CALL :COMPARE %TMPDIR%\igc_sed.out reference\igc1_igc.out

@echo on
@echo Testing...
%PNAME% -i gpx -f reference\igc1_baro.gpx -i igc -f reference\igc1_igc.out -o igc,timeadj=auto -F %TMPDIR%\igc.out
@echo off
@echo.
FINDSTR /V /R /C:"^LXXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
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
FINDSTR /V /R /C:"^LXXXGenerated by GPSBabel Version" %TMPDIR%\igc.out> %TMPDIR%\igc_sed.out
CALL :COMPARE %TMPDIR%\igc_sed.out reference\igc2_igc.out

@echo on
@echo Testing...
%PNAME% -i igc -f %TMPDIR%\igc.out -o gpx -F %TMPDIR%\igc.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\igc.gpx reference\igc2_gpx.out

REM 
REM Google Maps XML test
REM 
DEL %TMPDIR%\google.out
@echo on
@echo Testing...
%PNAME% -i google -f reference\google.xml -o csv -F %TMPDIR%\google.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\google.out reference\google.csv

DEL %TMPDIR%\google.out
@echo on
@echo Testing...
%PNAME% -i google -f reference\google.js -o csv -F %TMPDIR%\google.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\google.out reference\google.csv

DEL %TMPDIR%\google.out
@echo on
@echo Testing...
%PNAME% -i google -f reference\google_jan_06.html -o csv -F %TMPDIR%\google.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\google.out reference\google_jan_06.csv

REM 
REM DeLorme .an1 tests
REM 
DEL %TMPDIR%\an1.out
@echo on
@echo Testing...
%PNAME% -i an1 -f reference\foo.an1 -o csv -F %TMPDIR%\an1.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\an1.out reference\an1-in.ref

DEL %TMPDIR%\an1.out
@echo on
@echo Testing...
%PNAME% -i an1 -f reference\foo.an1 -o an1 -F %TMPDIR%\an1.out
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\an1.out reference\an1-an1.ref

DEL %TMPDIR%\an1.out
@echo on
@echo Testing...
%PNAME% -i xmap -f reference\xmap -o an1 -F %TMPDIR%\an1.out 
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\an1.out reference\an1-out.ref

DEL %TMPDIR%\an1.out
@echo on
@echo Testing...
%PNAME% -i google -f reference\google.js -o an1 -F %TMPDIR%\an1.out
@echo off
@echo.
CALL :BINCOMPARE %TMPDIR%\an1.out reference\an1-line-out.ref

REM 
REM TomTom .ov2 tests
REM 

DEL %TMPDIR%\ov2.out
@echo on
@echo Testing...
%PNAME% -i arc -f reference\google.arc -o tomtom -F %TMPDIR%\ov2.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ov2.out reference\ov2-arc-out.ref

DEL %TMPDIR%\ov2.out
@echo on
@echo Testing...
%PNAME% -i geo -f reference\gl.loc -o tomtom -F %TMPDIR%\ov2.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ov2.out reference\ov2-geo-out.ref

DEL %TMPDIR%\ov2.out
@echo on
@echo Testing...
%PNAME% -i tomtom -f reference\ov2-geo-out.ref -o gpsutil -F %TMPDIR%\ov2.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ov2.out reference\ov2-in.ref

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
REM FIXME: Can't compare directly because of potential FP rounding.
REM FIXME: compare ${TMPDIR}/Glad_5.exp reference

REM 
REM stack filter tests
REM These don't actually test for proper behavior, for now, but they do 
REM exercise all of the currently-extant filter code.
REM 

@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -x stack,push,copy,nowarn -x stack,push,copy -x stack,push -x stack,pop,replace -x stack,pop,append -x stack,push,copy -x stack,pop,discard -x stack,swap,depth=1 -o arc -F %TMPDIR%\stackfilt.txt
@echo off
@echo.

REM 
REM 'tabsep' isn't really tested in any non-trivial way, but we do exercise
REM it.
REM 

@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc  -o tabsep -F %TMPDIR%\tabsep.in
%PNAME% -i tabsep -f %TMPDIR%\tabsep.in -o geo -F %TMPDIR%\tabsep.out
%PNAME% -i geo -f geocaching.loc  -o geo -F %TMPDIR%\geotabsep.out
@echo off
@echo.
CALL :COMPARE %TMPDIR%\tabsep.out %TMPDIR%\geotabsep.out

REM 
REM Now do the same for custom - it has the same issues.
REM 

@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc  -o custom -F %TMPDIR%\custom.in
%PNAME% -i custom -f %TMPDIR%\custom.in -o geo -F %TMPDIR%\custom.out
%PNAME% -i geo -f geocaching.loc  -o geo -F %TMPDIR%\geocustom.out
@echo off
@echo.

REM 
REM Write something to the various output-only formats
REM 
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o text -F %TMPDIR%\text.out -o html -F %TMPDIR%\html.out -o vcard -F %TMPDIR%\vcard.out #-o palmdoc -F %TMPDIR%\pd.out
@echo off
@echo.

REM 
REM tef "TourExchangeFormat" read test
REM 
DEL %TMPDIR%\tef_xml*
@echo on
@echo Testing...
%PNAME% -r -i tef -f reference\route\tef_xml.sample.xml -o gpx -F %TMPDIR%\tef_xml.sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\tef_xml.sample.gpx reference\route\tef_xml.sample.gpx 

REM 
REM PathAway Palm Database .pdb tests
REM 
DEL %TMPDIR%\pathaway*
@echo on
@echo Testing...
%PNAME% -i geo -f geocaching.loc -o pathaway,dbname=pathaway-geo -F %TMPDIR%\pathaway-geo.pdb
%PNAME% -i pathaway -f %TMPDIR%\pathaway-geo.pdb -o geo -F %TMPDIR%\pathaway-geo.loc
@echo off
@echo.
CALL :COMPARE %TMPDIR%\pathaway-geo.loc reference\pathaway-geo.loc
DEL %TMPDIR%\pathaway*
@echo on
@echo Testing...
%PNAME% -t -i pathaway -f reference\track\pathaway.pdb -o gpx -F %TMPDIR%\pathaway.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\pathaway.gpx reference\track\pathaway.gpx

REM 
REM Garmin GPS Database .gdb tests
REM 
DEL %TMPDIR%\gdb-*
@echo on
@echo Testing...
%PNAME% -i gdb,via -f reference\gdb-sample.gdb -o gpx -F %TMPDIR%\gdb-sample.gpx
@echo off
@echo.
CALL :COMPARE reference\gdb-sample.gpx %TMPDIR%\gdb-sample.gpx
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\gdb-sample.gpx -o gdb,ver=1 -F %TMPDIR%\gdb-sample.gdb
%PNAME% -i gdb -f %TMPDIR%\gdb-sample.gdb -o gpx -F %TMPDIR%\gdb-sample.gpx
@echo off
@echo.
REM 
REM Because of Garmin coordinates storage gpx is not good for this test
REM compare reference/gdb-sample.gpx ${TMPDIR}/gdb-sample.gpx
REM 
REM compare ${TMPDIR}/gdb-sample.gpx reference/gdb-sample.gpx 

REM 
REM Vito Navigator II .smt tests
REM 
DEL %TMPDIR%\vitosmt*
@echo on
@echo Testing...
%PNAME%    -i vitosmt -f reference\vitosmt.smt -o gpx -F %TMPDIR%\vitosmt.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\vitosmt.gpx reference\vitosmt.gpx
@echo on
@echo Testing...
%PNAME% -t -i vitosmt -f reference\vitosmt.smt -o gpx -F %TMPDIR%\vitosmt_t.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\vitosmt_t.gpx reference\track\vitosmt_t.gpx

REM 
REM tracks filter tests
REM 

DEL %TMPDIR%\trackfilter*

@echo on
@echo Testing...
%PNAME% -t -i gpx -f reference\track\trackfilter.gpx -x track,pack,split,title=LOG-%%Y%%m%%d -o gpx -F %TMPDIR%\trackfilter.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\trackfilter.gpx reference\track\trackfilter.gpx

REM 
REM Map&Guide Motorrad Routenplaner .bcr files test
REM 
DEL %TMPDIR%\bcr*
@echo on
@echo Testing...
%PNAME% -r -i bcr -f reference\route\bcr-sample.bcr -o gpx -F %TMPDIR%\bcr-sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\bcr-sample.gpx reference\route\bcr-sample.gpx 
@echo on
@echo Testing...
%PNAME% -r -i gpx -f reference\route\bcr-sample.gpx -o bcr -F %TMPDIR%\bcr-sample2.bcr
@echo off
@echo.
CALL :COMPARE reference\route\bcr-sample2.bcr %TMPDIR%\bcr-sample2.bcr
@echo on
@echo Testing...
%PNAME% -r -i bcr -f %TMPDIR%\bcr-sample2.bcr -o gpx -F %TMPDIR%\bcr-sample2.gpx
@echo off
@echo.

REM 
REM cet - Character encoding transformation tests
REM 
DEL %TMPDIR%\cet-sample*
@echo on
@echo Testing...
%PNAME% -w -i gdb -f reference\cet\cet-sample.gdb -o gpx -F %TMPDIR%\cet-sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cet-sample.gpx reference\cet\cet-sample.gpx
@echo on
@echo Testing...
%PNAME% -w -i gpx -f %TMPDIR%\cet-sample.gpx -o tmpro -c Latin1 -F %TMPDIR%\cet-sample.latin1.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cet-sample.latin1.txt reference\cet\cet-sample.latin1.txt
@echo on
@echo Testing...
%PNAME% -w -i gdb -f reference\cet\cet-sample.gdb -o tmpro -c Latin2 -F %TMPDIR%\cet-sample.latin2.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cet-sample.latin2.txt reference\cet\cet-sample.latin2.txt
@echo on
@echo Testing...
%PNAME% -w -i gdb -f reference\cet\cet-sample.gdb -o tmpro -c cp1250 -F %TMPDIR%\cet-sample.cp1250.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cet-sample.cp1250.txt reference\cet\cet-sample.cp1250.txt
@echo on
@echo Testing...
%PNAME% -w -i gdb -f reference\cet\cet-sample.gdb -o tmpro -c macroman -F %TMPDIR%\cet-sample.macroman.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cet-sample.macroman.txt reference\cet\cet-sample.macroman.txt

REM 
REM Garmin logbook.   This format has an extra section (lap data with things
REM like heartbeat and calories burned) that we don't know what to do with,
REM so we convert it to gpx, convert it to itself, convert THAT to gpx, and
REM compare those.
REM 
DEL %TMPDIR%\glogbook*
@echo on
@echo Testing...
%PNAME% -i glogbook -f reference\track\garmin_logbook.xml -o gpx -F %TMPDIR%\glog1.gpx
%PNAME% -i glogbook -f reference\track\garmin_logbook.xml -o glogbook -F %TMPDIR%\glog.xml
%PNAME% -i glogbook -f %TMPDIR%\glog.xml -o gpx -F %TMPDIR%\glog2.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\glog1.gpx %TMPDIR%\glog2.gpx

REM 
REM Dop filter test
REM 
DEL %TMPDIR%\dop*
FINDSTR /V /R /C:"<hdop>50" reference\dop-test.gpx | %PNAME% -i gpx -f - -o openoffice -F - | sed 's\RPT...\\g'> %TMPDIR%\dop-hdop.ref
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\dop-test.gpx -x discard,hdop=50 -o openoffice -F - | sed 's\RPT...\\g'> %TMPDIR%\dop-hdop.fil
@echo off
@echo.
CALL :COMPARE %TMPDIR%\dop-hdop.ref %TMPDIR%\dop-hdop.fil
FINDSTR /V /R /C:"<vdop>50" reference\dop-test.gpx | %PNAME% -i gpx -f - -o openoffice -F - | sed 's\RPT...\\g'> %TMPDIR%\dop-vdop.ref
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\dop-test.gpx -x discard,vdop=50 -o openoffice -F - | sed 's\RPT...\\g'> %TMPDIR%\dop-vdop.fil
@echo off
@echo.
CALL :COMPARE %TMPDIR%\dop-vdop.ref %TMPDIR%\dop-vdop.fil

REM 
REM cotoGPS tests
REM 
DEL %TMPDIR%\coto*
REM Track reading
@echo on
@echo Testing...
%PNAME% -i coto -f reference\cototesttrack.pdb -o xcsv,style=reference\cototest.style -F %TMPDIR%\cototrack.csv
@echo off
@echo.
CALL :COMPARE reference\cototesttrack.csv %TMPDIR%\cototrack.csv
REM Marker read
@echo on
@echo Testing...
%PNAME% -i coto -f reference\cototestmarker.pdb -o gpx -F %TMPDIR%\cotomarker.gpx
@echo off
@echo.
CALL :COMPARE reference\cototestmarker.gpx %TMPDIR%\cotomarker.gpx
REM Marker write
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\cototestmarker.gpx -o coto -F %TMPDIR%\cotomarker.pdb
@echo off
@echo.
REM bincompare reference/cototestmarker.pdb ${TMPDIR}/cotomarker.pdb
@echo on
@echo Testing...
%PNAME% -i coto -f %TMPDIR%\cotomarker.pdb -o gpx -F %TMPDIR%\cotomarker.gpx
@echo off
@echo.
CALL :COMPARE reference\cototestmarker.gpx %TMPDIR%\cotomarker.gpx

REM 
REM Fugawi test cases
DEL %TMPDIR%\fugawi*
@echo on
@echo Testing...
%PNAME%  -i fugawi -f reference\fugawi.notime.txt -o fugawi -F %TMPDIR%\fugawi1.txt
@echo off
@echo.
CALL :COMPARE reference\fugawi.ref.txt %TMPDIR%\fugawi1.txt
@echo on
@echo Testing...
%PNAME%  -i geo -f geocaching.loc -o fugawi -F %TMPDIR%\fugawi2.txt
@echo off
@echo.
CALL :COMPARE reference\fugawi.ref.txt %TMPDIR%\fugawi2.txt
@echo on
@echo Testing...
%PNAME%  -i fugawi -f %TMPDIR%\fugawi2.txt -o fugawi -F %TMPDIR%\fugawi3.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\fugawi2.txt %TMPDIR%\fugawi3.txt
@echo on
@echo Testing...
%PNAME%  -i fugawi -f reference\fugawi.time.txt  -o fugawi -F %TMPDIR%\fugawi4.txt
@echo off
@echo.
CALL :COMPARE reference\fugawi.time.ref.txt %TMPDIR%\fugawi4.txt
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\track\tracks.gpx  -o fugawi -F %TMPDIR%\fugawi5.txt
@echo off
@echo.
CALL :COMPARE reference\track\fugawi.txt %TMPDIR%\fugawi5.txt

REM 
REM Magellan Explorist geocaching format (write-only).
REM 
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\gc\GC7FA4.gpx -f reference\gc\GCGCA8.gpx -o maggeo -F %TMPDIR%\maggeo.gs
@echo off
@echo.
CALL :COMPARE reference\gc\maggeo.gs %TMPDIR%\maggeo.gs

REM 
REM IGN Rando tests
REM 
@echo on
@echo Testing...
%PNAME% -i ignrando -f reference\track\ignrando-sample.rdn -o ignrando -F %TMPDIR%\ignrando-sample.rdn
%PNAME% -i ignrando -f %TMPDIR%\ignrando-sample.rdn -o gpx -F %TMPDIR%\ignrando-sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\ignrando-sample.gpx reference\track\ignrando-sample.gpx

REM 
REM STMwpp "Suunto Trek Manager" WaypointPlus format tests
REM 
DEL %TMPDIR%\stmwpp-*
@echo on
@echo Testing...
%PNAME% -i stmwpp -f reference\track\stmwpp-track.txt -o gpx -F %TMPDIR%\stmwpp-track.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\stmwpp-track.gpx reference\track\stmwpp-track.gpx
@echo on
@echo Testing...
%PNAME% -i stmwpp -f reference\route\stmwpp-route.txt -o gpx -F %TMPDIR%\stmwpp-route.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\stmwpp-route.gpx reference\route\stmwpp-route.gpx
@echo on
@echo Testing...
%PNAME% -i stmwpp -f reference\route\stmwpp-route.txt -o stmwpp -F %TMPDIR%\stmwpp-route.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\stmwpp-route.txt reference\route\stmwpp-route.txt

REM 
REM Microsoft AutoRoute 2002 test (read-only)
REM 
@echo on
@echo Testing...
%PNAME% -i msroute -f reference\route\msroute-sample.axe -o gpx -F %TMPDIR%\msroute-sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\msroute-sample.gpx reference\route\msroute-sample.gpx

REM 
REM CarteSurTable read test
REM 
DEL %TMPDIR%\cst-*
@echo on
@echo Testing...
%PNAME% -i cst -f reference\route\cst-sample.cst -o gpx -F %TMPDIR%\cst-sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\cst-sample.gpx reference\route\cst-sample.gpx

REM 
REM Navigon Mobile Navigator .rte tests
REM 
DEL %TMPDIR%\nmn4-sample*
@echo on
@echo Testing...
%PNAME% -i nmn4 -f reference\route\nmn4-sample.rte -o gpx -F %TMPDIR%\nmn4-sample.gpx
@echo off
@echo.
CALL :COMPARE reference\route\nmn4-sample.gpx %TMPDIR%\nmn4-sample.gpx
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\route\nmn4-sample.gpx -o nmn4 -F %TMPDIR%\nmn4-sample-out.rte
@echo off
@echo.
CALL :COMPARE reference\route\nmn4-sample-out.rte %TMPDIR%\nmn4-sample-out.rte

REM 
REM Map&Guide Palm/OS .pdb files (read-only)
REM 
DEL %TMPDIR%\mag_pdb-*
@echo on
@echo Testing...
%PNAME% -i mag_pdb -f reference\route\mag_pdb-sample.pdb -o gpx -F %TMPDIR%\mag_pdb-sample.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\mag_pdb-sample.gpx reference\route\mag_pdb-sample.gpx

REM 
REM CompeGPS I/O tests
REM 
DEL %TMPDIR%\compegps*
REM read (CompeGPS)
@echo on
@echo Testing...
%PNAME% -i compegps -f reference\compegps.wpt -o gpx -F %TMPDIR%\compegps-wpt.gpx
@echo off
@echo.
CALL :COMPARE reference\compegps-wpt.gpx %TMPDIR%\compegps-wpt.gpx
@echo on
@echo Testing...
%PNAME% -i compegps -f reference\route\compegps.rte -o gpx -F %TMPDIR%\compegps-rte.gpx
@echo off
@echo.
CALL :COMPARE reference\route\compegps-rte.gpx %TMPDIR%\compegps-rte.gpx
@echo on
@echo Testing...
%PNAME% -i compegps -f reference\track\compegps.trk -o gpx -F %TMPDIR%\compegps-trk.gpx
@echo off
@echo.
CALL :COMPARE reference\track\compegps-trk.gpx %TMPDIR%\compegps-trk.gpx
REM write (CompeGPS)
@echo on
@echo Testing...
%PNAME% -i compegps -f reference\compegps.wpt -o compegps -F %TMPDIR%\compegps.wpt
%PNAME% -i compegps -f %TMPDIR%\compegps.wpt -o gpx -F %TMPDIR%\compegps-wpt2.gpx
@echo off
@echo.
CALL :COMPARE reference\compegps-wpt.gpx %TMPDIR%\compegps-wpt2.gpx
@echo on
@echo Testing...
%PNAME% -t -i compegps -f reference\track\compegps.trk -o compegps -F %TMPDIR%\compegps.trk
%PNAME% -i compegps -f %TMPDIR%\compegps.trk -o gpx -F %TMPDIR%\compegps-trk2.gpx
@echo off
@echo.
CALL :COMPARE reference\track\compegps-trk.gpx %TMPDIR%\compegps-trk2.gpx
@echo on
@echo Testing...
%PNAME% -r -i compegps -f reference\route\compegps.rte -o compegps -F %TMPDIR%\compegps.rte
%PNAME% -i compegps -f %TMPDIR%\compegps.rte -o gpx -F %TMPDIR%\compegps-rte2.gpx
@echo off
@echo.
CALL :COMPARE reference\route\compegps-rte.gpx %TMPDIR%\compegps-rte2.gpx

REM 
REM Testing the 'nuketypes' filter is funky.
REM Convert a GPX file to GPX to eliminate jitter.
REM Then nuke the all but the three individual types, merge the result together
REM and verify we got the original back.
REM 
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\gdb-sample.gpx -o gpx -F %TMPDIR%\alltypes.gpx
%PNAME% -i gpx -f %TMPDIR%\alltypes.gpx -x nuketypes,tracks,routes -o gpx -F %TMPDIR%\wpts.gpx
%PNAME% -i gpx -f %TMPDIR%\alltypes.gpx -x nuketypes,waypoints,routes -o gpx -F %TMPDIR%\trks.gpx
%PNAME% -i gpx -f %TMPDIR%\alltypes.gpx -x nuketypes,waypoints,tracks -o gpx -F %TMPDIR%\rtes.gpx
%PNAME% -i gpx -f %TMPDIR%\wpts.gpx -f %TMPDIR%\trks.gpx -f %TMPDIR%\rtes.gpx -o gpx -F %TMPDIR%\merged.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\alltypes.gpx %TMPDIR%\merged.gpx

REM 
REM Interpolate filter
REM 

@echo on
@echo Testing...
%PNAME% -i gpx -f reference\track\simpletrack.gpx -x interpolate,distance=50m -o gpx -F %TMPDIR%\interp.gpx
@echo off
@echo.
CALL :COMPARE reference\track\interptrack.gpx %TMPDIR%\interp.gpx 
@echo on
@echo Testing...
%PNAME% -i gpx -f reference\track\simpletrack.gpx -x interpolate,time=1 -o gpx -F %TMPDIR%\tinterp.gpx
@echo off
@echo.
CALL :COMPARE reference\track\tinterptrack.gpx %TMPDIR%\tinterp.gpx 

REM 
REM Universal CSV - unicsv
REM 
ECHO lat,lon,descr,name,notes,unk,unk> %TMPDIR%\unicsv.txt
TYPE reference\mxf.mxf>> %TMPDIR%\unicsv.txt
@echo on
@echo Testing...
%PNAME% -i unicsv -f %TMPDIR%\unicsv.txt -o gpx -F %TMPDIR%\unicsv.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\unicsv.gpx reference\unicsv.gpx

REM 
REM Basic NMEA tests
REM 
@echo on
@echo Testing...
%PNAME% -i nmea -f reference\track\nmea -o gpx -F %TMPDIR%\nmea.gpx
@echo off
@echo.
CALL :COMPARE %TMPDIR%\nmea.gpx reference\track\nmea.gpx

REM 
REM Wfff.
REM 
@echo on
@echo Testing...
%PNAME% -i wfff -f reference\wfff.xml -o gpsutil -F %TMPDIR%\wfff.gpu
@echo off
@echo.
CALL :COMPARE %TMPDIR%\wfff.gpu reference\wfff.gpu

REM 
REM Garmin MapSource tab delimited text files - garmin_txt
REM 
DEL %TMPDIR%\garmin_txt*
REM 
REM !!! garmin_txt timestamps are stored in localtime !!!
REM 
@echo on
@echo Testing...
%PNAME% -i gdb -f reference\gdb-sample2.gdb -o garmin_txt,utc,prec=9 -F %TMPDIR%\garmin_txt.txt
@echo off
@echo.
CALL :COMPARE reference\garmin_txt.txt %TMPDIR%\garmin_txt.txt 
@echo on
@echo Testing...
%PNAME% -i garmin_txt -f reference\garmin_txt.txt -o garmin_txt,prec=9 -F %TMPDIR%\garmin_txt-2.txt
%PNAME% -i garmin_txt -f %TMPDIR%\garmin_txt-2.txt -o garmin_txt,prec=9 -F %TMPDIR%\garmin_txt-3.txt
@echo off
@echo.
CALL :COMPARE %TMPDIR%\garmin_txt-2.txt %TMPDIR%\garmin_txt-3.txt

