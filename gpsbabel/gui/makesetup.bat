rem $Id: makesetup.bat,v 1.2 2010-06-27 21:13:07 robertl Exp $
rem 
rem Copy the Qt stuff into a local directory. The Inno Setup compiler
rem cannot handle %QTDIR environment variable in the source file
rem specification

echo off
rd /q /s qtdir
mkdir qtdir
mkdir qtdir\bin
mkdir qtdir\translations
mkdir qtdir\plugins
mkdir qtdir\plugins\imageformats
mkdir qtdir\plugins\platforms
mkdir qtdir\mingw

rem Basic Qt runtime DLLs
rem  if "%QTDIR%"=="" call \QtSDK\Desktop\Qt\4.7.4\mingw\bin\qtenv2.bat
rem  if "%QTDIR%"=="" call \Qt\Qt5.2.1\5.2.1\mingw48_32\bin\qtenv2.bat
if "%QTDIR%"=="" set QTDIR=\Qt\Qt5.2.1\5.2.1\mingw48_32
copy %QTDIR%\bin\Qt5Core.dll qtdir\bin
copy %QTDIR%\bin\Qt5Gui.dll qtdir\bin
copy %QTDIR%\bin\Qt5Webkit.dll qtdir\bin
copy %QTDIR%\bin\Qt5Xml.dll qtdir\bin
copy %QTDIR%\bin\Qt5Network.dll qtdir\bin
rem copy %QTDIR%\bin\mingwm10.dll qtdir\bin
Rem  Because *obviously* libraries should live in the bin directory...
copy %QTDIR%\bin\libwinpthread*.dll qtdir\bin
copy %QTDIR%\bin\icu*.dll qtdir\bin
copy %QTDIR%\bin\libstdc*.dll qtdir\bin
copy %QTDIR%\bin\libgcc_s_dw2-1.dll qtdir\bin

rem Image format plugins needed at runtime, but not debug verions
xcopy %QTDIR%\plugins\imageformats qtdir\plugins\imageformats
xcopy %QTDIR%\plugins\platforms qtdir\plugins\platforms
rem del qtdir\plugins\imageformats\*d4*.dll
rem del qtdir\plugins\imageformats\lib*d4*.a

rem Mingw runtime support
copy %QTDIR%\..\mingw\bin\mingwm10.dll qtdir\mingw

rem Copy QT's own translations (Apply/OK, and the like)
copy %QTDIR%\translations\qt_*.qm qtdir\translations
del qtdir\translations\qt_help*

rem Generate the compiled translations.  All of this makes sense only if
rem the you're doing releases strictly 
rem  copy %QTDIR%\translations\qt_*.ts qtdir\translations
rem lrelease gpsbabel_de.ts
rem lrelease gpsbabel_es.ts
rem lrelease gpsbabel_fr.ts
rem lrelease gpsbabel_hu.ts
rem lrelease gpsbabel_it.ts
rem lrelease gpsbabelfe_de.ts
rem lrelease gpsbabelfe_es.ts
rem lrelease gpsbabelfe_fr.ts
rem lrelease gpsbabelfe_hu.ts
rem lrelease gpsbabelfe_it.ts
rem lrelease gpsbabelfe_ru.ts
rem for /f %%a in (dir /b *.ts) do lrelease %%a

"c:\Program Files\Inno Setup 5\ISCC.exe" setup.iss

rem cleanup
rd /q /s qtdir
rem del gpsbabel_*.qm
rem del gpsbabelfe_*.qm
