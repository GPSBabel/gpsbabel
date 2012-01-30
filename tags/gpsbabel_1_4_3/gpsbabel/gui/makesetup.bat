rem $Id: makesetup.bat,v 1.2 2010-06-27 21:13:07 robertl Exp $
rem 
rem Copy the Qt stuff into a local directory. The Inno Setup compiler
rem cannot handle %QTDIR environment variable in the source file
rem specification

rd /q /s qtdir
mkdir qtdir
mkdir qtdir\bin
mkdir qtdir\translations
mkdir qtdir\plugins
mkdir qtdir\plugins\imageformats
mkdir qtdir\mingw

rem Basic Qt runtime DLLs
if "%QTDIR%"=="" call \QtSDK\Desktop\Qt\4.7.4\mingw\bin\qtenv2.bat
copy %QTDIR%\bin\QtCore4.dll qtdir\bin
copy %QTDIR%\bin\QtGui4.dll qtdir\bin
copy %QTDIR%\bin\QtWebkit4.dll qtdir\bin
copy %QTDIR%\bin\QtXml4.dll qtdir\bin
copy %QTDIR%\bin\QtNetwork4.dll qtdir\bin
copy %QTDIR%\bin\mingwm10.dll qtdir\bin
copy %QTDIR%\bin\libgcc_s_dw2-1.dll qtdir\bin
copy %QTDIR%\bin\phonon4.dll qtdir\bin

rem Image format plugins needed at runtime, but not debug verions
xcopy %QTDIR%\plugins\imageformats qtdir\plugins\imageformats
del qtdir\plugins\imageformats\*d4*.dll
del qtdir\plugins\imageformats\lib*d4*.a

rem Mingw runtime support
copy %QTDIR%\..\mingw\bin\mingwm10.dll qtdir\mingw

rem Copy QT's own translations (Apply/OK, and the like)
copy %QTDIR%\translations\qt_*.ts qtdir\translations
copy %QTDIR%\translations\qt_*.qm qtdir\translations
del qtdir\translations\qt_help*

rem Generate the compiled translations
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

copy ..\msvc\Expat\libexpat.dll release

"c:\Program Files\Inno Setup 5\ISCC.exe" setup.iss

rem cleanup
rd /q /s qtdir
rem del gpsbabel_*.qm
rem del gpsbabelfe_*.qm
