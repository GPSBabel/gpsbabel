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
copy %QTDIR%\bin\QtCore4.dll qtdir\bin
copy %QTDIR%\bin\QtGui4.dll qtdir\bin
copy %QTDIR%\bin\QtWebkit4.dll qtdir\bin
copy %QTDIR%\bin\QtXml4.dll qtdir\bin
copy %QTDIR%\bin\QtNetwork4.dll qtdir\bin

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
lrelease gpsbabel_de.ts
lrelease gpsbabel_es.ts
lrelease gpsbabel_fr.ts
lrelease gpsbabel_hu.ts
lrelease gpsbabel_it.ts
lrelease gpsbabelfe_de.ts
lrelease gpsbabelfe_es.ts
lrelease gpsbabelfe_fr.ts
lrelease gpsbabelfe_hu.ts
lrelease gpsbabelfe_it.ts
lrelease gpsbabelfe_ru.ts


"c:\Program Files\Inno Setup 5\ISCC.exe" setup.iss

rem cleanup
rd /q /s qtdir
del gpsbabel_*.qm
del gpsbabelfe_*.qm
