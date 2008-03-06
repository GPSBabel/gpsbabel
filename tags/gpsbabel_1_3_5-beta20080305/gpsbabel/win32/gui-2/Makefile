#
# requires: unix utilities
#           GNU gettext
#

DCC = dcc32.exe
RC = brcc32.exe

SRC = delphi.pas gnugettext.pas gnugettextD4.pas gnugettextD5.pas \
  gnugettextDx.pas common.pas utils.pas \
  about.pas filter.pas options.pas main.pas select.pas

FRM = about.dfm filter.dfm options.dfm main.dfm select.dfm 

LANG = \
  locale\de\LC_MESSAGES\delphi.mo \
  locale\de\LC_MESSAGES\default.mo \
  locale\de\LC_MESSAGES\gpsbabel.mo \
  locale\es\LC_MESSAGES\delphi.mo \
  locale\es\LC_MESSAGES\default.mo \
  locale\es\LC_MESSAGES\gpsbabel.mo \
  locale\fr\LC_MESSAGES\delphi.mo \
  locale\fr\LC_MESSAGES\default.mo \
  locale\fr\LC_MESSAGES\gpsbabel.mo \
  locale\hu\LC_MESSAGES\delphi.mo \
  locale\hu\LC_MESSAGES\default.mo \
  locale\hu\LC_MESSAGES\gpsbabel.mo

.suffixes: .po

.po.mo:
	msgfmt -o $@ $<

.rc.res:
	$(RC) $<

.dpr.exe:
	$(DCC) $<

default: GPSBabelGUI.exe

GPSBabelGUI.exe: GPSBabelGUI.dpr GPSBabelGUI.res $(SRC) $(FRM) $(LANG)
	$(DCC) GPSBabelGUI.dpr
	assemble GPSBabelGUI.exe --dxgettext

run:	GPSBabelGUI.exe
	xcopy /Y ..\..\gpsbabel.exe
	xcopy /Y ..\..\mingw\libexpat.dll
	xcopy /Y ..\..\gpsbabel.html
	GPSBabelGUI.exe

GPSBabelGUI.res: GPSBabelGUI.dof dof2rc.exe
	dof2rc GPSBabelGUI
	$(RC) GPSBabelGUI.rc

clean:
	rm -f *.~* *.??~ 
	rm -f *.bak *.dcu *.exe *.mo *.dll *.res *.rc
	rm -f readme.html README readme.xml
	rm -f locale\de\LC_MESSAGES\*.mo
	rm -f locale\es\LC_MESSAGES\*.mo
	rm -f locale\fr\LC_MESSAGES\*.mo
	rm -f locale\hu\LC_MESSAGES\*.mo

