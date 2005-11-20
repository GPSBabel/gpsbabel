unit gnugettextDx;

interface

uses
  Classes, TypInfo,
{$IFDEF VER120}
  gnugettextD4;
{$ELSE}
{$IFDEF VER130}
  gnugettextD5;
{$ELSE}
  gnugettext;
{$ENDIF}
{$ENDIF}

function _(const szMsgId: string): string;
function gettext(const szMsgId: string): string;
function dgettext(const szDomain: string; const szMsgId: string): string;
procedure TranslateComponent(AnObject: TComponent);

{$IFDEF VER120}
function GetStrProp(Instance: TObject; const Name: string): string; overload;
function GetStrProp(Instance: TObject; Info: PPropInfo): string; overload;
procedure SetStrProp(Instance: TObject; const Name, Value: string); overload;
procedure SetStrProp(Instance: TObject; Info: PPropInfo; const Value: string); overload;
{$ENDIF}

implementation

function _(const szMsgId: string): string;
begin
{$IFDEF VER120}
  Result := gnugettextD4._(szMsgId);
{$ELSE}
{$IFDEF VER130}
  Result := gnugettextD5._(szMsgId);
{$ELSE}
  Result := gnugettext._(szMsgId);
{$ENDIF}{$ENDIF}
end;

function gettext(const szMsgId: string): string;
begin
{$IFDEF VER120}
  Result := gnugettextD4.gettext(szMsgId);
{$ELSE}
{$IFDEF VER130}
  Result := gnugettextD5.gettext(szMsgId);
{$ELSE}
  Result := gnugettext.gettext(szMsgId);
{$ENDIF}{$ENDIF}
end;

function dgettext(const szDomain: string; const szMsgId: string): string;
begin
{$IFDEF VER120}
  Result := gnugettextD4.dgettext(szDomain, szMsgId);
{$ELSE}
{$IFDEF VER130}
  Result := gnugettextD5.dgettext(szDomain, szMsgId);
{$ELSE}
  Result := gnugettext.dgettext(szDomain, szMsgId);
{$ENDIF}{$ENDIF}
end;

procedure TranslateComponent(AnObject: TComponent);
begin
{$IFDEF VER120}
  gnugettextD4.TranslateComponent(AnObject);
{$ELSE}
{$IFDEF VER130}
  gnugettextD5.TranslateComponent(AnObject);
{$ELSE}
  gnugettext.TranslateComponent(AnObject);
{$ENDIF}{$ENDIF}
end;

{$IFDEF VER120}
function GetStrProp(Instance: TObject; const Name: string): string;
begin
  Result := gnugettextD4.GetStrProp(Instance, Name);
end;

function GetStrProp(Instance: TObject; Info: PPropInfo): string;
begin
  Result := gnugettextD4.GetStrProp(Instance, Info);
end;

procedure SetStrProp(Instance: TObject; const Name, Value: string);
begin
  gnugettextD4.SetStrProp(Instance, Name, Value);
end;

procedure SetStrProp(Instance: TObject; Info: PPropInfo; const Value: string);
begin
  gnugettextD4.SetStrProp(Instance, Info, Value);
end;

{$ENDIF}

end.
