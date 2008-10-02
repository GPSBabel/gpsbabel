unit delphi;

// Delpi compatibility unit //

interface

uses
  SysUtils, TypInfo;

{$IFDEF VER120}
function GetPropInfo(Instance: TObject; const PropertyName: string): PPropInfo; overload;
function GetPropInfo(Instance: TObject; const Name: string; var PropInfo: TPropInfo): Boolean; overload;
function GetObjectProp(Instance: TObject; Info: PPropInfo): TObject;
function GetStrProp(Instance: TObject; const Name: string): string; overload;
function GetStrProp(Instance: TObject; Info: PPropInfo): string; overload;
procedure SetStrProp(Instance: TObject; const Name, Value: string); overload;
procedure SetStrProp(Instance: TObject; Info: PPropInfo; const Value: string); overload;
{$ENDIF}

implementation

{$IFDEF VER120}
function GetPropInfo(Instance: TObject; const PropertyName: string): PPropInfo;
begin
  Result := TypInfo.GetPropInfo(Instance.ClassInfo, PropertyName);
end;

function GetObjectProp(Instance: TObject; Info: PPropInfo): TObject;
begin
  Result := Pointer(TypInfo.GetOrdProp(Instance, Info));
end;

function GetPropInfo(Instance: TObject; const Name: string; var PropInfo: TPropInfo): Boolean;
var
  Props: PPropList;
  TypeData: PTypeData;
  Info: PPropInfo;
  i: Integer;
begin
  TypeData := GetTypeData(Instance.ClassInfo);
  if ((TypeData <> nil) and (TypeData.PropCount > 0)) then
  begin
    GetMem(Props, TypeData.PropCount * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, Props);
      for i := 0 to TypeData.PropCount - 1 do
      begin
        Info := Props[i];
        if (CompareText(Info.Name, Name) = 0) then
        begin
          PropInfo := Info^;
          Result := True;
          Exit;
        end
      end;
    finally
      FreeMem(Props);
    end;
  end;
  Result := False;
end;

function GetStrProp(Instance: TObject; Info: PPropInfo): string;
begin
  Result := TypInfo.GetStrProp(Instance, Info);
end;

function GetStrProp(Instance: TObject; const Name: string): string;
var
  Info: TPropInfo;
begin
  if GetPropInfo(Instance, Name, Info) then
    Result := TypInfo.GetStrProp(Instance, @Info)
  else
    Result := '';
end;

procedure SetStrProp(Instance: TObject; const Name, Value: string);
var
  Info: TPropInfo;
begin
  if GetPropInfo(Instance, Name, Info) then
    SetStrProp(Instance, @Info, Value);
end;

procedure SetStrProp(Instance: TObject; Info: PPropInfo; const Value: string);
begin
  TypInfo.SetStrProp(Instance, Info, Value);
end;

{$ENDIF}

end.
