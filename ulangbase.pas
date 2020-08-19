unit uLangBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, TypInfo, uSupports, IDEExternToolIntf, res2goResources;

type

  { TLangBase }

  TFnParam = record
    Name: string;
    &Type: string;
    Flags: TParamFlags;
  end;

  TFnParams = array of TFnParam;

  TLangBase = class
  private
    FPackageName: string;
  protected
    FTypeLists: TTypeLists;
    FBaseTypes: TTypeLists;

    procedure InitTypeLists; virtual; abstract;
    procedure InitBaseTypes; virtual; abstract;
    function GetParams(AProp: PPropInfo): TFnParams;
    function FirstCaseChar(Astr: string): string;

    function IsMainPackage: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property PackageName: string read FPackageName write FPackageName;

    procedure ConvertProjectFile(const AFileName, AOutPath, ATitle: string; AUseScaled: Boolean); virtual; abstract;
    function ToEventString(AProp: PPropInfo): string; virtual; abstract;
    procedure SaveToFile(AFileName: string; ARoot: TComponent; AEvents: array of TEventItem; AMem: TMemoryStream); virtual; abstract;
  end;

implementation

{ TLangBase }


constructor TLangBase.Create;
begin
  inherited Create;
  FTypeLists := TTypeLists.Create;
  FBaseTypes := TTypeLists.Create;

  InitTypeLists;
  InitBaseTypes;
end;

destructor TLangBase.Destroy;
begin
  FTypeLists.Free;
  FBaseTypes.Free;
  inherited Destroy;
end;


function TLangBase.GetParams(AProp: PPropInfo): TFnParams;
var
  LTypeData: PTypeData;
  I: Integer;

  LFlags: TParamFlags;
  LParamName, LParamType: string;
  LMem: TMemoryStream;
  LLen: Byte;
begin
  Result := nil;
  // 处理参数
  if AProp <> nil then
  begin
    LTypeData := GetTypeData(AProp^.PropType);
    if Assigned(LTypeData) then
    begin
      //mkClassProcedure,mkClassFunction
      LMem := TMemoryStream.Create;
      try
        LMem.Write(LTypeData^.ParamList[0], SizeOf(LTypeData^.ParamList));
        LMem.Position:=0;

        SetLength(Result, LTypeData^.ParamCount);
        for I := 0 to LTypeData^.ParamCount - 1 do
        begin
          LMem.Read(LFlags, SizeOf(TParamFlags));
          // name
          LMem.Read(LLen, 1);
          SetLength(LParamName, LLen);

          LMem.Read(LParamName[1], LLen);
          // type
          LMem.Read(LLen, 1);
          SetLength(LParamType, LLen);
          LMem.Read(LParamType[1], LLen);

          Result[I].Name := Trim(LParamName);
          Result[I].Flags := LFlags;
          Result[I].&Type := Trim(LParamType);
        end;
      finally
        LMem.Free;
      end;
    end;
  end;
end;

function TLangBase.FirstCaseChar(Astr: string): string;
begin
  Result := Astr;
  if Length(Result) > 0 then
    Result[1] := LowerCase(Result[1]);
end;

function TLangBase.IsMainPackage: Boolean;
begin
  Result := PackageName.Equals('main') or PackageName.IsEmpty
end;




end.

