//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------
unit uLangBase;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
//{$modeswitch class}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Forms,
  TypInfo,
  LazFileUtils,
  IDEExternToolIntf,
  LazIDEIntf,
  uSupports,
  res2goResources;

type

  { TLangBase }

  TFnParam = record
    Name: string;
    &Type: string;
    Flags: TParamFlags;
  end;

  TFnParams = array of TFnParam;

  TVaildForms = array of string;

  { TProjParam }

  TProjParam = record
    Title: string;
    UseScaled: Boolean;
    UseDefaultWinAppRes: Boolean;
  public
    constructor Create(ATitle: string; AUseScaled, AUseDefaultWinAppRes: Boolean);
  end;

  TLangBase = class
  private
    FPackageName: string;
    function GetProjectLPRFileName: string;
  protected
    FTypeLists: TTypeLists;
    FBaseTypes: TTypeLists;

    procedure InitTypeLists; virtual; abstract;
    procedure InitBaseTypes; virtual; abstract;
    function GetParams(AProp: PPropInfo): TFnParams;
    function FirstCaseChar(Astr: string): string;
    function GetVaildForms: TVaildForms;

    function IsMainPackage: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property PackageName: string read FPackageName write FPackageName;
    property ProjectLPRFileName: string read GetProjectLPRFileName;

    procedure ConvertProjectFile(const AOutPath: string; AParam: TProjParam); virtual; abstract;
    function ToEventString(AProp: PPropInfo): string; virtual; abstract;
    procedure SaveToFile(AFileName: string; ARoot: TComponent; AEvents: array of TEventItem; AMem: TMemoryStream); virtual; abstract;
  end;

implementation

{ TProjParam }

constructor TProjParam.Create(ATitle: string; AUseScaled,
  AUseDefaultWinAppRes: Boolean);
begin
  Self.Title:=ATitle;
  Self.UseScaled:=AUseScaled;
  Self.UseDefaultWinAppRes := AUseDefaultWinAppRes;
end;

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

function TLangBase.GetProjectLPRFileName: string;
begin
  Result := ChangeFileExt(LazarusIDE.ActiveProject.ProjectInfoFile, '.lpr');
end;

function TLangBase.GetParams(AProp: PPropInfo): TFnParams;
var
  LTypeData: PTypeData;
  I: Integer;

  LFlags: TParamFlags = [];
  LParamName: string = '';
  LParamType: string = '';
  LMem: TMemoryStream;
  LLen: Byte = 0;
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

function TLangBase.GetVaildForms: TVaildForms;
var
  LLPRFileName, S: string;
  LStrs: TStringList;
  LP: Integer;
begin
  Result := nil;
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    LLPRFileName := ProjectLPRFileName;
    if FileExists(LLPRFileName) then
    begin
      LStrs := TStringList.Create;
      try
        LStrs.LoadFromFile(LLPRFileName);
        for S in LStrs do
        begin
          // 开始提取 Application.CreateForm的
          if S.Trim.StartsWith('Application.CreateForm(') then
          begin
            SetLength(Result, Length(Result) + 1);
            LP := S.IndexOf(',');
            Result[High(Result)] := Trim(S.Substring(LP + 1, S.IndexOf(')') - LP - 1));
          end;
        end;
      finally
        LStrs.Free;
      end;
    end;
  end;
end;

function TLangBase.IsMainPackage: Boolean;
begin
  Result := PackageName.Equals('main') or PackageName.IsEmpty
end;




end.

