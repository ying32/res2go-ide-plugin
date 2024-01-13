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
  res2goResources,
  process;

type

  { TLangBase }

  TFnParam = record
    Name: string;
    &Type: string;
    Flags: TParamFlags;
  end;

  TFnParams = array of TFnParam;

  TAutoCreateForms = array of string;

  TCompileParam = record
    Input: string;
    Output: string;

    // for Golang
    GoUseTempdll: Boolean;
    GoEnabledFinalizerOn: Boolean;
    GoTags: string;
    GoEnabledCGO: Boolean;
    GoBuildMode: string;
    GoRoot: string;
    GoUseGoEmbed: Boolean;
  end;

  { TProjParam }

  TProjParam = record
    Title: string;
    UseScaled: Boolean;
    UseDefaultWinAppRes: Boolean;
    OutPath: string;
  public
    constructor Create(AOutPath, ATitle: string; AUseScaled, AUseDefaultWinAppRes: Boolean);
  end;

  TLangBase = class
  private
    FPackageName: string;

    function GetProjectLPRFileName: string;
    function GetWindResFileName: string;
  protected
    FTypeLists: TTypeLists;
    FBaseTypes: TTypeLists;

    function GetPackageName: string; virtual;
    procedure InitTypeLists; virtual; abstract;
    procedure InitBaseTypes; virtual; abstract;
    function GetParams(AProp: PPropInfo): TFnParams;
    function FirstCaseChar(Astr: string): string;
    function GetAutoCreateForms: TAutoCreateForms;
    function GetResFileExists: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AParams: TCompileParam): Boolean; virtual; abstract;

    procedure ExecuteCommand(const ACmds: array of string; AWait: Boolean; AShow: Boolean = False; AWorkDir: string = ''); overload;
    procedure ExecuteCommand(const ACmd: string; AWait: Boolean; AShow: Boolean = False; AWorkDir: string = ''); overload;
    function IsMainPackage: Boolean;

    property PackageName: string read GetPackageName write FPackageName;
    property ProjectLPRFileName: string read GetProjectLPRFileName;
    property WindResFileName: string read GetWindResFileName;
    property ResFileExists: Boolean read GetResFileExists;

    procedure ConvertProjectFile(AParam: TProjParam); virtual; abstract;
    function ToEventString(AProp: PPropInfo): string; virtual; abstract;
    procedure SaveToFile(AFileName: string; ARoot: TComponent; AEvents: array of TEventItem; AMem: TMemoryStream); virtual; abstract;
    procedure ConvertResource(const AResFileName, APath: string); virtual; abstract;
  end;

implementation

{ TProjParam }

constructor TProjParam.Create(AOutPath, ATitle: string; AUseScaled,
  AUseDefaultWinAppRes: Boolean);
begin
  Self.OutPath := AOutPath;
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

function TLangBase.GetPackageName: string;
begin
  Result := FPackageName;
end;

function TLangBase.GetResFileExists: Boolean;
begin
  Result := False;
end;

function TLangBase.GetWindResFileName: string;
begin
  Result := 'windres'{$ifdef windows}+'.exe'{$endif};
  Result :=  AppendPathDelim(ExtractFilePath(LazarusIDE.GetFPCompilerFilename)) + Result;
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

function TLangBase.GetAutoCreateForms: TAutoCreateForms;
var
  LLPRFileName, S: string;
  LStrs: TStringList;
  LP: Integer;
begin
  Result := nil;
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    LStrs := TStringList.Create;
    try
      LStrs.Text:=LazarusIDE.ActiveProject.MainFile.GetSourceText;
      for S in LStrs do
      begin
        // 开始提取 Application.CreateForm的
        if S.Trim.StartsWith('Application.CreateForm(') then
        begin
          SetLength(Result, Length(Result) + 1);
          LP := S.IndexOf(',');
          Result[High(Result)] :=  Trim(S.Substring(LP + 1, S.IndexOf(')') - LP - 1));
        end;
      end;
    finally
      LStrs.Free;
    end;

   // Logs('mainSource: '#13#10 + LazarusIDE.ActiveProject.MainFile.GetSourceText);

    //LLPRFileName := ProjectLPRFileName;
    //if FileExists(LLPRFileName) then
    //begin
    //  LStrs := TStringList.Create;
    //  try
    //    LStrs.LoadFromFile(LLPRFileName);
    //    for S in LStrs do
    //    begin
    //      // 开始提取 Application.CreateForm的
    //      if S.Trim.StartsWith('Application.CreateForm(') then
    //      begin
    //        SetLength(Result, Length(Result) + 1);
    //        LP := S.IndexOf(',');
    //        Result[High(Result)] :=  Trim(S.Substring(LP + 1, S.IndexOf(')') - LP - 1));
    //      end;
    //    end;
    //  finally
    //    LStrs.Free;
    //  end;
    //end;
  end;
end;

procedure TLangBase.ExecuteCommand(const ACmds: array of string;
  AWait, AShow: Boolean; AWorkDir: string);
var
  LProcess: TProcess;
  LCmd: string;
begin
  if Length(ACmds) = 0 then Exit;
  LProcess := TProcess.Create(nil);
  try
    LProcess.Options:= [];//[poWaitOnExit{, poNewProcessGroup, poStderrToOutPut}];
    if AShow then
      LProcess.ShowWindow := swoShowDefault
    else
      LProcess.ShowWindow := swoHIDE;
    if AWorkDir <> '' then
       LProcess.CurrentDirectory:=AWorkDir;
    for LCmd in ACmds do
    begin
      try
        LProcess.CommandLine := LCmd;
        LProcess.Execute;
        if AWait then
          LProcess.WaitOnExit;
        if LProcess.ExitCode <> 0 then
        begin
          //CtlWriteln(mluError);
          Exit;
        end;
      except
        on E: Exception do
        begin
          CtlWriteln(mluError, E.Message);
          Exit;
        end;
      end;
    end;
  finally
    LProcess.Free;
  end;
end;

procedure TLangBase.ExecuteCommand(const ACmd: string; AWait, AShow: Boolean;
  AWorkDir: string);
begin
  ExecuteCommand([ACmd], AWait, AShow, AWorkDir);
end;

function TLangBase.IsMainPackage: Boolean;
begin
  Result := PackageName.Equals('main') or PackageName.IsEmpty
end;




end.

