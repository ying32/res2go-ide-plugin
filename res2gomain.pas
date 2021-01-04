//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

{$if FPC_FULLVERSION < 30200}
   {$ERROR 'Requires FPC version>=3.2'}
{$endif}

unit res2gomain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LCLType,
  UITypes,
  StdCtrls,
  Buttons,
  Forms,
  TypInfo,
  LazIDEIntf,
  ProjectIntf,
  IDEMsgIntf,
  IDEExternToolIntf,
  IDEOptEditorIntf,
  FormEditingIntf,
  CompOptsIntf,
  usupports,
  res2goresources,
  uLangBase,
  LResources,
  LazFileUtils,
  MacroIntf;

type


  { TMyIDEIntf }

  TMyIDEIntf = class
  private const
    EXT_GFM = '.gfm';
    EXT_RES = '.res';
  private
    FEvents: array of TEventItem;

    FEnabledConvert: Boolean;
    FGoBuildMode: string;
    FGoEnabledCGO: Boolean;
    FGoEnabledFinalizerOn: Boolean;
    FGoRoot: string;
    FGoTags: string;
    FGoUseEmbed: Boolean;
    FGoUseTempdll: Boolean;
    FOutLang: TOutLang;
    FOutputPath: string;
    FPackageName: string;
    FReConvertRes: Boolean;
    FSaveGfmFile: Boolean;
    FUseDefaultWinAppRes: Boolean;
    FUseOriginalFileName: Boolean;

    FAddToLast: Boolean;
    FModalResult: TModalResult;
    FWithoutDebug: Boolean;


    function GetDefaultProjectParam: TProjParam;
    function GetLang: TLangBase;

    function GetProjectTitle: string;
    function GetRealOutputPackagePath: string;
    function GetRealOutputPath: string;
    function GetResFileName: string;
    function GetTargetDir: string;
    function GetTargetFile: string;
    function GetUseScaled: Boolean;

    procedure SaveComponents(ADesigner: TIDesigner; AUnitFileName, AOutPath: string);
    procedure OnWriteMethodProperty(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo;
      const MethodValue, DefMethodValue: TMethod; var Handled: boolean);

    procedure CheckAndCreateDir;


    function GetProjectPath: string;

    procedure SetEnabledConvert(AValue: Boolean);
    procedure SetPackageName(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    function onSaveAll(Sender: TObject): TModalResult;
    function onSaveEditorFile(Sender: TObject; aFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
    function onProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;

    function onProjectBuilding(Sender: TObject): TModalResult;
    procedure onProjectBuildingFinished(Sender: TObject; BuildSuccessful: Boolean);
    function onProjectDependenciesCompiling(Sender: TObject): TModalResult;
    function onProjectDependenciesCompiled(Sender: TObject): TModalResult;
    function onLazarusBuilding(Sender: TObject): TModalResult;
    procedure onLazarusBuildingFinished(Sender: TObject; BuildSuccessful: Boolean);
    function onRunDebug(Sender: TObject; var Handled: boolean): TModalResult;
    function onRunWithoutDebugBuilding(Sender: TObject; var Handled: boolean): TModalResult;
    function onRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
    procedure onRunFinished(Sender: TObject);
    procedure onChangeToolStatus(Sender: TObject; OldStatus, NewStatus: TLazToolStatus);



    class procedure AddHandlers;
    class procedure RemoveHandlers;

    property EnabledConvert: Boolean read FEnabledConvert write SetEnabledConvert;
    property OutputPath: string read FOutputPath write FOutputPath;
    property UseOriginalFileName: Boolean read FUseOriginalFileName write FUseOriginalFileName;
    property SaveGfmFile: Boolean read FSaveGfmFile write FSaveGfmFile;
    property OutLang: TOutLang read FOutLang write FOutLang;
    property PackageName: string read FPackageName write SetPackageName;
    property UseScaled: Boolean read GetUseScaled;
    property ProjectTitle: string read GetProjectTitle;
    property UseDefaultWinAppRes: Boolean read FUseDefaultWinAppRes write FUseDefaultWinAppRes;

    property GoUseTempdll: Boolean read FGoUseTempdll write FGoUseTempdll;
    property GoEnabledFinalizerOn: Boolean read FGoEnabledFinalizerOn write FGoEnabledFinalizerOn;
    property GoTags: string read FGoTags write FGoTags;
    property GoEnabledCGO: Boolean read FGoEnabledCGO write FGoEnabledCGO;
    property GoBuildMode: string read FGoBuildMode write FGoBuildMode;
    property GoRoot: string read FGoRoot write FGoRoot;
    property GoUseEmbed: Boolean read FGoUseEmbed write FGoUseEmbed;


    property DefaultProjectParam: TProjParam read GetDefaultProjectParam;


    property Lang: TLangBase read GetLang;

    property ProjectPath: string read GetProjectPath;
    property RealOutputPath: string read GetRealOutputPath;
    property RealOutputPackagePath: string read GetRealOutputPackagePath;

    property TargetFile: string read GetTargetFile;
    property TargetDir: string read GetTargetDir;



    property ReConvertRes: Boolean read FReConvertRes write FReConvertRes;
    property ResFileName: string read GetResFileName;
  end;

  // JITForms.pas
  TFakeJITMethod = class
  private
    FMethod: TMethod;
    FOwner: TObject;//TJITMethods;
    FTheClass: TClass;
    FTheMethodName: shortstring;
  public
    property TheMethodName: shortstring read FTheMethodName;
  end;


var
  MyIDEIntf: TMyIDEIntf;

procedure Register;

implementation

uses
  ugolang;


procedure Register;
begin
  //Logs('MyIDEIntf.Register');
end;

procedure Init;
begin
  MyIDEIntf := TMyIDEIntf.Create;
end;

procedure UnInit;
begin
  TMyIDEIntf.RemoveHandlers;
  MyIDEIntf.Free;
  MyIDEIntf := nil;
end;


{ TMyIDEIntf }

procedure TMyIDEIntf.SaveComponents(ADesigner: TIDesigner; AUnitFileName,
  AOutPath: string);
var
  LWriter: TWriter;
  LDestroyDriver: Boolean;
  LStream: TMemoryStream;
  LGfmFileName, LOutFileName: string;
begin
  if Assigned(ADesigner) and Assigned(ADesigner.LookupRoot) then
  begin
    LStream := TMemoryStream.Create;
    try
      LDestroyDriver := False;
      LWriter := nil;
      try
        // 清空事件
        SetLength(FEvents, 0);
        LWriter := CreateLRSWriter(LStream, LDestroyDriver);
        LWriter.OnWriteMethodProperty := @OnWriteMethodProperty;
        LWriter.WriteDescendent(ADesigner.LookupRoot, nil);
      finally
        if LDestroyDriver then
          LWriter.Driver.Free;
        LWriter.Free;
      end;
      // 保存go文件及impl文件
      LOutFileName := AOutPath;
      if Self.UseOriginalFileName then
        LOutFileName += AUnitFileName
      else
        LOutFileName += ADesigner.LookupRoot.Name;

      //
      if OutLang = olGo then
        TGoLang(Lang).UseGoEmbed:= GoUseEmbed;

      // 保存文件
      Lang.SaveToFile(LOutFileName, ADesigner.LookupRoot, FEvents, LStream);

      // 保存gfm文件，条件为勾选启用保存Gfm文件或者使用go:embed特性
      if Self.SaveGfmFile or ((OutLang = olGo) and GoUseEmbed) then
      begin
        LGfmFileName := AppendPathDelim(AppendPathDelim(AOutPath) + 'resources');
        if not SysUtils.DirectoryExists(LGfmFileName) then
          SysUtils.CreateDir(LGfmFileName);

        if Self.UseOriginalFileName then
          LGfmFileName += AUnitFileName
        else
          LGfmFileName += ADesigner.LookupRoot.Name;

        LGfmFileName += EXT_GFM;
        LStream.Position := 0;
        LStream.SaveToFile(LGfmFileName);
      end;
    finally
      LStream.Free;
    end;
  end;
end;

function TMyIDEIntf.GetRealOutputPath: string;
begin
  Result := OutputPath;
  if Assigned(IDEMacros) then
  begin
    IDEMacros.SubstituteMacros(Result);
    if not FileNameIsAbsolute(Result) then
      Result := ProjectPath + Result;
  end else
  begin
    if not FileNameIsAbsolute(Result) then
       Result := ProjectPath + OutputPath;
  end;
  Result := AppendPathDelim(Result);
end;

function TMyIDEIntf.GetResFileName: string;
begin
  Result := ChangeFileExt(LazarusIDE.ActiveProject.ProjectInfoFile, EXT_RES);
end;

function TMyIDEIntf.GetTargetDir: string;
begin
  Result := RealOutputPath;
  if (Result <> '') and (Result[Length(Result)] in AllowDirectorySeparators) then
    Result := Copy(Result, 1, Length(Result) - 1);
end;

function TMyIDEIntf.GetTargetFile: string;
begin
  Result := '$TargetFile()';
  if not IDEMacros.SubstituteMacros(Result) then
    Result := LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename + LazarusIDE.ActiveProject.LazCompilerOptions.TargetFileExt;
end;

function TMyIDEIntf.GetUseScaled: Boolean;
begin
  Result := True;
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
    Result := LazarusIDE.ActiveProject.Scaled;
end;

function TMyIDEIntf.GetLang: TLangBase;
begin
  Result := GoLang;
  case Self.FOutLang of
     olRust: ;
     olNim: ;
  end;
end;

function TMyIDEIntf.GetProjectTitle: string;
begin
  Result := '';
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
    Result := LazarusIDE.ActiveProject.Title;
end;

function TMyIDEIntf.GetDefaultProjectParam: TProjParam;
begin
  Result := TProjParam.Create(Self.RealOutputPath, Self.ProjectTitle, Self.UseScaled, Self.UseDefaultWinAppRes);
end;

function TMyIDEIntf.GetRealOutputPackagePath: string;
begin
  Result := Self.RealOutputPath;
  if not Lang.IsMainPackage then
    Result := AppendPathDelim(Result + PackageName);
end;

// FakeIsJITMethod
function FakeIsJITMethod(const aMethod: TMethod): boolean;
begin
  Result:= (aMethod.Data <> nil) and (aMethod.Code = nil) and (TObject(aMethod.Data).ClassType.ClassNameIs('TJITMethod'));
end;

procedure TMyIDEIntf.OnWriteMethodProperty(Writer: TWriter;
  Instance: TPersistent; PropInfo: PPropInfo; const MethodValue,
  DefMethodValue: TMethod; var Handled: boolean);

  function GetTypeName: string;
  begin
    Result := PropInfo^.Name;
    if Result.StartsWith('On') then
      Result := Copy(Result, 3, Length(Result) - 2);
  end;

var
  LMethodName, LComponentName: string;
  LEvent: TEventItem;
begin
  Handled := False;
  if (DefMethodValue.Data = MethodValue.Data) and (DefMethodValue.Code = MethodValue.Code) then
    Exit;

  LMethodName := '';
  LComponentName := '';
  if Instance is TComponent then
     LComponentName := TComponent(Instance).Name;

  if FakeIsJITMethod(MethodValue) then
    LMethodName := TFakeJITMethod(MethodValue.Data).TheMethodName
  else if MethodValue.Code <> nil then
  begin
    LMethodName := Writer.LookupRoot.MethodName(MethodValue.Code);
    if LMethodName = '' then
      Exit;
  end;
  if LMethodName = '' then
    Exit;

  LEvent.InstanceName := LComponentName;
  LEvent.EventName := LMethodName;
  LEvent.EventTypeName := GetTypeName;
  LEvent.EventParams := Lang.ToEventString(PropInfo);

  SetLength(FEvents, Length(FEvents) + 1);
  FEvents[High(FEvents)] := LEvent;

  //if Assigned(PropInfo) then
  //  Logs('LComponentName%s, LMethodName=%s, Event.Name=%s', [LComponentName, LMethodName, PropInfo^.Name]);
end;

procedure TMyIDEIntf.CheckAndCreateDir;
var
  LOutPath: string;
begin
  LOutPath := Self.RealOutputPath;
  if not SysUtils.DirectoryExists(LOutPath) then
    SysUtils.CreateDir(LOutPath);
  if not Lang.IsMainPackage then
  begin
    LOutPath := AppendPathDelim(LOutPath) + PackageName;
    if not SysUtils.DirectoryExists(LOutPath) then
      SysUtils.CreateDir(LOutPath);
  end;
end;

function TMyIDEIntf.GetProjectPath: string;
begin
  Result := '';
  if Assigned(LazarusIDE.ActiveProject) then
    Result := AppendPathDelim(LazarusIDE.ActiveProject.Directory);
   // Result := ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

procedure TMyIDEIntf.SetEnabledConvert(AValue: Boolean);
begin
  if FEnabledConvert=AValue then Exit;
  FEnabledConvert:=AValue;
  if FEnabledConvert then
    Self.AddHandlers
  else
    Self.RemoveHandlers;
end;

procedure TMyIDEIntf.SetPackageName(AValue: string);
begin
  if FPackageName=AValue then Exit;
  FPackageName:=AValue;
  Lang.PackageName:=FPackageName;
end;

constructor TMyIDEIntf.Create;
begin
  inherited Create;
  FAddToLast := False;   // 测试时用，方便修改，最终要改为 False
  FModalResult := mrAbort;//mrOK; // 测试时用，方便修改，最终要改为 mrAbort
end;

destructor TMyIDEIntf.Destroy;
begin
  inherited Destroy;
end;

function TMyIDEIntf.onSaveAll(Sender: TObject): TModalResult;
begin
  if EnabledConvert then
  begin
    ClearMsg;
    CheckAndCreateDir;
    Lang.ConvertProjectFile(DefaultProjectParam);
  {$ifdef windows}
    if ReConvertRes and (not UseDefaultWinAppRes) then
    begin
      ReConvertRes := False;
      Lang.ConvertResource(ResFileName, RealOutputPath);
    end;
  {$else}
    // 暂时不持非Windows下转换资源
  {$endif}
  end;
  Result := mrOk;
end;

function TMyIDEIntf.onSaveEditorFile(Sender: TObject; aFile: TLazProjectFile;
  SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
var
  LDesigner: TIDesigner;
begin
  if SaveStep = sefsAfterWrite then
  begin
    if EnabledConvert then
    begin
      ClearMsg;
      CheckAndCreateDir;
      LDesigner := LazarusIDE.GetDesignerWithProjectFile(aFile, False);
      if Assigned(LDesigner) and Assigned(LDesigner.LookupRoot) then
        SaveComponents(LDesigner, GetFileNameWithoutExt(TargetFilename), RealOutputPackagePath);
    end;
  end;
  Result := mrOk;
end;

function TMyIDEIntf.onProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  //Logs('ExtensionToLazSyntaxHighlighter=%d', [Integer(IDEEditorOptions.ExtensionToLazSyntaxHighlighter('.go'))]);
  //Logs('ExtensionToLazSyntaxHighlighter=%d', [Integer(IDEEditorOptions.ExtensionToLazSyntaxHighlighter('go'))]);
  //Logs('GetCompilerFilename=%s', [LazarusIDE.GetCompilerFilename]);
  //Logs('GetFPCompilerFilename=%s', [LazarusIDE.GetFPCompilerFilename]);
  //Logs('ActiveProject.Directory=%s', [LazarusIDE.ActiveProject.Directory]);
  //TProject.GetAutoCreatedFormsList
  //LazarusIDE.ActiveProject.;  // TmpAutoCreatedForms
  //GetPropInfo();
  Result := mrOk;
end;

function TMyIDEIntf.onProjectBuilding(Sender: TObject): TModalResult;

  //function GetCompileReasons: TCompileReasons;
  //begin
  //  Result := [];
  //  try
  //    PByte(@Result)^ := Byte(GetOrdProp(LazarusIDE.ActiveProject.LazCompilerOptions, 'CompileReasons'));
  //  except
  //  end;
  //end;

var
  LParams: TCompileParam;
  //LReasons: TCompileReasons;
  LResult: Boolean;
begin
  //LReasons := GetCompileReasons;

  //if crCompile in LReasons then
  //  Logs('crCompile');
  //if crBuild in LReasons then
  //  Logs('crBuild');
  //if crRun in LReasons then
  //  Logs('crRun');

  FWithoutDebug := True; // 总是真，没得区分是哪个按钮点的了。
  Logs('TMyIDEIntf.onProjectBuilding: Status: ' + IntToStr(Integer(LazarusIDE.ToolStatus)));

  LazarusIDE.DoShowMessagesView();

  LazarusIDE.ToolStatus:=itBuilder;
  try
    LParams.Input := TargetDir;
    LParams.Output := TargetFile;

    // for golang
    if OutLang = olGo then
    begin
      LParams.GoUseTempdll:= Self.GoUseTempdll;
      LParams.GoEnabledFinalizerOn:= Self.GoEnabledFinalizerOn;
      LParams.GoTags:=Self.GoTags;
      LParams.GoEnabledCGO:=Self.GoEnabledCGO;
      LParams.GoBuildMode:=Self.GoBuildMode;
      LParams.GoRoot:= Self.GoRoot;
    end;

    LResult := False;
    try
      LResult := Lang.Compile(LParams);
    except
      on E: Exception do
        CtlWriteln(mluError, E.Message);
    end;
    onProjectBuildingFinished(Self, LResult);
  finally
    LazarusIDE.ToolStatus:=itNone;
  end;
  Result := FModalResult;
end;

procedure TMyIDEIntf.onProjectBuildingFinished(Sender: TObject;
  BuildSuccessful: Boolean);
var
  LTargetFile, LTargetCmd: string;
begin
  LazarusIDE.ToolStatus := itNone;
  //Logs('TMyIDEIntf.onProjectBuildingFinished: ' + BoolToStr(BuildSuccessful, True));
  try
    if BuildSuccessful and FWithoutDebug then
    begin
      LTargetFile := Self.TargetFile;
      if FileExists(LTargetFile) then
      begin
        LTargetCmd := '$TargetCmdLine()';
        if IDEMacros.SubstituteMacros(LTargetCmd) then
        begin
          //Logs('onProjectBuildingFinished run: ' + LTargetCmd);
          Lang.ExecuteCommand(LTargetCmd, False, True, ExtractFileDir(LTargetFile));
          onRunFinished(Self);
        end;
      end;
    end;
  finally
    FWithoutDebug := False;
  end;
end;

function TMyIDEIntf.onProjectDependenciesCompiling(Sender: TObject): TModalResult;
begin
  Logs('TMyIDEIntf.onProjectDependenciesCompiling');
  Result := FModalResult;
end;

function TMyIDEIntf.onProjectDependenciesCompiled(Sender: TObject): TModalResult;
begin
  Logs('TMyIDEIntf.onProjectDependenciesCompiled');
  Result := FModalResult;
end;

function TMyIDEIntf.onLazarusBuilding(Sender: TObject): TModalResult;
begin
  Logs('TMyIDEIntf.onLazarusBuilding');
  Result := FModalResult;
end;

procedure TMyIDEIntf.onLazarusBuildingFinished(Sender: TObject;
  BuildSuccessful: Boolean);
begin
  Logs('TMyIDEIntf.onLazarusBuildingFinished');
end;

function TMyIDEIntf.onRunDebug(Sender: TObject; var Handled: boolean): TModalResult;
begin
  Logs('TMyIDEIntf.onRunDebug');
  Result := FModalResult;
end;

function TMyIDEIntf.onRunWithoutDebugBuilding(Sender: TObject;
  var Handled: boolean): TModalResult;
begin
  FWithoutDebug := True;
  Logs('TMyIDEIntf.onRunWithoutDebugBuilding');
  Result := FModalResult;
end;

function TMyIDEIntf.onRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
begin
   Logs('TMyIDEIntf.onRunWithoutDebugInit');
   Result := FModalResult;
end;

procedure TMyIDEIntf.onRunFinished(Sender: TObject);
begin
  Logs('TMyIDEIntf.onRunFinished');
end;

procedure TMyIDEIntf.onChangeToolStatus(Sender: TObject; OldStatus,
  NewStatus: TLazToolStatus);
begin
  Logs('TMyIDEIntf.onChangeToolStatus: OldStatus=%d, NewStatus=%d', [Ord(OldStatus), Ord(NewStatus)]);
end;

class procedure TMyIDEIntf.AddHandlers;
begin
  if Assigned(LazarusIDE) then
  begin
    Logs('MyIDEIntf.AddHandlers');
    with MyIDEIntf do
    begin
      LazarusIDE.AddHandlerOnSavedAll(@onSaveAll, True);
      LazarusIDE.AddHandlerOnSaveEditorFile(@onSaveEditorFile, True);
      LazarusIDE.AddHandlerOnProjectOpened(@onProjectOpened, True);

      //// complie
      //[4968] TMyIDEIntf.onProjectBuilding
      //[4968] TMyIDEIntf.onProjectDependenciesCompiling
      //[4968] TMyIDEIntf.onProjectDependenciesCompiled
      //[4968] TMyIDEIntf.onProjectBuildingFinished
      //
      //// debug
      //[4968] TMyIDEIntf.onProjectBuilding
      //[4968] TMyIDEIntf.onProjectDependenciesCompiling
      //[4968] TMyIDEIntf.onProjectDependenciesCompiled
      //[4968] TMyIDEIntf.onProjectBuildingFinished
      //[4968] TMyIDEIntf.onRunDebug
      //[4968] TMyIDEIntf.onRunFinished
      //
      //// run without debug
      //[4968] TMyIDEIntf.onRunFinished
      //[4968] TMyIDEIntf.onRunWithoutDebugBuilding
      //[4968] TMyIDEIntf.onProjectBuilding
      //[4968] TMyIDEIntf.onProjectDependenciesCompiling
      //[4968] TMyIDEIntf.onProjectDependenciesCompiled
      //[4968] TMyIDEIntf.onProjectBuildingFinished
      //[4968] TMyIDEIntf.onRunWithoutDebugInit


      // 编译
      if OutLang = olGo then
      begin
        LazarusIDE.AddHandlerOnChangeToolStatus(@onChangeToolStatus, FAddToLast);
        LazarusIDE.AddHandlerOnProjectBuilding(@onProjectBuilding, FAddToLast);
        LazarusIDE.AddHandlerOnProjectBuildingFinished(@onProjectBuildingFinished, FAddToLast);
        LazarusIDE.AddHandlerOnProjectDependenciesCompiling(@onProjectDependenciesCompiling, FAddToLast);
        LazarusIDE.AddHandlerOnProjectDependenciesCompiled(@onProjectDependenciesCompiled, FAddToLast);
        LazarusIDE.AddHandlerOnLazarusBuilding(@onLazarusBuilding, FAddToLast);
        LazarusIDE.AddHandlerOnLazarusBuildingFinished(@onLazarusBuildingFinished, FAddToLast);
        LazarusIDE.AddHandlerOnRunDebug(@onRunDebug, FAddToLast);
        LazarusIDE.AddHandlerOnRunWithoutDebugBuilding(@onRunWithoutDebugBuilding, FAddToLast);
        LazarusIDE.AddHandlerOnRunWithoutDebugInit(@onRunWithoutDebugInit, FAddToLast);
        LazarusIDE.AddHandlerOnRunFinished(@onRunFinished, FAddToLast);
      end;
    end;
  end;
end;

class procedure TMyIDEIntf.RemoveHandlers;
begin
  if Assigned(LazarusIDE) and Assigned(MyIDEIntf) then
  begin
    LazarusIDE.RemoveAllHandlersOfObject(MyIDEIntf);
    Logs('MyIDEIntf.Removehandlers');
  end;
end;


initialization
  Init;

finalization
  UnInit;

end.

