//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------
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
  LazIDEIntf,
  ProjectIntf,
  IDEMsgIntf,
  IDEExternToolIntf,
  FormEditingIntf,
  TypInfo,
  usupports,
  res2goresources,
  LResources;

type


  { TMyIDEIntf }

  TMyIDEIntf = class
  private
    FEvents: array of TEventItem;

    FEnabledCovert: Boolean;
    FOutLang: TOutLang;
    FOutputPath: string;
    FSaveGfmFile: Boolean;
    FUseOriginalFileName: Boolean;

    function GetReadOutputPath: string;
    procedure SaveComponents(ADesigner: TIDesigner; AUnitFileName, AOutPath: string);
    procedure OnWriteMethodProperty(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo;
      const MethodValue, DefMethodValue: TMethod; var Handled: boolean);


    procedure CheckAndCreateDir;

    function GetProjectPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    function onSaveAll(Sender: TObject): TModalResult;
    function onSaveEditorFile(Sender: TObject; aFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
    function onProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;

    property EnabledCovert: Boolean read FEnabledCovert write FEnabledCovert;
    property OutputPath: string read FOutputPath write FOutputPath;
    property UseOriginalFileName: Boolean read FUseOriginalFileName write FUseOriginalFileName;
    property SaveGfmFile: Boolean read FSaveGfmFile write FSaveGfmFile;
    property OutLang: TOutLang read FOutLang write FOutLang;


    property ProjectPath: string read GetProjectPath;
    property ReadOutputPath: string read GetReadOutputPath;
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
  if Assigned(LazarusIDE) then
  begin
    LazarusIDE.AddHandlerOnSavedAll(@MyIDEIntf.onSaveAll, True);
    LazarusIDE.AddHandlerOnSaveEditorFile(@MyIDEIntf.onSaveEditorFile, True);
    LazarusIDE.AddHandlerOnProjectOpened(@MyIDEIntf.onProjectOpened, True);
  end;
end;


procedure Init;
begin
  MyIDEIntf := TMyIDEIntf.Create;
end;

procedure UnInit;
begin
  if Assigned(LazarusIDE) then
    LazarusIDE.RemoveAllHandlersOfObject(MyIDEIntf);
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
  LGfmFileName, LGoFileName: string;
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
      LGoFileName := ReadOutputPath;
      if Self.UseOriginalFileName then
        LGoFileName += AUnitFileName
      else
        LGoFileName += ADesigner.LookupRoot.Name;

      case OutLang of
         olGo:
           begin
             LGoFileName += '.go';
             //CtlWriteln(mluNone, rsMsgTransformFile, [ExtractFileName(AUnitFileName)]);
             Golang.SaveToFile(LGoFileName, ADesigner.LookupRoot, FEvents, LStream);
           end;
         olNim:
           begin

           end;
         olRust:
           begin

           end;
      end;

      // 保存gfm文件
      if Self.SaveGfmFile then
      begin
        LGfmFileName := AOutPath;
        if Self.UseOriginalFileName then
          LGfmFileName += AUnitFileName
        else
          LGfmFileName += ADesigner.LookupRoot.Name;
        LGfmFileName += '.gfm';
        LStream.Position := 0;
        LStream.SaveToFile(LGfmFileName);
      end;
    finally
      LStream.Free;
    end;
  end;
end;

function TMyIDEIntf.GetReadOutputPath: string;
begin
  Result := ProjectPath + OutputPath + PathDelim;
end;

// FakeIsJITMethod
function IsJITMethod(const aMethod: TMethod): boolean;
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

  if IsJITMethod(MethodValue) then
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

  case OutLang of
    olGo: LEvent.EventParams := Golang.ToEventString(PropInfo);
    olNim:;
    olRust:;
  end;


  SetLength(FEvents, Length(FEvents) + 1);
  FEvents[High(FEvents)] := LEvent;

  //if Assigned(PropInfo) then
  //  Logs('LComponentName%s, LMethodName=%s, Event.Name=%s', [LComponentName, LMethodName, PropInfo^.Name]);
end;

procedure TMyIDEIntf.CheckAndCreateDir;
var
  LOutPath: string;
begin
  LOutPath := ProjectPath + OutputPath;
  if not SysUtils.DirectoryExists(LOutPath) then
    SysUtils.CreateDir(LOutPath);
end;

function TMyIDEIntf.GetProjectPath: string;
begin
  Result := '';
  if Assigned(LazarusIDE.ActiveProject) then
    Result := ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

constructor TMyIDEIntf.Create;
begin
  inherited Create;
end;

destructor TMyIDEIntf.Destroy;
begin
     inherited Destroy;
end;

function TMyIDEIntf.onSaveAll(Sender: TObject): TModalResult;
var
  I: Integer;
  LExt, LFileName: string;
begin
  if EnabledCovert then
  begin
    ClearMsg;
    CheckAndCreateDir;
    for I := 0 to LazarusIDE.ActiveProject.FileCount - 1 do
    begin
      LFileName := LazarusIDE.ActiveProject.Files[I].FileName;
      LExt := ExtractFileExt(LFileName);
      if SameText(LExt, '.lpr') then
      begin
        CtlWriteln(mluNone, rsMsgTransformFile, [ExtractFileName(LFileName)]);
        GoLang.ConvertProjectFile(LFileName, ReadOutputPath);
        Break;
      end
    end;
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
    if EnabledCovert then
    begin
      ClearMsg;
      CheckAndCreateDir;
      LDesigner := LazarusIDE.GetDesignerWithProjectFile(aFile, False);
      if Assigned(LDesigner) and Assigned(LDesigner.LookupRoot) then
      begin
        //Logs('onSaveEditorFile lookupRoot: %s', [LDesigner.LookupRoot.Name]);

        SaveComponents(LDesigner, GetFileNameWithoutExt(TargetFilename),  ReadOutputPath);
      end;
    end;
  end;
  Result := mrOk;
end;

function TMyIDEIntf.onProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result := mrOk;
end;


initialization
  Init;

finalization
  UnInit;

end.

