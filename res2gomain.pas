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
  LazIDEIntf,
  ProjectIntf,
  IDEMsgIntf,
  IDEExternToolIntf;

type

  { TMyIDEIntf }

  TMyIDEIntf = class
  private
    FEnabledCovert: Boolean;
    FOutputPath: string;
    function GetProjectPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    function onSaveAll(Sender: TObject): TModalResult;
    function onSaveEditorFile(Sender: TObject; aFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
    function onProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;

    property EnabledCovert: Boolean read FEnabledCovert write FEnabledCovert;
    property OutputPath: string read FOutputPath write FOutputPath;
    property ProjectPath: string read GetProjectPath;
  end;


var
  MyIDEIntf: TMyIDEIntf;

procedure Register;

implementation

uses
  uresourceformtogo;


procedure ClearMsg;
begin
  if Assigned(IDEMessagesWindow) then
    IDEMessagesWindow.Clear;
end;

function CombinePath(Path1, Path2: string): string;
var
  LC: Char;
begin
  if Path2 <> '' then
  begin
    if Path2.StartsWith(SysUtils.PathDelim) then
      Path2 := Copy(Path2, 2, Length(Path2) - 1);
    if not Path2.EndsWith(PathDelim) then
      Path2 := Copy(Path2, 1, Length(Path2) - 1);
  end;
  Result := Path1 + Path2;
end;

procedure ConvertFile(const AInputFile: string);
var
  LOutPath, LExt, LLfmFileName: string;
begin
  try
    //Outputdebugstring(PChar('cv File: ' + AInputFile));
    LOutPath := MyIDEIntf.ProjectPath + MyIDEIntf.OutputPath;

    if not SysUtils.DirectoryExists(LOutPath) then
      SysUtils.CreateDir(LOutPath);
    LExt := ExtractFileExt(AInputFile);
    if SameText(LExt, '.lpr') then
    begin
     if IsZhLang then
       CtlWriteln(mluNone, '转换文件：%s', [AInputFile])
     else
       CtlWriteln(mluNone, 'Transform file: %s', [AInputFile]);
      ProjectFileToMainDotGo(AInputFile, LOutPath)
    end
    else if SameText(LExt, '.pas') then
    begin
      LLfmFileName := ExtractFilePath(AInputFile) + ChangeExtName(AInputFile, '.lfm');
      if FileExists(LLfmFileName) then
      begin
        if IsZhLang then
          CtlWriteln(mluNone, '转换文件：%s', [LLfmFileName])
        else
          CtlWriteln(mluNone, 'Transform file: %s', [LLfmFileName]);
        ResouceFormToGo(LLfmFileName, LOutPath);
      end;
    end;
  except
    on E: Exception do
      //OutputDebugString(PChar('Exception: ' + E.Message));
  end;
end;

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
begin
  if Assigned(MyIDEIntf) and MyIDEIntf.EnabledCovert then
  begin
    ClearMsg;
    for I := 0 to LazarusIDE.ActiveProject.FileCount - 1 do
      ConvertFile(LazarusIDE.ActiveProject.Files[I].Filename);
  end;
  Result := mrOk;
end;

function TMyIDEIntf.onSaveEditorFile(Sender: TObject; aFile: TLazProjectFile;
  SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
begin
  if SaveStep = sefsAfterWrite then
  begin
    if Assigned(MyIDEIntf) and MyIDEIntf.EnabledCovert then
    begin
      ClearMsg;
      ConvertFile(TargetFilename);
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

