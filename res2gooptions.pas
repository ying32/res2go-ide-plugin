//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------
unit res2goOptions;

{$mode objfpc}{$H+}

interface

uses
{$ifdef windows}
  Windows,
{$endif}
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  IDEOptionsIntf,
  IDEOptEditorIntf,
  LazIDEIntf,
  ProjectIntf,
  ProjectResourcesIntf,
  Laz2_XMLCfg;

type

  { TRes2goIDEOptions }

  //TRes2goIDEOptions = class(TAbstractIDEProjectOptions)
  //private
  //  FProject: TLazProject;
  //public
  //  constructor Create(AProject: TLazProject);
  //  destructor Destroy; override;
  //  function GetProject: TLazProject; override;
  //  class function GetInstance: TAbstractIDEOptions; override;
  //  class function GetGroupCaption: string; override;
  //  property Project: TLazProject read FProject;
  //end;

  { TProjectRes2goRes }

  TProjectRes2goRes = class(TAbstractProjectResource)
  private
    FEnabled: Boolean;
    FOutputPath: string;
    procedure SetEnabled(AValue: Boolean);
    procedure SetOutputPath(AValue: string);
  public
    function UpdateResources(AResources: TAbstractProjectResources; const {%H-}MainFilename: string): Boolean; override;
    procedure WriteToProjectFile(AConfig: {TXMLConfig}TObject; const Path: String); override;
    procedure ReadFromProjectFile(AConfig: {TXMLConfig}TObject; const Path: String); override;
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OutputPath: string read FOutputPath write SetOutputPath;
  end;

  { TRes2goOptionsFrame }

  TRes2goOptionsFrame = class(TAbstractIDEOptionsEditor)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
  private
    FRes: TProjectRes2goRes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  res2gomain;

const
  ProjectOptionsRes2go = ProjectOptionsMisc + 500;

procedure Logs(const AFmt: string; AArgs: array of const);  overload;
begin
{$ifdef windows}
  OutputDebugStringW(PWideChar(UnicodeString(Format(AFmt, AArgs))));
{$endif}
end;

procedure Logs(const AStr: string); overload;
begin
{$ifdef windows}
  Logs('%s', [AStr]);
{$endif}
end;

{ TProjectRes2goRes }

procedure TProjectRes2goRes.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  Self.Modified:=True;
end;

procedure TProjectRes2goRes.SetOutputPath(AValue: string);
begin
  if FOutputPath=AValue then Exit;
  FOutputPath:=AValue;
  Self.Modified:=True;
end;

function TProjectRes2goRes.UpdateResources(
  AResources: TAbstractProjectResources; const MainFilename: string): Boolean;
begin

end;

procedure TProjectRes2goRes.WriteToProjectFile(AConfig: TObject;
  const Path: String);
begin
  with TXMLConfig(AConfig) do
  begin
    SetDeleteValue(Path+'Res2go/Enabled/Value', Enabled, False);
    SetDeleteValue(Path+'Res2go/OutputPath/Value', OutputPath, '');
  end;
end;

procedure TProjectRes2goRes.ReadFromProjectFile(AConfig: TObject;
  const Path: String);
begin
  Enabled := TXMLConfig(AConfig).GetValue(Path+'Res2go/Enabled/Value', False);
  OutputPath := TXMLConfig(AConfig).GetValue(Path+'Res2go/OutputPath/Value', '');
  if Assigned(MyIDEIntf) then
  begin
    MyIDEIntf.EnabledCovert := Enabled;
    MyIDEIntf.OutputPath := OutputPath;
    Logs('--TProjectRes2goRes.ReadFromProjectFile: ');
  end;
end;

{ TRes2goIDEOptions }

//constructor TRes2goIDEOptions.Create(AProject: TLazProject);
//begin
//  inherited Create;
//  FProject := AProject;
//end;
//
//destructor TRes2goIDEOptions.Destroy;
//begin
//  inherited Destroy;
//end;
//
//function TRes2goIDEOptions.GetProject: TLazProject;
//begin
//  Result:=FProject;
//end;
//
//class function TRes2goIDEOptions.GetInstance: TAbstractIDEOptions;
//begin
//  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) then
//    Result := LazarusIDE.ActiveProject.FIDEOptions
//  else
//    Result := nil;
//end;

//class function TRes2goIDEOptions.GetGroupCaption: string;
//begin
//  Result := 'res2go Options';
//end;

{ TRes2goOptionsFrame }

constructor TRes2goOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRes2goOptionsFrame.Destroy;
begin
  inherited Destroy;
end;

function TRes2goOptionsFrame.GetTitle: string;
begin
  Result := 'res2go';
end;

procedure TRes2goOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  //AppSettingsGroupBox.Caption := dlgApplicationSettings;
  //TitleLabel.Caption := dlgPOTitle;
  //TitleEdit.Text := '';
  //OutputdebugString('TRes2goOptionsFrame.Setup');
end;

procedure TRes2goOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LRes: TProjectRes2goRes;
begin
  if Assigned(MyIDEIntf) and Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) and Assigned(LazarusIDE.ActiveProject.Resources) then
  begin
    LRes := TProjectRes2goRes(TAbstractProjectResources(LazarusIDE.ActiveProject.Resources).Resource[TProjectRes2goRes]);
    if Assigned(LRes) then
    begin
      MyIDEIntf.EnabledCovert := LRes.Enabled;
      MyIDEIntf.OutputPath := LRes.OutputPath;
      CheckBox1.Checked := MyIDEIntf.EnabledCovert;
      LabeledEdit1.Text := MyIDEIntf.OutputPath;
    end;
  end;
end;

procedure TRes2goOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LRes: TProjectRes2goRes;
begin
  if Assigned(MyIDEIntf) and Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) and Assigned(LazarusIDE.ActiveProject.Resources) then
  begin
    LRes := TProjectRes2goRes(TAbstractProjectResources(LazarusIDE.ActiveProject.Resources).Resource[TProjectRes2goRes]);
    if Assigned(LRes) then
    begin
      MyIDEIntf.EnabledCovert := CheckBox1.Checked;
      MyIDEIntf.OutputPath:= LabeledEdit1.Text;
      LRes.OutputPath := MyIDEIntf.OutputPath;
      LRes.Enabled := MyIDEIntf.EnabledCovert;
    end;
  end;
end;

class function TRes2goOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil;//TProjectIDEOptions;
end;



initialization
  //RegisterIDEOptionsGroup(GroupProject, TRes2goIDEOptions);
  RegisterProjectResource(TProjectRes2goRes);
  RegisterIDEOptionsEditor(GroupProject, TRes2goOptionsFrame, ProjectOptionsRes2go);

end.

