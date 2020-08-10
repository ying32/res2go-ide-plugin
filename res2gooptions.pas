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
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  IDEOptionsIntf,
  IDEOptEditorIntf,
  LazIDEIntf,
  ProjectIntf,
  ProjectResourcesIntf,
  res2goresources,
  Laz2_XMLCfg,
  uSupports;

type


  { TProjectRes2goRes }

  TProjectRes2goRes = class(TAbstractProjectResource)
  private
    FEnabled: Boolean;
    FOutLang: TOutLang;
    FOutputPath: string;
    FSaveGfmFile: Boolean;
    FUseOriginalFileName: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetOutLang(AValue: TOutLang);
    procedure SetOutputPath(AValue: string);
    procedure SetSaveGfmFile(AValue: Boolean);
    procedure SetUseOriginalFileName(AValue: Boolean);
  public
    function UpdateResources(AResources: TAbstractProjectResources; const {%H-}MainFilename: string): Boolean; override;
    procedure WriteToProjectFile(AConfig: TObject; const Path: String); override;
    procedure ReadFromProjectFile(AConfig: TObject; const Path: String); override;
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OutputPath: string read FOutputPath write SetOutputPath;
    property UseOriginalFileName: Boolean read FUseOriginalFileName write SetUseOriginalFileName;
    property SaveGfmFile: Boolean read FSaveGfmFile write SetSaveGfmFile;
    property OutLang: TOutLang read FOutLang write SetOutLang;
  end;

  { TRes2goOptionsFrame }

  TRes2goOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkSaveGfmFile: TCheckBox;
    chkEanbledConvert: TCheckBox;
    chkUseOriginalFileName: TCheckBox;
    cbbLangs: TComboBox;
    Label1: TLabel;
    lblOutLang: TLabel;
    lblOutputPath: TLabeledEdit;
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



{ TProjectRes2goRes }

procedure TProjectRes2goRes.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  Self.Modified:=True;
end;

procedure TProjectRes2goRes.SetOutLang(AValue: TOutLang);
begin
  if FOutLang=AValue then Exit;
  FOutLang:=AValue;
  Self.Modified:=True;
end;

procedure TProjectRes2goRes.SetOutputPath(AValue: string);
begin
  if FOutputPath=AValue then Exit;
  FOutputPath:=AValue;
  Self.Modified:=True;
end;

procedure TProjectRes2goRes.SetSaveGfmFile(AValue: Boolean);
begin
  if FSaveGfmFile=AValue then Exit;
  FSaveGfmFile:=AValue;
  Self.Modified:=True;
end;

procedure TProjectRes2goRes.SetUseOriginalFileName(AValue: Boolean);
begin
  if FUseOriginalFileName=AValue then Exit;
  FUseOriginalFileName:=AValue;
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
    SetDeleteValue(Path+'Res2go/UseOriginalFileName/Value', UseOriginalFileName, False);
    SetDeleteValue(Path+'Res2go/SaveGfmFile/Value', SaveGfmFile, False);
    SetDeleteValue(Path+'Res2go/OutLang/Value', Integer(OutLang), 0);
  end;
end;

procedure TProjectRes2goRes.ReadFromProjectFile(AConfig: TObject;
  const Path: String);
begin
  with TXMLConfig(AConfig) do
  begin
    Enabled := GetValue(Path+'Res2go/Enabled/Value', False);
    OutputPath := GetValue(Path+'Res2go/OutputPath/Value', '');
    UseOriginalFileName := GetValue(Path+'Res2go/UseOriginalFileName/Value', False);
    SaveGfmFile := GetValue(Path+'Res2go/SaveGfmFile/Value', False);
    OutLang := TOutLang(GetValue(Path+'Res2go/SaveGfmFile/Value', 0));
  end;

  if Assigned(MyIDEIntf) then
  begin
    MyIDEIntf.EnabledCovert := Enabled;
    MyIDEIntf.OutputPath := OutputPath;
    MyIDEIntf.UseOriginalFileName := UseOriginalFileName;
    MyIDEIntf.SaveGfmFile := SaveGfmFile;
    MyIDEIntf.OutLang:=OutLang;
    //Logs('--TProjectRes2goRes.ReadFromProjectFile: ');
  end;
end;



{ TRes2goOptionsFrame }



constructor TRes2goOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  chkEanbledConvert.Caption:=rsEnabledConvert;
  chkUseOriginalFileName.Caption:=rsUseOriginalFileName;
  chkSaveGfmFile.Caption := rsSaveGfmFile;
  lblOutputPath.EditLabel.Caption:=rsOutputPath;
  Label1.Caption:=rsOutputPathEg;
  lblOutLang.Caption:=rsOutLang;
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
      MyIDEIntf.UseOriginalFileName:= LRes.UseOriginalFileName;
      MyIDEIntf.SaveGfmFile := LRes.SaveGfmFile;
      MyIDEIntf.OutLang := LRes.OutLang;

      chkEanbledConvert.Checked := MyIDEIntf.EnabledCovert;
      lblOutputPath.Text := MyIDEIntf.OutputPath;
      ChkUseOriginalFileName.Checked:= MyIDEIntf.UseOriginalFileName;
      ChkSaveGfmFile.Checked := MyIDEIntf.SaveGfmFile;
      cbbLangs.ItemIndex:=Integer(MyIDEIntf.OutLang);
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
      MyIDEIntf.EnabledCovert := chkEanbledConvert.Checked;
      MyIDEIntf.OutputPath:= lblOutputPath.Text;
      MyIDEIntf.UseOriginalFileName:=chkUseOriginalFileName.Checked;
      MyIDEIntf.SaveGfmFile := chkSaveGfmFile.Checked;
      MyIDEIntf.OutLang:=TOutLang(cbbLangs.ItemIndex);

      LRes.OutputPath := MyIDEIntf.OutputPath;
      LRes.Enabled := MyIDEIntf.EnabledCovert;
      LRes.UseOriginalFileName := MyIDEIntf.UseOriginalFileName;
      LRes.SaveGfmFile := MyIDEIntf.SaveGfmFile;
      LRes.OutLang:=MyIDEIntf.OutLang;
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

