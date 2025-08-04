//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------
unit usupports;

{$mode objfpc}{$H+}

interface

uses
{$ifdef windows}
  Windows,
{$endif}
  Classes, SysUtils, fgl, IDEMsgIntf, IDEExternToolIntf;


type
  TTypeLists = specialize TFPGMap<string, string>;

  TOutLang = (olError=-1, olGo, olNim, olRust);

  TEventItem = record
    InstanceName: string;
    EventName: string;
    EventTypeName: string;
    EventParams: string;
  end;

const
  PrivateFiledsFlagStr = '::private::';
  PrivateFiledsStr = 'T%sFields';

  //function IsZhLang: Boolean;
  function IsSupportsComponent(AClass: string): Boolean;
  procedure CtlWriteln(AUrgency: TMessageLineUrgency; const AFmt: string; AArgs: array of const); overload;
  procedure CtlWriteln(AUrgency: TMessageLineUrgency; const AFmt: string); overload;
  function GetFileNameWithoutExt(AFileName: string): string;
  function ChangeExtName(AFileName, AExt: string): string;
  procedure ClearMsg;
  procedure Logs(const AFmt: string; AArgs: array of const);  overload;
  procedure Logs(const AStr: string); overload;

implementation


type
  TClassLists = specialize  TFPGMap<string, string>;

var
  uClassLists: TClassLists;


// 中文环境，不区分简/繁
{
  "zh-CN":  2052, // 简体
  "zh-HK":  3076, // 繁体
  "zh-MO":  5124, // 繁体
  "zh-SG":  4100, // 简体
  "zh-TW":  1028, // 繁体
}
//function IsZhLang: Boolean;
//{$IFDEF WINDOWS}
//const
//   Langs:array[0..4] of Integer = (2052, 3076, 5124, 4100, 1028);
//{$ELSE}
//const
//   Langs:array[0..4] of string = ('zh_CN', 'zh_HK', 'zh_MO', 'zh_SG', 'zh_TW');
//{$ENDIF}
//var
//  I: Integer;
//begin
//  Result := False;
//  for I := 0 to High(Langs) do
//  begin
//  {$IFDEF MSWINDOWS}
//    if Langs[I] = SysLocale.DefaultLCID then
//  {$ELSE}
//    if Pos(Langs[I], GetEnvironmentVariable('LANG')) <> -1 then
//  {$ENDIF}
//      Exit(True);
//  end;
//end;

procedure CtlWriteln(AUrgency: TMessageLineUrgency; const AFmt: string; AArgs: array of const);
begin
  AddIDEMessage(AUrgency, Format(AFmt, AArgs));
end;

procedure CtlWriteln(AUrgency: TMessageLineUrgency; const AFmt: string);
begin
  CtlWriteln(AUrgency, '%s', [AFmt]);
end;

function GetFileNameWithoutExt(AFileName: string): string;
var
  LExt: string;
begin
  Result := ExtractFileName(AFileName);
  LExt := ExtractFileExt(AFileName);
  Result := Copy(Result, 1, Length(Result) - Length(LExt));
end;

function ChangeExtName(AFileName, AExt: string): string;
begin
  Result := GetFileNameWithoutExt(AFileName) + AExt;
end;

procedure ClearMsg;
begin
  if Assigned(IDEMessagesWindow) then
    IDEMessagesWindow.Clear;
end;


function IsSupportsComponent(AClass: string): Boolean;
begin
  Result := uClassLists.IndexOf(AClass) <> -1;
end;

procedure AddComponentClass(AClassName: string);
begin
  uClassLists.AddOrSetData(AClassName, '');
end;

procedure MyRegisterComponents(AClassNames: array of string);
var
  LB: string;
begin
  for LB in AClassNames do
    AddComponentClass(LB);
end;

procedure InitClassLists;
begin
  MyRegisterComponents([
    'TButton','TBitBtn','TMaskEdit', 'TEdit',
    'TMainMenu', 'TPopupMenu', 'TMemo', 'TCheckBox', 'TRadioButton', 'TGroupBox',
    'TLabel', 'TListBox', 'TComboBox', 'TPanel', 'TImage', 'TToggleBox',
    'TLinkLabel',
    'TSpeedButton', 'TSplitter', 'TRadioGroup', 'TStaticText', 'TColorBox',
    'TColorListBox', 'TTrayIcon', 'TOpenDialog', 'TSaveDialog', 'TColorDialog',
    'TFontDialog', 'TPrintDialog', 'TOpenPictureDialog', 'TSavePictureDialog',
    'TRichEdit', 'TTrackBar', 'TImageList', 'TUpDown', 'TProgressBar',
    'TDateTimePicker', 'TMonthCalendar', 'TListView', 'TTreeView',
    'TStatusBar', 'TToolBar', 'TPageControl', 'TTabSheet', 'TStatusPanels',
    'TStatusPanel', 'TActionList', 'TAction', 'TToolButton', 'TPaintBox', 'TTimer', 'TScrollBar',
    'TShape', 'TBevel', 'TScrollBox', 'TCheckListBox', 'TSelectDirectoryDialog', 'TMenuItem',
    'TGauge',
    'TImageButton', 'TControlBar',
    'TFontDialog', 'TFindDialog', 'TReplaceDialog', 'TPageSetupDialog',
    'TPrinterSetupDialog', 'TStringGrid', 'TDrawGrid', 'TValueListEditor',
    'THeaderControl', 'THeaderSection', 'THeaderSections', 'TLabeledEdit', 'TBoundLabel',
    'TFlowPanel', 'TCoolBar', 'TSpinEdit', 'TSpinEditEx',
    'TMiniWebview',
    'TTaskDialog', 'TCalendar', 'TComboBoxEx', 'TTimeEdit',
    'TXButton', 'TToggleBox', 'TCheckGroup', 'TColorButton', 'TFloatSpinEdit', 'TDirectoryEdit',
    'TCheckComboBox', 'TNoteBook', 'TPage', 'TButtonPanel', 'TPanelBitBtn'
    ]);
end;

procedure Logs(const AFmt: string; AArgs: array of const);
begin
{$ifdef windows}
  OutputDebugStringW(PWideChar(UnicodeString(Format(AFmt, AArgs))));
{$endif}
end;

procedure Logs(const AStr: string);
begin
{$ifdef windows}
  Logs('%s', [AStr]);
{$endif}
end;


initialization

  uClassLists := TClassLists.Create;
  InitClassLists;

finalization
  uClassLists.Free;

end.

