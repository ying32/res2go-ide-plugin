//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------
unit ugolang;

{$mode objfpc}{$H+}

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
  CompOptsIntf,
  uSupports,
  res2goResources,
  uLangBase;

type

  { TGoLang }

  TGoLang = class(TLangBase)
  private
    function ParamTypeCov(ASrc: string): string;
    function IsBaseType(AType: string): Boolean;
    function IsInterfaceType(AType: string): Boolean;
    procedure CreateImplFile(AFileName: string; AEvents: array of TEventItem; AFormName: string);
    procedure CreateNewMain(AStrs: TStrings; AParam: TProjParam);
    function GetPrefixPackage: string;

    function GetPackageImportPath(const AOutPath: string): string;

    procedure AddOrRemoveImports(ALists: TStrings; AAdds, ARemoves: array of string; AUIPackageName: string);
    procedure ProcessMainFunc(ALists: TStrings; ATitle: string; AUseScaled: Boolean; AForms: array of string);
    function GetOS(ATargetOS: string): string;
    function GetARCH(ATargetCPU: string): string;
  protected
    procedure InitTypeLists; override;
    procedure InitBaseTypes; override;
    function GetResFileExists: Boolean; override;
    function GetPackageName: string; override;
  public
    constructor Create;
    function Compile(AParams: TCompileParam): Boolean; override;

    procedure ConvertProjectFile(AParam: TProjParam); override;
    function ToEventString(AProp: PPropInfo): string; override;
    procedure SaveToFile(AFileName: string; ARoot: TComponent; AEvents: array of TEventItem; AMem: TMemoryStream); override;
    procedure ConvertResource(const AResFileName, APath: string); override;
  end;

var
  Golang: TGolang;

implementation


uses
  Math;

{ TGoLang }

constructor TGoLang.Create;
begin
  inherited Create;
end;

function TGoLang.Compile(AParams: TCompileParam): Boolean;
var
  LCmd, LCmd2: string;
  LTool: TIDEExternalToolOptions;
  LOpts: TLazCompilerOptions;
  LARCH, LOS: string;
  LTags: string = '';
  LIsWindows: Boolean = False;
  LLdFlags: string = '';
  LBuildMode: string = '';
begin
  Result := False;
  if not Assigned(RunExternalTool) then
    Exit;
{$ifdef windows}
  LIsWindows := True;
{$endif}

  // compiler options
  LOpts :=LazarusIDE.ActiveProject.LazCompilerOptions;
  LOS := GetOS(LOpts.TargetOS);
  LARCH := GetARCH(LOpts.TargetCPU);


  // -ldflags

  // windowsgui
  if LIsWindows and LOpts.Win32GraphicApp then
    LLdFlags += ' -H windowsgui';
  // no debug info
  if not LOpts.GenerateDebugInfo then
    LLDFlags += ' -w';
  // strip symbols
  if LOpts.StripSymbols then
    LLDFlags += ' -s';

  LLdFlags := LLdFlags.Trim;
  if not LLdFlags.IsEmpty then
    LLdFlags := ' -ldflags="' + LLdFlags + '"';

  // -tags
  if AParams.GoTags <> '' then
    LTags += AParams.GoTags;
  if AParams.GoUseTempdll and (LOS <> 'darwin') and (not LTags.Contains('tempdll')) then
    LTags += ' tempdll';
  if AParams.GoEnabledFinalizerOn and (not LTags.Contains('finalizerOn')) then
    LTags += ' finalizerOn';

  LTags := LTags.Trim;
  if not LTags.IsEmpty then
    LTags := ' -tags="' + LTags + '"';

  // -buildmode
  if AParams.GoBuildMode <> '' then
    LBuildMode := ' -buildmode=' + AParams.GoBuildMode;

  // command line
  LCmd := Format('build -i%s%s%s -o "%s"', [LBuildMode, LLdFlags, LTags, AParams.Output]);
  LCmd2 := 'go ' + LCmd;
  Logs('Complie Command: ' + LCmd2);
  LTool := TIDEExternalToolOptions.Create;
  try
    LTool.Title := LCmd2;
    LTool.Hint := LCmd2;
    LTool.Executable := 'go'{$ifdef windows}+'.exe'{$endif};
    LTool.WorkingDirectory := AParams.Input;
    LTool.CmdLineParams := LCmd;
    //Application.GetEnvironmentList(LTool.EnvironmentOverrides);

    if LOS <> '' then
      LTool.EnvironmentOverrides.Values['GOOS'] := LOS;
    if LARCH <> '' then
      LTool.EnvironmentOverrides.Values['GOARCH'] := LARCH;

    // cgo
    LTool.EnvironmentOverrides.Values['CGO_ENABLED'] := IfThen(AParams.GoEnabledCGO, '1', '0');

    LTool.Parsers.Add(SubToolFPC);
    LTool.Parsers.Add(SubToolDefault);
    LTool.ShowConsole := True;
    LTool.HideWindow := True;
    LTool.ResolveMacros := True;
    Result := RunExternalTool(LTool);
  finally
    LTool.Free;
  end;
end;

procedure TGoLang.ConvertProjectFile(AParam: TProjParam);

  function GetForms: TAutoCreateForms;
  var
    I: Integer;
  begin
    Result := Self.GetAutoCreateForms;
    if not Self.IsMainPackage then
    begin
      for I := 0 to High(Result) do
        Result[I] := Self.PackageName + '.' + Result[I];
    end;
  end;

const
  PkgArr: array[0..0] of string = ('_ "github.com/ying32/govcl/pkgs/winappres"');

var
  LMainFile: TStringList;
  LSaveFileName: string;
  LMainFileExists: boolean;
begin
  LMainFile := TStringList.Create;
  try
    LSaveFileName := AParam.OutPath + 'main.go';
    LMainFileExists := FileExists(LSaveFileName);

    // 如果不存在 main.go文件，则新建一个
    if not LMainFileExists then
      CreateNewMain(LMainFile, AParam)
    else
    begin
      // 存在则加载此文件
      LMainFile.LoadFromFile(LSaveFileName);

      if AParam.UseDefaultWinAppRes then
        AddOrRemoveImports(LMainFile, PkgArr, [], Self.PackageName)
      else
        AddOrRemoveImports(LMainFile, [], PkgArr, Self.PackageName);

      ProcessMainFunc(LMainFile, AParam.Title, AParam.UseScaled, GetForms);
    end;
    LMainFile.SaveToFile(LSaveFileName);
  finally
    LMainFile.Free;
  end;
end;

function TGoLang.ToEventString(AProp: PPropInfo): string;
var
  I: Integer;
  LFnParam: TFnParam;
  LCovName: string;
begin
  Result := '';
  // 处理参数
  I := 0;
  for LFnParam in GetParams(AProp) do
  begin
    if LFnParam.Name <> '$self' then
    begin
      if I > 1 then
        Result += ', ';

      Result += FirstCaseChar(LFnParam.Name) + ' '; // + ' <' + LFlagsStr + '>' + ParamTypeCov(LParamType);
      LCovName := ParamTypeCov(LFnParam.&Type);
      if ((pfAddress in LFnParam.Flags) or (pfVar in LFnParam.Flags) or (pfOut in LFnParam.Flags)) and not IsInterfaceType(LCovName) then // 要加 * 号的
        Result += '*';

      // 数组
      if pfArray in LFnParam.Flags then
        Result += '[]';

      // 包名确认
      if pfAddress in LFnParam.Flags then
        Result += 'vcl.'; // 包名
      if (not IsBaseType(LCovName)) and (not (pfAddress in LFnParam.Flags)) then
        Result += 'types.';

      Result += LCovName;
    end;
    Inc(I);
  end;
end;

procedure TGoLang.SaveToFile(AFileName: string; ARoot: TComponent;
  AEvents: array of TEventItem; AMem: TMemoryStream);
var
  LStrStream, LBuffer: TStringStream;
  LLines: TStringList;

  procedure WLine(s: string = '');
  begin
    LLines.Add(S);
  end;

  function GetMaxLength: integer;
  var
    I: integer;
    C: TComponent;
  begin
    Result := 0;
    for I := 0 to ARoot.ComponentCount - 1 do
    begin
      C := ARoot.Components[I];
      Result := Max(Result, Length(C.Name));
    end;
  end;

  function GetIsFrame: boolean;
  begin
    Result := ARoot is TCustomFrame;
  end;

var
  I, LMaxLen: integer;
  C: TComponent;
  LVarName, LFormName, LTempName: string;
  LItem: TEventItem;
  LFindEvent: boolean;
  LReadEventName: string;
  LIsFrame: boolean;
begin
  LStrStream := TStringStream.Create('');
  LBuffer := TStringStream.Create('');
  LLines := TStringList.Create;
  try
    WLine('// ' + rsAutomaticallyGeneratedByTheRes2goDoNotEdit);
    WLine('package ' + PackageName);
    WLine;
    WLine('import (');
    WLine('    "github.com/ying32/govcl/vcl"');
    WLine(')');
    WLine;
    LFormName := ARoot.Name;

    LIsFrame := False;
    if GetIsFrame then
      LIsFrame := True;

    WLine(Format('type T%s struct {', [LFormName]));
    if LIsFrame then
      WLine('    *vcl.TFrame')
    else
      WLine('    *vcl.TForm');
    LMaxLen := GetMaxLength;
    for I := 0 to ARoot.ComponentCount - 1 do
    begin
      C := ARoot.Components[I];

      if not IsSupportsComponent(C.ClassName) then
      begin
        CtlWriteln(mluError, rsComponentIsNotSupported, [LFormName + '.' + C.Name, C.ClassName]);
        //Exit;
      end;

      if C.Name = '' then
        Continue;
      if CharInSet(C.Name[1], ['a'..'z', '_']) then
      begin
        CtlWriteln(mluWarning, rsComponentMustBeCapitalizedFirstToBeExported, [LFormName + '.' + C.Name, C.ClassName]);
        Continue;
      end;
      //CtlWriteln('%s: %s', [C^.Name, C^.ClassName]);
      // 这里查找下，当前组件有事件，但是这个事件是共享的。
      LReadEventName := '';
      LFindEvent := False;
      for LItem in AEvents do
      begin
        if LItem.InstanceName = C.Name then
        begin
          // 当前实际关联的事件不是自己的，比如  Button2Click != Button1Click
          if C.Name + LItem.EventTypeName <> LItem.EventName then
          begin
            LFindEvent := True;
            if LReadEventName <> '' then
              LReadEventName := LReadEventName + ',';
            LReadEventName := LReadEventName + 'On' + LItem.EventName;
          end;
        end;
      end;
      // CtlWriteln('LReadEventName: %s', [LReadEventName]);

      LTempName := Copy(C.Name + DupeString(#32, LMaxLen), 1, LMaxLen);
      if LFindEvent and (LReadEventName <> '') then
        WLine(Format('    %s *%s.%s `events:"%s"`',
          [LTempName, 'vcl', C.ClassName, LReadEventName]))
      else
        WLine(Format('    %s *%s.%s', [LTempName, 'vcl', C.ClassName]));
    end;
    WLine;
    // 添加一个隐式字段，用于私有，方便写一些结构定自定义的变量什么的
    WLine('    ' + PrivateFiledsFlagStr); // 这是一个查找标识
    WLine(Format('    ' + PrivateFiledsStr, [LFormName]));
    WLine('}');
    WLine;
    if not LIsFrame then
    begin
      WLine(Format('var %s *T%s', [LFormName, LFormName]));
      WLine;
      WLine;
      WLine;
    end;
    WLine;
    begin
      LVarName := LFormName + 'Bytes';

      // 包名不为main时，起始不变为小写。
      //if PackageName.Equals('main') or PackageName.IsEmpty then
      LVarName[1] := LowerCase(LVarName[1]);

      if not LIsFrame then
      begin
        WLine(Format('// vcl.Application.CreateForm(&%s)', [LFormName]));
        WLine;
      end;
      // 添加一个默认构建的，不使用Application.CreateForm
      WLine(Format('func New%s(owner vcl.IComponent) (root *T%s)  {',
        [LFormName, LFormName]));
      if not LIsFrame then
        WLine(Format('    vcl.CreateResForm(owner, &root)', []))
      else
        WLine(Format('    vcl.CreateResFrame(owner, &root)', []));
      WLine('    return');
      WLine('}');
      WLine('');


      LBuffer.WriteString(Format('var %s = []byte("', [LVarName]));
      for I := 0 to AMem.Size - 1 do
      begin
        LBuffer.WriteString('\x');
        LBuffer.WriteString(PByte(PByte(AMem.Memory) + I)^.ToHexString(2));
      end;
      LBuffer.WriteString('")');
      WLine(LBuffer.DataString);

      WLine('');
      WLine('// ' + rsRegisterFormResources);
      if LIsFrame then
        WLine(Format('var _ = vcl.RegisterFormResource(T%s{}, &%s)',
          [LFormName, LVarName]))
      else
        WLine(Format('var _ = vcl.RegisterFormResource(%s, &%s)',
          [LFormName, LVarName]));
    end;
    LStrStream.WriteString(LLines.Text);
    LStrStream.SaveToFile(AFileName + '.go');
  finally
    LLines.Free;
    LBuffer.Free;
    LStrStream.Free;
  end;
  // 一定创建，因为多加了个
  CreateImplFile(AFileName, AEvents, LFormName);
end;

procedure TGoLang.ConvertResource(const AResFileName, APath: string);
const
  PlatformStr: array[Boolean] of string = ('pe-x86-64', 'pe-i386');

var
  LWindResFileName: string;

  function GetCmdLine(AOutFileName: string; AIsAmd64: Boolean): string;
  begin
    Result := Format('%s -i "%s" -J res -o "%s%s" -F %s', [LWindResFileName, AResFileName, APath, AOutFileName, PlatformStr[AIsAmd64]]);
  end;

begin
  if not FileExists(AResFileName) then
    Exit;
  LWindResFileName := WindResFileName;
  if FileExists(LWindResFileName) then
    LWindResFileName := '"' + LWindResFileName + '"'
  else
    LWindResFileName := 'windres';
  ExecuteCommand([GetCmdLine('defaultRes_windows_386.syso', False), GetCmdLine('defaultRes_windows_amd64.syso', True)], True);

      //olRust, olNim:
      //  if (OutLang = olNim) or ((OutLang = olRust) and (IsGNU)) then
      //     ExecuteCommand([GetCmdLine('appres_386.o', False), GetCmdLine('appres_amd64.o', True)], True);
end;

procedure TGoLang.CreateImplFile(AFileName: string; AEvents: array of TEventItem; AFormName: string);
var
  LMName, LTemp, LCode, LPrivateName, LFlags: string;
  LItem: TEventItem;
  LStream: TStringStream;
  LExists, LB: boolean;
  LListStr: TStringList;
  I: integer;
begin
  AFileName += 'Impl.go';
  LStream := TStringStream.Create('');
  try
    LExists := FileExists(AFileName);
    LListStr := TStringList.Create;
    try
      // 不存在，则添加
      if not LExists then
      begin
        LListStr.Add('');
        LListStr.Add('package ' + PackageName);
        LListStr.Add('');
        if Length(AEvents) > 0 then
        begin
          LListStr.Add('import (');
          LListStr.Add('    "github.com/ying32/govcl/vcl"');
          LListStr.Add(')');
        end;
      end
      else
      begin
        // 反之加载
        LStream.LoadFromFile(AFileName);
        LTemp := LStream.DataString;
        LListStr.Text := LTemp;

        // 有事件时检查下有没有添加govcl包
        if Length(AEvents) > 0 then
        begin
          if Pos('import', LTemp) = 0 then
          begin
            I := 0;
            while I < LListStr.Count do
            begin
              if Trim(LListStr[I]).StartsWith('package') then
              begin
                Inc(I);
                LListStr.Insert(I, ')');
                LListStr.Insert(I, '    "github.com/ying32/govcl/vcl"');
                LListStr.Insert(I, 'import (');
                LListStr.Insert(I, '');
                Break;
              end;
              Inc(I);
            end;
          end;
        end;
      end;


      // 添加事件
      for LItem in AEvents do
      begin
        LMName := Format('On%s', [LItem.EventName]);
        //CtlWriteln('method name: %s', [LMName]);
        LCode := Format(#13#10'func (f *T%s) %s(%s) {'#13#10#13#10'}'#13#10,
          [AFormName, LMName, LItem.EventParams]);
        // 不存在不查找了
        if not LExists then
        begin
          if Pos(LMName, LListStr.Text) = 0 then
            LListStr.Add(LCode);
        end else
        begin
          // 没有找到，则添加
          if Pos(LMName, LListStr.Text) = 0 then
            LListStr.Add(LCode);
        end;
      end;

      // 检查私有变量结构是否存在
      LPrivateName := Format(PrivateFiledsStr, [AFormName]);
      // 不存在，则添加
      if Pos(PrivateFiledsFlagStr, LListStr.Text) = 0 then
      begin
        I := 0;
        while I < LListStr.Count do
        begin
          // 在首个func前几行插入
          LFlags := 'import';
          LB := (not LExists) and (Length(AEvents) = 0);
          if LB then
            LFlags := 'package';
          if Trim(LListStr[I]).StartsWith(LFlags) then
          begin
            if not LB then
            begin
              repeat
                Inc(I);
              until Trim(LListStr[I]).StartsWith(')');
            end;
            Inc(I);
            LListStr.Insert(I, '');
            LListStr.Insert(I, '}');
            LListStr.Insert(I, 'type ' + LPrivateName + ' struct {');
            LListStr.Insert(I, PrivateFiledsFlagStr);
            LListStr.Insert(I, '');
            Break;
          end;
          Inc(I);
        end;
      end
      else
      begin
        // 如果存在，则更新，因为防止把窗口名称改了，这里同步更新
        for I := 0 to LListStr.Count - 1 do
        begin
          // 在首个func前几行插入
          if Trim(LListStr[I]).CompareTo(PrivateFiledsFlagStr) = 0 then
          begin
            LListStr[I + 1] := 'type ' + LPrivateName + ' struct {';
            Break;
          end;
        end;
      end;

      // 这里是不是还得处理下，将窗口名称做一次替换
      //f *TFrmMain
      LStream.Clear;
      LStream.WriteString(LListStr.Text);
    finally
      LListStr.Free;
    end;
    LStream.SaveToFile(AFileName);
  finally
    LStream.Free;
  end;
end;

procedure TGoLang.CreateNewMain(AStrs: TStrings; AParam: TProjParam);
var
  LForms: TAutoCreateForms;
  LS: string;
begin
  with AStrs do
  begin
    Add('// ' + rsAutomaticallyGeneratedByTheRes2go);
    Add('package main');  // main.go文件始终都必须为main
    Add('');
    Add('import (');
    Add('    "github.com/ying32/govcl/vcl"');
    // winappres
    if AParam.UseDefaultWinAppRes then
      Add('    _ "github.com/ying32/govcl/pkgs/winappres"');

    // 初始添加一个本地导入包的
    // 还要判断当前目标目录在GOPATH中？？？不然要应用不同的规则
    if not IsMainPackage then
      Add('    "%s"', [GetPackageImportPath(AParam.OutPath)]);


    Add(')');
    Add('');
    Add('func main() {');

    // scaled
    if AParam.UseScaled then
      Add('    vcl.Application.SetScaled(true)');

    // title
    if AParam.Title <> '' then
      Add('    vcl.Application.SetTitle("%s")', [AParam.Title]);

    Add('    vcl.Application.Initialize()');
    Add('    vcl.Application.SetMainFormOnTaskBar(true)');

    // forms
    LForms := Self.GetAutoCreateForms;
    for LS in LForms do
    begin
      Add('    vcl.Application.CreateForm(&%s%s)', [GetPrefixPackage, LS]);
    end;

    Add('    vcl.Application.Run()');
    Add('}');
  end;
end;

function TGoLang.GetPrefixPackage: string;
begin
  Result := '';
  if not IsMainPackage then
    Result := PackageName + '.';
end;

function TGoLang.GetPackageImportPath(const AOutPath: string): string;
//var
//  LGoPaths, LPath: string;
//  LPaths: array of string;
//  LP: Integer;
begin
  Result := '';
  if IsMainPackage then
    Exit;
  //LGoPaths := GetEnvironmentVariable('GOPATH');
  //if not LGoPaths.IsEmpty then
  //begin
  //  LPaths := LGoPaths.Split([';']);
  //  for LPath in LPaths do
  //  begin
  //    if CompareFilenameStarts() = 0 then
  //    begin
  //      Exit('');
  //    end;
  //  end;
  //end;
  Result := './' + PackageName;
end;

procedure TGoLang.AddOrRemoveImports(ALists: TStrings; AAdds,
  ARemoves: array of string; AUIPackageName: string);
const
  Keywords: array[0..3] of string = ('var', 'const', 'type', 'func');

type
  TImportItem = record
    Path: string;
    OrigPath: string;
    &Single: Boolean;
    LineNumber: Integer;
  end;

var

  I: Integer;
  LS: string;
  LIsEnd: Boolean;
  LImports: array of TImportItem;
  LPkgLineNumber: Integer; // package
  LInsertStartIndex: Integer;
  LIsSingle: Boolean;

  procedure UpdateLineNumber(AStart: Integer; AValue: Integer);
  var
    J: Integer;
  begin
    for J := AStart to High(LImports) do
      LImports[J].LineNumber += AValue;
  end;

  function GetRealImportPath(const APath: string): string;
  var
    LP1, LP2: Integer;
  begin
    LP1 := Pos('"', APath);
    if LP1 > 0 then
    begin
      LP2 := Pos('"', APath, LP1 + 1);
      if LP2 > 0 then
        Exit(Copy(APath, LP1+1, LP2 - LP1 - 1));
    end;
    Result := APath;
  end;

  procedure AddImportItem(APath: string; ASingle: Boolean);
  begin
    SetLength(LImports, Length(LImports) + 1);
    with LImports[High(LImports)] do
    begin
      OrigPath := APath;
      Path := GetRealImportPath(APath);
      &Single := ASingle;
      LineNumber := I;
    end;
  end;

  function LastItem: TImportItem;
  begin
    Result := LImports[High(LImports)];
  end;

  function IndexPkgNameOf(APath: string): Integer;
  var
    J: Integer;
  begin
    Result := -1;
    for J := 0 to High(LImports) do
      if LImports[J].Path = GetRealImportPath(APath) then
        Exit(J);
  end;

  function IndexUIPkgNameOf: Integer;
  var
    J: Integer;
  begin
    Result := -1;
    for J := 0 to High(LImports) do
      if LImports[J].Path.EndsWith('/'+ AUIPackageName) then
        Exit(J);
  end;

  function InKeyWords: Boolean;
  var
    LKey: string;
  begin
    Result := False;
    for LKey in Keywords do
    begin
      if LS.StartsWith(LKey) then
        Exit(True);
    end;
  end;

  function LineStr: string;
  begin
    Result := ALists[I].Trim;
  end;

  procedure CheckComment;
  begin
    if LS.StartsWith('/*') then
    begin
      repeat
        Inc(I);
        LS := LineStr;
      until LS.StartsWith('*/') or LS.EndsWith('*/') or (I >= ALists.Count-1);
      Inc(I);
      LS := LineStr;
    end;
  end;

begin

  I := 0;
  while I < ALists.Count do
  begin
    LS := LineStr;
    CheckComment;
    if LS.StartsWith('package') then
      LPkgLineNumber := I
    else
    if LS.StartsWith('import') then
    begin
      if LS.IndexOf('(') >= 6 then
      begin
        repeat
          Inc(I);
          LS := LineStr;
          CheckComment;
          LIsEnd := LS.StartsWith(')');
          if (not LIsEnd) and (not LS.IsEmpty) then
            AddImportItem(LS, False);
        until LIsEnd or (I >= ALists.Count-1);
      end else
      begin
        AddImportItem(Copy(LS, 7, Length(LS)-6).Trim, True);
      end;
    end;
    if InKeyWords then
      Break;
    Inc(I);
  end;

  // 添加测试
  if (Length(ARemoves) > 0) and (Length(LImports) > 0) then
  begin
    // 删除
    for LS in ARemoves do
    begin
      I := IndexPkgNameOf(LS);
      if I <> -1 then
      begin
        ALists.Delete(LImports[I].LineNumber);
        UpdateLineNumber(I, -1);
      end;
    end;
  end;

  if Length(AAdds) > 0 then
  begin
    if Length(LImports) > 0 then
    begin
      with LastItem do
      begin
        LInsertStartIndex := LineNumber;
        LIsSingle := &Single;
      end;
    end else
    begin
      LInsertStartIndex := LPkgLineNumber + 2;
      ALists.Insert(LInsertStartIndex, ')');
      ALists.Insert(LInsertStartIndex, 'import (');
      LIsSingle := False;
    end;
    // 添加
    for LS in AAdds do
    begin
      if IndexPkgNameOf(LS) = -1 then
      begin
        if LIsSingle then
          ALists.Insert(LInsertStartIndex + 1, 'import ' + LS)
        else
          ALists.Insert(LInsertStartIndex + 1, '    ' + LS);
        UpdateLineNumber(I, 1);
      end;
    end;
  end;

  // 独立检查的
  if (AUIPackageName <> '') and (AUIPackageName <> 'main') and (Length(LImports) > 0) then
  begin
    I := IndexUIPkgNameOf;
    if I = -1 then
    begin
      with LastItem do
      begin
        LS := '"./' + AUIPackageName + '"';
        if &Single then
          ALists.Insert(LineNumber + 1, 'import ' + LS)
        else
          ALists.Insert(LineNumber + 1, '    ' + LS);
      end;
    end;
  end;
end;

procedure TGoLang.ProcessMainFunc(ALists: TStrings; ATitle: string;
  AUseScaled: Boolean; AForms: array of string);
type
  TAppItem = record
    Name: string;
    UsePkgName: Boolean;
    LineNumber: Integer;
  end;

var
  I, N: Integer;
  LS: string;
  LUsePkgName: Boolean;
  LLineArr: array of string;
  LApps: array of TAppItem;

  procedure AddItem(AName: string);
  begin
    SetLength(LApps, Length(LApps) + 1);
    with LApps[High(LApps)] do
    begin
      Name := AName;
      LineNumber := I;
    end;
  end;

  procedure UpdateLineNumber(AStart: Integer; AValue: Integer);
  var
    J: Integer;
  begin
    for J := AStart to High(LApps) do
      LApps[J].LineNumber += AValue;
  end;

  function LineStr: string;
  begin
    Result := ALists[I].Trim;
  end;

  procedure CheckComment;
  begin
    if LS.StartsWith('/*') then
    begin
      repeat
        Inc(I);
        LS := LineStr;
      until LS.StartsWith('*/') or LS.EndsWith('*/') or (I >= ALists.Count-1);
      Inc(I);
      LS := LineStr;
    end;
  end;

  function IndexNameOf(const AName: string): Integer;
  var
    J: Integer;
  begin
    Result := -1;
    for J := 0 to High(LApps) do
    begin
      if SameText(LApps[J].Name, AName) then
        Exit(J);
    end;
  end;

  function RemoveAllCreateForm: Integer;
  var
    J: Integer;
  begin
    Result := -1;
    for J := 0 to High(LApps) do
    begin
      if SameText(LApps[J].Name, 'CreateForm') then
      begin
        ALists.Delete(LApps[J].LineNumber);
        UpdateLineNumber(J, -1);
        if Result = -1 then
          Result := LApps[J].LineNumber + 1;
      end;
    end;
  end;

  function GetPkgName: string;
  begin
    Result := '';
    if LUsePkgName then
      Result := 'vcl.';
  end;

begin
  I := 0;
  while I < ALists.Count do
  begin
    LS := LineStr;
    if LS.StartsWith('func main()') then
    begin
      Inc(I);
      while I < ALists.Count do
      begin
        LS := LineStr;
        CheckComment;
        LLineArr := LS.Split(['.', '(']);
        if Length(LLineArr) >= 2 then
        begin
          if LLineArr[0].Equals('vcl') and LLineArr[1].Equals('Application') then
          begin
            if Length(LLineArr) >= 3 then
            begin
              AddItem(LLineArr[2].Trim);
              LUsePkgName := True;
            end;
          end
          else if LLineArr[0] = 'Application' then
          begin
            AddItem(LLineArr[1].Trim);
            LUsePkgName := False;
          end;
        end;
        Inc(I);
      end;
      Break;
    end;
    Inc(I);
  end;


  // SetTitle
  I := IndexNameOf('SetTitle');
  if ATitle = '' then
  begin
    if I <> -1 then
    begin
      ALists.Delete(LApps[I].LineNumber);
      UpdateLineNumber(I, -1);
    end;
  end else
  begin
    LS := Format('    %sApplication.SetTitle("%s")', [GetPkgName, ATitle]);
    if I = -1 then
    begin
      I := IndexNameOf('Initialize');
      ALists.Insert(LApps[I].LineNumber, LS);
      UpdateLineNumber(I, 1);
    end else
      ALists[LApps[I].LineNumber] := LS;
  end;


  // SetScaled
  I := IndexNameOf('SetScaled');
  LS := Format('    %sApplication.SetScaled(%s)', [GetPkgName, BoolToStr(AuseScaled, True).ToLower]);
  if I = -1 then
  begin
    I := IndexNameOf('Initialize');
    ALists.Insert(LApps[I].LineNumber, LS);
    UpdateLineNumber(I, 1);
  end else
    ALists[LApps[I].LineNumber] := LS;

  // Remove All
  N := RemoveAllCreateForm;

  // Add Forms
  for I := High(AForms) downto 0 do
    ALists.Insert(N, Format('    %sApplication.CreateForm(&%s)', [GetPkgName, AForms[I]]));

end;

function TGoLang.GetOS(ATargetOS: string): string;
begin
  Result := '';
  // GOOS
  if SameText(ATargetOS, 'Darwin') or SameText(ATargetOS, 'MacOS') then
    Result := 'darwin'
  else if SameText(ATargetOS, 'Win32') or SameText(ATargetOS, 'Win64') then
    Result := 'windows'
  else if SameTExt(ATargetOS, 'Linux') then
    Result := 'linux';
end;

function TGoLang.GetARCH(ATargetCPU: string): string;
begin
  Result := '';
  // GOARCh
  if SameText(ATargetCPU, 'arm') then
    Result := 'arm'
  else if SameText(ATargetCPU, 'i386') then
    Result := '386'
  else if SameText(ATargetCPU, 'x86_64') then
    Result := 'amd64';
end;



procedure TGoLang.InitTypeLists;
begin
  with FTypeLists do
  begin
    Add('TObject', 'IObject');
    Add('Boolean', 'bool');
    Add('Integer', 'int32');
    Add('Word', 'uint16');
    Add('LongInt', 'int32');
    Add('TCustomTreeView', 'TTreeView');
    Add('TCustomListView', 'TListView');
    Add('TConstraintSize', 'int32');
    Add('AnsiString', 'string');
    Add('Int64', 'int64');
    Add('UInt64', 'uint64');
    Add('WideString', 'string');
    Add('Pointer', 'uintptr');
  end;
end;

procedure TGoLang.InitBaseTypes;
begin
  with FBaseTypes do
  begin
    Add('bool');
    Add('int32');
    Add('uint16');
    Add('uint');
    Add('int');
    Add('int16');
    Add('byte');
    Add('uint8');
    Add('uint32');
    Add('int8');
    Add('string');
    Add('int64');
    Add('uint64');
    Add('uintptr');
  end;
end;

function TGoLang.GetResFileExists: Boolean;
begin
  Result:= FileExists('') or FileExists('');
end;

function TGoLang.GetPackageName: string;
begin
  Result:=inherited GetPackageName;
  if Result = '' then
    Result := 'main';
end;

function TGoLang.ParamTypeCov(ASrc: string): string;
var
  LV: string;
begin
  Result := ASrc;
  if FTypeLists.TryGetData(ASrc, LV) then
    Result := LV;
end;

function TGoLang.IsBaseType(AType: string): Boolean;
begin
  Result := FBaseTypes.IndexOf(AType) <> -1;
end;

function TGoLang.IsInterfaceType(AType: string): Boolean;
begin
  Result := AType.Equals('IObject') or AType.Equals('IComponent') or AType.Equals('IControl') or AType.Equals('IWinControl');
end;


initialization
  GoLang := TGoLang.Create;

finalization
  FreeAndNil(GoLang);

end.

