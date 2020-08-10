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
  Classes, SysUtils, StrUtils, Forms, TypInfo, uSupports, IDEExternToolIntf, res2goResources, uLangBase;

type

  { TGoLang }

  TGoLang = class(TLangBase)
  private
    FPkgName: string;
    function ParamTypeCov(ASrc: string): string;
    function IsBaseType(AType: string): Boolean;
    function IsInterfaceType(AType: string): Boolean;
    procedure CreateImplFile(AFileName: string; AEvents: array of TEventItem; AFormName: string);
  protected
    procedure InitTypeLists; override;
    procedure InitBaseTypes; override;
  public
    constructor Create;
    procedure ConvertProjectFile(const AFileName, AOutPath: string); override;
    function ToEventString(AProp: PPropInfo): string; override;
    procedure SaveToFile(AFileName: string; ARoot: TComponent; AEvents: array of TEventItem; AMem: TMemoryStream); override;
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
  FPkgName := 'main';
end;

procedure TGoLang.ConvertProjectFile(const AFileName, AOutPath: string);
var
  LStrs, LMainDotGo: TStringList;
  S, LVarName, LFormName, LSaveFileName: string;
  LP: integer;
  LFile: TStringStream;
  LMainFileExists: boolean;
  LForms: array of string;
  LIndex, I: integer;
  LPkg: string;
  LProjFile: TFileStream;
begin
  LStrs := TStringList.Create;
  LMainDotGo := TStringList.Create;
  try
    LSaveFileName := AOutPath + 'main.go';
    LMainFileExists := FileExists(LSaveFileName);
    LProjFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      LProjFile.Position := 0;
      LStrs.LoadFromStream(LProjFile);
    finally
      LProjFile.Free;
    end;
    // 如果不存在 main.go文件，则新建一个
    if not LMainFileExists then
    begin
      LMainDotGo.Add('// ' + rsAutomaticallyGeneratedByTheRes2go);
      LMainDotGo.Add('package main');
      LMainDotGo.Add('');
      LMainDotGo.Add('import (');
      LMainDotGo.Add('    "github.com/ying32/govcl/vcl"');
      // winappres
      // 总是添加此包
        LMainDotGo.Add('    _ "github.com/ying32/govcl/pkgs/winappres"');
      LMainDotGo.Add(')');
      LMainDotGo.Add('');
      LMainDotGo.Add('func main() {');
      LMainDotGo.Add('');
      LMainDotGo.Add('    vcl.Application.Initialize()');
      if SameText(ExtractFileExt(AFileName), '.dpr') then
        LMainDotGo.Add('    vcl.Application.SetMainFormOnTaskBar(true)');
    end
    else
      // 存在则加载此文件
      LMainDotGo.LoadFromFile(LSaveFileName);

    // 如果不是main包，则输出的需要加上包名

    for S in LStrs do
    begin
      // 开始提取 Application.CreateForm的
      if S.Trim.StartsWith('Application.CreateForm(') then
      begin
        LP := S.IndexOf(',');
        LFormName := Trim(S.Substring(LP + 1, S.IndexOf(')') - LP - 1));
        LVarName := LFormName + 'Bytes';

        LVarName[1] := LowerCase(LVarName[1]);
        SetLength(LForms, Length(LForms) + 1);
        LForms[High(LForms)] :=
          Format('    vcl.Application.CreateForm(&%s)', [{LPkg+LVarName, }LFormName]);
        // main.go文件不存在则直接添加
        if not LMainFileExists then
          LMainDotGo.Add(LForms[High(LForms)]);
      end;
    end;

    // main.go文件存在的处理方式
    if LMainFileExists then
    begin
      LIndex := -1;
      for I := LMainDotGo.Count - 1 downto 0 do
      begin
        // 查找并移除现有的
        if LMainDotGo[I].Trim.StartsWith('vcl.Application.CreateForm(') then
          LMainDotGo.Delete(I);
      end;

      for I := 0 to LMainDotGo.Count - 1 do
      begin
        // 找初始语句
        if LMainDotGo[I].Trim.StartsWith('vcl.Application.Initialize') then
        begin
          // 找到了则 I+1为插入起始行
          LIndex := I + 1;
          // 判断下一行是不是 vcl.Application.SetMainFormOnTaskBar
          if LMainDotGo[I + 1].Trim.StartsWith('vcl.Application.SetMainFormOnTaskBar') then
            Inc(LIndex);
          Break;
        end;
      end;

      // 将前面找到的附加进去
      if LIndex <> -1 then
      begin
        for I := High(LForms) downto 0 do
          LMainDotGo.Insert(LIndex, LForms[I]);
      end;
    end;

    if not LMainFileExists then
    begin
      LMainDotGo.Add('    vcl.Application.Run()');
      LMainDotGo.Add('}');
    end;

    LFile := TStringStream.Create('');
    try
      LFile.WriteString(LMainDotGo.Text);
      LFile.SaveToFile(LSaveFileName);
    finally
      LFile.Free;
    end;
  finally
    LMainDotGo.Free;
    LStrs.Free;
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
  LSCPkgName: string;
  LIsFrame: boolean;
begin
  LStrStream := TStringStream.Create('');
  LBuffer := TStringStream.Create('');
  LLines := TStringList.Create;
  try
    WLine('// ' + rsAutomaticallyGeneratedByTheRes2goDoNotEdit);
    WLine('package ' + FPkgName);
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
      if FPkgName = 'main' then
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
        LListStr.Add('package ' + FPkgName);
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

