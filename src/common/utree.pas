{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, uCommon;

type

  { ITreeNode }

  ITreeNode = interface(IUnknown)
    ['{4DE3A81A-AF7A-4B6D-84D5-C76B961405A5}']
    function AddNode(AName: string): ITreeNode;
    function GetID: QWord;
    function GetName: string;
    function GetParent: ITreeNode;
    function GetNodesCount: integer;
    function GetNode(Index: integer): ITreeNode;
    function GetNodeByName(AName: string): ITreeNode;
    function GetNodeIndex(AName: string): integer;
    function GetParamsCount: integer;
    function GetParamName(Index: integer): string;
    function GetParamValue(Index: integer): olevariant;
    function GetParamValueByName(AName: string): olevariant;
    function GetParamIndex(AName: string): integer;
    function NodeExists(AName: string): boolean;
    function ParamExists(AName: string): boolean;
    function UpdateID(const OldIndex, NewIndex: QWord): Boolean;
    procedure Assign(const Node: ITreeNode);
    procedure AddParam(AName: string; const AValue: olevariant);
    procedure ClearParams;
    procedure Clear;
    procedure DeleteParam(Index: integer); overload;
    procedure DeleteParam(AName: string); overload;
    procedure DeleteNode(Index: integer); overload;
    procedure DeleteNode(AName: string); overload;
    procedure DeleteNode(Node: ITreeNode); overload;
    procedure ClearNodes;
    procedure SetID(const Value: QWord);
    procedure SetName(const Value: string);
    procedure SetParamValue(Index: integer; const Value: olevariant);
    procedure SetParamValueByName(AName: string; const Value: olevariant);
    property Name: string read GetName write SetName;
    property ID: QWord read GetID write SetID;
    property Parent: ITreeNode read GetParent;
    property NodesCount: integer read GetNodesCount;
    property Nodes[Index: integer]: ITreeNode read GetNode;
    property NodeByName[AName: string]: ITreeNode read GetNodeByName;
    property NodeIndex[AName: string]: integer read GetNodeIndex;
    property ParamsCount: integer read GetParamsCount;
    property ParamName[Index: integer]: string read GetParamName;
    property ParamValue[Index: integer]: olevariant
      read GetParamValue write SetParamValue;
    property ParamValueByName[AName: string]: olevariant
      read GetParamValueByName write SetParamValueByName;
    property ParamIndex[AName: string]: integer read GetParamIndex;
  end;

  ICustomSerialize = interface(IInterface)
    ['{C34F1F79-138C-4BE4-BEC2-5ADCB8E1D6DC}']
    procedure CustomSerialize(const Node: ITreeNode);
    procedure CustomDeSerialize(const Node: ITreeNode);
  end;

  TICustomTreeNode = class(TInterfacedObject)
  protected
    function GetNodeByName(AName: string): ITreeNode; virtual;
    function GetNodeIndex(AName: string): integer;
    function GetParamIndex(AName: string): integer;
    function GetParamValueByName(AName: string): olevariant; virtual;
    procedure SetParamValueByName(AName: string; const Value: olevariant);
      virtual;
    function GetID: QWord; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetNode(Index: integer): ITreeNode; virtual; abstract;
    function GetNodesCount: integer; virtual; abstract;
    function GetParamName(Index: integer): string; virtual; abstract;
    function GetParamsCount: integer; virtual; abstract;
    function GetParamValue(Index: integer): olevariant; virtual; abstract;
    function GetParent: ITreeNode; virtual; abstract;
    procedure SetName(const Value: string); virtual; abstract;
    procedure SetID(const Value: QWord); virtual; abstract;
    procedure SetParamValue(Index: integer; const Value: olevariant);
      virtual; abstract;
  public
    function AddNode(AName: string): ITreeNode; virtual; abstract;
    function NodeExists(AName: string): boolean;
    function ParamExists(AName: string): boolean;
    function UpdateID(const OldIndex, NewIndex: QWord): Boolean; virtual; abstract;
    procedure DeleteNode(Index: integer); overload; virtual; abstract;
    procedure DeleteNode(AName: string); overload;
    procedure DeleteNode(Node: ITreeNode); overload;
    procedure ClearNodes;
    procedure Assign(const Node: ITreeNode);
    procedure AddParam(AName: string; const AValue: olevariant);
      virtual; abstract;
    procedure DeleteParam(Index: integer); overload; virtual; abstract;
    procedure DeleteParam(AName: string); overload;
    procedure ClearParams;
    procedure Clear;
    property Name: string read GetName write SetName;
    property ID: QWord read GetID write SetID;
    property Parent: ITreeNode read GetParent;
    property NodesCount: integer read GetNodesCount;
    property Nodes[Index: integer]: ITreeNode read GetNode;
    property NodeByName[AName: string]: ITreeNode read GetNodeByName;
    property NodeIndex[AName: string]: integer read GetNodeIndex;
    property ParamsCount: integer read GetParamsCount;
    property ParamName[Index: integer]: string read GetParamName;
    property ParamValue[Index: integer]: olevariant
      read GetParamValue write SetParamValue;
    property ParamValueByName[AName: string]: olevariant
      read GetParamValueByName write SetParamValueByName;
    property ParamIndex[AName: string]: integer read GetParamIndex;
  end;

  { TITreeNode }

  TITreeNode = class(TICustomTreeNode, ITreeNode)
  private
    fNodesList: TList;
    fParamsList: TList;
    fName: string;
    fID: QWord;
    fParent: TITreeNode;
  protected
    function GetID: QWord; override;
    function GetName: string; override;
    function GetNode(Index: integer): ITreeNode; override;
    function GetNodesCount: integer; override;
    function GetParamName(Index: integer): string; override;
    function GetParamsCount: integer; override;
    function GetParamValue(Index: integer): olevariant; override;
    function GetParent: ITreeNode; override;
    procedure SetName(const Value: string); override;
    procedure SetID(const Value: QWord); override;
    procedure SetParamValue(Index: integer; const Value: olevariant); override;
  public
    constructor Create(AName: string); virtual;
    destructor Destroy; override;
    function AddNode(AName: string): ITreeNode; override;
    function UpdateID(const OldIndex, NewIndex: QWord): Boolean; override;
    procedure DeleteNode(Index: integer); override;
    procedure AddParam(AName: string; const AValue: olevariant); override;
    procedure DeleteParam(Index: integer); override;
  end;

  TITreeNodeClass = class of TITreeNode;

  PNodeRec = ^TNodeRec;

  TNodeRec = record
    Node: ITreeNode;
  end;

  PParamRec = ^TParamRec;

  TParamRec = record
    Name: string;
    Value: olevariant;
  end;

  { TISerializer }

  TISerializer = class
  public
    class procedure SaveToNode(AObject: TObject; const Node: ITreeNode); virtual;
    class procedure LoadFromNode(AObject: TObject; const Node: ITreeNode); virtual;
    class procedure SaveToStream(const Node: ITreeNode; Stream: TStream);
      overload; virtual; abstract;
    class procedure LoadFromStream(const Node: ITreeNode; Stream: TStream);
      overload; virtual; abstract;
    class procedure SaveToStream(AObject: TObject; Stream: TStream); overload;
    class procedure LoadFromStream(AObject: TObject; Stream: TStream); overload;
    class procedure SaveToFile(const Node: ITreeNode; FileName: string); overload;
    class procedure LoadFromFile(const Node: ITreeNode; FileName: string); overload;
    class procedure SaveToFile(AObject: TObject; FileName: string); overload;
    class procedure LoadFromFile(AObject: TObject; FileName: string); overload;
  end;

  TISerializerClass = class of TISerializer;

  { TIBinarySerializer }

  TIBinarySerializer = class(TISerializer)
  public
    class procedure SaveToStream(const Node: ITreeNode; Stream: TStream); override;
    class procedure LoadFromStream(const Node: ITreeNode; Stream: TStream); override;
  end;

  { TIDocumentSerializer }

  TIDocumentSerializer = class(TISerializer)
  public
    class procedure SaveToNode(AObject: TObject; const Node: ITreeNode); override;
    class procedure LoadFromNode(AObject: TObject; const Node: ITreeNode); override;
  end;


implementation

uses uIObject;

type

  TIFileHeader = packed record
    ID: cardinal;
    Version: word;
  end;

  IVarStreamable = interface
    ['{D60BA026-5E42-4C2A-BB01-3F1C8F30A28E}']
    procedure StreamIn(var Dest: TVarData; const Stream: TStream);
    procedure StreamOut(const Source: TVarData; const Stream: TStream);
  end;

const
  FileID: cardinal = 876892233; // 'IPD4'
  varByteArray = varByte or varArray;


procedure WriteVariant(Stream: TStream; const Value: variant);
var
  WStr: WideString;
  Str: string;
  B: wordbool;
  Size: longint;
  Data: PByteArray;
  CustomType: TCustomVariantType;
  VarStreamer: IVarStreamable;
begin
  Stream.WriteBuffer(TVarData(Value).VType, SizeOf(word));
  case TVarData(Value).VType of
    varSmallint: Stream.WriteBuffer(TVarData(Value).VSmallInt, SizeOf(smallint));
    varInteger: Stream.WriteBuffer(TVarData(Value).VInteger, SizeOf(longint));
    varSingle: Stream.WriteBuffer(TVarData(Value).VSingle, SizeOf(single));
    varDouble: Stream.WriteBuffer(TVarData(Value).VDouble, SizeOf(double));
    varCurrency: Stream.WriteBuffer(TVarData(Value).VCurrency, SizeOf(currency));
    varDate: Stream.WriteBuffer(TVarData(Value).VDate, SizeOf(TDateTime));
    varOleStr:
    begin
      WStr := Value;
      Size := Length(WStr);
      Stream.WriteBuffer(Size, SizeOf(integer));
      Stream.WriteBuffer(Pointer(WStr)^, Size * SizeOf(widechar));
    end;
    varBoolean:
    begin
      B := TVarData(Value).VBoolean;
      Stream.WriteBuffer(B, SizeOf(wordbool));
    end;
    varShortInt: Stream.WriteBuffer(TVarData(Value).VShortInt, SizeOf(shortint));
    varByte: Stream.WriteBuffer(TVarData(Value).VByte, SizeOf(byte));
    varWord: Stream.WriteBuffer(TVarData(Value).VWord, SizeOf(word));
    varLongWord: Stream.WriteBuffer(TVarData(Value).VLongWord, SizeOf(longword));
    varInt64: Stream.WriteBuffer(TVarData(Value).VInt64, SizeOf(int64));
    varString:
    begin
      Str := Value;
      Stream.WriteAnsiString(Str);
    end;
    varByteArray:
    begin
      Size := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;
      Data := VarArrayLock(Value);
      try
        Stream.WriteBuffer(Size, SizeOf(longint));
        Stream.WriteBuffer(Data^, Size);
      finally
        VarArrayUnlock(Value);
      end;
    end
    else
      try
        if FindCustomVariantType(TVarData(Value).VType, CustomType) and
          Supports(Value, IVarStreamable, VarStreamer) then
        begin
          VarStreamer.StreamOut(TVarData(Value), Stream);
        end
        else
        begin
          Str := Value;
          Size := Length(Str);
          Stream.WriteBuffer(Size, SizeOf(longint));
          Stream.WriteBuffer(Pointer(Str)^, Size * SizeOf(AnsiChar));
        end;
      except
        raise EIException.CreateRes(@SInvalidVType);
      end;
  end;
end;

procedure ReadVariant(Stream: TStream; out Value: variant);
var
  VType: word;
  IntVal: integer;
  SmlIntVal: smallint;
  SrtIntVal: shortint;
  ByteVal: byte;
  WordVal: word;
  DWordVal: longword;
  Int64Val: int64;
  SingleVal: single;
  DoubleVal: double;
  CurrencyVal: currency;
  DateVal: TDateTime;
  BoolVal: wordbool;
  WStr: WideString;
  Str: string;
  Source: variant;
  Size: longint;
  Data: PByteArray;
  CustomType: TCustomVariantType;
  VarStreamer: IVarStreamable;
begin
  VarClear(Value);
  Stream.ReadBuffer(VType, SizeOf(word));
  case VType of
    varSmallint:
    begin
      Stream.ReadBuffer(SmlIntVal, SizeOf(smallint));
      Value := SmlIntVal;
    end;
    varInteger:
    begin
      Stream.ReadBuffer(IntVal, SizeOf(longint));
      Value := IntVal;
    end;
    varSingle:
    begin
      Stream.ReadBuffer(SingleVal, SizeOf(single));
      Value := SingleVal;
    end;
    varDouble:
    begin
      Stream.ReadBuffer(DoubleVal, SizeOf(double));
      Value := DoubleVal;
    end;
    varCurrency:
    begin
      Stream.ReadBuffer(CurrencyVal, SizeOf(currency));
      Value := CurrencyVal;
    end;
    varDate:
    begin
      Stream.ReadBuffer(DateVal, SizeOf(TDateTime));
      Value := DateVal;
    end;
    varOleStr:
    begin
      Stream.ReadBuffer(IntVal, SizeOf(longint));
      SetLength(WStr, IntVal);
      Stream.ReadBuffer(Pointer(WStr)^, IntVal * SizeOf(widechar));
      Value := WStr;
    end;
    varBoolean:
    begin
      Stream.ReadBuffer(BoolVal, SizeOf(wordbool));
      Value := BoolVal;
    end;
    varShortint:
    begin
      Stream.ReadBuffer(SrtIntVal, SizeOf(shortint));
      Value := SrtIntVal;
    end;
    varByte:
    begin
      Stream.ReadBuffer(ByteVal, SizeOf(byte));
      Value := ByteVal;
    end;
    varWord:
    begin
      Stream.ReadBuffer(WordVal, SizeOf(word));
      Value := WordVal;
    end;
    varLongWord:
    begin
      Stream.ReadBuffer(DWordVal, SizeOf(longword));
      Value := DWordVal;
    end;
    varInt64:
    begin
      Stream.ReadBuffer(Int64Val, SizeOf(int64));
      Value := Int64Val;
    end;
    varString:
    begin
      Str := Stream.ReadAnsiString();
      Value := Str;
    end;
    varByteArray:
    begin
      Stream.ReadBuffer(Size, SizeOf(longint));
      Value := VarArrayCreate([0, Size - 1], varByte);
      Data := VarArrayLock(Value);
      try
        Stream.ReadBuffer(Data^, Size);
      finally
        VarArrayUnlock(Value);
      end;
    end
    else
      try
        if FindCustomVariantType(VType, CustomType) and Supports(
          Value, IVarStreamable, VarStreamer) then
        begin
          VarStreamer.StreamIn(TVarData(Value), Stream);
        end
        else
        begin
          Stream.ReadBuffer(IntVal, SizeOf(longint));
          SetLength(Str, IntVal);
          Stream.ReadBuffer(Pointer(Str)^, IntVal * SizeOf(AnsiChar));
          Source := Str;
          VarCast(Value, Source, VType);
        end;
      except
        raise EIException.CreateRes(@SInvalidVType);
      end;
  end;
end;


{ TICustomTreeNode }

procedure TICustomTreeNode.Assign(const Node: ITreeNode);
var
  i: integer;
  NewNode: ITreeNode;
begin
  Clear;
  for i := 0 to Node.ParamsCount - 1 do
    AddParam(Node.ParamName[i], Node.ParamValue[i]);
  for i := 0 to Node.NodesCount - 1 do
  begin
    NewNode := AddNode(Node.Nodes[i].Name);
    NewNode.Assign(Node.Nodes[i]);
  end;
end;

procedure TICustomTreeNode.Clear;
begin
  ClearNodes;
  ClearParams;
end;

procedure TICustomTreeNode.ClearNodes;
var
  i: integer;
begin
  for i := NodesCount - 1 downto 0 do
    DeleteNode(i);
end;

procedure TICustomTreeNode.ClearParams;
var
  i: integer;
begin
  for i := ParamsCount - 1 downto 0 do
    DeleteParam(i);
end;

procedure TICustomTreeNode.DeleteNode(Node: ITreeNode);
var
  i: integer;
begin
  for i := 0 to NodesCount - 1 do
    if Node = Nodes[i] then
    begin
      DeleteNode(i);
      exit;
    end;
  raise EIException.CreateResFmt(@SNodeNotFound, [Format('$%x', [Node])]);
end;

procedure TICustomTreeNode.DeleteNode(AName: string);
var
  Index: integer;
begin
  Index := NodeIndex[AName];
  if Index >= 0 then
    DeleteNode(Index)
  else
    raise EIException.CreateResFmt(@SNodeNotFound, [AName]);
end;

procedure TICustomTreeNode.DeleteParam(AName: string);
var
  Index: integer;
begin
  Index := ParamIndex[AName];
  if Index >= 0 then
    DeleteParam(Index)
  else
    raise EIException.CreateResFmt(@SParamNotFound, [AName]);
end;

function TICustomTreeNode.GetNodeByName(AName: string): ITreeNode;
var
  Index: integer;
begin
  Index := NodeIndex[AName];
  if Index > -1 then
    Result := Nodes[Index]
  else
    raise EIException.CreateResFmt(@SNodeNotFound, [AName]);
    // Z: Result := AddNode(AName);
end;

function TICustomTreeNode.GetNodeIndex(AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to NodesCount - 1 do
    if SameText(Nodes[i].Name, AName) then
    begin
      Result := i;
      Break;
    end;
end;

function TICustomTreeNode.GetParamIndex(AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to ParamsCount - 1 do
    if SameText(ParamName[i], AName) then
    begin
      Result := i;
      Break;
    end;
end;

function TICustomTreeNode.GetParamValueByName(AName: string): olevariant;
var
  Index: integer;
begin
  Index := ParamIndex[AName];
  if Index > -1 then
    Result := ParamValue[Index]
  else
    raise EIException.CreateResFmt(@SParamNotFound, [AName]);
end;

function TICustomTreeNode.NodeExists(AName: string): boolean;
begin
  Result := NodeIndex[AName] > -1;
end;

function TICustomTreeNode.ParamExists(AName: string): boolean;
begin
  Result := ParamIndex[AName] > -1;
end;

procedure TICustomTreeNode.SetParamValueByName(AName: string; const Value: olevariant);
var
  Index: integer;
begin
  Index := ParamIndex[AName];
  if Index > -1 then
    ParamValue[Index] := Value
  else
    AddParam(AName, Value);
end;


{ TITreeNode }

constructor TITreeNode.Create(AName: string);
begin
  inherited Create;
  fNodesList := TList.Create;
  fParamsList := TList.Create;
  fName := AName;
  fID := 0;
end;

destructor TITreeNode.Destroy;
begin
  ClearParams;
  fParamsList.Free;
  ClearNodes;
  fNodesList.Free;
  inherited Destroy;
end;

function TITreeNode.AddNode(AName: string): ITreeNode;
var
  NewNode: TITreeNode;
  P: PNodeRec;
begin
  NewNode := TITreeNodeClass(ClassType).Create(AName);
  NewNode.fParent := Self;
  New(P);
  P^.Node := NewNode;
  fNodesList.Add(P);
  Result := NewNode;
end;

procedure TITreeNode.AddParam(AName: string; const AValue: olevariant);
var
  P: PParamRec;
begin
  if not VarIsEmpty(AValue) then
  begin
    New(P);
    P^.Name := AName;
    P^.Value := AValue;
    fParamsList.Add(P);
  end;
end;

procedure TITreeNode.DeleteNode(Index: integer);
begin
  Dispose(PNodeRec(fNodesList[Index]));
  fNodesList.Delete(Index);
end;

procedure TITreeNode.DeleteParam(Index: integer);
begin
  Dispose(PParamRec(fParamsList[Index]));
  fParamsList.Delete(Index);
end;

function TITreeNode.UpdateID(const OldIndex, NewIndex: QWord): Boolean;
var
  i: integer;
  node: ITreeNode;
begin
  result := false;
  if fID = OldIndex then
  begin
    fID := NewIndex;
    result := true;
    exit;
  end;
  for i := 0 to NodesCount - 1 do
  begin
    node := GetNode(i);
    if (node.ID = 0) and (not SameText(node.Name, 'Childs')) then
      continue;
    result := node.UpdateID(OldIndex, NewIndex);
    if result then
      exit;
  end;
end;

function TITreeNode.GetID: QWord;
begin
  Result := fID;
end;

function TITreeNode.GetName: string;
begin
  Result := fName;
end;

function TITreeNode.GetNode(Index: integer): ITreeNode;
begin
  Result := PNodeRec(fNodesList[Index])^.Node;
end;

function TITreeNode.GetNodesCount: integer;
begin
  Result := fNodesList.Count;
end;

function TITreeNode.GetParamName(Index: integer): string;
begin
  Result := PParamRec(fParamsList[Index])^.Name;
end;

function TITreeNode.GetParamsCount: integer;
begin
  Result := fParamsList.Count;
end;

function TITreeNode.GetParamValue(Index: integer): olevariant;
begin
  Result := PParamRec(fParamsList[Index])^.Value;
end;

function TITreeNode.GetParent: ITreeNode;
begin
  Result := fParent;
end;

procedure TITreeNode.SetName(const Value: string);
begin
  fName := Value;
end;

procedure TITreeNode.SetID(const Value: QWord);
begin
  fID := Value;
end;

procedure TITreeNode.SetParamValue(Index: integer; const Value: olevariant);
begin
  if not VarIsEmpty(Value) then
    PParamRec(fParamsList[Index])^.Value := Value
  else
    DeleteParam(Index);
end;


{ TISerializer }

class procedure TISerializer.SaveToNode(AObject: TObject; const Node: ITreeNode);
var
  TypeData: PTypeData;
  Count, i: integer;
  PropList: PPropList;
  NewNode: ITreeNode;
  ObjProp: TObject;
  Value: variant;
  Intf: ICustomSerialize;
  NotifyIntf: ISerializeNotify;
begin
  if Supports(AObject, ISerializeNotify, NotifyIntf) then
    NotifyIntf.BeginSave;
  try
    Node.Clear;
    if AObject is TIObject then
      Node.ID := TIObject(AObject).ID;
    TypeData := GetTypeData(AObject.ClassInfo);
    Count := TypeData^.PropCount;
    if Count > 0 then
    begin
      GetMem(PropList, SizeOf(PPropInfo) * Count);
      try
        GetPropInfos(AObject.ClassInfo, PropList);
        for i := 0 to Count - 1 do
        begin
          if IsStoredProp(AObject, PropList^[i]) then
          begin
            case PropList^[i]^.PropType^.Kind of
              tkMethod, tkInterface: ;
              tkClass:
              begin
                ObjProp := GetObjectProp(AObject, PropList^[i]);
                NewNode := Node.AddNode(PropList^[i]^.Name);
                SaveToNode(ObjProp, NewNode);
                if (NewNode.ParamsCount = 0) and (NewNode.NodesCount = 0) then
                  Node.DeleteNode(NewNode);
              end;
              else
                Value := GetPropValue(AObject, PropList^[i], True);
                Node.AddParam(PropList^[i]^.Name, Value);
            end;
          end;
        end;
      finally
        FreeMem(PropList);
      end;
    end;
    if Supports(AObject, ICustomSerialize, Intf) then
      Intf.CustomSerialize(Node);
  finally
    if Assigned(NotifyIntf) then
      NotifyIntf.EndSave;
  end;
end;

class procedure TISerializer.LoadFromNode(AObject: TObject; const Node: ITreeNode);
var
  i: integer;
  Name: string;
  NewNode: ITreeNode;
  ObjProp: TObject;
  Value: variant;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
  Intf: ICustomSerialize;
  NotifyIntf: ISerializeNotify;
begin
  if Supports(AObject, ISerializeNotify, NotifyIntf) then
    NotifyIntf.BeginLoad;
  try
    for i := 0 to Node.NodesCount - 1 do
    begin
      NewNode := Node.Nodes[i];
      Name := NewNode.Name;
      if IsPublishedProp(AObject, Name) and PropIsType(AObject, Name, tkClass) then
      begin
        ObjProp := GetObjectProp(AObject, Name);
        LoadFromNode(ObjProp, NewNode);
      end;
    end;
    for i := 0 to Node.ParamsCount - 1 do
    begin
      Name := Node.ParamName[i];
      Value := Node.ParamValue[i];
      PropInfo := GetPropInfo(AObject, Name);
      if PropInfo <> nil then
      begin
        // fix DWord value bug
        if PropInfo^.PropType <> nil then
        begin
          TypeData := GetTypeData(PropInfo^.PropType);
          if (PropInfo^.PropType^.Kind = tkInteger)
            and (TypeData^.OrdType = otULong) then
              Value := Cardinal(Value);
        end;
        SetPropValue(AObject, PropInfo, Value);
      end;
    end;
    if Supports(AObject, ICustomSerialize, Intf) then
      Intf.CustomDeSerialize(Node);
  finally
    if Assigned(NotifyIntf) then
      NotifyIntf.EndLoad;
  end;
end;

class procedure TISerializer.SaveToStream(AObject: TObject; Stream: TStream);
var
  Node: ITreeNode;
begin
  Node := TITreeNode.Create(AObject.ClassName);
  SaveToNode(AObject, Node);
  SaveToStream(Node, Stream);
end;

class procedure TISerializer.LoadFromStream(AObject: TObject; Stream: TStream);
var
  Node: ITreeNode;
begin
  Node := TITreeNode.Create(AObject.ClassName);
  LoadFromStream(Node, Stream);
  LoadFromNode(AObject, Node);
end;

class procedure TISerializer.SaveToFile(const Node: ITreeNode; FileName: string);
var
  Stream: TStream;
  h: TIFileHeader;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    h.ID := FileID;
    h.Version := 10;
    Stream.Write(h, sizeof(h));
    SaveToStream(Node, Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TISerializer.LoadFromFile(const Node: ITreeNode; FileName: string);
var
  Stream: TStream;
  h: TIFileHeader;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.Read(h, sizeof(h));
    if h.ID <> FileID then
      raise EIException.CreateFmt(SInvalidDocFile, [FileName]);
    if h.Version > 10 then
      raise EIException.CreateFmt(SNewVerDocFile, [FileName]);
    LoadFromStream(Node, Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TISerializer.SaveToFile(AObject: TObject; FileName: string);
var
  Node: ITreeNode;
begin
  Node := TITreeNode.Create(AObject.ClassName);
  SaveToNode(AObject, Node);
  SaveToFile(Node, FileName);
end;

class procedure TISerializer.LoadFromFile(AObject: TObject; FileName: string);
var
  Node: ITreeNode;
begin
  Node := TITreeNode.Create(AObject.ClassName);
  LoadFromFile(Node, FileName);
  LoadFromNode(AObject, Node);
end;


{ TIBinarySerializer }

class procedure TIBinarySerializer.SaveToStream(const Node: ITreeNode; Stream: TStream);
var
  i, Count: longint;
  Name: string;
begin
  Count := Node.NodesCount;
  Stream.WriteBuffer(Count, SizeOf(longint));
  for i := 0 to Count - 1 do
  begin
    Name := Node.Nodes[i].Name;
    Stream.WriteAnsiString(Name);
    SaveToStream(Node.Nodes[i], Stream);
  end;
  Count := Node.ParamsCount;
  Stream.WriteBuffer(Count, SizeOf(longint));
  for i := 0 to Count - 1 do
  begin
    Name := Node.ParamName[i];
    Stream.WriteAnsiString(Name);
    WriteVariant(Stream, Node.ParamValue[i]);
  end;
end;

class procedure TIBinarySerializer.LoadFromStream(const Node: ITreeNode;
  Stream: TStream);
var
  i, Count, Size: longint;
  Name: string;
  Value: variant;
  NewNode: ITreeNode;
begin
  Node.Clear;
  Stream.ReadBuffer(Count, SizeOf(longint));
  for i := 0 to Count - 1 do
  begin
    Name := Stream.ReadAnsiString;
    NewNode := Node.AddNode(Name);
    LoadFromStream(NewNode, Stream);
  end;
  Stream.ReadBuffer(Count, SizeOf(longint));
  for i := 0 to Count - 1 do
  begin
    Name := Stream.ReadAnsiString;
    ReadVariant(Stream, Value);
    Node.AddParam(Name, Value);
  end;
end;

{ TIDocumentSerializer }

class procedure TIDocumentSerializer.SaveToNode(AObject: TObject;
  const Node: ITreeNode);
var
  TypeData: PTypeData;
  Count, i: integer;
  PropList: PPropList;
  Value: variant;
  Intf: ICustomSerialize;
  NotifyIntf: ISerializeNotify;
begin
  if Supports(AObject, ISerializeNotify, NotifyIntf) then
    NotifyIntf.BeginSave;
  try
    Node.Clear;
    TypeData := GetTypeData(AObject.ClassInfo);
    Count := TypeData^.PropCount;
    if Count > 0 then
    begin
      GetMem(PropList, SizeOf(PPropInfo) * Count);
      try
        GetPropInfos(AObject.ClassInfo, PropList);
        for i := 0 to Count - 1 do
        begin
          if IsStoredProp(AObject, PropList^[i]) then
          begin
            case PropList^[i]^.PropType^.Kind of
              tkClass, tkMethod, tkInterface: ;
              else
                if PropList^[i]^.Name = 'ActiveLayerIndex' then
                  continue; // ignore ActiveLayerIndex
                Value := GetPropValue(AObject, PropList^[i], True);
                Node.AddParam(PropList^[i]^.Name, Value);
            end;
          end;
        end;
      finally
        FreeMem(PropList);
      end;
    end;
  finally
    if Assigned(NotifyIntf) then
      NotifyIntf.EndSave;
  end;
end;

class procedure TIDocumentSerializer.LoadFromNode(AObject: TObject;
  const Node: ITreeNode);
var
  i: integer;
  Name: string;
  Value: variant;
  PropInfo: PPropInfo;
  NotifyIntf: ISerializeNotify;
begin
  if Supports(AObject, ISerializeNotify, NotifyIntf) then
    NotifyIntf.BeginLoad;
  try
    for i := 0 to Node.ParamsCount - 1 do
    begin
      Name := Node.ParamName[i];
      Value := Node.ParamValue[i];
      PropInfo := GetPropInfo(AObject, Name);
      if PropInfo <> nil then
        SetPropValue(AObject, PropInfo, Value);
    end;
  finally
    if Assigned(NotifyIntf) then
      NotifyIntf.EndLoad;
  end;
end;

end.
