{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uCommands;

{$mode objfpc}{$H+}
{$C+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, Contnrs, SysUtils, uIObject, uCommon, uTree, uIDocument, uGraphics;

type



  { TCommand }

  TCommand = class
  private
    fEnabled: boolean;
    fCommandState: TCommandState;
    fCommandName: string;
    fDocument: TIDocument;
  protected
    procedure DoBeforeCall; virtual;
    procedure DoAfterExec; virtual;
    property Document: TIDocument read fDocument;
  public
    constructor Create(aDocument: TIDocument; const aCmdName: string);
    procedure Rollback; virtual;
    procedure Execute; virtual;
    procedure UpdateID(const OldIndex, NewIndex: QWord); virtual;
    procedure Commit(aList: TObjectList); virtual;
    property CommandState: TCommandState read fCommandState;
    property CommandName: string read fCommandName;
    property Enabled: boolean read fEnabled write fEnabled;
  end;


  { TIObjectCommand }

  TIObjectCommand = class(TCommand)
  protected
    fNode: TITreeNode;
    procedure Store; virtual;
    procedure Restore; virtual;
    procedure DeleteObjects; virtual;
    procedure Setup(aNode: TITreeNode; aList: TObjectList);
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList;
      const aCmdName: string);
    destructor Destroy; override;
    procedure UpdateID(const OldIndex, NewIndex: QWord); override;
  end;

  { TCreationCommand }

  TCreationCommand = class(TIObjectCommand)
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aObject: TIObject);
  end;


  { TDuplicateCommand }

  TDuplicateCommand = class(TIObjectCommand)
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TDeleteCommand }

  TDeleteCommand = class(TIObjectCommand)
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TCutCommand }

  TCutCommand = class(TIObjectCommand)
  protected
    procedure DoBeforeCall; override;
    procedure DeleteObjects; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TGroupBaseCommand }

  TGroupBaseCommand = class(TIObjectCommand)
  protected
    fRecursive: Boolean;
    procedure Group; virtual;
    procedure Ungroup; virtual;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList; const aCmdName: string);
  end;

  { TGroupCommand }

  TGroupCommand = class(TGroupBaseCommand)
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TUngroupCommand }

  TUngroupCommand = class(TGroupBaseCommand)
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TUngroupAllCommand }

  TUngroupAllCommand = class(TGroupBaseCommand)
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TDragCommand }

  TDragCommand = class(TIObjectCommand)
  private
    fOldBasePoints: TIObjectVertexBuffer;
    fNewBasePoints: TIObjectVertexBuffer;
    procedure StoreBasePoints(var buf: TIObjectVertexBuffer; aList: TObjectList);
    procedure RestoreBasePoints(const buf: TIObjectVertexBuffer);
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList; hit: cardinal);
    procedure Commit(aList: TObjectList); override;
    destructor Destroy; override;
  end;

  { TObjPropertyCommand }

  TObjPropertyCommand = class(TIObjectCommand)
  private
    fNewNode: TITreeNode;
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList;
      const aPropertyName: string);
    procedure Commit(aList: TObjectList); override;
    destructor Destroy; override;
  end;

  { TOrderCommand }

  TOrderCommand = class(TIObjectCommand)
  private
    fOldOrder: TIObjectOrderBuffer;
    fNewOrder: TIObjectOrderBuffer;
    procedure StoreOrder(var buf: TIObjectOrderBuffer; aList: TObjectList);
    procedure RestoreOrder(const buf: TIObjectOrderBuffer);
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList;
      const aCmdName: string);
    destructor Destroy; override;
    procedure Commit(aList: TObjectList); override;
  end;

  { TObjOrderCommand }

  TObjOrderCommand = class(TOrderCommand)
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList; AOrder: TOrder);
  end;

  { TLayerOrderCommand }

  TLayerOrderCommand = class(TOrderCommand)
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList; AOrder: TOrder);
  end;

  { TLayerCreationCommand }

  TLayerCreationCommand = class(TCreationCommand)
  public
    constructor Create(aDocument: TIDocument; aObject: TIObject);
  end;

  { TLayerDeleteCommand }

  TLayerDeleteCommand = class(TDeleteCommand)
  public
    constructor Create(aDocument: TIDocument; aList: TObjectList);
  end;

  { TLayerRenameCommand }

  TLayerRenameCommand = class(TCommand)
  private
    fID: QWord;
    fOldName: string;
    fNewName: string;
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument; aID: QWord; const aOldName, aNewName: string);
    procedure UpdateID(const OldIndex, NewIndex: QWord); override;
  end;

  { TDocumentPropertiesCommand }

  TDocumentPropertiesCommand = class(TCommand)
  private
    fOldNode: TITreeNode;
    fNewNode: TITreeNode;
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument);
    destructor Destroy; override;
    procedure Commit(aDocument: TIDocument); reintroduce;
  end;

  { TImageCacheCommand }

  TImageCacheCommand = class(TCommand)
  private
    fOldStream: TStream;
    fNewStream: TStream;
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument);
    destructor Destroy; override;
    procedure Commit; reintroduce;
  end;

  { TSVGCacheCommand }

  TSVGCacheCommand = class(TCommand)
  private
    fOldStream: TStream;
    fNewStream: TStream;
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument);
    destructor Destroy; override;
    procedure Commit; reintroduce;
  end;

  { TStyleCacheCommand }

  TStyleCacheCommand = class(TCommand)
  private
    fOldNode: TITreeNode;
    fNewNode: TITreeNode;
  protected
    procedure DoBeforeCall; override;
  public
    constructor Create(aDocument: TIDocument);
    destructor Destroy; override;
    procedure Commit; reintroduce;
  end;

  { TTemporaryFileStream }

  TTemporaryFileStream = class(THandleStream)
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  LazFileUtils, LazUTF8, uClipboard;


{ TCommand }

constructor TCommand.Create(aDocument: TIDocument; const aCmdName: string);
begin
  fCommandState := csUndo;
  fCommandName := aCmdName;
  fDocument := aDocument;
end;


procedure TCommand.DoBeforeCall;
begin
  // do nothing
end;

procedure TCommand.DoAfterExec;
begin
  fDocument.SelectionChange;
end;

procedure TCommand.Rollback;
begin
  DoBeforeCall;
  fCommandState := csRedo;
  DoAfterExec;
end;

procedure TCommand.Execute;
begin
  DoBeforeCall;
  fCommandState := csUndo;
  DoAfterExec;
end;

procedure TCommand.UpdateID(const OldIndex, NewIndex: QWord);
begin
  // nothing
end;

procedure TCommand.Commit(aList: TObjectList);
begin
  // nothing
end;

{ TIObjectCommand }

constructor TIObjectCommand.Create(aDocument: TIDocument; aList: TObjectList;
  const aCmdName: string);
begin
  inherited Create(aDocument, aCmdName);
  fNode := TITreeNode.Create('Objects');
  Setup(fNode, aList);
end;

destructor TIObjectCommand.Destroy;
begin
  fNode.Free;
  inherited Destroy;
end;

procedure TIObjectCommand.Setup(aNode: TITreeNode; aList: TObjectList);
var
  i: integer;
  obj: TIObject;
  name: string;
  node: ITreeNode;
  factory: TIObjectsFactory;
begin
  aNode.Clear;
  aNode.ID := TIObject(aList[0]).Owner.ID;
  for i := 0 to aList.Count - 1 do
  begin
    obj := TIObject(aList[i]);
    name := factory.FindInstanceName(obj);
    node := aNode.AddNode(name);
    node.ID := obj.ID;
    TISerializer.SaveToNode(obj, node);
    node.AddParam('ZIndex', obj.Index);
    node.AddParam('Selected', obj.Selected);
  end;
end;

procedure TIObjectCommand.Store;
var
  i: integer;
  obj: TIObject;
  node: ITreeNode;
begin
  for i := 0 to fNode.NodesCount - 1 do
  begin
    node := fNode.Nodes[i];
    obj := fDocument.GetObjectByID(node.ID);
    TISerializer.SaveToNode(obj, node);
    node.AddParam('ZIndex', obj.Index);
    node.AddParam('Selected', obj.Selected);
  end;
end;

procedure TIObjectCommand.Restore;
var
  i: integer;
  node: ITreeNode;
  NewObj, OwnerObj: TIObject;
  factory: TIObjectsFactory;
begin
  factory := TIObjectsFactory.GetInstance;
  OwnerObj := fDocument.GetObjectByID(fNode.ID);
  for i := 0 to fNode.NodesCount - 1 do
  begin
    node := fNode.Nodes[i];
    NewObj := factory.CreateInstance(OwnerObj, fDocument, node.Name);
    fDocument.History.UpdateID(node.ID, NewObj.ID);
    TISerializer.LoadFromNode(NewObj, node);
    NewObj.Index := node.ParamValueByName['ZIndex'];
    NewObj.Selected := node.ParamValueByName['Selected'];
  end;
end;

procedure TIObjectCommand.DeleteObjects;
var
  i: integer;
  obj: TIObject;
begin
  for i := 0 to fNode.NodesCount - 1 do
  begin
    obj := fDocument.GetObjectByID(fNode.Nodes[i].ID);
    obj.Selected := false;
    obj.Free;
  end;
end;

procedure TIObjectCommand.UpdateID(const OldIndex, NewIndex: QWord);
begin
  fNode.UpdateID(OldIndex, NewIndex);
end;

{ TCreationCommand }

constructor TCreationCommand.Create(aDocument: TIDocument; aObject: TIObject);
var
  List: TObjectList;
begin
  List := TObjectList.Create(False);
  try
    List.Add(aObject);
    inherited Create(aDocument, List, 'Create ' + aObject.GetObjectName);
  finally
    List.Free;
  end;
end;

procedure TCreationCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo:
    begin
      Store;
      DeleteObjects;
    end;
    csRedo:
    begin
      Restore;
    end;
  end;
end;

{ TDuplicateCommand }

constructor TDuplicateCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList, 'Duplicate Object(s)');
end;

procedure TDuplicateCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo:
    begin
      Store;
      DeleteObjects;
    end;
    csRedo:
      Restore;
  end;
end;

{ TDeleteCommand }

constructor TDeleteCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList, 'Delete Object(s)');
  Store;
end;

procedure TDeleteCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: Restore;
    csRedo:
    begin
      Store;
      DeleteObjects;
    end;
  end;
end;

{ TCutCommand }

constructor TCutCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList, 'Cut Object(s)');
  Store;
end;

procedure TCutCommand.DeleteObjects;
var
  i: integer;
  obj: TIObject;
  list: TObjectList;
begin
  list := TObjectList.Create(False);
  try
    for i := 0 to fNode.NodesCount - 1 do
      list.Add(fDocument.GetObjectByID(fNode.Nodes[i].ID));
    Clipboard.SetObjects(list);
    for i := 0 to list.Count - 1 do
    begin
      obj := TIObject(list[i]);
      obj.Free;
    end;
  finally
    list.Free;
  end;
end;

procedure TCutCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: Restore;
    csRedo:
    begin
      Store;
      DeleteObjects;
    end;
  end;
end;

{ TGroupBaseCommand }

constructor TGroupBaseCommand.Create(aDocument: TIDocument; aList: TObjectList;
  const aCmdName: string);
begin
  inherited Create(aDocument, aList, aCmdName);
  fRecursive := false;
end;

procedure TGroupBaseCommand.Group;

  function GroupRecursive(gnode: ITreeNode): QWord;
  var
    q: Qword;
    g: TIGroup;
    i: integer;
    obj: TIObject;
    cnode, onode: ITreeNode;
    List: TObjectList;
  begin
    result := 0;
    cnode := gnode.NodeByName['Childs'];
    if cnode = nil then
      exit;
    List := TObjectList.Create(false);
    try
      for i := 0 to cnode.NodesCount - 1 do
      begin
        onode := cnode.Nodes[i];
        if onode.ID = 0 then
          continue;
        if fRecursive and (onode.Name = 'Group') then
        begin
          q := GroupRecursive(onode);
          if q = 0 then
            continue;
          obj := fDocument.GetObjectByID(q);
        end
        else
          obj := fDocument.GetObjectByID(onode.ID);
        List.Add(obj);
      end;
      if List.Count > 0 then
      begin
        g := TIGroup.Group(List, fDocument);
        fDocument.History.UpdateID(gnode.ID, g.ID);
        result := g.ID;
      end;
    finally
      List.Free;
    end;
  end;

var
  i: integer;
  node: ITreeNode;
begin
  for i := 0 to fNode.NodesCount - 1 do
  begin
    node := fNode.Nodes[i];
    if node.Name <> 'Group' then
      continue;
    GroupRecursive(node);
  end;
end;

procedure TGroupBaseCommand.Ungroup;

  procedure UngroupRecursive(gnode: ITreeNode);
  var
    i: integer;
    obj: TIObject;
    cnode, onode: ITreeNode;
  begin
    obj := fDocument.GetObjectByID(gnode.ID);
    if obj is TIGroup then
      TIGroup(obj).UnGroup;
    if not fRecursive then
      exit;
    cnode := gnode.NodeByName['Childs'];
    if cnode = nil then
      exit;
    for i := 0 to cnode.NodesCount - 1 do
    begin
      onode := cnode.Nodes[i];
      if onode.Name = 'Group' then
        UngroupRecursive(onode);
    end;
  end;

var
  i: integer;
  node: ITreeNode;
begin
  for i := 0 to fNode.NodesCount - 1 do
  begin
    node := fNode.Nodes[i];
    if node.Name <> 'Group' then
      continue;
    UngroupRecursive(node);
  end;
end;


{ TGroupCommand }

constructor TGroupCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList, 'Group');
end;

procedure TGroupCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: Ungroup;
    csRedo: Group;
  end;
end;

{ TUngroupCommand }

constructor TUngroupCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList, 'Ungroup');
end;

procedure TUngroupCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: Group;
    csRedo: Ungroup;
  end;
end;


{ TUngroupAllCommand }

constructor TUngroupAllCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList, 'Ungroup All');
  fRecursive := true;
end;

procedure TUngroupAllCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: Group;
    csRedo: Ungroup;
  end;
end;


{ TDragCommand }

constructor TDragCommand.Create(aDocument: TIDocument; aList: TObjectList;
  hit: cardinal);
var
  s: string;
begin
  if hit = HT_IN then
    s := 'Move Object(s)'
  else
    s := 'Transform Object(s)';
  inherited Create(aDocument, aList, s);
  StoreBasePoints(fOldBasePoints, aList);
end;

destructor TDragCommand.Destroy;
begin
  SetLength(fOldBasePoints, 0, 0);
  SetLength(fNewBasePoints, 0, 0);
  inherited Destroy;
end;

procedure TDragCommand.Commit(aList: TObjectList);
begin
  StoreBasePoints(fNewBasePoints, aList);
end;

procedure TDragCommand.StoreBasePoints(var buf: TIObjectVertexBuffer;
  aList: TObjectList);
var
  i, j: integer;
  obj: TIObject;
begin
  SetLength(buf, aList.Count);
  for i := 0 to aList.Count - 1 do
  begin
    obj := TIObject(aList[i]);
    SetLength(buf[i], obj.BasePointsCount);
    for j := 0 to obj.BasePointsCount - 1 do
      buf[i, j] := obj.BasePoints[j];
  end;
end;

procedure TDragCommand.RestoreBasePoints(const buf: TIObjectVertexBuffer);
var
  i, j: integer;
  obj: TIObject;
begin
  for i := 0 to fNode.NodesCount - 1 do
  begin
    obj := fDocument.GetObjectByID(fNode.Nodes[i].ID);
    for j := 0 to obj.BasePointsCount - 1 do
      obj.BasePoints[j] := buf[i, j];
  end;
end;

procedure TDragCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: RestoreBasePoints(fOldBasePoints);
    csRedo: RestoreBasePoints(fNewBasePoints);
  end;
end;

{ TObjPropertyCommand }

constructor TObjPropertyCommand.Create(aDocument: TIDocument;
  aList: TObjectList; const aPropertyName: string);
var
  s: string;
begin
  s := 'Modify ' + aPropertyName;
  inherited Create(aDocument, aList, s);
  fNewNode := TITreeNode.Create('Objects');
end;

destructor TObjPropertyCommand.Destroy;
begin
  fNewNode.Free;
  inherited Destroy;
end;

procedure TObjPropertyCommand.Commit(aList: TObjectList);
begin
  Setup(fNewNode, aList);
end;

procedure TObjPropertyCommand.DoBeforeCall;
var
  i: integer;
  obj: TIObject;
begin
  case CommandState of
    csUndo:
    begin
      for i := 0 to fNode.NodesCount - 1 do
      begin
        obj := fDocument.GetObjectByID(fNode.Nodes[i].ID);
        TISerializer.LoadFromNode(obj, fNode.Nodes[i]);
      end;
    end;
    csRedo:
    begin
      for i := 0 to fNewNode.NodesCount - 1 do
      begin
        obj := fDocument.GetObjectByID(fNode.Nodes[i].ID);
        TISerializer.LoadFromNode(obj, fNewNode.Nodes[i]);
      end;
    end;
  end;
end;

{ TOrderCommand }

constructor TOrderCommand.Create(aDocument: TIDocument; aList: TObjectList;
  const aCmdName: string);
begin
  inherited Create(aDocument, aList, aCmdName);
  StoreOrder(fOldOrder, aList);
end;

destructor TOrderCommand.Destroy;
begin
  SetLength(fOldOrder, 0);
  SetLength(fNewOrder, 0);
  inherited Destroy;
end;

procedure TOrderCommand.Commit(aList: TObjectList);
begin
  StoreOrder(fNewOrder, aList);
end;

procedure TOrderCommand.StoreOrder(var buf: TIObjectOrderBuffer;
  aList: TObjectList);
var
  i: integer;
  obj: TIObject;
begin
  SetLength(buf, aList.Count);
  for i := 0 to aList.Count - 1 do
  begin
    obj := TIObject(aList[i]);
    buf[i] := obj.Index;
  end;
end;

procedure TOrderCommand.RestoreOrder(const buf: TIObjectOrderBuffer);
var
  i: integer;
  obj: TIObject;
begin
  for i := 0 to fNode.NodesCount - 1 do
  begin
    obj := fDocument.GetObjectByID(fNode.Nodes[i].ID);
    obj.Index := buf[i];
  end;
end;

procedure TOrderCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: RestoreOrder(fOldOrder);
    csRedo: RestoreOrder(fNewOrder);
  end;
end;


{ TObjOrderCommand }

constructor TObjOrderCommand.Create(aDocument: TIDocument; aList: TObjectList;
  AOrder: TOrder);
begin
  inherited Create(aDocument, aList, 'Object ' + OrderCommandNames[AOrder]);
end;

{ TLayerOrderCommand }

constructor TLayerOrderCommand.Create(aDocument: TIDocument;
  aList: TObjectList; AOrder: TOrder);
begin
  inherited Create(aDocument, aList, 'Layer ' + OrderCommandNames[AOrder]);
end;

{ TLayerCreationCommand }

constructor TLayerCreationCommand.Create(aDocument: TIDocument;
  aObject: TIObject);
begin
  inherited Create(aDocument, aObject);
  fCommandName := 'New Layer';
end;

{ TLayerDeleteCommand }

constructor TLayerDeleteCommand.Create(aDocument: TIDocument; aList: TObjectList);
begin
  inherited Create(aDocument, aList);
  fCommandName := 'Delete Layer(s)';
end;

{ TLayerRenameCommand }

constructor TLayerRenameCommand.Create(aDocument: TIDocument; aID: QWord;
  const aOldName, aNewName: string);
begin
  inherited Create(aDocument, 'Reanme Layer');
  fID := aID;
  fOldName := aOldName;
  fNewName := aNewName;
end;

procedure TLayerRenameCommand.UpdateID(const OldIndex, NewIndex: QWord);
begin
  if fID = OldIndex then
    fID := NewIndex;
end;

procedure TLayerRenameCommand.DoBeforeCall;
var
  s: string;
  obj: TIObject;
begin
  case CommandState of
    csUndo: s := fOldName;
    csRedo: s := fNewName;
    else
      exit;
  end;
  obj := fDocument.GetObjectByID(fID);
  if obj is TILayer then
    TILayer(obj).Name := s;
end;

{ TDocumentPropertiesCommand }

constructor TDocumentPropertiesCommand.Create(aDocument: TIDocument);
begin
  inherited Create(aDocument, 'Modify Project Settings');
  fOldNode := TITreeNode.Create('Document');
  fNewNode := TITreeNode.Create('Document');
  TIDocumentSerializer.SaveToNode(fDocument, fOldNode);
end;

destructor TDocumentPropertiesCommand.Destroy;
begin
  fOldNode.Free;
  fNewNode.Free;
  inherited Destroy;
end;

procedure TDocumentPropertiesCommand.Commit(aDocument: TIDocument);
begin
  TIDocumentSerializer.SaveToNode(aDocument, fNewNode);
end;

procedure TDocumentPropertiesCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: TIDocumentSerializer.LoadFromNode(fDocument, fOldNode);
    csRedo: TIDocumentSerializer.LoadFromNode(fDocument, fNewNode);
  end;
end;


{ TImageCacheCommand }

constructor TImageCacheCommand.Create(aDocument: TIDocument);
begin
  inherited Create(aDocument, 'Modify Project Images');
  fOldStream := TTemporaryFileStream.Create;
  fNewStream := TTemporaryFileStream.Create;
  TIBinarySerializer.SaveToStream(fDocument.ImageCache, fOldStream);
end;

destructor TImageCacheCommand.Destroy;
begin
  FreeAndNil(fOldStream);
  FreeAndNil(fNewStream);
  inherited Destroy;
end;

procedure TImageCacheCommand.Commit;
begin
  TIBinarySerializer.SaveToStream(fDocument.ImageCache, fNewStream);
end;

procedure TImageCacheCommand.DoBeforeCall;
begin
  fOldStream.Seek(0, 0);
  fNewStream.Seek(0, 0);
  fDocument.ImageCache.Clear;
  case CommandState of
    csUndo: TIBinarySerializer.LoadFromStream(fDocument.ImageCache, fOldStream);
    csRedo: TIBinarySerializer.LoadFromStream(fDocument.ImageCache, fNewStream);
  end;
end;


{ TSVGCacheCommand }

constructor TSVGCacheCommand.Create(aDocument: TIDocument);
begin
  inherited Create(aDocument, 'Modify Project SVG');
  fOldStream := TTemporaryFileStream.Create;
  fNewStream := TTemporaryFileStream.Create;
  TIBinarySerializer.SaveToStream(fDocument.SVGCache, fOldStream);
end;

destructor TSVGCacheCommand.Destroy;
begin
  FreeAndNil(fOldStream);
  FreeAndNil(fNewStream);
  inherited Destroy;
end;

procedure TSVGCacheCommand.Commit;
begin
  TIBinarySerializer.SaveToStream(fDocument.SVGCache, fNewStream);
end;

procedure TSVGCacheCommand.DoBeforeCall;
begin
  fOldStream.Seek(0, 0);
  fNewStream.Seek(0, 0);
  fDocument.ImageCache.Clear;
  case CommandState of
    csUndo: TIBinarySerializer.LoadFromStream(fDocument.SVGCache, fOldStream);
    csRedo: TIBinarySerializer.LoadFromStream(fDocument.SVGCache, fNewStream);
  end;
end;


{ TStyleCacheCommand }

constructor TStyleCacheCommand.Create(aDocument: TIDocument);
begin
  inherited Create(aDocument, 'Modify Project Styles');
  fOldNode := TITreeNode.Create('StyleCache');
  fNewNode := TITreeNode.Create('StyleCache');
  TISerializer.SaveToNode(fDocument.StyleCache, fOldNode);
end;

destructor TStyleCacheCommand.Destroy;
begin
  FreeAndNil(fOldNode);
  FreeAndNil(fNewNode);
  inherited Destroy;
end;

procedure TStyleCacheCommand.Commit;
begin
  TISerializer.SaveToNode(fDocument.StyleCache, fNewNode);
end;

procedure TStyleCacheCommand.DoBeforeCall;
begin
  case CommandState of
    csUndo: TISerializer.LoadFromNode(fDocument.StyleCache, fOldNode);
    csRedo: TISerializer.LoadFromNode(fDocument.StyleCache, fNewNode);
  end;
end;



{ TTemporaryFileStream }

constructor TTemporaryFileStream.Create;
var
  s: string;
  TempHandle: THandle;
begin
  s := GetTempFileNameUTF8(GetTempDir, '~ipd');
  {$IFDEF MSWINDOWS}
  TempHandle := CreateFileW(PWideChar(UTF8Decode(s)), GENERIC_READ or
    GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY or
    FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_DELETE_ON_CLOSE, 0);
  {$ELSE}
  //TempHandle := FileCreate(path, 0, 0);
  //fpOpen
  {$ENDIF}
  inherited Create(TempHandle);
end;

destructor TTemporaryFileStream.Destroy;
begin
  inherited Destroy;
  if Handle <> feInvalidHandle then
    FileClose(Handle);
end;

end.

