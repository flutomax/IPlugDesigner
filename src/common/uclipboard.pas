{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uClipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Contnrs, Clipbrd, uCommon, uIObject;

type

  { TIClipboard }

  TIClipboard = class(TClipboard)
  public
    function SetObjects(AObjects: TObjectList): boolean; virtual;
    function GetObjects(AOwner: TIObject; ADocument: TObject;
      AObjects: TObjectList): boolean; virtual;
  end;

var
  CF_IPLUGDESIGNER: TClipboardFormat;

function Clipboard: TIClipboard;

implementation

uses
  uIDocument, uTree;

function Clipboard: TIClipboard;
begin
  Result := Clipbrd.Clipboard as TIClipboard;
end;

procedure InitModule;
begin
  CF_IPLUGDESIGNER := RegisterClipboardFormat('IPlugDesigner.IObject');
  if CF_IPLUGDESIGNER = 0 then
    RaiseLastOSError;
  SetClipboard(TIClipboard.Create);
end;

{ TIClipboard }

function TIClipboard.SetObjects(AObjects: TObjectList): boolean;
var
  i: integer;
  NewObj, AOwner: TIObject;
  Factory: TIObjectsFactory;
  Node, ObjectNode: ITreeNode;
  TypeName: string;
  ms: TMemoryStream;
begin
  Result := TIGroup.CanGroup(AObjects);
  if not Result then
    exit;
  AOwner := TIObject(AObjects[0]);
  while AOwner.Owner <> nil do
    AOwner := AOwner.Owner;
  if AOwner is TIRootLayer then
    AOwner := TIRootLayer(AOwner).Document.NonContentRoot
  else
    AOwner := nil;
  Factory := TIObjectsFactory.GetInstance;
  Node := TITreeNode.Create('Objects');
  for i := 0 to AObjects.Count - 1 do
  begin
    TypeName := Factory.FindInstanceName(TIObject(AObjects[i]));
    ObjectNode := Node.AddNode(TypeName);
    TISerializer.SaveToNode(AObjects[i], ObjectNode);
  end;
  ms := TMemoryStream.Create;
  try
    TIBinarySerializer.SaveToStream(Node, ms);
    ms.Seek(0, 0);
    Open;
    try
      SetFormat(CF_IPLUGDESIGNER, ms);
    finally
      Close;
    end;
  finally
    ms.Free;
  end;
end;

function TIClipboard.GetObjects(AOwner: TIObject; ADocument: TObject;
  AObjects: TObjectList): boolean;
var
  i: integer;
  NewObj: TIObject;
  Factory: TIObjectsFactory;
  Node: ITreeNode;
  ms: TMemoryStream;
begin
  Result := False;
  AObjects.Clear;
  if not HasFormat(CF_IPLUGDESIGNER) then
    exit;
  Open;
  try
    ms := TMemoryStream.Create;
    try
      if not GetFormat(CF_IPLUGDESIGNER, ms) then
        exit;
      ms.Seek(0, 0);
      Node := TITreeNode.Create('Objects');
      TIBinarySerializer.LoadFromStream(Node, ms);
      Factory := TIObjectsFactory.GetInstance;
      try
        for i := 0 to Node.NodesCount - 1 do
        begin
          NewObj := Factory.CreateInstance(AOwner, ADocument, Node.Nodes[i].Name);
          AObjects.Add(NewObj);
          TISerializer.LoadFromNode(NewObj, Node.Nodes[i]);
        end;
      except
        for i := AObjects.Count - 1 downto 0 do
        begin
          NewObj := TIObject(AObjects.Extract(AObjects[i]));
          NewObj.Free;
        end;
        raise;
      end;
    finally
      ms.Free;
    end;
  finally
    Close;
  end;
end;

initialization
  InitModule;

end.
