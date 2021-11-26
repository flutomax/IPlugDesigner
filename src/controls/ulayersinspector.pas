{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uLayersInspector;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, LMessages, Classes, SysUtils, Graphics, Contnrs, Controls,
  uCommon, uIObject, uIDocument, uIMiscControls, uGraphics;

type

  TLayersInspector = class;

  TLayerItem = class
  private
    fThumb: TBitmap;
    fThumbValid: boolean;
    fLayer: TILayer;
    fInspector: TLayersInspector;
    procedure UpdateThumb;
    function GetThumb: TBitmap;
  public
    constructor Create(AInspector: TLayersInspector; ALayer: TILayer);
    destructor Destroy; override;
    procedure InvalidateThumb;
    property Layer: TILayer read fLayer;
    property Inspector: TLayersInspector read fInspector;
    property ThumbValid: boolean read fThumbValid write fThumbValid;
    property Thumb: TBitmap read GetThumb;
  end;

  TLayerChangedEvent = procedure(Sender: TObject; ALayer: TILayer) of object;

  { TLayersInspector }

  TLayersInspector = class(TIScrollBox, IDocumentObserver, ILayeredDocumentObserver)
  private
    fItemHeight: integer;
    fItems: TObjectList;
    fSelectedList: TObjectList;
    fPaddings: integer;
    fDocument: TIDocument;
    fVisibleGlyph: TBitmap;
    fActiveGlyph: TBitmap;
    fLockedGlyph: TBitmap;
    fOnActiveLayerChanged: TLayerChangedEvent;
    procedure InvalidateLayers;
    procedure SetDocument(const Value: TIDocument);
    function GetObjectLayer(AObject: TIObject): TILayer;
    function GetLayerItem(Layer: TILayer): TLayerItem;
    procedure InvalidateLayerThumb(AObject: TIObject);
    function GetItemHeight: integer;
    procedure SetItemHeight(Value: integer);
    function GetItem(Index: integer): TLayerItem;
    function GetItemsCount: integer;
    procedure SetPaddings(Value: integer);
    function GetThumbHeight: integer;
    function GetThumbWidth: integer;
    function GetSelected(Index: integer): TILayer;
    function GetSelectedCount: integer;
    procedure SetActiveGlyph(const Value: TBitmap);
    procedure SetVisibleGlyph(const Value: TBitmap);
    procedure SetLockedGlyph(const Value: TBitmap);
    procedure CMFocusChanged(var msg: TLMessage); message CM_FOCUSCHANGED;
    procedure WMLButtonDown(var msg: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMKeyDown(var msg: TLMKeyDown); message LM_KEYDOWN;
  protected
    procedure Paint; override;
    function AreaWidth: integer; override;
    function AreaHeight: integer; override;
    function ScrollHLine: integer; override;
    function ScrollVLine: integer; override;
    procedure ObjectAdded(AObject: TIObject);
    procedure ObjectRemove(AObject: TIObject);
    procedure ObjectChanging(AObject: TIObject);
    procedure ObjectChanged(AObject: TIObject);
    procedure ObjectSelected(AObject: TIObject; Selected: boolean);
    procedure ObjectCommandReceived(AObject: TIObject; var Command: TICmdMessage);
    procedure ObjectCommandHandled(AObject: TIObject; var Command: TICmdMessage);
    procedure DocumentResize(NewWidth, NewHeight: double);
    procedure DocumentDestroying;
    procedure LayerAdded(ALayer: TILayer);
    procedure LayerRemove(ALayer: TILayer);
    procedure LayerChanging(ALayer: TILayer);
    procedure LayerChanged(ALayer: TILayer);
    procedure ActiveLayerChanged(ALayer: TILayer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemRect(Index: integer): TRect;
    function ItemAtPos(X, Y: integer): TLayerItem;
    procedure AddToSelection(ALayer: TILayer; MakePrimary: boolean);
    procedure RemoveFromSelection(ALayer: TILayer);
    procedure ClearSelection;
    procedure DeleteSelected;
    procedure Select(ALayer: TILayer);
    function LayerSelected(ALayer: TILayer): boolean;
    property Document: TIDocument read fDocument write SetDocument;
    property ItemsCount: integer read GetItemsCount;
    property Items[Index: integer]: TLayerItem read GetItem;
    property ThumbWidth: integer read GetThumbWidth;
    property ThumbHeight: integer read GetThumbHeight;
    property SelectedList: TObjectList read fSelectedList;
    property SelectedCount: integer read GetSelectedCount;
    property Selected[Index: integer]: TILayer read GetSelected;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Enabled;
    property Font;
    property Height;
    property PopupMenu;
    property Visible;
    property Width;
    property BorderStyle default bsSingle;
    property VisibleGlyph: TBitmap read fVisibleGlyph write SetVisibleGlyph;
    property ActiveGlyph: TBitmap read fActiveGlyph write SetActiveGlyph;
    property LockedGlyph: TBitmap read fLockedGlyph write SetLockedGlyph;
    property OnActiveLayerChanged: TLayerChangedEvent read fOnActiveLayerChanged write fOnActiveLayerChanged;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnResize;
    property OnContextPopup;
    property ItemHeight: integer read GetItemHeight write SetItemHeight default 36;
    property Paddings: integer read fPaddings write SetPaddings default 2;
  end;

implementation

{$R ulayersinspector.res}

uses
  RTLConsts, Math, Forms, ImgList, FPCanvas;


{ TLayerItem }

constructor TLayerItem.Create(AInspector: TLayersInspector; ALayer: TILayer);
begin
  inherited Create;
  fLayer := ALayer;
  fInspector := AInspector;
end;

destructor TLayerItem.Destroy;
begin
  fThumb.Free;
  inherited Destroy;
end;

function TLayerItem.GetThumb: TBitmap;
begin
  UpdateThumb;
  Result := fThumb;
end;

procedure TLayerItem.InvalidateThumb;
begin
  fThumbValid := False;
end;

procedure TLayerItem.UpdateThumb;
var
  c: TICanvas;
  r: TIFloatRect;
begin
  if fThumbValid then
    exit;
  FreeAndNil(fThumb);
  fThumb := TBitmap.Create;
  fThumb.SetSize(Inspector.ThumbWidth, Inspector.ThumbHeight);
  with Inspector.Document do
    r := TIFloatRect.Create(0, 0, Width, Height);
  c := TICanvas.Create(Inspector.ThumbWidth, Inspector.ThumbHeight);
  try
    c.ClipRect := r;
    c.DrawCheckboard;
    c.TransformSpace(r, TIFloatRect.Create(0, 0, fThumb.Width, fThumb.Height));
    Layer.Draw(c);
    c.DrawTo(fThumb);
  finally
    c.Free;
  end;
  fThumbValid := True;
end;

{ TLayersInspector }

procedure LoadResBitmap(const Name: string; Bitmap: TBitmap);
var
  bmp: TCustomBitmap;
begin
  bmp := GetDefaultGlyph(Name);
  try
    if Assigned(bmp) then
      Bitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

constructor TLayersInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems := TObjectList.Create(True);
  fSelectedList := TObjectList.Create(False);
  ControlStyle := [csClickEvents, csDoubleClicks];
  Width := 100;
  Height := 400;
  DoubleBuffered := True;
  fItemHeight := 32;
  fPaddings := 2;
  fVisibleGlyph := TBitmap.Create;
  LoadResBitmap('LR_VISIBLE', fVisibleGlyph);
  fActiveGlyph := TBitmap.Create;
  LoadResBitmap('LR_ACTIVE', fActiveGlyph);
  fLockedGlyph := TBitmap.Create;
  LoadResBitmap('LR_LOCKED', fLockedGlyph);
end;

destructor TLayersInspector.Destroy;
begin
  Document := nil;
  fSelectedList.Free;
  fItems.Free;
  fLockedGlyph.Free;
  fActiveGlyph.Free;
  fVisibleGlyph.Free;
  inherited Destroy;
end;

procedure TLayersInspector.InvalidateLayers;
var
  i: integer;
begin
  for i := 0 to Document.LayersCount - 1 do
    Items[i].InvalidateThumb;
end;

procedure TLayersInspector.SetDocument(const Value: TIDocument);
var
  i: integer;
  Layer: TILayer;
begin
  if Assigned(fDocument) then
    fDocument.UnRegisterObserver(Self);
  fItems.Clear;
  fDocument := Value;
  if Assigned(fDocument) then
  begin
    fDocument.RegisterObserver(Self);
    for i := 0 to Value.LayersCount - 1 do
    begin
      Layer := Value.Layers[i];
      fItems.Insert(0, TLayerItem.Create(Self, Layer));
    end;
  end;
  UpdateScrollBars;
  Invalidate;
end;

function TLayersInspector.GetObjectLayer(AObject: TIObject): TILayer;
begin
  Result := nil;
  if AObject.Owner <> nil then
  begin
    while AObject.Owner <> Document.RootObject do
      AObject := AObject.Owner;
    if AObject is TILayer then
      Result := TILayer(AObject);
  end;
end;

function TLayersInspector.GetLayerItem(Layer: TILayer): TLayerItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ItemsCount - 1 do
    if Items[i].Layer = Layer then
    begin
      Result := Items[i];
      Break;
    end;
end;

procedure TLayersInspector.InvalidateLayerThumb(AObject: TIObject);
var
  Item: TLayerItem;
  Layer: TILayer;
begin
  Layer := GetObjectLayer(AObject);
  if Layer <> nil then
  begin
    Item := GetLayerItem(Layer);
    if Item <> nil then
      Item.InvalidateThumb;
  end;
end;

function TLayersInspector.GetItemHeight: integer;
begin
  Result := Max(Abs(Font.Height), Max(VisibleGlyph.Height,
    Max(ActiveGlyph.Height, fItemHeight)));
  Inc(Result, Paddings * 2);
end;

procedure TLayersInspector.SetItemHeight(Value: integer);
begin
  Value := Max(1, Value);
  if fItemHeight = Value then
    exit;
  fItemHeight := Value;
  InvalidateLayers;
  UpdateScrollBars;
  Invalidate;
end;

function TLayersInspector.GetItem(Index: integer): TLayerItem;
begin
  Result := TLayerItem(fItems[Index]);
end;

function TLayersInspector.GetItemsCount: integer;
begin
  Result := fItems.Count;
end;

procedure TLayersInspector.SetPaddings(Value: integer);
begin
  Value := Max(1, Value);
  if fPaddings = Value then
    exit;
  fPaddings := Value;
  InvalidateLayers;
  UpdateScrollBars;
  Invalidate;
end;

function TLayersInspector.GetThumbHeight: integer;
begin
  Result := ItemHeight - 2 * Paddings;
end;

function TLayersInspector.GetThumbWidth: integer;
begin
  if Document <> nil then
    Result := Round(ItemHeight * Document.Width / Document.Height) - 2 * Paddings
  else
    Result := 0;
end;

function TLayersInspector.GetSelected(Index: integer): TILayer;
begin
  Result := TILayer(fSelectedList[Index]);
end;

function TLayersInspector.GetSelectedCount: integer;
begin
  Result := fSelectedList.Count;
end;

procedure TLayersInspector.SetActiveGlyph(const Value: TBitmap);
begin
  fActiveGlyph.Assign(Value);
  UpdateScrollBars;
  Invalidate;
end;

procedure TLayersInspector.SetVisibleGlyph(const Value: TBitmap);
begin
  fVisibleGlyph.Assign(Value);
  UpdateScrollBars;
  Invalidate;
end;

procedure TLayersInspector.SetLockedGlyph(const Value: TBitmap);
begin
  fLockedGlyph.Assign(Value);
  UpdateScrollBars;
  Invalidate;
end;

procedure TLayersInspector.CMFocusChanged(var msg: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TLayersInspector.WMLButtonDown(var msg: TLMLButtonDown);
var
  i: integer;
  r: TRect;
  Item: TLayerItem;
  ShiftState: TShiftState;
begin
  Item := ItemAtPos(msg.XPos, msg.YPos);
  if Assigned(Item) then
  begin
    i := fItems.IndexOf(Item);
    r := ItemRect(i);
    r.Left += Paddings;
    r.Right := r.Left + VisibleGlyph.Width + Paddings;
    if PtInRect(r, Point(msg.XPos + OriginX, msg.YPos + OriginY)) then
    begin
      Item.Layer.Visible := not Item.Layer.Visible;
    end
    else
    begin
      ShiftState := KeysToShiftState(msg.Keys);
      if ssCtrl in ShiftState then
      begin
        if not LayerSelected(Item.Layer) then
          AddToSelection(Item.Layer, True)
        else
          RemoveFromSelection(Item.Layer);
      end
      else
        Select(Item.Layer);
    end;
  end
  else
    ClearSelection;
  inherited;
end;

procedure TLayersInspector.WMKeyDown(var msg: TLMKeyDown);
begin
  if (msg.CharCode = VK_DELETE) and Assigned(Document) and (SelectedCount > 0) then
    DeleteSelected;
end;

procedure TLayersInspector.Paint;
var
  i, X, Y: integer;
  ts: TTextStyle;
  ARect, DrawRect: TRect;
begin
  for i := 0 to ItemsCount - 1 do
  begin
    Canvas.Brush.Style := FPCanvas.bsSolid;
    Canvas.Font := Font;
    Canvas.Brush.Color := IfThen(LayerSelected(Items[i].Layer),
      IfThen(Focused, clHighlight, clInactiveBorder), Color);
    ARect := ItemRect(i);
    OffsetRect(ARect, -OriginX, -OriginY);
    Canvas.FillRect(ARect);
    X := ARect.Left + Paddings;
    Y := ARect.Top + (ARect.Bottom - ARect.Top - fVisibleGlyph.Height) div 2;
    if Items[i].Layer.Visible then
      Canvas.Draw(X, Y, VisibleGlyph);
    X := X + fVisibleGlyph.Width + Paddings * 2;
    Y := ARect.Top + (ARect.Bottom - ARect.Top - fActiveGlyph.Height) div 2;
    if Items[i].Layer = Document.ActiveLayer then
      Canvas.Draw(X, Y, ActiveGlyph);
    DrawRect.Left := X + ActiveGlyph.Width + Paddings * 2;
    DrawRect.Top := ARect.Top + Paddings;
    DrawRect.Right := DrawRect.Left + ThumbWidth + 1;
    DrawRect.Bottom := DrawRect.Top + ThumbHeight + 1;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(DrawRect);
    Canvas.Draw(DrawRect.Left, DrawRect.Top, Items[i].Thumb);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.EndCap := pecRound;
    Canvas.Pen.Cosmetic := True;
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(DrawRect);
    DrawRect.Left := DrawRect.Right + Paddings * 2;
    DrawRect.Right := ClientWidth - fLockedGlyph.Width - Paddings * 3;
    Canvas.Font.Color := IfThen(LayerSelected(Items[i].Layer),
      IfThen(Focused, clHighlightText, Font.Color), Font.Color);
    ts := Canvas.TextStyle;
    ts.EndEllipsis := True;
    ts.Layout := tlCenter;
    Canvas.TextRect(DrawRect, DrawRect.Left, DrawRect.Top, Items[i].Layer.Name, ts);
    X := ClientWidth - fLockedGlyph.Width - Paddings * 2;
    Y := ARect.Top + (ARect.Bottom - ARect.Top - fLockedGlyph.Height) div 2;
    if Items[i].Layer.Locked then
      Canvas.Draw(X, Y, fLockedGlyph);
    Canvas.Pen.Style := psDot;
    Canvas.Pen.EndCap := pecFlat;
    Canvas.Pen.Cosmetic := False;
    Canvas.Pen.Color := cl3DShadow;
    Canvas.Line(0, ARect.Bottom, ARect.Right, ARect.Bottom);
    if i > 0 then
      Canvas.Line(0, ARect.Top, ARect.Right, ARect.Top);
  end;
end;

function TLayersInspector.AreaWidth: integer;
var
  i, TextWidth, W: integer;
begin
  Result := 0;
  if Assigned(Document) then
  begin
    Canvas.Font := Font;
    TextWidth := 0;
    for i := 0 to Document.LayersCount - 1 do
    begin
      W := Canvas.TextWidth(Document.Layers[i].Name);
      if W > TextWidth then
        TextWidth := W;
    end;
    Result := VisibleGlyph.Width + ActiveGlyph.Width + ThumbWidth +
      TextWidth + LockedGlyph.Width + Paddings * 8;
  end;
end;

function TLayersInspector.AreaHeight: integer;
begin
  Result := 0;
  if Assigned(Document) then
    Result := Document.LayersCount * ItemHeight;
end;

function TLayersInspector.ScrollHLine: integer;
begin
  Result := 20;
end;

function TLayersInspector.ScrollVLine: integer;
begin
  Result := ItemHeight;
end;

procedure TLayersInspector.ObjectAdded(AObject: TIObject);
begin
  InvalidateLayerThumb(AObject);
  Invalidate;
end;

procedure TLayersInspector.ObjectRemove(AObject: TIObject);
begin
  InvalidateLayerThumb(AObject);
  Invalidate;
end;

procedure TLayersInspector.ObjectChanging(AObject: TIObject);
begin
  // nothing
end;

procedure TLayersInspector.ObjectChanged(AObject: TIObject);
begin
  InvalidateLayerThumb(AObject);
  Invalidate;
end;

procedure TLayersInspector.ObjectSelected(AObject: TIObject; Selected: boolean);
begin
  // nothing
end;

procedure TLayersInspector.ObjectCommandReceived(AObject: TIObject;
  var Command: TICmdMessage);
begin
  // nothing
end;

procedure TLayersInspector.ObjectCommandHandled(AObject: TIObject;
  var Command: TICmdMessage);
begin
  // nothing
end;

procedure TLayersInspector.DocumentResize(NewWidth, NewHeight: double);
begin
  InvalidateLayers;
  UpdateScrollBars;
  Invalidate;
end;

procedure TLayersInspector.DocumentDestroying;
begin
  Document := nil;
end;

procedure TLayersInspector.LayerAdded(ALayer: TILayer);
var
  Index: integer;
begin
  Index := Document.LayersCount - Document.IndexOfLayer(ALayer) - 1;
  fItems.Insert(Index, TLayerItem.Create(Self, ALayer));
  Select(ALayer);
  UpdateScrollBars;
  Invalidate;
end;

procedure TLayersInspector.LayerRemove(ALayer: TILayer);
var
  Item: TLayerItem;
begin
  Item := GetLayerItem(ALayer);
  fItems.Remove(Item);
  UpdateScrollBars;
  Invalidate;
end;

procedure TLayersInspector.LayerChanging(ALayer: TILayer);
begin
  // nothing
end;

procedure TLayersInspector.LayerChanged(ALayer: TILayer);
var
  Item: TLayerItem;
  OldIndex, NewIndex: integer;
begin
  Item := GetLayerItem(ALayer);
  OldIndex := fItems.IndexOf(Item);
  NewIndex := Document.LayersCount - Document.IndexOfLayer(ALayer) - 1;
  if OldIndex <> NewIndex then
  begin
    fItems.Move(OldIndex, NewIndex);
    UpdateScrollBars;
  end;
  Invalidate;
end;

procedure TLayersInspector.ActiveLayerChanged(ALayer: TILayer);
begin
  Invalidate;
  if Assigned(fOnActiveLayerChanged) then
    fOnActiveLayerChanged(self, ALayer);
end;

function TLayersInspector.ItemRect(Index: integer): TRect;
begin
  if Index <= ItemsCount then
  begin
    Result.Left := 0;
    Result.Top := Index * ItemHeight;
    Result.Right := Max(AreaWidth, ClientWidth);
    Result.Bottom := (Index + 1) * ItemHeight;
  end
  else
    TList.Error(SListIndexError, Index);
end;

function TLayersInspector.ItemAtPos(X, Y: integer): TLayerItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ItemsCount - 1 do
    if PtInRect(ItemRect(i), Point(X + OriginX, Y + OriginY)) then
    begin
      Result := Items[i];
      Break;
    end;
end;

procedure TLayersInspector.AddToSelection(ALayer: TILayer; MakePrimary: boolean);
var
  Index: integer;
begin
  Index := fSelectedList.IndexOf(ALayer);
  if Index < 0 then
    if MakePrimary then
      fSelectedList.Insert(0, ALayer)
    else
      fSelectedList.Add(ALayer)
  else
    fSelectedList.Move(Index, 0);
  if fSelectedList.Count > 0 then
    Document.ActiveLayer := Selected[0];
  Invalidate;
end;

procedure TLayersInspector.RemoveFromSelection(ALayer: TILayer);
begin
  if LayerSelected(ALayer) then
  begin
    fSelectedList.Remove(ALayer);
    Invalidate;
  end;
end;

procedure TLayersInspector.ClearSelection;
begin
  if SelectedCount = 0 then
    exit;
  fSelectedList.Clear;
  Invalidate;
end;

procedure TLayersInspector.DeleteSelected;
begin
  Document.DeleteLayers(fSelectedList);
end;

procedure TLayersInspector.Select(ALayer: TILayer);
begin
  ClearSelection;
  AddToSelection(ALayer, True);
end;

function TLayersInspector.LayerSelected(ALayer: TILayer): boolean;
begin
  Result := fSelectedList.IndexOf(ALayer) >= 0;
end;

end.
