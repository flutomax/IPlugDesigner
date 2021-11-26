{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIDesigner;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LMessages, Classes, Contnrs, SysUtils, Graphics, Controls, Forms,
  FPCanvas, Agg_LCL, uCommon, uIObject, uIDocument, uGraphics, uIMiscControls,
  agg_fpimage, uAGGCanvas;

const
  crRotate = TCursor(100);
  crShear = TCursor(101);
  crVertexDrag = TCursor(102);
  crAreaCross = TCursor(103);
  crAdd = TCursor(104);

type

  TGridStyle = (gtLines, gtDots);

  TIDesigner = class;

  { TInputController }

  TInputController = class(TINonRefInterfacedObject)
  private
    fHandled: boolean;
    fSnapToGrid: boolean;
    fDesigner: TIDesigner;
    fOnFindObject: TIObjectQueryEvent;
    fOnEndConstruct: TIObjectEvent;
    procedure SetGridSize(AValue: double);
    procedure WMMouseMove(var Msg: TLMMouseMove); message LM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
  protected
    fGridSize: double;
    fConstructObject: TIObject;
    procedure Attached(ADesigner: TIDesigner); virtual;
    procedure Detached(ADesigner: TIDesigner); virtual;
    function DecodeCursor(Code: cardinal): TCursor; virtual;
    procedure EditObjectText(AObject: TIObject); virtual;
    procedure DoEndConstruct; virtual;
  public
    constructor Create(ADesigner: TIDesigner); virtual;
    destructor Destroy; override;
    function FindObject(X, Y: integer; var HitTest: cardinal): TIObject; virtual;
    procedure DefaultHandler(var Message); override;
    procedure SendMessage(var Message: TLMessage; var Handled: boolean);
    property Designer: TIDesigner read fDesigner;
    property GridSize: double read fGridSize write SetGridSize;
    property SnapToGrid: boolean read fSnapToGrid write fSnapToGrid;
    property OnFindObject: TIObjectQueryEvent read fOnFindObject write fOnFindObject;
    property OnEndConstruct: TIObjectEvent read fOnEndConstruct write fOnEndConstruct;
  end;

  TInputControllerClass = class of TInputController;

  TIDropCopyKind = (ckClone, ckConstructFromPrototype);

  IDragSource = interface(IInterface)
    ['{1B09A376-60C7-4F21-8C99-C857AB73E082}']
    function GetCopyKind: TIDropCopyKind;
  end;

  IDragTarget = interface(IInterface)
    ['{E2902B7D-EFCF-4788-94B3-F4B07EF2339F}']
    function GetDragCursor(DragObject: TDragObject; Accepted: boolean;
      X, Y: integer): TCursor;
  end;

  { TIDragObject }

  TIDragObject = class(TDragControlObjectEx)
  private
    fList: TObjectList;
    function GetObject(Index: integer): TIObject;
    function GetObjectsCount: integer;
  protected
    procedure WndProc(var Msg: TLMessage);
    function GetDragCursor(Accepted: boolean; X, Y: integer): TCursor; override;
  public
    constructor Create(AControl: TControl; AList: TObjectList); reintroduce;
    property ObjectsCount: integer read GetObjectsCount;
    property Objects[Index: integer]: TIObject read GetObject;
    function GetCopyKind: TIDropCopyKind;
  end;

  TIPutObjectEvent = procedure(Sender: TObject; DragObject: TIObject;
    Index, X, Y: integer; var AOwner: TIObject; var Pos: TIFloatPoint) of object;

  { TEditController }

  TEditController = class(TInputController, IDragTarget)
  private
    fDC: HDC;
    fDragList: TObjectList;
    fStartPoint: TPoint;
    fFinishPoint: TPoint;
    fRotateMode: boolean;
    fConstructPrototype: TIObject;
    fSelectParent: TIObject;
    fOnPutObject: TIPutObjectEvent;
    fOnEndSelectRect: TNotifyEvent;
    fOnBeginSelectRect: TIObjectQueryEvent;
    procedure SetRotateMode(const Value: boolean);
    procedure DesignerStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure DesignerDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure DesignerDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure DesignerDragEnd(Sender, Target: TObject; X, Y: integer);
    procedure RQSGetMouseMode(var cmd: TICmdMessage); message RQS_GETMOUSEMODE;
    procedure WMMouseWheel(var msg: TLMMouseEvent); message LM_MOUSEWHEEL;
  protected
    fDragging: boolean;
    fConstructing: boolean;
    fSelecting: boolean;
    fTextEditing: boolean;
    fEditObject: TIObject;
    function DecodeCursor(Code: cardinal): TCursor; override;
    procedure Attached(ADesigner: TIDesigner); override;
    procedure Detached(ADesigner: TIDesigner); override;
    procedure DrawSelectRect(DC: HDC; X1, Y1, X2, Y2: integer); virtual;
    procedure EraseSelectRect(DC: HDC; X1, Y1, X2, Y2: integer); virtual;
    procedure UpdateSelection(X1, Y1, X2, Y2: integer); virtual;
    function GetDragCursor(DragObject: TDragObject; Accepted: boolean;
      X, Y: integer): TCursor; virtual;
    procedure DoDesignerStartDrag(Sender: TObject; var DragObject: TDragObject);
      virtual; abstract;
    procedure DoDesignerDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean); virtual; abstract;
    procedure DoDesignerDragDrop(Sender, Source: TObject; X, Y: integer);
      virtual; abstract;
    procedure DoDesignerDragEnd(Sender, Target: TObject; X, Y: integer);
      virtual; abstract;
  public
    constructor Create(ADesigner: TIDesigner); override;
    destructor Destroy; override;
    procedure ClearSelection; virtual;
    procedure Select(AObject: TIObject); virtual;
    procedure BeginSelectRect(X, Y: integer); virtual;
    procedure ProcessSelectRect(X, Y: integer; var ScrollX, ScrollY: integer); virtual;
    procedure EndSelectRect; virtual;
    procedure BeginDrag(X, Y: integer; ADragList: TObjectList;
      HitTest: cardinal); virtual;
    procedure Drag(X, Y: integer); virtual;
    procedure CancelDrag; virtual;
    procedure EndDrag; virtual;
    function ConstructPoint(AObject: TIObject; X, Y: integer): boolean; virtual;
    procedure ProcessConstruct(X, Y: integer); virtual;
    function StopConstruct: boolean; virtual;
    procedure PutObject(Prototype: TIObject; Index, X, Y: integer;
      CopyKind: TIDropCopyKind; PrimaryConstructKind: TIConstructKind;
      var AObject: TIObject);
    function RemoveObject(AObject: TIObject): boolean; virtual;
    procedure RemoveObjects(AObjects: TObjectList);
    procedure ApplySpecCommand(X, Y: integer); virtual;
    property ConstructPrototype: TIObject read fConstructPrototype
      write fConstructPrototype;
    property RotateMode: boolean read fRotateMode write SetRotateMode;
    property GridSize: double read fGridSize write fGridSize;
    property OnPutObject: TIPutObjectEvent read fOnPutObject write fOnPutObject;
    property OnBeginSelectRect: TIObjectQueryEvent
      read fOnBeginSelectRect write fOnBeginSelectRect;
    property OnEndSelectRect: TNotifyEvent read fOnEndSelectRect write fOnEndSelectRect;
  end;

  { TFinalEditController }

  TFinalEditController = class(TEditController)
  private
    fLockPopup: boolean;
    procedure WMMouseMove(var msg: TLMMouseMove); message LM_MOUSEMOVE;
    procedure WMLButtonDown(var msg: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var msg: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMLButtonDblClk(var msg: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var msg: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMRButtonUp(var msg: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMKeyDown(var msg: TLMKeyDown); message LM_KEYDOWN;
  protected
    procedure DoDesignerStartDrag(Sender: TObject; var DragObject: TDragObject);
      override;
    procedure DoDesignerDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean); override;
    procedure DoDesignerDragDrop(Sender, Source: TObject; X, Y: integer); override;
    procedure DoDesignerDragEnd(Sender, Target: TObject; X, Y: integer); override;
  end;

  { TOutputController }

  TOutputController = class(TINonRefInterfacedObject)
  private
    fDesigner: TIDesigner;
    fGridSize: double;
    fLargeGridSize: integer;
    fShowGrid: boolean;
    fStepSize: integer;
    fCoordList: TCoordList;
    fGridStyle: TGridStyle;
    procedure SetGridSize(const AValue: double);
    procedure SetGridStyle(AValue: TGridStyle);
    procedure SetShowGrid(AValue: boolean);
    procedure SetStepSize(const AValue: integer);
    procedure SetLargeGridSize(const AValue: integer);
  protected
    function CreateCanvas(ACanvas: TAggCanvas): TICanvas; virtual;
    function GetInvalidRect(AObject: TIObject): TRect; virtual;
    function GetViewport: TIFloatRect; virtual;
    function GetUnitsPerInch: double; virtual;
    function MaxVertexSize: integer; virtual;
    procedure Attached(ADesigner: TIDesigner); virtual;
    procedure Detached(ADesigner: TIDesigner); virtual;
    procedure DrawVertex(X, Y: integer; Kind: word); virtual;
    procedure DrawBackground(ACanvas: TAggCanvas; Workspace: TIFloatRect); virtual;
    procedure DrawGrid(ACanvas: TAggCanvas; Workspace: TIFloatRect); virtual;
    procedure DrawObjects(ACanvas: TAggCanvas); virtual;
    procedure DrawAdditionalInfo; virtual;
  public
    constructor Create(ADesigner: TIDesigner); virtual;
    destructor Destroy; override;
    property Designer: TIDesigner read fDesigner;
    property GridSize: double read fGridSize write SetGridSize;
    property StepSize: integer read fStepSize write SetStepSize;
    property LargeGridSize: integer read fLargeGridSize write SetLargeGridSize;
    property ShowGrid: boolean read fShowGrid write SetShowGrid;
    property GridStyle: TGridStyle read fGridStyle write SetGridStyle;
  end;

  TOutputControllerClass = class of TOutputController;

  TScaleMode = (smManual, smFitX, smFitY, smFitXY);

  { TIDesigner }

  TIDesigner = class(TCustomControl, ICoordTransform, IDocumentObserver,
    IRequestHandler, ILayeredDocumentObserver)
  private
    fDocument: TIDocument;
    fScale: double;
    fOriginX: double;
    fOriginY: double;
    fTopRow: double;
    fLeftCol: double;
    fTolerance: integer;
    fScrollSpeed: integer;
    fBorderStyle: TBorderStyle;
    fInputController: TInputController;
    fOutputController: TOutputController;
    fScaleMode: TScaleMode;
    fBitmap: TBitmap;
    fCanvas: TAggCanvas;
    fOnScroll: TNotifyEvent;
    fOnScaleChanged: TNotifyEvent;
    fOnObjectChanging: TIObjectEvent;
    fOnObjectRemove: TIObjectEvent;
    fOnObjectAdded: TIObjectEvent;
    fOnObjectChanged: TIObjectEvent;
    fOnObjectSelect: TISelectEvent;
    function GetAutoScale: double;
    function GetResolution: integer;
    function GetViewport: TIFloatRect;
    function GetUnitsPerInch: double;
    procedure SetDocument(AValue: TIDocument);
    procedure SetInputController(AValue: TInputController);
    procedure SetOutputController(AValue: TOutputController);
    procedure SetScale(AValue: double);
    procedure SetScaleMode(AValue: TScaleMode);
    procedure SetScaleValue(const AValue: double);
    procedure UpdateAutoScale;
    procedure UpdateMouse;
    procedure WMHScroll(var msg: TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var msg: TLMVScroll); message LM_VScroll;
    procedure WMEraseBkGnd(var msg: TLMEraseBkgnd); message LM_ERASEBKGND;
  protected
    procedure WndProc(var Message: TLMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoScaleChanged; dynamic;
    procedure RecalcGrid;
    procedure SetTopRow(ATopRow: double);
    procedure SetLeftCol(ALeftCol: double);
    procedure InvalidateObjectRect(AObject: TIObject);
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
    procedure RequestHandler(AObject: TIObject; var Command: TICmdMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LogToScreen(lX, lY: double; var sX, sY: double); overload;
    procedure ScreenToLog(sX, sY: double; var lX, lY: double); overload;
    function LogToScreen(Value: double): double; overload;
    function ScreenToLog(Value: double): double; overload;
    procedure ResetScrollBars;
    procedure UpdateScrollBars;
    procedure ScrollBy(var Dx, Dy: integer);
    property Document: TIDocument read fDocument write SetDocument;
    property Viewport: TIFloatRect read GetViewport;
    property Resolution: integer read GetResolution;
    property UnitsPerInch: double read GetUnitsPerInch;
    property InputController: TInputController
      read fInputController write SetInputController;
    property OutputController: TOutputController
      read fOutputController write SetOutputController;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Enabled;
    property Height;
    property PopupMenu;
    property Visible;
    property Width;
    property BorderStyle: TBorderStyle
      read fBorderStyle write SetBorderStyle default bsSingle;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnResize;
    property OnContextPopup;
    property Scale: double read fScale write SetScale;
    property ScaleMode: TScaleMode read fScaleMode write SetScaleMode;
    property Tolerance: integer read fTolerance write fTolerance;
    property ScrollSpeed: integer read fScrollSpeed write fScrollSpeed;
    property OnScroll: TNotifyEvent read fOnScroll write fOnScroll;
    property OnScaleChanged: TNotifyEvent read fOnScaleChanged write fOnScaleChanged;
    property OnObjectAdded: TIObjectEvent read fOnObjectAdded write fOnObjectAdded;
    property OnObjectRemove: TIObjectEvent read fOnObjectRemove write fOnObjectRemove;
    property OnObjectChanging: TIObjectEvent
      read fOnObjectChanging write fOnObjectChanging;
    property OnObjectChanged: TIObjectEvent read fOnObjectChanged
      write fOnObjectChanged;
    property OnObjectSelect: TISelectEvent read fOnObjectSelect write fOnObjectSelect;
  end;

implementation

{$R cursors.res}

uses
  LCLIntf, Math, DelphiCompat;

{ TIDragObject }

constructor TIDragObject.Create(AControl: TControl; AList: TObjectList);
begin
  inherited Create(AControl);
  fList := AList;
end;

function TIDragObject.GetObject(Index: integer): TIObject;
begin
  Result := fList[Index] as TIObject;
end;

function TIDragObject.GetObjectsCount: integer;
begin
  Result := fList.Count;
end;

procedure TIDragObject.WndProc(var Msg: TLMessage);
var
  P: TPoint;
begin
  inherited;
  if (Msg.Msg >= LM_MOUSEFIRST) and (Msg.Msg <= LM_MOUSELAST) then
  begin
    P := SmallPointToPoint(TLMMouse(Msg).Pos);
    ClientToScreen(GetCapture, P);
    P := Control.ScreenToClient(P);
    TLMMouse(Msg).Pos := PointToSmallPoint(P);
    Control.Perform(Msg.Msg, Msg.WParam, Msg.LParam);
  end;
end;

function TIDragObject.GetDragCursor(Accepted: boolean; X, Y: integer): TCursor;
var
  Target: TControl;
  Intf: IDragTarget;
begin
  Result := crNoDrop;
  Target := FindDragTarget(Point(X, Y), False);
  if Assigned(Target) then
  begin
    if Supports(Target, IDragTarget, Intf) then
      Result := Intf.GetDragCursor(Self, Accepted, X, Y)
    else
      Result := inherited GetDragCursor(Accepted, X, Y);
  end;
end;

function TIDragObject.GetCopyKind: TIDropCopyKind;
var
  Intf: IDragSource;
begin
  Result := ckClone;
  if Supports(Control, IDragSource, Intf) then
    Result := Intf.GetCopyKind;
end;


{ TInputController }

constructor TInputController.Create(ADesigner: TIDesigner);
begin
  inherited Create;
  fDesigner := ADesigner;
  fSnapToGrid := True;
  fGridSize := 10;
end;

destructor TInputController.Destroy;
begin
  if (Designer <> nil) and (Designer.InputController = Self) then
    Designer.InputController := nil;
  inherited Destroy;
end;

procedure TInputController.WMMouseMove(var Msg: TLMMouseMove);
var
  Obj: TIObject;
  HitTest, Code: cardinal;
begin
  Obj := FindObject(Msg.XPos, Msg.YPos, HitTest);
  if Assigned(Obj) then
    if Obj.Selected then
    begin
      Code := Obj.SendCommand(CMD_GETCURSOR, 0, HitTest);
      Designer.Cursor := DecodeCursor(Code);
      Exit;
    end;
  Designer.Cursor := crDefault;
end;

procedure TInputController.SetGridSize(AValue: double);
begin
  if fGridSize = AValue then
    Exit;
  fGridSize := AValue;
end;

procedure TInputController.WMMouseWheel(var Msg: TLMMouseEvent);
var
  Dx, Dy: integer;
begin
  Dx := 0;
  Dy := Round(-Msg.WheelDelta / 100 * Designer.ScrollSpeed);
  Designer.ScrollBy(Dx, Dy);
end;

procedure TInputController.Attached(ADesigner: TIDesigner);
begin
  // nothing
end;

procedure TInputController.Detached(ADesigner: TIDesigner);
begin
  // nothing
end;

function TInputController.DecodeCursor(Code: cardinal): TCursor;
begin
  Result := crDefault;
end;

procedure TInputController.EditObjectText(AObject: TIObject);
begin
  // nothing
end;

procedure TInputController.DoEndConstruct;
begin
  if Assigned(fOnEndConstruct) then
    fOnEndConstruct(self, fConstructObject);
end;

procedure TInputController.DefaultHandler(var Message);
begin
  fHandled := False;
end;

function TInputController.FindObject(X, Y: integer; var HitTest: cardinal): TIObject;
var
  Params: TIHitTestParams;
  AObject, Root: TIObject;
  Res: cardinal;
begin
  Result := nil;
  HitTest := HT_OUT;
  if Assigned(Designer.Document) then
  begin
    Params.XPos := X;
    Params.YPos := Y;
    Params.TranslateIntf := Designer;
    Params.Tolerance := Designer.Tolerance;
    Root := Designer.Document.RootObject;
    if Assigned(fOnFindObject) then
      fOnFindObject(Self, Root);
    if Root = nil then
      exit;
    Res := Root.HitTest(@Params, AObject);
    if Res <> HT_OUT then
    begin
      HitTest := Res;
      Result := AObject;
    end;
  end;
end;

procedure TInputController.SendMessage(var Message: TLMessage; var Handled: boolean);
begin
  fHandled := True;
  Dispatch(Message);
  Handled := fHandled;
end;


{ TEditController }

constructor TEditController.Create(ADesigner: TIDesigner);
begin
  inherited Create(ADesigner);
  fDragList := TObjectList.Create(False);
end;

destructor TEditController.Destroy;
begin
  fDragList.Free;
  inherited Destroy;
end;

procedure TEditController.SetRotateMode(const Value: boolean);
begin
  if fRotateMode = Value then
    exit;
  fRotateMode := Value;
  Designer.Invalidate;
end;

procedure TEditController.DesignerStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DoDesignerStartDrag(Sender, DragObject);
end;

procedure TEditController.DesignerDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
begin
  DoDesignerDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TEditController.DesignerDragDrop(Sender, Source: TObject; X, Y: integer);
begin
  DoDesignerDragDrop(Sender, Source, X, Y);
end;

procedure TEditController.DesignerDragEnd(Sender, Target: TObject; X, Y: integer);
begin
  DoDesignerDragEnd(Sender, Target, X, Y);
end;

procedure TEditController.RQSGetMouseMode(var cmd: TICmdMessage);
begin
  cmd.Result := 1;
  cmd.Param1 := 0;
  cmd.Param2 := 0;
end;

procedure TEditController.WMMouseWheel(var msg: TLMMouseEvent);
var
  ScrollX, ScrollY: integer;
  Pt: TPoint;
begin
  if fSelecting then
  begin
    ScrollX := 0;
    ScrollY := Round(-msg.WheelDelta / 100 * Designer.ScrollSpeed);
    Pt := Designer.ScreenToClient(Point(msg.X, msg.Y));
    ProcessSelectRect(Pt.X, Pt.Y, ScrollX, ScrollY);
  end
  else
    inherited;
end;

function TEditController.DecodeCursor(Code: cardinal): TCursor;
begin
  case Code of
    CR_ARROW: Result := crArrow;
    CR_SIZEALL: Result := crSizeAll;
    CR_HORIZONTAL: Result := crSizeWE;
    CR_VERTICAL: Result := crSizeNS;
    CR_DIAG1: Result := crSizeNWSE;
    CR_DIAG2: Result := crSizeNESW;
    CR_ROTATE: Result := crRotate;
    CR_SHEAR: Result := crShear;
    else
      Result := crDefault;
  end;
end;

procedure TEditController.Attached(ADesigner: TIDesigner);
begin
  ADesigner.DragMode := dmManual;
  ADesigner.DragKind := dkDrag;
  ADesigner.OnStartDrag := @DesignerStartDrag;
  ADesigner.OnDragOver := @DesignerDragOver;
  ADesigner.OnDragDrop := @DesignerDragDrop;
  ADesigner.OnEndDrag := @DesignerDragEnd;
end;

procedure TEditController.Detached(ADesigner: TIDesigner);
begin
  ADesigner.OnStartDrag := nil;
  ADesigner.OnDragOver := nil;
  ADesigner.OnDragDrop := nil;
  ADesigner.OnEndDrag := nil;
end;

procedure TEditController.DrawSelectRect(DC: HDC; X1, Y1, X2, Y2: integer);
var
  Canvas: TCanvas;
begin
  if (X1 <> X2) or (Y1 <> Y2) then
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      SetBrushOrgEx(fDC, X1, Y1, nil);
      Canvas.DrawFocusRect(Rect(X1, Y1, X2 + 1, Y2 + 1));
    finally
      Canvas.Free;
    end;
  end;
end;

procedure TEditController.EraseSelectRect(DC: HDC; X1, Y1, X2, Y2: integer);
begin
  DrawSelectRect(DC, X1, Y1, X2, Y2);
end;

procedure TEditController.UpdateSelection(X1, Y1, X2, Y2: integer);
var
  i: integer;
  Bounds: TIFloatRect;
  X, Y: double;
  ObjRect, SelRect, DstRect: TRect;
begin
  SelRect := Rect(Min(fStartPoint.X, fFinishPoint.X),
    Min(fStartPoint.Y, fFinishPoint.Y), Max(fStartPoint.X, fFinishPoint.X),
    Max(fStartPoint.Y, fFinishPoint.Y));
  if Assigned(fSelectParent) then
    for i := 0 to fSelectParent.ChildsCount - 1 do
    begin
      Bounds := fSelectParent.Childs[i].GetBounds(True);
      Designer.LogToScreen(Bounds.Left, Bounds.Top, X, Y);
      ObjRect.Left := Trunc(X);
      ObjRect.Top := Trunc(Y);
      Designer.LogToScreen(Bounds.Right, Bounds.Bottom, X, Y);
      ObjRect.Right := Ceil(X);
      ObjRect.Bottom := Ceil(Y);
      IntersectRect(DstRect, ObjRect, SelRect);
      fSelectParent.Childs[i].Selected := not IsRectEmpty(DstRect);
    end;
  Designer.Update;
end;

function TEditController.GetDragCursor(DragObject: TDragObject;
  Accepted: boolean; X, Y: integer): TCursor;
begin
  Result := crNoDrop;
  if (DragObject is TIDragObject) then
  begin
    if TIDragObject(DragObject).Control = Designer then
      Result := Designer.Cursor
    else
    if Accepted then
      Result := crAdd;
  end;
end;

procedure TEditController.ClearSelection;
var
  i: integer;
begin
  if Assigned(Designer.Document) then
    with Designer.Document do
      for i := SelectedCount - 1 downto 0 do
        Selected[i].Selected := False;
end;

procedure TEditController.Select(AObject: TIObject);
begin
  if Designer.Document = nil then
    exit;
  ClearSelection;
  AObject.Selected := True;
end;

procedure TEditController.BeginSelectRect(X, Y: integer);
const
  DCX_CACHE = $2;
  DCX_LOCKWINDOWUPDATE = $400;
begin
  ClearSelection;
  Designer.Cursor := crAreaCross;
  fSelecting := True;
  fStartPoint := Point(X, Y);
  fFinishPoint := fStartPoint;
  SetCapture(Designer.Handle);
  fDC := GetDCEx(Designer.Handle, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  fSelectParent := Designer.Document.RootObject;
  if Assigned(fOnBeginSelectRect) then
    fOnBeginSelectRect(Self, fSelectParent);
end;

procedure TEditController.ProcessSelectRect(X, Y: integer;
  var ScrollX, ScrollY: integer);
var
  X1, Y1, X2, Y2: integer;
  ClipRect: TRect;
  Rgn: HRGN;

  procedure CalcCoord;
  begin
    X1 := Min(fStartPoint.X, fFinishPoint.X);
    Y1 := Min(fStartPoint.Y, fFinishPoint.Y);
    X2 := Max(fStartPoint.X, fFinishPoint.X);
    Y2 := Max(fStartPoint.Y, fFinishPoint.Y);
  end;

begin
  ClipRect := Designer.ClientRect;
  Designer.ScrollBy(ScrollX, ScrollY);
  if (ScrollX <> 0) or (ScrollY <> 0) then
  begin
    Designer.Update;
    fStartPoint.X := fStartPoint.X - ScrollX;
    fStartPoint.Y := fStartPoint.Y - ScrollY;
    fFinishPoint.X := fFinishPoint.X - ScrollX;
    fFinishPoint.Y := fFinishPoint.Y - ScrollY;
    if ScrollX > 0 then
      ClipRect.Right := ClipRect.Right - ScrollX
    else
      ClipRect.Left := ClipRect.Left - ScrollX;
    if ScrollY > 0 then
      ClipRect.Bottom := ClipRect.Bottom - ScrollY
    else
      ClipRect.Top := ClipRect.Top - ScrollY;
  end;
  CalcCoord;
  Rgn := CreateRectRgnIndirect(ClipRect);
  SelectClipRgn(fDC, Rgn);
  try
    EraseSelectRect(fDC, X1, Y1, X2, Y2);
  finally
    SelectClipRgn(fDC, 0);
    DeleteObject(Rgn);
  end;
  fFinishPoint := Point(X, Y);
  CalcCoord;
  DrawSelectRect(fDC, X1, Y1, X2, Y2);
  if (ScrollX <> 0) or (ScrollY <> 0) then
    Designer.Update;
end;

procedure TEditController.EndSelectRect;
var
  X1, Y1, X2, Y2: integer;
begin
  X1 := Min(fStartPoint.X, fFinishPoint.X);
  Y1 := Min(fStartPoint.Y, fFinishPoint.Y);
  X2 := Max(fStartPoint.X, fFinishPoint.X);
  Y2 := Max(fStartPoint.Y, fFinishPoint.Y);
  EraseSelectRect(fDC, X1, Y1, X2, Y2);
  UpdateSelection(X1, Y1, X2, Y2);
  fSelecting := False;
  ReleaseCapture;
  ReleaseDC(Designer.Handle, fDC);
  Designer.Cursor := crDefault;
  if Assigned(fOnEndSelectRect) then
    fOnEndSelectRect(Self);
end;

procedure TEditController.BeginDrag(X, Y: integer; ADragList: TObjectList;
  HitTest: cardinal);
var
  StartPos: TIFloatPoint;
  i: integer;
begin
  fDragList.Assign(ADragList);
  Designer.ScreenToLog(X, Y, StartPos.X, StartPos.Y);
  if fSnapToGrid then
    StartPos := PointToGrid(StartPos, GridSize);
  for i := 0 to fDragList.Count - 1 do
    TIObject(fDragList[i]).SendCommand(CMD_BEGINDRAG, HitTest, LParam(@StartPos));
  Designer.Document.BeginDrag(fDragList, HitTest and $FFFF0000);
  fDragging := True;
end;

procedure TEditController.Drag(X, Y: integer);
var
  Pos: TIFloatPoint;
  i: integer;
begin
  Designer.ScreenToLog(X, Y, Pos.X, Pos.Y);
  if fSnapToGrid then
    Pos := PointToGrid(Pos, GridSize);
  for i := 0 to fDragList.Count - 1 do
    TIObject(fDragList[i]).SendCommand(CMD_DRAG, 0, LParam(@Pos));
  Designer.Update;
end;

procedure TEditController.CancelDrag;
var
  i: integer;
begin
  fDragging := False;
  for i := 0 to fDragList.Count - 1 do
    TIObject(fDragList[i]).SendCommand(CMD_CANCELDRAG, 0, 0);
  fDragList.Clear;
end;

procedure TEditController.EndDrag;
var
  i: integer;
begin
  fDragging := False;
  for i := 0 to fDragList.Count - 1 do
    TIObject(fDragList[i]).SendCommand(CMD_ENDDRAG, 0, 0);
  Designer.Document.EndDrag(fDragList);
  fDragList.Clear;
end;

function TEditController.ConstructPoint(AObject: TIObject; X, Y: integer): boolean;
var
  Pos: TIFloatPoint;
begin
  Result := False;
  if fConstructing and (AObject <> fConstructObject) then
    Exit;
  Designer.ScreenToLog(X, Y, Pos.X, Pos.Y);
  if fSnapToGrid then
    Pos := PointToGrid(Pos, fGridSize);
  if AObject.SendCommand(CMD_CONSTRUCTPOINT, 0, LParam(@Pos)) = 0 then
  begin
    fConstructing := True;
    fConstructObject := AObject;
  end
  else
  begin
    fConstructing := False;
    DoEndConstruct;
    fConstructObject := nil;
  end;
  Result := fConstructing;
end;

procedure TEditController.ProcessConstruct(X, Y: integer);
var
  Pos: TIFloatPoint;
begin
  if fConstructing and Assigned(fConstructObject) then
  begin
    Designer.ScreenToLog(X, Y, Pos.X, Pos.Y);
    if fSnapToGrid then
      Pos := PointToGrid(Pos, fGridSize);
    fConstructObject.SendCommand(CMD_PROCESSCONSTRUCT, 0, LParam(@Pos));
  end;
end;

function TEditController.StopConstruct: boolean;
begin
  Result := True;
  if fConstructing then
  begin
    if ckOneShort in fConstructObject.ConstructKind then
    begin
      Result := fConstructObject.SendCommand(CMD_ENDCONSTRUCT, 0, 0) = 0;
      fConstructing := False;
      DoEndConstruct;
      exit;
    end;
    Result := fConstructObject.SendCommand(CMD_STOPCONSTRUCT, 0, 0) = 0;
    fConstructing := False;
    fConstructObject := nil;
  end;
end;

procedure TEditController.PutObject(Prototype: TIObject; Index, X, Y: integer;
  CopyKind: TIDropCopyKind; PrimaryConstructKind: TIConstructKind;
  var AObject: TIObject);
var
  lX, lY: double;
  Pos, DropAt: TIFloatPoint;
  AOwner: TIObject;
  Instantly, StepByStep: boolean;
begin
  AObject := nil;
  if Assigned(Designer.Document) then
  begin
    AOwner := Designer.Document.RootObject;
    Designer.ScreenToLog(X, Y, lX, lY);
    Pos := TIFloatPoint.Create(lX, lY);
    if fSnapToGrid then
      Pos := PointToGrid(Pos, GridSize);
    DropAt := Pos;
    if Assigned(fOnPutObject) then
      fOnPutObject(Self, Prototype, Index, X, Y, AOwner, DropAt);
    if CopyKind = ckClone then
    begin
      Instantly := True;
      StepByStep := False;
    end
    else
    begin
      if PrimaryConstructKind = ckInstantly then
      begin
        Instantly := ckInstantly in Prototype.ConstructKind;
        StepByStep := (not Instantly) and (ckStepByStep in Prototype.ConstructKind);
      end
      else
      begin
        StepByStep := (ckStepByStep in Prototype.ConstructKind) or
          (ckOneShort in Prototype.ConstructKind);
        Instantly := (not StepByStep) and (ckInstantly in Prototype.ConstructKind);
      end;
    end;
    if Instantly then
      AObject := Prototype.CloneAt(nil, DropAt.X, DropAt.Y)
    else
    if StepByStep then
    begin
      AObject := TIObjectClass(Prototype.ClassType).Create(nil, fDesigner.Document);
      ConstructPoint(AObject, X, Y);
    end;
    if Assigned(AObject) then
    begin
      AObject.Selected := True;
      AObject.Owner := AOwner;
      if Instantly then
      begin
        fConstructObject := AObject;
        DoEndConstruct;
        fConstructObject := nil;
      end;
    end;
  end;
end;

function TEditController.RemoveObject(AObject: TIObject): boolean;
begin
  if AObject.IsLocked then
    exit(False);
  AObject.Free;
  Result := True;
end;

procedure TEditController.RemoveObjects(AObjects: TObjectList);
var
  Obj: TIObject;
  i: integer;
begin
  for i := AObjects.Count - 1 downto 0 do
  begin
    Obj := TIObject(AObjects[i]);
    if RemoveObject(Obj) then
      AObjects.Remove(Obj);
  end;
end;

procedure TEditController.ApplySpecCommand(X, Y: integer);
var
  Obj: TIObject;
  HitTest: cardinal;
  CmdID: integer;
  Pos: TIFloatPoint;
begin
  Obj := FindObject(X, Y, HitTest);
  if Assigned(Obj) then
  begin
    Designer.ScreenToLog(X, Y, Pos.X, Pos.Y);
    CmdID := Obj.SendCommand(CMD_SPECCOMMAND, HitTest, LParam(@Pos));
  end;
end;


{ TFinalEditController }

procedure TFinalEditController.WMMouseMove(var msg: TLMMouseMove);
var
  ScrollX, ScrollY: integer;
  Pt: TPoint;
begin
  if fTextEditing then
  begin
    Designer.Cursor := crDefault;
    Exit;
  end;
  if fDragging then
    Drag(msg.XPos, msg.YPos)
  else
  if fConstructing then
  begin
    ProcessConstruct(msg.XPos, msg.YPos);
    Designer.Cursor := crAreaCross;
  end
  else
  if fSelecting then
  begin
    ScrollX := 0;
    ScrollY := 0;
    if msg.XPos <= 0 then
      ScrollX := msg.XPos;
    if msg.XPos > Designer.ClientWidth then
      ScrollX := (msg.XPos - Designer.ClientWidth);
    if msg.YPos <= 0 then
      ScrollY := msg.YPos;
    if msg.YPos > Designer.ClientHeight then
      ScrollY := (msg.YPos - Designer.ClientHeight);
    ProcessSelectRect(msg.XPos, msg.YPos, ScrollX, ScrollY);
    if (ScrollX <> 0) or (ScrollY <> 0) then
    begin
      Sleep(20);
      GetCursorPos(Pt);
      Pt := Designer.ScreenToClient(Pt);
      PostMessage(Designer.Handle, LM_MOUSEMOVE, 0, MakeLParam(Pt.X, Pt.Y));
    end;
  end
  else
  if (ConstructPrototype <> nil) and Assigned(Designer.Document) then
    Designer.Cursor := crAdd
  else
    inherited;
end;

procedure TFinalEditController.WMLButtonDown(var msg: TLMLButtonDown);
var
  i: integer;
  List: TObjectList;
  Obj, NewObject: TIObject;
  HitTest: cardinal;
begin
  if fTextEditing then
  begin
    DefaultHandler(msg);
    Exit;
  end;
  if fConstructing then
    ConstructPoint(fConstructObject, msg.XPos, msg.YPos)
  else
  begin
    if ConstructPrototype <> nil then
    begin
      ClearSelection;
      PutObject(ConstructPrototype, 0, msg.XPos, msg.YPos,
        ckConstructFromPrototype, ckStepByStep, NewObject);
    end
    else
    begin
      Obj := FindObject(msg.XPos, msg.YPos, HitTest);
      if Assigned(Obj) then
      begin
        if (msg.Keys and MK_CONTROL) <> 0 then
        begin
          if Obj.Selected then
            Obj.Selected := False
          else
            Obj.Selected := True;
        end
        else
        begin
          if Obj.Selected then
          begin
            List := TObjectList.Create(False);
            try
              if (HitTest and $FFFF0000) = HT_IN then
              begin
                for i := 0 to Designer.Document.SelectedCount - 1 do
                  List.Add(Designer.Document.Selected[i]);
                Designer.BeginDrag(Mouse.DragImmediate, Mouse.DragThreshold);
              end
              else
              begin
                List.Add(Obj);
                SetCapture(Designer.Handle);
              end;
              BeginDrag(msg.XPos, msg.YPos, List, HitTest);
            finally
              List.Free;
            end;
          end
          else
            Select(Obj);
        end;
      end
      else
      begin
        if Assigned(Designer.Document) and (not fTextEditing) then
        begin
          ClearSelection;
          BeginSelectRect(msg.XPos, msg.YPos);
        end;
      end;
    end;
  end;
end;

procedure TFinalEditController.WMLButtonUp(var msg: TLMLButtonUp);
begin
  if Assigned(fConstructPrototype) then
    if not (ckStepByStep in fConstructPrototype.ConstructKind) and fConstructing then
      StopConstruct;
  if fDragging then
  begin
    EndDrag;
    ReleaseCapture;
  end;
  if fSelecting then
    EndSelectRect;
end;

procedure TFinalEditController.WMLButtonDblClk(var msg: TLMLButtonDblClk);
begin
  ApplySpecCommand(msg.XPos, msg.YPos);
end;

procedure TFinalEditController.WMRButtonDown(var msg: TLMRButtonDown);
begin
  fLockPopup := False;
  if fDragging then
  begin
    CancelDrag;
    ReleaseCapture;
    fLockPopup := True;
  end;
  if fConstructing then
  begin
    if Assigned(fConstructObject) and (ckStepByStep in
      fConstructObject.ConstructKind) then
      DoEndConstruct;
    StopConstruct;
    fLockPopup := True;
  end;
end;

procedure TFinalEditController.WMRButtonUp(var msg: TLMRButtonUp);
begin
  if not fLockPopup then
    DefaultHandler(msg);
end;

procedure TFinalEditController.WMKeyDown(var msg: TLMKeyDown);
begin
  if (msg.CharCode = VK_DELETE) and Assigned(Designer.Document) and
    (Designer.Document.SelectedCount <> 0) then
  begin
    while Designer.Document.SelectedCount <> 0 do
      if not RemoveObject(Designer.Document.Selected[0]) then
        break;
  end;
end;

procedure TFinalEditController.DoDesignerStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragObject := TIDragObject.Create(Designer, fDragList);
end;

procedure TFinalEditController.DoDesignerDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := (Source is TIDragObject) and (Assigned(Designer.Document));
end;

procedure TFinalEditController.DoDesignerDragDrop(Sender, Source: TObject;
  X, Y: integer);
var
  i: integer;
  DragObject: TIDragObject;
  AObject: TIObject;
begin
  Designer.SetFocus;
  if Source is TIDragObject then
  begin
    DragObject := TIDragObject(Source);
    if DragObject.Control = Sender then
      Exit;
    ClearSelection;
    for i := 0 to DragObject.ObjectsCount - 1 do
    begin
      PutObject(DragObject.Objects[i], i, X, Y, DragObject.GetCopyKind,
        ckInstantly, AObject);
      if Assigned(AObject) and (osConstructing in AObject.State) then
        Break;
    end;
  end;
end;

procedure TFinalEditController.DoDesignerDragEnd(Sender, Target: TObject;
  X, Y: integer);
begin
  if Target <> Designer then
    CancelDrag;
end;


{ TOutputController }

constructor TOutputController.Create(ADesigner: TIDesigner);
begin
  inherited Create;
  fDesigner := ADesigner;
  fLargeGridSize := 5;
  fShowGrid := True;
  fGridStyle := gtLines;
  fCoordList := TCoordList.Create;
end;

destructor TOutputController.Destroy;
begin
  if (Designer <> nil) and (Designer.OutputController = Self) then
    Designer.OutputController := nil;
  FreeAndNil(fCoordList);
  inherited Destroy;
end;

procedure TOutputController.SetGridSize(const AValue: double);
begin
  if fGridSize = AValue then
    exit;
  fGridSize := AValue;
  Designer.Invalidate;
end;

procedure TOutputController.SetGridStyle(AValue: TGridStyle);
begin
  if fGridStyle = AValue then
    Exit;
  fGridStyle := AValue;
  Designer.Invalidate;
end;

procedure TOutputController.SetShowGrid(AValue: boolean);
begin
  if fShowGrid = AValue then
    exit;
  fShowGrid := AValue;
  Designer.Invalidate;
end;

procedure TOutputController.SetLargeGridSize(const AValue: integer);
begin
  if fLargeGridSize = AValue then
    exit;
  fLargeGridSize := AValue;
  Designer.Invalidate;
end;

procedure TOutputController.SetStepSize(const AValue: integer);
begin
  if fStepSize = AValue then
    Exit;
  fStepSize := AValue;
  Designer.Invalidate;
end;

function TOutputController.CreateCanvas(ACanvas: TAggCanvas): TICanvas;
begin
  Result := TICanvas.Create(ACanvas, Designer);
end;

function TOutputController.GetInvalidRect(AObject: TIObject): TRect;
var
  Box, ARect: TIFloatRect;
  PW: integer;
begin
  Box := AObject.GetBounds(True);
  Designer.LogToScreen(Box.Left, Box.Top, ARect.Left, ARect.Top);
  Designer.LogToScreen(Box.Right, Box.Bottom, ARect.Right, ARect.Bottom);
  PW := Round(Designer.LogToScreen(AObject.GraphicsSettings.Pen.Width)) + 1;
  ARect.Inflate(PW + MaxVertexSize);
  Result.Left := Trunc(ARect.Left);
  Result.Top := Trunc(ARect.Top);
  Result.Right := Ceil(ARect.Right);
  Result.Bottom := Ceil(ARect.Bottom);
end;

function TOutputController.GetViewport: TIFloatRect;
begin
  Result := TIFloatRect.Empty;
  if Assigned(Designer.Document) then
    Result := TIFloatRect.Create(0, 0, Designer.Document.Width,
      Designer.Document.Height);
end;

function TOutputController.GetUnitsPerInch: double;
begin
  Result := 254;
  if Assigned(Designer.Document) then
    Result := Designer.Document.UnitsPerInch;
end;

function TOutputController.MaxVertexSize: integer;
begin
  Result := 4;
end;

procedure TOutputController.Attached(ADesigner: TIDesigner);
begin

end;

procedure TOutputController.Detached(ADesigner: TIDesigner);
begin

end;

procedure TOutputController.DrawVertex(X, Y: integer; Kind: word);
var
  p: TPoint;
begin
  p := Point(X, Y);
  if fCoordList.Exists(p) then
    exit;
  fCoordList.Add(p);
  with fDesigner.fBitmap.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Mode := pmNot;
    Pen.Width := 1;
    Frame(X - 3, Y - 3, X + 4, Y + 4);
    Pen.Mode := pmCopy;
  end;
end;

procedure TOutputController.DrawBackground(ACanvas: TAggCanvas; Workspace: TIFloatRect);

  procedure MakeCheckBoard;
  const
    Colors: array[boolean] of byte = ($FF, $CC);
    TileSize = 8;
  var
    i, j, x, y, x0, l, t, xs, xe, ys, ye: integer;
    b, bx, bx0, by: boolean;
    p: PByte;
  begin
    bx0 := True;
    by := True;
    l := Round(Workspace.Left);
    t := Round(Workspace.Top);
    if l < 0 then
    begin
      l := -l;
      x0 := TileSize - (l mod TileSize);
      bx0 := (l div TileSize) and $1 <> 0;
    end
    else
      x0 := l + TileSize;
    if t < 0 then
    begin
      t := -t;
      y := TileSize - (t mod TileSize);
      by := (t div TileSize) and $1 <> 0;
    end
    else
      y := t + TileSize;
    xs := Max(0, Round(Workspace.Left));
    xe := Min(ACanvas.Image.Width - 1, Round(Workspace.Right));
    ys := Max(0, Round(Workspace.Top));
    ye := Min(ACanvas.Image.Height - 1, Round(Workspace.Bottom));
    for j := ys to ye do
    begin
      if j > y then
      begin
        by := not by;
        Inc(y, TileSize);
      end;
      x := x0;
      bx := bx0;
      for i := xs to xe do
      begin
        if i > x then
        begin
          bx := not bx;
          Inc(x, TileSize);
        end;
        b := bx xor by;
        p := @ACanvas.Image.Data[(j * ACanvas.Image.Width + i) * 4];
        p[0] := Colors[b];
        p[1] := Colors[b];
        p[2] := Colors[b];
        p[3] := $FF;
      end;
    end;
  end;

var
  clr: TAggColor;
begin
  ACanvas.AggClearAll(ColorToAggColor(Designer.Color));
  clr := IColorToAggColor(Designer.Document.BgColor);
  if clr.a < $FF then
    MakeCheckBoard;
  if clr.a > 0 then
  begin
    ACanvas.Brush.AggColor := clr;
    ACanvas.AggRectangle(Workspace.Left, Workspace.Top, Workspace.Right,
      Workspace.Bottom);
  end;
  if fGridSize <> 0 then
    DrawGrid(ACanvas, Workspace);
end;

procedure TOutputController.DrawGrid(ACanvas: TAggCanvas; Workspace: TIFloatRect);
var
  Pos, Pos1, Pos2: TIFloatPoint;
  i, j, sXi, sYi: integer;
  sX, sY: double;
  p: PByte;
begin
  if not fShowGrid then
    exit;
  with Designer do
  begin
    ACanvas.Pen.Color := clSilver;
    ACanvas.Pen.Style := psDot;
    ScreenToLog(Workspace.Left, Workspace.Top, Pos1.X, Pos1.Y);
    ScreenToLog(Workspace.Right, Workspace.Bottom, Pos2.X, Pos2.Y);
    Pos1 := PointToGrid(Pos1, fGridSize);
    if fGridStyle = gtDots then
    begin
      i := 0;
      while Pos1.X < Pos2.X do
      begin
        j := 0;
        Pos := Pos1;
        while Pos.Y < Pos2.Y do
        begin
          LogToScreen(Pos.X, Pos.Y, sX, sY);
          sYi := Round(sY);
          sXi := Round(sX);
          if InRange(sXi, 0, ACanvas.Image.Width - 1)
          and InRange(sYi, 0, ACanvas.Image.Height - 1) then
          begin
            p := @ACanvas.Image.Data[(sYi * ACanvas.Image.Width + sXi) * 4];
            p[0] := 0;
            p[1] := 0;
            p[2] := 0;
            p[3] := $FF;
          end;
          Pos.Y += fGridSize;
          Inc(j);
        end;
        Pos1.X += fGridSize;
        inc(i);
      end;
    end
    else
    begin
      i := 0;
      while Pos1.X < Pos2.X do
      begin
        LogToScreen(Pos1.X, 0, sX, sY);
        sXi := Round(sX);
        if sXi < Workspace.Right then
        begin
          if LargeGridSize > 0 then
            if i mod LargeGridSize = 0 then
              ACanvas.Pen.Color := clGray
            else
              ACanvas.Pen.Color := clSilver;
          ACanvas.MoveTo(sXi, Round(Workspace.Top));
          ACanvas.LineTo(sXi, Round(Workspace.Bottom));
        end;
        Pos1.X += fGridSize;
        Inc(i);
      end;
      i := 0;
      while Pos1.Y < Pos2.Y do
      begin
        LogToScreen(0, Pos1.Y, sX, sY);
        sYi := Round(sY);
        if sYi < Workspace.Bottom then
        begin
          if LargeGridSize > 0 then
            if i mod LargeGridSize = 0 then
              ACanvas.Pen.Color := clGray
            else
              ACanvas.Pen.Color := clSilver;
          ACanvas.MoveTo(Round(Workspace.Left), sYi);
          ACanvas.LineTo(Round(Workspace.Right), sYi);
        end;
        Pos1.Y += fGridSize;
        Inc(i);
      end;
    end;
  end;
end;

procedure TOutputController.DrawObjects(ACanvas: TAggCanvas);
var
  c: TICanvas;
  Clip: TRect;
  ClipRect, WindowRect, FloatClientRect: TIFloatRect;
begin
  with Designer do
  begin
    GetClipBox(Canvas.Handle, @Clip);
    c := CreateCanvas(ACanvas);
    try
      ScreenToLog(Clip.Left, Clip.Top, ClipRect.Left, ClipRect.Top);
      ScreenToLog(Clip.Right, Clip.Bottom, ClipRect.Right, ClipRect.Bottom);
      c.ClipRect := ClipRect;
      ScreenToLog(0, 0, WindowRect.Left, WindowRect.Top);
      ScreenToLog(ClientWidth, ClientHeight, WindowRect.Right,
        WindowRect.Bottom);
      FloatClientRect.Left := ClientRect.Left;
      FloatClientRect.Top := ClientRect.Top;
      FloatClientRect.Right := ClientRect.Right;
      FloatClientRect.Bottom := ClientRect.Bottom;
      c.TransformSpace(WindowRect, FloatClientRect);
      Document.RootObject.Draw(c);
      c.DrawTo(ACanvas);
    finally
      c.Free;
    end;
  end;
end;

procedure TOutputController.DrawAdditionalInfo;
var
  Pos: TIFloatPoint;
  i, j: integer;
  sX, sY: double;
  Obj: TIObject;
begin
  if Designer.Document = nil then
    exit;
  fCoordList.Clear;
  with Designer do
  begin
    for i := 0 to Document.SelectedCount - 1 do
    begin
      Obj := Document.Selected[i];
      if Obj = nil then
        continue;
      for j := 0 to Obj.VertexesCount - 1 do
      begin
        Pos := Obj.Vertexes[j];
        LogToScreen(Pos.X, Pos.Y, sX, sY);
        DrawVertex(Round(sX), Round(sY), Obj.VertexesKind[j]);
      end;
    end;
  end;
end;


{ TIDesigner }

constructor TIDesigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBitmap := TBitmap.Create;
  fCanvas := TAggCanvas.Create;
  fCanvas.Image.PixelFormat := afpimRGBA32;
  Color := clBtnFace;
  ControlStyle := [csClickEvents, csDoubleClicks];
  fBorderStyle := bsNone;
  Width := 400;
  Height := 400;
  DoubleBuffered := True;
  fScale := 100;
  fTolerance := 3;
  fScrollSpeed := 100;
end;

destructor TIDesigner.Destroy;
begin
  OutputController := nil;
  InputController := nil;
  Document := nil;
  FreeAndNil(fCanvas);
  FreeAndNil(fBitmap);
  inherited Destroy;
end;

procedure TIDesigner.WMHScroll(var msg: TLMHScroll);
var
  si: TScrollInfo;
  Dx, Dy: integer;
begin
  inherited;
  Dy := 0;
  Dx := 0;
  case msg.ScrollCode of
    SB_LINELEFT: Dx := -fScrollSpeed;
    SB_LINERIGHT: Dx := fScrollSpeed;
    SB_PAGELEFT: Dx := (-ClientWidth - 1);
    SB_PAGERIGHT: Dx := ClientWidth - 1;
    SB_THUMBPOSITION, SB_THUMBTRACK:
    begin
      si.cbSize := SizeOf(TScrollInfo);
      si.fMask := SIF_TRACKPOS;
      if LCLIntf.GetScrollInfo(Handle, SB_HORZ, si) then
        if si.nTrackPos <> fLeftCol - Viewport.Left then
          Dx := -Round(LogToScreen(fLeftCol - si.nTrackPos));
    end;
  end;
  ScrollBy(Dx, Dy);
end;

procedure TIDesigner.WMVScroll(var msg: TLMVScroll);
var
  si: TScrollInfo;
  Dx, Dy: integer;
begin
  inherited;
  Dy := 0;
  Dx := 0;
  case msg.ScrollCode of
    SB_LINEUP: Dy := -fScrollSpeed;
    SB_LINEDOWN: Dy := fScrollSpeed;
    SB_PAGEUP: Dy := -(ClientHeight - 1);
    SB_PAGEDOWN: Dy := ClientHeight - 1;
    SB_THUMBPOSITION, SB_THUMBTRACK:
    begin
      si.cbSize := SizeOf(TScrollInfo);
      si.fMask := SIF_TRACKPOS;
      if LCLIntf.GetScrollInfo(Handle, SB_VERT, si) then
        if si.nTrackPos <> fTopRow then
          Dy := -Round(LogToScreen(fTopRow - si.nTrackPos));
    end;
  end;
  ScrollBy(Dx, Dy);
end;

procedure TIDesigner.WMEraseBkGnd(var msg: TLMEraseBkgnd);
begin
  msg.Result := 1;
end;

procedure TIDesigner.WndProc(var Message: TLMessage);
var
  Handled: boolean;
begin
  Handled := False;
  if (Message.Msg = LM_LBUTTONDOWN) and (not Focused) then
    SetFocus;
  if Assigned(fInputController) then
    if ((Message.Msg >= LM_MOUSEFIRST) and (Message.Msg <= LM_MOUSELAST)) or
      ((Message.Msg >= LM_KEYFIRST) and (Message.Msg <= LM_KEYLAST)) then
      fInputController.SendMessage(Message, Handled);
  if (not Handled) or (Message.Msg = LM_MOUSEMOVE) then
    inherited WndProc(Message);
end;

procedure TIDesigner.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[fBorderStyle];
    if NewStyleControls and (fBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TIDesigner.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBars;
end;

procedure TIDesigner.Resize;
begin
  fBitmap.SetSize(ClientWidth, ClientHeight);
  fCanvas.Image.SetSize(ClientWidth, ClientHeight);
  UpdateAutoScale;
  UpdateScrollBars;
  inherited Resize;
end;

procedure TIDesigner.Paint;
var
  FloatBox, VpR: TIFloatRect;
begin
  if csDestroying in ComponentState then
    exit;
  if Assigned(fOutputController) and Assigned(Document) then
  begin
    VpR := Viewport;
    LogToScreen(VpR.Left, VpR.Top, FloatBox.Left, FloatBox.Top);
    LogToScreen(VpR.Right, VpR.Bottom, FloatBox.Right, FloatBox.Bottom);
    fOutputController.DrawBackground(fCanvas, FloatBox);
    fOutputController.DrawObjects(fCanvas);
  end;
  fBitmap.LoadFromIntfImage(fCanvas.Image.IntfImg);
  fOutputController.DrawAdditionalInfo;
  Canvas.Draw(0, 0, fBitmap);
end;

procedure TIDesigner.DoScaleChanged;
begin
  RecalcGrid;
  if Assigned(fOnScaleChanged) then
    fOnScaleChanged(Self);
end;

procedure TIDesigner.RecalcGrid;
begin
  if (fInputController = nil) or (OutputController = nil) then
    exit;
  if fScale < 25 then
  begin
    OutputController.StepSize := 100;
    OutputController.GridSize := 200;
    OutputController.LargeGridSize := 4;
  end
  else
  if (fScale >= 25) and (fScale < 50) then
  begin
    OutputController.StepSize := 50;
    OutputController.GridSize := 100;
    OutputController.LargeGridSize := 4;
  end
  else
  if (fScale >= 50) and (fScale < 75) then
  begin
    OutputController.StepSize := 20;
    OutputController.GridSize := 50;
    OutputController.LargeGridSize := 5;
  end
  else
  if (fScale >= 75) and (fScale < 200) then
  begin
    OutputController.StepSize := 10;
    OutputController.GridSize := 50;
    OutputController.LargeGridSize := 5;
  end
  else
  if (fScale >= 200) and (fScale < 400) then
  begin
    OutputController.StepSize := 5;
    OutputController.GridSize := 50;
    OutputController.LargeGridSize := 5;
  end
  else
  if (fScale >= 400) and (fScale < 800) then
  begin
    OutputController.StepSize := 2;
    OutputController.GridSize := 10;
    OutputController.LargeGridSize := 5;
  end
  else
  if (fScale >= 800) and (fScale < 1600) then
  begin
    OutputController.StepSize := 1;
    OutputController.GridSize := 10;
    OutputController.LargeGridSize := 5;
  end
  else
  if (fScale >= 1600) then
  begin
    OutputController.StepSize := 1;
    OutputController.GridSize := 5;
    OutputController.LargeGridSize := 5;
  end;
end;

procedure TIDesigner.SetTopRow(ATopRow: double);
var
  HMax: double;
begin
  HMax := Viewport.Bottom - Viewport.Top - ScreenToLog(ClientHeight - 1);
  fTopRow := Max(0, Min(ATopRow, HMax));
end;

procedure TIDesigner.SetLeftCol(ALeftCol: double);
var
  WMax: double;
begin
  WMax := Viewport.Right - Viewport.Left - ScreenToLog(ClientWidth - 1);
  fLeftCol := Max(0, Min(ALeftCol, WMax));
end;

procedure TIDesigner.InvalidateObjectRect(AObject: TIObject);
var
  Rect: TRect;
begin
  if Assigned(FOutputController) and HandleAllocated then
  begin
    Rect := fOutputController.GetInvalidRect(AObject);
    InvalidateRect(Handle, @Rect, True);
  end;
end;

procedure TIDesigner.ObjectAdded(AObject: TIObject);
begin
  InvalidateObjectRect(AObject);
  if Assigned(fOnObjectAdded) then
    fOnObjectAdded(Self, AObject);
end;

procedure TIDesigner.ObjectRemove(AObject: TIObject);
begin
  if Assigned(fOnObjectRemove) then
    fOnObjectRemove(Self, AObject);
  InvalidateObjectRect(AObject);
  Cursor := crDefault;
end;

procedure TIDesigner.ObjectChanging(AObject: TIObject);
begin
  if Assigned(fOnObjectChanging) then
    fOnObjectChanging(Self, AObject);
  InvalidateObjectRect(AObject);
end;

procedure TIDesigner.ObjectChanged(AObject: TIObject);
begin
  InvalidateObjectRect(AObject);
  if Assigned(fOnObjectChanged) then
    fOnObjectChanged(Self, AObject);
end;

procedure TIDesigner.ObjectSelected(AObject: TIObject; Selected: boolean);
begin
  InvalidateObjectRect(AObject);
  if Assigned(fOnObjectSelect) then
    fOnObjectSelect(Self, AObject, Selected);
end;

procedure TIDesigner.ObjectCommandReceived(AObject: TIObject;
  var Command: TICmdMessage);
begin
  // nothing
end;

procedure TIDesigner.ObjectCommandHandled(AObject: TIObject; var Command: TICmdMessage);
begin
  // nothing
end;

procedure TIDesigner.DocumentResize(NewWidth, NewHeight: double);
begin
  UpdateAutoScale;
  UpdateScrollBars;
  Invalidate;
end;

procedure TIDesigner.DocumentDestroying;
begin
  Document := nil;
end;

procedure TIDesigner.LayerAdded(ALayer: TILayer);
begin

end;

procedure TIDesigner.LayerRemove(ALayer: TILayer);
begin

end;

procedure TIDesigner.LayerChanging(ALayer: TILayer);
begin

end;

procedure TIDesigner.LayerChanged(ALayer: TILayer);
begin
  Invalidate;
end;

procedure TIDesigner.ActiveLayerChanged(ALayer: TILayer);
begin
  if Assigned(InputController) and (InputController is TEditController) then
    TEditController(InputController).ClearSelection;
end;

procedure TIDesigner.RequestHandler(AObject: TIObject; var Command: TICmdMessage);
begin
  if Assigned(InputController) then
    InputController.Dispatch(Command);
  if (Command.Result = 0) and Assigned(OutputController) then
    OutputController.Dispatch(Command);
end;

function TIDesigner.GetAutoScale: double;
var
  S1, S2: double;
begin
  Result := fScale;
  if Viewport.Right - Viewport.Left <> 0 then
    S1 := ((((ClientWidth - 1) / Screen.PixelsPerInch) * UnitsPerInch) /
      (Viewport.Right - Viewport.Left)) * 100
  else
    S1 := 100;
  if Viewport.Bottom - Viewport.Top <> 0 then
    S2 := ((((ClientHeight - 1) / Screen.PixelsPerInch) * UnitsPerInch) /
      (Viewport.Bottom - Viewport.Top)) * 100
  else
    S2 := 100;
  case ScaleMode of
    smFitX: Result := Max(1, S1);
    smFitY: Result := Max(1, S2);
    smFitXY: Result := Max(1, Min(S1, S2));
  end;
end;

function TIDesigner.GetResolution: integer;
begin
  Result := Screen.PixelsPerInch;
end;

function TIDesigner.GetViewport: TIFloatRect;
begin
  Result := TIFloatRect.Empty;
  if Assigned(fOutputController) then
    Result := fOutputController.GetViewport;
end;

function TIDesigner.GetUnitsPerInch: double;
begin
  Result := 254;
  if Assigned(fOutputController) then
    Result := fOutputController.GetUnitsPerInch;
end;

procedure TIDesigner.SetDocument(AValue: TIDocument);
begin
  if Assigned(fDocument) then
  begin
    fDocument.UnRegisterObserver(Self);
    fDocument.UnRegisterRequestHandler(Self);
  end;
  fDocument := AValue;
  if Assigned(fDocument) then
  begin
    fDocument.RegisterObserver(Self);
    fDocument.RegisterRequestHandler(Self);
    fDocument.Canvas := fCanvas;
  end
  else
    fCanvas.Clear;
  UpdateAutoScale;
  UpdateScrollBars;
  Invalidate;
end;

procedure TIDesigner.SetInputController(AValue: TInputController);
begin
  if fInputController = AValue then
    Exit;
  if Assigned(fInputController) then
    fInputController.Detached(Self);
  fInputController := AValue;
  if Assigned(fInputController) then
    fInputController.Attached(Self);
  RecalcGrid;
  UpdateMouse;
end;

procedure TIDesigner.SetOutputController(AValue: TOutputController);
begin
  if fOutputController = AValue then
    Exit;
  if Assigned(fOutputController) then
    fOutputController.Detached(Self);
  fOutputController := AValue;
  if Assigned(fOutputController) then
    fOutputController.Attached(Self);
  RecalcGrid;
  UpdateAutoScale;
  UpdateScrollBars;
  Invalidate;
end;

procedure TIDesigner.SetScale(AValue: double);
begin
  if ScaleMode = smManual then
    SetScaleValue(AValue);
end;

procedure TIDesigner.SetScaleMode(AValue: TScaleMode);
begin
  if fScaleMode = AValue then
    Exit;
  fScaleMode := AValue;
  UpdateAutoScale;
  UpdateScrollBars;
  Invalidate;
end;

procedure TIDesigner.SetScaleValue(const AValue: double);
var
  NewVal: double;
begin
  NewVal := Min(1600, Max(10, AValue));
  if NewVal = fScale then
    exit;
  fScale := NewVal;
  UpdateScrollBars;
  Invalidate;
  DoScaleChanged;
end;

procedure TIDesigner.UpdateAutoScale;
begin
  if ScaleMode <> smManual then
    SetScaleValue(GetAutoScale);
end;

procedure TIDesigner.UpdateMouse;
var
  Pos: TPoint;
begin
  if HandleAllocated then
  begin
    GetCursorPos(Pos);
    Pos := ScreenToClient(Pos);
    Perform(LM_MOUSEMOVE, 0, MakeLParam(Pos.X, Pos.Y));
  end;
end;

procedure TIDesigner.LogToScreen(lX, lY: double; var sX, sY: double);
begin
  sX := LogToScreen(lX - fOriginX);
  sY := LogToScreen(lY - fOriginY);
end;

procedure TIDesigner.ScreenToLog(sX, sY: double; var lX, lY: double);
begin
  lX := ScreenToLog(sX) + fOriginX;
  lY := ScreenToLog(sY) + fOriginY;
end;

function TIDesigner.LogToScreen(Value: double): double;
begin
  Result := Resolution * (Value / UnitsPerInch) * (fScale * 0.01);
end;

function TIDesigner.ScreenToLog(Value: double): double;
begin
  Result := ((Value / Resolution) * UnitsPerInch) / (fScale * 0.01);
end;

procedure TIDesigner.ResetScrollBars;
begin
  if not HandleAllocated then
    Exit;
  SetTopRow(0);
  SetLeftCol(0);
  UpdateScrollBars;
  Invalidate;
end;

procedure TIDesigner.UpdateScrollBars;
var
  h, w: integer;
  si: TScrollInfo;
  v: TIFloatRect;
begin
  if not HandleAllocated then
    Exit;
  v := Viewport;
  SetTopRow(fTopRow);
  SetLeftCol(fLeftCol);
  h := Round(ScreenToLog(ClientHeight));
  w := Round(ScreenToLog(ClientWidth));
  si.cbSize := SizeOf(si);
  si.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
  si.nMin := 0;

  si.nMax := IfThen(Round(v.Bottom - v.Top) > h, Round(v.Bottom - v.Top), 0);
  si.nPage := IfThen(si.nMax <> 0, h, 0);
  if (si.nMax <> 0) and (fTopRow > 0) then
  begin
    si.nPos := Round(fTopRow);
    si.nTrackPos := Round(fTopRow);
  end
  else
  begin
    fTopRow := 0;
    si.nPos := 0;
    si.nTrackPos := 0;
  end;
  fOriginY := IfThen(si.nMax <> 0, fTopRow + v.Top,
    (v.Bottom + v.Top - ScreenToLog(ClientHeight - 1)) / 2);
  SetScrollInfo(Handle, SB_VERT, si, True);

  si.nMax := IfThen(Round(v.Right - v.Left) > w, Round(v.Right - v.Left), 0);
  si.nPage := IfThen(si.nMax <> 0, w, 0);
  if (si.nMax <> 0) and (fLeftCol > 0) then
  begin
    si.nPos := Round(fLeftCol);
    si.nTrackPos := Round(fLeftCol);
  end
  else
  begin
    fLeftCol := 0;
    si.nPos := 0;
    si.nTrackPos := 0;
  end;
  fOriginX := IfThen(si.nMax <> 0, fLeftCol + v.Left,
    (v.Right + v.Left - ScreenToLog(ClientWidth - 1)) / 2);
  SetScrollInfo(Handle, SB_HORZ, si, True);
end;

procedure TIDesigner.ScrollBy(var Dx, Dy: integer);
var
  OldOX, OldOY: double;
  h: HBITMAP;
begin
  if HandleAllocated and ((Dx <> 0) or (Dy <> 0)) then
  begin
    OldOX := LogToScreen(fLeftCol);
    OldOY := LogToScreen(fTopRow);
    SetTopRow(fTopRow + ScreenToLog(Dy));
    SetLeftCol(fLeftCol + ScreenToLog(Dx));
    UpdateScrollBars;
    Dx := Round(LogToScreen(fLeftCol) - OldOX);
    Dy := Round(LogToScreen(fTopRow) - OldOY);
    ScrollWindow(Handle, -Dx, -Dy, nil, nil);
    //RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    if Assigned(fOnScroll) then
      fOnScroll(Self);
  end;
end;

const
  IDC_ROTATE = 'ROTATE';
  IDC_SHEAR = 'SHEAR';
  IDC_VERTEXDRAG = 'VERTEXDRAG';
  IDC_AREACROSS = 'AREACROSS';
  IDC_ADD = 'ADD';

  cCursor: array[0..4] of TIdentMapEntry = (
    (Value: crRotate; Name: IDC_ROTATE),
    (Value: crShear; Name: IDC_SHEAR),
    (Value: crVertexDrag; Name: IDC_VERTEXDRAG),
    (Value: crAreaCross; Name: IDC_AREACROSS),
    (Value: crAdd; Name: IDC_ADD)
    );

procedure LoadCursors;
var
  i: integer;
begin
  for i := Low(cCursor) to High(cCursor) do
    Screen.Cursors[cCursor[i].Value] := LoadCursor(HInstance, PChar(cCursor[i].Name));
end;

procedure DestroyCursors;
var
  i: integer;
begin
  for i := Low(cCursor) to High(cCursor) do
    Screen.Cursors[cCursor[i].Value] := 0;
end;

initialization
  LoadCursors;

finalization
  DestroyCursors;

end.
