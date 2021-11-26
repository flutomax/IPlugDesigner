{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIObject;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, Contnrs, Controls, Graphics, SysUtils, uCommon, uTree,
  uStorage, uGraphics;

type

  TIObject = class;
  TIBaseLayer = class;
  TIObjectClass = class of TIObject;

  TIObjectEvent = procedure(Sender: TObject; AObject: TIObject) of object;
  TIObjectQueryEvent = procedure(Sender: TObject; var AObject: TIObject) of object;
  TISelectEvent = procedure(Sender: TObject; AObject: TIObject;
    Selected: boolean) of object;
  TICommandEvent = procedure(Sender: TObject; AObject: TIObject;
    var Command: TICmdMessage) of object;


  { TIObject }

  TIObject = class(TINonRefInterfacedObject, ISerializeNotify,
    ICustomSerialize)
  private
    fLocked: boolean;
    fState: TIObjectState;
    fVisible: boolean;
    fLockCount: integer;
    fDragHitTest: cardinal;
    fDragCancelPos: TIFloatPoint;
    fDragStartPos: TIFloatPoint;
    fTmpOwner: TIObject;
    fOwner: TIObject;
    fOpacity: double;
    fGraphicsSettings: TIGraphicsSettings;
    fConstructingPointIndex: integer;
    fChildsList: TObjectList;
    fID: QWord;
    fDocument: TObject;
    fParamIdx: string;
    fOnChildAdded: TIObjectEvent;
    fOnChildRemove: TIObjectEvent;
    fOnChanged: TIObjectEvent;
    fOnChanging: TIObjectEvent;
    fOnSelected: TISelectEvent;
    fOnCommandHandled: TICommandEvent;
    fOnCommandReceived: TICommandEvent;
    fOnRequestParams: TICommandEvent;
    function GetChild(AIndex: integer): TIObject;
    function GetChildsCount: integer;
    function GetIndex: integer;
    function GetIsLocked: boolean;
    function GetPen: TIPen;
    function GetSelected: boolean;
    function GetBrush: TIBrush;
    procedure SetBrush(AValue: TIBrush);
    procedure SetGraphicsSettings(AValue: TIGraphicsSettings);
    procedure SetIndex(AValue: integer);
    procedure SetLocked(AValue: boolean);
    procedure SetOpacity(AValue: double);
    procedure SetOwner(AValue: TIObject);
    procedure SetParamIdx(AValue: string);
    procedure SetPen(AValue: TIPen);
    procedure SetSelected(AValue: boolean);
    procedure SetVisible(AValue: boolean);
    procedure Destroying;
    procedure AddChild(AValue: TIObject);
    procedure RemoveChilds;
    procedure RemoveChild(AIndex: integer);
    procedure UnselectChilds;
    procedure GraphicsSettingsChanging(Sender: TObject);
    procedure GraphicsSettingsChanged(Sender: TObject);
    procedure CmdGetCursor(var cmd: TICmdMessage); message CMD_GETCURSOR;
    procedure CmdBeginDrag(var cmd: TICmdMessage); message CMD_BEGINDRAG;
    procedure CmdEndDrag(var cmd: TICmdMessage); message CMD_ENDDRAG;
    procedure CmdCancelDrag(var cmd: TICmdMessage); message CMD_CANCELDRAG;
    procedure CmdDrag(var cmd: TICmdMessage); message CMD_DRAG;
    procedure CmdVertexMove(var cmd: TICmdMessage); message CMD_VERTEXMOVE;
    procedure CmdMove(var cmd: TICmdMessage); message CMD_MOVE;
    procedure CmdConstructPoint(var cmd: TICmdMessage); message CMD_CONSTRUCTPOINT;
    procedure CmdStopConstruct(var cmd: TICmdMessage); message CMD_STOPCONSTRUCT;
    procedure CmdEndConstruct(var cmd: TICmdMessage); message CMD_ENDCONSTRUCT;
    procedure CmdClipbrdGetData(var cmd: TICmdMessage); message CMD_CLIPBRDGETDATA;
    procedure CmdClipbrdFreeData(var cmd: TICmdMessage); message CMD_CLIPBRDFREEDATA;
  protected
    function GetVertex(AIndex: integer): TIFloatPoint; virtual; abstract;
    function GetVertexesCount: integer; virtual; abstract;
    function GetVertexKind(AIndex: integer): word; virtual; abstract;
    function GetBasePointsCount: integer; virtual;
    function GetBasePoint(AIndex: integer): TIFloatPoint; virtual; abstract;
    function ConstructPoint(AIndex: integer; Pos: TIFloatPoint): integer; virtual;
    function StopConstruct(AIndex: integer): integer; virtual;
    function ChildsHitTest: boolean; virtual;
    function HitTest(Params: PIHitTestParams): cardinal; overload; virtual;
    function CalcBounds: TIFloatRect; virtual;
    procedure SetBasePoint(AIndex: integer; Value: TIFloatPoint); virtual; abstract;
    procedure SetVertex(AIndex: integer; const AValue: TIFloatPoint); virtual;
    procedure BeginSave; dynamic;
    procedure EndSave; dynamic;
    procedure BeginLoad; dynamic;
    procedure EndLoad; dynamic;
    procedure DoSetOwner(AValue: TIObject); dynamic;
    procedure DoChanging(AValue: TIObject); dynamic;
    procedure DoChanged(AValue: TIObject); dynamic;
    procedure DoDestroying; dynamic;
    procedure DoChildAdded(AValue: TIObject); dynamic;
    procedure DoChildRemove(AValue: TIObject); dynamic;
    procedure DoSelect(AValue: TIObject; Selected: boolean); dynamic;
    procedure DoCommandReceived(AValue: TIObject; var cmd: TICmdMessage); dynamic;
    procedure DoCommandHandled(AValue: TIObject; var cmd: TICmdMessage); dynamic;
    procedure DoRequestParams(AValue: TIObject; var cmd: TICmdMessage); dynamic;
    procedure CustomSerialize(const Node: ITreeNode); dynamic;
    procedure CustomDeSerialize(const Node: ITreeNode); dynamic;
    procedure Move(dx, dy: double); dynamic;
    procedure DrawNotify(Canvas: TICanvas); virtual;
    procedure Paint(Canvas: TICanvas); virtual;
    procedure Resize; virtual;
    property Brush: TIBrush read GetBrush write SetBrush;
    property Pen: TIPen read GetPen write SetPen;
    property Opacity: double read fOpacity write SetOpacity;
    property ParamIdx: string read fParamIdx write SetParamIdx;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); virtual;
    destructor Destroy; override;
    class function GetObjectName: string; virtual;
    function Has(AObject: TIObject): boolean;
    function HitTest(Params: PIHitTestParams; var AObject: TIObject): cardinal;
      overload;
    function SendCommand(CmdID: cardinal; Param1: WPARAM; Param2: LPARAM): LRESULT;
      overload;
    function Clipped(Canvas: TICanvas): boolean;
    function GetBounds(WithChilds: boolean): TIFloatRect;
    function AbsoluteToRelative(const Point: TIFloatPoint): TIFloatPoint;
    function RelativeToAbsolute(const Point: TIFloatPoint): TIFloatPoint;
    function OwnerToRelative(AOwner: TIObject; const Point: TIFloatPoint): TIFloatPoint;
    function RelativeToOwner(AOwner: TIObject; const Point: TIFloatPoint): TIFloatPoint;
    function GetZOrder: integer;
    function GetSpaceOrigin: TIFloatPoint; virtual;
    function ConstructKind: TIConstructKinds; virtual; abstract;
    function Clone(CloneOwner: TIObject): TIObject;
    function CloneAt(CloneOwner: TIObject; const X, Y: double): TIObject;
    function GetLayer: TIBaseLayer;
    procedure Assign(Source: TIObject); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: double); overload; virtual;
    procedure SetBounds(const R: TIFloatRect); overload;
    procedure SetBoundsFromParent(R: TIFloatRect);
    procedure Refresh; dynamic;
    procedure SetOrder(AOrder: TOrder);
    procedure SendCommand(var cmd);
    procedure SetZOrder(AIndex: integer);
    procedure Draw(Canvas: TICanvas);
  public
    property BasePointsCount: integer read GetBasePointsCount;
    property BasePoints[AIndex: integer]: TIFloatPoint
      read GetBasePoint write SetBasePoint;
    property ChildsCount: integer read GetChildsCount;
    property Childs[AIndex: integer]: TIObject read GetChild;
    property ConstructingPointIndex: integer read fConstructingPointIndex;
    property Document: TObject read fDocument;
    property GraphicsSettings: TIGraphicsSettings read fGraphicsSettings write SetGraphicsSettings;
    property Index: integer read GetIndex write SetIndex;
    property ID: QWord read fID;
    property Owner: TIObject read fOwner write SetOwner;
    property State: TIObjectState read fState;
    property Selected: boolean read GetSelected write SetSelected;
    property VertexesCount: integer read GetVertexesCount;
    property Vertexes[AIndex: integer]: TIFloatPoint read GetVertex write SetVertex;
    property VertexesKind[AIndex: integer]: word read GetVertexKind;
    property Visible: boolean read fVisible write SetVisible;
    property IsLocked: boolean read GetIsLocked;
    property Locked: boolean read fLocked write SetLocked;
    property OnChildAdded: TIObjectEvent read fOnChildAdded write fOnChildAdded;
    property OnChildRemove: TIObjectEvent read fOnChildRemove write fOnChildRemove;
    property OnChanging: TIObjectEvent read fOnChanging write fOnChanging;
    property OnChanged: TIObjectEvent read fOnChanged write fOnChanged;
    property OnSelected: TISelectEvent read FOnSelected write FOnSelected;
    property OnCommandReceived: TICommandEvent
      read fOnCommandReceived write fOnCommandReceived;
    property OnCommandHandled: TICommandEvent
      read fOnCommandHandled write fOnCommandHandled;
    property OnRequestParams: TICommandEvent read fOnRequestParams
      write fOnRequestParams;
  end;


  { TIBaseLayer }

  TIBaseLayer = class(TIObject)
  private
    fName: string;
    procedure SetName(const Value: string);
  protected
    function GetBasePointsCount: integer; override;
    function GetBasePoint(AIndex: integer): TIFloatPoint; override;
    procedure SetBasePoint(AIndex: integer; Value: TIFloatPoint); override;
    function GetVertex(AIndex: integer): TIFloatPoint; override;
    function GetVertexesCount: integer; override;
    procedure SetVertex(AIndex: integer; const Value: TIFloatPoint); override;
    function GetVertexKind(AIndex: integer): word; override;
    function HitTest(AParams: PIHitTestParams): cardinal; override;
    procedure DoDestroying; override;
  public
    function ConstructKind: TIConstructKinds; override;
    property Name: string read fName write SetName;
  end;

  { TIGroup }

  TIGroup = class(TIObject)
  private
    fBasePoint: TIFloatPoint;
    fUnGrouping: Boolean;
    procedure CMDGetCursor(var cmd: TICmdMessage); message CMD_GETCURSOR;
  protected
    class function GetObjectName: string; override;
    function GetBasePointsCount: Integer; override;
    function GetBasePoint(AIndex: Integer): TIFloatPoint; override;
    function GetVertex(AIndex: integer): TIFloatPoint; override;
    function GetVertexesCount: integer; override;
    function GetVertexKind(AIndex: integer): word; override;
    function CalcBounds: TIFloatRect; override;
    function ChildsHitTest: Boolean; override;
    function HitTest(Params: PIHitTestParams): Cardinal; override;
    procedure SetBasePoint(AIndex: Integer; Value: TIFloatPoint); override;
    procedure DoChanging(AObject: TIObject); override;
    procedure DoChanged(AObject: TIObject); override;
  public
    class function CanGroup(Objects: TObjectList): Boolean;
    class function Group(Objects: TObjectList; ADocument: TObject): TIGroup;
    function ConstructKind: TIConstructKinds; override;
    procedure UnGroup;
  end;

  
  { TIBaseRect }

  TIBaseRect = class(TIObject)
  private
    fBasePoints: array[0..1] of TIFloatPoint;
    fCornerRadius: double;
    procedure CMDGetCursor(var cmd: TICmdMessage); message CMD_GETCURSOR;
    procedure CMDSideMove(var cmd: TICmdMessage); message CMD_SIDEMOVE;
    procedure CMDProcessConstruct(var cmd: TICmdMessage); message CMD_PROCESSCONSTRUCT;
    procedure NormalizeVertexes;
    function GetHeight: double;
    function GetLeft: double;
    function GetTop: double;
    function GetWidth: double;
    procedure SetCornerRadius(AValue: double);
    procedure SetHeight(AValue: double);
    procedure SetLeft(AValue: double);
    procedure SetTop(AValue: double);
    procedure SetWidth(AValue: double);
  protected
    function GetRect: TIFloatRect;
    function GetBasePointsCount: integer; override;
    function GetBasePoint(AIndex: integer): TIFloatPoint; override;
    function GetVertex(AIndex: integer): TIFloatPoint; override;
    function GetVertexesCount: integer; override;
    function GetVertexKind(AIndex: integer): word; override;
    function ConstructPoint(AIndex: integer; Pos: TIFloatPoint): integer; override;
    function StopConstruct(AIndex: integer): integer; override;
    function HitTest(Params: PIHitTestParams): cardinal; override;
    procedure SetBasePoint(AIndex: integer; Value: TIFloatPoint); override;
    procedure SetVertex(AIndex: integer; const Value: TIFloatPoint); override;
    property CornerRadius: double read fCornerRadius write SetCornerRadius;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject;
      X1, Y1, X2, Y2: double); reintroduce; overload;
    constructor Create(AOwner: TIObject; ADocument: TObject); override; overload;
    function ConstructKind: TIConstructKinds; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: double); override;
  published
    property Height: double read GetHeight write SetHeight;
    property Left: double read GetLeft write SetLeft;
    property Width: double read GetWidth write SetWidth;
    property Top: double read GetTop write SetTop;
    property Opacity;
  end;

  { TISpan }

  TISpan = class(TIBaseRect, ICodeGenerate)
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    function ConstructKind: TIConstructKinds; override;
    function GetCodeStr: string; virtual;
  end;

  { TIBaseText }

  TIBaseText = class(TIBaseRect)
  private
    fText: string;
    fTextRect: TIFloatRect;
    function GetFont: TIFont;
    procedure SetFont(AValue: TIFont);
    procedure SetText(const Value: string);
  protected
    function GetTextVertex(AIndex: integer): TIFloatPoint;
    procedure Paint(Canvas: TICanvas); override;
    property Text: string read fText write SetText;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    function ConstructKind: TIConstructKinds; override;
  published
    property Font: TIFont read GetFont write SetFont;
  end;

  { TIBaseLine }

  TIBaseLine = class(TIObject)
  private
    procedure CmdGetCursor(var cmd: TICmdMessage); message CMD_GETCURSOR;
    procedure CMDProcessConstruct(var cmd: TICmdMessage); message CMD_PROCESSCONSTRUCT;
  protected
    function SideCode: Cardinal; virtual;
    function GetVertexesCount: Integer; override;
    function GetVertex(AIndex: Integer): TIFloatPoint; override;
    function GetVertexKind(AIndex: Integer): Word; override;
    function HitTest(Params: PIHitTestParams): Cardinal; override;
    procedure SetVertex(AIndex: integer; const AValue: TIFloatPoint); override;
  public
    function ConstructKind: TIConstructKinds; override;
  published
    property Opacity;
  end;

  { TIBasePoly }

  TIBasePoly = class(TIBaseLine)
  private
    fBasePoints: TList;
    procedure CMDSpecCommand(var cmd: TICmdMessage); message CMD_SPECCOMMAND;
  protected
    function GetMinPointsCount: Integer; virtual; abstract;
    function GetBasePointsCount: Integer; override;
    function GetBasePoint(AIndex: Integer): TIFloatPoint; override;
    procedure SetBasePoint(AIndex: Integer; Value: TIFloatPoint); override;
    function ConstructPoint(AIndex: Integer; Pos: TIFloatPoint): Integer; override;
    function StopConstruct(AIndex: Integer): Integer; override;
    procedure BeginLoad; override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); overload; override;
    constructor Create(AOwner: TIObject; ADocument: TObject;
      Pts: array of TIFloatPoint); reintroduce; overload;
    destructor Destroy; override;
    procedure AddBasePoint(X, Y: double);
    procedure InsertBasePoint(AIndex: Integer; X, Y: double);
    procedure DeleteBasePoint(AIndex: Integer);
    procedure ClearBasePoints;
  end;


  IObjectsFactory = interface(IInterface)
  ['{9D314B43-0C60-4578-911D-FCC7A9221B8A}']
    function CreateInstance(AOwner: TIObject; const Name: string): TIObject;
    function GetInstanceName(AObject: TIObject): string;
  end;

  { TIObjectsFactory }

  TIObjectsFactory = class(TISingleton, IInterface, IClassesObserver)
  private
    fFactoryList: TList;
    fIDCounter: QWord;
    function GetFactory(Index: Integer): IObjectsFactory;
    function GetFactoryCount: Integer;
  protected
    constructor Create; override;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure ClassRegistered(AClassInfo: TIClassInfo);
    procedure ClassUnregistered(AClassInfo: TIClassInfo);
    property FactoryCount: Integer read GetFactoryCount;
    property Factory[Index: Integer]: IObjectsFactory read GetFactory;
  public
    destructor Destroy; override;
    function CreateInstance(AOwner: TIObject; ADocument: TObject; const Name: string): TIObject;
    function CreateObjectID: QWord;
    function GetInstanceName(AObject: TIObject): string;
    function FindInstanceName(AObject: TIObject): string;
    procedure RegisterFactory(const AFactory: TIObjectsFactory);
    procedure UnregisterFactory(const AFactory: TIObjectsFactory);
    procedure ResetID;
  end;


implementation

uses
  SysConst, Math, uIDocument;

{ TIObject }

constructor TIObject.Create(AOwner: TIObject; ADocument: TObject);
var
  Factory: TIObjectsFactory;
begin
  inherited Create;
  Include(fState, osCreating);
  Factory := TIObjectsFactory.GetInstance;
  fID := Factory.CreateObjectID;
  fDocument := ADocument;
  fChildsList := TObjectList.Create(False);
  fTmpOwner := AOwner;
  fVisible := True;
  fOpacity := 100;
  fParamIdx := kNoParameter;
  fGraphicsSettings := TIGraphicsSettings.Create;
  fGraphicsSettings.OnChange := @GraphicsSettingsChanged;
  fGraphicsSettings.OnChanging := @GraphicsSettingsChanging;
end;

destructor TIObject.Destroy;
begin
  if Assigned(Owner) then
    Owner.RemoveChild(Index);
  RemoveChilds;
  fChildsList.Free;
  fGraphicsSettings.Free;
  inherited Destroy;
end;

procedure TIObject.Destroying;
var
  i: integer;
begin
  Include(fState, osDestroying);
  DoDestroying;
  for i := 0 to ChildsCount - 1 do
    Childs[i].Destroying;
end;

class function TIObject.GetObjectName: string;
begin
  Result := 'None';
end;

procedure TIObject.AfterConstruction;
begin
  inherited AfterConstruction;
  Owner := fTmpOwner;
  Exclude(fState, osCreating);
end;

procedure TIObject.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if not (osDestroying in fState) then
    Destroying;
end;

function TIObject.GetIndex: integer;
begin
  Result := 0;
  if Assigned(fOwner) then
    Result := fOwner.fChildsList.IndexOf(Self);
end;

function TIObject.GetPen: TIPen;
begin
  result := GraphicsSettings.Pen;
end;

function TIObject.GetSelected: boolean;
begin
  Result := osSelected in fState;
end;

function TIObject.GetBrush: TIBrush;
begin
  result := GraphicsSettings.Brush;
end;

procedure TIObject.SetBrush(AValue: TIBrush);
begin
  GraphicsSettings.Brush := AValue;
end;

procedure TIObject.SetGraphicsSettings(AValue: TIGraphicsSettings);
begin
  fGraphicsSettings.Assign(AValue);
end;

function TIObject.GetChild(AIndex: integer): TIObject;
begin
  Result := TIObject(fChildsList[AIndex]);
end;

function TIObject.GetChildsCount: integer;
begin
  Result := fChildsList.Count;
end;

procedure TIObject.SetIndex(AValue: integer);
var
  i: integer;
begin
  if Assigned(fOwner) and (AValue < fOwner.fChildsList.Count) then
  begin
    DoChanging(Self);
    i := fOwner.fChildsList.IndexOf(Self);
    fOwner.fChildsList.Move(i, AValue);
    DoChanged(Self);
  end;
end;

procedure TIObject.SetLocked(AValue: boolean);
begin
  if fLocked = AValue then
    Exit;
  DoChanging(self);
  if AValue then
    UnselectChilds;
  fLocked := AValue;
  DoChanged(self);
end;

procedure TIObject.SetOpacity(AValue: double);
begin
  AValue := EnsureRange(AValue, 0, 100);
  if fOpacity = AValue then
    exit;
  DoChanging(self);
  fOpacity := AValue;
  DoChanged(self);
end;

procedure TIObject.SetParamIdx(AValue: string);
begin
  if fParamIdx = AValue then
    exit;
  DoChanging(self);
  fParamIdx := AValue;
  DoChanged(self);
end;

procedure TIObject.SetOwner(AValue: TIObject);
begin
  if fOwner <> AValue then
  begin
    if Assigned(fOwner) then
      fOwner.RemoveChild(Index);
    if Assigned(AValue) then
      AValue.AddChild(Self)
    else
      fOwner := nil;
    DoSetOwner(fOwner);
  end;
end;

procedure TIObject.SetPen(AValue: TIPen);
begin
  GraphicsSettings.Pen := AValue;
end;

procedure TIObject.AddChild(AValue: TIObject);
begin
  AValue.fOwner := Self;
  fChildsList.Add(AValue);
  DoChildAdded(AValue);
end;

procedure TIObject.RemoveChilds;
var
  i: integer;
  obj: TIObject;
begin
  for i := ChildsCount - 1 downto 0 do
  begin
    obj := Childs[i];
    RemoveChild(i);
    obj.Free;
  end;
end;

procedure TIObject.RemoveChild(AIndex: integer);
begin
  DoChildRemove(Childs[AIndex]);
  Childs[AIndex].fOwner := nil;
  fChildsList.Delete(AIndex);
end;

procedure TIObject.UnselectChilds;
var
  i: integer;
  obj: TIObject;
begin
  for i := ChildsCount - 1 downto 0 do
    Childs[i].Selected := false;
end;

procedure TIObject.GraphicsSettingsChanging(Sender: TObject);
begin
  DoChanging(Self);
end;

procedure TIObject.GraphicsSettingsChanged(Sender: TObject);
begin
  DoChanged(Self);
end;

procedure TIObject.SetSelected(AValue: boolean);
begin
  if (AValue = Selected) or IsLocked then
    exit;
  if AValue then
    Include(fState, osSelected)
  else
    Exclude(fState, osSelected);
  DoSelect(Self, AValue);
end;

procedure TIObject.SetVertex(AIndex: integer; const AValue: TIFloatPoint);
begin
  // nothing
end;

procedure TIObject.SetVisible(AValue: boolean);
begin
  if fVisible = AValue then
    Exit;
  DoChanging(Self);
  fVisible := AValue;
  DoChanged(Self);
end;

procedure TIObject.BeginSave;
begin
  Include(fState, osSaving);
end;

procedure TIObject.EndSave;
begin
  Exclude(fState, osSaving);
end;

procedure TIObject.BeginLoad;
begin
  BeginUpdate;
  Include(fState, osLoading);
  RemoveChilds;
end;

procedure TIObject.EndLoad;
begin
  Exclude(fState, osLoading);
  EndUpdate;
end;

procedure TIObject.DoSetOwner(AValue: TIObject);
begin
  // nothing
end;

procedure TIObject.BeginUpdate;
begin
  Inc(fLockCount);
  if fLockCount = 1 then
  begin
    DoChanging(Self);
    Include(fState, osLocked);
  end;
end;

procedure TIObject.EndUpdate;
begin
  fLockCount := Max(0, fLockCount - 1);
  if fLockCount = 0 then
  begin
    Exclude(fState, osLocked);
    DoChanged(Self);
  end;
end;

procedure TIObject.SetBounds(ALeft, ATop, AWidth, AHeight: double);
begin
  Resize;
end;

procedure TIObject.SetBounds(const R: TIFloatRect);
begin
  with R do
    SetBounds(Left, Top, Width, Height);
end;

procedure TIObject.SetBoundsFromParent(R: TIFloatRect);
begin
  R.Offset(-R.Left, -R.Top);
  SetBounds(R);
end;

procedure TIObject.Refresh;
begin
  Resize;
end;

procedure TIObject.Resize;
begin
  // nothing
end;

procedure TIObject.SetOrder(AOrder: TOrder);
begin
  case AOrder of
    ordBringToFront:
      if Assigned(Owner) then
        SetZOrder(Owner.ChildsCount - 1);
    ordBringForward:
      if Assigned(Owner) then
        SetZOrder(EnsureRange(Index + 1, 0, Owner.ChildsCount - 1));
    ordSendBackward:
      if Assigned(Owner) then
        SetZOrder(EnsureRange(Index - 1, 0, Owner.ChildsCount - 1));
    ordSendToBack: SetZOrder(0);
  end;
end;

function TIObject.Has(AObject: TIObject): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to ChildsCount - 1 do
  begin
    Result := (Childs[i] = AObject) or (Childs[i].Has(AObject));
    if Result then
      Break;
  end;
end;

function TIObject.SendCommand(CmdID: cardinal; Param1: WPARAM; Param2: LPARAM): LRESULT;
var
  cmd: TICmdMessage;
begin
  cmd.CmdID := CmdID;
  cmd.Param1 := Param1;
  cmd.Param2 := Param2;
  SendCommand(cmd);
  Result := cmd.Result;
end;

function TIObject.Clipped(Canvas: TICanvas): boolean;
var
  R: TIFloatRect;
begin
  R := GetBounds(False);
  Result := Canvas.ClipRect.IntersectsWith(R);
end;

function TIObject.GetBounds(WithChilds: boolean): TIFloatRect;
var
  i: integer;
begin
  Result := CalcBounds;
  if WithChilds then
    for i := 0 to ChildsCount - 1 do
      Result.Union(Childs[i].GetBounds(True));
end;

function TIObject.AbsoluteToRelative(const Point: TIFloatPoint): TIFloatPoint;
begin
  Result := OwnerToRelative(nil, Point);
end;

function TIObject.RelativeToAbsolute(const Point: TIFloatPoint): TIFloatPoint;
begin
  Result := RelativeToOwner(nil, Point);
end;

function TIObject.OwnerToRelative(AOwner: TIObject;
  const Point: TIFloatPoint): TIFloatPoint;
var
  AObject: TIObject;
  Origin: TIFloatPoint;
begin
  AObject := fOwner;
  Result := Point;
  while AObject <> AOwner do
  begin
    Origin := AObject.GetSpaceOrigin;
    Result.Offset(-Origin.X, -Origin.Y);
    AObject := AObject.Owner;
  end;
end;

function TIObject.RelativeToOwner(AOwner: TIObject;
  const Point: TIFloatPoint): TIFloatPoint;
var
  AObject: TIObject;
  Origin: TIFloatPoint;
begin
  AObject := fOwner;
  Result := Point;
  while AObject <> AOwner do
  begin
    Origin := AObject.GetSpaceOrigin;
    Result.Offset(Origin);
    AObject := AObject.Owner;
  end;
end;

function TIObject.GetZOrder: integer;
begin
  Result := Index;
end;

function TIObject.Clone(CloneOwner: TIObject): TIObject;
begin
  Result := TIObjectClass(ClassType).Create(CloneOwner, fDocument);
  Result.Assign(Self);
end;

function TIObject.CloneAt(CloneOwner: TIObject; const X, Y: double): TIObject;
var
  i: integer;
  dX, dY: double;
  Pos: TIFloatPoint;
begin
  Result := Clone(CloneOwner);
  dX := X - GetSpaceOrigin.X;
  dY := Y - GetSpaceOrigin.Y;
  Include(Result.fState, osLoading);
  try
    for i := 0 to Result.BasePointsCount - 1 do
    begin
      Pos := Result.BasePoints[i];
      Pos.X += dX;
      Pos.Y += dY;
      Result.BasePoints[i] := Pos;
    end;
  finally
    Exclude(Result.fState, osLoading);
    Result.DoChanged(Result);
  end;
end;

procedure TIObject.Assign(Source: TIObject);
var
  Node: ITreeNode;
begin
  Node := TITreeNode.Create('');
  TISerializer.SaveToNode(Source, Node);
  TISerializer.LoadFromNode(Self, Node);
end;

procedure TIObject.SendCommand(var cmd);
var
  Intf: IInterface;
begin
  Intf := Self;
  DoCommandReceived(Self, TICmdMessage(Cmd));
  Dispatch(Cmd);
  DoCommandHandled(Self, TICmdMessage(Cmd));
end;

procedure TIObject.SetZOrder(AIndex: integer);
begin
  Index := AIndex;
end;

procedure TIObject.Draw(Canvas: TICanvas);
var
  i, n: Integer;
begin
  if Visible then
  begin
    if Clipped(Canvas) then
    begin
      Canvas.GraphicsSettings := GraphicsSettings;
      Canvas.MasterAlpha := fOpacity * 0.01;
      Paint(Canvas);
    end;
    n := ChildsCount;
    for i := 0 to n - 1 do
      Childs[i].Draw(Canvas);
  end;
end;

procedure TIObject.DoChanging(AValue: TIObject);
begin
  if osLocked in fState then
    Exit;
  if Assigned(fOnChanging) then
    fOnChanging(Self, AValue);
  if Assigned(Owner) then
    Owner.DoChanging(AValue);
end;

procedure TIObject.DoChanged(AValue: TIObject);
begin
  if osLocked in fState then
    Exit;
  if Assigned(fOnChanged) then
    fOnChanged(Self, AValue);
  if Assigned(Owner) then
    Owner.DoChanged(AValue);
end;

procedure TIObject.DoDestroying;
begin
  // nothing
end;

procedure TIObject.DoChildAdded(AValue: TIObject);
begin
  if Assigned(fOnChildAdded) then
    fOnChildAdded(Self, AValue);
  if Assigned(Owner) then
    Owner.DoChildAdded(AValue);
end;

procedure TIObject.DoChildRemove(AValue: TIObject);
begin
  if Assigned(fOnChildRemove) then
    fOnChildRemove(Self, AValue);
  if Assigned(Owner) then
    Owner.DoChildRemove(AValue);
end;

procedure TIObject.DoSelect(AValue: TIObject; Selected: boolean);
begin
  if Assigned(fOnSelected) then
    fOnSelected(Self, AValue, Selected);
  if Assigned(Owner) then
    Owner.DoSelect(AValue, Selected);
end;

procedure TIObject.DoCommandReceived(AValue: TIObject; var cmd: TICmdMessage);
begin
  if Assigned(fOnCommandReceived) then
    fOnCommandReceived(Self, AValue, cmd);
  if Assigned(Owner) then
    Owner.DoCommandReceived(AValue, cmd);
end;

procedure TIObject.DoCommandHandled(AValue: TIObject; var cmd: TICmdMessage);
begin
  if Assigned(fOnCommandHandled) then
    fOnCommandHandled(Self, AValue, cmd);
  if Assigned(Owner) then
    Owner.DoCommandHandled(AValue, cmd);
end;

procedure TIObject.DoRequestParams(AValue: TIObject; var cmd: TICmdMessage);
begin
  if Assigned(fOnRequestParams) then
    fOnRequestParams(Self, AValue, cmd);
  if (cmd.Result = 0) and Assigned(Owner) then
    Owner.DoRequestParams(AValue, cmd);
end;

procedure TIObject.CustomSerialize(const Node: ITreeNode);
var
  Pt: TIFloatPoint;
  NewNode, PointNode, ChildNode: ITreeNode;
  ChildTypeName: string;
  i: Integer;
  Factory: TIObjectsFactory;
begin
  NewNode := Node.AddNode('BasePoints');
  for i := 0 to BasePointsCount - 1 do begin
    Pt := BasePoints[i];
    PointNode := NewNode.AddNode('P' + IntToStr(i));
    PointNode.AddParam('X', Pt.X);
    PointNode.AddParam('Y', Pt.Y);
  end;
  if ChildsCount > 0 then begin
    Factory := TIObjectsFactory.GetInstance;
    NewNode := Node.AddNode('Childs');
    for i := 0 to ChildsCount - 1 do begin
      ChildTypeName := Factory.FindInstanceName(Childs[i]);
      ChildNode := NewNode.AddNode(ChildTypeName);
      TISerializer.SaveToNode(Childs[i], ChildNode);
    end;
  end;
end;

procedure TIObject.CustomDeSerialize(const Node: ITreeNode);
var
  i, j: Integer;
  Pt: TIFloatPoint;
  NewChild: TIObject;
  NewNode, PointNode: ITreeNode;
  Factory: TIObjectsFactory;
begin
  NewNode := Node.NodeByName['BasePoints'];
  if Assigned(NewNode) then
    for i := 0 to NewNode.NodesCount - 1 do
    begin
      PointNode := NewNode.Nodes[i];
      Pt.X := PointNode.ParamValue[0];
      Pt.Y := PointNode.ParamValue[1];
      BasePoints[i] := Pt;
    end;
  j := Node.NodeIndex['Childs'];
  if j > -1 then begin
    Factory := TIObjectsFactory.GetInstance;
    NewNode := Node.Nodes[j];
    for i := 0 to NewNode.NodesCount - 1 do begin
      NewChild := Factory.CreateInstance(Self, fDocument, NewNode.Nodes[i].Name);
      TISerializer.LoadFromNode(NewChild, NewNode.Nodes[i]);
    end;
  end;
end;

procedure TIObject.Move(dx, dy: double);
var
  i: integer;
  Pos: TIFloatPoint;
begin
  BeginUpdate;
  try
    for i := 0 to BasePointsCount - 1 do
    begin
      Pos := RelativeToAbsolute(BasePoints[i]);
      Pos.X := Pos.X + dx;
      Pos.Y := Pos.Y + dy;
      BasePoints[i] := AbsoluteToRelative(Pos);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TIObject.DrawNotify(Canvas: TICanvas);
begin
  // nothing
end;

procedure TIObject.Paint(Canvas: TICanvas);
begin
  // nothing
end;

function TIObject.ConstructPoint(AIndex: integer; Pos: TIFloatPoint): integer;
begin
  Result := 0;
end;

function TIObject.StopConstruct(AIndex: integer): integer;
begin
  Result := 0;
end;

function TIObject.ChildsHitTest: boolean;
begin
  Result := True;
end;

function TIObject.HitTest(Params: PIHitTestParams): cardinal;
var
  i: integer;
  sX, sY: double;
begin
  Result := HT_OUT;
  if osDestroying in fState then
    exit;
  for i := VertexesCount - 1 downto 0 do
  begin
    Params^.TranslateIntf.LogToScreen(Vertexes[i].X,
      Vertexes[i].Y, sX, sY);
    if (Abs(Params^.XPos - sX) <= Params^.Tolerance) and
      (Abs(Params^.YPos - sY) <= Params^.Tolerance) then
    begin
      Result := HT_VERTEX + i;
      Exit;
    end;
  end;
end;

function TIObject.GetLayer: TIBaseLayer;
var
  obj: TIObject;
begin
  result := nil;
  obj := Owner;
  while Assigned(obj) do
  begin
    if obj is TILayer then
    begin
      result := TIBaseLayer(obj);
      exit;
    end;
    obj := obj.Owner;
  end;
end;

function TIObject.GetIsLocked: boolean;
var
  lr: TIBaseLayer;
begin
  result := fLocked;
  if result then
     exit;
  lr := GetLayer;
  if Assigned(lr) then
     result := lr.Locked;
end;

function TIObject.CalcBounds: TIFloatRect;
var
  i: integer;
  Pos: TIFloatPoint;
begin
  if VertexesCount = 0 then
    Result := TIFloatRect.Empty
  else
  begin
    Pos := Vertexes[0];
    Result := TIFloatRect.Create(Pos, Pos);
    for i := 1 to VertexesCount - 1 do
    begin
      Pos := Vertexes[i];
      Result.Left := Min(Pos.X, Result.Left);
      Result.Right := Max(Pos.X, Result.Right);
      Result.Top := Min(Pos.Y, Result.Top);
      Result.Bottom := Max(Pos.Y, Result.Bottom);
    end;
  end;
end;

function TIObject.HitTest(Params: PIHitTestParams; var AObject: TIObject): cardinal;
var
  i: integer;
begin
  AObject := Self;
  Result := HT_OUT;
  if Visible then
  begin
    if ChildsHitTest then
    begin
      for i := ChildsCount - 1 downto 0 do
      begin
        Result := Childs[i].HitTest(Params, AObject);
        if Result <> HT_OUT then
          Break;
      end;
    end;
    if Result = HT_OUT then
    begin
      AObject := Self;
      Result := HitTest(Params);
    end;
  end;
end;

function TIObject.GetBasePointsCount: integer;
begin
  Result := 0;
end;

function TIObject.GetSpaceOrigin: TIFloatPoint;
begin
  Result := BasePoints[0];
end;

procedure TIObject.CmdGetCursor(var cmd: TICmdMessage);
begin
  cmd.Result := CR_DEFAULT;
end;

procedure TIObject.CmdBeginDrag(var cmd: TICmdMessage);
begin
  Include(fState, osDragging);
  fDragHitTest := cmd.Param1;
  fDragStartPos := PIFloatPoint(cmd.Param2)^;
  fDragCancelPos := fDragStartPos;
end;

procedure TIObject.CmdEndDrag(var cmd: TICmdMessage);
begin
  Exclude(fState, osDragging);
end;

procedure TIObject.CmdCancelDrag(var cmd: TICmdMessage);
begin
  SendCommand(CMD_DRAG, 0, LPARAM(@FDragCancelPos));
  Exclude(fState, osDragging);
end;

procedure TIObject.CmdDrag(var cmd: TICmdMessage);
var
  hit: cardinal;
  i: integer;
  DeltaX, DeltaY: double;
begin
  if osDragging in fState then
  begin
    hit := fDragHitTest and $FFFF0000;
    i := fDragHitTest and $0000FFFF;
    case hit of
      HT_IN:
      begin
        DeltaX := PIFloatPoint(cmd.Param2)^.X - fDragStartPos.X;
        DeltaY := PIFloatPoint(cmd.Param2)^.Y - fDragStartPos.Y;
        fDragStartPos := PIFloatPoint(cmd.Param2)^;
        SendCommand(CMD_MOVE, WPARAM(@DeltaX), LPARAM(@DeltaY));
      end;
      HT_VERTEX: SendCommand(CMD_VERTEXMOVE, WPARAM(i), cmd.Param2);
      HT_SIDE: SendCommand(CMD_SIDEMOVE, WPARAM(i), cmd.Param2);
    end;
  end;
end;

procedure TIObject.CmdVertexMove(var cmd: TICmdMessage);
begin
  Vertexes[cmd.Param1] := PIFloatPoint(cmd.Param2)^;
end;

procedure TIObject.CmdMove(var cmd: TICmdMessage);
begin
  Move(PExtended(cmd.Param1)^, PExtended(cmd.Param2)^);
end;

procedure TIObject.CmdConstructPoint(var cmd: TICmdMessage);
begin
  if (ckOneShort in ConstructKind) or (ckStepByStep in ConstructKind) then
  begin
    if not (osConstructing in fState) then
      fConstructingPointIndex := 0;
    Include(fState, osConstructing);
    cmd.Result := ConstructPoint(fConstructingPointIndex, PIFloatPoint(cmd.Param2)^);
    if cmd.Result <> 0 then
    begin
      Exclude(fState, osConstructing);
      fConstructingPointIndex := 0;
    end
    else
      Inc(fConstructingPointIndex);
  end;
end;

procedure TIObject.CmdStopConstruct(var cmd: TICmdMessage);
begin
  cmd.Result := 0;
  if osConstructing in fState then
  begin
    cmd.Result := StopConstruct(fConstructingPointIndex);
    if cmd.Result = 0 then
      Exclude(fState, osConstructing)
    else
      Free;
  end;
end;

procedure TIObject.CmdEndConstruct(var cmd: TICmdMessage);
begin
  cmd.Result := 0;
  if osConstructing in fState then
  begin
    cmd.Result := StopConstruct(fConstructingPointIndex);
    Exclude(fState, osConstructing);
  end;
end;

procedure TIObject.CmdClipbrdGetData(var cmd: TICmdMessage);
begin
  cmd.Result := 0;
end;

procedure TIObject.CmdClipbrdFreeData(var cmd: TICmdMessage);
begin
  cmd.Result := 0;
end;


{ TIBaseLayer }

procedure TIBaseLayer.SetName(const Value: string);
begin
  if fName = Value then
    exit;
  DoChanging(Self);
  fName := Value;
  DoChanged(Self);
end;

function TIBaseLayer.GetBasePointsCount: integer;
begin
  Result := 1;
end;

function TIBaseLayer.GetBasePoint(AIndex: integer): TIFloatPoint;
begin
  Result := TIFloatPoint.Zero;
  if AIndex <> 0 then
    TObjectList.Error(SListIndexError, AIndex);
end;

procedure TIBaseLayer.SetBasePoint(AIndex: integer; Value: TIFloatPoint);
begin
  // nothing
end;

function TIBaseLayer.GetVertex(AIndex: integer): TIFloatPoint;
begin
  Result := TIFloatPoint.Zero;
  TObjectList.Error(SListIndexError, AIndex);
end;

function TIBaseLayer.GetVertexesCount: integer;
begin
  Result := 0;
end;

procedure TIBaseLayer.SetVertex(AIndex: integer; const Value: TIFloatPoint);
begin
  inherited SetVertex(AIndex, Value);
end;

function TIBaseLayer.GetVertexKind(AIndex: integer): word;
begin
  Result := VK_NONE;
end;

function TIBaseLayer.HitTest(AParams: PIHitTestParams): cardinal;
begin
  Result := HT_OUT;
end;

procedure TIBaseLayer.DoDestroying;
begin
  if Assigned(Owner) then
  Owner.DoChildRemove(self);
end;

function TIBaseLayer.ConstructKind: TIConstructKinds;
begin
  Result := [];
end;

{ TIGroup }

procedure TIGroup.CMDGetCursor(var cmd: TICmdMessage);
begin
  if cmd.Param2 = HT_IN then
    cmd.Result := CR_SIZEALL
  else
    cmd.Result := CR_DEFAULT;
end;

class function TIGroup.GetObjectName: string;
begin
  Result := 'Group';
end;

function TIGroup.GetBasePointsCount: Integer;
begin
  Result := 1;
end;

function TIGroup.GetBasePoint(AIndex: Integer): TIFloatPoint;
begin
  Result := fBasePoint;
  if AIndex <> 0 then
    TObjectList.Error(SListIndexError, AIndex);
end;

procedure TIGroup.SetBasePoint(AIndex: Integer; Value: TIFloatPoint);
begin
  if AIndex <> 0 then
    TObjectList.Error(SListIndexError, AIndex)
  else begin
    DoChanging(Self);
    fBasePoint := Value;
    DoChanged(Self);
  end;
end;

function TIGroup.GetVertex(AIndex: integer): TIFloatPoint;
var
  Rect: TIFloatRect;
begin
  Result := TIFloatPoint.Zero;
  if (AIndex < 0) or (AIndex > VertexesCount - 1) then
    TObjectList.Error(SListIndexError, AIndex)
  else
  begin
    Rect := GetBounds(True);
    case AIndex of
      0: Result := Rect.TopLeft;
      1:
      begin
        Result.X := Rect.Right;
        Result.Y := Rect.Top;
      end;
      2: Result := Rect.BottomRight;
      3:
      begin
        Result.X := Rect.Left;
        Result.Y := Rect.Bottom;
      end;
    end;
  end;
end;

function TIGroup.GetVertexesCount: integer;
begin
  Result := 4;
end;

function TIGroup.GetVertexKind(AIndex: integer): word;
begin
  Result := VK_NONE;
end;

function TIGroup.CalcBounds: TIFloatRect;
var
  p: TIFloatPoint;
begin
  p := RelativeToAbsolute(fBasePoint);
  Result := TIFloatRect.Create(p.X, p.Y, p.X, p.Y);
end;

function TIGroup.ChildsHitTest: Boolean;
begin
  Result := False;
end;

function TIGroup.HitTest(Params: PIHitTestParams): Cardinal;
var
  sX1, sY1, sX2, sY2: double;
begin
  Result := HT_OUT;
  Params^.TranslateIntf.LogToScreen(Vertexes[0].X, Vertexes[0].Y, sX1, sY1);
  Params^.TranslateIntf.LogToScreen(Vertexes[2].X, Vertexes[2].Y, sX2, sY2);
  if (Params^.XPos >= sX1) and (Params^.XPos <= sX2) and
     (Params^.YPos >= sY1) and (Params^.YPos <= sY2)
  then
    Result := HT_IN;
end;

procedure TIGroup.DoChanging(AObject: TIObject);
begin
  if not fUnGrouping then
    inherited DoChanging(AObject);
end;

procedure TIGroup.DoChanged(AObject: TIObject);
begin
  if not fUnGrouping then
    inherited DoChanged(AObject);
end;

class function TIGroup.CanGroup(Objects: TObjectList): Boolean;
var
  i: Integer;
  co: TIObject;
begin
  if Objects.Count = 0 then
    Result := False
  else begin
    Result := True;
    co := (Objects[0] as TIObject).Owner;
    for i := 0 to Objects.Count - 1 do
      Result := Result and (Objects[i] is TIObject) and
        (TIObject(Objects[i]).Owner = co);
  end;
end;

class function TIGroup.Group(Objects: TObjectList; ADocument: TObject): TIGroup;
var
  Origin, NewOrigin, p: TIFloatPoint;
  dx, dy: double;
  i: Integer;
  IsSelected: Boolean;
  Obj, Go: TIObject;
begin
  Result := nil;
  if CanGroup(Objects) then begin
    Go := TIObject(Objects[0]).Owner;
    p := TIObject(Objects[0]).GetSpaceOrigin;
    p := TIObject(Objects[0]).RelativeToOwner(Go, p);
    for i := 1 to Objects.Count - 1 do begin
      Origin := TIObject(Objects[i]).GetSpaceOrigin;
      Origin := TIObject(Objects[i]).RelativeToOwner(Go, Origin);
      p.X := Min(p.X, Origin.X);
      p.Y := Min(p.Y, Origin.Y);
    end;
    Result := TIGroup.Create(nil, ADocument);
    Result.FBasePoint := p;
    IsSelected := False;
    for i := 0 to Objects.Count - 1 do begin
      Obj := TIObject(Objects[i]);
      Origin := Obj.RelativeToAbsolute(Obj.GetSpaceOrigin);
      Obj.Owner := Result;
      NewOrigin := Obj.RelativeToAbsolute(Obj.GetSpaceOrigin);
      dx := NewOrigin.X - Origin.X;
      dy := NewOrigin.Y - Origin.Y;
      Obj.Move(-dx, -dy);
      IsSelected := IsSelected or Obj.Selected;
      Obj.Selected := False;
    end;
    Result.Selected := IsSelected;
    Result.Owner := Go;
  end;
end;

function TIGroup.ConstructKind: TIConstructKinds;
begin
  Result := [ckInstantly];
end;

procedure TIGroup.UnGroup;
var
  Origin, NewOrigin: TIFloatPoint;
  dx, dy: double;
  Obj: TIObject;
begin
  DoChanging(Self);
  FUnGrouping := True;
  while ChildsCount > 0 do begin
    Obj := Childs[0];
    Origin := Obj.RelativeToAbsolute(Obj.GetSpaceOrigin);
    Obj.Owner := Owner;
    Obj.BeginUpdate;
    try
      NewOrigin := Obj.RelativeToAbsolute(Obj.GetSpaceOrigin);
      dx := NewOrigin.X - Origin.X;
      dy := NewOrigin.Y - Origin.Y;
      Obj.Move(-dx, -dy);
      Obj.Selected := Selected;
    finally
      Obj.EndUpdate;
    end;
  end;
  Free;
end;


{ TIBaseRect }

constructor TIBaseRect.Create(AOwner: TIObject; ADocument: TObject;
  X1, Y1, X2, Y2: double);
begin
  inherited Create(AOwner, ADocument);
  fBasePoints[0] := TIFloatPoint.Create(Min(X1, X2), Min(Y1, Y2));
  fBasePoints[1] := TIFloatPoint.Create(Max(X1, X2), Max(Y1, Y2));
  fCornerRadius := 0;
end;

constructor TIBaseRect.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
end;

procedure TIBaseRect.CMDGetCursor(var cmd: TICmdMessage);
begin
  case cmd.Param2 of
    HT_IN: cmd.Result := CR_SIZEALL;
    HT_VERTEX + 0, HT_VERTEX + 2: cmd.Result := CR_DIAG1;
    HT_VERTEX + 1, HT_VERTEX + 3: cmd.Result := CR_DIAG2;
    HT_SIDE + 0, HT_SIDE + 2: cmd.Result := CR_HORIZONTAL;
    HT_SIDE + 1, HT_SIDE + 3: cmd.Result := CR_VERTICAL;
    else
      cmd.Result := CR_DEFAULT;
  end;
end;

procedure TIBaseRect.CMDSideMove(var cmd: TICmdMessage);
var
  p, np: TIFloatPoint;
begin
  p := PIFloatPoint(cmd.Param2)^;
  p := AbsoluteToRelative(p);
  case cmd.Param1 of
    0:
    begin
      np.X := Min(p.X, BasePoints[1].X);
      np.Y := BasePoints[0].Y;
      BasePoints[0] := np;
    end;
    1:
    begin
      np.X := BasePoints[0].X;
      np.Y := Min(p.Y, BasePoints[1].Y);
      BasePoints[0] := np;
    end;
    2:
    begin
      np.X := Max(p.X, BasePoints[0].X);
      np.Y := BasePoints[1].Y;
      BasePoints[1] := np;
    end;
    3:
    begin
      np.X := BasePoints[1].X;
      np.Y := Max(p.Y, BasePoints[0].Y);
      BasePoints[1] := np;
    end;
    else
      TList.Error(SListIndexError, cmd.Param1);
  end;
end;

procedure TIBaseRect.CMDProcessConstruct(var cmd: TICmdMessage);
begin
  case ConstructingPointIndex of
    0: Vertexes[0] := PIFloatPoint(cmd.Param2)^;
    1: Vertexes[2] := PIFloatPoint(cmd.Param2)^;
  end;
end;

function TIBaseRect.GetHeight: double;
begin
  result := fBasePoints[1].Y - fBasePoints[0].Y;
end;

function TIBaseRect.GetLeft: double;
begin
  result := fBasePoints[0].X;
end;

function TIBaseRect.GetTop: double;
begin
  result := fBasePoints[0].Y;
end;

function TIBaseRect.GetWidth: double;
begin
  result := fBasePoints[1].X - fBasePoints[0].X;
end;

procedure TIBaseRect.SetCornerRadius(AValue: double);
begin
  if fCornerRadius = AValue then
    Exit;
  DoChanging(Self);
  fCornerRadius := AValue;
  DoChanged(Self);
end;

procedure TIBaseRect.SetHeight(AValue: double);
var
  p: TIFloatPoint;
begin
  if AValue = GetHeight then
     exit;
  p := fBasePoints[1];
  p.Y := fBasePoints[0].Y + AValue;
  SetBasePoint(1, p);
end;

procedure TIBaseRect.SetLeft(AValue: double);
var
  p1, p2: TIFloatPoint;
  w: double;
begin
  if AValue = GetLeft then
     exit;
  p1 := fBasePoints[0];
  p2 := fBasePoints[1];
  w := p2.X - p1.X;
  p1.X := AValue;
  p2.X := AValue + w;
  DoChanging(Self);
  fBasePoints[0] := p1;
  fBasePoints[1] := p2;
  Resize;
  DoChanged(Self);
end;

procedure TIBaseRect.SetTop(AValue: double);
var
  p1, p2: TIFloatPoint;
  h: double;
begin
  if AValue = GetTop then
     exit;
  p1 := fBasePoints[0];
  p2 := fBasePoints[1];
  h := p2.Y - p1.Y;
  p1.Y := AValue;
  p2.Y := AValue + h;
  DoChanging(Self);
  fBasePoints[0] := p1;
  fBasePoints[1] := p2;
  Resize;
  DoChanged(Self);
end;

procedure TIBaseRect.SetWidth(AValue: double);
var
  p: TIFloatPoint;
begin
  if AValue = GetWidth then
     exit;
  p := fBasePoints[1];
  p.X := fBasePoints[0].X + AValue;
  SetBasePoint(1, p);
end;

function TIBaseRect.GetRect: TIFloatRect;
begin
  result := TIFloatRect.Create(RelativeToAbsolute(BasePoints[0]),
    RelativeToAbsolute(BasePoints[1]));
end;

procedure TIBaseRect.NormalizeVertexes;
begin
  NormalizeCoord(fBasePoints[0].X, fBasePoints[1].X);
  NormalizeCoord(fBasePoints[0].Y, fBasePoints[1].Y);
end;

function TIBaseRect.GetBasePointsCount: integer;
begin
  Result := 2;
end;

function TIBaseRect.GetBasePoint(AIndex: integer): TIFloatPoint;
begin
  Result := fBasePoints[AIndex];   //Result := RelativeToAbsolute(fBasePoints[AIndex]);
end;

function TIBaseRect.GetVertex(AIndex: integer): TIFloatPoint;
begin
  case AIndex of
    0: Result := RelativeToAbsolute(BasePoints[0]);
    1:
    begin
      Result.X := BasePoints[1].X;
      Result.Y := BasePoints[0].Y;
      Result := RelativeToAbsolute(Result);
    end;
    2: Result := RelativeToAbsolute(BasePoints[1]);
    3:
    begin
      Result.X := BasePoints[0].X;
      Result.Y := BasePoints[1].Y;
      Result := RelativeToAbsolute(Result);
    end;
    else
      TList.Error(SListIndexError, AIndex);
  end;
end;

function TIBaseRect.GetVertexesCount: integer;
begin
  Result := 4;
end;

function TIBaseRect.GetVertexKind(AIndex: integer): word;
begin
  Result := VK_MOVE;
end;

function TIBaseRect.ConstructPoint(AIndex: integer; Pos: TIFloatPoint): integer;
begin
  if AIndex = 0 then
    BasePoints[0] := AbsoluteToRelative(Pos);
  BasePoints[1] := AbsoluteToRelative(Pos);
  Result := AIndex;
end;

function TIBaseRect.StopConstruct(AIndex: integer): integer;
begin
  NormalizeVertexes;
  Result := 1;
end;

function TIBaseRect.HitTest(Params: PIHitTestParams): cardinal;
var
  sX1, sY1, sX2, sY2: double;
begin
  Result := inherited HitTest(Params);
  if Result = HT_OUT then
  begin
    Params^.TranslateIntf.LogToScreen(Vertexes[0].X, Vertexes[0].Y, sX1, sY1);
    Params^.TranslateIntf.LogToScreen(Vertexes[2].X, Vertexes[2].Y, sX2, sY2);
    if (Abs(Params^.XPos - sX1) <= Params^.Tolerance) and
      (Params^.YPos > sY1) and (Params^.YPos < sY2) then
    begin
      // SIDE_LEFT
      Result := HT_SIDE + 0;
      Exit;
    end;
    if (Abs(Params^.YPos - sY1) <= Params^.Tolerance) and
      (Params^.XPos > sX1) and (Params^.XPos < sX2) then
    begin
      // SIDE_TOP
      Result := HT_SIDE + 1;
      Exit;
    end;
    if (Abs(Params^.XPos - sX2) <= Params^.Tolerance) and
      (Params^.YPos > sY1) and (Params^.YPos < sY2) then
    begin
      // SIDE_RIGHT
      Result := HT_SIDE + 2;
      Exit;
    end;
    if (Abs(Params^.YPos - sY2) <= Params^.Tolerance) and
      (Params^.XPos > sX1) and (Params^.XPos < sX2) then
    begin
      // SIDE_BOTTOM
      Result := HT_SIDE + 3;
      Exit;
    end;
    if (Params^.XPos > sX1) and (Params^.XPos < sX2) and
      (Params^.YPos > sY1) and (Params^.YPos < sY2) then
    begin
      // inside
      Result := HT_IN;
      Exit;
    end;
  end;
end;

procedure TIBaseRect.SetBasePoint(AIndex: integer; Value: TIFloatPoint);
begin
  DoChanging(Self);
  fBasePoints[AIndex] := Value;
  Resize;
  DoChanged(Self);
end;

procedure TIBaseRect.SetVertex(AIndex: integer; const Value: TIFloatPoint);
var
  p, np: TIFloatPoint;
begin
  if VertexesKind[Index] = VK_MOVE then
  begin
    p := AbsoluteToRelative(Value);
    case AIndex of
      0:
      begin
        np.X := Min(p.X, BasePoints[1].X);
        np.Y := Min(p.Y, BasePoints[1].Y);
        BasePoints[0] := np;
      end;
      1:
      begin
        np.X := BasePoints[0].X;
        np.Y := Min(p.Y, BasePoints[1].Y);
        BasePoints[0] := np;
        np.X := Max(p.X, BasePoints[0].X);
        np.Y := BasePoints[1].Y;
        BasePoints[1] := np;
      end;
      2:
      begin
        if osConstructing in fState then
          np := p
        else
        begin
          np.X := Max(p.X, BasePoints[0].X);
          np.Y := Max(p.Y, BasePoints[0].Y);
        end;
        BasePoints[1] := np;
      end;
      3:
      begin
        np.X := BasePoints[1].X;
        np.Y := Max(p.Y, BasePoints[0].Y);
        BasePoints[1] := np;
        np.X := Min(p.X, BasePoints[1].X);
        np.Y := BasePoints[0].Y;
        BasePoints[0] := np;
      end;
      else
        TList.Error(SListIndexError, AIndex);
    end;
  end;
end;

function TIBaseRect.ConstructKind: TIConstructKinds;
begin
  Result := [ckInstantly, ckOneShort];
end;

procedure TIBaseRect.SetBounds(ALeft, ATop, AWidth, AHeight: double);
begin
  fBasePoints[0] := TIFloatPoint.Create(ALeft, ATop);
  fBasePoints[1] := TIFloatPoint.Create(ALeft + AWidth, ATop + AHeight);
  Resize;
end;


{ TISpan }

constructor TISpan.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  SetBounds(0, 0, 50, 25);
end;

function TISpan.ConstructKind: TIConstructKinds;
begin
  Result := [ckInstantly];
end;

function TISpan.GetCodeStr: string;
begin
  result := '';
end;

{ TIBaseText }

constructor TIBaseText.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fText := 'Text';
  fBasePoints[0] := TIFloatPoint.Zero;
  fBasePoints[1] := TIFloatPoint.Create(50, 25);
end;

procedure TIBaseText.SetText(const Value: string);
begin
  if fText = Value then
    exit;
  DoChanging(Self);
  fText := Value;
  DoChanged(Self);
end;

function TIBaseText.GetFont: TIFont;
begin
  result := fGraphicsSettings.Font;
end;

procedure TIBaseText.SetFont(AValue: TIFont);
begin
  fGraphicsSettings.Font := AValue;
end;

function TIBaseText.GetTextVertex(AIndex: integer): TIFloatPoint;
begin
  case AIndex of
    0: Result := RelativeToAbsolute(fTextRect.TopLeft);
    1:
    begin
      Result.X := fTextRect.Right;
      Result.Y := fTextRect.Top;
      Result := RelativeToAbsolute(Result);
    end;
    2: Result := RelativeToAbsolute(fTextRect.BottomRight);
    3:
    begin
      Result.X := fTextRect.Left;
      Result.Y := fTextRect.Bottom;
      Result := RelativeToAbsolute(Result);
    end;
    else
      TList.Error(SListIndexError, AIndex);
  end;
end;

function TIBaseText.ConstructKind: TIConstructKinds;
begin
  Result := [ckInstantly];
end;

procedure TIBaseText.Paint(Canvas: TICanvas);
begin
  Canvas.DrawText(Text, GetRect);
end;


{ TIBaseLine }

procedure TIBaseLine.CmdGetCursor(var cmd: TICmdMessage);
begin
  if cmd.Param2 <> HT_OUT then
    cmd.Result := CR_SIZEALL
  else
    inherited;
end;

procedure TIBaseLine.CMDProcessConstruct(var cmd: TICmdMessage);
begin
  Vertexes[ConstructingPointIndex] := PIFloatPoint(cmd.Param2)^;
end;

function TIBaseLine.SideCode: Cardinal;
begin
  Result := HT_IN;
end;

function TIBaseLine.GetVertexesCount: Integer;
begin
  Result := BasePointsCount;
end;

function TIBaseLine.GetVertex(AIndex: Integer): TIFloatPoint;
begin
  Result := RelativeToAbsolute(BasePoints[AIndex]);
end;

function TIBaseLine.GetVertexKind(AIndex: Integer): Word;
begin
  Result := VK_MOVE;
end;

function TIBaseLine.HitTest(Params: PIHitTestParams): Cardinal;
var
  i : Integer;
  sX1, sY1, sX2, sY2, D: double;
begin
  Result := inherited HitTest(Params);
  if osDestroying in fState then
    exit;
  if Result = HT_OUT then begin
    for i := VertexesCount - 1 downto 1 do begin
      Params^.TranslateIntf.LogToScreen(Vertexes[i].X, Vertexes[i].Y, sX1, sY1);
      Params^.TranslateIntf.LogToScreen(Vertexes[i - 1].X, Vertexes[i - 1].Y, sX2, sY2);
      D := LineDistance(Params^.XPos, Params^.YPos, sX1, sY1, sX2, sY2);
      if D <= Params^.Tolerance then begin
        Result := SideCode + Cardinal(i);
        Exit;
      end;
    end;
  end;
end;

procedure TIBaseLine.SetVertex(AIndex: integer; const AValue: TIFloatPoint);
begin
  BasePoints[AIndex] := AbsoluteToRelative(AValue);
end;

function TIBaseLine.ConstructKind: TIConstructKinds;
begin
  Result := [ckStepByStep];
end;

{ TIBasePoly }

constructor TIBasePoly.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fBasePoints := TList.Create;
end;

constructor TIBasePoly.Create(AOwner: TIObject; ADocument: TObject;
  Pts: array of TIFloatPoint);
var
  i: Integer;
begin
  Create(AOwner, ADocument);
  for i := Low(Pts) to High(Pts) do
    AddBasePoint(Pts[i].X, Pts[i].Y);
end;

destructor TIBasePoly.Destroy;
begin
  ClearBasePoints;
  FreeAndNil(fBasePoints);
  inherited Destroy;
end;

procedure TIBasePoly.CMDSpecCommand(var cmd: TICmdMessage);
var
  i: Integer;
  c: Cardinal;
  p: TIFloatPoint;
begin
  c := Cardinal(cmd.Param1);
  p := PIFloatPoint(cmd.Param2)^;
  i := c and $0000FFFF;
  if (c and HT_VERTEX <> 0) and (VertexesCount > GetMinPointsCount) then
    DeleteBasePoint(i);
  if c and SideCode <> 0 then
    InsertBasePoint(i, p.X, p.Y);
end;

function TIBasePoly.GetBasePointsCount: Integer;
begin
  if Assigned(fBasePoints) then
    Result := fBasePoints.Count
  else
    Result := 0;
end;

function TIBasePoly.GetBasePoint(AIndex: Integer): TIFloatPoint;
begin
  Result := PIFloatPoint(fBasePoints[AIndex])^;
end;

procedure TIBasePoly.SetBasePoint(AIndex: Integer; Value: TIFloatPoint);
begin
  DoChanging(Self);
  if (osLoading in State) and (AIndex >= fBasePoints.Count) then
    AddBasePoint(Value.X, Value.Y)
  else
  begin
    if (AIndex >= 0) and (AIndex < fBasePoints.Count) then
       PIFloatPoint(fBasePoints[AIndex])^ := Value;
  end;
  DoChanged(Self);
end;

function TIBasePoly.ConstructPoint(AIndex: Integer; Pos: TIFloatPoint): Integer;
begin
  Result := 0;
  Pos := AbsoluteToRelative(Pos);
  AddBasePoint(Pos.X, Pos.Y);
  if AIndex = 0 then
    AddBasePoint(Pos.X, Pos.Y);
end;

function TIBasePoly.StopConstruct(AIndex: Integer): Integer;
begin
  Result := IfThen(ConstructingPointIndex < GetMinPointsCount, 1, 0);
  if Result = 0 then
    DeleteBasePoint(AIndex);
end;

procedure TIBasePoly.BeginLoad;
begin
  inherited BeginLoad;
  ClearBasePoints;
end;

procedure TIBasePoly.AddBasePoint(X, Y: double);
begin
  InsertBasePoint(BasePointsCount, X, Y);
end;

procedure TIBasePoly.InsertBasePoint(AIndex: Integer; X, Y: double);
var
  APoint: PIFloatPoint;
begin
  DoChanging(Self);
  GetMem(APoint, SizeOf(TIFloatPoint));
  APoint^.X := X;
  APoint^.Y := Y;
  fBasePoints.Insert(AIndex, APoint);
  DoChanged(Self);
end;

procedure TIBasePoly.DeleteBasePoint(AIndex: Integer);
begin
  DoChanging(Self);
  FreeMem(fBasePoints[AIndex]);
  fBasePoints.Delete(AIndex);
  DoChanged(Self);
end;

procedure TIBasePoly.ClearBasePoints;
var
  i: Integer;
begin
  DoChanging(Self);
  for i := 0 to fBasePoints.Count - 1 do
    FreeMem(fBasePoints[i]);
  fBasePoints.Clear;
  DoChanged(Self);
end;

{ TIObjectsFactory }

constructor TIObjectsFactory.Create;
begin
  inherited Create;
  ResetID;
  fFactoryList := TList.Create;
end;

destructor TIObjectsFactory.Destroy;
begin
  fFactoryList.Free;
  inherited Destroy;
end;

function TIObjectsFactory.QueryInterface(constref IID: TGUID; out Obj
  ): longint; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIObjectsFactory._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TIObjectsFactory._Release: longint; stdcall;
begin
  Result := -1;
end;

procedure TIObjectsFactory.ClassRegistered(AClassInfo: TIClassInfo);
var
  Cls: TIObjectClass;
  Name: string;
begin
  if AClassInfo.ClsType.InheritsFrom(TIObject) then
  begin
    Cls := TIObjectClass(AClassInfo.ClsType);
    Name := Cls.GetObjectName;
    if Name <> '' then
      AClassInfo.Attributes.Add(SNameAttr, Name);
  end;
end;

procedure TIObjectsFactory.ClassUnregistered(AClassInfo: TIClassInfo);
begin
  // nothing
end;

function TIObjectsFactory.CreateInstance(AOwner: TIObject; ADocument: TObject;
  const Name: string): TIObject;
var
  i: Integer;
  Reg: TIClassList;
  Info: TIClassInfo;
begin
  Result := nil;
  Reg := TIClassList.GetInstance;
  Info := Reg.GetClassByAttr(SNameAttr, Name);
  if (Info <> nil) and (Info.ClsType.InheritsFrom(TIObject)) then
    Result := TIObjectClass(Info.ClsType).Create(AOwner, ADocument)
  else
    for i := 0 to FactoryCount - 1 do begin
      Result := Factory[i].CreateInstance(AOwner, Name);
      if Assigned(Result) then
        Break;
    end;
  if Result = nil then
    raise EIException.CreateFmt(SClassNotRegistered, [Name]);
end;

function TIObjectsFactory.CreateObjectID: QWord;
begin
  result := fIDCounter;
  Inc(fIDCounter);
end;

function TIObjectsFactory.GetInstanceName(AObject: TIObject): string;
var
  i: Integer;
begin
  Result := AObject.GetObjectName;
  if Result = '' then
    for i := 0 to FactoryCount - 1 do begin
      Result := Factory[i].GetInstanceName(AObject);
      if Result <> '' then
        Break;
    end;
end;

function TIObjectsFactory.FindInstanceName(AObject: TIObject): string;
begin
   Result := GetInstanceName(AObject);
  if Result = '' then
    raise EIException.CreateFmt(SClassNotRegistered, [AObject.ClassName]);
end;

procedure TIObjectsFactory.RegisterFactory(const AFactory: TIObjectsFactory);
begin
  if fFactoryList.IndexOf(Pointer(AFactory)) < 0 then
    fFactoryList.Add(Pointer(AFactory));
end;

procedure TIObjectsFactory.UnregisterFactory(const AFactory: TIObjectsFactory);
begin
  fFactoryList.Remove(Pointer(AFactory));
end;

procedure TIObjectsFactory.ResetID;
begin
  fIDCounter := 1;
end;

function TIObjectsFactory.GetFactory(Index: Integer): IObjectsFactory;
begin
  Result := IObjectsFactory(fFactoryList[Index])
end;

function TIObjectsFactory.GetFactoryCount: Integer;
begin
  Result := fFactoryList.Count;
end;


end.
