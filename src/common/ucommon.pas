{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uCommon;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Types, Classes, Contnrs, SysUtils, LCLType, agg_fpimage;

var
  UnitsPerInch: integer = 96;
  ReplaceRoboto: boolean = true;
  IniPath: string;

const

  RulerSize = 16;
  AppTitle = 'IPlug Designer';
  AppDescription = 'IPlug Designer Graphics and Code tool';

  CMD_NULL             = 0;
  CMD_GETCURSOR        = 1;
  CMD_BEGINDRAG        = 2;
  CMD_ENDDRAG          = 3;
  CMD_DRAG             = 4;
  CMD_CANCELDRAG       = 5;
  CMD_VERTEXMOVE       = 6;
  CMD_SIDEMOVE         = 7;
  CMD_MOVE             = 8;
  CMD_CONSTRUCTPOINT   = 9;
  CMD_PROCESSCONSTRUCT = 10;
  CMD_ENDCONSTRUCT     = 11;
  CMD_STOPCONSTRUCT    = 12;
  CMD_SPECCOMMAND      = 13;
  CMD_CLIPBRDGETDATA   = 14;
  CMD_CLIPBRDFREEDATA  = 15;
  CMD_USER             = 16;

  RQS_GETMOUSEMODE     = 1;
  RQS_GETUNITSPERINCH  = 2;

  VK_NONE         = 0;
  VK_MOVE         = 1;
  VK_SCALE        = 2;
  VK_ROTATE       = 3;
  VK_SHEAR        = 4;
  VK_ROTATEORIGIN = 5;
  VK_USER         = 6;

  HT_OUT          = $00000000;
  HT_IN           = $80000000;
  HT_VERTEX       = $40000000;
  HT_SIDE         = $20000000;
  HT_TEXT         = $10000000;

  CR_DEFAULT      = 0;
  CR_ARROW        = 1;
  CR_SIZEALL      = 2;
  CR_HORIZONTAL   = 3;
  CR_VERTICAL     = 4;
  CR_DIAG1        = 5;
  CR_DIAG2        = 6;
  CR_ROTATE       = 7;
  CR_SHEAR        = 8;
  CR_USER         = 9;

  DEFAULT_UNDODEPTH = 100;
  DEFAULT_FONT = 'Roboto';
  DEFAULT_TEXT_SIZE = 14;
  DEFAULT_STYLE_NAME = 'DEFAULT_STYLE';
  DEFAULT_ACT_FUNCTION = 'nullptr';
  DEFAULT_CLICK_ACTION_FUNC = 'DefaultClickActionFunc';
  DEFAULT_GEARING = 'DEFAULT_GEARING';
  kNoParameter = 'kNoParameter';


resourcestring
  SNameAttr = 'name';
  SCommon = 'Common';
  SGrid = 'Grid';
  SItemAlreadyExists = 'Item "%s" already exists';
  SItemNotFound = 'Item "%s" not found';
  SImageNotFound = 'Image "%s" not found';
  SNodeNotFound = 'Node "%s" not found';
  SParamNotFound = 'Parameter "%s" not found';
  SStyleNotFound = 'Style "%s" not found';
  SStyleAlreadyExists = 'Style "%s" already exists';
  SStyleInvalidName = 'Invalid style identifier "%s"';
  SClassNotRegistered = 'Class "%s" not registered';
  SClassNotFound = 'Class not found';
  STopLevelObject = 'Top-level object must be layer';
  SInvalidVType = 'Invalid variant type';
  SInvalidDocFile = 'File "%s" is invalid or corrupted';
  SNewVerDocFile = 'File "%s" is unsupported new version';
  SObjectNotFound = 'Object ID: %d not found';
  SEmptyIdentifier = 'Identifier is empty. Do you want try enter again?';
  SInvalidIdentifier = 'Invalid identifier "%s"';
  SFontDirNotExists = 'Fonts directory "%s" not exists';
  SFontFileNotFound = 'Font file "%s" not found';

type


  TIConstructKind = (ckInstantly, ckOneShort, ckStepByStep);
  TIConstructKinds = set of TIConstructKind;
  TCommandState = (csNone, csUndo, csRedo);
  TOrder = (ordBringToFront, ordBringForward, ordSendBackward, ordSendToBack);
  TDirection = (Vertical, Horizontal);

  TEBlend = (
    SrcOver,
    SrcIn,
    SrcOut,
    SrcAtop,
    DstOver,
    DstIn,
    DstOut,
    DstAtop,
    Add,
    &XOR,
    Default = SrcOver
  );

  EIException = class(Exception);

  TIColor = type cardinal;

  { TIColorHelper }

  TIColorHelper = type Helper for TIColor
  private
    function GetA: byte;
    function GetB: byte;
    function GetG: byte;
    function GetR: byte;
    procedure SetA(AValue: byte);
    procedure SetB(AValue: byte);
    procedure SetG(AValue: byte);
    procedure SetR(AValue: byte);
  public
    function ToAggColor: TAggColor;
    property A: byte read GetA write SetA;
    property B: byte read GetB write SetB;
    property G: byte read GetG write SetG;
    property R: byte read GetR write SetR;
  end;

  { TIFloatPoint }

  PIFloatPoint = ^TIFloatPoint;
  TIFloatPoint = packed record
    X, Y: double;
  public
    constructor Create(const aX, aY: double);
    class function Zero: TIFloatPoint; inline; static;
    function Distance(const P2: TIFloatPoint): double;
    procedure Offset(const Dx, Dy: double); overload;
    procedure Offset(const p: TIFloatPoint); overload;
  end;


  { TIFloatRect }

  PIFloatRect = ^TIFloatRect;
  TIFloatRect = packed record
  private
    function GetHeight: double;
    function GetWidth: double;
    procedure SetHeight(AValue: double);
    procedure SetWidth(AValue: double);
  public
    constructor Create(const ARect: TRect); overload;
    constructor Create(const ARect: TAggRectD); overload;
    constructor Create(const P1, P2: TIFloatPoint); overload;
    constructor Create(const X1, Y1, X2, Y2: double); overload;
    class function Empty: TIFloatRect; inline; static;
    function IsEmpty: boolean;
    function MW: double;
    function MH: double;
    function HW: double;
    function HH: double;
    function IntersectsWith(R: TIFloatRect): Boolean;
    function GetCentredInside(w: double; h: double = 0): TIFloatRect; overload;
    function GetCentredInside(const ARect: TIFloatRect): TIFloatRect; overload;
    function GetFromTop(const a: double): TIFloatRect;
    function GetFromBottom(const a: double): TIFloatRect;
    function GetFromLeft(const a: double): TIFloatRect;
    function GetFromRight(const a: double): TIFloatRect;
    function GetMidHPadded(const a: double): TIFloatRect;
    function GetMidVPadded(const a: double): TIFloatRect;
    function GetReducedFromTop(const a: double): TIFloatRect;
    function GetReducedFromBottom(const a: double): TIFloatRect;
    function GetPadded(const a: double): TIFloatRect; overload;
    function GetPadded(const l, t, r, b: double): TIFloatRect; overload;
    function GetTranslated(const a: double): TIFloatRect; overload;
    function GetTranslated(const dX, dY: double): TIFloatRect; overload;
    function GetScaledAboutCentre(const scale: double): TIFloatRect;
    function GetVPadded(const a: double): TIFloatRect;
    function GetHPadded(const a: double): TIFloatRect;
    function FracRectHorizontal(const f: double; FromRight: boolean = false): TIFloatRect;
    function FracRectVertical(const f: double; FromTop: boolean = false): TIFloatRect;
    function FracRect(Layout: TDirection; const f: double; FromTopOrRight: boolean = false): TIFloatRect;
    function SubRect(Layout: TDirection; NumSlices, SliceIdx: integer): TIFloatRect;
    function SubRectHorizontal(NumSlices, SliceIdx: integer): TIFloatRect;
    function SubRectVertical(NumSlices, SliceIdx: integer): TIFloatRect;
    function ReduceFromTop(const a: double): TIFloatRect; overload;
    function ReduceFromBottom(const a: double): TIFloatRect; overload;
    function ReduceFromLeft(const a: double): TIFloatRect; overload;
    function ReduceFromRight(const a: double): TIFloatRect; overload;
    procedure Alter(const l, t, r, b: double);
    procedure Inflate(const a: double);
    procedure Offset(const dX, dY: double);
    procedure HPad(const a: double);
    procedure VPad(const a: double);
    procedure Pad(const l, t, r, b: double);
    procedure ScaleAboutCentre(const scale: double);
    procedure Union(R: TIFloatRect);
    property Height: double read GetHeight write SetHeight;
    property Width: double read GetWidth write SetWidth;
    case Integer of
      0: (Left, Top, Right, Bottom: double);
      1: (TopLeft, BottomRight: TIFloatPoint);
  end;

  TSVGName = type string;
  TSVGNames = type string;
  TImageName = type string;
  TStyleName = type string;

  TIObjectVertexBuffer = array of array of TIFloatPoint;
  TIObjectOrderBuffer = array of Integer;

  ICoordTransform = interface(IInterface)
  ['{C2638D4E-0829-44C5-983F-DFC20C0A8AED}']
    procedure LogToScreen(lX, lY: double; var sX, sY: double); overload;
    procedure ScreenToLog(sX, sY: double; var lX, lY: double); overload;
    function LogToScreen(Value: double): double; overload;
    function ScreenToLog(Value: double): double; overload;
  end;

  ISerializeNotify = interface(IInterface)
  ['{9ED646CF-1C5E-48B2-A5C5-3191A8758BDB}']
    procedure BeginSave;
    procedure EndSave;
    procedure BeginLoad;
    procedure EndLoad;
  end;

  ICodeGenerate = interface(IInterface)
  ['{8CC99D23-652A-45DB-BADC-483F02D41181}']
    function GetCodeStr: string;
  end;

  TIObjectState = set of (
    osSelected,
    osDragging,
    osSaving,
    osLoading,
    osLocked,
    osConstructing,
    osCreating,
    osDestroying
  );

  TICmdMessage = packed record
    CmdID: Cardinal;
    {$ifdef cpu64}
    UnusedMsg: Cardinal;
    {$endif}
    Param1: WPARAM;
    Param2: LPARAM;
    Result: LRESULT;
  end;

  PIHitTestParams = ^TIHitTestParams;
  TIHitTestParams = packed record
    XPos: Integer;
    YPos: Integer;
    Tolerance: Integer;
    TranslateIntf: ICoordTransform;
  end;

  { TINonRefInterfacedObject }

  TINonRefInterfacedObject = class(TPersistent, IInterface)
  private
    fRefCount: Integer;
    fDestroyed: Boolean;
  protected
    function _AddRef: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}
      IID: TGUID; out Obj): Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure GetErrorParams(ExceptObject: TObject; var ErrorIID: TGUID;
      var HelpFileName: String); virtual;
  public
    procedure FreeInstance; override;
    function SafeCallException(ExceptObject: TObject; ExceptAddr:
      CodePointer): HResult; override;
  end;

  { TISingleton }

  TISingleton = class(TObject)
  protected
    class function FindInstance: TISingleton; virtual;
    class procedure RegisterInstance(Instance: TISingleton); virtual;
    procedure UnRegisterInstance; virtual;
    constructor Create; virtual;
  public
    class function NewInstance: TObject; override;
    procedure BeforeDestruction; override;
    constructor GetInstance;
  end;

  { TCoordList }

  TCoordList = class
  private
    fList: array of Int64;
  public
    destructor Destroy; override;
    procedure Clear;
    function Add(const p: TPoint): integer;
    function Exists(const p: TPoint): boolean;
  end;

const
  OrderCommandNames: array[TOrder] of string =
    ('Bring To Front', 'Bring Forward', 'Send Backward', 'Send To Back');

  function PtInRect(const X, Y: double; R: TIFloatRect): Boolean; overload; inline;
  function PtInRect(const P: TIFloatPoint; R: TIFloatRect): Boolean; overload; inline;
  function LineDistance(X, Y, X1, Y1, X2, Y2: double): double; overload; inline;
  function LineDistance(P, P1, P2: TIFloatPoint): double; overload; inline;
  function PointToGrid(APoint: TIFloatPoint; AGridSize: double): TIFloatPoint; inline;
  function PointInPolygon(P: TIFloatPoint; VX: array of TIFloatPoint): Boolean; inline;
  function BooleanToStr(const Value: boolean): string; inline;
  function DoubleToStr(const Value: double): string; inline;
  function IsValidIdentifier(const AName: string): boolean; inline;
  function NormalizeOptStr(const s: string): string;
  function FormatOptStr(const s: string): string;
  function FormatOptCode(sl: TStrings; const quoted: boolean = true): string;
  procedure NormalizeCoord(var X1, X2: double); inline;


implementation

uses
  Math, Forms, StrUtils;

var
  SingletonList: TObjectList;

function BooleanToStr(const Value: boolean): string;
const
  b: array[boolean] of string = ('false', 'true');
begin
  result := b[Value];
end;

procedure NormalizeCoord(var X1, X2: double);
var
  Temp: double;
begin
  if X1 > X2 then
  begin
    Temp := X2;
    X2 := X1;
    X1 := Temp;
  end;
end;


function LineDistance(X, Y, X1, Y1, X2, Y2: double): double;

  procedure Xchg(var A, B: double);
  var
    C: double;
  begin
    C := A;
    A := B;
    B := C;
  end;

var
  A1, B1, C1, A2, B2, C2,
  DD, D, Px, Py: double;
begin
  if (X2 = X1) and (Y2 = Y1) then
    Result := Sqrt(Sqr(X - X1) + Sqr(Y - Y1))
  else begin
    A1 := Y2 - Y1;
    B1 := X1 - X2;
    C1 := Y1*(X2 - X1) - X1*(Y2 - Y1);
    D := Abs(A1*X + B1*Y + C1)/Sqrt(Sqr(A1) + Sqr(B1));
    if X1 > X2 then Xchg(X1, X2);
    if Y2 > Y1 then Xchg(Y1, Y2);
    A2 := -B1;
    B2 := A1;
    C2 := B1*X - A1*Y;
    DD := A1*B2 - B1*A2;
    Px := (-C1*B2 + B1*C2)/DD;
    Py := (-A1*C2 + C1*A2)/DD;
    if (Px >= X1) and (Px <= X2) or (Py >= Y2) and (Py <= Y1) then
      Result := D
    else
      Result := Min(Sqrt(Sqr(X - X1) + Sqr(Y - Y1)),
                    Sqrt(Sqr(X - X2) + Sqr(Y - Y2)));
  end;
end;

function LineDistance(P, P1, P2: TIFloatPoint): double;
begin
  Result := LineDistance(P.X, P.Y, P1.X, P1.Y, P2.X, P2.Y);
end;

function PtInRect(const X, Y: double; R: TIFloatRect): Boolean;
begin
  Result := PtInRect(TIFloatPoint.Create(X, Y), R);
end;

function PtInRect(const P: TIFloatPoint; R: TIFloatRect): Boolean;
begin
  NormalizeCoord(R.Left, R.Right);
  NormalizeCoord(R.Top, R.Bottom);
  Result := (P.X <= R.Right) and (P.X >= R.Left) and (P.Y <= R.Bottom) and
    (P.Y >= R.Top);
end;

function PointToGrid(APoint: TIFloatPoint; AGridSize: double): TIFloatPoint;
begin
  Result := APoint;
  if AGridSize > 0 then begin
    Result.X := Round(APoint.X / AGridSize) * AGridSize;
    Result.Y := Round(APoint.Y / AGridSize) * AGridSize;
  end;
end;

function PointInPolygon(P: TIFloatPoint; VX: array of TIFloatPoint): Boolean;
var
  X: array of TIFloatPoint;
  i, N: Integer;
  b1, b2: Boolean;
begin
  Result := False;
  N := Length(VX);
  SetLength(X, N + 1);
  for i := 1 to N do
    X[i] := VX[i - 1];
  X[0] := X[N];
  i := 0;
  repeat
    b1 := P.Y > X[i].Y;
    b2 := P.Y <= X[i + 1].Y;
    if not (b1 xor b2) then
      if P.X - X[i].X < (P.Y - X[i].Y) * (X[i + 1].X - X[i].X) / (X[i + 1].Y - X[i].Y) then
        Result := not Result;
    Inc(i);
  until not (i <= N - 1);
end;

function DoubleToStr(const Value: double): string;
begin
  Result := FormatFloat('0.0#####', Value);
end;

function IsValidIdentifier(const AName: string): boolean;
const
  AllowedFirstChar = ['A'..'Z', 'a'..'z', '_' ];
  AllowedSubsequentChars = AllowedFirstChar + ['0'..'9'];
var
  I: integer;
begin
  Result := False;
  if Length(AName) < 1 then
    Exit;
  if not CharInSet(AName[1], AllowedFirstChar) then
    Exit;
  for I := 2 to Length(AName) do
    if not CharInSet(AName[I], AllowedSubsequentChars) then
      Exit;
  Result := True;
end;

function NormalizeOptStr(const s: string): string;
begin
  result := DelSpace(s);
end;

function FormatOptStr(const s: string): string;
begin
  result := StringReplace(s, ',', ', ', [rfReplaceAll]);
end;

function FormatOptCode(sl: TStrings; const quoted: boolean): string;
const
  q: array[boolean] of string = ('', '"');
var
  i: integer;
begin
  result := '';
  for i := 0 to sl.Count - 1 do
    if i = sl.Count - 1 then
      result := Format('%s%s%s%s', [result, q[quoted], sl[i], q[quoted]])
    else
      result := Format('%s%s%s%s, ', [result, q[quoted], sl[i], q[quoted]]);
end;

{ TIColorHelper }

function TIColorHelper.GetA: byte;
begin
  result := (self shr 24) and $FF;
end;

function TIColorHelper.GetB: byte;
begin
  result := self and $000000FF;
end;

function TIColorHelper.GetG: byte;
begin
  Result := (self and $0000FF00) shr 8;
end;

function TIColorHelper.GetR: byte;
begin
  Result := (self and $00FF0000) shr 16;
end;

procedure TIColorHelper.SetA(AValue: byte);
begin
  self := (self and $00FFFFFF) or (TIColor(AValue) shl 24);
end;

procedure TIColorHelper.SetB(AValue: byte);
begin
  self := (self and $FFFFFF00) or AValue;
end;

procedure TIColorHelper.SetG(AValue: byte);
begin
  self := (self and $FFFF00FF) or (TIColor(AValue) shl 8);
end;

procedure TIColorHelper.SetR(AValue: byte);
begin
  self := (self and $FF00FFFF) or (TIColor(AValue) shl 16);
end;

function TIColorHelper.ToAggColor: TAggColor;
begin
  result.a := A;
  result.b := B;
  result.g := G;
  result.r := R;
end;

{ TIFloatPoint }

constructor TIFloatPoint.Create(const aX, aY: double);
begin
  X := aX;
  Y := aY;
end;

class function TIFloatPoint.Zero: TIFloatPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function TIFloatPoint.Distance(const P2: TIFloatPoint): double;
begin
  Result := Sqrt(Sqr(P2.X - X) + Sqr(P2.Y - Y));
end;

procedure TIFloatPoint.Offset(const Dx, Dy: double);
begin
  X += Dx;
  Y += Dy;
end;

procedure TIFloatPoint.Offset(const p: TIFloatPoint);
begin
  X += p.X;
  Y += p.Y;
end;


{ TIFloatRect }

constructor TIFloatRect.Create(const ARect: TRect);
begin
  Left := ARect.Left;
  Top := ARect.Top;
  Right := ARect.Right;
  Bottom := ARect.Bottom;
end;

constructor TIFloatRect.Create(const ARect: TAggRectD);
begin
  Left := ARect.x1;
  Top := ARect.y1;
  Right := ARect.x2;
  Bottom := ARect.y2;
end;

constructor TIFloatRect.Create(const P1, P2: TIFloatPoint);
begin
  TopLeft := P1;
  BottomRight := P2;
end;

constructor TIFloatRect.Create(const X1, Y1, X2, Y2: double);
begin
  Left := X1;
  Top := Y1;
  Right := X2;
  Bottom := Y2;
end;

class function TIFloatRect.Empty: TIFloatRect;
begin
  Result := TIFloatRect.Create(0, 0, 0, 0);
end;

function TIFloatRect.IsEmpty: boolean;
begin
  result := (Height = 0) or (Width = 0);
end;

function TIFloatRect.GetHeight: double;
begin
  result := Bottom - Top;
end;

function TIFloatRect.GetWidth: double;
begin
  result := Right - Left;
end;

procedure TIFloatRect.SetHeight(AValue: double);
begin
  Bottom := Top + AValue;
end;

procedure TIFloatRect.SetWidth(AValue: double);
begin
  Right := Left + AValue;
end;

function TIFloatRect.MW: double;
begin
  result := 0.5 * (Left + Right);
end;

function TIFloatRect.MH: double;
begin
  result := 0.5 * (Top + Bottom);
end;

function TIFloatRect.HW: double;
begin
  result := 0.5 * (Right - Left);
end;

function TIFloatRect.HH: double;
begin
  result := 0.5 * (Bottom - Top);
end;

function TIFloatRect.IntersectsWith(R: TIFloatRect): Boolean;
var
  R1: TIFloatRect;
begin
  R1 := self;
  NormalizeCoord(R1.Left, R1.Right);
  NormalizeCoord(R1.Top, R1.Bottom);
  NormalizeCoord(R.Left, R.Right);
  NormalizeCoord(R.Top, R.Bottom);
  Result := (R1.Left <= R.Right) and (R.Left <= R1.Right) and (R1.Top <= R.Bottom) and
    (R.Top <= R1.Bottom);
end;

function TIFloatRect.GetCentredInside(w: double; h: double): TIFloatRect;
begin
  w := Max(w, 1.0);
  if h <= 0 then
    h := w;
  result.Left := MW - w / 2.0;
  result.Top := MH - h / 2.0;
  result.Right := result.Left + w;
  result.Bottom := result.Top + h;
end;

function TIFloatRect.GetCentredInside(const ARect: TIFloatRect): TIFloatRect;
begin
  Result.Left := MW - ARect.Width / 2;
  Result.Top := MH - ARect.Height / 2;
  Result.Right := Result.Left + ARect.Width;
  Result.Bottom := Result.Top + ARect.Height;
end;

function TIFloatRect.GetFromTop(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, Top, Right, Top + a);
end;

function TIFloatRect.GetFromBottom(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, Bottom - a, Right, Bottom);
end;

function TIFloatRect.GetFromLeft(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, Top, Left + a, Bottom);
end;

function TIFloatRect.GetFromRight(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Right - a, Top, Right, Bottom);
end;

function TIFloatRect.GetMidHPadded(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(MW - a, Top, MW + a, Bottom);
end;

function TIFloatRect.GetMidVPadded(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, MH - a, Right, MH + a);
end;

function TIFloatRect.GetReducedFromTop(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, Top + a, Right, Bottom);
end;

function TIFloatRect.GetReducedFromBottom(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, Top, Right, Bottom - a);
end;

function TIFloatRect.GetPadded(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left - a, Top - a, Right + a, Bottom + a);
end;

function TIFloatRect.GetPadded(const l, t, r, b: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left - l, Top - t, Right + r, Bottom + b);
end;

function TIFloatRect.GetTranslated(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left + a, Top + a, Right + a, Bottom + a);
end;

function TIFloatRect.GetTranslated(const dX, dY: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left + dX, Top + dY, Right + dX, Bottom + dY);
end;

function TIFloatRect.GetScaledAboutCentre(const scale: double): TIFloatRect;
begin
  result := self;
  result.ScaleAboutCentre(scale);
end;

function TIFloatRect.GetVPadded(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left, Top - a, Right, Bottom + a);
end;

function TIFloatRect.GetHPadded(const a: double): TIFloatRect;
begin
  Result := TIFloatRect.Create(Left - a, Top, Right + a, Bottom);
end;

function TIFloatRect.FracRectHorizontal(const f: double; FromRight: boolean
  ): TIFloatRect;
var
  w: double;
begin
  w := Width * f;
  if FromRight then
    result := TIFloatRect.Create(Right - w, Top, Right, Bottom)
  else
    result := TIFloatRect.Create(Left, Top, Left + w, Bottom)
end;

function TIFloatRect.FracRectVertical(const f: double; FromTop: boolean
  ): TIFloatRect;
var
  h: double;
begin
  h := Height * f;
  if FromTop then
    result := TIFloatRect.Create(Left, Top, Right, Top + h)
  else
    result := TIFloatRect.Create(Left, Bottom - h, Right, Bottom)
end;

function TIFloatRect.FracRect(Layout: TDirection; const f: double;
  FromTopOrRight: boolean): TIFloatRect;
begin
  if Layout = Vertical then
    result := FracRectVertical(f, FromTopOrRight)
  else
    result := FracRectHorizontal(f, FromTopOrRight)
end;

function TIFloatRect.SubRectHorizontal(NumSlices, SliceIdx: integer
  ): TIFloatRect;
var
  srw, l: double;
begin
  srw := Width / NumSlices;
  l := srw * sliceIdx;
  result := TIFloatRect.Create(Left + l, Top, Left + l + srw, Bottom);
end;

function TIFloatRect.SubRectVertical(NumSlices, SliceIdx: integer): TIFloatRect;
var
  srh, t: double;
begin
  srh := Height / NumSlices;
  t := srh * SliceIdx;
  result := TIFloatRect.Create(Left, Top + t, Right, Top + t + srh);
end;

function TIFloatRect.ReduceFromTop(const a: double): TIFloatRect;
begin
  result := GetFromTop(a);
  Top += a;
end;

function TIFloatRect.ReduceFromBottom(const a: double): TIFloatRect;
begin
  result := GetFromBottom(a);
  Bottom -= a;
end;

function TIFloatRect.ReduceFromLeft(const a: double): TIFloatRect;
begin
  result := GetFromLeft(a);
  Left += a;
end;

function TIFloatRect.ReduceFromRight(const a: double): TIFloatRect;
begin
  result := GetFromRight(a);
  Right -= a;
end;

function TIFloatRect.SubRect(Layout: TDirection; NumSlices, SliceIdx: integer
  ): TIFloatRect;
begin
  if Layout =  Vertical then
    result := SubRectVertical(NumSlices, SliceIdx)
  else
    result := SubRectHorizontal(NumSlices, SliceIdx);
end;

procedure TIFloatRect.Alter(const l, t, r, b: double);
begin
  Left += l;
  Top += t;
  Right += r;
  Bottom += b;
end;

procedure TIFloatRect.Inflate(const a: double);
begin
  Left -= a;
  Right += a;
  Top -= a;
  Bottom += a;
end;

procedure TIFloatRect.Offset(const dX, dY: double);
begin
  Left += dX;
  Top += dY;
  Right += dX;
  Bottom += dY;
end;

procedure TIFloatRect.HPad(const a: double);
begin
  Left -= a;
  Right += a;
end;

procedure TIFloatRect.VPad(const a: double);
begin
  Top -= a;
  Bottom += a;
end;

procedure TIFloatRect.Pad(const l, t, r, b: double);
begin
  Left -= l;
  Right += r;
  Top -= t;
  Bottom += b;
end;

procedure TIFloatRect.ScaleAboutCentre(const scale: double);
var
  x, y, _hw, _hh: double;
begin
  x := MW;
  y := MH;
  _hw := HW;
  _hh := HH;
  Left := x - (_hw * scale);
  Top := y - (_hh * scale);
  Right := x + (_hw * scale);
  Bottom := y + (_hh * scale);
end;

procedure TIFloatRect.Union(R: TIFloatRect);
begin
  NormalizeCoord(Left, Right);
  NormalizeCoord(Top, Bottom);
  NormalizeCoord(R.Left, R.Right);
  NormalizeCoord(R.Top, R.Bottom);
  Left := Min(Left, R.Left);
  Top := Min(Top, R.Top);
  Right := Max(Right, R.Right);
  Bottom := Max(Bottom, R.Bottom);
end;

{ TINonRefInterfacedObject }

function TINonRefInterfacedObject._AddRef: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterlockedIncrement(fRefCount);
end;

function TINonRefInterfacedObject._Release: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterlockedDecrement(fRefCount);
  if (fRefCount = 0) and fDestroyed then
    FreeInstance;
end;

function TINonRefInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}
  IID: TGUID; out Obj): Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TINonRefInterfacedObject.GetErrorParams(ExceptObject: TObject;
  var ErrorIID: TGUID; var HelpFileName: String);
const
  EmptyGUID: TGUID='{00000000-0000-0000-0000-000000000000}';
begin
  ErrorIID := EmptyGUID;
  HelpFileName := '';
end;

procedure TINonRefInterfacedObject.FreeInstance;
begin
  fDestroyed := true;
  if fRefCount = 0 then
    inherited FreeInstance;
end;

function TINonRefInterfacedObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: CodePointer): HResult;
var
  ErrorIID: TGUID;
  HelpFileName: string;
begin
  GetErrorParams(ExceptObject, ErrorIID, HelpFileName);
  Result := inherited SafeCallException(ExceptObject, ExceptAddr);
end;


{ TISingleton }

constructor TISingleton.Create;
begin
  inherited Create;
end;

class function TISingleton.FindInstance: TISingleton;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SingletonList.Count - 1 do
    if SingletonList[i].ClassType = Self then
    begin
      Result := TISingleton(SingletonList[i]);
      Break;
    end;
end;

class procedure TISingleton.RegisterInstance(Instance: TISingleton);
begin
  SingletonList.Add(Instance);
end;

procedure TISingleton.UnRegisterInstance;
begin
  SingletonList.Extract(Self);
end;

class function TISingleton.NewInstance: TObject;
begin
  Result := FindInstance;
  if Result = nil then
  begin
    Result := inherited NewInstance;
    TISingleton(Result).Create;
    RegisterInstance(TISingleton(Result));
  end;
end;

procedure TISingleton.BeforeDestruction;
begin
  UnregisterInstance;
  inherited BeforeDestruction;
end;

constructor TISingleton.GetInstance;
begin
  inherited Create;
end;


{ TCoordList }


destructor TCoordList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCoordList.Clear;
begin
  SetLength(fList, 0);
end;

function TCoordList.Add(const p: TPoint): integer;
begin
  Result := Length(fList);
  SetLength(fList, Result + 1);
  fList[Result] := p.X + (Int64(p.Y) shl 32);
end;

function TCoordList.Exists(const p: TPoint): boolean;
var
  v: Int64;
  i: Integer;
begin
  v := p.X + (Int64(p.Y) shl 32);
  i := High(fList);
  while (i >= 0) and (fList[i] <> v) do
    Dec(i);
  result := i >= 0;
end;

initialization
  SingletonList := TObjectList.Create(true);
  IniPath := ChangeFileExt(Application.ExeName, '.ini');

finalization
  SingletonList.Free;

end.

