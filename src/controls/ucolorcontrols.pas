{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uColorControls;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, LMessages, Graphics, Controls, Forms,
  Agg_LCL, agg_fpimage;

const
  Hue000 = 0;
  Hue360 = 65536;
  Hue060 = Hue360 div 6;
  Hue120 = Hue360 div 3;
  Hue180 = Hue360 div 2;
  Hue240 = Hue360 * 2 div 3;
  Hue300 = Hue360 * 5 div 6;
  HueMask = 65535;

  Sat100 = 65536;
  SatMask = 65535;

  Bri100 = 65536;
  BriMask = 65535;

type

  TColorComponent = (ccHue, ccSaturation, ccBrightness, ccRed, ccGreen, ccBlue);
  THSBChangeEvent = procedure(Sender: TObject; Hue, Sat, Bri: integer) of object;
  TRGBChangeEvent = procedure(Sender: TObject; Red, Green, Blue: integer) of object;

  { TCustomColorControl }

  TCustomColorControl = class(TGraphicControl)
  private
    fBorderColor: TColor;
    fOnMouseEnter: TNotifyEvent;
    fOnMouseLeave: TNotifyEvent;
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;
    procedure SetBorderColor(AValue: TColor);
  protected
    fBitmap: TBitmap;
    procedure Resize; override;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure ChangeCursor(const C: TCursor);
    procedure Paint; override;
    procedure PaintPicker; virtual; abstract;
    procedure DrawBounds; virtual;
    procedure DrawTarget(const x, y: integer); virtual; abstract;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BorderColor: TColor read fBorderColor write SetBorderColor;
  end;

  { TColorsControl }

  TColorsControl = class(TCustomColorControl)
  private
    fHue: integer;
    fSat: integer;
    fBri: integer;
    fRed: integer;
    fGreen: integer;
    fBlue: integer;
    fTargeting: boolean;
    fFixComponent: TColorComponent;
    fOnHSBChange: THSBChangeEvent;
    fOnRGBChange: TRGBChangeEvent;
    procedure SetFixComponent(Value: TColorComponent);
  protected
    fGradBmp: TBitmap;
    procedure Resize; override;
    procedure RefreshTarget; virtual; abstract;
    procedure RefreshValues; virtual; abstract;
    procedure CreateGradient; virtual; abstract;
    procedure DrawTarget(const x, y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetHSB(const Hue, Sat, Bri: integer); virtual; abstract;
    procedure SetRGB(const Red, Green, Blue: integer); virtual; abstract;
    property OnHSBChange: THSBChangeEvent read fOnHSBChange write fOnHSBChange;
    property OnRGBChange: TRGBChangeEvent read fOnRGBChange write fOnRGBChange;
    property FixComponent: TColorComponent read fFixComponent write SetFixComponent;
  end;

  { TColorChart }

  TColorChart = class(TColorsControl)
  private
    fTarget: TPoint;
    procedure NotifyChange;
    procedure SetTarget(const X, Y: integer; const ARedraw: boolean = False;
      const ARefresh: boolean = False);
  protected
    procedure PaintPicker; override;
    procedure CreateGradient; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure RefreshTarget; override;
    procedure RefreshValues; override;
  public
    procedure SetHSB(const Hue, Sat, Bri: integer); override;
    procedure SetRGB(const Red, Green, Blue: integer); override;
  end;

  { TColorBar }

  TColorBar = class(TColorsControl)
  private
    fTarget: integer;
    procedure NotifyChange;
    procedure SetTarget(const Z: integer; const ARedraw: boolean = False;
      const ARefresh: boolean = False);
  protected
    procedure CreateGradient; override;
    procedure PaintPicker; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;
    procedure RefreshTarget; override;
    procedure RefreshValues; override;
  public
    procedure SetHSB(const Hue, Sat, Bri: integer); override;
    procedure SetRGB(const Red, Green, Blue: integer); override;
  end;

  { TColorPreview }

  TColorPreview = class(TCustomColorControl)
  private
    fColor: TAggColor;
    fRecentColor: TAggColor;
    fHover: boolean;
    fDown: boolean;
    fColorBmp: TBitmap;
    fOnRevertColor: TNotifyEvent;
    procedure SetColor(Value: TAggColor);
    procedure SetRecentColor(Value: TAggColor);
  protected
    procedure PaintPicker; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    property Color: TAggColor read fColor write SetColor;
    property RecentColor: TAggColor read fRecentColor write SetRecentColor;
    property OnRevertColor: TNotifyEvent read fOnRevertColor write fOnRevertColor;
  end;

  procedure IColorToHSB(const Color: TAggColor; out Hue, Sat, Bri: integer);
  procedure IColorToRGB(const Color: TAggColor; out R, G, B: byte); overload;
  procedure IColorToRGB(const Color: TAggColor; out R, G, B: integer); overload;
  function HSBToIColor(const Hue, Sat, Bri: integer): TAggColor;
  function RGBToIColor(const R, G, B: byte): TAggColor;


implementation

uses
  Types, LCLIntf, uGraphics, Math;

type

  TColorSpaceFunction = function(const X, Y, Z: integer): TAggColor;

  TBitmapPixel = packed record
    B, G, R, A: UInt8;
  end;
  PBitmapLine = ^TBitmapLine;
  TBitmapLine = array[UInt16] of TBitmapPixel;

var
  ColorSpaceFunctions: array[TColorComponent] of TColorSpaceFunction;

const
  crCircle = TCursor(105);

function GetChannelIndex(const c: TAggColor; index: byte): byte;
begin
  case index of
    0: Result := c.B;
    1: Result := c.G;
    2: Result := c.R;
    3: Result := c.A;
  end;
end;

procedure SetChannelIndex(var c: TAggColor; index: byte; AValue: byte);
begin
  case index of
    0: c.B := AValue;
    1: c.G := AValue;
    2: c.R := AValue;
    3: c.A := AValue;
  end;
end;

procedure IColorToRGB(const Color: TAggColor; out R, G, B: byte);
begin
  R := Color.R;
  G := Color.G;
  B := Color.B;
end;

procedure IColorToRGB(const Color: TAggColor; out R, G, B: integer);
begin
  R := Color.R;
  G := Color.G;
  B := Color.B;
end;

procedure IColorToHSB(const Color: TAggColor; out Hue, Sat, Bri: integer);
const
  PureHues: array[0..2] of word = (Hue240, Hue120, Hue000);
  NextChannels: array[0..2] of integer = (2, 0, 1);
var
  StrongestChannel, WeakestChannel, MediumChannel: integer;
  WeakestValue, MediumValue: word;
  Diff: longint;
begin
  if (Color.B = Color.G) and (Color.B = Color.R) then
  begin // gray
    Hue := 0;
    Sat := 0;
    Bri := Color.B * 257;
  end

  else
  begin // not gray
    if (Color.B >= Color.G) and (Color.B >= Color.R) then
    begin
      StrongestChannel := 0; // blue dominates
      if (Color.G <= Color.R) then
        WeakestChannel := 1
      else
        WeakestChannel := 2;
    end
    else if (Color.G >= Color.R) then
    begin
      StrongestChannel := 1; // green dominates
      if (Color.B <= Color.R) then
        WeakestChannel := 0
      else
        WeakestChannel := 2;
    end
    else
    begin
      StrongestChannel := 2; // red dominates
      if (Color.B <= Color.G) then
        WeakestChannel := 0
      else
        WeakestChannel := 1;
    end;
    Bri := GetChannelIndex(Color, StrongestChannel) * $10000 div 255;

    if Bri = 0 then
    begin // black
      Sat := 0;
      Hue := 0;
    end

    else
    begin // not black: "normal" color
      MediumChannel := 3 - WeakestChannel - StrongestChannel;
      WeakestValue := GetChannelIndex(Color, WeakestChannel) shl 8;
      MediumValue := GetChannelIndex(Color, MediumChannel) shl 8;
      Sat := $10000 - ((WeakestValue * 256) div GetChannelIndex(Color,
        StrongestChannel));
      Hue := PureHues[StrongestChannel];
      Diff := (((((MediumValue - WeakestValue) * 256) div
        GetChannelIndex(Color, StrongestChannel)) * Hue060)) div Sat;
      if MediumChannel = NextChannels[StrongestChannel] then
        Hue := (Hue + Diff) and HueMask
      else
        Hue := (Hue - Diff) and HueMask;
    end;
  end;
end;


function GetSaturatedColor(const Hue: integer): TAggColor;
const
  AddTab: array[0..2] of word = (Hue240, Hue000, Hue120);
  HueMul = 255 * 6;
var
  I, S, F: integer;
begin
  for I := 0 to 2 do
  begin
    S := (Hue + AddTab[I]) and HueMask;
    if S < Hue060 then
      F := (S * HueMul) shr 16
    else if S <= Hue180 then
      F := 255
    else if S < Hue240 then
      F := ((Hue240 - S) * HueMul) shr 16
    else
      F := 0;
    SetChannelIndex(Result, I, F);
  end;
  Result.A := 255;
end;

function HSBToIColor(const Hue, Sat, Bri: integer): TAggColor;
var
  b, s: double;
begin
  Result := GetSaturatedColor(Hue);
  b := Bri / 1048576;
  s := Sat / 4096;
  Result.R := Round((4080 - ((255 - Result.R) * s)) * b);
  Result.G := Round((4080 - ((255 - Result.G) * s)) * b);
  Result.B := Round((4080 - ((255 - Result.B) * s)) * b);
  Result.A := 255;
end;

function RGBToIColor(const R, G, B: byte): TAggColor;
begin
  Result.B := B;
  Result.G := G;
  Result.R := R;
  Result.A := 255;
end;

function GetColorHSB(const X, Y, Z: integer): TAggColor;
begin
  Result := HSBToIColor(X, Y, Z);
end;

function GetColorSBH(const X, Y, Z: integer): TAggColor;
begin
  Result := HSBToIColor(Z, X, Y);
end;

function GetColorHBS(const X, Y, Z: integer): TAggColor;
begin
  Result := HSBToIColor(X, Z, Y);
end;

function GetColorGBR(const X, Y, Z: integer): TAggColor;
begin
  Result := RGBToIColor(Z shr 8, X shr 8, Y shr 8);
end;

function GetColorRBG(const X, Y, Z: integer): TAggColor;
begin
  Result := RGBToIColor(X shr 8, Z shr 8, Y shr 8);
end;

function GetColorRGB(const X, Y, Z: integer): TAggColor;
begin
  Result := RGBToIColor(X shr 8, Y shr 8, Z shr 8);
end;

function GetContrastColor(const Color: TColor): TColor;
var
  c: longint;
  r, g, b, gray: byte;
begin
  c := ColorToRGB(Color);
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
  gray := (77 * r + 151 * g + 28 * b) shr 8;
  if gray < 128 then
    Result := clWhite
  else
    Result := clBlack;
end;

procedure InitModule;
begin
  Screen.Cursors[crCircle] := LoadCursor(HInstance, 'CIRCLE');
  ColorSpaceFunctions[ccHue] := @GetColorSBH;
  ColorSpaceFunctions[ccSaturation] := @GetColorHBS;
  ColorSpaceFunctions[ccBrightness] := @GetColorHSB;
  ColorSpaceFunctions[ccRed] := @GetColorGBR;
  ColorSpaceFunctions[ccGreen] := @GetColorRBG;
  ColorSpaceFunctions[ccBlue] := @GetColorRGB;
end;


{ TCustomColorControl }

constructor TCustomColorControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  fBorderColor := cl3DShadow;
  fBitmap := TBitmap.Create;
  fBitmap.PixelFormat := pf24bit;
end;

destructor TCustomColorControl.Destroy;
begin
  FreeAndNil(fBitmap);
  inherited Destroy;
end;

procedure TCustomColorControl.Paint;
begin
  PaintPicker;
end;

procedure TCustomColorControl.DrawBounds;
var
  r: TRect;
begin
  r := Rect(0, 0, fBitmap.Width, fBitmap.Height);
  fBitmap.Canvas.Pen.Color := fBorderColor;
  fBitmap.Canvas.PolyLine([Point(r.Left + 1, r.Top), Point(r.Right - 2, r.Top),
    Point(r.Right - 1, r.Top + 1), Point(r.Right - 1, r.Bottom - 2),
    Point(r.Right - 2, r.Bottom - 1), Point(r.Left + 1, r.Bottom - 1),
    Point(r.Left, r.Bottom - 2), Point(r.Left, r.Top)]);
end;

procedure TCustomColorControl.CMMouseEnter(var Msg: TLMessage);
begin
  DoMouseEnter;
  inherited;
end;

procedure TCustomColorControl.SetBorderColor(AValue: TColor);
begin
  if fBorderColor = AValue then
    Exit;
  fBorderColor := AValue;
  Invalidate;
end;

procedure TCustomColorControl.Resize;
var
  c: TColor;
begin
  inherited Resize;
  c := GetDefaultColor(dctBrush);
  fBitmap.SetSize(Width, Height);
  if Assigned(Parent) and (Parent.Color <> clDefault) then
    c := Parent.Color;
  fBitmap.Canvas.Brush.Color := c;
  fBitmap.Canvas.FillRect(fBitmap.Canvas.ClipRect);
end;

procedure TCustomColorControl.CMMouseLeave(var Msg: TLMessage);
begin
  DoMouseLeave;
  inherited;
end;

procedure TCustomColorControl.DoMouseEnter;
begin
  if Assigned(fOnMouseEnter) then
    fOnMouseEnter(Self);
end;

procedure TCustomColorControl.DoMouseLeave;
begin
  if Assigned(fOnMouseLeave) then
    fOnMouseLeave(Self);
end;

procedure TCustomColorControl.ChangeCursor(const C: TCursor);
begin
  if Cursor <> C then
  begin
    Cursor := C;
    Parent.Perform(LM_SETCURSOR, Parent.Handle, HTCLIENT);
  end;
end;

{ TColorsControl }

constructor TColorsControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fGradBmp := TBitmap.Create;
  fGradBmp.PixelFormat := pf32bit;
end;

destructor TColorsControl.Destroy;
begin
  FreeAndNil(fGradBmp);
  inherited Destroy;
end;

procedure TColorsControl.SetFixComponent(Value: TColorComponent);
begin
  if fFixComponent = Value then
    exit;
  fFixComponent := Value;
  RefreshTarget;
  CreateGradient;
  Invalidate;
end;

procedure TColorsControl.Resize;
begin
  inherited Resize;
  fGradBmp.SetSize(Width - 2, Height - 2);
  CreateGradient;
end;

procedure TColorsControl.DrawTarget(const x, y: integer);
begin
  fBitmap.Canvas.Brush.Style := bsClear;
  fBitmap.Canvas.Pen.Mode := pmNot;
  fBitmap.Canvas.MoveTo(x - 5, y - 2);
  fBitmap.Canvas.LineTo(x - 2, y - 5);
  fBitmap.Canvas.LineTo(x + 2, y - 5);
  fBitmap.Canvas.LineTo(x + 5, y - 2);
  fBitmap.Canvas.LineTo(x + 5, y + 2);
  fBitmap.Canvas.LineTo(x + 2, y + 5);
  fBitmap.Canvas.LineTo(x - 2, y + 5);
  fBitmap.Canvas.LineTo(x - 5, y + 2);
  fBitmap.Canvas.LineTo(x - 5, y - 2);
  fBitmap.Canvas.Pen.Mode := pmCopy;
  fBitmap.Canvas.Brush.Style := bsSolid;
end;

{ TColorChart }

procedure TColorChart.NotifyChange;
begin
  case FFixComponent of
    ccHue, ccSaturation, ccBrightness:
      if Assigned(fOnHSBChange) then
        fOnHSBChange(Self, fHue, fSat, fBri);
    ccRed, ccGreen, ccBlue:
      if Assigned(fOnRGBChange) then
        fOnRGBChange(Self, fRed, fGreen, fBlue);
  end;
end;

procedure TColorChart.SetTarget(const X, Y: integer; const ARedraw: boolean;
  const ARefresh: boolean);
begin
  if (fTarget.X <> X) or (fTarget.Y <> Y) then
  begin
    fTarget.X := EnsureRange(X, 0, 255);
    fTarget.Y := EnsureRange(Y, 0, 255);
    if ARedraw then
      Invalidate;
    if ARefresh then
    begin
      RefreshValues;
      NotifyChange;
    end;
  end;
end;

procedure TColorChart.PaintPicker;
var
  x, y: integer;
begin
  fBitmap.Canvas.Draw(1, 1, fGradBmp);
  DrawBounds;
  x := 1 + fTarget.X;
  y := 256 - fTarget.Y;
  DrawTarget(x, y);
  Canvas.Draw(0, 0, fBitmap);
end;

procedure TColorChart.CreateGradient;
var
  i, j, z: integer;
  c: TAggColor;
  p: PBitmapLine;
  f: TColorSpaceFunction;
begin
  case fFixComponent of
    ccHue: z := fHue;
    ccSaturation: z := fSat;
    ccBrightness: z := fBri;
    ccRed: z := fRed shl 8;
    ccGreen: z := fGreen shl 8;
    ccBlue: z := fBlue shl 8;
    else
      z := 0;
  end;
  fGradBmp.BeginUpdate;
  try
    f := ColorSpaceFunctions[fFixComponent];
    for i := 0 to 255 do
    begin
      p := PBitmapLine(fGradBmp.RawImage.GetLineStart(i));
      for j := 0 to 255 do
      begin
        c := f((j + 1) shl 8 - 128, (256 - i) shl 8 - 128, z);
        p^[j].R := c.r;
        p^[j].G := c.g;
        p^[j].B := c.b;
        p^[j].A := c.a;
      end;
    end;
  finally
    fGradBmp.EndUpdate;
  end;
end;

procedure TColorChart.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if (Button = mbLeft) and (X >= 1) and (Y >= 1) and (X < 257) and (Y < 257) then
  begin
    fTargeting := True;
    SetTarget(X - 1, 256 - Y, True, True);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TColorChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  fTargeting := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TColorChart.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if fTargeting or ((X >= 1) and (Y >= 1) and (X < 257) and (Y < 257)) then
    ChangeCursor(crCircle)
  else
    ChangeCursor(crDefault);
  if fTargeting then
  begin
    SetTarget(X - 1, 256 - Y, True, True);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TColorChart.RefreshTarget;
begin
  case FFixComponent of
    ccHue: SetTarget(fSat shr 8, fBri shr 8);
    ccSaturation: SetTarget(fHue shr 8, fBri shr 8);
    ccBrightness: SetTarget(fHue shr 8, fSat shr 8);
    ccRed: SetTarget(fGreen, fBlue);
    ccGreen: SetTarget(fRed, fBlue);
    ccBlue: SetTarget(fRed, fGreen);
  end;
end;

procedure TColorChart.RefreshValues;
begin
  case FFixComponent of
    ccHue:
    begin
      fSat := MulDiv(fTarget.X, Sat100, 255);
      fBri := MulDiv(fTarget.Y, Bri100, 255);
    end;
    ccSaturation:
    begin
      fHue := MulDiv(fTarget.X, Hue360, 255) and HueMask;
      fBri := MulDiv(fTarget.Y, Bri100, 255);
    end;
    ccBrightness:
    begin
      fHue := MulDiv(fTarget.X, Hue360, 255) and HueMask;
      fSat := MulDiv(fTarget.Y, Sat100, 255);
    end;
    ccRed:
    begin
      fGreen := fTarget.X;
      fBlue := fTarget.Y;
    end;
    ccGreen:
    begin
      fRed := fTarget.X;
      fBlue := fTarget.Y;
    end;
    ccBlue:
    begin
      fRed := fTarget.X;
      fGreen := fTarget.Y;
    end;
  end;
  case FFixComponent of
    ccHue, ccSaturation, ccBrightness:
      IColorToRGB(HSBToIColor(fHue, fSat, fBri), fRed, fGreen, fBlue);
    ccRed, ccGreen, ccBlue:
      IColorToHSB(RGBToIColor(fRed, fGreen, fBlue), fHue, fSat, fBri);
  end;
end;

procedure TColorChart.SetHSB(const Hue, Sat, Bri: integer);
begin
  if (fHue <> Hue) or (fSat <> Sat) or (fBri <> Bri) then
  begin
    fHue := Hue;
    fSat := Sat;
    fBri := Bri;
    IColorToRGB(HSBToIColor(fHue, fSat, fBri), fRed, fGreen, fBlue);
    RefreshTarget;
    CreateGradient;
    Invalidate;
  end;
end;

procedure TColorChart.SetRGB(const Red, Green, Blue: integer);
begin
  if (fRed <> Red) or (fGreen <> Green) or (fBlue <> Blue) then
  begin
    fRed := Red;
    fGreen := Green;
    fBlue := Blue;
    IColorToHSB(RGBToIColor(fRed, fGreen, fBlue), fHue, fSat, fBri);
    RefreshTarget;
    CreateGradient;
    Invalidate;
  end;
end;

{ TColorBar }

procedure TColorBar.NotifyChange;
begin
  RefreshValues;
  case fFixComponent of
    ccHue, ccSaturation, ccBrightness:
      if Assigned(fOnHSBChange) then
        fOnHSBChange(Self, fHue, fSat, fBri);
    ccRed, ccGreen, ccBlue:
      if Assigned(fOnRGBChange) then
        fOnRGBChange(Self, fRed, fGreen, fBlue);
  end;
end;

procedure TColorBar.SetTarget(const Z: integer; const ARedraw: boolean;
  const ARefresh: boolean);
begin
  if fTarget = Z then
    exit;
  fTarget := EnsureRange(Z, 0, 255);
  if ARedraw then
    Invalidate;
  if ARefresh then
  begin
    RefreshValues;
    NotifyChange;
  end;
end;

procedure TColorBar.CreateGradient;
var
  c: TAggColor;
  i, j, z1, z2: integer;
  p: PBitmapLine;
  f: TColorSpaceFunction;
begin
  f := ColorSpaceFunctions[fFixComponent];
  case fFixComponent of
    ccHue:
    begin
      Z1 := Sat100;
      Z2 := Bri100;
    end;
    ccSaturation:
    begin
      Z1 := fHue;
      Z2 := fBri;
    end;
    ccBrightness:
    begin
      Z1 := fHue;
      Z2 := fSat;
    end;
    ccRed:
    begin
      Z1 := fGreen shl 8;
      Z2 := fBlue shl 8;
    end;
    ccGreen:
    begin
      Z1 := fRed shl 8;
      Z2 := fBlue shl 8;
    end;
    ccBlue:
    begin
      Z1 := fRed shl 8;
      Z2 := fGreen shl 8;
    end;
    else
    begin
      Z1 := 0;
      Z2 := 0;
    end;
  end;
  fGradBmp.BeginUpdate;
  try
    for i := 0 to 255 do
    begin
      c := f(Z1, Z2, (256 - i) shl 8 - 128);
      p := PBitmapLine(fGradBmp.RawImage.GetLineStart(i));
      for j := 0 to fGradBmp.Width - 1 do
      begin
        p^[j].R := c.r;
        p^[j].G := c.g;
        p^[j].B := c.b;
        p^[j].A := c.a;
      end;
    end;
  finally
    fGradBmp.EndUpdate();
  end;
end;

procedure TColorBar.PaintPicker;
var
  x, y: integer;
begin
  fBitmap.Canvas.Draw(1, 1, fGradBmp);
  DrawBounds;
  x := Width div 2;
  y := 256 - fTarget;
  DrawTarget(x, y);
  Canvas.Draw(0, 0, fBitmap);
end;

procedure TColorBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)) then
  begin
    fTargeting := True;
    SetTarget(256 - Y, True, True);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TColorBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  fTargeting := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TColorBar.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if fTargeting or ((X >= 1) and (Y >= 1) and (X < ClientWidth - 1) and (Y < 257)) then
    ChangeCursor(crCircle)
  else
    ChangeCursor(crDefault);
  if fTargeting then
    SetTarget(256 - Y, True, True);
  inherited MouseMove(Shift, X, Y);
end;

function TColorBar.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  SetTarget(fTarget + Sign(WheelDelta), True, True);
end;

procedure TColorBar.RefreshTarget;
begin
  case fFixComponent of
    ccHue: SetTarget(fHue shr 8);
    ccSaturation: SetTarget(fSat shr 8);
    ccBrightness: SetTarget(fBri shr 8);
    ccRed: SetTarget(fRed);
    ccGreen: SetTarget(fGreen);
    ccBlue: SetTarget(fBlue);
  end;
end;

procedure TColorBar.RefreshValues;
begin
  case FFixComponent of
    ccHue: fHue := MulDiv(fTarget, Hue360, 255) and HueMask;
    ccSaturation: fSat := MulDiv(fTarget, Sat100, 255);
    ccBrightness: fBri := MulDiv(fTarget, Bri100, 255);
    ccRed: fRed := fTarget;
    ccGreen: fGreen := fTarget;
    ccBlue: fBlue := fTarget;
  end;
  case fFixComponent of
    ccHue, ccSaturation, ccBrightness:
      IColorToRGB(HSBToIColor(fHue, fSat, fBri), fRed, fGreen, fBlue);
    ccRed, ccGreen, ccBlue:
      IColorToHSB(RGBToIColor(fRed, fGreen, fBlue), fHue, fSat, fBri);
  end;
end;

procedure TColorBar.SetHSB(const Hue, Sat, Bri: integer);
begin
  if (fHue = Hue) and (fSat = Sat) and (fBri = Bri) then
    exit;
  fHue := Hue;
  fSat := Sat;
  fBri := Bri;
  IColorToRGB(HSBToIColor(fHue, fSat, fBri), fRed, fGreen, fBlue);
  RefreshTarget;
  CreateGradient;
  Invalidate;
end;

procedure TColorBar.SetRGB(const Red, Green, Blue: integer);
begin
  if (fRed = Red) and (fGreen = Green) and (fBlue = Blue) then
    exit;
  fRed := Red;
  fGreen := Green;
  fBlue := Blue;
  IColorToHSB(RGBToIColor(fRed, fGreen, fBlue), fHue, fSat, fBri);
  RefreshTarget;
  CreateGradient;
  Invalidate;
end;

{ TColorPreview }

procedure TColorPreview.PaintPicker;
const
  sRevert = 'Revert';
var
  r: TRect;
  h: integer;
  e: TSize;
begin
  h := Height div 2;
  r := Rect(1, 1, Width - 1, h);
  DrawIColorRect(fBitmap.Canvas, r, AggColorToIColor(fColor), true, false, 8);
  r := Rect(1, h, Width - 1, Height - 1);
  DrawIColorRect(fBitmap.Canvas, r, AggColorToIColor(fRecentColor), true, false, 8);
  if fHover then
  begin
    fBitmap.Canvas.Brush.Style := bsClear;
    fBitmap.Canvas.Font.Color := GetContrastColor(fBitmap.Canvas.Pixels[1, Height - 2]);
    e := fBitmap.Canvas.TextExtent(sRevert);
    fBitmap.Canvas.TextOut((Width - e.CX) div 2, (h + e.CY div 2), sRevert);
  end;
  DrawBounds;
  Canvas.Draw(0, 0, fBitmap);
end;

procedure TColorPreview.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if (Button = mbLeft) and (X >= 1) and (Y >= 1) and (X < Width - 1) and
    (Y > Height div 2) then
    fDown := True
  else
    fDown := False;
  inherited;
end;

procedure TColorPreview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if fDown and fHover and (Button = mbLeft) then
  begin
    if Assigned(fOnRevertColor) then
      fOnRevertColor(Self);
  end;
  fDown := False;
  inherited;
end;

procedure TColorPreview.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if (X >= 1) and (Y >= 1) and (X < Width - 1) and (Y > Height div 2) then
  begin
    if not fHover then
    begin
      fHover := True;
      ChangeCursor(crCircle);
      Invalidate;
    end;
  end
  else
  begin
    if fHover then
    begin
      fHover := False;
      ChangeCursor(crDefault);
      Invalidate;
    end;
  end;
  inherited;
end;

procedure TColorPreview.DoMouseLeave;
begin
  if fHover then
  begin
    fHover := False;
    Invalidate;
  end;
  inherited;
end;

procedure TColorPreview.SetColor(Value: TAggColor);
begin
  if CompareMem(@fColor, @Value, SizeOf(TAggColor)) then
    exit;
  fColor := Value;
  Invalidate;
end;

procedure TColorPreview.SetRecentColor(Value: TAggColor);
begin
  if CompareMem(@fRecentColor, @Value, SizeOf(TAggColor)) then
    exit;
  fRecentColor := Value;
  Invalidate;
end;



initialization
  InitModule;

end.
