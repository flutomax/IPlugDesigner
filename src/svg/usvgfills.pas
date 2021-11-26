{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uSVGFills;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, Graphics, GR32, GR32_Paths, GR32_Transforms, GR32_VPR,
  GR32_Polygons, GR32_ColorGradients, uSVGObjects, uSVGTypes;

type

  TColors = record
    Colors: packed array of cardinal;
    Positions: packed array of single;
    Count: integer;
  end;

  { TSVGStop }

  TSVGStop = class(TSVGObject)
  private
    FStop: TFloat;
    FStopColor: TColor;
    FOpacity: TFloat;
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TFlattenedPath); override;
    property Stop: TFloat read FStop write FStop;
    property StopColor: TColor read FStopColor write FStopColor;
    property Opacity: TFloat read FOpacity write FOpacity;
  end;

  { TSVGFiller }

  TSVGFiller = class(TSVGTransformedObject)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
    function GetBrush(Alpha: byte; const DestObject: TSVGBase): TCustomPolygonFiller;
      virtual; abstract;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TFlattenedPath); override;
  end;

  { TSVGPattern }

  TSVGPattern = class(TSVGFiller)
    // http://www.w3.org/TR/SVG/pservers.html#PatternElement
  private
    FViewBox: TFRect;
    FFill: TBitmapPolygonFiller;
    FY: TFloat;
    FHeight: TFloat;
    FWidth: TFloat;
    FX: TFloat;
    procedure SetViewBox(const Value: TFRect);
  public
    function GetBrush(Alpha: byte; const DestObject: TSVGBase): TCustomPolygonFiller;
      override;
    procedure ReadIn(const Node: TDOMNode); override;
    property X: TFloat read FX write FX;
    property Y: TFloat read FY write FY;
    property Width: TFloat read FWidth write FWidth;
    property Height: TFloat read FHeight write FHeight;
    property ViewBox: TFRect read FViewBox write SetViewBox;
  end;

  { TSVGGradient }

  TSVGGradient = class(TSVGFiller)
  private
    FURI: string;
    FGradientUnits: TGradientUnits;
  protected
    function GetColors(Alpha: byte): TColors; virtual;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGLinearGradient }

  TSVGLinearGradient = class(TSVGGradient)
  private
    FX1: TFloat;
    FY1: TFloat;
    FX2: TFloat;
    FY2: TFloat;
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
    function GetBrush(Alpha: byte; const DestObject: TSVGBase): TCustomPolygonFiller;
      override;
    property X1: TFloat read FX1 write FX1;
    property Y1: TFloat read FY1 write FY1;
    property X2: TFloat read FX2 write FX2;
    property Y2: TFloat read FY2 write FY2;
  end;

  { TSVGRadialGradient }

  TSVGRadialGradient = class(TSVGGradient)
  private
    fCX: TFloat;
    fCY: TFloat;
    fR: TFloat;
    fFX: TFloat;
    fFY: TFloat;
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure Clear; override;
    procedure ReadIn(const Node: TDOMNode); override;
    function GetBrush(Alpha: byte; const DestObject: TSVGBase): TCustomPolygonFiller;
      override;
    property CX: TFloat read fCX write fCX;
    property CY: TFloat read fCY write fCY;
    property R: TFloat read fR write fR;
    property FX: TFloat read fFX write fFX;
    property FY: TFloat read fFY write fFY;
  end;

  { TSVGRadialGradientFiller }

  TSVGRadialGradientFiller = class(TCustomRadialGradientPolygonFiller)
  private
    fOffset: TFloatPoint;
    fRadius: TFloatPoint;
    fCenter: TFloatPoint;
    fFocalPt: TFloatPoint;
    fVertDist: TFloat;
    fFocalPointNative: TFloatPoint;
    procedure InitMembers;
    procedure SetFocalPoint(AValue: TFloatPoint);
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure EllipseBoundsChanged; override;
    procedure FillLineEllipse(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    procedure BeginRendering; override;
    property FocalPoint: TFloatPoint read fFocalPointNative write SetFocalPoint;
  end;

  { TSolidPoligonFiller }

  TSolidPoligonFiller = class(TCustomPolygonFiller)
  private
    fColor: TColor32;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineBlend(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    property Color: TColor32 read fColor write fColor;
  end;

  { TSolidPoligonRenderer }

  TSolidPoligonRenderer = class(TPolygonRenderer32VPR)
  private
    fFillProc: TFillProc;
    procedure UpdateFillProcs;
  protected
    procedure FillSpan(const Span: TValueSpan; DstY: integer); override;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;


  { TSVGLinearGradientFiller }

  TSVGLinearGradientFiller = class(TLinearGradientPolygonFiller)
  private
    fIncline: TFloat;
    function ColorStopToScanLine(Index: integer; Y: integer): TFloat;
  protected
    function GetFillLine: TFillLineEvent; override;

    procedure FillLineNegative(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLinePositive(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineVertical(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineVerticalExtreme(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineVerticalPad(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineVerticalPadExtreme(Dst: PColor32;
      DstX, DstY, Length: integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineVerticalWrap(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineHorizontalPadPos(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineHorizontalPadNeg(Dst: PColor32; DstX, DstY, Length: integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineHorizontalWrapNeg(Dst: PColor32;
      DstX, DstY, Length: integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineHorizontalWrapPos(Dst: PColor32;
      DstX, DstY, Length: integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
  public
    procedure BeginRendering; override;
  end;


procedure SVGPolyPolygonFS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller;
  FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);
procedure SVGPolyPolylineFS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller;
  Closed: boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter;
  EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0;
  Transformation: TTransformation = nil);
function MixColors(F, B: TColor32; Alpha: integer): TColor32;

implementation

uses
  GR32_Math, GR32_LowLevel, GR32_Blend, GR32_VectorUtils, GR32_Gamma,
  uSVGProc;

type
  TBitmap32Access = class(TCustomBitmap32);

const
  CFloatTolerance = 0.001;

procedure SVGPolyPolygonFS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller;
  FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TSolidPoligonRenderer;
begin
  if not Assigned(Filler) then
    Exit;
  Renderer := TSolidPoligonRenderer.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure SVGPolyPolylineFS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller;
  Closed: boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFloat; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  if MiterLimit = 0 then
    MiterLimit := 0.00001;
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  SVGPolyPolygonFS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

procedure MakeAlphaNonZero(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: integer; Color: TColor32);
var
  I, V: integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Clamp(Round(Abs(Coverage^[I]) * 256));
    V := GAMMA_DECODING_TABLE[V];
    AlphaValues^[I] := V;
  end;
end;

function MixColors(F, B: TColor32; Alpha: integer): TColor32;
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  RX: TColor32Entry absolute Result;
begin
  RX.R := BX.R + (FX.R - BX.R) * Alpha div 256;
  RX.G := BX.G + (FX.G - BX.G) * Alpha div 256;
  RX.B := BX.B + (FX.B - BX.B) * Alpha div 256;
  RX.A := BX.A + (FX.A - BX.A) * Alpha div 256;
end;

procedure FillLineAlpha(var Dst, AlphaValues: PColor32; Count: integer;
  Color: TColor32);
var
  X: integer;
begin
  for X := 0 to Count - 1 do
  begin
    Dst^ := MixColors(Color, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

{ TSVGStop }

procedure TSVGStop.ReadIn(const Node: TDOMNode);
var
  S: string;
begin
  inherited;
  LoadPercent(Node, 'offset', FStop);

  LoadString(Node, 'stop-color', S);
  FStopColor := GetColor(S);

  if FStopColor = INHERIT then
  begin
    S := Style['stop-color'];
    FStopColor := GetColor(S);
  end;

  S := Style['stop-opacity'];
  if (S <> '') then
    FOpacity := ParsePercent(S)
  else
    FOpacity := 1;

  if (FOpacity < 0) then
    FOpacity := 0;

  if (FOpacity > 1) then
    FOpacity := 1;
end;

procedure TSVGStop.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGStop then
  begin
    FStop := TSVGStop(SVG).FStop;
    FStopColor := TSVGStop(SVG).FStopColor;
  end;
end;

function TSVGStop.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGStop.Create(AParent);
end;

procedure TSVGStop.PaintToGraphics(Graphics: TBitmap32);
begin

end;

procedure TSVGStop.PaintToPath(Path: TFlattenedPath);
begin

end;

{ TSVGFiller }

function TSVGFiller.New(AParent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;

procedure TSVGFiller.ReadIn(const Node: TDOMNode);
begin
  inherited ReadIn(Node);
  Display := 0;
end;

procedure TSVGFiller.PaintToGraphics(Graphics: TBitmap32);
begin

end;

procedure TSVGFiller.PaintToPath(Path: TFlattenedPath);
begin

end;


{ TSVGPattern }

procedure TSVGPattern.SetViewBox(const Value: TFRect);
begin
  FViewBox := Value;
end;

function TSVGPattern.GetBrush(Alpha: byte;
  const DestObject: TSVGBase): TCustomPolygonFiller;

  procedure PaintItem(const Item: TSVGObject);
  var
    C: integer;
  begin
    Item.PaintToGraphics(TBitmap32(FFill.Pattern));
    for C := 0 to Item.Count - 1 do
      PaintItem(Item[C]);
  end;

var
  I: integer;

begin
  if not Assigned(FFill) then
  begin
    FFill := TBitmapPolygonFiller.Create;
    FFill.Pattern := TBitmap32.Create;
    FFill.Pattern.SetSize(Round(Width), Round(Height));
    FFill.Pattern.DrawMode := dmBlend;
    FFill.Pattern.CombineMode := cmBlend;
    FFill.Pattern.Clear($0);
    for I := 0 to Count - 1 do
      PaintItem(Items[I]);
  end;

  Result := FFill;
end;

procedure TSVGPattern.ReadIn(const Node: TDOMNode);
var
  LViewBox: string;
  T: TAffineTransformation;
  dummy: TFloatPoint;
  w, h: TSVGUnit;
begin
  inherited ReadIn(Node);
  LoadLength(Node, 'x', FX);
  LoadLength(Node, 'y', FY);
  LoadLength(Node, 'width', FWidth);
  LoadLength(Node, 'height', FHeight);

  LViewBox := GetAttrValue(Node, 'viewBox');
  if LViewBox <> '' then
    FViewBox := ParseDRect(LViewBox, w, h);

  if (FViewBox.Width <> 0) and (ViewBox.Height <> 0) then
  begin
    T := TAffineTransformation.Create;
    T.Scale(Width / ViewBox.Width, Height / ViewBox.Height);
    dummy := T.Transform(FloatPoint(1, 1)); //validate transformation
    Self.PureMatrix := T.Matrix;
    T.Free;
  end;
  ReadChildren(Node);
end;

{ TSVGGradient }

function TSVGGradient.GetColors(Alpha: byte): TColors;
var
  C, Start, ColorCount: integer;
  Stop: TSVGStop;
  Item: TSVGGradient;
begin
  Result.Count := 0;
  if FURI = '' then
    Item := Self
  else
  begin
    Item := TSVGGradient(GetRoot.FindByID(FURI));
    if not (Item is TSVGGradient) then
      Exit;
  end;

  Start := 0;
  ColorCount := Item.Count;

  if Item.Count = 0 then
    Exit;

  if TSVGStop(Item.Items[ColorCount - 1]).FStop < 1 then
    Inc(ColorCount);

  if TSVGStop(Item.Items[0]).FStop > 0 then
  begin
    Inc(ColorCount);
    Inc(Start);
  end;

  SetLength(Result.Colors, ColorCount);
  SetLength(Result.Positions, ColorCount);

  if Start > 0 then
  begin
    Stop := TSVGStop(Item.Items[0]);
    Result.Colors[0] := ConvertColor(Stop.FStopColor, Round(Alpha * Stop.FOpacity));
    Result.Positions[0] := 0;
  end;

  for C := 0 to Item.Count - 1 do
  begin
    Stop := TSVGStop(Item.Items[C]);
    Result.Colors[C + Start] :=
      ConvertColor(Stop.FStopColor, Round(Alpha * Stop.FOpacity));
    Result.Positions[C + Start] := Stop.FStop;
  end;

  if (ColorCount - Start) > Item.Count then
  begin
    Stop := TSVGStop(Item.Items[Item.Count - 1]);
    Result.Colors[ColorCount - 1] :=
      ConvertColor(Stop.FStopColor, Round(Alpha * Stop.FOpacity));
    Result.Positions[ColorCount - 1] := 1;
  end;

  Result.Count := ColorCount;
end;

procedure TSVGGradient.ReadIn(const Node: TDOMNode);
var
  C: integer;
  Stop: TSVGStop;
begin
  inherited ReadIn(Node);
  LoadGradientUnits(Node, FGradientUnits);

  for C := 0 to Node.childNodes.Count - 1 do
    if Node.childNodes[C].nodeName = 'stop' then
    begin
      Stop := TSVGStop.Create(Self);
      Stop.ReadIn(Node.childNodes[C]);
    end;

  FURI := Style['xlink:href'];
  if FURI <> '' then
  begin
    FURI := Trim(FURI);
    if (FURI <> '') and (FURI[1] = '#') then
      FURI := Copy(FURI, 2, MaxInt);
  end;
end;

{ TSVGLinearGradient }

function TSVGLinearGradient.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGLinearGradient.Create(AParent);
end;

procedure TSVGLinearGradient.Assign(SVG: TSVGObject);
begin
  inherited Assign(SVG);
  if SVG is TSVGLinearGradient then
  begin
    FX1 := TSVGLinearGradient(SVG).FX1;
    FY1 := TSVGLinearGradient(SVG).FY1;
    FX2 := TSVGLinearGradient(SVG).FX2;
    FY2 := TSVGLinearGradient(SVG).FY2;
  end;
end;

procedure TSVGLinearGradient.ReadIn(const Node: TDOMNode);
var
  m: TFloatMatrix;
begin
  inherited ReadIn(Node);
  LoadLength(Node, 'x1', FX1);
  LoadLength(Node, 'y1', FY1);
  LoadLength(Node, 'x2', FX2);
  LoadLength(Node, 'y2', FY2);

  FillChar(m, SizeOf(m), 0);
  LoadTransform(Node, 'gradientTransform', m);
  PureMatrix := m;
end;

function TSVGLinearGradient.GetBrush(Alpha: byte;
  const DestObject: TSVGBase): TCustomPolygonFiller;
var
  Colors: TColors;
  Gradient: TColor32Gradient;
  GradientLUT: TColor32LookupTable;
  LinearGradFiller: TCustomLinearGradientPolygonFiller;
  i: integer;
  R: TFloatRect;
  T: TAffineTransformation;
  M: TFloatMatrix;
begin
  if not Assigned(DestObject) then
    M := IdentityMatrix
  else
    M := DestObject.Matrix;

  if Assigned(DestObject) and (FGradientUnits = guObjectBoundingBox) then
    R := FloatRect(DestObject.X, DestObject.Y, DestObject.X +
      DestObject.Width, DestObject.Y + DestObject.Height)
  else
    R := FloatRect(FX1, FY1, FX2, FY2);

  if PureMatrix[2, 2] <> 0 then
    M := Mult(M, PureMatrix);

  T := uSVGTypes.GetSVGTransformation(M);
  try
    R.TopLeft := T.Transform(R.TopLeft);
    R.BottomRight := T.Transform(R.BottomRight);
  finally
    T.Free;
  end;

  Colors := GetColors(Alpha);
  GradientLUT := TColor32LookupTable.Create;
  try
    Gradient := TColor32Gradient.Create;
    try
      for i := 0 to Colors.Count - 1 do
        Gradient.AddColorStop(Colors.Positions[i], Colors.Colors[i]);
      Gradient.FillColorLookUpTable(GradientLUT);
    finally
      Gradient.Free;
    end;

    LinearGradFiller := TSVGLinearGradientFiller.Create(GradientLUT);
    LinearGradFiller.StartPoint := R.TopLeft;
    LinearGradFiller.EndPoint := R.BottomRight;
    LinearGradFiller.WrapMode := wmClamp;
  finally
    //GradientLUT.Free;
  end;
  Finalize(Colors);
  Result := LinearGradFiller;
end;


{ TSVGRadialGradient }

function TSVGRadialGradient.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGRadialGradient.Create(AParent);
end;

procedure TSVGRadialGradient.Assign(SVG: TSVGObject);
begin
  inherited Assign(SVG);
  if SVG is TSVGRadialGradient then
  begin
    fCX := TSVGRadialGradient(SVG).fCX;
    fCY := TSVGRadialGradient(SVG).fCY;
    fFX := TSVGRadialGradient(SVG).fFX;
    fFY := TSVGRadialGradient(SVG).fFY;
    fR := TSVGRadialGradient(SVG).fR;
  end;
end;

procedure TSVGRadialGradient.Clear;
begin
  inherited Clear;
  fCX := 0.5;
  fCY := 0.5;
  fR := 0.5;
  fFX := fCX;
  fFY := fCY;
end;

procedure TSVGRadialGradient.ReadIn(const Node: TDOMNode);
var
  m: TFloatMatrix;
begin
  inherited ReadIn(Node);
  LoadLength(Node, 'cx', fCX);
  LoadLength(Node, 'cy', fCY);
  LoadLength(Node, 'r', fR);
  LoadLength(Node, 'fx', fFX);
  LoadLength(Node, 'fy', fFY);

  FillChar(m, SizeOf(m), 0);
  LoadTransform(Node, 'gradientTransform', m);
  PureMatrix := m;
end;

function TSVGRadialGradient.GetBrush(Alpha: byte;
  const DestObject: TSVGBase): TCustomPolygonFiller;
var
  Colors: TColors;
  Gradient: TColor32Gradient;
  GradientLUT: TColor32LookupTable;
  RadialGradFiller: TSVGRadialGradientFiller;
  i: integer;
  LR: TFloatRect;
  F, C: TFloatPoint;
  T: TAffineTransformation;
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  if Assigned(DestObject) then
    M := DestObject.Matrix;

  if Assigned(DestObject) and (FGradientUnits = guObjectBoundingBox) then
    LR := FloatRect(DestObject.X, DestObject.Y, DestObject.X +
      DestObject.Width, DestObject.Y + DestObject.Height)
  else
    LR := FloatRect(fCX - fR, fCY - fR, fCX + fR, fCY + fR);

  Colors := GetColors(Alpha);

  GradientLUT := TColor32LookupTable.Create;
  try
    Gradient := TColor32Gradient.Create;
    try
      for i := 0 to Colors.Count - 1 do
      begin
        Gradient.AddColorStop(Colors.Positions[i], Colors.Colors[i]);
      end;
      Gradient.FillColorLookUpTable(GradientLUT);
    finally
      Gradient.Free;
    end;

    F := FloatPoint(fFX, fFY);
    C := FloatPoint(fCX, fCY);

    if PureMatrix[2, 2] <> 0 then
      M := Mult(M, PureMatrix);

    T := uSVGTypes.GetSVGTransformation(M);
    try
      LR.TopLeft := T.Transform(LR.TopLeft);
      LR.BottomRight := T.Transform(LR.BottomRight);
      F := T.Transform(F);
      C := T.Transform(C);
    finally
      T.Free;
    end;

    RadialGradFiller := TSVGRadialGradientFiller.Create(GradientLUT);
    RadialGradFiller.WrapMode := wmClamp;
    RadialGradFiller.EllipseBounds := LR;
    RadialGradFiller.FocalPoint := F;

  finally
    //GradientLUT.Free;
  end;
  Finalize(Colors);
  Result := RadialGradFiller;
end;


{ TSVGRadialGradientFiller }

procedure TSVGRadialGradientFiller.InitMembers;
var
  X, Y: TFloat;
  Temp: TFloat;
begin
  fRadius.X := (EllipseBounds.Right - EllipseBounds.Left) * 0.5;
  fRadius.Y := (EllipseBounds.Bottom - EllipseBounds.Top) * 0.5;
  fCenter.X := (EllipseBounds.Right + EllipseBounds.Left) * 0.5;
  fCenter.Y := (EllipseBounds.Bottom + EllipseBounds.Top) * 0.5;
  fOffset.X := EllipseBounds.Left;
  fOffset.Y := EllipseBounds.Top;

  // make FFocalPoint relative to the ellipse midpoint ...
  fFocalPt.X := fFocalPointNative.X - fCenter.X;
  fFocalPt.Y := fFocalPointNative.Y - fCenter.Y;

  // make sure the focal point stays within the bounding ellipse ...
  if Abs(fFocalPt.X) < CFloatTolerance then
  begin
    X := 0;
    if fFocalPt.Y < 0 then
      Y := -1
    else
      Y := 1;
  end
  else
  begin
    Temp := fRadius.X * fFocalPt.Y / (fRadius.Y * fFocalPt.X);
    X := 1 / FastSqrtBab1(1 + Sqr(Temp));
    Y := Temp * X;
  end;
  if fFocalPt.X < 0 then
  begin
    X := -X;
    Y := -Y;
  end;
  X := X * fRadius.X;
  Y := Y * fRadius.Y;
  if (Y * Y + X * X) < (Sqr(fFocalPt.X) + Sqr(fFocalPt.Y)) then
  begin
    fFocalPt.X := 0.999 * X;
    fFocalPt.Y := 0.999 * Y;
  end;

  // Because the slope of vertical lines is infinite, we need to find where a
  // vertical line through the FocalPoint intersects with the Ellipse, and
  // store the distances from the focal point to these 2 intersections points
  fVertDist := fRadius.Y * FastSqrtBab1(1.0 - Sqr(fFocalPt.X) / Sqr(fRadius.X));
end;

procedure TSVGRadialGradientFiller.SetFocalPoint(AValue: TFloatPoint);
begin
  if (fFocalPointNative.X <> AValue.X) and (fFocalPointNative.Y <> AValue.Y) then
  begin
    fFocalPointNative := AValue;
    GradientFillerChanged;
  end;
end;

function TSVGRadialGradientFiller.GetFillLine: TFillLineEvent;
begin
  Result := @FillLineEllipse;
end;

procedure TSVGRadialGradientFiller.EllipseBoundsChanged;
begin
  GradientFillerChanged;
end;

procedure TSVGRadialGradientFiller.FillLineEllipse(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Mask: integer;
  ColorLUT: PColor32Array;
  Rad, Rad2, X2, Y2: TFloat;
  m, b, Qa, Qb, Qc, Qz, XSqr: double;
  RelPos: TFloatPoint;
  Color32: TColor32;
begin
  if (fRadius.X = 0) or (fRadius.Y = 0) then
    Exit;

  ColorLUT := GradientLUT.Color32Ptr;

  RelPos.Y := DstY - fCenter.Y - fFocalPt.Y;
  Mask := integer(GradientLUT.Mask);

  // check if out of bounds (vertically)
  if (DstY < fOffset.Y) or (DstY >= (fRadius.Y * 2) + 1 + fOffset.Y) then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  for X := DstX to DstX + Length - 1 do
  begin
    // check if out of bounds (horizontally)
    if (X < fOffset.X) or (X >= (fRadius.X * 2) + 1 + fOffset.X) then
      Color32 := ColorLUT^[Mask]
    else
    begin
      RelPos.X := X - fCenter.X - fFocalPt.X;

      if Abs(RelPos.X) < CFloatTolerance then //ie on the vertical line (see above)
      begin
        Assert(Abs(X - fCenter.X) <= fRadius.X);

        Rad := Abs(RelPos.Y);
        if Abs(Abs(X - fCenter.X)) <= fRadius.X then
        begin
          if RelPos.Y < 0 then
            Rad2 := Abs(-fVertDist - fFocalPt.Y)
          else
            Rad2 := Abs(fVertDist - fFocalPt.Y);
          if Rad >= Rad2 then
            Color32 := ColorLUT^[Mask]
          else
            Color32 := ColorLUT^[Round(Mask * Rad / Rad2)];
        end
        else
          Color32 := ColorLUT^[Mask];
      end
      else
      begin
        m := RelPos.Y / RelPos.X;
        b := fFocalPt.Y - m * fFocalPt.X;
        XSqr := Sqr(fRadius.X);

        // apply quadratic equation ...
        Qa := 2 * (Sqr(fRadius.Y) + XSqr * m * m);
        Qb := XSqr * 2 * m * b;
        Qc := XSqr * (b * b - Sqr(fRadius.Y));
        Qz := Qb * Qb - 2 * Qa * Qc;

        if Qz >= 0 then
        begin
          Qz := FastSqrtBab2(Qz);
          Qa := 1 / Qa;
          X2 := (-Qb + Qz) * Qa;
          if (fFocalPt.X > X2) = (RelPos.X > 0) then
            X2 := -(Qb + Qz) * Qa;
          Y2 := m * X2 + b;
          Rad := Sqr(RelPos.X) + Sqr(RelPos.Y);
          Rad2 := Sqr(X2 - fFocalPt.X) + Sqr(Y2 - fFocalPt.Y);

          if Rad >= Rad2 then
            Color32 := ColorLUT^[Mask]
          else
            Color32 := ColorLUT^[Round(Mask * FastSqrtBab1(Rad / Rad2))];
        end
        else
          Color32 := ColorLUT^[Mask];
      end;
    end;

    Dst^ := MixColors(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSVGRadialGradientFiller.BeginRendering;
begin
  if LookUpTableNeedsUpdate then
  begin
    if UseLookUpTable then
    begin
      if not Assigned(GradientLUT) then
        raise Exception.Create('No ColorLookupTable object specified');

      if Assigned(Gradient) then
        Gradient.FillColorLookUpTable(GradientLUT);
    end
    else
      if not Assigned(Gradient) then
        raise Exception.Create('No ColorGradient specified');
    inherited;
  end;
  InitMembers;
end;

{ TSolidPoligonFiller }

function TSolidPoligonFiller.GetFillLine: TFillLineEvent;
begin
  Result := @FillLineBlend;
end;

procedure TSolidPoligonFiller.FillLineBlend(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: integer;
begin
  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(fColor, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;


{ TSolidPoligonRenderer }

procedure TSolidPoligonRenderer.UpdateFillProcs;
begin
  fFillProc := @MakeAlphaNonZero;
end;

procedure TSolidPoligonRenderer.FillSpan(const Span: TValueSpan; DstY: integer);
var
  AlphaValues: PColor32Array;
  Count: integer;
begin
  Count := Span.X2 - Span.X1 + 1;
  GetMem(AlphaValues, Count * SizeOf(TColor32));
  fFillProc(Span.Values, AlphaValues, Count, Color);
  Filler.FillLine(@Bitmap.ScanLine[DstY][Span.X1], Span.X1, DstY, Count,
    PColor32(AlphaValues), Bitmap.CombineMode);
  EMMS;
  FreeMem(AlphaValues);
end;

procedure TSolidPoligonRenderer.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect);
var
  I: integer;
begin
  UpdateFillProcs;
  if Assigned(Filler) then
  begin
    Filler.BeginRendering;
    RenderPolyPolygon(Points, ClipRect, GetRenderSpan());
    Filler.EndRendering;
  end
  else
    RenderPolyPolygon(Points, ClipRect, GetRenderSpan());

  if TBitmap32Access(Bitmap).UpdateCount = 0 then
    for I := 0 to High(Points) do
      if Length(Points[I]) > 0 then
        Bitmap.Changed(MakeRect(PolygonBounds(Points[I])));
end;

{ TSVGLinearGradientFiller }


procedure TSVGLinearGradientFiller.BeginRendering;
begin
  inherited;
  if (EndPoint.X - StartPoint.X) <> 0 then
    fIncline := (EndPoint.Y - StartPoint.Y) / (EndPoint.X - StartPoint.X)
  else
  if (EndPoint.Y - StartPoint.Y) <> 0 then
    fIncline := 1 / (EndPoint.Y - StartPoint.Y);
end;

function TSVGLinearGradientFiller.ColorStopToScanLine(Index, Y: integer): TFloat;
var
  Offset: array [0 .. 1] of TFloat;
begin
  Offset[0] := Gradient.GradientEntry[Index].Offset;
  Offset[1] := 1.0 - Offset[0];
  Result := Offset[1] * StartPoint.X + Offset[0] * EndPoint.X +
    fIncline * (Offset[1] * (StartPoint.Y - Y) + Offset[0] * (EndPoint.Y - Y));
end;


function TSVGLinearGradientFiller.GetFillLine: TFillLineEvent;
var
  GradientCount: integer;
begin
  if Assigned(Gradient) then
    GradientCount := Gradient.GradientCount
  else
    GradientCount := GradientLUT.Size;

  case GradientCount of
    0:
      Result := @FillLineNone;
    1:
      Result := @FillLineSolid;
    else
      if UseLookUpTable then
        case WrapMode of
          wmClamp:
            if StartPoint.X = EndPoint.X then
              if StartPoint.Y = EndPoint.Y then
                Result := @FillLineVerticalPadExtreme
              else
                Result := @FillLineVerticalPad
            else
            if StartPoint.X < EndPoint.X then
              Result := @FillLineHorizontalPadPos
            else
              Result := @FillLineHorizontalPadNeg;
          wmMirror, wmRepeat:
            if StartPoint.X = EndPoint.X then
              Result := @FillLineVerticalWrap
            else
            if StartPoint.X < EndPoint.X then
              Result := @FillLineHorizontalWrapPos
            else
              Result := @FillLineHorizontalWrapNeg;
        end
      else
      if StartPoint.X = EndPoint.X then
        if StartPoint.Y = EndPoint.Y then
          Result := @FillLineVerticalExtreme
        else
          Result := @FillLineVertical
      else
      if StartPoint.X < EndPoint.X then
        Result := @FillLinePositive
      else
        Result := @FillLineNegative;
  end;
end;

procedure TSVGLinearGradientFiller.FillLineVertical(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: integer;
  Color32: TColor32;
begin
  Color32 := Gradient.GetColorAt((DstY - StartPoint.Y) * fIncline);

  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TSVGLinearGradientFiller.FillLineVerticalExtreme(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: integer;
  Color32: TColor32;
begin
  if DstY < StartPoint.Y then
    Color32 := Gradient.StartColor
  else
    Color32 := Gradient.EndColor;

  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TSVGLinearGradientFiller.FillLinePositive(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index: integer;
  IntScale, IntValue: integer;
  Colors: array [0..1] of TColor32;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  XPos: array [0..2] of integer;
begin

  // set first offset/position
  XOffset[0] := ColorStopToScanLine(0, DstY);
  XPos[0] := Round(XOffset[0]);
  XPos[2] := DstX + Length;

  // check if only a solid start color should be drawn.
  if XPos[0] >= XPos[2] - 1 then
  begin
    FillLineSolid(Dst, DstX, DstY, Length, AlphaValues, CombineMode);
    Exit;
  end;

  // set start color
  Colors[0] := Gradient.GradientEntry[0].Color32;

  // eventually draw solid start color
  FillLineAlpha(Dst, AlphaValues, XPos[0] - DstX, Colors[0]);

  Index := 1;
  repeat
    // set start position to be at least DstX
    if XPos[0] < DstX then
      XPos[0] := DstX;

    // set destination color and offset
    Colors[1] := Gradient.GradientEntry[Index].Color32;
    XOffset[1] := ColorStopToScanLine(Index, DstY);

    // calculate destination pixel position
    XPos[1] := Round(XOffset[1]);
    if XPos[1] > XPos[2] then
      XPos[1] := XPos[2];

    // check whether
    if XPos[1] > XPos[0] then
    begin
      Scale := 1 / (XOffset[1] - XOffset[0]);
      IntScale := Round($7FFFFFFF * Scale);
      IntValue := Round($7FFFFFFF * (XPos[0] - XOffset[0]) * Scale);

      for X := XPos[0] to XPos[1] - 1 do
      begin
        Dst^ := MixColors(CombineReg(Colors[1], Colors[0], IntValue shr 23),
          Dst^, AlphaValues^);
        IntValue := IntValue + IntScale;

        Inc(Dst);
        Inc(AlphaValues);
      end;
      EMMS;
    end;

    // check whether further drawing is still necessary
    if XPos[1] = XPos[2] then
      Exit;

    Inc(Index);

    XPos[0] := XPos[1];
    XOffset[0] := XOffset[1];
    Colors[0] := Colors[1];
  until (Index = Gradient.GradientCount);

  if XPos[0] < DstX then
    XPos[0] := DstX;

  FillLineAlpha(Dst, AlphaValues, XPos[2] - XPos[0], Colors[0]);
end;


procedure TSVGLinearGradientFiller.FillLineNegative(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index: integer;
  IntScale, IntValue: integer;
  Colors: array [0..1] of TColor32;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  XPos: array [0..2] of integer;
begin
  Index := Gradient.GradientCount - 1;

  // set first offset/position
  XOffset[0] := ColorStopToScanLine(Index, DstY);
  XPos[0] := Round(XOffset[0]);
  XPos[2] := DstX + Length;

  // set start color
  Colors[0] := Gradient.GradientEntry[Index].Color32;

  // check if only a solid start color should be drawn.
  if XPos[0] >= XPos[2] - 1 then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, Colors[0]);
    Exit;
  end;

  // eventually draw solid start color
  FillLineAlpha(Dst, AlphaValues, XPos[0] - DstX, Colors[0]);

  Dec(Index);
  repeat
    // set start position to be at least DstX
    if XPos[0] < DstX then
      XPos[0] := DstX;

    // set destination color and offset
    Colors[1] := Gradient.GradientEntry[Index].Color32;
    XOffset[1] := ColorStopToScanLine(Index, DstY);

    // calculate destination pixel position
    XPos[1] := Round(XOffset[1]);
    if XPos[1] > XPos[2] then
      XPos[1] := XPos[2];

    // check whether next color needs to be drawn
    if XPos[1] > XPos[0] then
    begin
      Scale := 1 / (XOffset[1] - XOffset[0]);
      IntScale := Round($7FFFFFFF * Scale);
      IntValue := Round($7FFFFFFF * (XPos[0] - XOffset[0]) * Scale);

      for X := XPos[0] to XPos[1] - 1 do
      begin
        Dst^ := MixColors(CombineReg(Colors[1], Colors[0], IntValue shr 23),
          Dst^, AlphaValues^);
        IntValue := IntValue + IntScale;

        Inc(Dst);
        Inc(AlphaValues);
      end;
      EMMS;
    end;

    // check whether further drawing is still necessary
    if XPos[1] = XPos[2] then
      Exit;

    Dec(Index);

    XPos[0] := XPos[1];
    XOffset[0] := XOffset[1];
    Colors[0] := Colors[1];
  until (Index < 0);

  if XPos[0] < DstX then
    XPos[0] := DstX;

  FillLineAlpha(Dst, AlphaValues, XPos[2] - XPos[0], Colors[0]);
end;

procedure TSVGLinearGradientFiller.FillLineVerticalPad(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: integer;
  Color32: TColor32;
begin
  Color32 := GradientLUT.Color32Ptr^[Clamp(Round(GradientLUT.Mask *
    (DstY - StartPoint.Y) * fIncline), GradientLUT.Mask)];

  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TSVGLinearGradientFiller.FillLineVerticalPadExtreme(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: integer;
  Color32: TColor32;
begin
  if DstY < StartPoint.Y then
    Color32 := GradientLUT.Color32Ptr^[0]
  else
    Color32 := GradientLUT.Color32Ptr^[GradientLUT.Mask];

  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TSVGLinearGradientFiller.FillLineVerticalWrap(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: integer;
  Color32: TColor32;
begin
  X := Round(GradientLUT.Mask * (DstY - StartPoint.Y) * fIncline);
  Color32 := GradientLUT.Color32Ptr^[Clamp(X, integer(GradientLUT.Mask))];

  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TSVGLinearGradientFiller.FillLineHorizontalPadPos(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, XPos, Count, Mask: integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := StartPoint.X + (StartPoint.Y - DstY) * fIncline;
  XOffset[1] := EndPoint.X + (EndPoint.Y - DstY) * fIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;
  ColorLUT := GradientLUT.Color32Ptr;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[0]);
    Exit;
  end;

  Mask := GradientLUT.Mask;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(ColorLUT^[Clamp(Round((X - XOffset[0]) * Scale), Mask)],
      Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSVGLinearGradientFiller.FillLineHorizontalPadNeg(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, XPos, Count, Mask: integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := EndPoint.X + (EndPoint.Y - DstY) * fIncline;
  XOffset[1] := StartPoint.X + (StartPoint.Y - DstY) * fIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;

  Mask := GradientLUT.Mask;
  ColorLUT := GradientLUT.Color32Ptr;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[0]);
    Exit;
  end;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Dst^ := MixColors(ColorLUT^[Clamp(Round((XOffset[1] - X) * Scale), Mask)],
      Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSVGLinearGradientFiller.FillLineHorizontalWrapPos(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index, Mask: integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := StartPoint.X + (StartPoint.Y - DstY) * fIncline;
  XOffset[1] := EndPoint.X + (EndPoint.Y - DstY) * fIncline;
  Mask := integer(GradientLUT.Mask);
  ColorLUT := GradientLUT.Color32Ptr;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round((X - XOffset[0]) * Scale);
    Dst^ := MixColors(ColorLUT^[Clamp(Index, Mask)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSVGLinearGradientFiller.FillLineHorizontalWrapNeg(Dst: PColor32;
  DstX, DstY, Length: integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index, Mask: integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := EndPoint.X + (EndPoint.Y - DstY) * fIncline;
  XOffset[1] := StartPoint.X + (StartPoint.Y - DstY) * fIncline;
  Mask := integer(GradientLUT.Mask);
  ColorLUT := GradientLUT.Color32Ptr;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round((XOffset[1] - X) * Scale);
    Dst^ := MixColors(ColorLUT^[Clamp(Index, Mask)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

end.
