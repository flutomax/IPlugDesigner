{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uSVGPath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GR32, GR32_Paths, uSVGTypes, uSVGObjects;

type

  { TSVGPathElement }

  TSVGPathElement = class(TSVGObject)
  private
    FStartX: TFloat;
    FStartY: TFloat;
    FStopX: TFloat;
    FStopY: TFloat;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; virtual; abstract;
    procedure AddToPath(Path: TFlattenedPath); virtual; abstract;
    procedure Read(SL: TStrings; var Position: integer;
      Previous: TSVGPathElement); virtual;

    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TFlattenedPath); override;

    property StartX: TFloat read FStartX write FStartX;
    property StartY: TFloat read FStartY write FStartY;
    property StopX: TFloat read FStopX write FStopX;
    property StopY: TFloat read FStopY write FStopY;
  end;

  { TSVGPathMove }

  TSVGPathMove = class(TSVGPathElement)
  private
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TFlattenedPath); override;
    procedure Read(SL: TStrings; var Position: integer;
      Previous: TSVGPathElement); override;
  end;

  { TSVGPathLine }

  TSVGPathLine = class(TSVGPathElement)
  private
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TFlattenedPath); override;
    procedure Read(SL: TStrings; var Position: integer;
      Previous: TSVGPathElement); override;
  end;

  { TSVGPathCurve }

  TSVGPathCurve = class(TSVGPathElement)
  private
    FControl1X: TFloat;
    FControl1Y: TFloat;
    FControl2X: TFloat;
    FControl2Y: TFloat;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TFlattenedPath); override;
    procedure Read(SL: TStrings; var Position: integer;
      Previous: TSVGPathElement); override;

    property Control1X: TFloat read FControl1X write FControl1X;
    property Control1Y: TFloat read FControl1Y write FControl1Y;
    property Control2X: TFloat read FControl2X write FControl2X;
    property Control2Y: TFloat read FControl2Y write FControl2Y;
  end;

  { TSVGPathEllipticArc }

  TSVGPathEllipticArc = class(TSVGPathElement)
  private
    FRX: TFloat;
    FRY: TFloat;
    FXRot: TFloat;
    FLarge: integer;
    FSweep: integer;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TFlattenedPath); override;
    procedure Read(SL: TStrings; var Position: integer;
      Previous: TSVGPathElement); override;

    property RX: TFloat read FRX write FRX;
    property RY: TFloat read FRY write FRY;
    property XRot: TFloat read FXRot write FXRot;
    property Large: integer read FLarge write FLarge;
    property Sweep: integer read FSweep write FSweep;
  end;

  { TSVGPathClose }

  TSVGPathClose = class(TSVGPathElement)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TFlattenedPath); override;
    procedure Read(SL: TStrings; var Position: integer;
      Previous: TSVGPathElement); override;
  end;

implementation

uses
  Math, uSVGProc;

procedure minest(var a, b: TFloat);
var
  c: TFloat;
begin
  if a > b then
  begin
    c := b;
    b := a;
    a := c;
  end;
end;

procedure maxest(var a, b: TFloat);
var
  c: TFloat;
begin
  if b < a then
  begin
    c := b;
    b := a;
    a := c;
  end;
end;

procedure append(var dst: TArrayOfArrayOfFloatPoint; src: array of TFloat);
var
  i, l, First: integer;
begin
  l := Length(src) div 2;
  First := Length(dst);
  SetLength(dst, First + 1);
  SetLength(dst[First], l);


  for i := 0 to l - 1 do
  begin
    dst[First][i] := FloatPoint(src[i * 2], src[i * 2 + 1]);
  end;

end;

function bezier_arc(x1, y1, x2, y2: TFloat; start_angle: TFloat = 0;
  extent: TFloat = 90): TArrayOfArrayOfFloatPoint;
    {

    Compute a cubic Bezier approximation of an elliptical arc.

    (x1, y1) and (x2, y2) are the corners of the enclosing rectangle.  The
    coordinate system has coordinates that increase to the right and down.
    Angles, measured in degress, start with 0 to the right (the positive X axis)
    and increase counter-clockwise.  The arc extends from start_angle to
    start_angle+extent.  I.e. start_angle=0 and extent=180 yields an openside-down
    semi-circle.

    The resulting coordinates are of the form (x1,y1, x2,y2, x3,y3, x4,y4)
    such that the curve goes from (x1, y1) to (x4, y4) with (x2, y2) and
    (x3, y3) as their respective Bezier control points.

    Original code is svg_extras.py from "Enable" library.
    https://svn.enthought.com/enthought/browser/Enable/trunk/enthought/savage/svg/svg_extras.py?rev=21150

    Translated to pascal by x2nie at yahoo dot com
    31 December 2014

    }

var
  frag_angle, x_cen, y_cen: TFloat;
  rx, ry, half_angle, kappa, sign: TFloat;
  theta0, theta1, c0, c1, s0, s1, signed_kappa: TFloat;
  nfrag, i: integer;
  mi, ma: TFloatPoint;
begin
  SetLength(Result, 0);

  //x1,y1, x2,y2 = min(x1,x2), max(y1,y2), max(x1,x2), min(y1,y2)
  minest(x1, x2);
  minest(y2, y1);


  if abs(extent) <= 90 then
  begin
    frag_angle := extent;
    nfrag := 1;
  end
  else
  begin
    nfrag := ceil(abs(extent) / 90);
    if nfrag = 0 then
    begin
      //warnings.warn('Invalid value for extent: %r' % extent)
      Exit;
    end;
    frag_angle := extent / nfrag;
  end;

  x_cen := (x1 + x2) / 2;
  y_cen := (y1 + y2) / 2;
  rx := (x2 - x1) / 2;
  ry := (y2 - y1) / 2;
  half_angle := DegToRad(frag_angle) / 2;
  kappa := abs(4 / 3 * (1 - cos(half_angle)) / sin(half_angle));

  if frag_angle < 0 then
    sign := -1
  else
    sign := 1;

  //point_list = []


  for i := 0 to nfrag - 1 do
  begin
    theta0 := DegToRad(start_angle + i * frag_angle);
    theta1 := DegToRad(start_angle + (i + 1) * frag_angle);
    c0 := cos(theta0);
    c1 := cos(theta1);
    s0 := sin(theta0);
    s1 := sin(theta1);
    if frag_angle > 0 then
      signed_kappa := -kappa
    else
      signed_kappa := kappa;
    //point_list.append((
    append(Result, [x_cen + rx * c0,
      y_cen - ry * s0, x_cen +
      rx * (c0 + signed_kappa * s0), y_cen - ry *
      (s0 - signed_kappa * c0), x_cen + rx *
      (c1 - signed_kappa * s1), y_cen - ry *
      (s1 + signed_kappa * c1), x_cen + rx * c1,
      y_cen - ry * s1]
      //))
      );

  end;
  sign := 1;
  //return point_list
end;



function angle(x1, y1, x2, y2: TFloat): tfloat;
  //The angle in degrees between two vectors.
var
  sign, usign, num, den, ratio: TFloat;
begin
  sign := 1.0;
  usign := (x1 * y2 - y1 * x2);
  if usign < 0 then
    sign := -1.0;
  num := x1 * x2 + y1 * y2;
  den := hypot(x1, y1) * hypot(x2, y2);
  ratio := min(max(num / den, -1.0), 1.0);
  Result := sign * RadToDeg(ArcCos(ratio));
end;

function transform_from_local(xp, yp, cphi, sphi, mx, my: TFloat): TFloatPoint; overload;
  //Transform from the local frame to absolute space.
var
  x, y: TFloat;
begin
  x := xp * cphi - yp * sphi + mx;
  y := xp * sphi + yp * cphi + my;
  Result := FloatPoint(x, y);
end;

function transform_from_local(p: TFloatPoint; cphi, sphi, mx, my: TFloat): TFloatPoint;
  overload;
begin
  Result := transform_from_local(p.X, p.Y, cphi, sphi, mx, my);
end;

procedure elliptical_arc_to(path: TFlattenedPath; rx, ry, phi: TFloat;
  large_arc_flag, sweep_flag: boolean; x1, y1, x2, y2: TFloat);


    {Add an elliptical arc to the kiva CompiledPath by approximating it with
    Bezier curves or a line segment.

    Algorithm taken from the SVG 1.1 Implementation Notes:
        http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
    }
var
  rphi, cphi, sphi, dx, dy, x1p, y1p, lam, scale, num, den, cxp, cyp, mx, my, a: TFloat;
  dx2, dy2, dtheta, theta1: TFloat;
  arcs, args: TArrayOfFloatPoint;
  CARRAY, control_points: TArrayOfArrayOfFloatPoint;
  xp: TArrayOfFloatPoint;
  p1, p2, p3, p4: TFloatPoint;
  i: integer;
begin
  // Basic normalization.
  rx := abs(rx);
  ry := abs(ry);
  while phi > 360 do
    phi := phi - 360;
  while phi < -360 do
    phi := phi + 360;

  // Check for certain special cases.
  if (x1 = x2) and (y1 = y2) then
  begin
    // Omit the arc.
    // x1 and y1 can obviously remain the same for the next segment.
    //return []
    exit;
  end;
  if (rx = 0) or (ry = 0) then
  begin
    // Line segment.
    Path.LineTo(x2, y2);
    //return []
    Exit;
  end;



  rphi := DegToRad(phi);
  cphi := cos(rphi);
  sphi := sin(rphi);

  // Step 1: Rotate to the local coordinates.
  dx := 0.5 * (x1 - x2);
  dy := 0.5 * (y1 - y2);
  x1p := cphi * dx + sphi * dy;
  y1p := -sphi * dx + cphi * dy;
  // Ensure that rx and ry are large enough to have a unique solution.
  lam := Sqr(x1p / rx) + Sqr(y1p / ry);
  if lam > 1.0 then
  begin
    scale := sqrt(lam);
    rx := rx * scale;
    ry := ry * scale;
  end;

  // Step 2: Solve for the center in the local coordinates.
  num := max(Sqr(rx * ry) - Sqr(rx * y1p) - Sqr(ry * x1p), 0.0);
  den := (Sqr(rx * y1p) + Sqr(ry * x1p));
  a := sqrt(num / den);
  cxp := a * rx * y1p / ry;
  cyp := -a * ry * x1p / rx;
  if large_arc_flag = sweep_flag then
  begin
    cxp := -cxp;
    cyp := -cyp;
  end;

  // Step 3: Transform back.
  mx := 0.5 * (x1 + x2);
  my := 0.5 * (y1 + y2);

  // Step 4: Compute the start angle and the angular extent of the arc.
  // Note that theta1 is local to the phi-rotated coordinate space.
  dx := (x1p - cxp) / rx;
  dy := (y1p - cyp) / ry;
  dx2 := (-x1p - cxp) / rx;
  dy2 := (-y1p - cyp) / ry;
  theta1 := angle(1, 0, dx, dy);
  dtheta := angle(dx, dy, dx2, dy2);
  if (not sweep_flag) and (dtheta > 0) then
    dtheta := dtheta - 360
  else if sweep_flag and (dtheta < 0) then
    dtheta := dtheta + 360;



  // Step 5: Break it apart into Bezier arcs.
  SetLength(arcs, 0);
  control_points := bezier_arc(cxp - rx, cyp - ry, cxp + rx, cyp + ry, theta1, dtheta);

  SetLength(CARRAY, 0);

  //for x1p,y1p, x2p,y2p, x3p,y3p, x4p,y4p in control_points:
  for i := 0 to Length(control_points) - 1 do
  begin
    xp := control_points[i];
    // Transform them back to asbolute space.

    p2 := transform_from_local(xp[1], cphi, sphi, mx, my);
    p3 := transform_from_local(xp[2], cphi, sphi, mx, my);
    p4 := transform_from_local(xp[3], cphi, sphi, mx, my);

    //arcs.append(args)
    Path.CurveTo(p2, p3, p4);
    append(CARRAY, [p2.X, p2.Y, p3.X, p3.Y, p4.X, p4.Y]);
  end;
end;


{TSVGPathElement }

procedure TSVGPathElement.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathElement then
  begin
    FStartX := TSVGPathElement(SVG).FStartX;
    FStartY := TSVGPathElement(SVG).FStartY;
    FStopX := TSVGPathElement(SVG).FStopX;
    FStopY := TSVGPathElement(SVG).FStopY;
  end;
end;

function TSVGPathElement.New(AParent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;

procedure TSVGPathElement.Read(SL: TStrings; var Position: integer;
  Previous: TSVGPathElement);
begin
  if Assigned(Previous) then
  begin
    FStartX := Previous.FStopX;
    FStartY := Previous.FStopY;
  end;
end;

procedure TSVGPathElement.PaintToGraphics(Graphics: TBitmap32);
begin
end;

procedure TSVGPathElement.PaintToPath(Path: TFlattenedPath);
begin
end;

{ TSVGPathMove }

function TSVGPathMove.GetBounds: TFRect;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

function TSVGPathMove.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathMove.Create(AParent);
end;

procedure TSVGPathMove.AddToPath(Path: TFlattenedPath);
begin
  Path.EndPath(True);
  Path.MoveTo(FStopX, FStopY);
end;

procedure TSVGPathMove.Read(SL: TStrings; var Position: integer;
  Previous: TSVGPathElement);
begin
  inherited;
  if not TryStrToTFloat(SL[Position + 1], FStopX) then
    FStopX := 0;
  if not TryStrToTFloat(SL[Position + 2], FStopY) then
    FStopY := 0;

  if SL[Position] = 'm' then
  begin
    FStopX := FStartX + FStopX;
    FStopY := FStartY + FStopY;
  end;

  Inc(Position, 2);
end;


{ TSVGPathLine }

function TSVGPathLine.GetBounds: TFRect;
begin
  Result.Left := Min(FStartX, FStopX);
  Result.Top := Min(FStartY, FStopY);
  Result.Width := Abs(FStartX - FStopX);
  Result.Height := Abs(FStartY - FStopY);
end;

function TSVGPathLine.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathLine.Create(AParent);
end;

procedure TSVGPathLine.AddToPath(Path: TFlattenedPath);
begin
  Path.LineTo(FStopX, FStopY);
end;

procedure TSVGPathLine.Read(SL: TStrings; var Position: integer;
  Previous: TSVGPathElement);
var
  Command: ansistring;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'L') or (Command = 'l') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopX) then
      FStopX := 0;
    if not TryStrToTFloat(SL[Position + 2], FStopY) then
      FStopY := 0;

    if SL[Position] = 'l' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;

    Inc(Position, 2);
  end;

  if (Command = 'H') or (Command = 'h') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopX) then
      FStopX := 0;

    if Command = 'h' then
      FStopX := FStartX + FStopX;
    FStopY := FStartY;
    Inc(Position);
  end;


  if (Command = 'V') or (Command = 'v') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopY) then
      FStopY := 0;

    if Command = 'v' then
      FStopY := FStartY + FStopY;
    FStopX := FStartX;
    Inc(Position);
  end;
end;


{ TSVGPathCurve }

procedure TSVGPathCurve.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathCurve then
  begin
    FControl1X := TSVGPathCurve(SVG).FControl1X;
    FControl1Y := TSVGPathCurve(SVG).FControl1Y;
    FControl2X := TSVGPathCurve(SVG).FControl2X;
    FControl2Y := TSVGPathCurve(SVG).FControl2Y;
  end;
end;

function TSVGPathCurve.GetBounds: TFRect;
var
  Right, Bottom: TFloat;
begin
  Result.Left := Min(FStartX, Min(FStopX, Min(FControl1X, FControl2X)));
  Result.Top := Min(FStartY, Min(FStopY, Min(FControl1Y, FControl2Y)));

  Right := Max(FStartX, Max(FStopX, Max(FControl1X, FControl2X)));
  Bottom := Max(FStartY, Max(FStopY, Max(FControl1Y, FControl2Y)));
  Result.Width := Right - Result.Left;
  Result.Height := Bottom - Result.Top;
end;

function TSVGPathCurve.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathCurve.Create(AParent);
end;

procedure TSVGPathCurve.AddToPath(Path: TFlattenedPath);
begin
  Path.CurveTo(FControl1X, FControl1Y, FControl2X, FControl2Y, FStopX, FStopY);
end;

procedure TSVGPathCurve.Read(SL: TStrings; var Position: integer;
  Previous: TSVGPathElement);
var
  Command: ansistring;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'C') or (Command = 'c') then
  begin
    TryStrToTFloat(SL[Position + 1], FControl1X);
    TryStrToTFloat(SL[Position + 2], FControl1Y);
    TryStrToTFloat(SL[Position + 3], FControl2X);
    TryStrToTFloat(SL[Position + 4], FControl2Y);
    TryStrToTFloat(SL[Position + 5], FStopX);
    TryStrToTFloat(SL[Position + 6], FStopY);
    Inc(Position, 6);

    if Command = 'c' then
    begin
      FControl1X := FStartX + FControl1X;
      FControl1Y := FStartY + FControl1Y;
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'S') or (Command = 's') then
  begin
    FControl1X := FStartX;
    FControl1Y := FStartY;
    TryStrToTFloat(SL[Position + 1], FControl2X);
    TryStrToTFloat(SL[Position + 2], FControl2Y);
    TryStrToTFloat(SL[Position + 3], FStopX);
    TryStrToTFloat(SL[Position + 4], FStopY);
    Inc(Position, 4);

    if Previous is TSVGPathCurve then
    begin
      FControl1X := FStartX + (FStartX - TSVGPathCurve(Previous).FControl2X);
      FControl1Y := FStartY + (FStartY - TSVGPathCurve(Previous).FControl2Y);
    end;

    if Command = 's' then
    begin
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'Q') or (Command = 'q') then
  begin
    TryStrToTFloat(SL[Position + 1], FControl1X);
    TryStrToTFloat(SL[Position + 2], FControl1Y);
    TryStrToTFloat(SL[Position + 3], FStopX);
    TryStrToTFloat(SL[Position + 4], FStopY);
    FControl2X := FControl1X;
    FControl2Y := FControl1Y;
    Inc(Position, 4);

    if Command = 'q' then
    begin
      FControl1X := FStartX + FControl1X;
      FControl1Y := FStartY + FControl1Y;
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'T') or (Command = 't') then
  begin
    FControl1X := FStartX;
    FControl1Y := FStartY;
    TryStrToTFloat(SL[Position + 1], FStopX);
    TryStrToTFloat(SL[Position + 2], FStopY);
    Inc(Position, 2);

    if Previous is TSVGPathCurve then
    begin
      FControl1X := FStartX + (FStartX - TSVGPathCurve(Previous).FControl2X);
      FControl1Y := FStartY + (FStartY - TSVGPathCurve(Previous).FControl2Y);
    end;

    FControl2X := FControl1X;
    FControl2Y := FControl1Y;

    if Command = 't' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;
end;


{ TSVGPathEllipticArc }

procedure TSVGPathEllipticArc.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathEllipticArc then
  begin
    FRX := TSVGPathEllipticArc(SVG).FRX;
    FRY := TSVGPathEllipticArc(SVG).FRY;
    FXRot := TSVGPathEllipticArc(SVG).FXRot;
    FLarge := TSVGPathEllipticArc(SVG).FLarge;
    FSweep := TSVGPathEllipticArc(SVG).FSweep;
  end;
end;

function TSVGPathEllipticArc.GetBounds: TFRect;
begin
  Result.Left := Min(FStartX, FStopX);
  Result.Top := Min(FStartY, FStopY);
  Result.Width := Abs(FStartX - FStopX);
  Result.Height := Abs(FStartY - FStopY);
end;

function TSVGPathEllipticArc.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathEllipticArc.Create(AParent);
end;

procedure TSVGPathEllipticArc.AddToPath(Path: TFlattenedPath);
//algorithm shall taken from here http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
var
  R: TFloatRect;
  X1, Y1, X2, Y2: TFloat;
  Xl, Yl: TFloat;
  Phi: TFloat;
  Factor: TFloat;
  CXl, CYl: TFloat;
  CenterX, CenterY: TFloat;
  StartAngle, SweepAngle: TFloat;
  A, B, C, D: TFloat;
begin
  elliptical_arc_to(Path, RX, RY, XRot, Large <> 0, Sweep <> 0,
    FStartX, FStartY, FStopX, FStopY);
  Exit;

  if (FStartX = FStopX) and (FStartY = FStopY) then
    Exit;

  //F.6.6 Ensure radii are non-zero
  if (FRX = 0) or (FRY = 0) then
  begin
    Path.LineTo(FStopX, FStopY);
    Exit;
  end;

  X1 := FStartX;
  Y1 := FStartY;
  X2 := FStopX;
  Y2 := FStopY;

  Phi := DegToRad(-FXRot);

  Xl := (X1 - X2) / 2;
  Yl := (Y1 - Y2) / 2;

  Xl := Cos(Phi) * Xl + Sin(Phi) * Yl;
  Yl := -Sin(Phi) * Xl + Cos(Phi) * Yl;

  A := Sqr(Xl) / Sqr(FRX) + Sqr(Yl) / Sqr(FRY);
  if A > 1 then
  begin
    A := Sqrt(A);
    FRX := A * FRX;
    FRY := A * FRY;
  end;


  A := Sqr(FRX) * Sqr(FRY) - Sqr(FRX) * Sqr(Yl) - Sqr(FRY) * Sqr(Xl);
  B := Sqr(FRX) * Sqr(Yl) + Sqr(FRY) * Sqr(Xl);
  A := A / B;
  if A > 0 then
    Factor := Sqrt(A)
  else
    Factor := 1;

  if FLarge = FSweep then
    Factor := -Factor;

  CXl := Factor * (FRX * Yl) / FRY;
  CYl := Factor * -(FRY * Xl) / FRX;

  CenterX := Cos(Phi) * CXl - Sin(Phi) * CYl + (X1 + X2) / 2;
  CenterY := Sin(Phi) * CXl + Cos(Phi) * CYl + (Y1 + Y2) / 2;

  R.Left := CenterX - FRX;
  R.Top := CenterY - FRY;
  R.Right := FRX * 2;
  R.Bottom := FRY * 2;

  A := 1;
  B := 0;
  C := (Xl - CXl) / FRX;
  D := (Yl - CYl) / FRY;

  StartAngle := (A * C + B * D) / (Sqrt(Sqr(A) + Sqr(B)) * Sqrt(Sqr(C) + Sqr(D)));

  StartAngle := ArcCos(StartAngle);
  StartAngle := RadToDeg(StartAngle);

  A := C;
  B := D;
  C := (-Xl - CXl) / FRX;
  D := (-Yl - CYl) / FRY;
  SweepAngle := (A * C + B * D) / (Sqrt(Sqr(A) + Sqr(B)) * Sqrt(Sqr(C) + Sqr(D)));
  SweepAngle := ArcCos(SweepAngle);
  SweepAngle := RadToDeg(SweepAngle);

  if FSweep = 0 then
    SweepAngle := -SweepAngle;

  if FLarge = 0 then
    StartAngle := -StartAngle;

  if (FLarge = 1) then
  begin
    if Sweep = 0 then
      StartAngle := -StartAngle;
    if SweepAngle < 0 then
      SweepAngle := -360 - SweepAngle
    else
      SweepAngle := 360 - SweepAngle;
  end;

  Path.Arc(FloatPoint(CenterX, CenterY), StartAngle, SweepAngle, FRX);
end;

procedure TSVGPathEllipticArc.Read(SL: TStrings; var Position: integer;
  Previous: TSVGPathElement);
var
  Command: ansistring;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'A') or (Command = 'a') then
  begin
    TryStrToTFloat(SL[Position + 1], FRX);
    TryStrToTFloat(SL[Position + 2], FRY);
    TryStrToTFloat(SL[Position + 3], FXRot);
    TryStrToInt(SL[Position + 4], FLarge);
    TryStrToInt(SL[Position + 5], FSweep);
    TryStrToTFloat(SL[Position + 6], FStopX);
    TryStrToTFloat(SL[Position + 7], FStopY);
    Inc(Position, 7);

    FRX := Abs(FRX);
    FRY := Abs(FRY);

    if FLarge <> 0 then
      FLarge := 1;

    if FSweep <> 0 then
      FSweep := 1;

    if Command = 'a' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;
end;

{ TSVGPathClose }

function TSVGPathClose.GetBounds: TFRect;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

function TSVGPathClose.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathClose.Create(AParent);
end;

procedure TSVGPathClose.Read(SL: TStrings; var Position: integer;
  Previous: TSVGPathElement);
begin
  FStartX := Previous.FStopX;
  FStartY := Previous.FStopY;
  FStopX := FStartX;
  FStopY := FStartY;
end;

procedure TSVGPathClose.AddToPath(Path: TFlattenedPath);
begin
  Path.EndPath(True);
end;

end.
