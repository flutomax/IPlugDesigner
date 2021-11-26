{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uStandartIControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImgList, Graphics, FPImage, uCommon, uIObject, uGraphics;

type

  { TIRect }

  TIRect = class(TIBaseRect)
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    class function GetObjectName: string; override;
  published
    property Pen;
    property Brush;
    property CornerRadius;
  end;

  { TIEllipse }

  TIEllipse = class(TIBaseRect)
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    class function GetObjectName: string; override;
  published
    property Pen;
    property Brush;
  end;

  { TILine }

  TILine = class(TIBaseLine)
  private
    FBasePoints: array[0..1] of TIFloatPoint;
  protected
    function GetBasePointsCount: integer; override;
    function GetBasePoint(AIndex: integer): TIFloatPoint; override;
    procedure SetBasePoint(AIndex: integer; Value: TIFloatPoint); override;
    function ConstructPoint(AIndex: integer; Pos: TIFloatPoint): integer; override;
    function StopConstruct(AIndex: integer): integer; override;
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject; X1, Y1, X2, Y2: double); reintroduce;
    class function GetObjectName: string; override;
  published
    property Pen;
  end;


  { TIPolyline }

  TIPolyline = class(TIBasePoly)
  protected
    function GetMinPointsCount: integer; override;
    procedure Paint(Canvas: TICanvas); override;
  public
    class function GetObjectName: string; override;
  published
    property Pen;
  end;

  { TIPolygon }

  TIPolygon = class(TIBasePoly)
  protected
    function SideCode: cardinal; override;
    function GetMinPointsCount: integer; override;
    function HitTest(Params: PIHitTestParams): cardinal; override;
    procedure Paint(Canvas: TICanvas); override;
  public
    class function GetObjectName: string; override;
  published
    property Pen;
    property Brush;
  end;


  { TIText }

  TIText = class(TIBaseText)
  public
    class function GetObjectName: string; override;
  published
    property Text;
  end;

  { TIImage }

  TIImage = class(TIBaseRect)
  private
    fImageName: TImageName;
    procedure SetImageName(AValue: TImageName);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    class function GetObjectName: string; override;
  published
    property ImageName: TImageName read fImageName write SetImageName;
  end;

  { TICustomTextControl }

  TICustomTextControl = class(TIBaseText, ICodeGenerate)
  private
    function GetBGColor: TIColor;
    procedure SetBGColor(AValue: TIColor);
  protected
    procedure Paint(Canvas: TICanvas); override;
    function GetCodeStr: string; virtual;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
  published
    property BGColor: TIColor read GetBGColor write SetBGColor;
  end;

  { TITextControl }

  TITextControl = class(TICustomTextControl)
  public
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property Text;
  end;

  { TIURLControl }

  TIURLControl = class(TITextControl)
  private
    fURL: string;
    fCLColor: TIColor;
    fMOColor: TIColor;
    procedure SetCLColor(AValue: TIColor);
    procedure SetMOColor(AValue: TIColor);
    procedure SetURL(AValue: string);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property URL: string read fURL write SetURL;
    property MOColor: TIColor read fMOColor write SetMOColor;
    property CLColor: TIColor read fCLColor write SetCLColor;
  end;

  { TIEditableTextControl }

  TIEditableTextControl = class(TITextControl)
  public
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  end;

  { TICaptionControl }

  TICaptionControl = class(TITextControl)
  private
    fTriangleColor: TIColor;
    fTypeEnum: boolean;
    fShowParamLabel: boolean;
    procedure SetShowParamLabel(AValue: boolean);
    procedure SetTypeEnum(AValue: boolean);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property TypeEnum: boolean read fTypeEnum write SetTypeEnum;
    property ShowParamLabel: boolean read fShowParamLabel write SetShowParamLabel;
    property ParamIdx;
  end;

  { TITextToggleControl }

  TITextToggleControl = class(TICustomTextControl)
  private
    fOnText: string;
    fOffText: string;
    procedure SetOffText(AValue: string);
    procedure SetOnText(AValue: string);
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property OffText: string read fOffText write SetOffText;
    property OnText: string read fOnText write SetOnText;
    property ParamIdx;
  end;

  { TIPanelControl }

  TIPanelControl = class(TISpan)
  private
    function GetColor: TIColor;
    function GetDrawFrame: Boolean;
    procedure SetColor(AValue: TIColor);
    procedure SetDrawFrame(AValue: Boolean);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property Color: TIColor read GetColor write SetColor;
    property DrawFrame: Boolean read GetDrawFrame write SetDrawFrame;
  end;

  { TICornerResizerControl }

  TICornerResizerControl = class(TISpan)
  private
    fSize: double;
    procedure SetSize(AValue: double);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property Size: double read fSize write SetSize;
  end;

implementation

uses
  SysConst, Math, uStorage, uIDocument;



{ TIRect }

procedure TIRect.Paint(Canvas: TICanvas);
var
  r: TIFloatRect;
begin
  r := GetRect;
  if CornerRadius <> 0.0 then
    Canvas.DrawRoundedRect(r.Left, r.Top, r.Right, r.Bottom, CornerRadius)
  else
    Canvas.DrawRect(r);
end;

class function TIRect.GetObjectName: string;
begin
  Result := 'Rect';
end;

{ TIEllipse }

procedure TIEllipse.Paint(Canvas: TICanvas);
var
  p1, p2: TIFloatPoint;
begin
  p1 := RelativeToAbsolute(BasePoints[0]);
  p2 := RelativeToAbsolute(BasePoints[1]);
  Canvas.DrawEllipse(p1.X, p1.Y, p2.X, p2.Y);
end;

class function TIEllipse.GetObjectName: string;
begin
  Result := 'Ellipse';
end;

{ TIText }

class function TIText.GetObjectName: string;
begin
  Result := 'Text';
end;

{ TILine }

constructor TILine.Create(AOwner: TIObject; ADocument: TObject; X1, Y1, X2,
  Y2: double);
begin
  inherited Create(AOwner, ADocument);
  FBasePoints[0] := TIFloatPoint.Create(X1, Y1);
  FBasePoints[1] := TIFloatPoint.Create(X2, Y2);
end;

function TILine.GetBasePointsCount: integer;
begin
  Result := 2;
end;

function TILine.GetBasePoint(AIndex: integer): TIFloatPoint;
begin
  Result := FBasePoints[AIndex];
end;

procedure TILine.SetBasePoint(AIndex: integer; Value: TIFloatPoint);
begin
  DoChanging(Self);
  FBasePoints[AIndex] := Value;
  DoChanged(Self);
end;

function TILine.ConstructPoint(AIndex: integer; Pos: TIFloatPoint): integer;
begin
  Vertexes[AIndex] := Pos;
  if AIndex = 0 then
    Vertexes[AIndex + 1] := Pos;
  Result := AIndex;
end;

function TILine.StopConstruct(AIndex: integer): integer;
begin
  Result := 1;
end;

procedure TILine.Paint(Canvas: TICanvas);
var
  p1, p2: TIFloatPoint;
begin
  p1 := RelativeToAbsolute(BasePoints[0]);
  p2 := RelativeToAbsolute(BasePoints[1]);
  Canvas.DrawLine(p1.X, p1.Y, p2.X, p2.Y);
end;

class function TILine.GetObjectName: string;
begin
  Result := 'Line';
end;


{ TIPolyline }

class function TIPolyline.GetObjectName: string;
begin
  Result := 'Polyline';
end;

function TIPolyline.GetMinPointsCount: integer;
begin
  Result := 2;
end;

procedure TIPolyline.Paint(Canvas: TICanvas);
var
  i: integer;
  Pts: array of TIFloatPoint;
begin
  SetLength(Pts, BasePointsCount);
  for i := 0 to BasePointsCount - 1 do
    Pts[i] := RelativeToAbsolute(BasePoints[i]);
  Canvas.DrawPolyline(@Pts[0], BasePointsCount);
end;


{ TIPolygon }

function TIPolygon.SideCode: cardinal;
begin
  Result := HT_SIDE;
end;

function TIPolygon.GetMinPointsCount: integer;
begin
  Result := 3;
end;

function TIPolygon.HitTest(Params: PIHitTestParams): cardinal;
var
  i: integer;
  D, sX1, sY1, sX2, sY2: double;
  Pts: array of TIFloatPoint;
begin
  Result := inherited HitTest(Params);
  if Result = HT_OUT then
  begin
    Params^.TranslateIntf.LogToScreen(Vertexes[0].X, Vertexes[0].Y, sX1, sY1);
    Params^.TranslateIntf.LogToScreen(Vertexes[VertexesCount - 1].X,
      Vertexes[VertexesCount - 1].Y, sX2, sY2);
    D := LineDistance(Params^.XPos, Params^.YPos, sX1, sY1, sX2, sY2);
    if D <= Params^.Tolerance then
    begin
      Result := SideCode + cardinal(VertexesCount);
      Exit;
    end;
    SetLength(Pts, VertexesCount);
    for i := 0 to VertexesCount - 1 do
    begin
      Params^.TranslateIntf.LogToScreen(Vertexes[i].X, Vertexes[i].Y, sX1, sY1);
      Pts[i] := TIFloatPoint.Create(sX1, sY1);
    end;
    if PointInPolygon(TIFloatPoint.Create(Params^.XPos, Params^.YPos), Pts) then
      Result := HT_IN;
  end;
end;

procedure TIPolygon.Paint(Canvas: TICanvas);
var
  i: integer;
  Pts: array of TIFloatPoint;
begin
  SetLength(Pts, BasePointsCount);
  for i := 0 to BasePointsCount - 1 do
    Pts[i] := RelativeToAbsolute(BasePoints[i]);
  Canvas.DrawPolygon(@Pts[0], BasePointsCount);
end;

class function TIPolygon.GetObjectName: string;
begin
  Result := 'Polygon';
end;


{ TIImage }

class function TIImage.GetObjectName: string;
begin
  Result := 'Image';
end;

procedure TIImage.SetImageName(AValue: TImageName);
begin
  if SameText(fImageName, AValue) then
    Exit;
  DoChanging(Self);
  fImageName := AValue;
  DoChanged(Self);
end;

procedure TIImage.Paint(Canvas: TICanvas);
var
  img: TIEmbededImage;
begin
  img := TIDocument(Document).ImageCache.ImageByName[fImageName] as TIEmbededImage;
  if Assigned(img) then
  begin
    Canvas.DrawImage(img, GetRect);
  end;
end;


{ TICustomTextControl }

constructor TICustomTextControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  Pen.Width := 0;
  Brush.Color := iclTransparent;
end;

function TICustomTextControl.GetBGColor: TIColor;
begin
  result := Brush.Color;
end;

procedure TICustomTextControl.SetBGColor(AValue: TIColor);
begin
  Brush.Color := AValue;
end;

procedure TICustomTextControl.Paint(Canvas: TICanvas);
var
  r: TIFloatRect;
begin
  r := GetRect;
  Canvas.DrawRect(r);
  Canvas.DrawText(Text, r);
end;

function TICustomTextControl.GetCodeStr: string;
begin
  result := '';
end;

{ TITextControl }

class function TITextControl.GetObjectName: string;
begin
  Result:= 'ITextControl';
end;

function TITextControl.GetCodeStr: string;
begin
  result := Format('%s(%s, "%s", %s, %s)', [GetObjectName, IRect(GetRect), Text,
    ITextStr(Font), IColorStr(Brush.Color)]);
end;

{ TIURLControl }

constructor TIURLControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fMOColor := iclWhite;
  fCLColor := iclBlue;
end;

class function TIURLControl.GetObjectName: string;
begin
  Result := 'IURLControl';
end;

function TIURLControl.GetCodeStr: string;
begin
  result := Format('%s(%s, "%s", "%s", %s, %s, %s, %s)', [GetObjectName,
    IRect(GetRect), Text, URL, ITextStr(Font),
    IColorStr(Brush.Color), IColorStr(MOColor),
    IColorStr(CLColor)]);
end;

procedure TIURLControl.SetCLColor(AValue: TIColor);
begin
  if fCLColor = AValue then
    Exit;
  DoChanging(Self);
  fCLColor := AValue;
  DoChanged(Self);
end;

procedure TIURLControl.SetMOColor(AValue: TIColor);
begin
  if fMOColor = AValue then
    Exit;
  DoChanging(Self);
  fMOColor := AValue;
  DoChanged(Self);
end;

procedure TIURLControl.SetURL(AValue: string);
begin
  if fURL = AValue then
    Exit;
  DoChanging(Self);
  fURL := AValue;
  DoChanged(Self);
end;

procedure TIURLControl.Paint(Canvas: TICanvas);
var
  r: TIFloatRect;
begin
  r := GetRect;
  Canvas.DrawRect(r);
  Canvas.DrawText(Text, r, true);
end;

{ TIEditableTextControl }

class function TIEditableTextControl.GetObjectName: string;
begin
  Result := 'IEditableTextControl';
end;

function TIEditableTextControl.GetCodeStr: string;
begin
  result := Format('%s(%s, "%s", %s)', [GetObjectName,
    IRect(GetRect), Text, ITextStr(Font)]);
end;


{ TICaptionControl }

constructor TICaptionControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fTriangleColor := iclBlack;
  fShowParamLabel := true;
end;

procedure TICaptionControl.SetTypeEnum(AValue: boolean);
begin
  if fTypeEnum = AValue then
    Exit;
  DoChanging(Self);
  fTypeEnum := AValue;
  DoChanged(Self);
end;

procedure TICaptionControl.SetShowParamLabel(AValue: boolean);
begin
  if fShowParamLabel = AValue then
    Exit;
  DoChanging(Self);
  fShowParamLabel := AValue;
  DoChanged(Self);
end;

procedure TICaptionControl.Paint(Canvas: TICanvas);
var
  r1, r2: TIFloatRect;
  OldColor: TIColor;
  cx, cy: double;
  a: array[0..2] of TIFloatPoint;
begin
  r1 := GetRect;
  Canvas.DrawRect(r1);
  if fTypeEnum then
  begin
    r2 := r1;
    r2.Left := r1.Right - (r1.Right - r1.Left) * 0.2;
    r1.Right := r2.Left;
    cx := (r2.Left + r2.Right) * 0.5;
    cy := (r2.Top + r2.Bottom) * 0.5;
    a[0] := TIFloatPoint.Create(cx - 4, cy - 2.5);
    a[1] := TIFloatPoint.Create(cx + 4, cy - 2.5);
    a[2] := TIFloatPoint.Create(cx, cy + 2.5);
    Canvas.PenWidth := 0;
    OldColor := Canvas.BrushColor;
    Canvas.BrushColor := fTriangleColor;
    Canvas.DrawPolygon(@a[0], 3);
    Canvas.BrushColor := OldColor;
  end;
  Canvas.DrawText(Text, r1);
end;

class function TICaptionControl.GetObjectName: string;
begin
  Result := 'ICaptionControl';
end;

function TICaptionControl.GetCodeStr: string;
begin
  result := Format('%s(%s, %s, %s, %s, %s)', [GetObjectName, IRect(GetRect), ParamIdx,
  ITextStr(Font), IColorStr(Brush.Color), BooleanToStr(ShowParamLabel)]);
end;

{ TITextToggleControl }

constructor TITextToggleControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fOnText := 'ON';
  fOffText:= 'OFF';
  Text := fOffText;
end;

class function TITextToggleControl.GetObjectName: string;
begin
  Result := 'ITextToggleControl';
end;

function TITextToggleControl.GetCodeStr: string;
begin
  result := Format('%s(%s, %s, "%s", "%s", %s, %s)', [GetObjectName, IRect(GetRect), ParamIdx,
  fOffText, fOnText, ITextStr(Font), IColorStr(Brush.Color)]);
end;

procedure TITextToggleControl.SetOffText(AValue: string);
begin
  if fOffText = AValue then
    Exit;
  DoChanging(Self);
  fOffText := AValue;
  DoChanged(Self);
end;

procedure TITextToggleControl.SetOnText(AValue: string);
begin
  if fOnText = AValue then
    Exit;
  DoChanging(Self);
  fOnText := AValue;
  DoChanged(Self);
end;


{ TIPanelControl }

constructor TIPanelControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  Pen.Color := iclLightGray;
  Pen.Width := 0;
  SetBounds(0, 0, 100, 100);
end;

class function TIPanelControl.GetObjectName: string;
begin
  Result := 'IPanelControl';
end;

function TIPanelControl.GetColor: TIColor;
begin
  result := Brush.Color;
end;

procedure TIPanelControl.SetColor(AValue: TIColor);
begin
  Brush.Color := AValue;
end;

function TIPanelControl.GetDrawFrame: Boolean;
begin
  result := Pen.Width > 0;
end;

procedure TIPanelControl.SetDrawFrame(AValue: Boolean);
begin
  Pen.Width := Ord(AValue);
end;

procedure TIPanelControl.Paint(Canvas: TICanvas);
begin
  Canvas.DrawRect(GetRect);
end;

function TIPanelControl.GetCodeStr: string;
begin
  result := Format('%s(%s, %s, %s)', [GetObjectName, IRect(GetRect),
    IColorStr(Color), BooleanToStr(DrawFrame)]);
end;

{ TICornerResizerControl }

constructor TICornerResizerControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fSize := 15;
  SetBounds(0, 0, 25, 25);
end;

class function TICornerResizerControl.GetObjectName: string;
begin
  Result := 'ICornerResizerControl';
end;

procedure TICornerResizerControl.SetSize(AValue: double);
begin
  if fSize = AValue then
    Exit;
  DoChanging(Self);
  fSize := AValue;
  DoChanged(Self);
end;

procedure TICornerResizerControl.Paint(Canvas: TICanvas);
var
  r: TIFloatRect;
  a: array[0..2] of TIFloatPoint;
begin
  r := GetRect;
  a[0].X := r.Right;
  a[0].Y := r.Bottom - fSize;
  a[1].X := r.Right;
  a[1].Y := r.Bottom;
  a[2].X := r.Right - fSize;
  a[2].Y := r.Bottom;
  Canvas.PenWidth := 0;
  Canvas.BrushColor := iclTranslucent;
  Canvas.DrawPolygon(@a[0], 3);
end;

function TICornerResizerControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s)', [GetObjectName, IRect(GetRect),
    DoubleToStr(Size)]);
end;

end.

