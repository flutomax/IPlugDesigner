{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uSVGControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImgList, Graphics, GR32, FPImage, agg_fpimage,
  uCommon, uIObject, uGraphics, uStorage, uSVGObjects;

type

  { TISVGObject }

  TISVGObject = class(TINonRefInterfacedObject)
  private
    fBuffer: TAggFPImage;
    fDocument: TObject;
    fAngle: double;
    fSVGName: TSVGName;
    function GetHeight: double;
    function GetIsEmpty: boolean;
    function GetWidth: double;
  public
    constructor Create(ADocument: TObject);
    destructor Destroy; override;
    procedure RenderSVG(const AWidth, AHeight: double);
    procedure Paint(Canvas: TICanvas; const ARect: TIFloatRect);
    property SVGName: TSVGName read fSVGName write fSVGName;
    property Width: double read GetWidth;
    property Height: double read GetHeight;
    property IsEmpty: boolean read GetIsEmpty;
  end;

  { TISVGControlBase }

  TISVGControlBase = class(TISpan)
  private
    fValue: double;
    fActionFunction: string;
    procedure SetActionFunction(AValue: string);
  protected
    fRenderNeeded: boolean;
    procedure Resize; override;
    procedure DoChanged(AValue: TIObject); override;
    procedure MakeRects(const ARect: TIFloatRect); virtual;
    procedure DrawSVG(Canvas: TICanvas; svg: TISVGObject;
      rect: PIFloatRect = nil); virtual;
    procedure SetValue(AValue: double); dynamic;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    property ActionFunction: string read fActionFunction write SetActionFunction;
    property Value: double read fValue write SetValue;
  end;

  { TISVGKnobControl }

  TISVGKnobControl = class(TISVGControlBase)
  private
    fSVG: TISVGObject;
    fStartAngle: double;
    fEndAngle: double;
    function GetSVGName: TSVGName;
    procedure SetSVGName(AValue: TSVGName);
  protected
    procedure Paint(Canvas: TICanvas); override;
    procedure DrawSVG(Canvas: TICanvas; svg: TISVGObject;
      rect: PIFloatRect = nil); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ParamIdx;
    property Value;
    property SVGName: TSVGName read GetSVGName write SetSVGName;
  end;

  { TISVGButtonControl }

  TISVGButtonControl = class(TISVGControlBase)
  private
    fOffSVG: TISVGObject;
    fOnSVG: TISVGObject;
    function GetOffSVGName: TSVGName;
    function GetOnSVGName: TSVGName;
    procedure SetOffSVGName(AValue: TSVGName);
    procedure SetOnSVGName(AValue: TSVGName);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property Value;
    property ActionFunction;
    property OffSVGName: TSVGName read GetOffSVGName write SetOffSVGName;
    property OnSVGName: TSVGName read GetOnSVGName write SetOnSVGName;
  end;

  { TISVGSwitchControl }

  TISVGSwitchControl = class(TISVGControlBase)
  private
    fSVGs: TStringList;
    function GetSVGs: TSVGNames;
    function GetSelectedIdx: integer;
    procedure SVGsChange;
    procedure SetSVGs(AValue: TSVGNames);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ParamIdx;
    property ActionFunction;
    property Value;
    property SVGs: TSVGNames read GetSVGs write SetSVGs;
  end;

  { TISVGSliderControl }

  TISVGSliderControl = class(TISVGControlBase)
  private
    fDirection: TDirection;
    fGearing: string;
    fHandleSVG: TISVGObject;
    fTrackSVG: TISVGObject;
    fTrackBounds: TIFloatRect;
    fTrackSVGBounds: TIFloatRect;
    fHandleBoundsAtMax: TIFloatRect;
    function GetHandleSVGName: TSVGName;
    function GetTrackSVGName: TSVGName;
    function GetHandleBounds(AValue: double): TIFloatRect;
    procedure SetDirection(AValue: TDirection);
    procedure SetGearing(AValue: string);
    procedure SetHandleSVGName(AValue: TSVGName);
    procedure SetTrackSVGName(AValue: TSVGName);
  protected
    procedure Paint(Canvas: TICanvas); override;
    procedure Resize; override;
    procedure SetValue(AValue: double); override;
    procedure DrawSVG(Canvas: TICanvas; svg: TISVGObject;
      rect: PIFloatRect = nil); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ParamIdx;
    property Value;
    property Gearing: string read fGearing write SetGearing;
    property Direction: TDirection read fDirection write SetDirection;
    property HandleSVGName: TSVGName read GetHandleSVGName write SetHandleSVGName;
    property TrackSVGName: TSVGName read GetTrackSVGName write SetTrackSVGName;
  end;

implementation

uses
  Math, TypInfo, uIDocument;

function GetSVGClass(AControl: TISVGControlBase; const SvgName: string): string;
  overload;
var
  img: TIEmbededGraphics;
begin
  img := TIDocument(AControl.Document).SVGCache.ImageByName[SvgName];
  if Assigned(img) then
    Result := img.ImageName
  else
    Result := 'nullptr';
end;

function GetSVGClass(AControl: TISVGControlBase; SvgNames: TStrings): string; overload;
var
  img: TIEmbededGraphics;
  i: integer;
begin
  Result := '';
  for i := 0 to SvgNames.Count - 1 do
  begin
    img := TIDocument(AControl.Document).SVGCache.ImageByName[SvgNames[i]];
    if i = SvgNames.Count - 1 then
    begin
      if Assigned(img) then
        Result += img.ImageName
      else
        Result += 'nullptr';
    end
    else
    begin
      if Assigned(img) then
        Result += img.ImageName + ', '
      else
        Result += 'nullptr, ';
    end;
  end;
end;

{ TISVGObject }

constructor TISVGObject.Create(ADocument: TObject);
begin
  inherited Create;
  fDocument := ADocument;
  fBuffer := TAggFPImage.Create(0, 0);
  fBuffer.PixelFormat := afpimRGBA32;
end;

destructor TISVGObject.Destroy;
begin
  FreeAndNil(fBuffer);
  inherited Destroy;
end;

function TISVGObject.GetIsEmpty: boolean;
begin
  Result := (GetWidth = 0) or (GetHeight = 0);
end;

function TISVGObject.GetHeight: double;
var
  svg: TIEmbededSVG;
begin
  Result := 0;
  if fDocument = nil then
    exit;
  svg := TIDocument(fDocument).SVGCache.ImageByName[fSVGName] as TIEmbededSVG;
  if svg = nil then
    exit;
  Result := svg.Height;
end;

function TISVGObject.GetWidth: double;
var
  svg: TIEmbededSVG;
begin
  Result := 0;
  if fDocument = nil then
    exit;
  svg := TIDocument(fDocument).SVGCache.ImageByName[fSVGName] as TIEmbededSVG;
  if svg = nil then
    exit;
  Result := svg.Width;
end;

procedure TISVGObject.RenderSVG(const AWidth, AHeight: double);
var
  w, h: integer;
  svg: TIEmbededSVG;
  bmp: TBitmap32;
begin
  if fDocument = nil then
    exit;
  svg := TIDocument(fDocument).SVGCache.ImageByName[fSVGName] as TIEmbededSVG;
  if not Assigned(svg) then
    exit;
  w := Ceil(AWidth);
  h := Ceil(AHeight);
  fBuffer.SetSize(w, h);
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(w, h);
    svg.SVG.RenderTo(bmp, fAngle);
    PaintBitmap32ToAggFPImage(bmp, fBuffer);
  finally
    bmp.Free;
  end;
end;

procedure TISVGObject.Paint(Canvas: TICanvas; const ARect: TIFloatRect);
begin
  if (fBuffer.Width = 0) or (fBuffer.Height = 0) then
    exit;
  Canvas.DrawImage(fBuffer, ARect);
end;


{ TISVGControlBase }

constructor TISVGControlBase.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fRenderNeeded := False;
  fActionFunction := DEFAULT_ACT_FUNCTION;
end;

procedure TISVGControlBase.SetValue(AValue: double);
begin
  AValue := EnsureRange(AValue, 0, 1);
  if fValue = AValue then
    Exit;
  DoChanging(Self);
  fValue := AValue;
  DoChanged(Self);
end;

procedure TISVGControlBase.SetActionFunction(AValue: string);
begin
  if fActionFunction = AValue then
    Exit;
  DoChanging(Self);
  if AValue.IsEmpty then
    fActionFunction := DEFAULT_ACT_FUNCTION
  else
    fActionFunction := AValue;
  DoChanged(Self);
end;

procedure TISVGControlBase.Resize;
begin
  inherited Resize;
  if osCreating in State then
    Exit;
  MakeRects(GetRect);
end;

procedure TISVGControlBase.DoChanged(AValue: TIObject);
begin
  inherited DoChanged(AValue);
  fRenderNeeded := True;
end;

procedure TISVGControlBase.MakeRects(const ARect: TIFloatRect);
begin
  // nothing
end;

procedure TISVGControlBase.DrawSVG(Canvas: TICanvas; svg: TISVGObject;
  rect: PIFloatRect);
begin
  if osCreating in State then
    Exit;
  if fRenderNeeded then
  begin
    svg.RenderSVG(Width, Height);
    fRenderNeeded := False;
  end;
  svg.Paint(Canvas, GetRect);
end;

{ TISVGKnobControl }

constructor TISVGKnobControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fSVG := TISVGObject.Create(ADocument);
  fStartAngle := -135;
  fEndAngle := 135;
  SetBounds(0, 0, 50, 50);
end;

destructor TISVGKnobControl.Destroy;
begin
  FreeAndNil(fSVG);
  inherited Destroy;
end;

procedure TISVGKnobControl.SetSVGName(AValue: TSVGName);
begin
  if GetSVGName = AValue then
    Exit;
  DoChanging(Self);
  fSVG.SVGName := AValue;
  DoChanged(Self);
end;

procedure TISVGKnobControl.Paint(Canvas: TICanvas);
begin
  DrawSVG(Canvas, fSVG);
end;

procedure TISVGKnobControl.DrawSVG(Canvas: TICanvas; svg: TISVGObject;
  rect: PIFloatRect);
begin
  if fRenderNeeded then
  begin
    svg.fAngle := fStartAngle + fValue * (fEndAngle - fStartAngle);
    svg.RenderSVG(Width, Height);
    fRenderNeeded := False;
  end;
  svg.Paint(Canvas, GetRect);
end;

function TISVGKnobControl.GetSVGName: TSVGName;
begin
  Result := fSVG.SVGName;
end;

class function TISVGKnobControl.GetObjectName: string;
begin
  Result := 'ISVGKnobControl';
end;

function TISVGKnobControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s)', [GetObjectName, IRect(GetRect),
    GetSVGClass(self, fSVG.SVGName), ParamIdx]);
end;

{ TISVGButtonControl }

constructor TISVGButtonControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fOffSVG := TISVGObject.Create(ADocument);
  fOnSVG := TISVGObject.Create(ADocument);
  fActionFunction := 'nullptr';
end;

destructor TISVGButtonControl.Destroy;
begin
  FreeAndNil(fOffSVG);
  FreeAndNil(fOnSVG);
  inherited Destroy;
end;

class function TISVGButtonControl.GetObjectName: string;
begin
  Result := 'ISVGButtonControl';
end;

function TISVGButtonControl.GetOffSVGName: TSVGName;
begin
  Result := fOffSVG.SVGName;
end;

function TISVGButtonControl.GetOnSVGName: TSVGName;
begin
  Result := fOnSVG.SVGName;
end;

procedure TISVGButtonControl.SetOffSVGName(AValue: TSVGName);
begin
  if GetOffSVGName = AValue then
    Exit;
  DoChanging(Self);
  fOffSVG.SVGName := AValue;
  DoChanged(Self);
end;

procedure TISVGButtonControl.SetOnSVGName(AValue: TSVGName);
begin
  if GetOnSVGName = AValue then
    Exit;
  DoChanging(Self);
  fOnSVG.SVGName := AValue;
  DoChanged(Self);
end;

procedure TISVGButtonControl.Paint(Canvas: TICanvas);
begin
  if Value > 0.5 then
    DrawSVG(Canvas, fOnSVG)
  else
    DrawSVG(Canvas, fOffSVG);
end;

function TISVGButtonControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s, %s)', [GetObjectName, IRect(GetRect),
    ActionFunction, GetSVGClass(self, fOffSVG.SVGName),
    GetSVGClass(self, fOnSVG.SVGName)]);
end;


{ TISVGSwitchControl }

constructor TISVGSwitchControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fSVGs := TStringList.Create;
  fSVGs.OwnsObjects := True;
end;

destructor TISVGSwitchControl.Destroy;
begin
  FreeAndNil(fSVGs);
  inherited Destroy;
end;

class function TISVGSwitchControl.GetObjectName: string;
begin
  Result := 'ISVGSwitchControl';
end;

function TISVGSwitchControl.GetSVGs: TSVGNames;
begin
  Result := FormatOptStr(fSVGs.CommaText);
end;

function TISVGSwitchControl.GetSelectedIdx: integer;
begin
  Result := Round(Value * (fSVGs.Count - 1));
end;

procedure TISVGSwitchControl.SVGsChange;
var
  i: integer;
  svg: TISVGObject;
begin
  for i := 0 to fSVGs.Count - 1 do
  begin
    fSVGs.Objects[i].Free;
    svg := TISVGObject.Create(Document);
    svg.SVGName := fSVGs[i];
    fSVGs.Objects[i] := svg;
  end;
end;

procedure TISVGSwitchControl.SetSVGs(AValue: TSVGNames);
begin
  AValue := NormalizeOptStr(AValue);
  if AValue = fSVGs.CommaText then
    exit;
  DoChanging(Self);
  fSVGs.CommaText := AValue;
  SVGsChange;
  DoChanged(Self);
end;

procedure TISVGSwitchControl.Paint(Canvas: TICanvas);
var
  idx: integer;
  svg: TISVGObject;
begin
  idx := GetSelectedIdx;
  if not InRange(idx, 0, fSVGs.Count - 1) then
    exit;
  svg := TISVGObject(fSVGs.Objects[idx]);
  DrawSVG(Canvas, svg);
end;

function TISVGSwitchControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, {%s}, %s, %s)', [GetObjectName, IRect(GetRect),
    GetSVGClass(self, fSVGs), ParamIdx, ActionFunction]);
end;


{ TISVGSliderControl }

constructor TISVGSliderControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fDirection := Vertical;
  fGearing := DEFAULT_GEARING;
  fHandleSVG := TISVGObject.Create(ADocument);
  fTrackSVG := TISVGObject.Create(ADocument);
  fTrackBounds := TIFloatRect.Empty;
  fTrackSVGBounds := TIFloatRect.Empty;
  fHandleBoundsAtMax := TIFloatRect.Empty;
  SetBounds(0, 0, 25, 100);
end;

destructor TISVGSliderControl.Destroy;
begin
  FreeAndNil(fHandleSVG);
  FreeAndNil(fTrackSVG);
  inherited Destroy;
end;

class function TISVGSliderControl.GetObjectName: string;
begin
  Result := 'ISVGSliderControl';
end;

procedure TISVGSliderControl.SetDirection(AValue: TDirection);
begin
  if fDirection = AValue then
    exit;
  DoChanging(Self);
  fDirection := AValue;
  Refresh;
  DoChanged(Self);
end;

function TISVGSliderControl.GetHandleSVGName: TSVGName;
begin
  Result := fHandleSVG.SVGName;
end;

function TISVGSliderControl.GetTrackSVGName: TSVGName;
begin
  Result := fTrackSVG.SVGName;
end;

function TISVGSliderControl.GetHandleBounds(AValue: double): TIFloatRect;
var
  offs: double;
begin
  if fHandleBoundsAtMax.IsEmpty then
    Refresh;
  if AValue < 0.0 then
    AValue := Value;
  Result := fHandleBoundsAtMax;
  if fDirection = Vertical then
  begin
    offs := (1 - AValue) * fTrackBounds.Height;
    Result.Top += offs;
    Result.Bottom += offs;
  end
  else
  begin
    offs := AValue * fTrackBounds.Width;
    Result.Left += offs;
    Result.Right += offs;
  end;
end;

procedure TISVGSliderControl.SetHandleSVGName(AValue: TSVGName);
begin
  if GetHandleSVGName = AValue then
    Exit;
  DoChanging(Self);
  fHandleSVG.SVGName := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TISVGSliderControl.SetTrackSVGName(AValue: TSVGName);
begin
  if GetTrackSVGName = AValue then
    Exit;
  DoChanging(Self);
  fTrackSVG.SVGName := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TISVGSliderControl.SetGearing(AValue: string);
begin
  if fGearing = AValue then
    exit;
  DoChanging(Self);
  if AValue.IsEmpty then
    fGearing := DEFAULT_GEARING
  else
    fGearing := AValue;
  DoChanged(Self);
end;

procedure TISVGSliderControl.Resize;
var
  tas, hds, hoth: double;
  r, hbm: TIFloatRect;
begin
  inherited Resize;
  fRenderNeeded := True;
  if osCreating in State then
    exit;
  if fTrackSVG.IsEmpty or fHandleSVG.IsEmpty then
    exit;
  tas := fTrackSVG.Width / fTrackSVG.Height;
  hds := fHandleSVG.Width / fHandleSVG.Height;
  hoth := fHandleSVG.Height / fTrackSVG.Height;

  r := GetRect;
  fTrackSVGBounds := r.GetCentredInside(r.Height * tas, r.Height);
  hbm := r.GetCentredInside(r.Height * hds * hoth, r.Height * hoth);
  fHandleBoundsAtMax := TIFloatRect.Create(hbm.Left, fTrackSVGBounds.Top,
    hbm.Right, fTrackSVGBounds.Top + hbm.Height);
  fTrackBounds := fTrackSVGBounds.GetPadded(0, -hbm.Height, 0, 0);
end;

procedure TISVGSliderControl.SetValue(AValue: double);
begin
  inherited SetValue(AValue);
  Refresh;
end;

procedure TISVGSliderControl.DrawSVG(Canvas: TICanvas; svg: TISVGObject;
  rect: PIFloatRect);
begin
  if osCreating in State then
    Exit;
  if fRenderNeeded then
    svg.RenderSVG(rect^.Width, rect^.Height);
  svg.Paint(Canvas, rect^);
end;

procedure TISVGSliderControl.Paint(Canvas: TICanvas);
var
  r: TIFloatRect;
begin
  r := GetHandleBounds(Value);
  DrawSVG(Canvas, fTrackSVG, @fTrackSVGBounds);
  DrawSVG(Canvas, fHandleSVG, @r);
  if fRenderNeeded then
    fRenderNeeded := False;
end;

function TISVGSliderControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s, %s, EDirection::%s, %s)',
    [GetObjectName, IRect(GetRect), GetSVGClass(self, fHandleSVG.fSVGName),
    GetSVGClass(self, fTrackSVG.fSVGName), ParamIdx,
    GetEnumName(TypeInfo(TDirection), Ord(Direction)), fGearing]);
end;

end.
