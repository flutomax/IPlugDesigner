{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uBitmapControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImgList, Graphics, FPImage, agg_fpimage,
  uCommon, uIObject, uGraphics, uStorage;

type

  { TIBitmapBaseControl }

  TIBitmapBaseControl = class(TISpan)
  private
    fBitmapName: TImageName;
    fActionFunction: string;
    fValue: double;
    procedure SetActionFunction(AValue: string);
    procedure SetBitmapName(AValue: TImageName);
    procedure SetValue(AValue: double);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    procedure DrawBitmap(Canvas: TICanvas);
    property Value: double read fValue write SetValue;
    property ActionFunction: string read fActionFunction write SetActionFunction;
    property BitmapName: TImageName read fBitmapName write SetBitmapName;
  end;

  { TIBButtonControl }

  TIBButtonControl = class(TIBitmapBaseControl)
  public
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property BitmapName;
    property Value;
  end;

  { TIBSwitchControl }

  TIBSwitchControl = class(TIBitmapBaseControl)
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property BitmapName;
    property ParamIdx;
    property Value;
  end;

  { TIBKnobControl }

  TIBKnobControl = class(TIBitmapBaseControl)
  private
    fDirection: TDirection;
    fGearing: string;
    procedure SetDirection(AValue: TDirection);
    procedure SetGearing(AValue: string);
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property BitmapName;
    property ParamIdx;
    property Value;
    property Gearing: string read fGearing write SetGearing;
    property Direction: TDirection read fDirection write SetDirection;
  end;

  { TIBKnobRotaterControl }

  TIBKnobRotaterControl = class(TIBitmapBaseControl)
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property BitmapName;
    property ParamIdx;
    property Value;
  end;

  { TIBSliderControl }

  TIBSliderControl = class(TIBitmapBaseControl)
  private
    fDirection: TDirection;
    fGearing: string;
    fTrackBounds: TIFloatRect;
    fTrackBitmapName: TImageName;
    function GetHandleBounds(AValue: double = -1.0): TIFloatRect;
    procedure SetDirection(AValue: TDirection);
    procedure SetGearing(AValue: string);
    procedure SetTrackBitmapName(AValue: TImageName);
  protected
    procedure Resize; override;
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ParamIdx;
    property Value;
    property HandleBitmapName: TImageName read fBitmapName write SetBitmapName;
    property TrackBitmapName: TImageName read fTrackBitmapName write SetTrackBitmapName;
    property Gearing: string read fGearing write SetGearing;
    property Direction: TDirection read fDirection write SetDirection;
  end;

  { TIBTextControl }

  TIBTextControl = class(TIBitmapBaseControl)
  private
    fCharWidth: integer;
    fCharHeight: integer;
    fCharOffset: integer;
    fMultiLine: boolean;
    fText: string;
    fVCentre: boolean;
    fBlend: TEBlend;
    function GetFont: TIFont;
    procedure SetBlend(AValue: TEBlend);
    procedure SetCharHeight(AValue: integer);
    procedure SetCharOffset(AValue: integer);
    procedure SetCharWidth(AValue: integer);
    procedure SetFont(AValue: TIFont);
    procedure SetMultiLine(AValue: boolean);
    procedure SetText(AValue: string);
    procedure SetVCentre(AValue: boolean);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property BitmapName;
    property CharWidth: integer read fCharWidth write SetCharWidth;
    property CharHeight: integer read fCharHeight write SetCharHeight;
    property CharOffset: integer read fCharOffset write SetCharOffset;
    property MultiLine: boolean read fMultiLine write SetMultiLine;
    property VCentre: boolean read fVCentre write SetVCentre;
    property Font: TIFont read GetFont write SetFont;
    property Text: string read fText write SetText;
    property Blend: TEBlend read fBlend write SetBlend;
  end;


implementation

uses
  Math, TypInfo, uIDocument;

function GetBitmapClass(AControl: TIBitmapBaseControl; const BitmapName: string): string;
var
  img: TIEmbededGraphics;
begin
  img := TIDocument(AControl.Document).ImageCache.ImageByName[BitmapName];
  if Assigned(img) then
    Result := img.ImageName
  else
    Result := 'nullptr';
end;

{ TIBitmapBaseControl }

constructor TIBitmapBaseControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fActionFunction := DEFAULT_CLICK_ACTION_FUNC;
end;

procedure TIBitmapBaseControl.SetValue(AValue: double);
begin
  AValue := EnsureRange(AValue, 0, 1);
  if fValue = AValue then
    Exit;
  DoChanging(Self);
  fValue := AValue;
  DoChanged(Self);
end;

procedure TIBitmapBaseControl.SetBitmapName(AValue: TImageName);
begin
  if fBitmapName = AValue then
    Exit;
  DoChanging(Self);
  fBitmapName := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIBitmapBaseControl.SetActionFunction(AValue: string);
begin
  if fActionFunction = AValue then
    Exit;
  DoChanging(Self);
  if AValue.IsEmpty then
    fActionFunction := DEFAULT_CLICK_ACTION_FUNC
  else
    fActionFunction := AValue;
  DoChanged(Self);
end;

procedure TIBitmapBaseControl.DrawBitmap(Canvas: TICanvas);
var
  i: integer;
  r, tr: TIFloatRect;
  img: TIEmbededImage;
begin
  img := TIDocument(Document).ImageCache.ImageByName[fBitmapName] as TIEmbededImage;
  if img = nil then
    exit;
  tr := Canvas.ClipRect;
  try
    Canvas.ClipRect := GetRect;
    i := 1;
    if img.States > 1 then
    begin
      i := 1 + Round(Value * (img.States - 1));
      i := EnsureRange(i, 1, img.States);
    end;
    r := GetRect.GetCentredInside(TIFloatRect.Create(0, 0, img.FrameWidth,
      img.FrameHeight));
    Canvas.DrawImage(img, r, i);
  finally
    Canvas.ClipRect := tr;
  end;
end;

procedure TIBitmapBaseControl.Paint(Canvas: TICanvas);
begin
  DrawBitmap(Canvas);
end;


{ TIBButtonControl }

class function TIBButtonControl.GetObjectName: string;
begin
  Result := 'IBButtonControl';
end;

function TIBButtonControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s)', [GetObjectName, IRect(GetRect),
    GetBitmapClass(self, BitmapName), ActionFunction]);
end;

{ TIBSwitchControl }

constructor TIBSwitchControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  SetBounds(0, 0, 50, 50);
end;

class function TIBSwitchControl.GetObjectName: string;
begin
  Result := 'IBSwitchControl';
end;

function TIBSwitchControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s)', [GetObjectName, IRect(GetRect),
    GetBitmapClass(self, BitmapName), ParamIdx]);
end;

{ TIBKnobControl }

constructor TIBKnobControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fGearing := DEFAULT_GEARING;
  fDirection := Vertical;
  SetBounds(0, 0, 50, 50);
end;

class function TIBKnobControl.GetObjectName: string;
begin
  Result := 'IBKnobControl';
end;

procedure TIBKnobControl.SetDirection(AValue: TDirection);
begin
  if fDirection = AValue then
    exit;
  DoChanging(Self);
  fDirection := AValue;
  DoChanged(Self);
end;

procedure TIBKnobControl.SetGearing(AValue: string);
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

function TIBKnobControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s, EDirection::%s, %s)',
    [GetObjectName, IRect(GetRect), GetBitmapClass(self, BitmapName),
    ParamIdx, GetEnumName(TypeInfo(TDirection), Ord(Direction)), fGearing]);
end;


{ TIBKnobRotaterControl }

constructor TIBKnobRotaterControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  SetBounds(0, 0, 50, 50);
end;

class function TIBKnobRotaterControl.GetObjectName: string;
begin
  Result := 'IBKnobRotaterControl';
end;

procedure TIBKnobRotaterControl.Paint(Canvas: TICanvas);
var
  angle: double;
  r, tr: TIFloatRect;
  img: TIEmbededImage;
begin
  img := TIDocument(Document).ImageCache.ImageByName[fBitmapName] as TIEmbededImage;
  if img = nil then
    exit;
  angle := -130 + Value * 260;
  r := GetRect;
  tr := Canvas.ClipRect;
  try
    Canvas.ClipRect := r;
    Canvas.DrawRotatedImage(img, r.MW, r.MH, angle);
  finally
    Canvas.ClipRect := tr;
  end;
end;

function TIBKnobRotaterControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s)', [GetObjectName, IRect(GetRect),
    GetBitmapClass(self, BitmapName), ParamIdx]);
end;


{ TIBSliderControl }

constructor TIBSliderControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fGearing := DEFAULT_GEARING;
  fDirection := Vertical;
  fTrackBounds := TIFloatRect.Empty;
  SetBounds(0, 0, 50, 100);
end;

class function TIBSliderControl.GetObjectName: string;
begin
  Result := 'IBSliderControl';
end;

function TIBSliderControl.GetHandleBounds(AValue: double): TIFloatRect;
var
  img: TIEmbededImage;
begin
  Result := TIFloatRect.Empty;
  img := TIDocument(Document).ImageCache.ImageByName[fBitmapName] as TIEmbededImage;
  if img = nil then
    exit;
  if AValue < 0.0 then
    AValue := Value;

  Result := TIFloatRect.Create(fTrackBounds.Left, fTrackBounds.Top,
    fTrackBounds.Left + img.FrameWidth, fTrackBounds.Top + img.FrameHeight);

  if fDirection = Vertical then
    Result.Offset(0, (1 - AValue) * (fTrackBounds.Height - img.Height))
  else
    Result.Offset(AValue * (fTrackBounds.Width - img.Width), 0);
end;

procedure TIBSliderControl.SetDirection(AValue: TDirection);
begin
  if fDirection = AValue then
    exit;
  DoChanging(Self);
  fDirection := AValue;
  DoChanged(Self);
end;

procedure TIBSliderControl.SetGearing(AValue: string);
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

procedure TIBSliderControl.SetTrackBitmapName(AValue: TImageName);
begin
  if fTrackBitmapName = AValue then
    Exit;
  DoChanging(Self);
  fTrackBitmapName := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIBSliderControl.Resize;
var
  TrImg, HImg: TIEmbededImage;
  h: double;
  r: TIFloatRect;
begin
  inherited Resize;
  HImg := TIDocument(Document).ImageCache.ImageByName[fBitmapName] as TIEmbededImage;
  TrImg := TIDocument(Document).ImageCache.ImageByName[fTrackBitmapName] as
    TIEmbededImage;
  if HImg = nil then
    exit;
  r := GetRect;
  if fDirection = Vertical then
  begin
    if Assigned(TrImg) then
      fTrackBounds := r.GetCentredInside(TIFloatRect.Create(0, 0,
        TrImg.FrameWidth, TrImg.FrameHeight))
    else
    begin
      h := HImg.Width / 2;
      fTrackBounds := r.GetMidHPadded(h);
    end;
  end
  else
  begin
    if Assigned(TrImg) then
      fTrackBounds := r.GetCentredInside(TIFloatRect.Create(0, 0,
        TrImg.FrameWidth, TrImg.FrameHeight))
    else
    begin
      h := HImg.Height / 2;
      fTrackBounds := r.GetMidVPadded(h);
    end;
  end;
end;

procedure TIBSliderControl.Paint(Canvas: TICanvas);
var
  TrImg, HImg: TIEmbededImage;
  r, tr: TIFloatRect;
begin
  if fTrackBounds.IsEmpty then
    Refresh;
  HImg := TIDocument(Document).ImageCache.ImageByName[fBitmapName] as TIEmbededImage;
  TrImg := TIDocument(Document).ImageCache.ImageByName[fTrackBitmapName] as
    TIEmbededImage;
  tr := Canvas.ClipRect;
  try
    Canvas.ClipRect := GetRect;
    if Assigned(TrImg) then
    begin
      r := GetRect;
      r := r.GetCentredInside(fTrackBounds);
      Canvas.DrawImage(TrImg, 0, 0, r);
    end;
    if Assigned(HImg) then
      Canvas.DrawImage(HImg, GetHandleBounds, 1);
  finally
    Canvas.ClipRect := tr;
  end;
end;

function TIBSliderControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s, %s, EDirection::%s, %s)',
    [GetObjectName, IRect(GetRect), GetBitmapClass(self, fBitmapName),
    GetBitmapClass(self, fTrackBitmapName), ParamIdx,
    GetEnumName(TypeInfo(TDirection), Ord(Direction)), fGearing]);
end;


{ TIBTextControl }

constructor TIBTextControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fCharWidth := 6;
  fCharHeight := 12;
  fCharOffset := 0;
  fMultiLine := False;
  fVCentre := True;
end;

class function TIBTextControl.GetObjectName: string;
begin
  Result := 'IBTextControl';
end;

function TIBTextControl.GetFont: TIFont;
begin
  Result := GraphicsSettings.Font;
end;

procedure TIBTextControl.SetBlend(AValue: TEBlend);
begin
  if fBlend = AValue then
    Exit;
  DoChanging(Self);
  fBlend := AValue;
  DoChanged(Self);
end;

procedure TIBTextControl.SetCharHeight(AValue: integer);
begin
  if fCharHeight = AValue then
    Exit;
  DoChanging(Self);
  fCharHeight := AValue;
  DoChanged(Self);
end;

procedure TIBTextControl.SetCharOffset(AValue: integer);
begin
  if fCharOffset = AValue then
    Exit;
  DoChanging(Self);
  fCharOffset := AValue;
  DoChanged(Self);
end;

procedure TIBTextControl.SetCharWidth(AValue: integer);
begin
  if fCharWidth = AValue then
    Exit;
  DoChanging(Self);
  fCharWidth := AValue;
  DoChanged(Self);
end;

procedure TIBTextControl.SetFont(AValue: TIFont);
begin
  GraphicsSettings.Font := AValue;
end;

procedure TIBTextControl.SetMultiLine(AValue: boolean);
begin
  if fMultiLine = AValue then
    Exit;
  DoChanging(Self);
  fMultiLine := AValue;
  DoChanged(Self);
end;

procedure TIBTextControl.SetText(AValue: string);
begin
  if fText = AValue then
    Exit;
  DoChanging(Self);
  fText := AValue;
  DoChanged(Self);
end;

procedure TIBTextControl.SetVCentre(AValue: boolean);
begin
  if fVCentre = AValue then
    Exit;
  fVCentre := AValue;
end;

procedure TIBTextControl.Paint(Canvas: TICanvas);
var
  i, p, sl, si, cw, ct, nl, fo: integer;
  r, cr: TIFloatRect;
  xo, yo, xf, yf: double;
  img: TIEmbededImage;
begin
  img := TIDocument(Document).ImageCache.ImageByName[fBitmapName] as TIEmbededImage;
  if fText.IsEmpty or (img = nil) then
    exit;
  xo := 0;
  yo := 0;
  si := 1;
  sl := fText.Length;
  r := GetRect;
  yo := r.Top;
  if fVCentre then
    yo += ((r.Height - fCharHeight) / 2);
  cw := sl * fCharWidth;
  case Font.Align of
    Near: xo := r.Left;
    Center: xo := r.Left + ((r.Width - cw) / 2);
    Far: xo := r.Right - cw;
  end;

  if fMultiLine then
  begin
    if cw > r.Width then
    begin
      ct := Trunc(r.Width / fCharWidth);
      nl := Trunc(cw / r.Width) + 1;
    end
    else // line is shorter than width of bounds
    begin
      ct := sl;
      nl := 1;
    end;
  end
  else
  begin
    ct := Trunc(r.Width / fCharWidth);
    nl := 1;
  end;

  for i := 0 to nl - 1 do
  begin
    yf := yo + i * fCharHeight;
    for p := 0 to ct - 1 do
    begin
      if fText[si] = #0 then
        exit;
      fo := Ord(fText[si]) - 31; // calculate which frame to look up
      Inc(si);
      xf := (p * (fCharWidth + fCharOffset)) + xo;
      // calculate xf for character we're drawing
      cr := TIFloatRect.Create(xf, yf, xf + fCharWidth, yf + fCharHeight);
      Canvas.DrawImage(img, cr, fo);
    end;
  end;
end;

function TIBTextControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s, "%s", %d, %d, %d, %s, %s, EBlend::%s)',
    [GetObjectName, IRect(GetRect), GetBitmapClass(self, BitmapName),
    ITextStr(Font), Text, CharWidth, CharHeight, CharOffset,
    BooleanToStr(MultiLine), BooleanToStr(VCentre),
    GetEnumName(TypeInfo(TEBlend), Ord(Blend))]);
end;

end.
