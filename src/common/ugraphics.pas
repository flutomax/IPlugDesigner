{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uGraphics;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  LCLType, LCLIntf, Classes, Contnrs, Controls, Graphics, SysUtils, Variants,
  FPImage, GR32, agg_fpimage, uAGGCanvas, uCommon, uSVGObjects, uTree,
  uIniFileEx;

type

  TIBrushStyle = (Solid, Clear);
  TIFontStyleFlag = (Bold, Italic);
  TIFontStyle = set of TIFontStyleFlag;
  TITextAlign = (Near, Center, Far);
  TITextVAlign = (Top, Middle, Bottom);
  TICanvasState = set of (csPenValid, csBrushValid, csFontValid);
  TIDrawingTool = (dtPen, dtBrush, dtFont);
  TIDrawingTools = set of TIDrawingTool;
  TIEmbededImage = class;

  { TIGraphicsObject }

  TIGraphicsObject = class(TINonRefInterfacedObject)
  private
    fOnChanging: TNotifyEvent;
    fOnChange: TNotifyEvent;
  protected
    procedure Changing;
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TIPen }

  TIPen = class(TIGraphicsObject)
  private
    fWidth: double;
    fColor: TIColor;
    procedure SetColor(const AValue: TIColor);
    procedure SetWidth(const AValue: double);
    function GetWidth: double;
    function GetColor: TIColor;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TIColor read fColor write SetColor;
    property Width: double read fWidth write SetWidth;
  end;

  { TIBrush }

  TIBrush = class(TIGraphicsObject)
  private
    fStyle: TIBrushStyle;
    fColor: TIColor;
    procedure SetColor(const AValue: TIColor);
    procedure SetStyle(const AValue: TIBrushStyle);
    function GetColor: TIColor;
    function GetStyle: TIBrushStyle;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TIColor read fColor write SetColor;
    property Style: TIBrushStyle read fStyle write SetStyle;
  end;

  { TIFont }

  TIFont = class(TIGraphicsObject)
  private
    fSize: double;
    fFamily: TFontName;
    fColor: TIColor;
    fStyle: TIFontStyle;
    fAlign: TITextAlign;
    fVAlign: TITextVAlign;
    fAngle: double;
    procedure SetColor(const AValue: TIColor);
    procedure SetFamily(const AValue: TFontName);
    procedure SetSize(const AValue: double);
    procedure SetStyle(const AValue: TIFontStyle);
    procedure SetAlign(const AValue: TITextAlign);
    procedure SetVAlign(const AValue: TITextVAlign);
    procedure SetAngle(const AValue: double);
    function GetSize: double;
    function GetFamily: string;
    function GetColor: TIColor;
    function GetStyle: TIFontStyle;
    function GetAlign: TITextAlign;
    function GetVAlign: TITextVAlign;
    function GetAngle: double;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure AssignFont(AValue: TFont);
    procedure AssignToFont(AValue: TFont);
    procedure LoadFromIni(ini: TIniFileEx; const Section: string);
    procedure SaveToIni(ini: TIniFileEx; const Section: string);
  published
    property Family: TFontName read fFamily write SetFamily;
    property Size: double read fSize write SetSize;
    property Color: TIColor read fColor write SetColor;
    property Style: TIFontStyle read fStyle write SetStyle;
    property Align: TITextAlign read fAlign write SetAlign;
    property VAlign: TITextVAlign read fVAlign write SetVAlign;
    property Angle: double read GetAngle write SetAngle;
  end;

  { TIGraphicsSettings }

  TIGraphicsSettings = class(TIGraphicsObject)
  private
    fBrush: TIBrush;
    fFont: TIFont;
    fPen: TIPen;
    procedure ParamChanged(Sender: TObject);
    procedure SetFont(const Value: TIFont);
    procedure SetBrush(const Value: TIBrush);
    procedure SetPen(const Value: TIPen);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Font: TIFont read fFont write SetFont;
    property Pen: TIPen read fPen write SetPen;
    property Brush: TIBrush read fBrush write SetBrush;
  end;

  TIGraphicsSettingsClass = class of TIGraphicsSettings;

  { TICanvas }

  TICanvas = class(TINonRefInterfacedObject)
  private
    fCanvas: TAggCanvas;
    fDesigner: TObject;
    fState: TICanvasState;
    fGraphicsSettings: TIGraphicsSettings;
    function GetBrushColor: TIColor;
    function GetFontAlign: TITextAlign;
    function GetFontAngle: double;
    function GetFontColor: TIColor;
    function GetFontVAlign: TITextVAlign;
    function GetMasterAlpha: double;
    function GetPenColor: TIColor;
    function GetPenWidth: double;
    procedure GraphicsSettingsChanged(Sender: TObject);
    procedure SetBrushColor(AValue: TIColor);
    procedure SetClipRect(AValue: TIFloatRect);
    procedure SetFontAlign(AValue: TITextAlign);
    procedure SetFontAngle(AValue: double);
    procedure SetFontColor(AValue: TIColor);
    procedure SetFontVAlign(AValue: TITextVAlign);
    procedure SetGraphicsSettings(const AValue: TIGraphicsSettings);
    procedure SetMasterAlpha(AValue: double);
    procedure SetPenColor(AValue: TIColor);
    procedure SetPenWidth(AValue: double);
  protected
    function GetClipRect: TIFloatRect;
    procedure Init;
    procedure RequireState(AState: TICanvasState);
    procedure UpdateFont; virtual;
    procedure UpdatePen; virtual;
    procedure UpdateBrush; virtual;
  public
    constructor Create(AWidth, AHeight: integer); overload;
    constructor Create(ACanvas: TAggCanvas; ADesigner: TObject); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure DrawCheckboard(const TileSize: integer = 4);
    procedure DrawTo(ACanvas: TAggCanvas); overload;
    procedure DrawTo(ABitmap: TBitmap); overload;
    procedure DrawLine(X1, Y1, X2, Y2: double);
    procedure DrawRect(const R: TIFloatRect); overload;
    procedure DrawRect(X1, Y1, X2, Y2: double); overload;
    procedure DrawRoundedRect(const R: TIFloatRect; rd: double); overload;
    procedure DrawRoundedRect(X1, Y1, X2, Y2, r: double); overload;
    procedure DrawRoundedRect(const R: TIFloatRect; rTL, rTR, rBR, rBL: double);
      overload;
    procedure DrawEllipse(X1, Y1, X2, Y2: double); overload;
    procedure DrawEllipse(const R: TIFloatRect); overload;
    procedure DrawArc(const cx, cy, r, a1, a2: double);
    procedure DrawRadialLine(const cx, cy, angle, rMin, rMax: double);
    procedure DrawGroupboxFrame(const R: TIFloatRect; const ll, lr, cr, hft: double);
    procedure DrawText(Text: string; ARect: TIFloatRect; Underline: boolean = False);
    procedure MeasureText(Text: string; ARect: TIFloatRect; var TextRect: TIFloatRect);
    procedure DrawPolyline(Pts: PIFloatPoint; Count: integer);
    procedure DrawPolygon(Pts: PIFloatPoint; Count: integer);
    procedure DrawImage(image: TIEmbededImage; X, Y: integer;
      Dst: TIFloatRect); overload;
    procedure DrawImage(image: TIEmbededImage; const ARect: TIFloatRect;
      AState: integer); overload;
    procedure DrawImage(image: TIEmbededImage; ARect: TIFloatRect); overload;
    procedure DrawImage(image: TAggFPImage; ARect: TIFloatRect); overload;
    procedure DrawRotatedImage(image: TIEmbededImage; const cx, cy, angle: double);
    procedure TransformSpace(srcRect, dstRect: TIFloatRect);
    property MasterAlpha: double read GetMasterAlpha write SetMasterAlpha;
    property ClipRect: TIFloatRect read GetClipRect write SetClipRect;
    property GraphicsSettings: TIGraphicsSettings
      read fGraphicsSettings write SetGraphicsSettings;
    property BrushColor: TIColor read GetBrushColor write SetBrushColor;
    property PenColor: TIColor read GetPenColor write SetPenColor;
    property PenWidth: double read GetPenWidth write SetPenWidth;
    property FontColor: TIColor read GetFontColor write SetFontColor;
    property FontAngle: double read GetFontAngle write SetFontAngle;
    property FontAlign: TITextAlign read GetFontAlign write SetFontAlign;
    property FontVAlign: TITextVAlign read GetFontVAlign write SetFontVAlign;
    property AggCanvas: TAggCanvas read fCanvas;
  end;


  { TIEmbededGraphics }

  TIEmbededGraphics = class(TINonRefInterfacedObject)
  private
    fFileName: string;
    function GetImageName: string;
  protected
    function GetHeight: integer; virtual; abstract;
    function GetWidth: integer; virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    function Clone: TIEmbededGraphics; virtual;
    function LoadImageFile(const aFileName: string): boolean; virtual;
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    procedure SaveToFile(const aFileName: string);
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  published
    property FileName: string read fFileName;
    property ImageName: string read GetImageName;
  end;

  { TIEmbededImage }

  TIEmbededImage = class(TIEmbededGraphics)
  private
    fImage: TAggFPImage;
    fStates: integer;
    fTargetScale: integer;
    fFramesAreHorizontal: boolean;
    procedure SetStates(AValue: integer);
    procedure SetTargetScale(AValue: integer);
  protected
    function GetHeight: integer; override;
    function GetWidth: integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Clone: TIEmbededGraphics; override;
    procedure Assign(Source: TPersistent); override;
    function LoadImageFile(const aFileName: string): boolean; override;
    function FrameWidth: integer;
    function FrameHeight: integer;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    property Image: TAggFPImage read fImage;
    property States: integer read fStates write SetStates;
    property FramesAreHorizontal: boolean read fFramesAreHorizontal
      write fFramesAreHorizontal;
    property TargetScale: integer read fTargetScale write SetTargetScale;

  end;

  { TIEmbededSVG }

  TIEmbededSVG = class(TIEmbededGraphics)
  private
    fSVG: TSVG;
    function GetRealHeight: integer;
    function GetRealWidth: integer;
  protected
    function GetHeight: integer; override;
    function GetWidth: integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Clone: TIEmbededGraphics; override;
    function LoadImageFile(const aFileName: string): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure RenderToBitmap(ABitmap: TCustomBitmap);
    procedure RenderToMiniature(ABitmap: TCustomBitmap);
    procedure RenderToPreview(ABitmap: TCustomBitmap);
    procedure RenderToStream(AStream: TStream);
    property RealWidth: integer read GetRealWidth;
    property RealHeight: integer read GetRealHeight;
    property SVG: TSVG read fSVG;
  end;

  { TImageCache }

  TImageCache = class(TINonRefInterfacedObject, ICustomSerialize)
  private
    fOwner: TObject;
    fImages: TObjectList;
    function GetImage(AIndex: integer): TIEmbededGraphics;
    function GetImageByName(AName: string): TIEmbededGraphics;
    function GetImageIndex(AName: string): integer;
    function GetImagesCount: integer;
    procedure CustomSerialize(const Node: ITreeNode);
    procedure CustomDeSerialize(const Node: ITreeNode);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
    procedure AddImage(aImage: TIEmbededGraphics);
    procedure GetCodeStr(Lst: TStrings; AsSVG: boolean);
    function ImageExists(AName: string): boolean;
    property ImagesCount: integer read GetImagesCount;
    property Images[AIndex: integer]: TIEmbededGraphics read GetImage; default;
    property ImageIndex[AName: string]: integer read GetImageIndex;
    property ImageByName[AName: string]: TIEmbededGraphics read GetImageByName;
  end;


const

  // standard IColors
  iclTransparent = $00000000;
  iclTranslucent = $0A000000;
  iclBlack = $FF000000;
  iclBlackDropShadow = $80000000;
  iclGray = $FF7F7F7F;
  iclLightGray = $FFF0F0F0;
  iclMidGray = $FFC8C8C8;
  iclDarkGray = $FF464646;
  iclWhite = $FFFFFFFF;
  iclRed = $FFFF0000;
  iclGreen = $FF00FF00;
  iclBlue = $FF0000FF;
  iclYellow = $FFFFFF00;
  iclOrange = $FFFF7F00;
  iclIndigo = $FF4B0082;
  iclViolet = $FF9400D3;

  IColors: array[0..15] of TIdentMapEntry = (
    (Value: iclTransparent; Name: 'COLOR_TRANSPARENT'),
    (Value: iclTranslucent; Name: 'COLOR_TRANSLUCENT'),
    (Value: iclBlack; Name: 'COLOR_BLACK'),
    (Value: iclBlackDropShadow; Name: 'COLOR_BLACK_DROP_SHADOW'),
    (Value: iclGray; Name: 'COLOR_GRAY'),
    (Value: iclLightGray; Name: 'COLOR_LIGHT_GRAY'),
    (Value: iclMidGray; Name: 'COLOR_MID_GRAY'),
    (Value: iclDarkGray; Name: 'COLOR_DARK_GRAY'),
    (Value: iclWhite; Name: 'COLOR_WHITE'),
    (Value: iclRed; Name: 'COLOR_RED'),
    (Value: iclGreen; Name: 'COLOR_GREEN'),
    (Value: iclBlue; Name: 'COLOR_BLUE'),
    (Value: iclYellow; Name: 'COLOR_YELLOW'),
    (Value: iclOrange; Name: 'COLOR_ORANGE'),
    (Value: iclIndigo; Name: 'COLOR_INDIGO'),
    (Value: iclViolet; Name: 'COLOR_VIOLET')
    );


function IColorToColorRef(const Color: TIColor): COLORREF;
function ColorToIColor(const Color: TColor; const A: byte = 255): TIColor;
function ColorToAggColor(const Color: TColor; const A: byte = 255): TAggColor;
function IColorToAggColor(const Color: TIColor): TAggColor;
function AggColorToIColor(const Color: TAggColor): TIColor;
function IColorToStr(const Color: TIColor): string;
function IColorToString(const Color: TIColor): string;
function StrToIColorDef(const s: string; const DefaultValue: TIColor): TIColor;
function StringToIColorDef(const s: string; const DefaultValue: TIColor): TIColor;
function IColorStr(const Color: TIColor): string;
function ITextStr(const Font: TIFont): string;
function IRect(const r: TIFloatRect): string;
function ConvertBrushStyle(Value: TIBrushStyle): TBrushStyle;
function MakeMiniature(AImg: TIEmbededImage; ASize: integer): TCustomBitmap;
function IdentToIColor(const Ident: string; out Color: TIColor): boolean;
function IColorToIdent(const Color: TIColor; out Ident: string): boolean;
procedure GetIColorValues(Proc: TGetColorStringProc);
procedure DrawIColorRect(ACanvas: TCanvas; const ARect: TRect;
  const Color: TIColor; const AEnabled, AFrame: boolean; const TileSize: integer = 3);
procedure RadialPoints(const angle, cx, cy, rMin, rMax: double;
  nPoints: integer; Data: PIFloatPoint);
procedure PaintBitmap32ToAggFPImage(bmp: TBitmap32; img: TAggFPImage);

implementation

uses
  Math, TypInfo, FPReadPNG, FPWritePNG, FPReadJPEG, FPReadGif, FPReadTiff,
  FPReadTGA, FPReadPCX, FPReadPSD, FPReadPNM, FPReadXPM, FPReadXWD, uBmpReader,
  uStorage, uFauIcons, uIDesigner, agg_rounded_rect, agg_path_storage, agg_arc,
  GR32_Resamplers, LazFileUtils;

function DisabledIColor(const Color: TIColor): TIColor;
begin
  Result := (Color and $FFFFFF) or ((((Color shr 24) div 3) and $FF) shl 24);
end;

function IColorToColorRef(const Color: TIColor): COLORREF;
begin
  Result := Color and $FFFFFF;
  Result := ((Result and $FF) shl 16) or (((Result shr 8) and $FF) shl 8) or
    ((Result shr 16) and $FF);
end;

function ColorToIColor(const Color: TColor; const A: byte): TIColor;
var
  clr: COLORREF;
begin
  clr := ColorToRGB(Color);
  Result := (A shl 24) or ((clr and $FF) shl 16) or (((clr shr 8) and $FF) shl 8) or
    ((clr shr 16) and $FF);
end;

function ColorToAggColor(const Color: TColor; const A: byte): TAggColor;
var
  clr: COLORREF;
begin
  clr := ColorToRGB(Color);
  Result.B := clr and $FF;
  Result.G := (clr shr 8) and $FF;
  Result.R := (clr shr 16) and $FF;
  Result.A := A;
end;

function IColorToAggColor(const Color: TIColor): TAggColor;
begin
  Result := Color.ToAggColor;
end;

function AggColorToIColor(const Color: TAggColor): TIColor;
begin
  with Color do
    Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

function IColorToStr(const Color: TIColor): string;
begin
  with IColorToAggColor(Color) do
    Result := Format('$%.2x%.2x%.2x%.2x', [a, r, g, b]);
end;

function IColorToString(const Color: TIColor): string;
begin
  Result := '';
  if not IColorToIdent(Color, Result) then
    Result := IColorToStr(Color);
end;

function StrToIColorDef(const s: string; const DefaultValue: TIColor): TIColor;
begin
  Result := StrToDWordDef(s, DefaultValue);
end;

function StringToIColorDef(const s: string; const DefaultValue: TIColor): TIColor;
begin
  Result := DefaultValue;
  if not IdentToIColor(s, Result) then
    Result := StrToIColorDef(s, DefaultValue);
end;

function IColorStr(const Color: TIColor): string;
begin
  Result := '';
  if not IColorToIdent(Color, Result) then
    with IColorToAggColor(Color) do
      Result := Format('IColor(%d, %d, %d, %d)', [a, r, g, b]);
end;

function IFontName(const FontName: string): string;
begin
  result := FontName;
  if ReplaceRoboto and SameText(DEFAULT_FONT, FontName) then
    result += '-Regular';
end;

function ITextStr(const Font: TIFont): string;
begin
  Result := Format('IText(%.1f, %s, "%s", EAlign::%s, EVAlign::%s, %.1f)',
    [Font.Size, IColorStr(Font.Color), IFontName(Font.Family ),
    GetEnumName(TypeInfo(TITextAlign), Ord(Font.Align)),
    GetEnumName(TypeInfo(TITextVAlign), Ord(Font.VAlign)), Font.Angle]);
end;

function IRect(const r: TIFloatRect): string;
begin
  Result := Format('IRECT(%.1f, %.1f, %.1f, %.1f)',
    [r.Left, r.Top, r.Right, r.Bottom]);
end;

function ConvertBrushStyle(Value: TIBrushStyle): TBrushStyle;
begin
  case Value of
    Clear: Result := Graphics.bsClear;
    Solid: Result := Graphics.bsSolid;
  end;
end;

function MemoryStreamToOleVariant(Strm: TMemoryStream): olevariant;
var
  Data: PByteArray;
begin
  Result := VarArrayCreate([0, Strm.Size - 1], varByte);
  Data := VarArrayLock(Result);
  try
    Strm.Position := 0;
    Strm.ReadBuffer(Data^, Strm.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;

function OleVariantToMemoryStream(OV: olevariant): TMemoryStream;
var
  Data: PByteArray;
  Size: integer;
begin
  Result := TMemoryStream.Create;
  try
    Size := VarArrayHighBound(OV, 1) - VarArrayLowBound(OV, 1) + 1;
    Data := VarArrayLock(OV);
    try
      Result.Position := 0;
      Result.WriteBuffer(Data^, Size);
    finally
      VarArrayUnlock(OV);
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function GetBitmapFromFPImage(img: TFPCustomImage): TCustomBitmap;
var
  wr: TFPWriterPNG;
  ms: TMemoryStream;
begin
  Result := nil;
  ms := TMemoryStream.Create;
  try
    wr := TFPWriterPNG.Create;
    try
      wr.WordSized := False;
      wr.UseAlpha := True;
      wr.ImageWrite(ms, img);
    finally
      wr.Free;
    end;
    ms.Seek(0, 0);
    Result := TCustomBitmap(TPortableNetworkGraphic.Create);
    Result.LoadFromStream(ms, ms.Size);
  finally
    ms.Free;
  end;
end;

function MakeMiniature(AImg: TIEmbededImage; ASize: integer): TCustomBitmap;
var
  b: TCustomBitmap;
  c: TAggCanvas;
  w, h, x, y: integer;
begin
  c := TAggCanvas.Create;
  try
    c.Image.PixelFormat := afpimRGBA32;
    c.Image.SetSize(ASize, ASize);
    c.Image.IntfImg.FillPixels(colTransparent);
    w := ASize;
    h := ASize;
    x := 0;
    y := 0;
    if AImg.Width > AImg.Height then
    begin
      h := MulDiv(AImg.Height, ASize, AImg.Width);
      y := (ASize - h) div 2;
    end
    else
    begin
      w := MulDiv(AImg.Width, ASize, AImg.Height);
      x := (ASize - w) div 2;
    end;
    c.StretchDraw(x, y, w, h, AImg.Image);
    Result := GetBitmapFromFPImage(c.Image);
  finally
    c.Free;
  end;
end;

function IdentToIColor(const Ident: string; out Color: TIColor): boolean;
var
  i: integer;
begin
  for i := Low(IColors) to High(IColors) do
    if CompareText(IColors[i].Name, Ident) = 0 then
    begin
      Color := TIColor(IColors[i].Value);
      exit(True);
    end;
  Result := False;
end;

function IColorToIdent(const Color: TIColor; out Ident: string): boolean;
var
  i: integer;
begin
  for i := Low(IColors) to High(IColors) do
    if TIColor(IColors[i].Value) = Color then
    begin
      Ident := IColors[i].Name;
      exit(True);
    end;
  Result := False;
end;

procedure GetIColorValues(Proc: TGetColorStringProc);
var
  i: integer;
begin
  for i := Low(IColors) to High(IColors) do
    Proc(IColors[i].Name);
end;

procedure DrawIColorRect(ACanvas: TCanvas; const ARect: TRect;
  const Color: TIColor; const AEnabled, AFrame: boolean; const TileSize: integer);

  function BlendColor(const bg, fg: TIColor): TIColor;
  var
    r1, g1, b1, r2, g2, b2, a: byte;
  begin
    Result := bg;
    a := (fg shr 24) and $FF;
    if a = 0 then
      exit;
    b1 := bg and $FF;
    g1 := (bg shr 8) and $FF;
    r1 := (bg shr 16) and $FF;
    b2 := fg and $FF;
    g2 := (fg shr 8) and $FF;
    r2 := (fg shr 16) and $FF;
    b1 := ((b2 * a) + (b1 * (not a))) shr 8;
    g1 := ((g2 * a) + (g1 * (not a))) shr 8;
    r1 := ((r2 * a) + (r1 * (not a))) shr 8;
    Result := ($FF shl 24) or (r1 shl 16) or (g1 shl 8) or b1;
  end;

var
  a: byte;
  i, j, k, x, y, xr, yb: integer;
  c: TIColor;
  chk: array[boolean] of TColor;
  OldPen, OldBrush, FrameColor: TColor;
begin
  OldPen := ACanvas.Pen.Color;
  OldBrush := ACanvas.Brush.Color;
  FrameColor := IfThen(AEnabled, clBlack, clGrayText);
  a := (Color shr 24) and $FF;
  if a = $FF then
  begin
    if AEnabled then
      ACanvas.Brush.Color := IColorToColorRef(Color)
    else
      ACanvas.Brush.Color := IColorToColorRef(
        BlendColor(ColorToIColor(clBtnFace), DisabledIColor(Color)));
    ACanvas.FillRect(ARect);
  end
  else
  begin
    if AEnabled then
    begin
      chk[False] := IColorToColorRef(BlendColor(iclWhite, Color));
      chk[True] := IColorToColorRef(BlendColor($FFCCCCCC, Color));
    end
    else
    begin
      chk[False] := IColorToColorRef(BlendColor(iclWhite,
        BlendColor(ColorToIColor(clBtnFace), DisabledIColor(Color))));
      chk[True] := IColorToColorRef(BlendColor($FFCCCCCC,
        BlendColor(ColorToIColor(clBtnFace), DisabledIColor(Color))));
    end;
    k := IfThen(AFrame, 1);
    y := ARect.Top + k;
    i := 0;
    while y < ARect.Bottom - k do
    begin
      x := ARect.Left + k;
      yb := EnsureRange(y + TileSize, y, ARect.Bottom - k);
      j := 0;
      while x < ARect.Right - k do
      begin
        xr := EnsureRange(x + TileSize, x, ARect.Right - k);
        ACanvas.Brush.Color := chk[Odd(i) = Odd(j)];
        ACanvas.FillRect(x, y, xr, yb);
        Inc(x, TileSize);
        Inc(j);
      end;
      Inc(y, TileSize);
      Inc(i);
    end;
  end;
  if AFrame then
  begin
    ACanvas.Brush.Color := FrameColor;
    ACanvas.FrameRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  end;
  ACanvas.Pen.Color := OldPen;
  ACanvas.Brush.Color := OldBrush;
end;

procedure RadialPoints(const angle, cx, cy, rMin, rMax: double;
  nPoints: integer; Data: PIFloatPoint);
var
  a, s, c, r: double;
  i: integer;
begin
  a := DegToRad(angle - 90);
  SinCos(a, s, c);
  for i := 0 to nPoints - 1 do
  begin
    r := rMin + (rMax - rMin) * i / (nPoints - 1);
    Data[i].X := cx + r * c;
    Data[i].Y := cy + r * s;
  end;
end;

procedure PaintBitmap32ToAggFPImage(bmp: TBitmap32; img: TAggFPImage);
var
  size: cardinal;
  s: PColor32Entry;
  p: PByte;
begin
  if (bmp = nil) or (img = nil) then
    exit;
  if (bmp.Width <> img.Width) or (bmp.Height <> img.Height) or
    (img.PixelFormat <> afpimRGBA32) then
    exit;
{$IFDEF RGBA_FORMAT}
  size := bmp.Width * bmp.Height * 4;
  Move(bmp.Bits^, img.Data^, size);
{$ELSE}
  // Swap R <-> B bytes
  size := bmp.Width * bmp.Height;
  s := PColor32Entry(bmp.Bits);
  p := img.Data;
  while Size > 0 do
  begin
    p^ := s^.R;
    Inc(p);
    p^ := s^.G;
    Inc(p);
    p^ := s^.B;
    Inc(p);
    p^ := s^.A;
    Inc(p);
    Inc(s);
    Dec(Size);
  end;
{$ENDIF}
end;


{ TIEmbededGraphics }

function TIEmbededGraphics.GetImageName: string;
var
  i: integer;
begin
  if fFileName.IsEmpty then
    exit;
  Result := ExtractFileNameWithoutExt(fFileName);
  if not Result.IsEmpty then
  begin
    Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
    for i := 1 to Length(Result) do
      if not (Result[i] in ['_', 'a'..'z', 'A'..'Z', '0'..'9']) then
        Result[i] := '_';
    if not (Result[1] in ['_', 'a'..'z', 'A'..'Z']) then
      Result.Insert(1, '_');
  end;
end;

procedure TIEmbededGraphics.Assign(Source: TPersistent);
begin
  if Source is TIEmbededGraphics then
  begin
    fFileName := TIEmbededGraphics(Source).fFileName;
  end
  else
    inherited Assign(Source);
end;

function TIEmbededGraphics.Clone: TIEmbededGraphics;
begin
  Result := nil;
end;

function TIEmbededGraphics.LoadImageFile(const aFileName: string): boolean;
begin
  fFileName := ExtractFileName(aFileName);
  Result := False;
end;

procedure TIEmbededGraphics.SaveToFile(const aFileName: string);
var
  s: TFileStream;
begin
  s := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;
end;


{ TIEmbededImage }

constructor TIEmbededImage.Create;
begin
  inherited Create;
  fImage := TAggFPImage.Create(0, 0);
  fImage.PixelFormat := afpimRGBA32;
  fStates := 1;
  fFramesAreHorizontal := False;
  fTargetScale := 0;
end;

destructor TIEmbededImage.Destroy;
begin
  FreeAndNil(fImage);
  inherited Destroy;
end;

function TIEmbededImage.Clone: TIEmbededGraphics;
begin
  Result := TIEmbededImage.Create;
  Result.Assign(Self);
end;

procedure TIEmbededImage.Assign(Source: TPersistent);
begin
  if Source is TIEmbededImage then
  begin
    fImage.Assign(TIEmbededImage(Source).fImage);
    fStates := TIEmbededImage(Source).fStates;
    fTargetScale := TIEmbededImage(Source).fTargetScale;
    fFramesAreHorizontal := TIEmbededImage(Source).fFramesAreHorizontal;
  end;
  inherited Assign(Source);
end;

function TIEmbededImage.LoadImageFile(const aFileName: string): boolean;
begin
  inherited LoadImageFile(aFileName);
  Result := fImage.LoadFromFile(aFileName);
end;

function TIEmbededImage.FrameWidth: integer;
begin
  Result := IfThen(fFramesAreHorizontal, Width div States, Width);
end;

function TIEmbededImage.FrameHeight: integer;
begin
  Result := IfThen(fFramesAreHorizontal, Height, Height div States);
end;

procedure TIEmbededImage.LoadFromStream(AStream: TStream);
var
  r: TFPReaderPNG;
begin
  r := TFPReaderPNG.Create;
  try
    fImage.LoadFromStream(AStream);
  finally
    r.Free;
  end;
end;

procedure TIEmbededImage.SaveToStream(AStream: TStream);
var
  w: TFPWriterPNG;
begin
  w := TFPWriterPNG.Create;
  try
    w.UseAlpha := True;
    w.WordSized := False;
    fImage.SaveToStream(AStream, w);
  finally
    w.Free;
  end;
end;

procedure TIEmbededImage.SetStates(AValue: integer);
begin
  fStates := Max(AValue, 1);
end;

procedure TIEmbededImage.SetTargetScale(AValue: integer);
begin
  fTargetScale := EnsureRange(AValue, 0, 10);
end;

function TIEmbededImage.GetHeight: integer;
begin
  Result := fImage.Height;
end;

function TIEmbededImage.GetWidth: integer;
begin
  Result := fImage.Width;
end;


{ TIEmbededSVG }

constructor TIEmbededSVG.Create;
begin
  fSVG := TSVG.Create;
end;

destructor TIEmbededSVG.Destroy;
begin
  FreeAndNil(fSVG);
  inherited Destroy;
end;

function TIEmbededSVG.Clone: TIEmbededGraphics;
begin
  Result := TIEmbededSVG.Create;
  Result.Assign(Self);
end;

procedure TIEmbededSVG.Assign(Source: TPersistent);
begin
  if Source is TIEmbededSVG then
  begin
    TIEmbededSVG(Source).fSVG.AssignTo(fSVG);
  end;
  inherited Assign(Source);
end;

function TIEmbededSVG.GetRealHeight: integer;
begin
  Result := Round(fSVG.RealHeight);
end;

function TIEmbededSVG.GetRealWidth: integer;
begin
  Result := Round(fSVG.RealWidth);
end;

function TIEmbededSVG.GetHeight: integer;
begin
  Result := Round(fSVG.HeightAsPixel);
end;

function TIEmbededSVG.GetWidth: integer;
begin
  Result := Round(fSVG.WidthAsPixel);
end;

function TIEmbededSVG.LoadImageFile(const aFileName: string): boolean;
var
  s: TFileStream;
begin
  inherited LoadImageFile(aFileName);
  try
    s := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  except
    Result := False;
    exit;
  end;
  try
    LoadFromStream(s);
  finally
    s.Free;
  end;
  Result := not fSVG.Source.IsEmpty;
end;

procedure TIEmbededSVG.LoadFromStream(AStream: TStream);
begin
  fSVG.LoadFromStream(AStream);
end;

procedure TIEmbededSVG.SaveToStream(AStream: TStream);
begin
  fSVG.SaveToStream(AStream);
end;

procedure TIEmbededSVG.RenderToBitmap(ABitmap: TCustomBitmap);
var
  bmp: TBitmap32;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(Width, Height);
    bmp.Clear($00000000);
    fSVG.RenderTo(bmp);
    ABitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TIEmbededSVG.RenderToMiniature(ABitmap: TCustomBitmap);
var
  bmp, b2: TBitmap32;
  rd: TRect;
  m: integer;
  w1, h1: TFloat;
  r: TFloatRect;
begin
  bmp := TBitmap32.Create;
  b2 := TBitmap32.Create;
  try
    bmp.Resampler := TLinearResampler.Create;
    bmp.SetSize(Width, Height);
    b2.SetSize(ABitmap.Width, ABitmap.Height);
    bmp.Clear($00000000);
    r := FloatRect(0, 0, fSVG.Width, fSVG.Height);
    fSVG.RenderTo(bmp);
    rd := Rect(0, 0, ABitmap.Width, ABitmap.Height);
    if Width < Height then
    begin
      w1 := (ABitmap.Height * Width) / Height;
      m := Round((ABitmap.Width - w1) / 2);
      rd.Left := m;
      rd.Right -= m;
    end;
    if Width > Height then
    begin
      h1 := (ABitmap.Width * Height) / Width;
      m := Round((ABitmap.Height - h1) / 2);
      rd.Top := m;
      rd.Bottom -= m;
    end;
    bmp.DrawTo(b2, rd);
    ABitmap.Assign(b2);
  finally
    bmp.Free;
    b2.Free;
  end;
end;

procedure TIEmbededSVG.RenderToPreview(ABitmap: TCustomBitmap);
var
  bmp, b2: TBitmap32;
  rd: TRect;
  m: integer;
  w1, h1: TFloat;
  r: TFloatRect;
  k: double;
begin
  bmp := TBitmap32.Create;
  b2 := TBitmap32.Create;
  try
    bmp.Resampler := TLinearResampler.Create;
    k := Min(Width / ABitmap.Width, Height / ABitmap.Height);
    if k = 0 then
      exit;
    bmp.SetSize(Round(Width / k), Round(Height / k));
    b2.SetSize(ABitmap.Width, ABitmap.Height);
    bmp.Clear($00000000);
    r := FloatRect(0, 0, fSVG.Width, fSVG.Height);
    fSVG.RenderTo(bmp);
    rd := Rect(0, 0, ABitmap.Width, ABitmap.Height);
    if Width < Height then
    begin
      w1 := (ABitmap.Height * Width) / Height;
      m := Round((ABitmap.Width - w1) / 2);
      rd.Left := m;
      rd.Right -= m;
    end;
    if Width > Height then
    begin
      h1 := (ABitmap.Width * Height) / Width;
      m := Round((ABitmap.Height - h1) / 2);
      rd.Top := m;
      rd.Bottom -= m;
    end;
    bmp.DrawTo(b2, rd);
    ABitmap.Assign(b2);
  finally
    bmp.Free;
    b2.Free;
  end;
end;

procedure TIEmbededSVG.RenderToStream(AStream: TStream);
var
  bmp: TBitmap32;
  r: TFloatRect;
  c: TFPColor;
  v: TColor32Entry;
  x, y: integer;
  img: TFPMemoryImage;
  wr: TFPWriterPNG;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(Width, Height);
    bmp.Clear($00000000);
    r := FloatRect(0, 0, fSVG.Width, fSVG.Height);
    fSVG.PaintTo(bmp, r, nil, 0);
    img := TFPMemoryImage.Create(Width, Height);
    try
      for y := 0 to img.Height - 1 do
        for x := 0 to img.Width - 1 do
        begin
          v.ARGB := bmp.Pixel[x, y];
          c.Red := (v.R shl 8) + v.R;
          c.green := (v.G shl 8) + v.G;
          c.blue := (v.B shl 8) + v.B;
          c.alpha := (v.A shl 8) + v.A;
          img.Colors[x, y] := c;
        end;
      wr := TFPWriterPNG.Create;
      try
        wr.WordSized := False;
        wr.UseAlpha := True;
        wr.ImageWrite(AStream, img);
      finally
        wr.Free;
      end;
    finally
      img.Free;
    end;
  finally
    bmp.Free;
  end;
end;

{ TIGraphicsObject }

procedure TIGraphicsObject.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TIGraphicsObject.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TIGraphicsObject.Changing;
begin
  if Assigned(fOnChanging) then
    fOnChanging(Self);
end;

{ TIPen }

constructor TIPen.Create;
begin
  inherited Create;
  fWidth := 1;
  fColor := iclBlack;
end;

procedure TIPen.Assign(Source: TPersistent);
begin
  if Source is TIPen then
  begin
    Width := TIPen(Source).fWidth;
    Color := TIPen(Source).fColor;
  end
  else
    inherited Assign(Source);
end;

procedure TIPen.SetColor(const AValue: TIColor);
begin
  if fColor = AValue then
    exit;
  Changing;
  fColor := AValue;
  Changed;
end;

procedure TIPen.SetWidth(const AValue: double);
begin
  if fWidth = AValue then
    exit;
  Changing;
  fWidth := AValue;
  Changed;
end;

function TIPen.GetWidth: double;
begin
  Result := fWidth;
end;

function TIPen.GetColor: TIColor;
begin
  Result := fColor;
end;

{ TIBrush }

constructor TIBrush.Create;
begin
  inherited Create;
  fColor := iclWhite;
end;

procedure TIBrush.Assign(Source: TPersistent);
begin
  if Source is TIBrush then
  begin
    Style := TIBrush(Source).fStyle;
    Color := TIBrush(Source).fColor;
  end
  else
    inherited Assign(Source);
end;

procedure TIBrush.SetColor(const AValue: TIColor);
begin
  if fColor = AValue then
    exit;
  Changing;
  fColor := AValue;
  Changed;
end;

procedure TIBrush.SetStyle(const AValue: TIBrushStyle);
begin
  if fStyle = AValue then
    exit;
  Changing;
  fStyle := AValue;
  Changed;
end;

function TIBrush.GetColor: TIColor;
begin
  Result := fColor;
end;

function TIBrush.GetStyle: TIBrushStyle;
begin
  Result := fStyle;
end;


{ TIFont }

constructor TIFont.Create;
begin
  inherited Create;
  fFamily := DEFAULT_FONT;
  fSize := DEFAULT_TEXT_SIZE;
  fColor := iclBlack;
  fAlign := Center;
  fVAlign := Middle;
end;

procedure TIFont.Assign(Source: TPersistent);
begin
  if Source is TIFont then
  begin
    Size := TIFont(Source).fSize;
    Color := TIFont(Source).fColor;
    Style := TIFont(Source).fStyle;
    Align := TIFont(Source).fAlign;
    VAlign := TIFont(Source).fVAlign;
    Family := TIFont(Source).fFamily;
    Angle := TIFont(Source).fAngle;
  end
  else
    inherited Assign(Source);
end;

procedure TIFont.AssignFont(AValue: TFont);
begin
  Changing;
  fFamily := AValue.Name;
  fColor := ColorToIColor(AValue.Color);
  fSize := AValue.Size;
  if fsBold in AValue.Style then
    Include(fStyle, Bold)
  else
    Exclude(fStyle, Bold);
  if fsItalic in AValue.Style then
    Include(fStyle, Italic)
  else
    Exclude(fStyle, Italic);
  Changed;
end;

procedure TIFont.AssignToFont(AValue: TFont);
begin
  AValue.Name := fFamily;
  AValue.Color := IColorToColorRef(fColor);
  AValue.Size := Round(fSize);
  AValue.Style := [];
  if Bold in fStyle then
    AValue.Style := [fsBold];
  if Italic in fStyle then
    AValue.Style := AValue.Style + [fsItalic];
end;

procedure TIFont.LoadFromIni(ini: TIniFileEx; const Section: string);
begin
  fFamily := ini.ReadString(Section, 'Family', DEFAULT_FONT);
  fColor := ini.ReadHex(Section, 'Color', iclBlack);
  fSize := ini.ReadFloat(Section, 'Size', DEFAULT_TEXT_SIZE);
  fAlign := TITextAlign(GetEnumValue(TypeInfo(TITextAlign),
    ini.ReadString(Section, 'Align', 'Center')));
  fVAlign := TITextVAlign(GetEnumValue(TypeInfo(TITextVAlign),
    ini.ReadString(Section, 'VAlign', 'Middle')));
  fStyle := TIFontStyle(StringToSet(PTypeInfo(TypeInfo(TIFontStyle)),
    ini.ReadString(Section, 'Style', '')));
end;

procedure TIFont.SaveToIni(ini: TIniFileEx; const Section: string);
begin
  ini.WriteString(Section, 'Family', fFamily);
  ini.WriteHex(Section, 'Color', fColor);
  ini.WriteFloat(Section, 'Size', fSize);
  ini.WriteString(Section, 'Align', GetEnumName(TypeInfo(TITextAlign), Ord(fAlign)));
  ini.WriteString(Section, 'VAlign', GetEnumName(TypeInfo(TITextVAlign), Ord(fVAlign)));
  ini.WriteString(Section, 'Style', SetToString(PTypeInfo(TypeInfo(TIFontStyle)),
    integer(fStyle), True));
end;

procedure TIFont.SetColor(const AValue: TIColor);
begin
  if fColor = AValue then
    exit;
  Changing;
  fColor := AValue;
  Changed;
end;

procedure TIFont.SetFamily(const AValue: TFontName);
begin
  if fFamily = AValue then
    exit;
  Changing;
  fFamily := AValue;
  Changed;
end;

procedure TIFont.SetSize(const AValue: double);
begin
  if fSize = AValue then
    exit;
  Changing;
  fSize := AValue;
  Changed;
end;

procedure TIFont.SetStyle(const AValue: TIFontStyle);
begin
  if fStyle = AValue then
    exit;
  Changing;
  fStyle := AValue;
  Changed;
end;

procedure TIFont.SetAlign(const AValue: TITextAlign);
begin
  if fAlign = AValue then
    exit;
  Changing;
  fAlign := AValue;
  Changed;
end;

procedure TIFont.SetVAlign(const AValue: TITextVAlign);
begin
  if fVAlign = AValue then
    exit;
  Changing;
  fVAlign := AValue;
  Changed;
end;

procedure TIFont.SetAngle(const AValue: double);
begin
  if fAngle = AValue then
    exit;
  Changing;
  fAngle := AValue;
  Changed;
end;

function TIFont.GetSize: double;
begin
  Result := fSize;
end;

function TIFont.GetFamily: string;
begin
  Result := fFamily;
end;

function TIFont.GetColor: TIColor;
begin
  Result := fColor;
end;

function TIFont.GetStyle: TIFontStyle;
begin
  Result := fStyle;
end;

function TIFont.GetAlign: TITextAlign;
begin
  Result := fAlign;
end;

function TIFont.GetVAlign: TITextVAlign;
begin
  Result := fVAlign;
end;

function TIFont.GetAngle: double;
begin
  Result := fAngle;
end;


{ TIGraphicsSettings }

constructor TIGraphicsSettings.Create;
begin
  inherited Create;
  fFont := TIFont.Create;
  fFont.OnChange := @ParamChanged;
  fPen := TIPen.Create;
  fPen.OnChange := @ParamChanged;
  fBrush := TIBrush.Create;
  fBrush.OnChange := @ParamChanged;
end;

destructor TIGraphicsSettings.Destroy;
begin
  fBrush.Free;
  fPen.Free;
  fFont.Free;
  inherited Destroy;
end;

procedure TIGraphicsSettings.Assign(Source: TPersistent);
begin
  if Source is TIGraphicsSettings then
  begin
    fPen.Assign(TIGraphicsSettings(Source).fPen);
    fFont.Assign(TIGraphicsSettings(Source).fFont);
    fBrush.Assign(TIGraphicsSettings(Source).fBrush);
  end
  else
    inherited Assign(Source);
end;

procedure TIGraphicsSettings.ParamChanged(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Sender);
end;

procedure TIGraphicsSettings.SetFont(const Value: TIFont);
begin
  fFont.Assign(Value);
end;

procedure TIGraphicsSettings.SetBrush(const Value: TIBrush);
begin
  fBrush.Assign(Value);
end;

procedure TIGraphicsSettings.SetPen(const Value: TIPen);
begin
  fPen.Assign(Value);
end;


{ TICanvas }

constructor TICanvas.Create(ACanvas: TAggCanvas; ADesigner: TObject);
begin
  inherited Create;
  fDesigner := ADesigner;
  fCanvas := TAggCanvas.Create;
  fCanvas.Image.Assign(ACanvas.Image);
  Init;
end;

constructor TICanvas.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  fDesigner := nil;
  fCanvas := TAggCanvas.Create;
  fCanvas.Image.SetSize(AWidth, AHeight);
  fCanvas.Image.PixelFormat := afpimRGBA32;
  Init;
end;

procedure TICanvas.Init;
begin
  fGraphicsSettings := TIGraphicsSettings.Create;
  fGraphicsSettings.OnChange := @GraphicsSettingsChanged;
end;

destructor TICanvas.Destroy;
begin
  fGraphicsSettings.Free;
  fCanvas.Free;
  inherited Destroy;
end;

procedure TICanvas.Clear;
begin
  fCanvas.Clear;
end;

procedure TICanvas.DrawCheckboard(const TileSize: integer);
const
  Colors: array[boolean] of byte = ($FF, $CC);
var
  i, j, x, y, OddY, TilesHorz, TilesVert, TileX, TileY: integer;
  p: PByte;
begin
  TilesHorz := fCanvas.Image.Width div TileSize;
  TilesVert := fCanvas.Image.Height div TileSize;
  TileY := 0;
  for j := 0 to TilesVert do
  begin
    TileX := 0;
    OddY := j and $1;
    for i := 0 to TilesHorz do
    begin
      for x := TileX to TileX + TileSize - 1 do
      begin
        if x = fCanvas.Image.Width then
          break;
        for y := TileY to tileY + TileSize - 1 do
        begin
          if y = fCanvas.Image.Height then
            break;
          p := @fCanvas.Image.Data[(y * fCanvas.Image.Width + x) * 4];
          p[0] := Colors[i and $1 = OddY];
          p[1] := Colors[i and $1 = OddY];
          p[2] := Colors[i and $1 = OddY];
          p[3] := $FF;
        end;
      end;
      Inc(TileX, TileSize);
    end;
    Inc(TileY, TileSize);
  end;
end;

procedure TICanvas.GraphicsSettingsChanged(Sender: TObject);
begin
  if Sender = fGraphicsSettings.Font then
    Exclude(fState, csFontValid);
  if Sender = fGraphicsSettings.Pen then
  begin
    Exclude(fState, csPenValid);
    if fGraphicsSettings.Brush.Style > TIBrushStyle.Clear then
      Exclude(fState, csBrushValid);
  end;
  if Sender = fGraphicsSettings.Brush then
    Exclude(fState, csBrushValid);
end;

procedure TICanvas.SetBrushColor(AValue: TIColor);
begin
  fGraphicsSettings.Brush.Color := AValue;
end;

function TICanvas.GetBrushColor: TIColor;
begin
  Result := fGraphicsSettings.Brush.Color;
end;

function TICanvas.GetFontAlign: TITextAlign;
begin
  Result := fGraphicsSettings.Font.Align;
end;

function TICanvas.GetFontAngle: double;
begin
  Result := fGraphicsSettings.Font.Angle;
end;

function TICanvas.GetFontColor: TIColor;
begin
  Result := fGraphicsSettings.Font.Color;
end;

function TICanvas.GetFontVAlign: TITextVAlign;
begin
  Result := fGraphicsSettings.Font.VAlign;
end;

procedure TICanvas.SetPenColor(AValue: TIColor);
begin
  fGraphicsSettings.Pen.Color := AValue;
end;

function TICanvas.GetPenColor: TIColor;
begin
  Result := fGraphicsSettings.Pen.Color;
end;

procedure TICanvas.SetPenWidth(AValue: double);
begin
  fGraphicsSettings.Pen.Width := AValue;
end;

function TICanvas.GetPenWidth: double;
begin
  Result := fGraphicsSettings.Pen.Width;
end;

function TICanvas.GetMasterAlpha: double;
begin
  Result := fCanvas.AggMasterAlpha;
end;

procedure TICanvas.SetGraphicsSettings(const AValue: TIGraphicsSettings);
begin
  fGraphicsSettings.Assign(AValue);
end;

procedure TICanvas.SetMasterAlpha(AValue: double);
begin
  fCanvas.AggMasterAlpha := AValue;
end;

procedure TICanvas.RequireState(AState: TICanvasState);
var
  LState: TICanvasState;
begin
  LState := AState - fState;
  if LState <> [] then
  begin
    if csFontValid in LState then
      UpdateFont;
    if csPenValid in LState then
      UpdatePen;
    if csBrushValid in LState then
      UpdateBrush;
    fState := fState + LState;
  end;
end;

procedure TICanvas.UpdateFont;
begin
  fCanvas.SetupFont(GraphicsSettings.Font);
end;

procedure TICanvas.UpdatePen;
begin
  with GraphicsSettings.Pen do
  begin
    fCanvas.Pen.AggColor := IColorToAggColor(Color);
    fCanvas.Pen.AggLineWidth := Width;
  end;
end;

procedure TICanvas.UpdateBrush;
begin
  with GraphicsSettings.Brush do
  begin
    if Style = TIBrushStyle.Solid then
      fCanvas.Brush.AggColor := IColorToAggColor(Color)
    else
      fCanvas.Brush.AggColor := ColorToAggColor(0, 0);
  end;
end;

procedure TICanvas.DrawLine(X1, Y1, X2, Y2: double);
begin
  RequireState([csPenValid]);
  fCanvas.AggLine(X1, Y1, X2, Y2);
end;

procedure TICanvas.DrawRect(const R: TIFloatRect);
begin
  RequireState([csPenValid, csBrushValid]);
  with R do
    fCanvas.AggRectangle(Left, Top, Right, Bottom);
end;

procedure TICanvas.DrawRect(X1, Y1, X2, Y2: double);
begin
  RequireState([csPenValid, csBrushValid]);
  fCanvas.AggRectangle(X1, Y1, X2, Y2);
end;

procedure TICanvas.DrawRoundedRect(const R: TIFloatRect; rd: double);
begin
  RequireState([csPenValid, csBrushValid]);
  with R do
    if rd = 0.0 then
      fCanvas.AggRectangle(Left, Top, Right, Bottom)
    else
      fCanvas.AggRoundedRect(Left, Top, Right, Bottom, rd);
end;

procedure TICanvas.DrawRoundedRect(X1, Y1, X2, Y2, r: double);
begin
  RequireState([csPenValid, csBrushValid]);
  if r = 0.0 then
    fCanvas.AggRectangle(X1, Y1, X2, Y2)
  else
    fCanvas.AggRoundedRect(X1, Y1, X2, Y2, r);
end;

procedure TICanvas.DrawRoundedRect(const R: TIFloatRect; rTL, rTR, rBR, rBL: double);
var
  rc: rounded_rect;

  procedure normalize_radius;
  var
    dx, dy, k, t: double;
  begin
    dx := Abs(rc.m_y2 - rc.m_y1);
    dy := Abs(rc.m_x2 - rc.m_x1);
    k := 1.0;
    if (rc.m_rx1 + rc.m_rx2) <> 0 then
    begin
      t := dx / (rc.m_rx1 + rc.m_rx2);
      if t < k then
        k := t;
    end;
    if (rc.m_rx3 + rc.m_rx4) <> 0 then
    begin
      t := dx / (rc.m_rx3 + rc.m_rx4);
      if t < k then
        k := t;
    end;
    if (rc.m_ry1 + rc.m_ry2) <> 0 then
    begin
      t := dy / (rc.m_ry1 + rc.m_ry2);
      if t < k then
        k := t;
    end;
    if (rc.m_ry3 + rc.m_ry4) <> 0 then
    begin
      t := dy / (rc.m_ry3 + rc.m_ry4);
      if t < k then
        k := t;
    end;
    if k < 1.0 then
    begin
      rc.m_rx1 *= k;
      rc.m_ry1 *= k;
      rc.m_rx2 *= k;
      rc.m_ry2 *= k;
      rc.m_rx3 *= k;
      rc.m_ry3 *= k;
      rc.m_rx4 *= k;
      rc.m_ry4 *= k;
    end;
  end;

begin
  RequireState([csPenValid, csBrushValid]);
  if (rTL = 0) and (rTR = 0) and (rBR = 0) and (rBL = 0) then
  begin
    DrawRect(R);
    exit;
  end;
  fCanvas.Path.m_path.remove_all;
  rc.Construct;
  with R do
    rc.rect(Left, Top, Right, Bottom);
  rc.radius(rTL, rTL, rTR, rTR, rBR, rBR, rBL, rBL);
  normalize_radius; // standart method call exception on radius = 0
  rc.approximation_scale_(fCanvas.AggWorldToScreen(1.0) * 2.0);
  fCanvas.Path.m_path.add_path(@rc, 0, False);
  fCanvas.AggDrawPath(AGG_FillAndStroke);
end;

procedure TICanvas.DrawEllipse(X1, Y1, X2, Y2: double);
var
  cx, cy, rx, ry: double;
begin
  RequireState([csPenValid, csBrushValid]);
  cx := (X1 + X2) * 0.5;
  cy := (Y1 + Y2) * 0.5;
  rx := (X2 - X1) * 0.5;
  ry := (Y2 - Y1) * 0.5;
  fCanvas.AggEllipse(cx, cy, rx, ry);
end;

procedure TICanvas.DrawEllipse(const R: TIFloatRect);
begin
  RequireState([csPenValid, csBrushValid]);
  with R do
    fCanvas.AggEllipse(MW, MH, HW, HH);
end;

procedure TICanvas.DrawArc(const cx, cy, r, a1, a2: double);
begin
  RequireState([csPenValid]);
  fCanvas.AggArc(cx, cy, r, r, DegToRad(a1 - 90), DegToRad(a2 - 90));
end;

procedure TICanvas.DrawRadialLine(const cx, cy, angle, rMin, rMax: double);
var
  Data: array[0..1] of TIFloatPoint;
begin
  RequireState([csPenValid]);
  RadialPoints(angle, cx, cy, rMin, rMax, 2, @Data[0]);
  DrawLine(Data[0].X, Data[0].Y, Data[1].X, Data[1].Y);
end;

procedure TICanvas.DrawGroupboxFrame(const R: TIFloatRect;
  const ll, lr, cr, hft: double);

  procedure PathArc(const cx, cy, r, a1, a2: double);
  var
    ar: agg_arc.arc;
  begin
    if r = 0 then
      exit;
    ar.Construct(cx, cy, r, r, DegToRad(a1 - 90), DegToRad(a2 - 90));
    fCanvas.Path.m_path.add_path(@ar, 0, False);
  end;

begin
  RequireState([csPenValid]);
  fCanvas.Path.m_path.remove_all;
  fCanvas.Path.m_path.move_to(lr, R.Top + hft);
  fCanvas.Path.m_path.line_to(R.Right - cr - hft, R.Top + hft);
  PathArc(R.Right - cr - hft, R.Top + cr + hft, cr, 0, 90);
  fCanvas.Path.m_path.line_to(R.Right - hft, R.Bottom - cr - hft);
  PathArc(R.Right - cr - hft, R.Bottom - cr - hft, cr, 90, 180);
  fCanvas.Path.m_path.line_to(R.Left + cr + hft, R.Bottom - hft);
  PathArc(R.Left + cr + hft, R.Bottom - cr - hft, cr, 180, 270);
  fCanvas.Path.m_path.line_to(R.Left + hft, R.Top + cr + hft);
  PathArc(R.Left + cr + hft, R.Top + cr + hft, cr, 270, 360);
  fCanvas.Path.m_path.line_to(ll, R.Top + hft);
  fCanvas.AggDrawPath(AGG_StrokeOnly);
end;

procedure TICanvas.DrawText(Text: string; ARect: TIFloatRect; Underline: boolean);
const
  TXT_PAD = 3;
var
  x, y, h, x1, y1, x2, y2, wt: double;
  r: TAggRectD;
  ct: TAggColor;
begin
  if Text.Length = 0 then
    exit;
  RequireState([csFontValid]);
  Text := StrToFauIcon(Text); // for fontaudio
  case GraphicsSettings.Font.Align of
    Near: x := ARect.Left;
    Center: x := ARect.Left + (ARect.Right - ARect.Left -
        fCanvas.AggTextWidth(Text)) * 0.5;
    Far: x := ARect.Right - fCanvas.AggTextWidth(Text);
  end;
  h := fCanvas.AggFontHeight;
  case GraphicsSettings.Font.VAlign of
    Top: y := ARect.Top + TXT_PAD;
    Middle: y := (ARect.Bottom + ARect.Top) * 0.5 - h * 0.5;
    Bottom: y := ARect.Bottom - h - TXT_PAD;
  end;
  r := fCanvas.AggGetClipBox;
  try
    x1 := ARect.Left - TXT_PAD;
    y1 := ARect.Top - TXT_PAD;
    x2 := ARect.Right + TXT_PAD;
    y2 := ARect.Bottom + TXT_PAD;
    fCanvas.AggWorldToScreen(@x1, @y1);
    fCanvas.AggWorldToScreen(@x2, @y2);
    fCanvas.AggSetClipBox(x1, y1, x2, y2);
    fCanvas.RenderText(x, y, Text);
    if Underline then
    begin
      ct := fCanvas.Pen.AggColor;
      wt := fCanvas.Pen.AggLineWidth;
      fCanvas.Pen.AggColor := fCanvas.Font.AggColor;
      fCanvas.Pen.AggLineWidth := fCanvas.Font.Size / 14;
      y += fCanvas.Font.AggHeight - fCanvas.Pen.AggLineWidth;
      fCanvas.AggLine(x, y, x + fCanvas.AggTextWidth(Text) + 1, y);
      fCanvas.Pen.AggColor := ct;
      fCanvas.Pen.AggLineWidth := wt;
    end;
  finally
    fCanvas.AggSetClipBox(r.x1, r.y1, r.x2, r.y2);
  end;
end;

procedure TICanvas.MeasureText(Text: string; ARect: TIFloatRect;
  var TextRect: TIFloatRect);
begin
  RequireState([csFontValid]);
  TextRect.Left := ARect.Left;
  TextRect.Top := ARect.Top;
  TextRect.Right := ARect.Left + fCanvas.AggTextWidth(Text);
  TextRect.Bottom := ARect.Top + fCanvas.AggTextHeight(Text);
end;

procedure TICanvas.DrawPolyline(Pts: PIFloatPoint; Count: integer);
var
  Points: array of double;
  i, n: integer;
begin
  n := Count * 2;
  SetLength(Points, n);
  i := 0;
  while i < n do
  begin
    Points[i] := Pts^.X;
    Inc(i);
    Points[i] := Pts^.Y;
    Inc(i);
    Inc(Pts);
  end;
  RequireState([csPenValid]);
  fCanvas.AggPolyline(@Points[0], Count);
  Points := nil;
end;

procedure TICanvas.DrawPolygon(Pts: PIFloatPoint; Count: integer);
var
  Points: array of double;
  i, n: integer;
begin
  n := Count * 2;
  SetLength(Points, n);
  i := 0;
  while i < n do
  begin
    Points[i] := Pts^.X;
    Inc(i);
    Points[i] := Pts^.Y;
    Inc(i);
    Inc(Pts);
  end;
  RequireState([csPenValid, csBrushValid]);
  fCanvas.AggPolygon(@Points[0], Count);
  Points := nil;
end;

procedure TICanvas.DrawImage(image: TIEmbededImage; X, Y: integer; Dst: TIFloatRect);
begin
  if Dst.IsEmpty then
    exit;
  with Dst do
    fCanvas.AggTransformImage(image.Image, X, Y, X + image.FrameWidth,
      Y + image.FrameHeight, Left, Top, Right, Bottom);
end;

procedure TICanvas.DrawImage(image: TIEmbededImage; const ARect: TIFloatRect;
  AState: integer);
var
  x, y: integer;
begin
  x := 0;
  y := 0;
  AState := EnsureRange(AState, 1, image.States);
  if (image.States > 1) and (AState > 1) then
  begin
    if image.FramesAreHorizontal then
      x := MulDiv(image.Width, AState - 1, image.States)
    else
      y := MulDiv(image.Height, AState - 1, image.States);
  end;
  DrawImage(image, x, y, ARect);
end;

procedure TICanvas.DrawImage(image: TIEmbededImage; ARect: TIFloatRect);
begin
  if ARect.IsEmpty then
    exit;
  with ARect do
    fCanvas.AggTransformImage(image.Image, Left, Top, Right, Bottom);
end;

procedure TICanvas.DrawImage(image: TAggFPImage; ARect: TIFloatRect);
begin
  if ARect.IsEmpty then
    exit;
  with ARect do
    fCanvas.AggTransformImage(image, Left, Top, Right, Bottom);
end;

procedure TICanvas.DrawRotatedImage(image: TIEmbededImage; const cx, cy,
  angle: double);
var
  x, y, w, h: double;
  tt: TAggTransformations;
begin
  w := image.Width * 0.5;
  h := image.Height * 0.5;
  x := cx; y := cy;
  tt := fCanvas.AggTransformations;
  try
    fCanvas.AggWorldToScreen(@x, @y);
    fCanvas.AggTranslate(-x, -y);
    fCanvas.AggRotate(DegToRad(angle));
    fCanvas.AggTranslate(x, y);
    fCanvas.AggTransformImage(image.Image, cx - w, cy - h, cx + w, cy + h);
  finally
    fCanvas.AggTransformations := tt;
  end;
end;

procedure TICanvas.DrawTo(ACanvas: TAggCanvas);
begin
  ACanvas.Draw(0, 0, fCanvas.Image);
end;

procedure TICanvas.DrawTo(ABitmap: TBitmap);
begin
  ABitmap.LoadFromIntfImage(fCanvas.Image.IntfImg);
end;

procedure TICanvas.TransformSpace(srcRect, dstRect: TIFloatRect);
begin
  if (srcRect.Right - srcRect.Left = 0) and (dstRect.Right - dstRect.Left = 0) or
    (srcRect.Bottom - srcRect.Top = 0) and (dstRect.Bottom - dstRect.Top = 0) then
    Exit;
  fCanvas.AggViewport(srcRect.Left, srcRect.Top, srcRect.Right, srcRect.Bottom,
    dstRect.Left, dstRect.Top, dstRect.Right, dstRect.Bottom);
end;

function TICanvas.GetClipRect: TIFloatRect;
var
  r: TAggRectD;
begin
  r := fCanvas.AggGetClipBox;
  if Assigned(fDesigner) then
  begin
    TIDesigner(fDesigner).ScreenToLog(r.x1, r.y1, Result.Left, Result.Top);
    TIDesigner(fDesigner).ScreenToLog(r.x2, r.y2, Result.Right, Result.Bottom);
  end
  else
  begin
    fCanvas.AggScreenToWorld(@r.x1, @r.y1);
    fCanvas.AggScreenToWorld(@r.x2, @r.y2);
    Result := TIFloatRect.Create(r);
  end;
end;

procedure TICanvas.SetClipRect(AValue: TIFloatRect);
var
  x1, x2, y1, y2: double;
begin
  with AValue do
  begin
    if Assigned(fDesigner) then
    begin
      TIDesigner(fDesigner).LogToScreen(Left, Top, x1, y1);
      TIDesigner(fDesigner).LogToScreen(Right, Bottom, x2, y2);
      fCanvas.AggSetClipBox(x1, y1, x2, y2);
    end
    else
    begin
      fCanvas.AggWorldToScreen(@Left, @Top);
      fCanvas.AggWorldToScreen(@Right, @Bottom);
      fCanvas.AggSetClipBox(Left, Top, Right, Bottom);
    end;
  end;
end;

procedure TICanvas.SetFontAngle(AValue: double);
begin
  fGraphicsSettings.Font.Angle := AValue;
end;

procedure TICanvas.SetFontColor(AValue: TIColor);
begin
  fGraphicsSettings.Font.Color := AValue;
end;

procedure TICanvas.SetFontAlign(AValue: TITextAlign);
begin
  fGraphicsSettings.Font.Align := AValue;
end;

procedure TICanvas.SetFontVAlign(AValue: TITextVAlign);
begin
  fGraphicsSettings.Font.VAlign := AValue;
end;


{ TImageCache }

constructor TImageCache.Create(AOwner: TObject);
begin
  fOwner := AOwner;
  fImages := TObjectList.Create(True);
  fImages.FPOAttachObserver(fOwner);
end;

destructor TImageCache.Destroy;
begin
  fImages.FPODetachObserver(fOwner);
  FreeAndNil(fImages);
  inherited Destroy;
end;

procedure TImageCache.Clear;
begin
  fImages.Clear;
end;

procedure TImageCache.AddImage(aImage: TIEmbededGraphics);
var
  img: TIEmbededGraphics;
begin
  img := aImage.Clone;
  fImages.Add(img);
end;

procedure TImageCache.GetCodeStr(Lst: TStrings; AsSVG: boolean);
var
  i: integer;
  s, fn: string;
  img: TIEmbededImage;
begin
  for i := 0 to ImagesCount - 1 do
  begin
    fn := Images[i].FileName;
    s := Images[i].ImageName;
    if AsSVG then
      Lst.Add(Format(#9'const ISVG %s = pGraphics->LoadSVG("%s");', [s, fn]))
    else
    begin
      if not (Images[i] is TIEmbededImage) then
        continue;
      img := TIEmbededImage(Images[i]);
      if (img.States > 1) or img.FramesAreHorizontal or (img.TargetScale <> 0) then
      begin
        if img.FramesAreHorizontal or (img.TargetScale <> 0) then
        begin
          if img.TargetScale <> 0 then
            Lst.Add(Format(
              #9'const IBitmap %s = pGraphics->LoadBitmap("%s", %d, %s, %d);',
              [s, fn, img.States, BooleanToStr(img.FramesAreHorizontal),
              img.TargetScale]))
          else
          if img.FramesAreHorizontal then
            Lst.Add(Format(#9'const IBitmap %s = pGraphics->LoadBitmap("%s", %d, true);',
              [s, fn, img.States]));
        end
        else
          Lst.Add(Format(#9'const IBitmap %s = pGraphics->LoadBitmap("%s", %d);',
            [s, fn, img.States]));
      end
      else
        Lst.Add(Format(#9'const IBitmap %s = pGraphics->LoadBitmap("%s");', [s, fn]));
    end;
  end;
  Lst.Add('');
end;

function TImageCache.ImageExists(AName: string): boolean;
begin
  Result := ImageIndex[AName] > -1;
end;

function TImageCache.GetImage(AIndex: integer): TIEmbededGraphics;
begin
  if InRange(AIndex, 0, fImages.Count - 1) then
    Result := TIEmbededGraphics(fImages[AIndex])
  else
    Result := nil;
end;

function TImageCache.GetImageByName(AName: string): TIEmbededGraphics;
var
  Index: integer;
begin
  Index := ImageIndex[AName];
  if Index > -1 then
    Result := Images[Index]
  else
    //raise EIException.CreateResFmt(@SImageNotFound, [AName]);
    Result := nil;
end;

function TImageCache.GetImageIndex(AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to ImagesCount - 1 do
    if SameText(Images[i].FileName, AName) then
    begin
      Result := i;
      Break;
    end;
end;

function TImageCache.GetImagesCount: integer;
begin
  Result := fImages.Count;
end;

procedure TImageCache.CustomSerialize(const Node: ITreeNode);
var
  i: integer;
  m: TIEmbededGraphics;
  n: ITreeNode;
  w: TFPWriterPNG;
  s: TMemoryStream;
  o: olevariant;
begin
  Node.AddParam('ImagesCount', fImages.Count);
  for i := 0 to ImagesCount - 1 do
  begin
    m := Images[i];
    Assert(Assigned(m), 'Image is nil');
    n := Node.AddNode(Format('Image%d', [i]));
    n.AddParam('ClassName', m.ClassName);
    n.AddParam('FileName', m.FileName);
    if m is TIEmbededImage then
    begin
      n.AddParam('States', TIEmbededImage(m).States);
      n.AddParam('TargetScale', TIEmbededImage(m).TargetScale);
      n.AddParam('FramesAreHorizontal', TIEmbededImage(m).FramesAreHorizontal);
    end;
    s := TMemoryStream.Create;
    try
      m.SaveToStream(s);
      s.Seek(0, 0);
      o := MemoryStreamToOleVariant(s);
      n.AddParam('Stream', o);
    finally
      s.Free;
    end;
  end;
end;

procedure TImageCache.CustomDeSerialize(const Node: ITreeNode);
var
  i, x: integer;
  n: ITreeNode;
  f, c: string;
  s: TMemoryStream;
  o: olevariant;
  img: TIEmbededGraphics;
begin
  x := Node.GetParamValueByName('ImagesCount');
  for i := 0 to x - 1 do
  begin
    n := Node.GetNodeByName(Format('Image%d', [i]));
    if Assigned(n) then
    begin
      f := n.GetParamValueByName('FileName');
      c := n.GetParamValueByName('ClassName');
      o := n.GetParamValueByName('Stream');
      if VarIsArray(o) then
      begin
        img := nil;
        if c = 'TIEmbededImage' then
        begin
          img := TIEmbededImage.Create;
          TIEmbededImage(img).fStates := n.GetParamValueByName('States');
          TIEmbededImage(img).fTargetScale := n.GetParamValueByName('TargetScale');
          TIEmbededImage(img).fFramesAreHorizontal := n.GetParamValueByName('FramesAreHorizontal');
        end else
          if c = 'TIEmbededSVG' then
            img := TIEmbededSVG.Create;
        if Assigned(img) then
        begin
          img.fFileName := f;
          s := OleVariantToMemoryStream(o);
          try
            s.Seek(0, 0);
            img.LoadFromStream(s);
            fImages.Add(img);
          finally
            s.Free;
          end;
        end;
      end;
    end;
  end;
end;


end.
