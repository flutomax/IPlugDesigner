{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIMiscControls;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, LMessages, Classes, SysUtils, Controls, StdCtrls, Graphics,
  Buttons, Dialogs, ImgList, uCommon, uGraphics;

type

  TIObjButton = class(TSpeedButton)
  public
    ObjClass: TClass;
  end;

  { TColorButtonEx }

  TColorButtonEx = class(TColorButton)
  private
    fCheckBmp: TBitmap;
    fColorBmp: TBitmap;
    fSelectedColor: TIColor;
    procedure SetSelectedColor(AValue: TIColor);
    procedure CreateCheckBmp(const AWidth, AHeight: integer);
  protected
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; override;
    procedure ShowColorDialog; override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; Override;
  published
    property SelectedColor: TIColor read fSelectedColor write SetSelectedColor;
  end;

  { TIScrollBox }

  TIScrollBox = class(TCustomControl)
  private
    fOriginX: Integer;
    fOriginY: Integer;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
  protected
    procedure WndProc(var Msg: TLMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure UpdateScrollBars;
    function AreaWidth: Integer; virtual; abstract;
    function AreaHeight: Integer; virtual; abstract;
    function ScrollHLine: Integer; virtual; abstract;
    function ScrollVLine: Integer; virtual; abstract;
    property OriginX: Integer read fOriginX;
    property OriginY: Integer read fOriginY;
  public
    procedure ScrollBy(Dx, Dy: Integer);
  end;


  procedure DrawDisabledImagelist(src, dst: TCustomImageList);

implementation

uses
  Types, Math, FPImage, IntfGraphics, GraphType, uFrmColorDialog;


procedure DrawDisabledImagelist(src, dst: TCustomImageList);
var
  i, x, y: integer;
  b: TBitmap;
  m: TLazIntfImage;
  fd: TRawImageDescription;
  c: TFPColor;
  ih, mh: HBitmap;
begin
  dst.Clear;
  b := TBitmap.Create;
  m := TLazIntfImage.Create(0, 0);
  try
    b.Width := src.Width;
    b.Height := src.Height;
    fd.Init_BPP32_B8G8R8_BIO_TTB(b.Width, b.Height);
    m.DataDescription := fd;
    for i := 0 to src.Count - 1 do
    begin
      src.GetBitmap(i, b);
      m.LoadFromBitmap(b.Handle, b.MaskHandle);
      for y := 0 to m.Height - 1 do
        for x := 0 to m.Width - 1 do
        begin
          c := m.Colors[x, y];
          c.alpha := c.alpha div 3;
          m.Colors[x, y] := c;
        end;
      m.CreateBitmaps(ih, mh, False);
      b.SetHandles(ih, mh);
      dst.Add(b, nil);
    end;
  finally
    b.Free;
    m.Free;
  end;
end;

{ TColorButtonEx }

constructor TColorButtonEx.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  fCheckBmp := TBitmap.Create;
  fColorBmp := TBitmap.Create;
  fColorBmp.PixelFormat := pf32bit;
end;

destructor TColorButtonEx.Destroy;
begin
  FreeAndNil(fCheckBmp);
  FreeAndNil(fColorBmp);
  inherited Destroy;
end;

procedure TColorButtonEx.SetSelectedColor(AValue: TIColor);
begin
  if fSelectedColor = AValue then
    exit;
  fSelectedColor := AValue;
  Invalidate;
end;

procedure TColorButtonEx.CreateCheckBmp(const AWidth, AHeight: integer);
var
  x, y, i: Integer;
begin
  fCheckBmp.SetSize(AWidth, AHeight);
  fColorBmp.SetSize(AWidth, AHeight);
  with fCheckBmp.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClipRect);
    Brush.Color := $CCCCCC;
    y := 0;
    i := 0;
    while y < AHeight do
    begin
      x := IfThen(Odd(i), 4, 0);
      while x < AWidth do
      begin
        FillRect(x, y, x + 4, y + 4);
        inc(x, 8);
      end;
      inc(y, 4);
      inc(i);
    end;
  end;
end;

function TColorButtonEx.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  Size: TSize;
  a, r, g, b: byte;
  i, j: integer;
  p: PByteArray;
begin
  Size := GetGlyphSize(true, AClient);
  Result := Bounds(AClient.Left + AOffset.X, AClient.Top + AOffset.Y,
    Size.CX - 1, Size.CY - 1);
  Canvas.Pen.Color := clBlack;
  if AState = bsDisabled then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Bitmap := GetDisabledPattern;
    Canvas.Rectangle(Result);
    exit;
  end;

  Canvas.Brush.Bitmap := nil;
  DrawIColorRect(Canvas, Result, fSelectedColor, true, true, 4);
end;

procedure TColorButtonEx.ShowColorDialog;
begin
  if not Enabled then
    exit;
  if not IColorDialog(fSelectedColor) then
    exit;
  if Assigned(OnColorChanged) and (not (csLoading in ComponentState)) then
    OnColorChanged(Self);
  Invalidate;
end;


{ TIScrollBox }

procedure TIScrollBox.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
    {$R-}
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or WS_VSCROLL or WS_CLIPCHILDREN;
    {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TIScrollBox.CreateWnd;
begin
  inherited CreateWnd;
  Color := ColorToRGB(clForm);
  UpdateScrollBars;
end;

procedure TIScrollBox.Resize;
begin
  inherited Resize;
  UpdateScrollBars;
  Invalidate;
end;

procedure TIScrollBox.ScrollBy(Dx, Dy: Integer);
begin
  if not HandleAllocated then
    Exit;
  fOriginX := fOriginX + Dx;
  fOriginY := fOriginY + Dy;
  UpdateScrollBars;
  Repaint;
end;

procedure TIScrollBox.UpdateScrollBars;
var
  h, w: Integer;
  si: TScrollInfo;
begin
  if not HandleAllocated then
    Exit;
  h := AreaHeight;
  w := AreaWidth;
  fOriginX := Max(0, Min(w - ClientWidth, fOriginX));
  fOriginY := Max(0, Min(h - ClientHeight, fOriginY));
  si.cbSize := SizeOf(si);
  si.fMask := SIF_ALL;
  si.nMin := 0;
  si.nMax := IfThen(h > ClientHeight, h, 0);
  si.nPage := IfThen(si.nMax <> 0, ClientHeight, 0);
  if (si.nMax <> 0) and (fOriginY > 0) then begin
    si.nPos := fOriginY;
    si.nTrackPos := fOriginY;
  end
  else begin
    fOriginY := 0;
    si.nPos := 0;
    si.nTrackPos := 0;
  end;
  SetScrollInfo(Handle, SB_VERT, si, True);

  si.nMax := IfThen(w > ClientWidth, w, 0);
  si.nPage := IfThen(si.nMax <> 0, ClientWidth, 0);
  if (si.nMax <> 0) and (fOriginX > 0) then begin
    si.nPos := fOriginX;
    si.nTrackPos := fOriginX;
  end
  else begin
    fOriginX := 0;
    si.nPos := 0;
    si.nTrackPos := 0;
  end;
  SetScrollInfo(Handle, SB_HORZ, si, True);
end;

procedure TIScrollBox.WMHScroll(var Msg: TLMHScroll);
var
  si: TScrollInfo;
begin
  inherited;
  case Msg.ScrollCode of
    SB_LINELEFT: ScrollBy(-ScrollHLine, 0);
    SB_LINERIGHT: ScrollBy(ScrollHLine, 0);
    SB_PAGELEFT: ScrollBy(-ClientWidth, 0);
    SB_PAGERIGHT: ScrollBy(ClientWidth, 0);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        si.cbSize := SizeOf(TScrollInfo);
        si.fMask  := SIF_TRACKPOS;
        if GetScrollInfo(Handle, SB_HORZ, si) then
          if si.nTrackPos <> fOriginX then
            ScrollBy(si.nTrackPos - fOriginX, 0);
      end;
  end;
end;

procedure TIScrollBox.WMMouseWheel(var Msg: TLMMouseEvent);
var
  Dx, Dy: Integer;
begin
  Dx := 0;
  Dy := Round(-Msg.WheelDelta / 100 * ScrollVLine);
  ScrollBy(Dx, Dy);
end;

procedure TIScrollBox.WMVScroll(var Msg: TLMVScroll);
var
  si: TScrollInfo;
begin
  inherited;
  case Msg.ScrollCode of
    SB_LINEUP: ScrollBy(0, -ScrollVLine);
    SB_LINEDOWN: ScrollBy(0, ScrollVLine);
    SB_PAGEUP: ScrollBy(0, -ClientHeight);
    SB_PAGEDOWN: ScrollBy(0, ClientHeight);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        si.cbSize := SizeOf(TScrollInfo);
        si.fMask  := SIF_TRACKPOS;
        if GetScrollInfo(Handle, SB_VERT, si) then
          if si.nTrackPos <> fOriginY then
            ScrollBy(0, si.nTrackPos - fOriginY);
      end;
  end;
end;

procedure TIScrollBox.WndProc(var Msg: TLMessage);
begin
  if Msg.Msg = LM_LBUTTONDOWN then
    SetFocus;
  inherited WndProc(Msg);
end;


end.
