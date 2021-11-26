{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIVKeyboardControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImgList, Graphics, FPImage, uCommon, uIObject, uGraphics,
  uStorage;

const
  DEFAULT_BK_COLOR = $FF464646;
  DEFAULT_WK_COLOR = $FFF0F0F0;
  DEFAULT_PK_COLOR = $3C000000;
  DEFAULT_FR_COLOR = iclBlack;
  DEFAULT_HK_COLOR = iclOrange;

type

  { TIVKeyboardControl }

  TIVKeyboardControl = class(TISpan)
  private
    fRoundedKeys: boolean;
    fRoundness: double;
    fDrawShadows: boolean;
    fDrawFrame: boolean;
    fFrameThickness: double;
    fShowNoteAndVel: boolean;
    fWKWidth: double;
    fBKWidthRatio: double;
    fBKHeightRatio: double;
    fBKAlpha: double;
    fMinNote: integer;
    fMaxNote: integer;
    fWK_COLOR: TIColor;
    fBK_COLOR: TIColor;
    fPK_COLOR: TIColor;
    fFR_COLOR: TIColor;
    fHK_COLOR: TIColor;
    fPressedKeys: array of boolean;
    fIsBlackKeyList: array of boolean;
    fKeyXPos: array of double;
    function NKeys: integer;
    function GetBKWidth: double;
    procedure RecreateKeyBounds(const KeepWidth: boolean);
    procedure SetBK_COLOR(AValue: TIColor);
    procedure SetFR_COLOR(AValue: TIColor);
    procedure SetHK_COLOR(AValue: TIColor);
    procedure SetMaxNote(AValue: integer);
    procedure SetMinNote(AValue: integer);
    procedure SetNoteRange(const AMin, AMax: integer; const KeepWidth: boolean = True);
    procedure SetPK_COLOR(AValue: TIColor);
    procedure SetRoundedKeys(AValue: boolean);
    procedure SetWK_COLOR(AValue: TIColor);
  protected
    procedure Paint(Canvas: TICanvas); override;
    procedure Resize; override;
    procedure DrawKey(Canvas: TICanvas; const Bounds: TIFloatRect; Color: TIColor);
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property RoundedKeys: boolean read fRoundedKeys write SetRoundedKeys;
    property MinNote: integer read fMinNote write SetMinNote;
    property MaxNote: integer read fMaxNote write SetMaxNote;
    property WK_COLOR: TIColor read fWK_COLOR write SetWK_COLOR;
    property BK_COLOR: TIColor read fBK_COLOR write SetBK_COLOR;
    property PK_COLOR: TIColor read fPK_COLOR write SetPK_COLOR;
    property FR_COLOR: TIColor read fFR_COLOR write SetFR_COLOR;
    property HK_COLOR: TIColor read fHK_COLOR write SetHK_COLOR;
  end;

implementation

function ColorStr(const c: TIColor): string;
begin
  if c = DEFAULT_BK_COLOR then
    Exit('DEFAULT_BK_COLOR')
  else if c = DEFAULT_WK_COLOR then
    Exit('DEFAULT_WK_COLOR')
  else if c = DEFAULT_PK_COLOR then
    Exit('DEFAULT_PK_COLOR')
  else if c = DEFAULT_FR_COLOR then
    Exit('DEFAULT_FR_COLOR')
  else if c = DEFAULT_HK_COLOR then
    Exit('DEFAULT_HK_COLOR')
  else
    Result := IColorStr(c);
end;

{ TIVKeyboardControl }

constructor TIVKeyboardControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fRoundedKeys := False;
  fRoundness := 5;
  fDrawShadows := False;
  fDrawFrame := True;
  fFrameThickness := 1;
  fShowNoteAndVel := False;
  fWKWidth := 0;
  fBKWidthRatio := 0.6;
  fBKHeightRatio := 0.6;
  fBKAlpha := 100;
  fMinNote := 48;
  fMaxNote := 72;
  fWK_COLOR := DEFAULT_WK_COLOR;
  fBK_COLOR := DEFAULT_BK_COLOR;
  fPK_COLOR := DEFAULT_PK_COLOR;
  fFR_COLOR := DEFAULT_FR_COLOR;
  fHK_COLOR := DEFAULT_HK_COLOR;
  SetNoteRange(fMinNote, fMaxNote, True);
  SetBounds(0, 0, 300, 100);
end;

destructor TIVKeyboardControl.Destroy;
begin
  SetLength(fKeyXPos, 0);
  SetLength(fPressedKeys, 0);
  SetLength(fIsBlackKeyList, 0);
  inherited Destroy;
end;

class function TIVKeyboardControl.GetObjectName: string;
begin
  Result := 'IVKeyboardControl';
end;

function TIVKeyboardControl.NKeys: integer;
begin
  Result := fMaxNote - fMinNote + 1;
end;

function TIVKeyboardControl.GetBKWidth: double;
begin
  Result := fWKWidth;
  if NKeys > 1 then
    Result *= fBKWidthRatio;
end;

procedure TIVKeyboardControl.SetNoteRange(const AMin, AMax: integer;
  const KeepWidth: boolean);
begin
  if (AMin < 0) or (AMax < 0) then
    exit;
  if AMin < AMax then
  begin
    fMinNote := AMin;
    fMaxNote := AMax;
  end
  else
  begin
    fMinNote := AMax;
    fMaxNote := AMin;
  end;
  SetLength(fPressedKeys, NKeys);
  FillChar(fPressedKeys[0], NKeys * sizeof(boolean), 0);
  RecreateKeyBounds(KeepWidth);
end;

procedure TIVKeyboardControl.SetBK_COLOR(AValue: TIColor);
begin
  if fBK_COLOR = AValue then
    Exit;
  DoChanging(Self);
  fBK_COLOR := AValue;
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetFR_COLOR(AValue: TIColor);
begin
  if fFR_COLOR = AValue then
    Exit;
  DoChanging(Self);
  fFR_COLOR := AValue;
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetHK_COLOR(AValue: TIColor);
begin
  if fHK_COLOR = AValue then
    Exit;
  DoChanging(Self);
  fHK_COLOR := AValue;
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetPK_COLOR(AValue: TIColor);
begin
  if fPK_COLOR = AValue then
    Exit;
  DoChanging(Self);
  fPK_COLOR := AValue;
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetWK_COLOR(AValue: TIColor);
begin
  if fWK_COLOR = AValue then
    Exit;
  DoChanging(Self);
  fWK_COLOR := AValue;
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetRoundedKeys(AValue: boolean);
begin
  if fRoundedKeys = AValue then
    Exit;
  DoChanging(Self);
  fRoundedKeys := AValue;
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetMaxNote(AValue: integer);
begin
  if fMaxNote = AValue then
    Exit;
  DoChanging(Self);
  fMaxNote := AValue;
  SetNoteRange(fMinNote, fMaxNote, True);
  DoChanged(Self);
end;

procedure TIVKeyboardControl.SetMinNote(AValue: integer);
begin
  if fMinNote = AValue then
    Exit;
  DoChanging(Self);
  fMinNote := AValue;
  SetNoteRange(fMinNote, fMaxNote, True);
  DoChanged(Self);
end;

procedure TIVKeyboardControl.RecreateKeyBounds(const KeepWidth: boolean);

  function GetShiftForPitchClass(pitch: integer): double;
  begin
    // usually black key width + distance to the closest black key = white key width,
    // and often b width is ~0.6 * w width
    Result := 0;
    if pitch = 0 then
      Exit
    else if pitch mod 12 = 1 then
      Exit(7 / 12)
    else if pitch mod 12 = 3 then
      Exit(5 / 12)
    else if pitch mod 12 = 6 then
      Exit(2 / 3)
    else if pitch mod 12 = 8 then
      Exit(1 / 2)
    else if pitch mod 12 = 10 then
      Exit(1 / 3);
  end;

var
  NumWhites, WKPadStart, WKPadEnd, BKWidth, PrevWKLeft, l: double;
  i, k, n: integer;
  r: TIFloatRect;
begin
  if KeepWidth then
    fWKWidth := 0;
  r := GetRect;
  // create size-independent data.
  SetLength(fIsBlackKeyList, NKeys);
  SetLength(fKeyXPos, NKeys);
  NumWhites := 0;
  i := 0;
  for n := fMinNote to fMaxNote do
  begin
    fIsBlackKeyList[i] := (n mod 12 = 1) or (n mod 12 = 3) or
      (n mod 12 = 6) or (n mod 12 = 8) or (n mod 12 = 10);
    if not fIsBlackKeyList[i] then
      NumWhites += 1;
    Inc(i);
  end;

  // black key middle isn't aligned exactly between whites
  WKPadStart := 0; // 1st note may be black
  WKPadEnd := 0;   // last note may be black
  WKPadStart := GetShiftForPitchClass(fMinNote);

  if (fMinNote <> fMaxNote) and fIsBlackKeyList[Length(fIsBlackKeyList) - 1] then
    WKPadEnd := 1 - GetShiftForPitchClass(fMaxNote);

  // build rects
  if fWKWidth = 0.0 then
    fWKWidth := 0.2 * r.Height; // first call from the constructor

  if KeepWidth then
  begin
    fWKWidth := r.Width;
    if NumWhites <> 0 then
      fWKWidth /= (NumWhites + fBKWidthRatio * (WKPadStart + WKPadEnd));
  end;

  BKWidth := fWKWidth;

  if NumWhites <> 0 then
    BKWidth *= fBKWidthRatio;

  PrevWKLeft := r.Left;

  for k := 0 to Length(fIsBlackKeyList) - 1 do
  begin
    if fIsBlackKeyList[k] then
    begin
      l := PrevWKLeft;
      if k <> 0 then
        l -= GetShiftForPitchClass(fMinNote + k) * BKWidth
      else
        PrevWKLeft += WKPadStart * BKWidth;
      fKeyXPos[k] := l;
    end
    else
    begin
      fKeyXPos[k] := PrevWKLeft;
      PrevWKLeft += fWKWidth;
    end;
  end;
end;

procedure TIVKeyboardControl.Paint(Canvas: TICanvas);
var
  ShadowColor, cBP: TIColor;
  BKBottom, BKWidth, kL, w: double;
  i: integer;
  r, KeyBounds, ShadowBounds: TIFloatRect;
begin
  r := GetRect;
  // create size-
  ShadowColor := DEFAULT_PK_COLOR;

  BKBottom := r.Top + r.Height * fBKHeightRatio;
  BKWidth := GetBKWidth;

  // first draw white keys
  for i := 0 to NKeys - 1 do
  begin
    if not fIsBlackKeyList[i] then
    begin
      kL := fKeyXPos[i];
      KeyBounds := TIFloatRect.Create(kL, r.Top, kL + fWKWidth, r.Bottom);

      DrawKey(Canvas, KeyBounds, fWK_COLOR);

      if fPressedKeys[i] then
      begin
        // draw played white key
        DrawKey(Canvas, keyBounds, fPK_COLOR);

        if fDrawShadows then
        begin
          ShadowBounds := KeyBounds;
          ShadowBounds.Right := ShadowBounds.Left + 0.35 * ShadowBounds.Width;

          Canvas.PenWidth := 0;
          Canvas.BrushColor := ShadowColor;
          if fRoundedKeys then
            // this one looks strange with rounded corners
            Canvas.DrawRoundedRect(ShadowBounds, 0, 0, fRoundness, fRoundness)
          else
            Canvas.DrawRect(ShadowBounds);
        end;
      end;
      if fDrawFrame and (i <> 0) then
      begin
        // only draw the left border if it doesn't overlay mRECT left border
        Canvas.PenWidth := fFrameThickness;
        Canvas.PenColor := fFR_COLOR;
        Canvas.DrawLine(kL, r.Top, kL, r.Bottom);
        if (i = NKeys - 2) and fIsBlackKeyList[NKeys - 1] then
          Canvas.DrawLine(kL + fWKWidth, r.Top, kL + fWKWidth, r.Bottom);
      end;
    end;
  end;

  // then blacks
  for i := 0 to NKeys - 1 do
  begin
    if fIsBlackKeyList[i] then
    begin
      kL := fKeyXPos[i];
      KeyBounds := TIFloatRect.Create(kL, r.Top, kL + BKWidth, BKBottom);
      // first draw underlying shadows
      if fDrawShadows and (not fPressedKeys[i]) and (i < NKeys - 1) then
      begin
        ShadowBounds := KeyBounds;
        w := ShadowBounds.Width;
        ShadowBounds.Left += 0.6 * w;
        if fPressedKeys[i + 1] then
        begin
          // if white to the right is pressed, shadow is longer
          w *= 1.3;
          ShadowBounds.Bottom := ShadowBounds.Top + 1.05 * ShadowBounds.Height;
        end;
        ShadowBounds.Right := ShadowBounds.Left + w;
        DrawKey(Canvas, ShadowBounds, ShadowColor);
      end;
      DrawKey(Canvas, keyBounds, fBK_COLOR);

      if fPressedKeys[i] then
      begin
        // draw pressed black key
        cBP := fPK_COLOR;
        cBP := (Round(fBKAlpha) shl 24) or (cBP and $FFFFFF);
        Canvas.BrushColor := cBP;
        Canvas.DrawRect(KeyBounds);
      end;

      if not fRoundedKeys then
      begin
        // draw l, r and bottom if they don't overlay the mRECT borders
        Canvas.PenColor := fFR_COLOR;
        if fBKHeightRatio <> 1.0 then
          Canvas.DrawLine(kL, BKBottom, kL + BKWidth, BKBottom);
        if i > 0 then
          Canvas.DrawLine(kL, r.Top, kL, BKBottom);
        if i <> NKeys - 1 then
          Canvas.DrawLine(kL + BKWidth, r.Top, kL + BKWidth, BKBottom);
      end;
    end;
  end;

  if fDrawFrame then
  begin
    Canvas.BrushColor := 0;
    Canvas.PenColor := fFR_COLOR;
    Canvas.PenWidth := fFrameThickness;
    Canvas.DrawRect(r);
  end;
end;

procedure TIVKeyboardControl.Resize;
begin
  inherited Resize;
  RecreateKeyBounds(True);
end;

procedure TIVKeyboardControl.DrawKey(Canvas: TICanvas; const Bounds: TIFloatRect;
  Color: TIColor);
begin
  Canvas.PenWidth := 0;
  Canvas.BrushColor := Color;
  if fRoundedKeys then
    Canvas.DrawRoundedRect(Bounds, 0, 0, fRoundness, fRoundness)
  else
    Canvas.DrawRect(Bounds);
end;

function TIVKeyboardControl.GetCodeStr: string;
begin
  if (fWK_COLOR = DEFAULT_WK_COLOR) and (fBK_COLOR = DEFAULT_BK_COLOR) and
    (fPK_COLOR = DEFAULT_PK_COLOR) and (fFR_COLOR = DEFAULT_FR_COLOR) and
    (fHK_COLOR = DEFAULT_HK_COLOR) then
    Result := Format('%s(%s, %d, %d, %s)', [GetObjectName, IRect(GetRect),
      MinNote, MaxNote, BooleanToStr(fRoundedKeys)])
  else
    Result := Format('%s(%s, %d, %d, %s, %s, %s, %s, %s, %s)',
      [GetObjectName, IRect(GetRect), MinNote, MaxNote, BooleanToStr(fRoundedKeys),
      ColorStr(fWK_COLOR), ColorStr(fBK_COLOR), ColorStr(fPK_COLOR),
      ColorStr(fFR_COLOR), ColorStr(fHK_COLOR)]);
end;

end.
