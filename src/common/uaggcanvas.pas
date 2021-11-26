{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uAGGCanvas;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Agg_LCL, agg_fpimage, agg_font_cache_manager,
  agg_trans_affine, uCommon;

type

  { TAggCanvas }

  TAggCanvas = class(TAggLCLCanvas)
  public
    procedure RenderText(const x, y: double; str: ansistring);
    procedure SetupFont(aFont: TObject);
    function AggFontHeight: double;
  end;

implementation

uses
  Math, agg_conv_transform, agg_path_storage, agg_svg_parser_lcl, uGraphics;

{ TAggCanvas }

procedure TAggCanvas.RenderText(const x, y: double; str: ansistring);
var
  dx, dy, start_x, start_y: double;
  mtx: trans_affine;
  str_: PChar;
  glyph: glyph_cache_ptr;
  tat: trans_affine_translation;
  tar: trans_affine_rotation;
  tr: conv_transform;
  charlen, char_id: longint;
  First: boolean;
begin
  if str = '' then
    exit;

  dx := 0.0;
  dy := -AggFontHeight;
  if m_fontEngine._flip_y then
    dy := -dy;

  mtx.Construct;
  start_x := x + dx;
  start_y := y + dy;
  tat.Construct(-x, -y);
  mtx.multiply(@tat);

  tar.Construct(Font.AggAngle);
  mtx.multiply(@tar);

  tat.Construct(x, y);
  mtx.multiply(@tat);

  tr.Construct(m_fontCacheManager.path_adaptor, @mtx);
  str_ := @str[1];
  First := True;

  while str_^ <> #0 do
  begin
    if UseUTF8 then
    begin
      char_id := AggUTF8CharToUnicode(str_, charlen);
      Inc(str_, charlen);
    end
    else
    begin
      char_id := longint(PChar(str_)^);
      Inc(str_, sizeof(char));
    end;
    glyph := m_fontCacheManager.glyph(char_id);

    if glyph <> nil then
    begin
      if First then
      begin
        m_fontCacheManager.add_kerning(@x, @y);
        First := False;
      end;
      m_fontCacheManager.init_embedded_adaptors(glyph, start_x, start_y);

      if glyph^.data_type = glyph_data_outline then
      begin
        Path.m_path.remove_all;
        Path.m_path.add_path(@tr, 0, False);

        if Font.AggUseOnlyFont then
          AggDrawPath(AGG_FillOnly, True)
        else
          AggDrawPath(AGG_FillAndStroke, True);
      end;

      if glyph^.data_type = glyph_data_gray8 then
      begin
        Render(
          m_fontCacheManager.gray8_adaptor,
          m_fontCacheManager.gray8_scanline);
      end;

      start_x := start_x + glyph^.advance_x;
      start_y := start_y + glyph^.advance_y;
    end;
  end;
end;

procedure TAggCanvas.SetupFont(aFont: TObject);
begin
  if not (aFont is TIFont) then
    exit;
  with aFont as TIFont do
  begin
    Font.AggColor := IColorToAggColor(Color);
    Font.AggAlignX := AGG_AlignLeft;
    Font.AggAlignY := AGG_AlignTop;
    Font.LoadFromFile(Family, Font.AggHeightToSize(Size), Bold in Style,
      Italic in Style, AGG_VECTORFONTCACHE, DegToRad(Angle));
  end;
end;

function TAggCanvas.AggFontHeight: double;
var
  glyph: glyph_cache_ptr;
begin
  Result := 0;
  glyph := m_fontCacheManager.glyph(longint('H'));
  if glyph <> nil then
    Result := glyph^.bounds.y2 - glyph^.bounds.y1;
  if Result = 0 then // fontaudio?
  begin
    glyph := m_fontCacheManager.glyph(61739);
    if glyph <> nil then
      Result := glyph^.bounds.y2 - glyph^.bounds.y1;
  end;
end;


end.

