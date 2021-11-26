{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIPlugPropEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, TypInfo, SysUtils, LCLProc, Forms, Controls, LCLType, GraphType,
  LazFileUtils, Graphics, Buttons, Menus, ExtCtrls, Dialogs, LCLIntf, PropEdits,
  PropEditUtils, ImgList, EditBtn, uCommon, uIObject, uGraphics, uVectorControls;

type

  { TIColorPropertyEditor }

  TIColorPropertyEditor = class(TIntegerPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure ListMeasureWidth(const {%H-}CurValue: ansistring;
    {%H-}Index: integer; ACanvas: TCanvas; var AWidth: integer); override;
    procedure ListDrawValue(const CurValue: ansistring; Index: integer;
      ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
  end;

  { TIFontPropertyEditor }

  TIFontPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TFontFamilyPropertyEditor }

  TFontFamilyPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TTImageNamePropertyEditor }

  TTImageNamePropertyEditor = class(TStringPropertyEditor)
  public
    procedure Edit; override;
    function GetImageCache: TImageCache;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TISVGNamePropertyEditor }

  TISVGNamePropertyEditor = class(TStringPropertyEditor)
  public
    procedure Edit; override;
    function GetImageCache: TImageCache;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TISVGNamesPropertyEditor }

  TISVGNamesPropertyEditor = class(TStringPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TStyleNamePropertyEditor }

  TStyleNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetStyleCache: TIVStyleCache;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


implementation

uses
  Math, uIDocument, uStorage, uFrmColorDialog, uFrmFontDialog,
  uFrmSelectImage, uFrmSelectSVG, uFrmSelectSVGs;


{ TIColorPropertyEditor }

procedure TIColorPropertyEditor.Edit;
var
  c: TIColor;
begin
  c := TIColor(GetOrdValue);
  if IColorDialog(c) then
    SetOrdValue(longint(c));
end;

function TIColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paCustomDrawn, paRevertable];
end;

function TIColorPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  Result := IColorToString(TIColor(OrdValue));
end;

procedure TIColorPropertyEditor.GetValues(Proc: TGetStrProc);
var
  c: TIColor;
begin
  if not IdentToIColor(GetVisualValue, c) then
    Proc(GetVisualValue);
  GetIColorValues(Proc);
end;

procedure TIColorPropertyEditor.SetValue(const NewValue: ansistring);
var
  c: TIColor;
begin
  if IdentToIColor(NewValue, c) then
    SetOrdValue(longint(c))
  else
    inherited SetValue(NewValue);
end;

procedure TIColorPropertyEditor.ListMeasureWidth(const CurValue: ansistring;
  Index: integer; ACanvas: TCanvas; var AWidth: integer);
begin
  AWidth := ACanvas.TextWidth('COLOR_BLACK_DROP_SHADOW') + 25;
end;

procedure TIColorPropertyEditor.ListDrawValue(const CurValue: ansistring;
  Index: integer; ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
var
  rt: integer;
  r: TRect;
begin
  rt := (ARect.Bottom - ARect.Top) + ARect.Left - 2;
  r := Rect(ARect.Left + 1, ARect.Top + 1, rt - 1, ARect.Bottom - 3);
  DrawIColorRect(ACanvas, r, StringToIColorDef(CurValue, iclBlack), True, True);
  inherited ListDrawValue(CurValue, Index, ACanvas,
    Rect(rt, ARect.Top, ARect.Right, ARect.Bottom), AState);
end;

procedure TIColorPropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;


{ TIFontPropertyEditor }

procedure TIFontPropertyEditor.Edit;
var
  fnt: TIFont;
begin
  fnt := TIFont(GetObjectValue(TIFont));
  ShowFontDialog(fnt);
end;

function TIFontPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TFontFamilyPropertyEditor }

function TFontFamilyPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TFontFamilyPropertyEditor.GetValues(Proc: TGetStrProc);
var
  i: integer;
  lf: TLogFont;
  DC: HDC;
  lst: TStringList;
begin
  DC := GetDC(0);
  FillChar(lf, sizeof(lf), 0);
  lst := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    EnumFontFamiliesEX(DC, @lf, @EnumFontsProc, PtrInt(lst), 0);
    lst.Sort;
    for i := 0 to lst.Count - 1 do
      Proc(lst[i]);
  finally
    lst.Free;
    ReleaseDC(0, DC);
    Screen.Cursor := crDefault;
  end;
end;

{ TTImageNamePropertyEditor }

procedure TTImageNamePropertyEditor.Edit;
var
  s: string;
begin
  s := GetStrValue;
  if ShowSelectImageName(s) then
    SetStrValue(s);
end;

function TTImageNamePropertyEditor.GetImageCache: TImageCache;
var
  Persistent: TPersistent;
  doc: TIDocument;
begin
  Result := nil;
  Persistent := GetComponent(0);
  if not (Persistent is TIObject) then
    exit;
  doc := TIObject(Persistent).Document as TIDocument;
  if doc = nil then
    exit;
  Result := doc.ImageCache;
end;

function TTImageNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

procedure TTImageNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  ic: TImageCache;
  i: integer;
begin
  ic := GetImageCache;
  if Assigned(ic) then
  begin
    for i := 0 to ic.ImagesCount - 1 do
      Proc(ic[i].FileName);
  end;
end;

{ TISVGNamePropertyEditor }

procedure TISVGNamePropertyEditor.Edit;
var
  s: string;
begin
  s := GetStrValue;
  if ShowSelectSVGName(s) then
    SetStrValue(s);
end;

function TISVGNamePropertyEditor.GetImageCache: TImageCache;
var
  Persistent: TPersistent;
  doc: TIDocument;
begin
  Result := nil;
  Persistent := GetComponent(0);
  if not (Persistent is TIObject) then
    exit;
  doc := TIObject(Persistent).Document as TIDocument;
  if doc = nil then
    exit;
  Result := doc.SVGCache;
end;

function TISVGNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

procedure TISVGNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  ic: TImageCache;
  i: integer;
begin
  ic := GetImageCache;
  if Assigned(ic) then
  begin
    for i := 0 to ic.ImagesCount - 1 do
      Proc(ic[i].FileName);
  end;
end;


{ TISVGNamesPropertyEditor }

procedure TISVGNamesPropertyEditor.Edit;
var
  s: string;
begin
  s := GetStrValue;
  if ShowSelectSVGNames(s) then
    SetStrValue(s);
end;

function TISVGNamesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable];
end;


{ TStyleNamePropertyEditor }

function TStyleNamePropertyEditor.GetStyleCache: TIVStyleCache;
var
  Persistent: TPersistent;
  doc: TIDocument;
begin
  Result := nil;
  Persistent := GetComponent(0);
  if not (Persistent is TIObject) then
    exit;
  doc := TIObject(Persistent).Document as TIDocument;
  if doc = nil then
    exit;
  Result := doc.StyleCache;
end;

function TStyleNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TStyleNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  sc: TIVStyleCache;
  i: integer;
begin
  sc := GetStyleCache;
  if Assigned(sc) then
  begin
    for i := 0 to sc.StylesCount - 1 do
      Proc(sc[i].Name);
  end;
end;



initialization
  RegisterPropertyEditor(TypeInfo(TIColor), nil, '', TIColorPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TIFont), nil, '', TIFontPropertyEditor);
  RegisterPropertyEditor(TypeInfo(ansistring), TIFont, 'Family',
    TFontFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyleName), TIVectorBase, 'StyleName',
    TStyleNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageName), TIObject, '',
    TTImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSVGName), TIObject, '',
      TISVGNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSVGNames), TIObject, 'SVGs',
      TISVGNamesPropertyEditor);

end.
