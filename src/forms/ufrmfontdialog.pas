{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmFontDialog;

{$mode objfpc}{$H+}

interface

uses
  LclIntf, LclType, Types, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ButtonPanel, uGraphics, uIMiscControls;

type

  { TFrmFontDialog }

  TFrmFontDialog = class(TForm)
    BpMain: TButtonPanel;
    CbSize: TComboBox;
    EdFamily: TEdit;
    EdAngle: TFloatSpinEdit;
    GpStyle: TCheckGroup;
    GpFamily: TGroupBox;
    GpPreview: TGroupBox;
    ImgPreview: TImage;
    LblSize: TLabel;
    LblColor: TLabel;
    LblAngle: TLabel;
    LbxFonts: TListBox;
    RgAlign: TRadioGroup;
    RgVAlign: TRadioGroup;
    procedure CbSizeChange(Sender: TObject);
    procedure EdAngleChange(Sender: TObject);
    procedure EdFamilyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GpStyleItemClick(Sender: TObject; Index: integer);
    procedure ImgPreviewResize(Sender: TObject);
    procedure LbxFontsClick(Sender: TObject);
    procedure LbxFontsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LbxFontsSelectionChange(Sender: TObject; User: boolean);
    procedure RgAlignClick(Sender: TObject);
    procedure RgVAlignClick(Sender: TObject);
  private
    fLoading: boolean;
    fBtnColor: TColorButtonEx;
    fFont: TIFont;
    procedure MakeFontList;
    procedure SetFont(AValue: TIFont);
    procedure BtnColorChanged(Sender: TObject);
    procedure PreviewFont;
  public
    property IFont: TIFont read fFont write SetFont;
  end;

var
  FrmFontDialog: TFrmFontDialog;

function ShowFontDialog(aFont: TIFont): boolean;
function EnumFontsProc(var LogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx;
  FontType: longint; Data: LParam): longint; stdcall;

implementation

uses
  agg_fpimage, uCommon, FPWritePNG;

{$R *.lfm}


function ShowFontDialog(aFont: TIFont): boolean;
begin
  Application.CreateForm(TFrmFontDialog, FrmFontDialog);
  with FrmFontDialog do
    try
      IFont := aFont;
      Result := Showmodal = mrOk;
      if Result then
      begin
        aFont.Assign(IFont);
      end;
    finally
      Release;
    end;
end;

function EnumFontsProc(var LogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx;
  FontType: longint; Data: LParam): longint; stdcall;
var
  lst: TStringList;
  s: string;
begin
  lst := TStringList(PtrInt(Data));
  s := LogFont.elfLogFont.lfFaceName;
  if (lst.IndexOf(s) < 0) and (FontType = TRUETYPE_FONTTYPE) then
  begin
    lst.Add(s);
  end;
  Result := 1;
end;


{ TFrmFontDialog }

procedure TFrmFontDialog.FormCreate(Sender: TObject);
begin
  fFont := TIFont.Create;
  LbxFonts.ItemHeight := MulDiv(19, Screen.PixelsPerInch, 96);
  LbxFonts.Canvas.Font.Size := LbxFonts.ItemHeight - 1;
  fBtnColor := TColorButtonEx.Create(self);
  fBtnColor.Parent := self;
  fBtnColor.SetBounds(390, 16, 63, 25);
  fBtnColor.OnColorChanged := @BtnColorChanged;
  MakeFontList;
end;

procedure TFrmFontDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fFont);
end;

procedure TFrmFontDialog.FormShow(Sender: TObject);
begin
  PreviewFont;
end;

procedure TFrmFontDialog.MakeFontList;
var
  lst: TStringList;
  lf: TLogFont;
  DC: HDC;
begin
  DC := GetDC(0);
  FillChar(lf, sizeof(lf), 0);
  lst := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    EnumFontFamiliesEX(DC, @lf, @EnumFontsProc, PtrInt(lst), 0);
    lst.Sort;
    LbxFonts.Items.Assign(lst);
  finally
    lst.Free;
    ReleaseDC(0, DC);
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmFontDialog.LbxFontsDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  LbxFonts.Canvas.FillRect(ARect);
  LbxFonts.Canvas.Font.Name := LbxFonts.Items[Index];
  LbxFonts.Canvas.TextRect(ARect, ARect.Left, ARect.Top, LbxFonts.Items[Index]);
end;

procedure TFrmFontDialog.LbxFontsSelectionChange(Sender: TObject; User: boolean);
begin
  if User then
    LbxFontsClick(Sender);
end;

procedure TFrmFontDialog.GpStyleItemClick(Sender: TObject; Index: integer);
var
  fs: TIFontStyle;
begin
  if fLoading then
    exit;
  fs := [];
  if GpStyle.Checked[0] then
    Include(fs, Bold);
  if GpStyle.Checked[1] then
    Include(fs, Italic);
  fFont.Style := fs;
  PreviewFont;
end;

procedure TFrmFontDialog.ImgPreviewResize(Sender: TObject);
begin
  with TImage(Sender) do
    Picture.Bitmap.SetSize(Width, Height);
end;

procedure TFrmFontDialog.LbxFontsClick(Sender: TObject);
begin
  if fLoading then
    exit;
  fFont.Family := LbxFonts.Items[LbxFonts.ItemIndex];
  EdFamily.Text := fFont.Family;
  PreviewFont;
end;

procedure TFrmFontDialog.EdFamilyChange(Sender: TObject);
begin
  if fLoading then
    exit;
  fFont.Family := EdFamily.Text;
  LbxFonts.ItemIndex := LbxFonts.Items.IndexOf(fFont.Family);
  PreviewFont;
end;

procedure TFrmFontDialog.RgAlignClick(Sender: TObject);
begin
  if fLoading then
    exit;
  fFont.Align := TITextAlign(RgAlign.ItemIndex);
end;

procedure TFrmFontDialog.RgVAlignClick(Sender: TObject);
begin
  if fLoading then
    exit;
  fFont.VAlign := TITextVAlign(RgVAlign.ItemIndex);
end;

procedure TFrmFontDialog.EdAngleChange(Sender: TObject);
begin
  if fLoading then
    exit;
  fFont.Angle := EdAngle.Value;
end;

procedure TFrmFontDialog.CbSizeChange(Sender: TObject);
var
  v: double;
begin
  if fLoading then
    exit;
  v := StrToFloatDef(CbSize.Text, fFont.Size);
  fFont.Size := v;
  PreviewFont;
end;

procedure TFrmFontDialog.SetFont(AValue: TIFont);
begin
  if fFont = AValue then
    Exit;
  fLoading := True;
  try
    fFont.Assign(AValue);
    EdFamily.Text := fFont.Family;
    LbxFonts.ItemIndex := LbxFonts.Items.IndexOf(fFont.Family);
    CbSize.Text := FloatToStr(fFont.Size);
    fBtnColor.SelectedColor := fFont.Color;
    GpStyle.Checked[0] := Bold in fFont.Style;
    GpStyle.Checked[1] := Italic in fFont.Style;
    RgAlign.ItemIndex := Ord(fFont.Align);
    RgVAlign.ItemIndex := Ord(fFont.VAlign);
    EdAngle.Value := fFont.Angle;
  finally
    fLoading := False;
  end;
end;

procedure TFrmFontDialog.BtnColorChanged(Sender: TObject);
begin
  fFont.Color := fBtnColor.SelectedColor;
  PreviewFont;
end;

procedure TFrmFontDialog.PreviewFont;
var
  c: TICanvas;
  r: TIFloatRect;
begin
  r := TIFloatRect.Create(ImgPreview.ClientRect);
  c := TICanvas.Create(ImgPreview.Width, ImgPreview.Height);
  try
    c.ClipRect := r;
    c.TransformSpace(r, r);
    c.GraphicsSettings.Font := fFont;
    c.BrushColor := ColorToIColor(GetDefaultColor(dctBrush));
    c.PenWidth := 0;
    c.FontAlign := Center;
    c.FontVAlign := Middle;
    c.FontAngle := 0;
    c.DrawRect(r);
    c.DrawText('AaBbYyZz', r);
    c.DrawTo(ImgPreview.Picture.Bitmap);
  finally
    c.Free;
  end;
end;


end.


