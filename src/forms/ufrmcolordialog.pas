{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmColorDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, Arrow,
  ComCtrls, ButtonPanel, ExtCtrls, Grids, SpinEx, agg_fpimage, uCommon,
  uGraphics, uColorControls, Types;

type

  { TFrmColorDialog }

  TFrmColorDialog = class(TForm)
    BpMain: TButtonPanel;
    BtnAdd: TButton;
    GdAdditional: TDrawGrid;
    EdAlpha: TSpinEdit;
    EdHex: TEdit;
    EdSat: TSpinEdit;
    EdBri: TSpinEdit;
    EdRed: TSpinEdit;
    EdGre: TSpinEdit;
    EdBlu: TSpinEdit;
    GbValue: TGroupBox;
    LbAdditional: TLabel;
    LblAlpha: TLabel;
    LblAlpha1: TLabel;
    LblNew: TLabel;
    LblCurrent: TLabel;
    RbHue: TRadioButton;
    EdHue: TSpinEdit;
    RbSat: TRadioButton;
    RbBri: TRadioButton;
    RbRed: TRadioButton;
    RbGre: TRadioButton;
    RbBlu: TRadioButton;
    procedure BtnAddClick(Sender: TObject);
    procedure EdAlphaChange(Sender: TObject);
    procedure EdHexChange(Sender: TObject);
    procedure EdHexExit(Sender: TObject);
    procedure EdHexKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EdHexKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GdAdditionalDblClick(Sender: TObject);
    procedure GdAdditionalDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GdAdditionalGetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure LblAlphaClick(Sender: TObject);
    procedure RbClick(Sender: TObject);
    procedure HSBEditChange(Sender: TObject);
    procedure RGBEditChange(Sender: TObject);
  private
    fColor: TAggColor;
    fRecentColor: TAggColor;
    fColorChart: TColorChart;
    fColorBar: TColorBar;
    fPreview: TColorPreview;
    fUpdate: boolean;
    fColorComponent: TColorComponent;
    procedure ColorChartHSBChange(Sender: TObject; Hue, Sat, Bri: integer);
    procedure ColorChartRGBChange(Sender: TObject; Red, Green, Blue: integer);
    procedure ColorBarHSBChange(Sender: TObject; Hue, Sat, Bri: integer);
    procedure ColorBarRGBChange(Sender: TObject; Red, Green, Blue: integer);
    function GetColor: TIColor;
    procedure SetColor(AValue: TIColor); overload;
    procedure SetColor(AValue: TAggColor); overload;
    procedure SetColorComponent(AValue: TColorComponent);
    procedure SetGBValueCaption;
    procedure SetHSB(Hue, Sat, Bri: integer);
    procedure SetRGB(Red, Green, Blue: integer);
    procedure AdoptHSB(Hue, Sat, Bri: integer);
    procedure AdoptRGB(Red, Green, Blue: integer);
    procedure AdoptHex(AValue: TAggColor);
    procedure PreviewRevertColor(Sender: TObject);
    procedure UpdateHexEdit;
    procedure LoadStaticColors;
    procedure SaveStaticColors;
    function HexValue: TAggColor;
  public
    property ColorComponent: TColorComponent read fColorComponent
      write SetColorComponent;
    property Color: TIColor read GetColor write SetColor;
    property RecentColor: TAggColor read fRecentColor;
  end;

var
  FrmColorDialog: TFrmColorDialog;

function IColorDialog(var clr: TIColor): boolean;

implementation

{$R *.lfm}

uses LCLType, uIniFileEx;

var
  StaticColors: array[0..7] of TIColor = (0, 0, 0, 0, 0, 0, 0, 0);

function IColorDialog(var clr: TIColor): boolean;
begin
  Application.CreateForm(TFrmColorDialog, FrmColorDialog);
  with FrmColorDialog do
    try
      Color := clr;
      Result := ShowModal = mrOk;
      if Result then
        clr := Color;
    finally
    end;
end;

{ TFrmColorDialog }

procedure TFrmColorDialog.FormCreate(Sender: TObject);
begin
  fUpdate := False;
  fColor.Construct(0, 0, 0);
  fRecentColor := fColor;
  fColorChart := TColorChart.Create(Self);
  fColorChart.SetBounds(8, 8, 258, 258);
  fColorChart.Parent := Self;
  fColorChart.OnHSBChange := @ColorChartHSBChange;
  fColorChart.OnRGBChange := @ColorChartRGBChange;
  fColorBar := TColorBar.Create(Self);
  fColorBar.SetBounds(272, 8, 25, 258);
  fColorBar.Parent := Self;
  fColorBar.OnHSBChange := @ColorBarHSBChange;
  fColorBar.OnRGBChange := @ColorBarRGBChange;
  fPreview := TColorPreview.Create(Self);
  fPreview.SetBounds(388, 8, 104, 66);
  fPreview.Parent := Self;
  fPreview.OnRevertColor := @PreviewRevertColor;
  SetGBValueCaption;
  LoadStaticColors;
end;

procedure TFrmColorDialog.LblAlphaClick(Sender: TObject);
begin

end;

procedure TFrmColorDialog.EdAlphaChange(Sender: TObject);
begin
  if fUpdate then
    exit;
  fColor.a := EdAlpha.Value;
  fPreview.Color := fColor;
  UpdateHexEdit;
end;

procedure TFrmColorDialog.RbClick(Sender: TObject);
begin
  ColorComponent := TColorComponent(TComponent(Sender).Tag);
end;

procedure TFrmColorDialog.ColorChartHSBChange(Sender: TObject; Hue, Sat, Bri: integer);
begin
  fColorBar.SetHSB(Hue, Sat, Bri);
  SetHSB(Hue, Sat, Bri);
end;

procedure TFrmColorDialog.ColorChartRGBChange(Sender: TObject;
  Red, Green, Blue: integer);
begin
  fColorBar.SetRGB(Red, Green, Blue);
  SetRGB(Red, Green, Blue);
end;

procedure TFrmColorDialog.ColorBarHSBChange(Sender: TObject; Hue, Sat, Bri: integer);
begin
  fColorChart.SetHSB(Hue, Sat, Bri);
  SetHSB(Hue, Sat, Bri);
end;

procedure TFrmColorDialog.ColorBarRGBChange(Sender: TObject; Red, Green, Blue: integer);
begin
  fColorChart.SetRGB(Red, Green, Blue);
  SetRGB(Red, Green, Blue);
end;

function TFrmColorDialog.GetColor: TIColor;
begin
  Result := AggColorToIColor(fColor);
end;

procedure TFrmColorDialog.SetColor(AValue: TIColor);
begin
  SetColor(IColorToAggColor(AValue));
end;

procedure TFrmColorDialog.SetColor(AValue: TAggColor);
begin
  fColor := AValue;
  fRecentColor := fColor;
  fColorChart.SetRGB(fColor.r, fColor.g, fColor.b);
  fColorBar.SetRGB(fColor.r, fColor.g, fColor.b);
  fPreview.Color := fColor;
  fPreview.RecentColor := fColor;
  EdAlpha.Value := fColor.a;
  SetRGB(fColor.r, fColor.g, fColor.b);
end;

procedure TFrmColorDialog.HSBEditChange(Sender: TObject);
var
  Hue, Sat, Bri: integer;
begin
  if fUpdate then
    exit;
  Hue := MulDiv(EdHue.Value, Hue360, 360);
  Sat := MulDiv(EdSat.Value, Sat100, 100);
  Bri := MulDiv(EdBri.Value, Sat100, 100);
  fColorChart.SetHSB(Hue, Sat, Bri);
  fColorBar.SetHSB(Hue, Sat, Bri);
  AdoptHSB(Hue, Sat, Bri);
end;

procedure TFrmColorDialog.RGBEditChange(Sender: TObject);
var
  Red, Green, Blue: integer;
begin
  if fUpdate then
    exit;
  Red := EdRed.Value;
  Green := EdGre.Value;
  Blue := EdBlu.Value;
  fColorChart.SetRGB(Red, Green, Blue);
  fColorBar.SetRGB(Red, Green, Blue);
  AdoptRGB(Red, Green, Blue);
end;

procedure TFrmColorDialog.SetColorComponent(AValue: TColorComponent);
begin
  if fColorComponent = AValue then
    Exit;
  fColorComponent := AValue;
  SetGBValueCaption;
  fColorChart.FixComponent := fColorComponent;
  fColorBar.FixComponent := fColorComponent;
end;

procedure TFrmColorDialog.SetGBValueCaption;
const
  cpt: array[TColorComponent] of string = ('hue', 'saturation', 'brightness',
    'red', 'green', 'blue');
begin
  GbValue.Caption := Format('Values (%s)', [cpt[fColorComponent]]);
end;

procedure TFrmColorDialog.SetHSB(Hue, Sat, Bri: integer);
begin
  fUpdate := True;
  try
    EdHue.Value := MulDiv(Hue, 360, Hue360);
    EdSat.Value := MulDiv(Sat, 100, Sat100);
    EdBri.Value := MulDiv(Bri, 100, Sat100);
    AdoptHSB(Hue, Sat, Bri);
  finally
    fUpdate := False;
  end;
end;

procedure TFrmColorDialog.SetRGB(Red, Green, Blue: integer);
begin
  fUpdate := True;
  try
    EdRed.Value := Red;
    EdGre.Value := Green;
    EdBlu.Value := Blue;
    AdoptRGB(Red, Green, Blue);
  finally
    fUpdate := False;
  end;
end;

procedure TFrmColorDialog.AdoptHSB(Hue, Sat, Bri: integer);
var
  Red, Green, Blue: integer;
begin
  fUpdate := True;
  try
    IColorToRGB(HSBToIColor(Hue, Sat, Bri), Red, Green, Blue);
    fColor.r := Red;
    fColor.g := Green;
    fColor.b := Blue;
    fPreview.Color := fColor;
    EdRed.Value := Red;
    EdGre.Value := Green;
    EdBlu.Value := Blue;
  finally
    fUpdate := False;
  end;
  UpdateHexEdit;
end;

procedure TFrmColorDialog.AdoptRGB(Red, Green, Blue: integer);
var
  Hue, Sat, Bri: integer;
begin
  fUpdate := True;
  try
    fColor.r := Red;
    fColor.g := Green;
    fColor.b := Blue;
    fPreview.Color := fColor;
    IColorToHSB(RGBToIColor(Red, Green, Blue), Hue, Sat, Bri);
    EdHue.Value := MulDiv(Hue, 360, Hue360);
    EdSat.Value := MulDiv(Sat, 100, Sat100);
    EdBri.Value := MulDiv(Bri, 100, Sat100);
  finally
    fUpdate := False;
  end;
  UpdateHexEdit;
end;

procedure TFrmColorDialog.AdoptHex(AValue: TAggColor);
var
  Hue, Sat, Bri: integer;
begin
  fUpdate := True;
  try
    fColor := AValue;
    fPreview.Color := fColor;
    IColorToHSB(RGBToIColor(fColor.r, fColor.g, fColor.b), Hue, Sat, Bri);
    EdRed.Value := fColor.r;
    EdGre.Value := fColor.g;
    EdBlu.Value := fColor.b;
    EdAlpha.Value := fColor.a;
    EdHue.Value := MulDiv(Hue, 360, Hue360);
    EdSat.Value := MulDiv(Sat, 100, Sat100);
    EdBri.Value := MulDiv(Bri, 100, Sat100);
  finally
    fUpdate := False;
  end;
end;

procedure TFrmColorDialog.PreviewRevertColor(Sender: TObject);
begin
  SetColor(fRecentColor);
end;

procedure TFrmColorDialog.UpdateHexEdit;
begin
  fUpdate := True;
  try
    with fColor do
      EdHex.Text := Format('$%.2x%.2x%.2x%.2x', [a, r, g, b]);
  finally
    fUpdate := False;
  end;
end;

function TFrmColorDialog.HexValue: TAggColor;
var
  v: TIColor;
begin
  v := AggColorToIColor(fColor);
  v := StrToIColorDef(EdHex.Text, v);
  Result := IColorToAggColor(v);
end;

procedure TFrmColorDialog.EdHexChange(Sender: TObject);
var
  c: TAggColor;
begin
  if fUpdate then
    exit;
  if Length(EdHex.Text) = 0 then
  begin
    UpdateHexEdit;
    exit;
  end;
  if EdHex.Text[1] <> '$' then
  begin
    UpdateHexEdit;
    exit;
  end;
  c := HexValue;
  fColorChart.SetRGB(c.r, c.g, c.b);
  fColorBar.SetRGB(c.r, c.g, c.b);
  AdoptHex(c);
end;

procedure TFrmColorDialog.EdHexExit(Sender: TObject);
var
  c: TAggColor;
begin
  c := HexValue;
  SetRGB(c.r, c.g, c.b);
  fColorChart.SetRGB(c.r, c.g, c.b);
  fColorBar.SetRGB(c.r, c.g, c.b);
  UpdateHexEdit;
end;

procedure TFrmColorDialog.EdHexKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (EdHex.SelStart = 0) then
    Key := 0;
end;

procedure TFrmColorDialog.EdHexKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', 'A'..'F', 'a'..'f', #3, #8, #22]) then
    Key := #0;
  if (EdHex.SelStart <= 1) and (Key = #8) then
    Key := #0;
end;

procedure TFrmColorDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    SaveStaticColors;
end;


procedure TFrmColorDialog.LoadStaticColors;
var
  i: integer;
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(IniPath);
  try
    for i := 0 to 7 do
      StaticColors[i] := ini.ReadHex('ColorDialog', Format('Color%d', [i]), 0);
  finally
    ini.Free;
  end;
end;

procedure TFrmColorDialog.SaveStaticColors;
var
  i: integer;
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(IniPath);
  try
    for i := 0 to 7 do
      ini.WriteHex('ColorDialog', Format('Color%d', [i]), StaticColors[i]);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmColorDialog.BtnAddClick(Sender: TObject);
begin
  StaticColors[GdAdditional.Col] := Color;
  GdAdditional.Invalidate;
end;

procedure TFrmColorDialog.GdAdditionalDblClick(Sender: TObject);
begin
  Color := StaticColors[GdAdditional.Col];
end;

procedure TFrmColorDialog.GdAdditionalDrawCell(Sender: TObject;
  aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  c: TIColor;
begin
  c := StaticColors[aCol];
  DrawIColorRect(GdAdditional.Canvas, aRect, c, True, False);
end;

procedure TFrmColorDialog.GdAdditionalGetCellHint(Sender: TObject;
  ACol, ARow: integer; var HintText: string);
var
  c: TIColor;
begin
  c := StaticColors[aCol];
  HintText := Format('$%.2x%.2x%.2x%.2x', [c.a, c.r, c.g, c.b]);
end;

end.






