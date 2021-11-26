{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmAbout;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, GR32;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    btClose: TButton;
    BtnDonate: TImage;
    Label1: TLabel;
    lblCopy: TLabel;
    lblHomePage: TLabel;
    lblHomePageAddress: TLabel;
    LbLogo: TLabel;
    LbVersion: TLabel;
    LbExeVersion: TLabel;
    procedure BtnDonateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure lblHomePageAddressClick(Sender: TObject);
  private
    fBmp: TBitmap32;
    procedure MakeBackground;
  public

  end;

var
  FrmAbout: TFrmAbout;

procedure ShowAbout;

implementation

{$R *.lfm}

uses
  FileInfo, GR32_Polygons, GR32_VectorUtils, GR32_Blurs, GR32_ColorGradients;

procedure ShowAbout;
begin
  Application.CreateForm(TFrmAbout, FrmAbout);
  with FrmAbout do
    try
      ShowModal;
    finally
      Release;
    end;
end;


function ExeVersion: string;
var
  pv: TProgramVersion;
begin
  if GetProgramVersion(pv) then
    result := ProgramversionToStr(pv);
end;

{ TFrmAbout }

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  fBmp := TBitmap32.Create;
  fBmp.SetSize(ClientWidth, ClientHeight);
  MakeBackground;
  LbExeVersion.Caption := ExeVersion;
end;

procedure TFrmAbout.BtnDonateClick(Sender: TObject);
begin
  OpenUrl('http://stone-voices.ru/donation/?lang=en');
end;

procedure TFrmAbout.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fBmp);
end;

procedure TFrmAbout.FormPaint(Sender: TObject);
begin
  fBmp.DrawTo(Canvas.Handle);
end;

procedure TFrmAbout.lblHomePageAddressClick(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/?lang=en');
end;

procedure TFrmAbout.MakeBackground;
const
  offs = 2;
var
  gf: TLinearGradientPolygonFiller;
  gd: TColor32Gradient;
  lut: TColor32LookupTable;
  r: TArrayOfFloatPoint;
begin
  r := Rectangle(FloatRect(ClientRect));
  gd := TColor32Gradient.Create.Create($FF2989CC, $FFFFFFFF);
  try
    lut := TColor32LookupTable.Create;
    gd.FillColorLookUpTable(lut);
    try
      gf := TLinearGradientPolygonFiller.Create(lut);
      try
        gf.StartPoint := FloatPoint(ClientWidth * 0.5, 0);
        gf.EndPoint := FloatPoint(ClientWidth * 0.5, ClientHeight);
        gf.WrapMode := wmClamp;
        PolygonFS(fBmp, r, gf, pfWinding);
      finally
        gf.Free;
      end;
    finally
      lut.Free;
    end;
  finally
    gd.Free;
  end;
  r := nil;

  fBmp.Font := LbLogo.Font;
  // Make shadow
  fBmp.RenderText(LbLogo.Left + offs, LbLogo.Top + offs, LbLogo.Caption, 4, clBlack32);
  GaussianBlur(fBmp, 7);
  fBmp.RenderText(LbLogo.Left, LbLogo.Top, LbLogo.Caption, 4, $FFFFFFFF);
end;

end.
