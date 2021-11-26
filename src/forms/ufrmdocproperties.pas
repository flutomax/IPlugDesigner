{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmDocProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Spin,
  StdCtrls, ActnList, SpinEx, Buttons, uCommon, uGraphics, uIMiscControls;

type

  { TFrmDocProperties }

  TFrmDocProperties = class(TForm)
    CmdOK: TAction;
    ActionList1: TActionList;
    BpMain: TButtonPanel;
    EdHeight: TSpinEdit;
    EdWidth: TSpinEdit;
    EdResolution: TSpinEdit;
    GbSizes: TGroupBox;
    GbDescription: TGroupBox;
    LbHeight: TLabel;
    LbBackground: TLabel;
    LbWidth: TLabel;
    LbResolution: TLabel;
    EdDescription: TMemo;
    procedure CmdOKExecute(Sender: TObject);
    procedure CmdOKUpdate(Sender: TObject);
    procedure EdHeightChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fBtnColor: TColorButtonEx;
    procedure BtnColorChanged(Sender: TObject);
  public
    PropertyChanged: Boolean;
    BgColor: TIColor;
  end;

var
  FrmDocProperties: TFrmDocProperties;

implementation

{$R *.lfm}

type

  TCustomBitBtnAccess = class(TCustomBitBtn);

{ TFrmDocProperties }


procedure TFrmDocProperties.FormCreate(Sender: TObject);
begin
  fBtnColor:= TColorButtonEx.Create(self);
  fBtnColor.SetBounds(108, 152, 56, 25);
  fBtnColor.Parent := self;
  fBtnColor.OnColorChanged := @BtnColorChanged;
end;

procedure TFrmDocProperties.FormShow(Sender: TObject);
begin
  BpMain.OKButton.Action := CmdOK;
  fBtnColor.SelectedColor := BgColor;
end;

procedure TFrmDocProperties.BtnColorChanged(Sender: TObject);
begin
  BgColor := fBtnColor.SelectedColor;
  PropertyChanged := true;
end;

procedure TFrmDocProperties.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFrmDocProperties.CmdOKUpdate(Sender: TObject);
begin
  CmdOK.Enabled := PropertyChanged;
end;

procedure TFrmDocProperties.EdHeightChange(Sender: TObject);
begin
  PropertyChanged := true;
end;


end.

object ClbtnBgColor: TColorButton
    Left = 108
    Height = 25
    Top = 152
    Width = 56
    BorderWidth = 2
    ButtonColorSize = 16
    ButtonColor = clWhite
    OnClick = ClbtnBgColorClick
    OnColorChanged = EdHeightChange
  end

