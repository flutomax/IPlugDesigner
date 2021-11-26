{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmFauIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ActnList, StdCtrls, uStorage, uCommon, Types;

type

  { TFrmFauIcons }

  TFrmFauIcons = class(TForm)
    CmdPaste: TAction;
    ActionList1: TActionList;
    BpMain: TButtonPanel;
    LbFontAudio: TListBox;
    procedure CmdPasteExecute(Sender: TObject);
    procedure CmdPasteUpdate(Sender: TObject);
    procedure LbFontAudioDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
  private
  public

  end;

var
  FrmFauIcons: TFrmFauIcons;

function ShowFauIcons(out s: string): boolean;

implementation

uses uFauIcons;

function ShowFauIcons(out s: string): boolean;
begin
  Application.CreateForm(TFrmFauIcons, FrmFauIcons);
  with FrmFauIcons do
    try
      Result := Showmodal = mrOk;
      if Result then
      begin
        s := LbFontAudio.Items[LbFontAudio.ItemIndex];
      end;
    finally
      Release;
    end;
end;

{$R *.lfm}

{ TFrmFauIcons }

procedure TFrmFauIcons.FormCreate(Sender: TObject);
var
  i: integer;
begin
  LbFontAudio.Items.BeginUpdate;
  try
    for i := Low(ICONS_FAU) to High(ICONS_FAU) do
      LbFontAudio.Items.Add(ICONS_FAU[i]);
  finally
    LbFontAudio.Items.EndUpdate;
  end;
end;

procedure TFrmFauIcons.LbFontAudioDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ts: TTextStyle;
begin
  FillChar(ts, SizeOf(ts), 0);
  with ts do
  begin
    Clipping := True;
    SingleLine := True;
    Layout := tlCenter;
  end;
  LbFontAudio.Canvas.FillRect(ARect);
  ARect.Left += 2;
  LbFontAudio.Canvas.Font.Name := 'fontaudio';
  LbFontAudio.Canvas.TextRect(ARect, ARect.Left, ARect.Top, WideChar($f101 + Index), ts);
  ARect.Left += 20;
  LbFontAudio.Canvas.Font.Name := 'Default';
  LbFontAudio.Canvas.TextRect(ARect, ARect.Left, ARect.Top, LbFontAudio.Items[Index], ts);
end;

procedure TFrmFauIcons.CmdPasteUpdate(Sender: TObject);
begin
  CmdPaste.Enabled := LbFontAudio.ItemIndex >= 0;
end;

procedure TFrmFauIcons.CmdPasteExecute(Sender: TObject);
begin
  ModalResult := MrOK;
end;

end.
