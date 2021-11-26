{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmInputText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;

type

  TInputTextCloseQueryEvent = procedure(Sender: TObject; const AValue: string;
    var ACanClose: boolean) of object;

  { TFrmInputText }

  TFrmInputText = class(TForm)
    BpMain: TButtonPanel;
    EdInput: TEdit;
    LblPrompt: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    fCloseQuery: TInputTextCloseQueryEvent;
  public

  end;

function InputText(const APrompt: string; OnDialogCloseQuery: TInputTextCloseQueryEvent;
  var Value: string): boolean;

var
  FrmInputText: TFrmInputText;

implementation

{$R *.lfm}

uses
  uCommon;

function InputText(const APrompt: string; OnDialogCloseQuery: TInputTextCloseQueryEvent;
  var Value: string): boolean;
begin
  Application.CreateForm(TFrmInputText, FrmInputText);
  with FrmInputText do
    try
      LblPrompt.Caption := APrompt;
      EdInput.Text := Value;
      fCloseQuery := OnDialogCloseQuery;
      Result := Showmodal = mrOk;
      if Result then
        Value := Trim(EdInput.Text);
    finally
      Release;
    end;
end;

{ TFrmInputText }

procedure TFrmInputText.FormCreate(Sender: TObject);
begin
  Caption := AppTitle;
end;

procedure TFrmInputText.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    fCloseQuery(self, EdInput.Text, CanClose);
  if not CanClose then
    EdInput.SetFocus;
end;

end.
