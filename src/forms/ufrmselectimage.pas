{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmSelectImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ActnList, uCommon, uGraphics;

type

  { TFrmSelectImage }

  TFrmSelectImage = class(TForm)
    CmdOK: TAction;
    ActionList1: TActionList;
    BpMain: TButtonPanel;
    ILMiniature: TImageList;
    LV: TListView;
    procedure CmdOKExecute(Sender: TObject);
    procedure CmdOKUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure AddImage(img: TIEmbededGraphics);
    function AddMiniature(AImg: TIEmbededGraphics): integer;
  public
    procedure TrySelectImage(const AName: string);
  end;

var
  FrmSelectImage: TFrmSelectImage;

  function ShowSelectImageName(var AImageName: TImageName): boolean;

implementation

{$R *.lfm}

uses
  uFrmMain;

function ShowSelectImageName(var AImageName: TImageName): boolean;
begin
  Application.CreateForm(TFrmSelectImage, FrmSelectImage);
  with FrmSelectImage do
  try
    TrySelectImage(AImageName);
    result := ShowModal = mrOK;
    if result then
      AImageName := LV.Selected.Caption;
  finally
    Release;
  end;
end;

{ TFrmSelectImage }

procedure TFrmSelectImage.FormCreate(Sender: TObject);
var
  i: integer;
begin
  if Assigned(FrmMain.Document) then
    with FrmMain.Document.ImageCache do
      for i := 0 to ImagesCount - 1 do
        self.AddImage(Images[i]);
  BpMain.OKButton.Action := CmdOK;
end;

procedure TFrmSelectImage.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFrmSelectImage.CmdOKUpdate(Sender: TObject);
begin
  CmdOK.Enabled := Assigned(LV.Selected);
end;

procedure TFrmSelectImage.FormShow(Sender: TObject);
begin
  LV.SetFocus;
end;

procedure TFrmSelectImage.AddImage(img: TIEmbededGraphics);
var
  m: TListItem;
begin
  m := LV.Items.Add;
  m.Caption := img.FileName;
  m.SubItems.Add(Format('%dx%d', [img.Width, img.Height]));
  m.Data := img;
  m.ImageIndex := AddMiniature(img);
end;

function TFrmSelectImage.AddMiniature(AImg: TIEmbededGraphics): integer;
var
  b: TCustomBitmap;
begin
  b := MakeMiniature(AImg as TIEmbededImage, ILMiniature.Width);
  try
    Result := ILMiniature.Add(b, nil);
  finally
    b.Free;
  end;
end;

procedure TFrmSelectImage.TrySelectImage(const AName: string);
var
  i: integer;
begin
  for i := 0 to LV.Items.Count - 1 do
    if SameText(AName, LV.Items[i].Caption) then
    begin
      LV.ItemIndex := i;
      break;
    end;
end;

end.

