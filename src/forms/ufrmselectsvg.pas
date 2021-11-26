{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmSelectSVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ActnList, uCommon, uGraphics;

type

  { TFrmSelectSVG }

  TFrmSelectSVG = class(TForm)
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
    procedure AddImage(svg: TIEmbededSVG);
    function AddMiniature(svg: TIEmbededSVG): integer;
  public
    procedure TrySelectImage(const AName: string);
  end;

var
  FrmSelectSVG: TFrmSelectSVG;

  function ShowSelectSVGName(var ASVGName: TSVGName): boolean;

implementation

{$R *.lfm}

uses
  uFrmMain;

function ShowSelectSVGName(var ASVGName: TSVGName): boolean;
begin
  Application.CreateForm(TFrmSelectSVG, FrmSelectSVG);
  with FrmSelectSVG do
  try
    TrySelectImage(ASVGName);
    result := ShowModal = mrOK;
    if result then
      ASVGName := LV.Selected.Caption;
  finally
    Release;
  end;
end;


{ TFrmSelectSVG }

procedure TFrmSelectSVG.FormCreate(Sender: TObject);
var
  i: integer;
begin
  if Assigned(FrmMain.Document) then
    with FrmMain.Document.SVGCache do
      for i := 0 to ImagesCount - 1 do
        self.AddImage(Images[i] as TIEmbededSVG);
  BpMain.OKButton.Action := CmdOK;
end;

procedure TFrmSelectSVG.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFrmSelectSVG.CmdOKUpdate(Sender: TObject);
begin
  CmdOK.Enabled := Assigned(LV.Selected);
end;

procedure TFrmSelectSVG.FormShow(Sender: TObject);
begin
  LV.SetFocus;
end;

procedure TFrmSelectSVG.AddImage(svg: TIEmbededSVG);
var
  m: TListItem;
begin
  m := LV.Items.Add;
  m.Caption := svg.FileName;
  m.SubItems.Add(Format('%dx%d', [svg.Width, svg.Height]));
  m.Data := svg;
  m.ImageIndex := AddMiniature(svg);
end;

function TFrmSelectSVG.AddMiniature(svg: TIEmbededSVG): integer;
var
  b: TCustomBitmap;
begin
  b := TBitmap.Create;
  try
    b.PixelFormat := pf32bit;
    b.SetSize(ILMiniature.Width, ILMiniature.Height);
    svg.RenderToMiniature(b);
    Result := ILMiniature.Add(b, nil);
  finally
    b.Free;
  end;
end;

procedure TFrmSelectSVG.TrySelectImage(const AName: string);
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

{
procedure TFrmSelectSVG.TrySelectImages(const AName: string);
var
  i: integer;
begin
  LV.Checkboxes := true;
  for i := 0 to LV.Items.Count - 1 do
    if SameText(AName, LV.Items[i].Caption) then
    begin
      LV.Items[i].Checked := true;
    end;
end;


function TFrmSelectSVG.GetSelectedImages: string;
begin
  Result := '';
  for i := 0 to LV.Items.Count - 1 do
    if LV.Items[i].Checked then
      Result := Format('%s%s, ', [Result, LV.Items[i].Caption]);
  Result := Result.TrimRight;
  if Result[Result.Length] = ',' then
    Result := Copy(Result, 1, Result.Length - 1);
end;
}
end.

