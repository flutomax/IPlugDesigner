{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmSVG;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, ExtDlgs, FPImage, agg_fpimage, Agg_LCL, uCommon,
  uIDocument, uGraphics;

type

  { TFrmSVG }

  TFrmSVG = class(TForm)
    ActionList1: TActionList;
    BtnCancel: TButton;
    BtnOK: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button9: TButton;
    CmdAddImages: TAction;
    CmdClearImages: TAction;
    CmdDeleteImages: TAction;
    CmdExport: TAction;
    CmdMoveDown: TAction;
    CmdMoveUp: TAction;
    CmdOK: TAction;
    CmdReplaceImage: TAction;
    GbImages: TGroupBox;
    GbImages1: TGroupBox;
    ILMiniature: TImageList;
    Image: TImage;
    LV: TListView;
    DlgOpenImages: TOpenDialog;
    DlgExportImage: TSaveDialog;
    procedure CmdAddImagesExecute(Sender: TObject);
    procedure CmdClearImagesExecute(Sender: TObject);
    procedure CmdClearImagesUpdate(Sender: TObject);
    procedure CmdDeleteImagesExecute(Sender: TObject);
    procedure CmdDeleteImagesUpdate(Sender: TObject);
    procedure CmdExportExecute(Sender: TObject);
    procedure CmdMoveDownExecute(Sender: TObject);
    procedure CmdMoveUpExecute(Sender: TObject);
    procedure CmdOKExecute(Sender: TObject);
    procedure CmdReplaceImageExecute(Sender: TObject);
    procedure CmdReplaceImageUpdate(Sender: TObject);
    procedure LVDeletion(Sender: TObject; Item: TListItem);
    procedure LVKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    procedure AddImage(const aFileName: string); overload;
    procedure AddImage(img: TIEmbededSVG); overload;
    function AddMiniature(AImg: TIEmbededSVG): integer;
    function ImagenameExists(const aFileName: string): boolean;
  public
    PropertyChanged: Boolean;
  end;

var
  FrmSVG: TFrmSVG;

  procedure ShowSVG(doc: TIDocument);

implementation

{$R *.lfm}


uses
  uListViewUtils, uCommands;

procedure ShowSVG(doc: TIDocument);
var
  i: integer;
  svg: TIEmbededSVG;
  cmd: TSVGCacheCommand;
begin
  Application.CreateForm(TFrmSVG, FrmSVG);
  with FrmSVG do
    try
      for i := 0 to doc.SVGCache.ImagesCount - 1 do
      begin
        svg := TIEmbededSVG.Create;
        svg.Assign(doc.SVGCache.Images[i]);
        AddImage(svg);
      end;
      PropertyChanged := false;
      if ShowModal = mrOK then
      begin
        cmd := TSVGCacheCommand.Create(doc);
        doc.SVGCache.Clear;
        for i := 0 to LV.Items.Count - 1 do
          doc.SVGCache.AddImage(TIEmbededGraphics(LV.Items[i].Data));
        cmd.Commit;
        doc.History.Add(cmd);
      end;
    finally
      Release;
    end;
end;

{ TFrmSVG }

procedure TFrmSVG.AddImage(const aFileName: string);
var
  img: TIEmbededSVG;
begin
  if ImagenameExists(aFileName) then
  begin
    MessageDlg(AppTitle,
      Format('Can''t load "%s". SVG with this name already exists.',
      [ExtractFileName(aFileName)]), mtError, [mbOK], '');
    exit;
  end;
  img := TIEmbededSVG.Create;
  if img.LoadImageFile(aFileName) then
  begin
    AddImage(img);
  end
  else
  begin
    img.Free;
    MessageDlg(AppTitle,
      Format('Can''t load "%s". Format is not supported or file is corrupted.',
      [aFileName]), mtError, [mbOK], '');
  end;
end;

procedure TFrmSVG.AddImage(img: TIEmbededSVG);
var
  m: TListItem;
begin
  m := LV.Items.Add;
  m.Caption := img.FileName;
  m.SubItems.Add(Format('%dx%d', [img.Width, img.Height]));
  m.SubItems.Add(Format('%dx%d', [img.RealWidth, img.RealHeight]));
  m.Data := img;
  m.ImageIndex := AddMiniature(img);
end;

function TFrmSVG.AddMiniature(AImg: TIEmbededSVG): integer;
var
  b: TCustomBitmap;
begin
  b := TBitmap.Create;
  try
    b.PixelFormat := pf32bit;
    b.SetSize(ILMiniature.Width, ILMiniature.Height);
    AImg.RenderToMiniature(b);
    Result := ILMiniature.Add(b, nil);
  finally
    b.Free;
  end;
end;

function TFrmSVG.ImagenameExists(const aFileName: string): boolean;
var
  s: string;
  i: integer;
begin
  result := false;
  s := ExtractFileName(aFileName);
  for i := 0 to LV.Items.Count - 1 do
    if SameText(s, LV.Items[i].Caption) then
    begin
      result := true;
      break;
    end;
end;

procedure TFrmSVG.CmdAddImagesExecute(Sender: TObject);
var
  i: integer;
begin
  if not DlgOpenImages.Execute then
    exit;
  LV.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    for i := 0 to DlgOpenImages.Files.Count - 1 do
      AddImage(DlgOpenImages.Files[i]);
  finally
    LV.EndUpdate;
    Screen.Cursor := crDefault;
    PropertyChanged := true;
  end;
end;

procedure TFrmSVG.CmdClearImagesExecute(Sender: TObject);
begin
  LV.Clear;
  Image.Picture.Clear;
  PropertyChanged := true;
end;

procedure TFrmSVG.CmdClearImagesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LV.Items.Count > 0;
end;

procedure TFrmSVG.CmdDeleteImagesExecute(Sender: TObject);
begin
  TListViewUtils.DeleteItems(LV);
  PropertyChanged := true;
end;

procedure TFrmSVG.CmdDeleteImagesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LV.SelCount > 0;
end;

procedure TFrmSVG.CmdExportExecute(Sender: TObject);
var
  img: TIEmbededGraphics;
begin
  img := TIEmbededGraphics(LV.Selected.Data);
  if not DlgExportImage.Execute then
    exit;
  img.SaveToFile(DlgExportImage.FileName);
end;

procedure TFrmSVG.CmdMoveDownExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsDown(LV);
end;

procedure TFrmSVG.CmdMoveUpExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsUp(LV);
end;

procedure TFrmSVG.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFrmSVG.CmdReplaceImageExecute(Sender: TObject);
var
  img: TIEmbededSVG;
  m: TListItem;
begin
  if not DlgOpenImages.Execute then
    exit;
  LV.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    img := TIEmbededSVG.Create;
  if img.LoadImageFile(DlgOpenImages.FileName) then
  begin
    m := LV.Selected;
    TIEmbededSVG(m.Data).Free;
    m.Caption := IntToStr(m.Index);
    m.SubItems.Clear;
    m.SubItems.Add(ExtractFileName(DlgOpenImages.FileName));
    m.SubItems.Add(Format('%dx%d', [img.Width, img.Height]));
    m.SubItems.Add(Format('%dx%d', [img.RealWidth, img.RealHeight]));
    m.Data := img;
    m.ImageIndex := AddMiniature(img);
    LVSelectItem(LV, m, true);
  end
  finally
    LV.EndUpdate;
    Screen.Cursor := crDefault;
    PropertyChanged := true;
  end;
end;

procedure TFrmSVG.CmdReplaceImageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(LV.Selected);
end;

procedure TFrmSVG.LVDeletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item.Data) then
    TIEmbededGraphics(Item.Data).Free;
end;

procedure TFrmSVG.LVKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    Key := 0;
    CmdDeleteImages.Execute;
  end;
end;

procedure TFrmSVG.LVSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    Image.Picture.Bitmap.SetSize(Image.Height, Image.Height);
    TIEmbededSVG(Item.Data).RenderToPreview(Image.Picture.Bitmap);
  end
  else
    Image.Picture.Clear;
end;

end.

