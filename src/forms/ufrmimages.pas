unit uFrmImages;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, ExtDlgs, Spin, EditBtn, FPImage, agg_fpimage,
  Agg_LCL, SpinEx, uCommon, uIDocument, uGraphics;

type

  { TFrmImages }

  TFrmImages = class(TForm)
    EdStates: TCalcEdit;
    CkFramesAreHorizontal: TCheckBox;
    CmdOK: TAction;
    CmdExport: TAction;
    BtnOK: TButton;
    BtnCancel: TButton;
    Button9: TButton;
    CmdClearImages: TAction;
    Button6: TButton;
    CmdReplaceImage: TAction;
    Button4: TButton;
    Button5: TButton;
    CmdMoveDown: TAction;
    CmdMoveUp: TAction;
    Button3: TButton;
    CmdDeleteImages: TAction;
    CmdAddImages: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    EdTargetScale: TSpinEdit;
    GbImages: TGroupBox;
    GbImages1: TGroupBox;
    GbSelImgProperties: TGroupBox;
    ILMiniature: TImageList;
    DlgOpenImages: TOpenPictureDialog;
    Image: TImage;
    LbStates: TLabel;
    LbTargetScale: TLabel;
    LV: TListView;
    DlgExportImage: TSavePictureDialog;
    procedure CmdAddImagesExecute(Sender: TObject);
    procedure CmdClearImagesExecute(Sender: TObject);
    procedure CmdClearImagesUpdate(Sender: TObject);
    procedure CmdDeleteImagesExecute(Sender: TObject);
    procedure CmdDeleteImagesUpdate(Sender: TObject);
    procedure CmdExportExecute(Sender: TObject);
    procedure CmdMoveDownExecute(Sender: TObject);
    procedure CmdMoveUpExecute(Sender: TObject);
    procedure CmdOKExecute(Sender: TObject);
    procedure CmdOKUpdate(Sender: TObject);
    procedure CmdReplaceImageExecute(Sender: TObject);
    procedure CmdReplaceImageUpdate(Sender: TObject);
    procedure EdStatesChange(Sender: TObject);
    procedure LVDeletion(Sender: TObject; Item: TListItem);
    procedure LVKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    procedure AddImage(const aFileName: string); overload;
    procedure AddImage(img: TIEmbededImage); overload;
    procedure SetImageInfo(img: TIEmbededImage; m: TListItem);
    function AddMiniature(AImg: TIEmbededImage): integer;
    function ImagenameExists(const aFileName: string): boolean;
  public
    PropertyChanged: boolean;
  end;

var
  FrmImages: TFrmImages;

procedure ShowImages(doc: TIDocument);

implementation

uses
  uListViewUtils, uCommands;

{$R *.lfm}

procedure ShowImages(doc: TIDocument);
var
  i: integer;
  img: TIEmbededImage;
  cmd: TImageCacheCommand;
begin
  Application.CreateForm(TFrmImages, FrmImages);
  with FrmImages do
    try
      for i := 0 to doc.ImageCache.ImagesCount - 1 do
      begin
        img := TIEmbededImage.Create;
        img.Assign(doc.ImageCache.Images[i]);
        AddImage(img);
      end;
      PropertyChanged := False;
      if ShowModal = mrOk then
      begin
        cmd := TImageCacheCommand.Create(doc);
        doc.ImageCache.Clear;
        for i := 0 to LV.Items.Count - 1 do
          doc.ImageCache.AddImage(TIEmbededGraphics(LV.Items[i].Data));
        cmd.Commit;
        doc.History.Add(cmd);
      end;
    finally
      Release;
    end;
end;

{ TFrmImages }

procedure TFrmImages.CmdAddImagesExecute(Sender: TObject);
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
    PropertyChanged := True;
  end;
end;

procedure TFrmImages.CmdClearImagesExecute(Sender: TObject);
begin
  LV.Clear;
  Image.Picture.Clear;
  PropertyChanged := True;
end;

procedure TFrmImages.CmdClearImagesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LV.Items.Count > 0;
end;

procedure TFrmImages.CmdDeleteImagesExecute(Sender: TObject);
begin
  TListViewUtils.DeleteItems(LV);
  PropertyChanged := True;
end;

procedure TFrmImages.CmdDeleteImagesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LV.SelCount > 0;
end;

procedure TFrmImages.CmdExportExecute(Sender: TObject);
var
  img: TIEmbededGraphics;
begin
  img := TIEmbededGraphics(LV.Selected.Data);
  DlgExportImage.FileName := ChangeFileExt(img.FileName, '.png');
  if not DlgExportImage.Execute then
    exit;
  img.SaveToFile(DlgExportImage.FileName);
end;

procedure TFrmImages.CmdMoveDownExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsDown(LV);
end;

procedure TFrmImages.CmdMoveUpExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsUp(LV);
end;

procedure TFrmImages.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFrmImages.CmdOKUpdate(Sender: TObject);
begin
  CmdOK.Enabled := PropertyChanged;
end;

procedure TFrmImages.CmdReplaceImageExecute(Sender: TObject);
var
  img: TIEmbededImage;
  m: TListItem;
begin
  if not DlgOpenImages.Execute then
    exit;
  LV.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    img := TIEmbededImage.Create;
    if img.LoadImageFile(DlgOpenImages.FileName) then
    begin
      m := LV.Selected;
      TIEmbededImage(m.Data).Free;
      SetImageInfo(img, m);
      LVSelectItem(LV, m, True);
    end
  finally
    LV.EndUpdate;
    Screen.Cursor := crDefault;
    PropertyChanged := True;
  end;
end;

procedure TFrmImages.CmdReplaceImageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(LV.Selected);
end;

procedure TFrmImages.EdStatesChange(Sender: TObject);
var
  img: TIEmbededImage;
begin
  if LV.Selected = nil then
    exit;
  img := TIEmbededImage(LV.Selected.Data);
  img.States := StrToIntDef(EdStates.Text, 1);
  img.TargetScale := EdTargetScale.Value;
  img.FramesAreHorizontal := CkFramesAreHorizontal.Checked;
  SetImageInfo(img, LV.Selected);
  PropertyChanged := true;
end;

procedure TFrmImages.LVDeletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item.Data) then
    TIEmbededGraphics(Item.Data).Free;
end;

procedure TFrmImages.LVKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    Key := 0;
    CmdDeleteImages.Execute;
  end;
end;

procedure TFrmImages.LVSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
var
  ms: TMemoryStream;
  img: TIEmbededImage;
begin
  GbSelImgProperties.Enabled := Selected;
  if Selected then
  begin
    img := TIEmbededImage(Item.Data);
    ms := TMemoryStream.Create;
    try
      img.SaveToStream(ms);
      ms.Seek(0, 0);
      Image.Stretch := (img.Width > Image.Width) or (img.Height > Image.Height);
      Image.Proportional := Image.Stretch;
      Image.Picture.LoadFromStreamWithFileExt(ms, '.png');
    finally
      ms.Free;
    end;
    EdStates.Text := IntToStr(img.States);
    EdTargetScale.Value := img.TargetScale;
    CkFramesAreHorizontal.Checked := img.FramesAreHorizontal;
  end
  else
  begin
    Image.Picture.Clear;
    EdStates.Text := '1';
    EdTargetScale.Value := 0;
    CkFramesAreHorizontal.Checked := False;
  end;
end;

procedure TFrmImages.AddImage(const aFileName: string);
var
  img: TIEmbededImage;
begin
  if ImagenameExists(aFileName) then
  begin
    MessageDlg(AppTitle,
      Format('Can''t load "%s". Image with this name already exists.',
      [ExtractFileName(aFileName)]), mtError, [mbOK], '');
    exit;
  end;
  img := TIEmbededImage.Create;
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

procedure TFrmImages.AddImage(img: TIEmbededImage);
begin
  SetImageInfo(img, LV.Items.Add);
end;

procedure TFrmImages.SetImageInfo(img: TIEmbededImage; m: TListItem);
begin
  m.Caption := img.FileName;
  m.SubItems.Clear;
  m.SubItems.Add(Format('%dx%d', [img.Width, img.Height]));
  m.SubItems.Add(IntToStr(img.States));
  m.SubItems.Add(BooleanToStr(img.FramesAreHorizontal));
  m.SubItems.Add(IntToStr(img.TargetScale));
  m.Data := img;
  m.ImageIndex := AddMiniature(img);
end;

function TFrmImages.AddMiniature(AImg: TIEmbededImage): integer;
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

function TFrmImages.ImagenameExists(const aFileName: string): boolean;
var
  s: string;
  i: integer;
begin
  Result := False;
  s := ExtractFileName(aFileName);
  for i := 0 to LV.Items.Count - 1 do
    if SameText(s, LV.Items[i].Caption) then
    begin
      Result := True;
      break;
    end;
end;


end.
