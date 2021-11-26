{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmStyles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  LCLType, ExtCtrls, Spin, ActnList, ButtonPanel, Menus, uCommon, uIDocument,
  uVectorControls, Types;

type

  { TFrmStyles }

  TFrmStyles = class(TForm)
    CmdLoad: TAction;
    Button3: TButton;
    Button4: TButton;
    CmdSave: TAction;
    CmdRename: TAction;
    Button2: TButton;
    CmdOK: TAction;
    CmdMoveDown: TAction;
    CmdMoveUp: TAction;
    Button1: TButton;
    CmdClone: TAction;
    CmdDelete: TAction;
    BpMain: TButtonPanel;
    BtnAdd: TButton;
    BtnLabelFont: TButton;
    BtnValueFont: TButton;
    BtnDelete: TButton;
    CmdAdd: TAction;
    ActionList1: TActionList;
    GpOptions: TCheckGroup;
    EdShadowOffset: TFloatSpinEdit;
    EdRoundness: TFloatSpinEdit;
    EdFrameThickness: TFloatSpinEdit;
    EdWidgetFrac: TFloatSpinEdit;
    EdAngle: TFloatSpinEdit;
    GbStyles: TGroupBox;
    GpProperties: TGroupBox;
    GpColors: TGroupBox;
    GroupBox1: TGroupBox;
    ImgPreview: TImage;
    LblShadowOffset: TLabel;
    LblRoundness: TLabel;
    LblFrameThickness: TLabel;
    LblWidgetFrac: TLabel;
    LblAngle: TLabel;
    LbColors: TListBox;
    LV: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    DlgLoadStyle: TOpenDialog;
    PmLV: TPopupMenu;
    DlgSaveStyle: TSaveDialog;
    procedure BtnLabelFontClick(Sender: TObject);
    procedure BtnValueFontClick(Sender: TObject);
    procedure CmdAddExecute(Sender: TObject);
    procedure CmdCloneExecute(Sender: TObject);
    procedure CmdCloneUpdate(Sender: TObject);
    procedure CmdDeleteExecute(Sender: TObject);
    procedure CmdDeleteUpdate(Sender: TObject);
    procedure CmdLoadExecute(Sender: TObject);
    procedure CmdMoveDownExecute(Sender: TObject);
    procedure CmdMoveUpExecute(Sender: TObject);
    procedure CmdOKExecute(Sender: TObject);
    procedure CmdOKUpdate(Sender: TObject);
    procedure CmdRenameExecute(Sender: TObject);
    procedure CmdSaveExecute(Sender: TObject);
    procedure CmdSaveUpdate(Sender: TObject);
    procedure ImgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LVEdited(Sender: TObject; Item: TListItem; var AValue: string);
    procedure ParamChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GpOptionsItemClick(Sender: TObject; Index: integer);
    procedure LbColorsClick(Sender: TObject);
    procedure LbColorsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LVDeletion(Sender: TObject; Item: TListItem);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    fLoading: boolean;
    fPropertyChanged: boolean;
    fTestDown: boolean;
    function GetNewStyleName: string;
    function GetSelectedStyle: TIVStyle;
    procedure SetPropertyChanged(AValue: boolean);
    function StyleNameExists(const AName: string): boolean;
    procedure AddStyle(aStyle: TIVStyle);
    procedure DisplayStyle(Item: TListItem); overload;
    procedure DisplayStyle(sty: TIVStyle); overload;
    procedure AsynCall(Data: PtrInt);
    procedure PreviewStyle;
  public
    property PropertyChanged: boolean read fPropertyChanged write SetPropertyChanged;
    property SelectedStyle: TIVStyle read GetSelectedStyle;
  end;

var
  FrmStyles: TFrmStyles;

procedure ShowStyles(doc: TIDocument);

implementation

uses
  Math, uFrmColorDialog, uFrmFontDialog, uGraphics, uListViewUtils,
  uCommands;

procedure ShowStyles(doc: TIDocument);
var
  sty: TIVStyle;
  i: integer;
  cmd: TStyleCacheCommand;
begin
  Application.CreateForm(TFrmStyles, FrmStyles);
  with FrmStyles do
    try
      for i := 0 to doc.StyleCache.StylesCount - 1 do
      begin
        sty := TIVStyle.Create;
        sty.Assign(doc.StyleCache.Styles[i]);
        AddStyle(sty);
      end;
      PropertyChanged := False;
      if ShowModal = mrOk then
      begin
        cmd := TStyleCacheCommand.Create(doc);
        doc.StyleCache.Clear;
        for i := 0 to LV.Items.Count - 1 do
          doc.StyleCache.AddStyle(TIVStyle(LV.Items[i].Data));
        doc.Refresh;
        cmd.Commit;
        doc.History.Add(cmd);
      end;
    finally
      Release;
    end;
end;

{$R *.lfm}

{ TFrmStyles }

procedure TFrmStyles.FormCreate(Sender: TObject);
begin
  LbColors.ItemHeight := MulDiv(17, Screen.PixelsPerInch, 96);
  BpMain.OKButton.Action := CmdOK;
end;

procedure TFrmStyles.FormShow(Sender: TObject);
begin
  if LV.Items.Count > 0 then
    LV.ItemIndex := 0;
  if LV.CanFocus then
    LV.SetFocus;
  if LV.ItemIndex = 0 then
    PreviewStyle;
end;

function TFrmStyles.GetSelectedStyle: TIVStyle;
begin
  Result := nil;
  if LV.Selected = nil then
    exit;
  Result := TIVStyle(LV.Selected.Data);
end;

procedure TFrmStyles.SetPropertyChanged(AValue: boolean);
begin
  fPropertyChanged := AValue;
  if fPropertyChanged then
    PreviewStyle;
end;

procedure TFrmStyles.LbColorsClick(Sender: TObject);
var
  c: TIColor;
begin
  if (LbColors.ItemIndex < 0) or (SelectedStyle = nil) then
    exit;
  c := TIColor(PtrInt(LbColors.Items.Objects[LbColors.ItemIndex]));
  if not IColorDialog(c) then
    exit;
  LbColors.Items.Objects[LbColors.ItemIndex] := TObject(PtrInt(c));
  SelectedStyle.Colors[TEVColor(LbColors.ItemIndex)] := c;
  PropertyChanged := True;
end;

procedure TFrmStyles.LVDeletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item.Data) then
    TIVStyle(Item.Data).Free;
end;

procedure TFrmStyles.LVSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
begin
  GpProperties.Enabled := Selected;
  if Selected then
    DisplayStyle(Item);
end;

function TFrmStyles.GetNewStyleName: string;
var
  i: integer;
begin
  i := LV.Items.Count;
  repeat
    Result := 'Style' + IntToStr(i);
    Inc(i);
  until not StyleNameExists(Result);
end;

function TFrmStyles.StyleNameExists(const AName: string): boolean;
var
  i: integer;
  sty: TIVStyle;
begin
  Result := False;
  for i := 0 to LV.Items.Count - 1 do
  begin
    sty := TIVStyle(LV.Items[i].Data);
    if SameText(sty.Name, AName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TFrmStyles.CmdAddExecute(Sender: TObject);
var
  sty: TIVStyle;
begin
  sty := TIVStyle.Create;
  sty.Name := GetNewStyleName;
  AddStyle(sty);
  if LV.CanFocus then
    LV.SetFocus;
  LV.Selected.EditCaption;
  PropertyChanged := True;
end;

procedure TFrmStyles.CmdCloneExecute(Sender: TObject);
var
  sty: TIVStyle;
begin
  sty := SelectedStyle.Clone;
  sty.Name := GetNewStyleName;
  AddStyle(sty);
  if LV.CanFocus then
    LV.SetFocus;
  PropertyChanged := True;
end;

procedure TFrmStyles.CmdCloneUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(LV.Selected);
end;

procedure TFrmStyles.CmdDeleteExecute(Sender: TObject);
begin
  TListViewUtils.DeleteItems(LV);
  PropertyChanged := True;
end;

procedure TFrmStyles.CmdDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(SelectedStyle) and (not SelectedStyle.IsDefault);
end;

procedure TFrmStyles.CmdMoveDownExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsDown(LV);
end;

procedure TFrmStyles.CmdMoveUpExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsUp(LV);
end;

procedure TFrmStyles.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFrmStyles.CmdOKUpdate(Sender: TObject);
begin
  CmdOK.Enabled := PropertyChanged;
end;

procedure TFrmStyles.CmdRenameExecute(Sender: TObject);
begin
  LV.Selected.EditCaption;
end;

procedure TFrmStyles.CmdLoadExecute(Sender: TObject);
begin
  if not DlgLoadStyle.Execute then
    exit;
  SelectedStyle.LoadFromFile(DlgLoadStyle.FileName);
  DisplayStyle(SelectedStyle);
  PropertyChanged := True;
end;

procedure TFrmStyles.CmdSaveExecute(Sender: TObject);
begin
  if not DlgSaveStyle.Execute then
    exit;
  SelectedStyle.SaveToFile(DlgSaveStyle.FileName);
end;

procedure TFrmStyles.CmdSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(SelectedStyle);
end;

procedure TFrmStyles.ImgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fTestDown := true;
  PreviewStyle;
end;

procedure TFrmStyles.ImgPreviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fTestDown := false;
  PreviewStyle;
end;

procedure TFrmStyles.LVEdited(Sender: TObject; Item: TListItem; var AValue: string);
var
  sty: TIVStyle;
begin
  sty := TIVStyle(Item.Data);
  if sty.IsDefault then
  begin
    AValue := sty.Name;
    exit;
  end;
  if StyleNameExists(AValue) and (not SameText(sty.Name, AValue)) then
  begin
    MessageDlg(AppTitle, Format(SStyleAlreadyExists, [AValue]), mtWarning, [mbOK], '');
    AValue := sty.Name;
    Application.QueueAsyncCall(@AsynCall, 0);
    exit;
  end;
  if not IsValidIdentifier(AValue) then
  begin
    MessageDlg(AppTitle, Format(SStyleInvalidName, [AValue]), mtWarning, [mbOK], '');
    AValue := sty.Name;
    Application.QueueAsyncCall(@AsynCall, 0);
    exit;
  end;
  sty.Name := AValue;
end;

procedure TFrmStyles.AsynCall(Data: PtrInt);
begin
  CmdRename.Execute;
end;

procedure TFrmStyles.ParamChange(Sender: TObject);
var
  sty: TIVStyle;
begin
  sty := SelectedStyle;
  if fLoading or (sty = nil) then
    exit;
  sty.HideCursor := GpOptions.Checked[0];
  sty.ShowLabel := GpOptions.Checked[1];
  sty.ShowValue := GpOptions.Checked[2];
  sty.DrawFrame := GpOptions.Checked[3];
  sty.DrawShadows := GpOptions.Checked[4];
  sty.Emboss := GpOptions.Checked[5];
  sty.Roundness := EdRoundness.Value;
  sty.FrameThickness := EdFrameThickness.Value;
  sty.ShadowOffset := EdShadowOffset.Value;
  sty.WidgetFrac := EdWidgetFrac.Value;
  sty.Angle := EdAngle.Value;
  PropertyChanged := True;
end;

procedure TFrmStyles.GpOptionsItemClick(Sender: TObject; Index: integer);
begin
  ParamChange(Sender);
end;

procedure TFrmStyles.BtnLabelFontClick(Sender: TObject);
var
  sty: TIVStyle;
begin
  sty := SelectedStyle;
  if sty = nil then
    exit;
  if ShowFontDialog(sty.LabelFont) then
    PropertyChanged := True;
end;

procedure TFrmStyles.BtnValueFontClick(Sender: TObject);
var
  sty: TIVStyle;
begin
  sty := GetSelectedStyle;
  if sty = nil then
    exit;
  if ShowFontDialog(sty.ValueFont) then
    PropertyChanged := True;
end;

procedure TFrmStyles.LbColorsDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  rt: integer;
  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
begin
  if Index = -1 then
    Exit;
  rt := (ARect.Bottom - ARect.Top) + ARect.Left - 2;
  r := Rect(ARect.Left + 1, ARect.Top + 1, rt - 1, ARect.Bottom - 3);
  if odDisabled in State then
    LbColors.Canvas.Brush.Color := clBtnFace;
  if not (odBackgroundPainted in State) then
    LbColors.Canvas.FillRect(ARect);
  DrawIColorRect(LbColors.Canvas, r, TIColor(PtrInt(LbColors.Items.Objects[Index])),
    not (odDisabled in State), True);
  r := ARect;
  r.left := rt + 4;
  OldBrushStyle := LbColors.Canvas.Brush.Style;
  LbColors.Canvas.Brush.Style := bsClear;
  OldTextStyle := LbColors.Canvas.TextStyle;
  NewTextStyle := OldTextStyle;
  NewTextStyle.Layout := tlCenter;
  NewTextStyle.RightToLeft := Control.UseRightToLeftReading;
  NewTextStyle.Alignment := taLeftJustify;
  LbColors.Canvas.Font.Color :=
    IfThen(odDisabled in State, clGrayText, IfThen(odSelected in
    State, clHighlightText, clWindowText));
  LbColors.Canvas.TextStyle := NewTextStyle;
  LbColors.Canvas.TextRect(r, r.Left, r.Top, LbColors.Items[Index]);
  LbColors.Canvas.Brush.Style := OldBrushStyle;
  LbColors.Canvas.TextStyle := OldTextStyle;
end;

procedure TFrmStyles.AddStyle(aStyle: TIVStyle);
var
  m: TListItem;
begin
  m := LV.Items.Add;
  m.Caption := aStyle.Name;
  m.Data := aStyle;
  m.ImageIndex := 32;
  LV.ItemIndex := m.Index;
  DisplayStyle(m);
end;

procedure TFrmStyles.DisplayStyle(Item: TListItem);
var
  sty: TIVStyle;
begin
  sty := TIVStyle(Item.Data);
  DisplayStyle(sty);
end;

procedure TFrmStyles.DisplayStyle(sty: TIVStyle);
var
  i: TEVColor;
begin
  fLoading := True;
  try
    GpOptions.Checked[0] := sty.HideCursor;
    GpOptions.Checked[1] := sty.ShowLabel;
    GpOptions.Checked[2] := sty.ShowValue;
    GpOptions.Checked[3] := sty.DrawFrame;
    GpOptions.Checked[4] := sty.DrawShadows;
    GpOptions.Checked[5] := sty.Emboss;

    EdRoundness.Value := sty.Roundness;
    EdFrameThickness.Value := sty.FrameThickness;
    EdShadowOffset.Value := sty.ShadowOffset;
    EdWidgetFrac.Value := sty.WidgetFrac;
    EdAngle.Value := sty.Angle;
    for i := Low(TEVColor) to High(TEVColor) do
      LbColors.Items.Objects[Ord(i)] := TObject(PtrInt(sty.Colors[i]));
    LbColors.Invalidate;
  finally
    fLoading := False;
  end;
end;

procedure TFrmStyles.PreviewStyle;
var
  c: TICanvas;
  r: TIFloatRect;
  t: TIVTestControl;
begin
  r := TIFloatRect.Create(ImgPreview.ClientRect);
  c := TICanvas.Create(ImgPreview.Width, ImgPreview.Height);
  try
    c.ClipRect := r;
    //c.TransformSpace(r, r);
    c.BrushColor := ColorToIColor(GetDefaultColor(dctBrush));
    c.PenWidth := 0;
    c.DrawRect(r);
    t := TIVTestControl.Create(SelectedStyle, r.GetCentredInside(75, 75), c);
    try
      t.Paint(fTestDown);
    finally
      t.Free;
    end;
    c.DrawTo(ImgPreview.Picture.Bitmap);
  finally
    c.Free;
  end;
end;

end.


