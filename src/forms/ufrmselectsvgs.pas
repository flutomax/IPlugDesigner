{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmSelectSVGs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, uCommon, uGraphics;

type

  { TFrmSelectSVGs }

  TFrmSelectSVGs = class(TForm)
    Button6: TButton;
    CmdCopyAllSVG: TAction;
    CmdClear: TAction;
    CmdDelete: TAction;
    CmdMoveDown: TAction;
    CmdMoveUp: TAction;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CmdCopySelectedSVG: TAction;
    ActionList1: TActionList;
    BpMain: TButtonPanel;
    Button1: TButton;
    CmdOK: TAction;
    GbProjectSVGs: TGroupBox;
    GbSelectedSVGs: TGroupBox;
    ILMiniature: TImageList;
    LVProject: TListView;
    LVSelected: TListView;
    PnNav: TPanel;
    procedure CmdClearExecute(Sender: TObject);
    procedure CmdClearUpdate(Sender: TObject);
    procedure CmdCopyAllSVGExecute(Sender: TObject);
    procedure CmdCopyAllSVGUpdate(Sender: TObject);
    procedure CmdCopySelectedSVGExecute(Sender: TObject);
    procedure CmdCopySelectedSVGUpdate(Sender: TObject);
    procedure CmdDeleteExecute(Sender: TObject);
    procedure CmdMoveDownExecute(Sender: TObject);
    procedure CmdMoveUpExecute(Sender: TObject);
    procedure CmdMoveUpUpdate(Sender: TObject);
    procedure CmdOKExecute(Sender: TObject);
    procedure CmdOKUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LVSelectedChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    fChanged: boolean;
    function GetProjectSVG(const src: string): integer;
    function GetSelectedSVG(const src: string): integer;
    function GetSelectedImages: string;
    procedure SetSelectedImages(AValue: string);
    procedure AddImage(svg: TIEmbededSVG);
    function AddMiniature(svg: TIEmbededSVG): integer;
  public
    property SelectedImages: string read GetSelectedImages write SetSelectedImages;
  end;

var
  FrmSelectSVGs: TFrmSelectSVGs;

function ShowSelectSVGNames(var ASVGName: TSVGNames): boolean;

implementation

{$R *.lfm}

uses uFrmMain, uListViewUtils;

function ShowSelectSVGNames(var ASVGName: TSVGNames): boolean;
begin
  Application.CreateForm(TFrmSelectSVGs, FrmSelectSVGs);
  with FrmSelectSVGs do
    try
      SelectedImages := ASVGName;
      Result := ShowModal = mrOk;
      if Result then
        ASVGName := SelectedImages;
    finally
      Release;
    end;
end;

{ TFrmSelectSVGs }

procedure TFrmSelectSVGs.FormCreate(Sender: TObject);
var
  i: integer;
begin
  fChanged := False;
  if Assigned(FrmMain.Document) then
    with FrmMain.Document.SVGCache do
      for i := 0 to ImagesCount - 1 do
        self.AddImage(Images[i] as TIEmbededSVG);
  BpMain.OKButton.Action := CmdOK;
end;

procedure TFrmSelectSVGs.LVSelectedChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctText then
    fChanged := True;
end;

procedure TFrmSelectSVGs.CmdOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFrmSelectSVGs.CmdCopySelectedSVGUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LVProject.SelCount > 0;
end;

procedure TFrmSelectSVGs.CmdDeleteExecute(Sender: TObject);
begin
  TListViewUtils.DeleteItems(LVSelected);
end;

procedure TFrmSelectSVGs.CmdMoveDownExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsDown(LVSelected);
end;

procedure TFrmSelectSVGs.CmdMoveUpExecute(Sender: TObject);
begin
  TListViewUtils.MoveItemsUp(LVSelected);
end;

procedure TFrmSelectSVGs.CmdMoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LVSelected.SelCount > 0;
end;

procedure TFrmSelectSVGs.CmdClearUpdate(Sender: TObject);
begin
  CmdClear.Enabled := LVSelected.Items.Count > 0;
end;

procedure TFrmSelectSVGs.CmdCopyAllSVGExecute(Sender: TObject);
var
  i: integer;
  m: TListItem;
begin
  for i := 0 to LVProject.Items.Count - 1 do
  begin
    m := LVSelected.Items.Add;
    m.Caption := LVProject.Items[i].Caption;
    m.ImageIndex := LVProject.Items[i].ImageIndex;
  end;
end;

procedure TFrmSelectSVGs.CmdCopyAllSVGUpdate(Sender: TObject);
begin
  CmdCopyAllSVG.Enabled:= LVProject.Items.Count > 0;
end;

procedure TFrmSelectSVGs.CmdClearExecute(Sender: TObject);
begin
  LVSelected.Clear;
end;

procedure TFrmSelectSVGs.CmdCopySelectedSVGExecute(Sender: TObject);
var
  i: integer;
  m: TListItem;
begin
  for i := 0 to LVProject.Items.Count - 1 do
    if LVProject.Items[i].Selected then
    begin
      m := LVSelected.Items.Add;
      m.Caption := LVProject.Items[i].Caption;
      m.ImageIndex := LVProject.Items[i].ImageIndex;
    end;
end;

procedure TFrmSelectSVGs.CmdOKUpdate(Sender: TObject);
begin
  CmdOK.Enabled := fChanged;
end;

function TFrmSelectSVGs.GetSelectedImages: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to LVSelected.Items.Count - 1 do
    if i = LVSelected.Items.Count - 1 then
      Result := Format('%s%s', [Result, LVSelected.Items[i].Caption])
    else
      Result := Format('%s%s, ', [Result, LVSelected.Items[i].Caption]);
end;

function TFrmSelectSVGs.GetProjectSVG(const src: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to LVProject.Items.Count - 1 do
    if LVProject.Items[i].Caption = src then
      exit(i);
end;

function TFrmSelectSVGs.GetSelectedSVG(const src: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to LVSelected.Items.Count - 1 do
    if LVSelected.Items[i].Caption = src then
      exit(i);
end;

procedure TFrmSelectSVGs.SetSelectedImages(AValue: string);
var
  Lst: TStringList;
  i, idx: integer;
  m: TListItem;
begin
  Lst := TStringList.Create;
  try
    Lst.CommaText := NormalizeOptStr(AValue);
    for i := 0 to Lst.Count - 1 do
    begin
      idx := GetProjectSVG(Lst[i]);
      if idx < 0 then
        continue;
      m := LVProject.Items[idx];
      idx := m.ImageIndex;
      m := LVSelected.Items.Add;
      m.Caption := Lst[i];
      m.ImageIndex := idx;
    end;
  finally
    Lst.Free;
  end;
  fChanged := False;
end;

procedure TFrmSelectSVGs.AddImage(svg: TIEmbededSVG);
var
  m: TListItem;
begin
  m := LVProject.Items.Add;
  m.Caption := svg.FileName;
  m.SubItems.Add(Format('%dx%d', [svg.Width, svg.Height]));
  m.ImageIndex := AddMiniature(svg);
end;

function TFrmSelectSVGs.AddMiniature(svg: TIEmbededSVG): integer;
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

end.

