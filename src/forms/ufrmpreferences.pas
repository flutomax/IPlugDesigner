{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmPreferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ComCtrls, ExtCtrls, Spin, uCommon;

type

  { TFrmPreferences }

  TFrmPreferences = class(TForm)
    BpMain: TButtonPanel;
    BtnClearMRU: TButton;
    CbGridStyle: TComboBox;
    CkReplaceRoboto: TCheckBox;
    CkFileAssociation: TCheckBox;
    CkShowGrid: TCheckBox;
    CkShowHints: TCheckBox;
    CkSnapToGrid: TCheckBox;
    EdGridSize: TLabeledEdit;
    EdMaxMRU: TSpinEdit;
    EdMaxUndo: TSpinEdit;
    GpGrid: TGroupBox;
    LbGridStyle: TLabel;
    LbMaxMHU: TLabel;
    LbMaxUndo: TLabel;
    procedure BpMainClick(Sender: TObject);
    procedure BtnClearMRUClick(Sender: TObject);
    procedure CkSnapToGridChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    fFileAssociation: boolean;
    fOldUndo: integer;
    procedure SetFileAssociation(aValue: boolean);
  public
    property FileAssociation: boolean read fFileAssociation write SetFileAssociation;
  end;

var
  FrmPreferences: TFrmPreferences;

procedure ShowPreferences;

implementation

{$R *.lfm}

uses
{$IFDEF MSWINDOWS}
  uFileAssoc,
{$ENDIF}
  uFrmMain, uIDesigner, uIniFileEx;

procedure ShowPreferences;
begin
  Application.CreateForm(TFrmPreferences, FrmPreferences);
  try
    FrmPreferences.ShowModal;
  finally
    FrmPreferences.Release;
  end;
end;


{ TFrmPreferences }

procedure TFrmPreferences.FormCreate(Sender: TObject);
var
  ini: TIniFileEx;
begin
  BtnClearMRU.SetBounds(0, 8, 91, 26);
  CkShowHints.Checked := Application.ShowHint;
  CkReplaceRoboto.Checked := ReplaceRoboto;
  CkShowGrid.Checked := FrmMain.OutCtrl.ShowGrid;
  CkSnapToGrid.Checked := FrmMain.InCtrl.SnapToGrid;
  EdGridSize.Enabled := CkSnapToGrid.Checked;
  EdGridSize.Text := FloatToStr(FrmMain.InCtrl.GridSize);
  CbGridStyle.ItemIndex := Ord(FrmMain.OutCtrl.GridStyle);
  EdMaxMRU.Value := FrmMain.MRUList.MaxRecent;
  ini := TIniFileEx.Create(IniPath);
  try
    fFileAssociation := ini.ReadBool(SCommon, 'FileAssociation', False);
    fOldUndo := ini.ReadInteger(SCommon, 'MaxUndoStates', 100);
    EdMaxUndo.Value := fOldUndo;
  finally
    ini.Free;
  end;
{$IFDEF MSWINDOWS}
  CkFileAssociation.Checked := FileAssociation;
  CkFileAssociation.Enabled := true;
{$ENDIF}
end;

procedure TFrmPreferences.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  ini: TIniFileEx;
begin
  if ModalResult <> mrOk then
    exit;
  Application.ShowHint := CkShowHints.Checked;
  ReplaceRoboto := CkReplaceRoboto.Checked;
  FrmMain.OutCtrl.ShowGrid := CkShowGrid.Checked;
  FrmMain.InCtrl.SnapToGrid := CkSnapToGrid.Checked;
  FrmMain.InCtrl.GridSize := StrToFloatDef(EdGridSize.Text, 10);
  FrmMain.OutCtrl.GridStyle := TGridStyle(CbGridStyle.ItemIndex);
  FrmMain.MRUList.MaxRecent := EdMaxMRU.Value;
  FileAssociation := CkFileAssociation.Checked;
  ini := TIniFileEx.Create(IniPath);
  try
    ini.WriteBool(SCommon, 'FileAssociation', FileAssociation);
    ini.WriteInteger(SCommon, 'MaxUndoStates', EdMaxUndo.Value);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
  if EdMaxUndo.Value <> fOldUndo then
    MessageDlg('The new value of the undo history states will be active on the next design.',
    mtInformation, [mbOk], 0);
end;

procedure TFrmPreferences.CkSnapToGridChange(Sender: TObject);
begin
  EdGridSize.Enabled := CkSnapToGrid.Checked;
end;

procedure TFrmPreferences.BtnClearMRUClick(Sender: TObject);
const
  sClearMRU = 'The list of recent files will be empty.' +
    LineEnding + 'Are you sure you want to clear the list?';
begin
  if MessageDlg(sClearMRU, mtWarning, mbYesNo, 0) = mrYes then
    FrmMain.MRUList.Clear;
end;

procedure TFrmPreferences.BpMainClick(Sender: TObject);
begin

end;

procedure TFrmPreferences.SetFileAssociation(aValue: boolean);
{$IFDEF MSWINDOWS}
var
  a: TFileAssociation;
begin
  if fFileAssociation = aValue then
    Exit;
  fFileAssociation := aValue;
  a := TFileAssociation.Create(nil);
  try
    a.UnReg := not fFileAssociation;
    a.ApplicationName := AppTitle;
    a.ApplicationDescription := AppDescription;
    a.Extension := '.ipd';
    a.ExtensionName := 'IPlug Designer File';
    a.ExtensionIcon := Format('"%s",1', [Application.ExeName]);
    a.Action := Format('"%s" "%%1"', [Application.ExeName]);
    a.ActionName := 'Open';
    a.ActionIcon := Format('"%s",0', [Application.ExeName]);
    a.RegisterForAllUsers := True;
    // you can change it to False and register for current user only
    if a.Execute then
      a.ClearIconCache;
  finally
    a.Free;
  end;
end;
{$ELSE}
begin
  fFileAssociation := false;
end;
{$ENDIF}

end.

