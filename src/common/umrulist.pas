{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uMRUList;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, Menus, IniFiles, FileCtrl, Forms,
  ExtCtrls, ImgList;

type

  TRecentMenuItem = class(TMenuItem)
  private
    fFileName: string;
  public
    property FileName: string read fFileName;
  end;

  TRecentMenuItemClass = class of TRecentMenuItem;

  TOnRecentFileEvent = procedure(Sender: TObject; const aFileName: string) of object;

  { TMRUList }

  TMRUList = class(TComponent)
  private
    fRecent: TStrings;
    fMaxRecent: integer;
    fMIRecent: TMenuItem;
    fTopSepar: TMenuItem;
    fBotSepar: TMenuItem;
    fMinimizeWidth: integer;
    fOnRecent: TOnRecentFileEvent;
    function CreateMenuItem(aOwner: TComponent): TRecentMenuItem;
    function CreateMenuCaption(aIndex: integer; const aFileName: string): string;
    procedure DoOnRecentClick(Sender: TObject);
    procedure SetMaxRecent(AValue: integer);
    procedure SetTopSepar(const aValue: TMenuItem);
    procedure SetBotSepar(const aValue: TMenuItem);
    procedure SetMIRecent(const aValue: TMenuItem);
    procedure SetRecent(const aValue: TStrings);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddToRecent(const aFileName: string);
    procedure RemoveFromRecent(const aFileName: string);
    procedure UpdateRecentFiles;
    procedure LoadFromIni(Ini: TCustomIniFile; const aSection: string);
    procedure SaveToIni(Ini: TCustomIniFile; const aSection: string);
  published
    property MaxRecent: integer read fMaxRecent write SetMaxRecent default 10;
    property MinimizeWidth: integer read fMinimizeWidth write fMinimizeWidth default 200;
    property MIRecent: TMenuItem read fMIRecent write SetMIRecent;
    property TopSepar: TMenuItem read fTopSepar write SetTopSepar;
    property BotSepar: TMenuItem read fBotSepar write SetBotSepar;
    property OnRecent: TOnRecentFileEvent read fOnRecent write fOnRecent;
  end;

implementation

const

  KeyMaxRecent = 'MaxRecent';
  KeyCount = 'Count';
  KeyFile = 'File%d';

{ TMRUList }

constructor TMRUList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRecent := TStringList.Create;
  fMaxRecent := 10;
  fMinimizeWidth := 200;
end;

destructor TMRUList.Destroy;
begin
  FreeAndNil(fRecent);
  inherited Destroy;
end;

procedure TMRUList.Clear;
begin
  fRecent.Clear;
  UpdateRecentFiles;
end;

procedure TMRUList.UpdateRecentFiles;
var
  i, a, b, n: integer;
  m: TRecentMenuItem;
begin
  if (not Assigned(fMIRecent)) or (not Assigned(fTopSepar)) or
    (not Assigned(fBotSepar)) then
    exit;
  a := fMIRecent.IndexOf(fTopSepar) + 1;
  b := fMIRecent.IndexOf(fBotSepar) - 1;
  for i := b downto a do
    fMIRecent.Delete(i);
  n := 0;
  for i := 0 to fRecent.Count - 1 do
  begin
    m := CreateMenuItem(self.Owner);
    m.Caption := CreateMenuCaption(i, fRecent[i]);
    m.fFileName := fRecent[i];
    m.OnClick := @DoOnRecentClick;
    fMIRecent.Insert(a + i, m);
    Inc(n);
  end;
  fBotSepar.Visible := n > 0;
end;

procedure TMRUList.AddToRecent(const aFileName: string);
var
  i: integer;
  s: string;
begin
  s := ExpandFileName(aFileName);
  i := fRecent.IndexOf(s);
  if i > -1 then
  begin
    if i > 0 then
      fRecent.Exchange(0, i);
  end
  else
  begin
    while fRecent.Count >= fMaxRecent do
      fRecent.Delete(fRecent.Count - 1);
    fRecent.Insert(0, s);
  end;
  UpdateRecentFiles;
end;

procedure TMRUList.RemoveFromRecent(const aFileName: string);
var
  i: integer;
begin
  i := fRecent.IndexOf(ExpandFileName(aFileName));
  if i > -1 then
    fRecent.Delete(i);
end;

procedure TMRUList.LoadFromIni(Ini: TCustomIniFile; const aSection: string);
var
  i, c: integer;
  s: string;
begin
  fRecent.Clear;
  fMaxRecent := Ini.ReadInteger(aSection, KeyMaxRecent, 10);
  c := Ini.ReadInteger(aSection, KeyCount, 0);
  for i := 1 to c do
  begin
    s := Ini.ReadString(aSection, Format(KeyFile, [i]), '');
    if s <> '' then
      fRecent.Add(s);
  end;
end;

procedure TMRUList.SaveToIni(Ini: TCustomIniFile; const aSection: string);
var
  i: integer;
begin
  Ini.EraseSection(aSection);
  Ini.WriteInteger(aSection, KeyMaxRecent, fMaxRecent);
  Ini.WriteInteger(aSection, KeyCount, fRecent.Count);
  for i := 0 to fRecent.Count - 1 do
    Ini.WriteString(aSection, Format(KeyFile, [i + 1]), fRecent[i]);
  Ini.UpdateFile;
end;

function TMRUList.CreateMenuItem(aOwner: TComponent): TRecentMenuItem;
begin
  Result := TRecentMenuItem.Create(aOwner);
end;

function TMRUList.CreateMenuCaption(aIndex: integer; const aFileName: string): string;
begin
  if (fMinimizeWidth > 0) and Assigned(Application.MainForm) then
    Result := Format('%d. %s', [aIndex + 1, MiniMizeName(aFileName,
      Application.MainForm.Canvas, fMinimizeWidth)])
  else
    Result := Format('%d. %s', [aIndex + 1, aFileName]);
end;

procedure TMRUList.DoOnRecentClick(Sender: TObject);
var
  s: string;
begin
  if Assigned(Sender) and (Sender is TRecentMenuItem) then
    s := (Sender as TRecentMenuItem).FileName;
  if (s <> '') and Assigned(fOnRecent) then
    fOnRecent(self, s);
end;

procedure TMRUList.SetMaxRecent(AValue: integer);
var
  i: integer;
begin
  if fMaxRecent = AValue then
    Exit;
  fMaxRecent := AValue;
  for i := fRecent.Count - 1 downto fMaxRecent do
    fRecent.Delete(i);
  UpdateRecentFiles;
end;

procedure TMRUList.SetTopSepar(const aValue: TMenuItem);
begin
  if fTopSepar = aValue then
    exit;
  if Assigned(fTopSepar) then
    fTopSepar.RemoveFreeNotification(Self);
  fTopSepar := aValue;
  if Assigned(fTopSepar) then
    fTopSepar.FreeNotification(Self);
  UpdateRecentFiles;
end;

procedure TMRUList.SetBotSepar(const aValue: TMenuItem);
begin
  if fBotSepar = aValue then
    exit;
  if Assigned(fBotSepar) then
    fBotSepar.RemoveFreeNotification(Self);
  fBotSepar := aValue;
  if Assigned(fBotSepar) then
    fBotSepar.FreeNotification(Self);
  UpdateRecentFiles;
end;

procedure TMRUList.SetMIRecent(const aValue: TMenuItem);
begin
  if fMIRecent = aValue then
    exit;
  if Assigned(fMIRecent) then
    fMIRecent.RemoveFreeNotification(Self);
  fMIRecent := aValue;
  if Assigned(fMIRecent) then
    fMIRecent.FreeNotification(Self);
  UpdateRecentFiles;
end;

procedure TMRUList.SetRecent(const aValue: TStrings);
begin
  if fRecent = aValue then
    exit;
  fRecent.Assign(aValue);
  UpdateRecentFiles;
end;

procedure TMRUList.Loaded;
begin
  inherited Loaded;
  if (fRecent.Count > 0) and Assigned(fMIRecent) then
    UpdateRecentFiles;
end;



end.
