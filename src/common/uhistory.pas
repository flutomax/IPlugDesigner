{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, uCommon;

type

  { THistory }

  THistory = class
  private
    fDepth: integer;
    fIndex: integer;
    fCommandState: TCommandState;
    fHistory: TList;
    fRecovering: boolean;
    function GetCommandCount: integer;
    function GetCount: integer;
    function GetCurrentCommandName: string;
    function GetNextCommandName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const cmd: TObject): integer;
    function Last: TObject;
    function SelectByIndex(const AIndex: integer): boolean;
    procedure Clear;
    procedure DeleteLast;
    procedure UpdateID(const OldIndex, NewIndex: QWord);
    procedure DeselectAll;
    procedure DeleteByRange(const AStartIndex, AEndIndex: integer);
    procedure Undo;
    procedure Redo;
    procedure UpdateUndo(const Target: TAction);
    procedure UpdateRedo(const Target: TAction);
    property Recovering: boolean read fRecovering;
    property Count: integer read GetCount;
  end;

implementation

uses
  Math, Controls, Forms, uCommands, uIniFileEx;

const
  S_UNDO = 'Undo ';
  S_REDO = 'Redo ';
  S_CANNOTUNDO = 'Can''t Undo';
  S_CANNOTREDO = 'Can''t Redo';

{ THistory }

constructor THistory.Create;
var
  ini: TIniFileEx;
begin
  fHistory := TList.Create;
  ini := TIniFileEx.Create(IniPath);
  try
    fDepth := EnsureRange(ini.ReadInteger(SCommon, 'MaxUndoStates', 100), 4, 32768);
  finally
    ini.Free;
  end;
  fIndex := -1;
  fRecovering := False;
  fCommandState := csNone;
end;

destructor THistory.Destroy;
begin
  Clear;
  fHistory.Free;
  inherited Destroy;
end;

procedure THistory.Clear;
var
  i: integer;
  cmd: TCommand;
begin
  if fHistory.Count = 0 then
    exit;
  for i := fHistory.Count - 1 downto 0 do
  begin
    cmd := TCommand(fHistory[i]);
    FreeAndNil(cmd);
  end;
  fHistory.Clear;
  fIndex := -1;
  fRecovering := False;
end;

function THistory.GetCommandCount: integer;
begin
  Result := fHistory.Count;
end;

function THistory.GetCount: integer;
begin
  result := fHistory.Count;
end;

function THistory.GetCurrentCommandName: string;
begin
  if (fIndex >= 0) and (fIndex < fHistory.Count) then
    Result := TCommand(fHistory[fIndex]).CommandName
  else
    Result := '';
end;

function THistory.GetNextCommandName: string;
begin
  if (fIndex + 1 >= 0) and (fIndex + 1 < fHistory.Count) then
    Result := TCommand(fHistory[fIndex + 1]).CommandName
  else
    Result := '';
end;

function THistory.Add(const cmd: TObject): integer;
var
  Diff: integer;
begin
  Result := -1;
  if (cmd = nil) then
    exit;
  if fRecovering then
  begin
    // ignore command
    cmd.Free;
    exit;
  end;
  if fIndex < (fHistory.Count - 1) then
    DeleteByRange(fIndex + 1, fHistory.Count - 1);
  fHistory.Add(cmd);
  // delete superfluous history states in the list
  Diff := fHistory.Count - fDepth;
  if Diff > 0 then
    DeleteByRange(0, diff - 1);
  SelectByIndex(fHistory.Count - 1);
  Result := fHistory.Count - 1;
  fIndex := Result;
  fCommandState := TCommand(cmd).CommandState;
end;

function THistory.Last: TObject;
begin
  result := TObject(fHistory.Last);
end;

procedure THistory.DeselectAll;
begin
  if fHistory.Count = 0 then
    exit;
  fIndex := -1;
end;

procedure THistory.DeleteLast;
var
  i: integer;
  cmd: TCommand;
begin
  i := fHistory.Count - 1;
  cmd := TCommand(fHistory[i]);
  FreeAndNil(cmd);
  fHistory.Delete(i);
end;

procedure THistory.DeleteByRange(const AStartIndex, AEndIndex: integer);
var
  i: integer;
  cmd: TCommand;
begin
  if fHistory.Count = 0 then
    exit;
  if (AStartIndex >= 0) and (AStartIndex < fHistory.Count) and
    (AEndIndex >= 0) and (AEndIndex < fHistory.Count) and
    (AStartIndex <= AEndIndex) then
  begin
    Screen.Cursor := crHourGlass;
    try
      for i := AEndIndex downto AStartIndex do
      begin
        cmd := TCommand(fHistory[i]);
        FreeAndNil(cmd);
        fHistory.Delete(i);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

function THistory.SelectByIndex(const AIndex: integer): boolean;
begin
  Result := False;
  if fHistory.Count = 0 then
    exit;
  if (AIndex >= 0) and (AIndex < fHistory.Count) then
  begin
    DeselectAll;
    fIndex := AIndex;
    Result := True;
  end;
end;

procedure THistory.UpdateID(const OldIndex, NewIndex: QWord);
var
  i: integer;
begin
  for i := 0 to fHistory.Count - 1 do
    TCommand(fHistory[i]).UpdateID(OldIndex, NewIndex);
end;

procedure THistory.Undo;
var
  cmd: TCommand;
begin
  fRecovering := True;
  try
    if (fIndex >= 0) and (fIndex < fHistory.Count) then
    begin
      cmd := TCommand(fHistory[fIndex]);
      cmd.Rollback;
      cmd.Enabled := False;
      fCommandState := cmd.CommandState;
      Dec(fIndex);
    end;
  finally
    fRecovering := False;
  end;
end;

procedure THistory.Redo;
var
  cmd: TCommand;
begin
  fRecovering := True;
  try
    Inc(fIndex);
    if (fIndex >= 0) and (fIndex < fHistory.Count) then
    begin
      cmd := TCommand(fHistory[fIndex]);
      cmd.Execute;
      cmd.Enabled := True;
      fCommandState := cmd.CommandState;
    end;
    if fIndex >= fHistory.Count then
      fIndex := fHistory.Count - 1;
  finally
    fRecovering := False;
  end;
end;

procedure THistory.UpdateUndo(const Target: TAction);
begin
  if (fIndex >= 0) and (fIndex < fHistory.Count) then
  begin
    Target.Caption := S_UNDO + GetCurrentCommandName;
    Target.Enabled := True;
  end
  else
  begin
    Target.Caption := S_CANNOTUNDO;
    Target.Enabled := False;
  end;
  Target.Hint := Target.Caption;
end;

procedure THistory.UpdateRedo(const Target: TAction);
begin
  if (fIndex < fHistory.Count - 1) then
  begin
    Target.Caption := S_REDO + GetNextCommandName;
    Target.Enabled := True;
  end
  else
  begin
    Target.Caption := S_CANNOTREDO;
    Target.Enabled := False;
  end;
  Target.Hint := Target.Caption;
end;

end.
