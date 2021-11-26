{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uListViewUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls;

type
  TListItemDynArray = array of TListItem;

  { TListViewUtils }

  TListViewUtils = class
  private
    class var FDragStartPoint: TPoint;
    class function MovingConditionsMet(LV: TListView): boolean;
    class function GetSelectedItems(LV: TListView): TListItemDynArray;
    class function IndexOfFirstSelected(LV: TListView): integer;
    class function IndexOfLastSelected(LV: TListView): integer;
    class procedure SelectItems(Items: TListItemDynArray);
    class procedure MoveItems(LV: TListView; DestIndex: integer);
  public
    class procedure DragDrop(Sender, Source: TObject; X, Y: integer);
    class procedure DragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    class procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    class procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    class procedure MoveItemsUp(LV: TListView);
    class procedure MoveItemsDown(LV: TListView);
    class procedure MoveItemsTop(LV: TListView);
    class procedure MoveItemsBottom(LV: TListView);
    class procedure ToggleCheckItems(LV: TListView);
    class procedure ToggleSelectAll(LV: TListView);
    class procedure DeleteItems(LV: TListView);
    class function GetItems(LV: TListView; const ASep: char): string; overload;
    class procedure SetItems(LV: TListView; const ASettings: string;
      const ASep: char); overload;
    class function GetItems(LV: TListView): TStringArray; overload;
    class procedure SetItems(LV: TListView; const ASettings: TStringArray); overload;
  end;

implementation

class function TListViewUtils.MovingConditionsMet(LV: TListView): boolean;
begin
  Result := True;
  if (LV.Items.Count < 1) or (LV.SelCount < 1) or (LV.SelCount = LV.Items.Count) then
    Result := False;
end;

class function TListViewUtils.GetSelectedItems(LV: TListView
  ): TListItemDynArray;
var
  i, j: integer;
begin
  SetLength(Result, LV.SelCount);
  j := 0;
  for i := 0 to LV.Items.Count - 1 do
    if LV.Items[i].Selected then
    begin
      Result[j] := LV.Items[i];
      Inc(j);
    end;
end;

class function TListViewUtils.IndexOfFirstSelected(LV: TListView): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to LV.Items.Count - 1 do
    if LV.Items[i].Selected then
      Exit(i);
end;

class function TListViewUtils.IndexOfLastSelected(LV: TListView): integer;
var
  i: integer;
begin
  Result := -1;
  for i := LV.Items.Count - 1 downto 0 do
    if LV.Items[i].Selected then
      Exit(i);
end;

class procedure TListViewUtils.SelectItems(Items: TListItemDynArray);
var
  i: integer;
begin
  for i := High(Items) downto 0 do
    Items[i].Selected := True;
end;

class procedure TListViewUtils.MoveItems(LV: TListView; DestIndex: integer);
const
  PH = '.PlaceHolder.';
var
  TheItems: TListItemDynArray;
  tmp: TListItem;
  i: integer;
begin
  with LV do
  begin
    TheItems := GetSelectedItems(LV);
    ClearSelection;
    for i := 0 to High(TheItems) do
    begin
      Items.Add;
      Items[Items.Count - 1].Caption := PH;
      Items.Exchange(TheItems[i].Index, Items.Count - 1);
      TheItems[i] := Items[Items.Count - 1];
    end;
    for i := 0 to High(TheItems) do
      Items.Move(TheItems[i].Index, DestIndex + i);
    for i := 0 to High(TheItems) do
    begin
      tmp := Items.FindCaption(0, PH, False, True, False);
      if Assigned(tmp) then
        Items.Delete(tmp.Index);
    end;
    SelectItems(TheItems);
    SetFocus;
  end;
end;

class procedure TListViewUtils.DragDrop(Sender, Source: TObject; X, Y: integer);
var
  LV: TListView;
  Dest: TListItem;
begin
  if (Sender <> Source) then
    Exit;
  LV := (Sender as TListView);
  with LV do
  begin
    if not MovingConditionsMet(LV) then
      Exit;
    Dest := GetItemAt(X, Y);
    if not Assigned(Dest) then
    begin
      if (Y > FDragStartPoint.y) then
        MoveItemsBottom(LV)
      else
        MoveItemsTop(LV);
    end
    else
    begin
      BeginUpdate;
      try
        MoveItems(LV, Dest.Index);
      finally
        EndUpdate;
      end;
    end;
  end;
end;

class procedure TListViewUtils.DragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
  Accept := Sender = Source;
end;

class procedure TListViewUtils.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FDragStartPoint := Point(X, Y);
  {$ifdef LINUX}
  if not Assigned((Sender as TListView).GetItemAt(X, Y)) then
    (Sender as TListView).ClearSelection;
  {$endif}
  ;
end;

class procedure TListViewUtils.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if not (ssLeft in Shift) then
    (Sender as TListView).EndDrag(False);
end;

class procedure TListViewUtils.MoveItemsUp(LV: TListView);
var
  i: integer;
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then
      Exit;
    i := IndexOfFirstSelected(LV);
    Dec(i);
    if i < 0 then
      i := 0;
    BeginUpdate;
    try
      MoveItems(LV, i);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

class procedure TListViewUtils.MoveItemsDown(LV: TListView);
var
  i: integer;
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then
      Exit;
    i := IndexOfLastSelected(LV);
    Inc(i, 2);
    if i > Items.Count then
      i := Items.Count;
    BeginUpdate;
    try
      MoveItems(LV, i);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

class procedure TListViewUtils.MoveItemsTop(LV: TListView);
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then
      Exit;
    BeginUpdate;
    try
      MoveItems(LV, 0);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

class procedure TListViewUtils.MoveItemsBottom(LV: TListView);
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then
      Exit;
    BeginUpdate;
    try
      MoveItems(LV, Items.Count);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

class procedure TListViewUtils.ToggleCheckItems(LV: TListView);
var
  TheItems: TListItemDynArray;
  OnCheck: TLVCheckedItemEvent;
  i: integer;
  Check: boolean;
begin
  with LV do
  begin
    if (Items.Count < 1) or (SelCount < 1) or not CheckBoxes then
      Exit;
    OnCheck := OnItemChecked;
    OnItemChecked := nil;
    TheItems := GetSelectedItems(LV);
    Check := not TheItems[0].Checked;
    BeginUpdate;
    try
      for i := 0 to High(TheItems) do
        TheItems[i].Checked := Check;
    finally
      OnItemChecked := OnCheck;
      EndUpdate;
      {$IFDEF LINUX}
      Refresh;
      {$ENDIF}
    end;
    SetFocus;
  end;
end;

class procedure TListViewUtils.ToggleSelectAll(LV: TListView);
begin
  with LV do
  begin
    if Items.Count < 1 then
      Exit;
    if SelCount > 0 then
      ClearSelection
    else
      SelectAll;
    SetFocus;
  end;
end;

class procedure TListViewUtils.DeleteItems(LV: TListView);
var
  TheItems: TListItemDynArray;
  i: integer;
begin
  with LV do
  begin
    if SelCount < 1 then
      Exit;
    BeginUpdate;
    try
      if SelCount = Items.Count then
        Items.Clear
      else
      begin
        TheItems := GetSelectedItems(LV);
        for i := 0 to High(TheItems) do
          Items.Delete(TheItems[i].Index);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

class function TListViewUtils.GetItems(LV: TListView; const ASep: char): string;
var
  sl: TStringList;
  i, j: integer;
begin
  Result := EmptyStr;
  if ASep in [#0..#32] then
    Exit;
  with LV do
  begin
    sl := TStringList.Create;
    try
      for i := 0 to Items.Count - 1 do
      begin
        if i > 0 then
          sl.Add(ASep + Items[i].Checked.ToString)
        else
          sl.Add(Items[i].Checked.ToString);
        sl.Add(integer(Items[i].ImageIndex).ToString);
        sl.Add(Items[i].Caption);
        for j := 0 to Items[i].SubItems.Count - 1 do
          sl.Add(Items[i].SubItems[j]);
      end;
      Result := sl.CommaText;
    finally
      if Assigned(sl) then
        sl.Free;
    end;
  end;
end;

class procedure TListViewUtils.SetItems(LV: TListView; const ASettings: string;
  const ASep: char);
var
  OnCheck: TLVCheckedItemEvent;
  sl: TStringList;
  sa: TStringArray;
  i, j: integer;
begin
  if ASettings = EmptyStr then
    Exit;
  if ASep in [#0..#32] then
    Exit;
  with LV do
  begin
    OnCheck := OnItemChecked;
    OnItemChecked := nil;
    BeginUpdate;
    Clear;
    sl := TStringList.Create;
    try
      sa := ASettings.Split([ASep]);
      for i := 0 to High(sa) do
      begin
        sl.CommaText := sa[i];
        Items.Add;
        Items[i].Checked := Sl.Strings[0].ToBoolean;
        Items[i].ImageIndex := Sl.Strings[1].ToInteger;
        Items[i].Caption := Sl.Strings[2];
        for j := 3 to Sl.Count - 1 do
          Items[i].SubItems.Add(Sl[j]);
      end;
    finally
      EndUpdate;
      OnItemChecked := OnCheck;
      if Assigned(sl) then
        sl.Free;
    end;
  end;
end;

class function TListViewUtils.GetItems(LV: TListView): TStringArray;
var
  sl: TStringList;
  i, j: integer;
begin
  sl := TStringList.Create;
  try
    with LV do
    begin
      SetLength(Result, Items.Count);
      for i := 0 to Items.Count - 1 do
      begin
        sl.Clear;
        sl.Add(Items[i].Checked.ToString);
        sl.Add(integer(Items[i].ImageIndex).ToString);
        sl.Add(Items[i].Caption);
        for j := 0 to Items[i].SubItems.Count - 1 do
          sl.Add(Items[i].SubItems[j]);
        Result[i] := sl.CommaText;
      end;
    end;
  finally
    if Assigned(sl) then
      sl.Free;
  end;
end;

class procedure TListViewUtils.SetItems(LV: TListView;
  const ASettings: TStringArray);
var
  OnCheck: TLVCheckedItemEvent;
  sl: TStringList;
  i, j: integer;
begin
  if Length(ASettings) = 0 then
    Exit;
  with LV do
  begin
    OnCheck := OnItemChecked;
    OnItemChecked := nil;
    BeginUpdate;
    Clear;
    sl := TStringList.Create;
    try
      for i := 0 to High(ASettings) do
      begin
        sl.CommaText := ASettings[i];
        Items.Add;
        Items[i].Checked := Sl.Strings[0].ToBoolean;
        Items[i].ImageIndex := Sl.Strings[1].ToInteger;
        Items[i].Caption := Sl.Strings[2];
        for j := 3 to Sl.Count - 1 do
          Items[i].SubItems.Add(Sl[j]);
      end;
    finally
      EndUpdate;
      OnItemChecked := OnCheck;
      if Assigned(sl) then
        sl.Free;
    end;
  end;
end;


end.
