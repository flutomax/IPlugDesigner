{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIniFileEx;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, Graphics, IniFiles;

type

  { TIniFileEx }

  TIniFileEx = class(TMemIniFile)
  public
    procedure WriteBool(const Section, Ident: string; Value: boolean); override;
    procedure WriteFloat(const Section, Name: string; Value: double); override;
    procedure WriteExtended(const Section, Name: string; Value: extended);
    procedure WriteHex(const Section, Name: string; Value: cardinal);
    procedure WriteColor(const Section, Name: string; Value: TColor);
    procedure WriteSize(const Section, Name: string; const aSize: TSize);
    procedure WriteStringList(const Section, Name: string; Value: TStrings;
      const Limit: integer = -1);
    procedure WriteObject(const Section, Name: string; const Buffer;
      const Size: integer);
    procedure WriteMemoryStream(const Section, Name: string;
      MemoryStream: TMemoryStream);
    function ReadBool(const Section, Ident: string; Default: boolean): boolean; override;
    function ReadFloat(const Section, Name: string; Default: double): double; override;
    function ReadExtended(const Section, Name: string; Default: extended): extended;
    function ReadHex(const Section, Name: string; Default: cardinal): cardinal;
    function ReadColor(const Section, Name: string; Default: TColor): TColor;
    function ReadSize(const Section, Name: string; var aSize: TSize): boolean;
    function ReadStringList(const Section, Name: string; Value: TStrings): boolean;
    function ReadObject(const Section, Name: string; var Buffer;
      const Size: integer): boolean;
    function ReadMemoryStream(const Section, Name: string;
      MemoryStream: TMemoryStream): boolean;
  end;

implementation

uses
  RtlConsts, Math;

function BinToText(const Buffer; const Size: integer): string;
var
  p: PByte;
  i: integer;
begin
  Result := '';
  p := @Buffer;
  for i := 1 to Size do
  begin
    Result := Result + IntToHex(p^, 2);
    Inc(p);
  end;
end;

function TextToBin(Text: string; var Buffer; const Size: integer): boolean;

  function ExtractByte(var B: byte): boolean;
  var
    Code: integer;
  begin
    Val('$' + Copy(Text, 1, 2), B, Code);
    Result := Code = 0;
    if Result then
      Delete(Text, 1, 2);
  end;

var
  P: PByte;
begin
  Result := Length(Text) = Size * 2;
  if not Result then
    Exit;
  P := @Buffer;
  while Text <> '' do
  begin
    if not ExtractByte(P^) then
    begin
      Result := False;
      Exit;
    end;
    Inc(P);
  end;
end;


{ TIniFileEx }

function TIniFileEx.ReadBool(const Section, Ident: string; Default: boolean): boolean;
begin
  Result := StrToBoolDef(ReadString(Section, Ident, ''), Default);
end;

function TIniFileEx.ReadFloat(const Section, Name: string; Default: double): double;
var
  FloatStr: string;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := StrToFloatDef(FloatStr, Default);
end;

function TIniFileEx.ReadExtended(const Section, Name: string;
  Default: extended): extended;
var
  FloatStr: string;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := StrToFloatDef(FloatStr, Default);
end;

function TIniFileEx.ReadHex(const Section, Name: string; Default: cardinal): cardinal;
var
  s: string;
  e: integer;
begin
  s := Format('$%.4x', [Default]);
  s := ReadString(Section, Name, s);
  Val(s, Result, e);
  if e <> 0 then
    Result := Default;
end;

function TIniFileEx.ReadColor(const Section, Name: string; Default: TColor): TColor;
var
  s: string;
begin
  s := ReadString(Section, Name, ColorToString(Default));
  Result := StringToColor(s);
end;

function TIniFileEx.ReadSize(const Section, Name: string; var aSize: TSize): boolean;
var
  s: string;
  L: TStringList;
begin
  Result := False;
  s := Format('%d, %d', [aSize.cx, aSize.cy]);
  s := ReadString(Section, Name, s);
  L := TStringList.Create;
  try
    L.CommaText := s;
    if L.Count = 2 then
    begin
      aSize.cx := StrToIntDef(L[0], aSize.cx);
      aSize.cy := StrToIntDef(L[1], aSize.cy);
      Result := True;
    end;
  finally
    L.Free;
  end;
end;

procedure TIniFileEx.WriteBool(const Section, Ident: string; Value: boolean);
const
  Values: array[boolean] of string = ('False', 'True');
begin
  WriteString(Section, Ident, Values[Value]);
end;

procedure TIniFileEx.WriteFloat(const Section, Name: string; Value: double);
begin
  WriteString(Section, Name, FloatToStr(Value));
end;

procedure TIniFileEx.WriteExtended(const Section, Name: string; Value: extended);
begin
  WriteString(Section, Name, FloatToStrF(Value, ffgeneral, 18, 22));
end;

procedure TIniFileEx.WriteHex(const Section, Name: string; Value: cardinal);
begin
  WriteString(Section, Name, Format('$%.8x', [Value]));
end;

procedure TIniFileEx.WriteColor(const Section, Name: string; Value: TColor);
begin
  WriteString(Section, Name, ColorToString(Value));
end;

procedure TIniFileEx.WriteSize(const Section, Name: string; const aSize: TSize);
begin
  WriteString(Section, Name, Format('%d, %d', [aSize.cx, aSize.cy]));
end;

procedure TIniFileEx.WriteStringList(const Section, Name: string;
  Value: TStrings; const Limit: integer);
var
  i, m: integer;
  s: string;
begin
  s := '';
  if Limit < 0 then
    m := Value.Count
  else
    m := Min(Value.Count, Limit);

  for i := 0 to m - 1 do
    s := s + '"' + Value[i] + '", ';

  s := TrimRight(s);
  Delete(s, Length(s), 1);
  WriteString(Section, Name, s);
end;

procedure TIniFileEx.WriteObject(const Section, Name: string; const Buffer;
  const Size: integer);
begin
  WriteString(Section, Name, BinToText(Buffer, Size));
end;

procedure TIniFileEx.WriteMemoryStream(const Section, Name: string;
  MemoryStream: TMemoryStream);
begin
  WriteString(Section, Name, BinToText(MemoryStream.Memory^, MemoryStream.Size));
end;

function TIniFileEx.ReadStringList(const Section, Name: string;
  Value: TStrings): boolean;
var
  s: string;
begin
  s := ReadString(Section, Name, '');
  Result := s <> '';
  if not Result then
    Exit;
  Value.BeginUpdate;
  try
    Value.Clear;
    Value.CommaText := s;
  finally
    Value.EndUpdate;
  end;
end;

function TIniFileEx.ReadObject(const Section, Name: string; var Buffer;
  const Size: integer): boolean;
var
  s: string;
begin
  s := ReadString(Section, Name, '');
  Result := TextToBin(s, Buffer, Size);
end;

function TIniFileEx.ReadMemoryStream(const Section, Name: string;
  MemoryStream: TMemoryStream): boolean;
var
  s: string;
begin
  Result := False;
  s := ReadString(Section, Name, '');
  if s = '' then
    exit;
  MemoryStream.SetSize(Length(s) div 2);
  Result := TextToBin(s, MemoryStream.Memory^, MemoryStream.Size);
  if Result then
    MemoryStream.Position := 0;
end;

end.
