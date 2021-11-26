{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uSVGProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GR32, GR32_Transforms, DOM, uSVGTypes;

type
  TColorRef = record
    Name: string;
    Color: TColor;
  end;

var
  Colors: array [0..144] of TColorRef;

function GetColor(const S: string): TColor;
function ConvertColor(Color: TColor; Alpha: byte): TColor32;
function StrToTFloat(const S: string): TFloat;
function TryStrToTFloat(const S: string; out Value: TFloat): boolean;
function ParseAngle(const Angle: string): TFloat;
function ParsePercent(const S: string): TFloat;
function ParseInteger(const S: string): integer;
function ParseLength(const S: string): TFloat;
function ParseUnit(const S: string): TSVGUnit;
function ParseDRect(const S: string; out WidthUnits, HeightUnits: TSVGUnit): TFRect;
function ParseURI(const URI: string): string;
function ParseTransform(const ATransform: string): TFloatMatrix;
function GetFactor(const SVGUnit: TSVGUnit): TFloat;
function GetAttrValue(ANode: TDOMNode; AAttrName: string): string;
procedure LoadLength(const Node: TDOMNode; const S: string; var X: TFloat);
procedure LoadTFloat(const Node: TDOMNode; const S: string; var X: TFloat);
function LoadString(const Node: TDOMNode; const S: string): string; overload;
procedure LoadString(const Node: TDOMNode; const S: string; var X: string); overload;
procedure LoadTransform(const Node: TDOMNode; const S: string;
  var Matrix: TFloatMatrix);
procedure LoadPercent(const Node: TDOMNode; const S: string; var X: TFloat); overload;
procedure LoadPercent(const Node: TDOMNode; const S: string; Max: integer;
  var X: TFloat); overload;
procedure LoadBytePercent(const Node: TDOMNode; const S: string; var X: integer);
procedure LoadBoolean(const Node: TDOMNode; const S: string; var X: boolean);
procedure LoadDisplay(const Node: TDOMNode; var X: integer);
procedure LoadVisible(const Node: TDOMNode; var X: integer);
procedure LoadGradientUnits(const Node: TDOMNode; var Units: TGradientUnits);

implementation

uses
  Math;

function TryStrToTFloat(const S: string; out Value: TFloat): boolean;
var
  S1: string;
begin
  S1 := StringReplace(S, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  S1 := StringReplace(S1, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(S1, Value);
  if not Result then
    Value := 0;
end;

function StrToTFloat(const S: string): TFloat;
begin
  TryStrToTFloat(S, Result);
end;


function IsHex(const S: string): boolean;
var
  C: integer;
  Help: string;
begin
  Result := False;
  if S[1] = '#' then
    Help := Copy(S, 2, Length(S))
  else
    Help := S;
  for C := 1 to Length(Help) do
    if not (((Help[C] >= '0') and (Help[C] <= '9')) or
      ((Help[C] >= 'A') and (Help[C] <= 'F')) or
      ((Help[C] >= 'a') and (Help[C] <= 'f'))) then
      Exit;

  Result := True;
end;

function IsDecimal(const S: string): boolean;
var
  C: integer;
begin
  Result := False;
  for C := 1 to Length(S) do
    if not ((S[C] >= '0') and (S[C] <= '9')) then
      Exit;
  Result := True;
end;

function DecodeToInt(const S: string): integer;
var
  C: integer;
  Percent: boolean;
  Help: string;
begin
  Result := -1;
  Help := '0' + S;
  Percent := False;
  if Help[Length(Help)] = '%' then
  begin
    Help := Copy(Help, 1, Length(Help) - 1);
    Percent := True;
  end;

  C := -1;
  if IsDecimal(Help) then
    C := StrToInt(Help)
  else
  if IsHex(Help) then
    C := StrToInt('$' + Help);
  if C = -1 then
    Exit;
  if C > 255 then
    C := 255;
  if Percent then
  begin
    if C > 100 then
      C := 100;
    C := Round(C * 2.55);
  end;
  Result := C;
end;

function CharToInt(const Ch: char): integer;
begin
  Result := Ord(Ch);
  if (Result > 47) and (Result < 58) then
    Dec(Result, 48)
  else
  if (Result > 64) and (Result < 71) then
    Dec(Result, 55)
  else
  if (Result > 96) and (Result < 103) then
    Dec(Result, 87)
  else
    Result := 0;
end;

function PrepareHex(const S: string): string;
var
  C: integer;
  Help: string;
begin
  if S[1] = '#' then
    Help := Copy(S, 2, Length(S))
  else
    Help := S;
  if Length(Help) > 6 then
    Help := Copy(Help, 1, 6);

  if Length(Help) = 3 then
  begin
    Help := IntToHex(CharToInt(Help[1]) * 17, 2) +
      IntToHex(CharToInt(Help[2]) * 17, 2) + IntToHex(CharToInt(Help[3]) * 17, 2);
  end;

  Result := '$';

  for C := 0 to 2 do
    Result := Result + Copy(Help, 5 - C * 2, 2);
end;

function RGB(r, g, b: byte): integer;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;

function DecodeRGB(const S: string): integer;
var
  RS, GS, BS: string;
  sRGB: string;
  R, B, G: integer;
begin
  Result := -1;
  if not ((Copy(S, 1, 4) = 'sRGB(') and (S[Length(S)] = ')')) then
    Exit;

  sRGB := Copy(S, 5, Length(S) - 5);
  sRGB := Trim(sRGB);

  RS := Copy(sRGB, 1, Pos(',', sRGB) - 1);
  sRGB := Copy(sRGB, Pos(',', sRGB) + 1, Length(sRGB));
  sRGB := Trim(sRGB);

  GS := Copy(sRGB, 1, Pos(',', sRGB) - 1);
  sRGB := Copy(sRGB, Pos(',', sRGB) + 1, Length(sRGB));
  sRGB := Trim(sRGB);

  BS := sRGB;

  R := DecodeToInt(RS);
  G := DecodeToInt(GS);
  B := DecodeToInt(BS);

  if (R = -1) or (G = -1) or (B = -1) then
    Exit;

  Result := RGB(R, G, B);
end;

function ConvertColor(Color: TColor; Alpha: byte): TColor32;
var
  R, G, B: byte;
begin
  R := (Color and $000000FF);
  G := (Color and $0000FF00) shr 8;
  B := (Color and $00FF0000) shr 16;
  Result := Color32(R, G, B, Alpha);
end;


function GetColor(const S: string): TColor;
var
  C: integer;
  Color: string;
begin
  if S = '' then
  begin
    Result := -1;
    Exit;
  end;
  for C := 0 to 144 do
    if S = Colors[C].Name then
    begin
      Result := Colors[C].Color;
      Exit;
    end;

  if IsHex(S) and (not IsDecimal(S)) then
    Color := PrepareHex(S)
  else
    Color := S;

  if TryStrToInt(Color, C) then
  begin
    Result := C;
  end
  else
  begin
    Result := DecodeRGB(Color);
  end;
end;

function ParseAngle(const Angle: string): TFloat;
var
  D: TFloat;
  C: integer;
  S: string;
begin
{$IFDEF PRESERVE_RAD}
  if Angle <> '' then
  begin
    S := Angle;
    C := Pos('deg', S);
    if C <> 0 then
    begin
      S := Copy(S, 1, C - 1);
      if TryStrToTFloat(S, D) then
        Result := DegToRad(D)
      else
        Result := 0;
      Exit;
    end;

    C := Pos('rad', S);
    if C <> 0 then
    begin
      TryStrToTFloat(S, Result);
      Exit;
    end;

    C := Pos('grad', S);
    if C <> 0 then
    begin
      S := Copy(S, 1, C - 1);
      if TryStrToTFloat(S, D) then
        Result := GradToRad(D)
      else
        Result := 0;
      Exit;
    end;

    if TryStrToTFloat(S, D) then
      Result := DegToRad(D)
    else
      Result := 0;
  end
  else
    Result := 0;

{$ELSE}
  //GR32 GOES HERE, using degree instead of rad

  if Angle <> '' then
  begin
    S := Angle;
    C := Pos('deg', S);
    if C <> 0 then
    begin
      S := Copy(S, 1, C - 1);
      if TryStrToTFloat(S, D) then
        Result := D
      else
        Result := 0;
      Exit;
    end;

    C := Pos('rad', S);
    if C <> 0 then
    begin
      TryStrToTFloat(S, D);
      Result := RadToDeg(D);
      Exit;
    end;

    C := Pos('grad', S);
    if C <> 0 then
    begin
      S := Copy(S, 1, C - 1);
      if TryStrToTFloat(S, D) then
        Result := GradToDeg(D)
      else
        Result := 0;
      Exit;
    end;

    if TryStrToTFloat(S, D) then
      Result := D
    else
      Result := 0;
  end
  else
    Result := 0;

  Result := Result * -1;
{$ENDIF}
end;

function ParseByte(const S: string): byte;
begin
  Result := ParseInteger(S);
end;

function ParsePercent(const S: string): TFloat;
begin
  Result := -1;
  if S = '' then
    Exit;

  if S[Length(S)] = '%' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 1)) / 100
  else
    Result := StrToTFloat(S);
end;

function ParseInteger(const S: string): integer;
begin
  Result := StrToInt(S);
end;

function ParseLength(const S: string): TFloat;
var
  U: string;
  SVGUnit: TSVGUnit;
  Factor: TFloat;
begin
  SVGUnit := ParseUnit(S);
  if SVGUnit = suPercent then
    U := Copy(S, Length(S), 1)
  else
  if SVGUnit <> suNone then
    U := Copy(S, Length(S) - 1, 2);

  Factor := GetFactor(SVGUnit);
  if U = 'px' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 2))
  else
  if U = 'pt' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 2)) * Factor
  else
  if U = 'pc' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 2)) * Factor
  else
  if U = 'mm' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 2)) * Factor
  else
  if U = 'cm' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 2)) * Factor
  else
  if U = 'in' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 2)) * Factor
  else
  if U = '%' then
    Result := StrToTFloat(Copy(S, 1, Length(S) - 1)) * Factor
  else
    Result := StrToTFloat(S);
end;

function ParseUnit(const S: string): TSVGUnit;
begin
  Result := suNone;

  if Copy(S, Length(S) - 1, 2) = 'px' then
  begin
    Result := suPx;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'pt' then
  begin
    Result := suPt;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'pc' then
  begin
    Result := suPC;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'mm' then
  begin
    Result := suMM;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'cm' then
  begin
    Result := suCM;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'in' then
  begin
    Result := suIN;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'em' then
  begin
    Result := suEM;
    Exit;
  end;

  if Copy(S, Length(S) - 1, 2) = 'ex' then
  begin
    Result := suEX;
    Exit;
  end;

  if Copy(S, Length(S), 1) = '%' then
  begin
    Result := suPercent;
    Exit;
  end;
end;

function GetFactor(const SVGUnit: TSVGUnit): TFloat;
begin
  case SVGUnit of
    suPX: Result := 1;
    suPT: Result := 1.25;
    suPC: Result := 15;
    suMM: Result := 10;
    suCM: Result := 100;
    suIN: Result := 25.4;
    suEM: Result := 1;
    suEX: Result := 1;
    suPercent: Result := 1;
    else
      Result := 1;
  end;
end;

function GetValues(const S: string; Delimiter: char): TStringList;
var
  C: integer;
begin
  Result := TStringList.Create;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := S;

  for C := Result.Count - 1 downto 0 do
    if Result[C] = '' then
      Result.Delete(C);
end;

function ParseDRect(const S: string; out WidthUnits, HeightUnits: TSVGUnit
  ): TFRect;
var
  SL: TStringList;

begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(Trim(S), ' ');

  if SL.Count = 4 then
  begin
    Result.Left := ParseLength(SL[0]);
    Result.Top := ParseLength(SL[1]);
    Result.Width := ParseLength(SL[2]);
    Result.Height := ParseLength(SL[3]);
    WidthUnits := ParseUnit(SL[2]);
    HeightUnits := ParseUnit(SL[3]);
  end;

  SL.Free;
end;

function ParseURI(const URI: string): string;
var
  S: string;
begin
  Result := '';
  if URI = '' then
    Exit;
  S := Trim(URI);
  if (Copy(S, 1, 5) = 'url(#') and (S[Length(S)] = ')') then
    Result := Copy(S, 6, Length(S) - 6);
end;

function GetMatrix(const S: string): TFloatMatrix;
var
  SL: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  if SL.Count = 6 then
  begin
    Result[0, 0] := StrToTFloat(SL[0]);
    Result[0, 1] := StrToTFloat(SL[1]);
    Result[1, 0] := StrToTFloat(SL[2]);
    Result[1, 1] := StrToTFloat(SL[3]);
    Result[2, 0] := StrToTFloat(SL[4]);
    Result[2, 1] := StrToTFloat(SL[5]);
    Result[2, 2] := 1;
  end;
  SL.Free;
end;

function GetTranslate(const S: string): TFloatMatrix;
var
  SL: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);
  SL := GetValues(S, ',');
  if SL.Count = 1 then
    SL.Add('0');

  if SL.Count = 2 then
  begin
    Result[0, 0] := 1;
    Result[0, 1] := 0;
    Result[1, 0] := 0;
    Result[1, 1] := 1;
    Result[2, 0] := StrToTFloat(SL[0]);
    Result[2, 1] := StrToTFloat(SL[1]);
    Result[2, 2] := 1;
  end;
  SL.Free;
end;

function GetScale(const S: string): TFloatMatrix;
var
  SL: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);
  SL := GetValues(S, ',');
  if SL.Count = 1 then
    SL.Add(SL[0]);
  if SL.Count = 2 then
  begin
    Result[0, 0] := StrToTFloat(SL[0]);
    Result[0, 1] := 0;
    Result[1, 0] := 0;
    Result[1, 1] := StrToTFloat(SL[1]);
    Result[2, 0] := 0;
    Result[2, 1] := 0;
    Result[2, 2] := 1;
  end;
  SL.Free;
end;

function GetRotation(const S: string): TFloatMatrix;
var
  SL: TStringList;
  X, Y, Angle: TFloat;
  TA: TAffineTransformation;
begin
  SL := GetValues(S, ',');

  Angle := ParseAngle(SL[0]);

  if SL.Count = 3 then
  begin
    X := StrToTFloat(SL[1]);
    Y := StrToTFloat(SL[2]);
  end
  else
  begin
    X := 0;
    Y := 0;
  end;
  SL.Free;
  {$IFDEF GPPen}
  Result := CalcRotationMatrix(X, Y, Angle);
  {$ENDIF}
  TA := TAffineTransformation.Create;
  TA.Rotate(X, Y, Angle);
  Result := TA.Matrix;
  TA.Free;
end;

function GetSkewX(const S: string): TFloatMatrix;
var
  SL: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  if SL.Count = 1 then
  begin
    Result[0, 0] := 1;
    Result[0, 1] := 0;
    Result[1, 0] := Tan(StrToTFloat(SL[0]));
    Result[1, 1] := 1;
    Result[2, 0] := 0;
    Result[2, 1] := 0;
    Result[2, 2] := 1;
  end;
  SL.Free;
end;

function GetSkewY(const S: string): TFloatMatrix;
var
  SL: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  if SL.Count = 1 then
  begin
    Result[0, 0] := 1;
    Result[0, 1] := Tan(StrToTFloat(SL[0]));
    Result[1, 0] := 0;
    Result[1, 1] := 1;
    Result[2, 0] := 0;
    Result[2, 1] := 0;
    Result[2, 2] := 1;
  end;
  SL.Free;
end;

function ParseTransform(const ATransform: string): TFloatMatrix;
var
  Start, Stop: integer;
  TType: string;
  Values: string;
  S: string;
  M: TFloatMatrix;
begin
  FillChar(Result, SizeOf(Result), 0);

  S := Trim(ATransform);

  while S <> '' do
  begin
    Start := Pos('(', S);
    Stop := Pos(')', S);
    if (Start = 0) or (Stop = 0) then
      Exit;
    TType := Copy(S, 1, Start - 1);
    Values := Trim(Copy(S, Start + 1, Stop - Start - 1));
    Values := StringReplace(Values, ' ', ',', [rfReplaceAll]);
    M[2, 2] := 0;

    if TType = 'matrix' then
      M := GetMatrix(Values);

    if TType = 'translate' then
      M := GetTranslate(Values);

    if TType = 'scale' then
      M := GetScale(Values);

    if TType = 'rotate' then
      M := GetRotation(Values);

    if TType = 'skewX' then
      M := GetSkewX(Values);

    if TType = 'skewY' then
      M := GetSkewY(Values);

    if M[2, 2] = 1 then
    begin
      if Result[2, 2] = 0 then
        Result := M
      else
        Result := Mult(Result, M);
    end;

    S := Trim(Copy(S, Stop + 1, Length(S)));
  end;
end;

procedure DecodeBase64(const S: ansistring; St: TStream);
const
  Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

var
  C: integer;
  BitCount: integer;
  B: integer;
  X: integer;
  By: byte;
begin
  BitCount := 0;
  B := 0;
  St.Position := 0;
  St.Size := 0;

  for C := 1 to Length(S) do
  begin
    X := Pos(S[C], Codes64) - 1;
    if X >= 0 then
    begin
      B := B * 64 + X;
      BitCount := BitCount + 6;
      if BitCount >= 8 then
      begin
        BitCount := BitCount - 8;
        By := (B shr BitCount) mod 256;
        B := B mod (1 shl BitCount);
        St.Write(By, 1);
      end;
    end;
  end;
end;

function GetAttrValue(ANode: TDOMNode; AAttrName: string): string;
var
  i: longword;
  Found: boolean;
begin
  Result := '';
  if (ANode = nil) or (ANode.Attributes = nil) then
    exit;

  Found := False;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do
  begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then
    begin
      Found := True;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    Inc(i);
  end;
end;

procedure LoadLength(const Node: TDOMNode; const S: string; var X: TFloat);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := ParseLength(Attribute.nodeValue);
end;

procedure LoadTFloat(const Node: TDOMNode; const S: string; var X: TFloat);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := StrToTFloat(Attribute.nodeValue);
end;

function LoadString(const Node: TDOMNode; const S: string): string;
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    Result := Attribute.TextContent
  else
    Result := '';
end;

procedure LoadString(const Node: TDOMNode; const S: string; var X: string);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := Attribute.TextContent;
end;

procedure LoadTransform(const Node: TDOMNode; const S: string;
  var Matrix: TFloatMatrix);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    Matrix := ParseTransform(Attribute.nodeValue);
end;

procedure LoadPercent(const Node: TDOMNode; const S: string; var X: TFloat);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := ParsePercent(Attribute.nodeValue);
end;

procedure LoadPercent(const Node: TDOMNode; const S: string; Max: integer;
  var X: TFloat);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := Max * ParsePercent(Attribute.nodeValue);
end;

procedure LoadBytePercent(const Node: TDOMNode; const S: string; var X: integer);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := Round(255 * ParsePercent(Attribute.nodeValue));
end;

procedure LoadBoolean(const Node: TDOMNode; const S: string; var X: boolean);
var
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem(S);
  if Assigned(Attribute) then
    X := boolean(ParseInteger(Attribute.nodeValue));
end;

procedure LoadDisplay(const Node: TDOMNode; var X: integer);
var
  S: string;
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem('display');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'inherit' then
      X := -1
    else
    if S = 'none' then
      X := 0
    else
      X := 1;
  end;
end;

procedure LoadVisible(const Node: TDOMNode; var X: integer);
var
  S: string;
  Attribute: TDOMNode;
begin
  Attribute := Node.Attributes.GetNamedItem('visibility');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'inherit' then
      X := -1
    else
    if S = 'visible' then
      X := 1
    else
      X := 0;
  end;
end;

procedure LoadGradientUnits(const Node: TDOMNode; var Units: TGradientUnits);
var
  S: string;
  Attribute: TDOMNode;
begin
  Units := guObjectBoundingBox;
  Attribute := Node.Attributes.GetNamedItem('gradientUnits');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'userSpaceOnUse' then
      Units := guUserSpaceOnUse
    else
    if S = 'objectBoundingBox' then
      Units := guObjectBoundingBox;
  end;
end;

initialization

  Colors[000].Name := 'none';
  Colors[000].Color := -2;

  Colors[001].Name := 'aliceblue';
  Colors[001].Color := rgb(240, 248, 255);

  Colors[002].Name := 'antiquewhite';
  Colors[002].Color := rgb(250, 235, 215);

  Colors[003].Name := 'aqua';
  Colors[003].Color := rgb(0, 255, 255);

  Colors[004].Name := 'aquamarine';
  Colors[004].Color := rgb(127, 255, 212);

  Colors[005].Name := 'azure';
  Colors[005].Color := rgb(240, 255, 255);

  Colors[006].Name := 'beige';
  Colors[006].Color := rgb(245, 245, 220);

  Colors[007].Name := 'bisque';
  Colors[007].Color := rgb(255, 228, 196);

  Colors[008].Name := 'black';
  Colors[008].Color := rgb(0, 0, 0);

  Colors[009].Name := 'blanchedalmond';
  Colors[009].Color := rgb(255, 235, 205);

  Colors[010].Name := 'blue';
  Colors[010].Color := rgb(0, 0, 255);

  Colors[011].Name := 'blueviolet';
  Colors[011].Color := rgb(138, 43, 226);

  Colors[012].Name := 'brown';
  Colors[012].Color := rgb(165, 42, 42);

  Colors[013].Name := 'burlywood';
  Colors[013].Color := rgb(222, 184, 135);

  Colors[014].Name := 'cadetblue';
  Colors[014].Color := rgb(95, 158, 160);

  Colors[015].Name := 'chartreuse';
  Colors[015].Color := rgb(127, 255, 0);

  Colors[016].Name := 'chocolate';
  Colors[016].Color := rgb(210, 105, 30);

  Colors[017].Name := 'coral';
  Colors[017].Color := rgb(255, 127, 80);

  Colors[018].Name := 'cornflowerblue';
  Colors[018].Color := rgb(100, 149, 237);

  Colors[019].Name := 'cornsilk';
  Colors[019].Color := rgb(255, 248, 220);

  Colors[020].Name := 'crimson';
  Colors[020].Color := rgb(220, 20, 60);

  Colors[021].Name := 'cyan';
  Colors[021].Color := rgb(0, 255, 255);

  Colors[022].Name := 'darkblue';
  Colors[022].Color := rgb(0, 0, 139);

  Colors[023].Name := 'darkcyan';
  Colors[023].Color := rgb(0, 139, 139);

  Colors[024].Name := 'darkgoldenrod';
  Colors[024].Color := rgb(184, 134, 11);

  Colors[025].Name := 'darkgray';
  Colors[025].Color := rgb(169, 169, 169);

  Colors[026].Name := 'darkgreen';
  Colors[026].Color := rgb(0, 100, 0);

  Colors[027].Name := 'darkgrey';
  Colors[027].Color := rgb(169, 169, 169);

  Colors[028].Name := 'darkkhaki';
  Colors[028].Color := rgb(189, 183, 107);

  Colors[029].Name := 'darkmagenta';
  Colors[029].Color := rgb(139, 0, 139);

  Colors[030].Name := 'darkolivegreen';
  Colors[030].Color := rgb(85, 107, 47);

  Colors[031].Name := 'darkorange';
  Colors[031].Color := rgb(255, 140, 0);

  Colors[032].Name := 'darkorchid';
  Colors[032].Color := rgb(153, 50, 204);

  Colors[033].Name := 'darkred';
  Colors[033].Color := rgb(139, 0, 0);

  Colors[034].Name := 'darksalmon';
  Colors[034].Color := rgb(233, 150, 122);

  Colors[035].Name := 'darkseagreen';
  Colors[035].Color := rgb(143, 188, 143);

  Colors[036].Name := 'darkslateblue';
  Colors[036].Color := rgb(72, 61, 139);

  Colors[037].Name := 'darkslategray';
  Colors[037].Color := rgb(47, 79, 79);

  Colors[038].Name := 'darkslategrey';
  Colors[038].Color := rgb(47, 79, 79);

  Colors[039].Name := 'darkturquoise';
  Colors[039].Color := rgb(0, 206, 209);

  Colors[040].Name := 'darkviolet';
  Colors[040].Color := rgb(148, 0, 211);

  Colors[041].Name := 'deeppink';
  Colors[041].Color := rgb(255, 20, 147);

  Colors[042].Name := 'deepskyblue';
  Colors[042].Color := rgb(0, 191, 255);

  Colors[043].Name := 'dimgray';
  Colors[043].Color := rgb(105, 105, 105);

  Colors[044].Name := 'dimgrey';
  Colors[044].Color := rgb(105, 105, 105);

  Colors[045].Name := 'dodgerblue';
  Colors[045].Color := rgb(30, 144, 255);

  Colors[046].Name := 'firebrick';
  Colors[046].Color := rgb(178, 34, 34);

  Colors[047].Name := 'floralwhite';
  Colors[047].Color := rgb(255, 250, 240);

  Colors[048].Name := 'forestgreen';
  Colors[048].Color := rgb(34, 139, 34);

  Colors[049].Name := 'fuchsia';
  Colors[049].Color := rgb(255, 0, 255);

  Colors[050].Name := 'gainsboro';
  Colors[050].Color := rgb(220, 220, 220);

  Colors[051].Name := 'ghostwhite';
  Colors[051].Color := rgb(248, 248, 255);

  Colors[052].Name := 'gold';
  Colors[052].Color := rgb(255, 215, 0);

  Colors[053].Name := 'goldenrod';
  Colors[053].Color := rgb(218, 165, 32);

  Colors[054].Name := 'gray';
  Colors[054].Color := rgb(128, 128, 128);

  Colors[055].Name := 'grey';
  Colors[055].Color := rgb(128, 128, 128);

  Colors[056].Name := 'green';
  Colors[056].Color := rgb(0, 128, 0);

  Colors[057].Name := 'greenyellow';
  Colors[057].Color := rgb(173, 255, 47);

  Colors[058].Name := 'honeydew';
  Colors[058].Color := rgb(240, 255, 240);

  Colors[059].Name := 'hotpink';
  Colors[059].Color := rgb(255, 105, 180);

  Colors[060].Name := 'indianred';
  Colors[060].Color := rgb(205, 92, 92);

  Colors[061].Name := 'indigo';
  Colors[061].Color := rgb(75, 0, 130);

  Colors[062].Name := 'ivory';
  Colors[062].Color := rgb(255, 255, 240);

  Colors[063].Name := 'khaki';
  Colors[063].Color := rgb(240, 230, 140);

  Colors[064].Name := 'lavender';
  Colors[064].Color := rgb(230, 230, 250);

  Colors[065].Name := 'lavenderblush';
  Colors[065].Color := rgb(255, 240, 245);

  Colors[066].Name := 'lawngreen';
  Colors[066].Color := rgb(124, 252, 0);

  Colors[067].Name := 'lemonchiffon';
  Colors[067].Color := rgb(255, 250, 205);

  Colors[068].Name := 'lightblue';
  Colors[068].Color := rgb(173, 216, 230);

  Colors[069].Name := 'lightcoral';
  Colors[069].Color := rgb(240, 128, 128);

  Colors[070].Name := 'lightcyan';
  Colors[070].Color := rgb(224, 255, 255);

  Colors[071].Name := 'lightgoldenrodyellow';
  Colors[071].Color := rgb(250, 250, 210);

  Colors[072].Name := 'lightgray';
  Colors[072].Color := rgb(211, 211, 211);

  Colors[073].Name := 'lightgreen';
  Colors[073].Color := rgb(144, 238, 144);

  Colors[074].Name := 'lightpink';
  Colors[074].Color := rgb(255, 182, 193);

  Colors[075].Name := 'lightsalmon';
  Colors[075].Color := rgb(255, 160, 122);

  Colors[076].Name := 'lightseagreen';
  Colors[076].Color := rgb(32, 178, 170);

  Colors[077].Name := 'lightskyblue';
  Colors[077].Color := rgb(135, 206, 250);

  Colors[078].Name := 'lightslategray';
  Colors[078].Color := rgb(119, 136, 153);

  Colors[079].Name := 'lightslategrey';
  Colors[079].Color := rgb(119, 136, 153);

  Colors[080].Name := 'lightsteelblue';
  Colors[080].Color := rgb(176, 196, 222);

  Colors[081].Name := 'lightyellow';
  Colors[081].Color := rgb(255, 255, 224);

  Colors[082].Name := 'lime';
  Colors[082].Color := rgb(0, 255, 0);

  Colors[083].Name := 'limegreen';
  Colors[083].Color := rgb(50, 205, 50);

  Colors[084].Name := 'linen';
  Colors[084].Color := rgb(250, 240, 230);

  Colors[085].Name := 'magenta';
  Colors[085].Color := rgb(255, 0, 255);

  Colors[086].Name := 'maroon';
  Colors[086].Color := rgb(128, 0, 0);

  Colors[087].Name := 'mediumaquamarine';
  Colors[087].Color := rgb(102, 205, 170);

  Colors[088].Name := 'mediumblue';
  Colors[088].Color := rgb(0, 0, 205);

  Colors[089].Name := 'mediumorchid';
  Colors[089].Color := rgb(186, 85, 211);

  Colors[090].Name := 'mediumpurple';
  Colors[090].Color := rgb(147, 112, 219);

  Colors[091].Name := 'mediumseagreen';
  Colors[091].Color := rgb(60, 179, 113);

  Colors[092].Name := 'mediumslateblue';
  Colors[092].Color := rgb(123, 104, 238);

  Colors[093].Name := 'mediumspringgreen';
  Colors[093].Color := rgb(0, 250, 154);

  Colors[094].Name := 'mediumturquoise';
  Colors[094].Color := rgb(72, 209, 204);

  Colors[095].Name := 'mediumvioletred';
  Colors[095].Color := rgb(199, 21, 133);

  Colors[096].Name := 'midnightblue';
  Colors[096].Color := rgb(25, 25, 112);

  Colors[097].Name := 'mintcream';
  Colors[097].Color := rgb(245, 255, 250);

  Colors[098].Name := 'mistyrose';
  Colors[098].Color := rgb(255, 228, 225);

  Colors[099].Name := 'moccasin';
  Colors[099].Color := rgb(255, 228, 181);

  Colors[100].Name := 'navajowhite';
  Colors[100].Color := rgb(255, 222, 173);

  Colors[101].Name := 'navy';
  Colors[101].Color := rgb(0, 0, 128);

  Colors[102].Name := 'oldlace';
  Colors[102].Color := rgb(253, 245, 230);

  Colors[103].Name := 'olive';
  Colors[103].Color := rgb(128, 128, 0);

  Colors[104].Name := 'olivedrab';
  Colors[104].Color := rgb(107, 142, 35);

  Colors[105].Name := 'orange';
  Colors[105].Color := rgb(255, 165, 0);

  Colors[106].Name := 'orangered';
  Colors[106].Color := rgb(255, 69, 0);

  Colors[107].Name := 'orchid';
  Colors[107].Color := rgb(218, 112, 214);

  Colors[108].Name := 'palegoldenrod';
  Colors[108].Color := rgb(238, 232, 170);

  Colors[109].Name := 'palegreen';
  Colors[109].Color := rgb(152, 251, 152);

  Colors[110].Name := 'paleturquoise';
  Colors[110].Color := rgb(175, 238, 238);

  Colors[111].Name := 'palevioletred';
  Colors[111].Color := rgb(219, 112, 147);

  Colors[112].Name := 'papayawhip';
  Colors[112].Color := rgb(255, 239, 213);

  Colors[113].Name := 'peachpuff';
  Colors[113].Color := rgb(255, 218, 185);

  Colors[114].Name := 'peru';
  Colors[114].Color := rgb(205, 133, 63);

  Colors[115].Name := 'pink';
  Colors[115].Color := rgb(255, 192, 203);

  Colors[116].Name := 'plum';
  Colors[116].Color := rgb(221, 160, 221);

  Colors[117].Name := 'powderblue';
  Colors[117].Color := rgb(176, 224, 230);

  Colors[118].Name := 'purple';
  Colors[118].Color := rgb(128, 0, 128);

  Colors[119].Name := 'red';
  Colors[119].Color := rgb(255, 0, 0);

  Colors[120].Name := 'rosybrown';
  Colors[120].Color := rgb(188, 143, 143);

  Colors[121].Name := 'royalblue';
  Colors[121].Color := rgb(65, 105, 225);

  Colors[122].Name := 'saddlebrown';
  Colors[122].Color := rgb(139, 69, 19);

  Colors[123].Name := 'salmon';
  Colors[123].Color := rgb(250, 128, 114);

  Colors[124].Name := 'sandybrown';
  Colors[124].Color := rgb(244, 164, 96);

  Colors[125].Name := 'seagreen';
  Colors[125].Color := rgb(46, 139, 87);

  Colors[126].Name := 'seashell';
  Colors[126].Color := rgb(255, 245, 238);

  Colors[127].Name := 'sienna';
  Colors[127].Color := rgb(160, 82, 45);

  Colors[128].Name := 'silver';
  Colors[128].Color := rgb(192, 192, 192);

  Colors[129].Name := 'skyblue';
  Colors[129].Color := rgb(135, 206, 235);

  Colors[130].Name := 'slateblue';
  Colors[130].Color := rgb(106, 90, 205);

  Colors[131].Name := 'slategray';
  Colors[131].Color := rgb(112, 128, 144);

  Colors[132].Name := 'springgreen';
  Colors[132].Color := rgb(0, 255, 127);

  Colors[133].Name := 'steelblue';
  Colors[133].Color := rgb(70, 130, 180);

  Colors[134].Name := 'tan';
  Colors[134].Color := rgb(210, 180, 140);

  Colors[135].Name := 'teal';
  Colors[135].Color := rgb(0, 128, 128);

  Colors[136].Name := 'thistle';
  Colors[136].Color := rgb(216, 191, 216);

  Colors[137].Name := 'tomato';
  Colors[137].Color := rgb(255, 99, 71);

  Colors[138].Name := 'turquoise';
  Colors[138].Color := rgb(64, 224, 208);

  Colors[139].Name := 'violet';
  Colors[139].Color := rgb(238, 130, 238);

  Colors[140].Name := 'wheat';
  Colors[140].Color := rgb(245, 222, 179);

  Colors[141].Name := 'white';
  Colors[141].Color := rgb(255, 255, 255);

  Colors[142].Name := 'whitesmoke';
  Colors[142].Color := rgb(245, 245, 245);

  Colors[143].Name := 'yellow';
  Colors[143].Color := rgb(255, 255, 0);

  Colors[144].Name := 'yellowgreen';
  Colors[144].Color := rgb(154, 205, 50);

end.
