{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uSVGTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, GR32, GR32_Transforms, GR32_Polygons;

const

  INHERIT = -1;
  FontNormal = 0;
  FontItalic = 1;
  MaxTFloat = MaxSingle;

type

  TFRect = record
    Left, Top,
    Width, Height: TFloat;
  end;

  TSingleA = array of single;
  PSingleA = ^TSingleA;

  TRectarray = packed array of TRect;
  PRectArray = ^TRectArray;

  TTextDecoration = set of (tdInherit, tdUnderLine, tdOverLine, tdStrikeOut);
  TTextPathMethod = (tpmAlign, tpmStretch);
  TTextPathSpacing = (tpsAuto, tpsExact);
  TSVGUnit = (suNone, suPX, suPT, suPC, suMM, suCM, suIN, suEM, suEX, suPercent);
  TGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);

  TBounds = record
    TopLeft: TFloatPoint;
    TopRight: TFloatPoint;
    BottomLeft: TFloatPoint;
    BottomRight: TFloatPoint;
  end;

  { TStyle }

  TStyle = class(TObject)
  private
    FValues: TStringList;
    FName: string;

    function GetCount: integer;
    procedure Put(const Key: string; const Value: string);
    function Get(const Key: string): string;

    procedure PutValue(Index: integer; const Value: string);
    function GetValue(Index: integer): string;

    procedure PutKey(Index: integer; const Key: string);
    function GetKey(Index: integer): string;

    function Dequote(const Value: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Clone: TStyle;
    procedure SetValues(const Values: string);

    function AddStyle(const Key, Value: string): integer;
    function IndexOf(const Key: string): integer;
    procedure Delete(Index: integer);
    function Remove(const Key: string): integer;

    property Count: integer read GetCount;
    property Values[const Key: string]: string read Get write Put; default;
    property ValuesByNum[Index: integer]: string read GetValue write PutValue;
    property Keys[Index: integer]: string read GetKey write PutKey;
  end;

  { TStyleList }

  TStyleList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function Get(Index: integer): TStyle;
    procedure Put(Index: integer; Style: TStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Clone: TStyleList;

    procedure Delete(Index: integer);
    function Remove(Style: TStyle): integer;
    function Add(Style: TStyle): integer; overload;
    function Add(const Name, Values: string): integer; overload;
    function Add(const AStyle: string): integer; overload;

    procedure Insert(Index: integer; Style: TStyle); overload;
    procedure Insert(Index: integer; const Name, Values: string); overload;
    procedure Exchange(Index1, Index2: integer);
    procedure Move(CurIndex, NewIndex: integer);
    function IndexOf(Style: TStyle): integer;
    function GetStyle(const Name: string): TStyle;

    property Style[Index: integer]: TStyle read Get write Put; default;
    property Count: integer read GetCount;
  end;

function GetSVGTransformation(M: TFloatMatrix): TAffineTransformation;
function Intersect(const Bounds: TBounds; const Rect: TRect): boolean;

implementation

uses
  LCLType, LCLIntf;

type
  TAffineTransformationAccess = class(TAffineTransformation);

function GetSVGTransformation(M: TFloatMatrix): TAffineTransformation;
begin
  Result := TAffineTransformation.Create;
  with TAffineTransformationAccess(Result) do
  begin
    FMatrix := M;
    Changed;
  end;
end;

function Intersect(const Bounds: TBounds; const Rect: TRect): boolean;
var
  R1, R2: THandle;
  P: array[0..3] of TPoint;
begin
  P[0].X := Round(Bounds.TopLeft.X);
  P[0].Y := Round(Bounds.TopLeft.Y);

  P[1].X := Round(Bounds.TopRight.X);
  P[1].Y := Round(Bounds.TopRight.Y);

  P[2].X := Round(Bounds.BottomRight.X);
  P[2].Y := Round(Bounds.BottomRight.Y);

  P[3].X := Round(Bounds.BottomLeft.X);
  P[3].Y := Round(Bounds.BottomLeft.Y);

  R1 := CreatePolygonRgn(P, 4, ALTERNATE);
  R2 := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);

  Result := CombineRgn(R1, R1, R2, RGN_AND) <> NULLREGION;

  DeleteObject(R1);
  DeleteObject(R2);
end;


{ TStyle }

constructor TStyle.Create;
begin
  inherited;
  FValues := TStringList.Create;
  FValues.NameValueSeparator := '"';
end;

destructor TStyle.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TStyle.Clear;
begin
  if FValues <> nil then
    FValues.Clear;
end;

function TStyle.Clone: TStyle;
begin
  Result := TStyle.Create;
  Result.FName := FName;
  Result.FValues.Assign(FValues);
end;

function TStyle.GetCount: integer;
begin
  Result := FValues.Count;
end;

procedure TStyle.Put(const Key: string; const Value: string);
var
  Index: integer;
begin
  Index := IndexOf(Key);
  if Index > 0 then
    PutValue(Index, Value)
  else
    AddStyle(Key, Value);
end;

function TStyle.Get(const Key: string): string;
begin
  Result := GetValue(IndexOf(Key));
end;

procedure TStyle.PutValue(Index: integer; const Value: string);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues.ValueFromIndex[Index] := DeQuote(Value);
end;

function TStyle.GetValue(Index: integer): string;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues.ValueFromIndex[Index]
  else
    Result := '';
end;

procedure TStyle.PutKey(Index: integer; const Key: string);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues[Index] := Key + FValues.NameValueSeparator + FValues.ValueFromIndex[Index];
end;

function TStyle.GetKey(Index: integer): string;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues.Names[Index]
  else
    Result := '';
end;

function TStyle.Dequote(const Value: string): string;
begin
  if Value <> '' then
  begin
    if (Value[1] = '''') and (Value[Length(Value)] = '''') then
      Result := Copy(Value, 2, Length(Value) - 2)
    else
    if (Value[1] = '"') and (Value[Length(Value)] = '"') then
      Result := Copy(Value, 2, Length(Value) - 2)
    else
      Result := Value;
  end
  else
    Result := Value;
end;

procedure TStyle.SetValues(const Values: string);
var
  C: integer;
  Key: string;
  Value: string;
  Help: string;
begin
  Help := Trim(Values);

  while Help <> '' do
  begin
    C := Pos(';', Help);
    if C = 0 then
      C := Length(Help) + 1;
    Key := Copy(Help, 1, C - 1);
    Help := Trim(Copy(Help, C + 1, MaxInt));
    C := Pos(':', Key);
    if C <> 0 then
    begin
      Value := Trim(Copy(Key, C + 1, MaxInt));
      Key := Trim(Copy(Key, 1, C - 1));

      C := IndexOf(Key);
      if C = -1 then
        FValues.Add(Key + FValues.NameValueSeparator + DeQuote(Value))
      else
        PutValue(C, Value);
    end;
  end;
end;

function TStyle.AddStyle(const Key, Value: string): integer;
begin
  Result := IndexOf(Key);
  if Result = -1 then
    Result := FValues.Add(Key + FValues.NameValueSeparator + DeQuote(Value))
  else
    PutValue(Result, Value);
end;

function TStyle.IndexOf(const Key: string): integer;
begin
  for Result := 0 to FValues.Count - 1 do
    if FValues.Names[Result] = Key then
      Exit;
  Result := -1;
end;

procedure TStyle.Delete(Index: integer);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues.Delete(Index);
end;

function TStyle.Remove(const Key: string): integer;
begin
  Result := IndexOf(Key);
  Delete(Result);
end;


{ TStyleList }

constructor TStyleList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TStyleList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TStyleList.Clear;
begin
  while FList.Count > 0 do
  begin
    TStyle(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TStyleList.Clone: TStyleList;
var
  C: integer;
begin
  Result := TStyleList.Create;
  for C := 0 to FList.Count - 1 do
    Result.Add(Get(C).Clone);
end;

function TStyleList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TStyleList.Get(Index: integer): TStyle;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := TStyle(FList[Index])
  else
    Result := nil;
end;

procedure TStyleList.Put(Index: integer; Style: TStyle);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    try
      TStyle(FList[Index]).Free;
    except
    end;
    FList[Index] := Style;
  end;
end;

procedure TStyleList.Delete(Index: integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    TStyle(FList[Index]).Free;
    FList.Delete(Index);
  end;
end;

function TStyleList.Remove(Style: TStyle): integer;
begin
  Result := IndexOf(Style);
  Delete(Result);
end;

function TStyleList.Add(Style: TStyle): integer;
begin
  Result := FList.Add(Style);
end;

function TStyleList.Add(const Name, Values: string): integer;
var
  S: TStyle;
begin
  S := TStyle.Create;
  S.FName := Name;
  S.SetValues(Values);
  Result := Add(S);
end;

function TStyleList.Add(const AStyle: string): integer;
var
  Name: string;
  LStyle: string;
  Values: string;
  C, D: integer;
begin
  Result := -1;
  LStyle := Trim(AStyle);
  for C := 1 to Length(LStyle) do
    if LStyle[C] = '{' then
    begin
      for D := Length(LStyle) downto C + 1 do
        if LStyle[D] = '}' then
        begin
          Name := Trim(Copy(LStyle, 1, C - 1));

          Values := Copy(LStyle, C + 1, D - C - 1);
          Result := Add(Name, Values);
        end;
    end;
end;

procedure TStyleList.Insert(Index: integer; Style: TStyle);
begin
  if (Index >= 0) and (Index < FList.Count) then
    FList.Insert(Index, Style);
end;

procedure TStyleList.Insert(Index: integer; const Name, Values: string);
var
  S: TStyle;
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    S := TStyle.Create;
    S.FName := Name;
    S.SetValues(Values);
    Insert(Index, S);
  end;
end;

procedure TStyleList.Exchange(Index1, Index2: integer);
begin
  if (Index1 >= 0) and (Index1 < FList.Count) and (Index2 >= 0) and
    (Index2 < FList.Count) then
    FList.Exchange(Index1, Index2);
end;

procedure TStyleList.Move(CurIndex, NewIndex: integer);
begin
  if (CurIndex >= 0) and (CurIndex < FList.Count) and (NewIndex >= 0) and
    (NewIndex < FList.Count) then
    FList.Move(CurIndex, NewIndex);
end;

function TStyleList.IndexOf(Style: TStyle): integer;
begin
  Result := FList.IndexOf(Style);
end;

function TStyleList.GetStyle(const Name: string): TStyle;
var
  C: integer;
begin
  for C := 0 to FList.Count - 1 do
  begin
    Result := TStyle(FList[C]);
    if Result.FName = Name then
      Exit;
  end;

  Result := nil;
end;

end.
