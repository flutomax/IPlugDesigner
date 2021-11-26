{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uSVGObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DOM, GR32, GR32_Paths, GR32_Transforms,
  GR32_Polygons, uSVGTypes;

type

  TSVG = class;

  { TSVGObject }

  TSVGObject = class(TObject)
  private
    fItems: TList;
    fVisible: integer;
    fDisplay: integer;
    fBounds: TBounds;
    fParent: TSVGObject;
    fStyle: TStyle;
    fID: string;
    fObjectName: string;
    fClasses: TStringList;
    function GetCount: integer;
    procedure SetItem(Index: integer; Item: TSVGObject);
    function GetItem(Index: integer): TSVGObject;
    function GetDisplay: integer;
    function GetObjectBounds: TBounds;
    function GetVisible: integer;
  protected
    procedure Assign(SVG: TSVGObject); virtual;
    function New(AParent: TSVGObject): TSVGObject; virtual; abstract;
    procedure CalcObjectBounds; virtual;
    function GetRoot: TSVG;
    procedure ReadChildren(const Node: TDOMNode); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(Parent: TSVGObject); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Clone(Parent: TSVGObject): TSVGObject;
    function Add(Item: TSVGObject): integer;
    procedure Delete(Index: integer);
    function Remove(Item: TSVGObject): integer;
    function IndexOf(Item: TSVGObject): integer;
    function FindByID(const Name: string): TSVGObject;
    function FindByType(Typ: TClass; Previous: TSVGObject = nil): TSVGObject;
    procedure CalculateMatrices;
    procedure PaintToGraphics(Graphics: TBitmap32); virtual; abstract;
    procedure PaintToPath(Path: TFlattenedPath); virtual; abstract;
    procedure ReadIn(const Node: TDOMNode); virtual;
    property Items[Index: integer]: TSVGObject read GetItem write SetItem; default;
    property Count: integer read GetCount;
    property Display: integer read GetDisplay write fDisplay;
    property Visible: integer read GetVisible write fVisible;
    property ObjectBounds: TBounds read GetObjectBounds;
    property Parent: TSVGObject read fParent;
    property Style: TStyle read fStyle;
    property ID: string read fID;
    property ObjectName: string read fObjectName;
  end;

  { TSVGTransformedObject }

  TSVGTransformedObject = class(TSVGObject)
  private
    fMatrix: TFloatMatrix;
    fCompleteCalculatedMatrix: TFloatMatrix;
    fCalculatedMatrix: TFloatMatrix;
    procedure SetMatrix(const Value: TFloatMatrix); virtual;
    procedure CalcMatrix;
    function Transform(P: TFloatPoint): TFloatPoint; overload;
    function Transform(X, Y: TFloat): TFloatPoint; overload;
  protected
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure Clear; override;
    procedure ReadIn(const Node: TDOMNode); override;
    property Matrix: TFloatMatrix read fCompleteCalculatedMatrix;
    property PureMatrix: TFloatMatrix read fMatrix write SetMatrix;
  end;

  { TSVGBase }

  TSVGBase = class(TSVGTransformedObject)
  private
    fFillColor: integer;
    fStrokeColor: integer;
    fFillOpacity: TFloat;
    fStrokeOpacity: TFloat;
    fStrokeWidth: TFloat;
    fStrokeLineJoin: string;
    fStrokeLineCap: string;
    fStrokeMiterLimit: TFloat;
    fStrokeDashOffset: TFloat;
    fStrokeDashArray: TSingleA;
    fStrokeDashArrayCount: integer;
    fArrayNone: boolean;
    fFontName: string;
    fFontSize: TFloat;
    fFontWeight: integer;
    fFontStyle: integer;
    fTextDecoration: TTextDecoration;
    fPath: TFlattenedPath;
    fClipPath: TFlattenedPath;
    fX: TFloat;
    fY: TFloat;
    fWidth: TFloat;
    fHeight: TFloat;
    fWidthAsPixel: TFloat;
    fHeightAsPixel: TFloat;
    fFillRuleEvenOdd: boolean;
    function IsFontAvailable: boolean;
    procedure SetStrokeDashArray(const S: string);
    procedure SetClipURI(const Value: string);
    function GetFillColor: integer;
    function GetStrokeColor: integer;
    function GetFillOpacity: TFloat;
    function GetStrokeOpacity: TFloat;
    function GetStrokeWidth: TFloat;
    function GetClipURI: string;
    function GetStrokeLineCap: TEndStyle;
    function GetStrokeLineJoin: TJoinStyle;
    function GetStrokeMiterLimit: TFloat;
    function GetStrokeDashOffset: TFloat;
    function GetStrokeDashArray(var ACount: integer): PSingle;
    function GetFontName: string;
    function GetFontWeight: integer;
    function GetFontSize: TFloat;
    function GetFontStyle: integer;
    function GetTextDecoration: TTextDecoration;
    procedure ParseFontWeight(const S: string);
  protected
    fRX: TFloat;
    fRY: TFloat;
    fFillURI: string;
    fStrokeURI: string;
    fClipURI: string;
    fLineWidth: TFloat;
    fFillRule: integer;
    fColorInterpolation: TFloat;
    fColorRendering: TFloat;
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ReadStyle(AStyle: TStyle); virtual;
    procedure ConstructPath; virtual;
    function GetClipPath: TFlattenedPath;
    procedure CalcClipPath;
    function GetBrush(AURI: string): TCustomPolygonFiller;
    function GetFillBrush: TCustomPolygonFiller;
    function GetStrokeBrush: TCustomPolygonFiller;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TFlattenedPath); override;
    procedure ReadIn(const Node: TDOMNode); override;
    property Root: TSVG read GetRoot;
    property FillColor: integer read GetFillColor write fFillColor;
    property StrokeColor: integer read GetStrokeColor write fStrokeColor;
    property FillOpacity: TFloat read GetFillOpacity write fFillOpacity;
    property FillRuleEvenOdd: boolean read fFillRuleEvenOdd write fFillRuleEvenOdd;
    property StrokeOpacity: TFloat read GetStrokeOpacity write fStrokeOpacity;
    property StrokeWidth: TFloat read GetStrokeWidth write fStrokeWidth;
    property ClipURI: string read GetClipURI write SetClipURI;
    property FillURI: string read fFillURI write fFillURI;
    property StrokeURI: string read fStrokeURI write fStrokeURI;
    property X: TFloat read fX write fX;
    property Y: TFloat read fY write fY;
    property Width: TFloat read fWidth write fWidth;
    property Height: TFloat read fHeight write fHeight;
    property WidthAsPixel: TFloat read fWidthAsPixel;
    property HeightAsPixel: TFloat read fHeightAsPixel;
    property RX: TFloat read fRX write fRX;
    property RY: TFloat read fRY write fRY;
    property StrokeLineCap: TEndStyle read GetStrokeLineCap;
    property StrokeLineJoin: TJoinStyle read GetStrokeLineJoin;
    property StrokeMiterLimit: TFloat read GetStrokeMiterLimit write fStrokeMiterLimit;
    property StrokeDashOffset: TFloat read GetStrokeDashOffset write fStrokeDashOffset;
    property FontName: string read GetFontName write fFontName;
    property FontSize: TFloat read GetFontSize write fFontSize;
    property FontWeight: integer read GetFontWeight write fFontWeight;
    property FontStyle: integer read GetFontStyle write fFontStyle;
    property TextDecoration: TTextDecoration
      read GetTextDecoration write fTextDecoration;
  end;

  { TSVG }

  TSVG = class(TSVGBase)
  private
    fRootBounds: TFloatRect;
    fDX: TFloat;
    fDY: TFloat;
    fCX: TFloat;
    fCY: TFloat;
    fStyles: TStyleList;
    fInitialMatrix: TFloatMatrix;
    fSource: string;
    fAngle: TFloat;
    fAngleMatrix: TFloatMatrix;
    fRootMatrix: TFloatMatrix;
    fViewBox: TFRect;
    fViewBoxWidthUnits: TSVGUnit;
    fViewBoxHeightUnits: TSVGUnit;
    fFileName: string;
    fSize: TFloatRect;
    fRealWidth: TFloat;
    fRealHeight: TFloat;
    procedure SetViewBox(const Value: TFRect);
    procedure SetSVGOpacity(Opacity: TFloat);
    procedure SetAngle(Angle: TFloat);
    procedure Paint(const Graphics: TBitmap32; Rects: PRectArray;
      RectCount: integer);
    procedure CalcRootMatrix;
    procedure CalcCompleteSize;
    function GetContainerHeight(DestUnit: TSVGUnit): TFloat;
    function GetContainerWidth(DestUnit: TSVGUnit): TFloat;
    function Convert(ASize: TFloat; SourceUnit, DestUnit: TSVGUnit;
      ContainerSize: TFloat): TFloat;
    function ConvertWidth(ax: TFloat; SourceUnit, DestUnit: TSVGUnit;
      ContainerWidth: TFloat): TFloat;
    function ConvertHeight(ay: TFloat; SourceUnit, DestUnit: TSVGUnit;
      ContainerHeight: TFloat): TFloat;
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ReadStyles(const Node: TDOMNode);
    procedure Assign(SVG: TSVGObject); override;
    property RootMatrix: TFloatMatrix read fRootMatrix;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignTo(ADest: TSVG);
    procedure Clear; override;
    procedure ReadIn(const Node: TDOMNode); override;
    procedure DeReferenceUse;
    function GetStyleValue(const Name, Key: string): string;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetBounds(const Bounds: TFloatRect);
    procedure Scale(DX: TFloat; DY: TFloat = -1);
    procedure PaintTo(Graphics: TBitmap32; Bounds: TFloatRect;
      Rects: PRectArray; RectCount: integer); overload;
    procedure PaintTo(Bitmap: TBitmap; Bounds: TFloatRect; Rects: PRectArray;
      RectCount: integer); overload;
    procedure RenderTo(Bitmap: TBitmap32; const AAngle: TFloat = 0);
    property InitialMatrix: TFloatMatrix read fInitialMatrix write fInitialMatrix;
    property SVGOpacity: TFloat write SetSVGOpacity;
    property Source: string read fSource;
    property Angle: TFloat read fAngle write SetAngle;
    property ViewBox: TFRect read fViewBox write SetViewBox;
    property RealWidth: TFloat read fRealWidth;
    property RealHeight: TFloat read fRealHeight;
  end;

  { TSVGContainer }

  TSVGContainer = class(TSVGBase)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGSwitch }

  TSVGSwitch = class(TSVGBase)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGDefs }

  TSVGDefs = class(TSVGBase)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGUse }

  TSVGUse = class(TSVGBase)
  private
    fReference: string;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure Construct;
  public
    procedure PaintToPath(Path: TFlattenedPath); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure Clear; override;
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGRect }

  TSVGRect = class(TSVGBase)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  protected
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGLine }

  TSVGLine = class(TSVGBase)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGPolyLine }

  TSVGPolyLine = class(TSVGBase)
  private
    fPoints: TArrayOfFloatPoint;
    fPointCount: integer;
    function GetPoints: PArrayOfFloatPoint;
    procedure ConstructPoints(const S: string);
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure ReadIn(const Node: TDOMNode); override;
    property Points: PArrayOfFloatPoint read GetPoints;
  end;

  { TSVGPolygon }

  TSVGPolygon = class(TSVGPolyLine)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
  end;

  { TSVGEllipse }

  TSVGEllipse = class(TSVGBase)
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGPath }

  TSVGPath = class(TSVGBase)
  private
    procedure PrepareMoveLineCurveArc(SL: TStringList);
    function SeparateValues(const S: string): TStringList;
    function Split(const S: string): TStringList;
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
  end;

  { TSVGImage }

  TSVGImage = class(TSVGBase)
  private
    fFileName: string;
    fStream: TMemoryStream;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure CalcObjectBounds; override;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure ReadIn(const Node: TDOMNode); override;
    property Data: TMemoryStream read fStream;
  end;

  { TSVGClipPath }

  TSVGClipPath = class(TSVGBase)
  private
    fInClipPath: TFlattenedPath;
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
    procedure ConstructClipPath;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure PaintToPath(Path: TFlattenedPath); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure ReadIn(const Node: TDOMNode); override;
    function GetClipPath: TFlattenedPath;
  end;

  { TSVGSymbol }

  TSVGSymbol = class(TSVGBase)
  private
    fViewBox: TFRect;
    procedure SetViewBox(const Value: TFRect);
  protected
    function New(AParent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: TDOMNode); override;
    property ViewBox: TFRect read fViewBox write SetViewBox;
  end;

implementation

uses
  Math, LCLType, GR32_Clipper, XMLRead, XMLWrite, uSVGProc, uSVGFills, uSVGPath;

const

  InchFactor: array[TSVGUnit] of integer =
    (9600, 9600, 7200, 600, 2540, 254, 100, 0, 0, 0);

{ TSVGObject }

constructor TSVGObject.Create;
begin
  inherited;
  fParent := nil;
  fStyle := TStyle.Create;
  fItems := TList.Create;
  fClasses := TStringList.Create;
  fClasses.Delimiter := ' ';
  Clear;
end;

constructor TSVGObject.Create(Parent: TSVGObject);
begin
  Create;
  if Assigned(Parent) then
    Parent.Add(Self);
end;

destructor TSVGObject.Destroy;
begin
  Clear;
  fItems.Free;

  if Assigned(fParent) then
    try
      fParent.Remove(Self);
    except
    end;

  fStyle.Free;
  fClasses.Free;

  inherited;
end;

procedure TSVGObject.CalcObjectBounds;
begin
end;

procedure TSVGObject.CalculateMatrices;
var
  C: integer;
begin
  if Self is TSVGTransformedObject then
  begin
    if Self is TSVG then
      TSVG(Self).CalcRootMatrix
    else
      TSVGTransformedObject(Self).CalcMatrix;

    if Self is TSVGBase then
      TSVGBase(Self).CalcClipPath;

    CalcObjectBounds;
  end;

  for C := 0 to fItems.Count - 1 do
    TSVGObject(fItems[C]).CalculateMatrices;
end;

procedure TSVGObject.Clear;
begin
  while Count > 0 do
    Items[0].Free;

  Visible := 1;
  Display := 1;
  fID := '';

  fClasses.Clear;
  fStyle.Clear;
  fObjectName := '';
end;

function TSVGObject.Clone(Parent: TSVGObject): TSVGObject;
var
  C: integer;
begin
  Result := New(Parent);
  Result.Assign(Self);

  for C := 0 to fItems.Count - 1 do
    GetItem(C).Clone(Result);
end;

function TSVGObject.Add(Item: TSVGObject): integer;
begin
  Result := fItems.Add(Item);
  Item.fParent := Self;
end;

procedure TSVGObject.Delete(Index: integer);
var
  Item: TSVGBase;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Item := TSVGBase(fItems[Index]);
    fItems.Delete(Index);
    Remove(Item);
  end;
end;

function TSVGObject.Remove(Item: TSVGObject): integer;
begin
  Result := fItems.Remove(Item);
  if Assigned(Item) then
    try
      if Item.fParent = Self then
        Item.fParent := nil;
    except
    end;
end;

function TSVGObject.IndexOf(Item: TSVGObject): integer;
begin
  Result := fItems.IndexOf(Item);
end;

function TSVGObject.FindByID(const Name: string): TSVGObject;

  procedure Walk(SVG: TSVGObject);
  var
    C: integer;
  begin
    if (SVG.fID = Name) or ('#' + SVG.fID = Name) then
    begin
      Result := SVG;
      Exit;
    end;

    for C := 0 to SVG.Count - 1 do
    begin
      Walk(SVG[C]);
      if Assigned(Result) then
        Exit;
    end;
  end;

begin
  Result := nil;
  Walk(Self);
end;

function TSVGObject.FindByType(Typ: TClass; Previous: TSVGObject = nil): TSVGObject;
var
  Found: boolean;

  procedure Walk(SVG: TSVGObject);
  var
    C: integer;
  begin
    if (SVG.ClassName = Typ.ClassName) and (Found) then
    begin
      Result := SVG;
      Exit;
    end;

    if SVG = Previous then
      Found := True;

    for C := 0 to SVG.Count - 1 do
    begin
      Walk(SVG[C]);
      if Assigned(Result) then
        Exit;
    end;
  end;

begin
  Found := (Previous = nil);
  Result := nil;
  Walk(Self);
end;

procedure TSVGObject.Assign(SVG: TSVGObject);
begin
  fVisible := SVG.fVisible;
  fDisplay := SVG.fDisplay;
  fBounds := SVG.GetObjectBounds;
  fID := SVG.fID;
  fObjectName := SVG.fObjectName;

  FreeAndNil(fStyle);
  fStyle := SVG.fStyle.Clone;
  fClasses.Assign(SVG.fClasses);
end;

function TSVGObject.GetCount: integer;
begin
  Result := fItems.Count;
end;

procedure TSVGObject.SetItem(Index: integer; Item: TSVGObject);
begin
  if (Index >= 0) and (Index < Count) then
    fItems[Index] := Item;
end;

function TSVGObject.GetItem(Index: integer): TSVGObject;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TSVGObject(fItems[Index])
  else
    Result := nil;
end;

function TSVGObject.GetObjectBounds: TBounds;
begin
  Result := fBounds;
end;

function TSVGObject.GetRoot: TSVG;
var
  Temp: TSVGObject;
begin
  Temp := Self;

  while Assigned(Temp) and (not (Temp is TSVG)) do
    Temp := Temp.fParent;

  Result := TSVG(Temp);
end;

function TSVGObject.GetDisplay: integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (SVG.fDisplay = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := SVG.fDisplay
  else
    Result := 1;
end;

function TSVGObject.GetVisible: integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (SVG.fVisible = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := SVG.fVisible
  else
    Result := 1;
end;

procedure TSVGObject.ReadIn(const Node: TDOMNode);
var
  C: integer;
  AttributeNode: TDOMNode;
begin
  LoadString(Node, 'id', fID);

  LoadDisplay(Node, fDisplay);
  LoadVisible(Node, fVisible);

  for C := 0 to Node.Attributes.Length - 1 do
  begin
    AttributeNode := Node.Attributes[C];
    fStyle.AddStyle(AttributeNode.nodeName, AttributeNode.TextContent);
  end;

  fStyle.SetValues(LoadString(Node, 'style'));

  fClasses.DelimitedText := LoadString(Node, 'class');
  for C := fClasses.Count - 1 downto 0 do
  begin
    fClasses[C] := Trim(fClasses[C]);
    if fClasses[C] = '' then
      fClasses.Delete(C);
  end;

  fObjectName := Node.nodeName;
end;


{ TSVGTransformedObject }

procedure TSVGTransformedObject.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGTransformedObject then
    fMatrix := TSVGTransformedObject(SVG).fMatrix;
end;

procedure TSVGTransformedObject.CalcMatrix;
var
  C: integer;
  List: TList;
  SVG: TSVGObject;
  CompleteMatrix, LMatrix, NewMatrix: TFloatMatrix;
begin
  List := TList.Create;

  SVG := Self;

  while Assigned(SVG) do
  begin
    List.Insert(0, SVG);
    SVG := SVG.fParent;
  end;

  FillChar(CompleteMatrix, SizeOf(CompleteMatrix), 0);
  FillChar(LMatrix, SizeOf(LMatrix), 0);
  for C := 0 to List.Count - 1 do
  begin
    SVG := TSVGTransformedObject(List[C]);
    if (SVG is TSVGTransformedObject) then
    begin
      if SVG is TSVG then
        NewMatrix := TSVG(SVG).RootMatrix
      else
        NewMatrix := TSVGTransformedObject(SVG).fMatrix;

      if NewMatrix[2, 2] = 1 then
      begin
        if CompleteMatrix[2, 2] = 0 then
          CompleteMatrix := NewMatrix
        else
          CompleteMatrix := Mult(CompleteMatrix, NewMatrix);

        if not (SVG is TSVG) then
        begin
          if LMatrix[2, 2] = 0 then
            LMatrix := NewMatrix
          else
            LMatrix := Mult(LMatrix, NewMatrix);
        end;
      end;
    end;
  end;

  List.Free;

  fCompleteCalculatedMatrix := CompleteMatrix;
  fCalculatedMatrix := LMatrix;
end;

procedure TSVGTransformedObject.Clear;
begin
  inherited;
  FillChar(fMatrix, SizeOf(fMatrix), 0);
end;

procedure TSVGTransformedObject.ReadIn(const Node: TDOMNode);
var
  M: TFloatMatrix;
begin
  inherited;
  M := fMatrix;
  LoadTransform(Node, 'transform', M);
  fMatrix := M;
end;

procedure TSVGTransformedObject.SetMatrix(const Value: TFloatMatrix);
begin
  fMatrix := Value;

  CalcMatrix;
end;

function TSVGTransformedObject.Transform(P: TFloatPoint): TFloatPoint;
var
  TGP: TAffineTransformation;
  Point: TFloatPoint;
begin
  if fCalculatedMatrix[2, 2] = 1 then
  begin
    TGP := GetSVGTransformation(fCalculatedMatrix);
    Point.X := P.X;
    Point.Y := P.Y;
    P := TGP.Transform(Point);
    TGP.Free;
  end;
  Result := P;
end;

function TSVGTransformedObject.Transform(X, Y: TFloat): TFloatPoint;
var
  P: TFloatPoint;
begin
  P.X := X;
  P.Y := Y;
  Result := Transform(P);
end;


{ TSVGBase }

constructor TSVGBase.Create;
begin
  inherited;
  fPath := nil;
  SetLength(fStrokeDashArray, 0);

  fClipPath := nil;
end;

{$IFDEF GPPen}
procedure TSVGBasic.BeforePaint(const Graphics: TBitmap32; const Brush: TGPBrush;
  const Pen: TGPPen);
begin

end;

{$ENDIF}

procedure TSVGBase.CalcClipPath;
begin
  //FreeAndNil(fClipPath);
  fClipPath := GetClipPath;
end;

procedure TSVGBase.Clear;
begin
  inherited;

  fX := 0;
  fY := 0;
  fWidth := 0;
  fHeight := 0;
  fWidthAsPixel := 0;
  fHeightAsPixel := 0;
  fRX := INHERIT;
  fRY := INHERIT;
  fFillURI := '';
  fStrokeURI := '';
  FillColor := INHERIT;
  StrokeColor := INHERIT;

  StrokeWidth := INHERIT;

  StrokeOpacity := 1;
  FillOpacity := 1;
  fLineWidth := INHERIT;

  fStrokeLineJoin := '';
  fStrokeLineCap := '';
  fStrokeMiterLimit := INHERIT;
  fStrokeDashOffset := INHERIT;

  SetLength(fStrokeDashArray, 0);
  fStrokeDashArrayCount := 0;
  fArrayNone := False;

  fFontName := '';
  fFontSize := INHERIT;
  fFontWeight := INHERIT;
  fFontStyle := INHERIT;

  fTextDecoration := [tdInherit];

  FreeAndNil(fPath);
  fClipPath := nil;
end;

procedure TSVGBase.PaintToGraphics(Graphics: TBitmap32);
const
  JoinTypeOfJoinStyle: array[TJoinStyle] of TJoinType =
    (jtMiter, jtSquare, jtRound, jtRoundEx);
  EndTypeOfEndStyle: array[TEndStyle] of TEndType =
    (etOpenButt, etOpenSquare, etOpenRound);

  function Grow(const src: TArrayOfArrayOfFloatPoint; Growth: TFloat;
    JoinStyle: TJoinStyle; EndStyle: TEndStyle;
    AMiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
  begin
    with TClipperOffset.Create() do
      try
        MiterLimit := AMiterLimit;
        AddPaths(src);
        Execute(Growth / 2, JoinTypeOfJoinStyle[JoinStyle],
          EndTypeOfEndStyle[EndStyle], Result);
      finally
        Free;
      end;
  end;

  function Grow1(const src: TArrayOfArrayOfFloatPoint; Growth: TFloat;
    JoinStyle: TJoinStyle; EndStyle: TEndStyle;
    AMiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
  begin
    Result := InflatePaths(src, Growth / 2, JoinTypeOfJoinStyle[JoinStyle],
      EndTypeOfEndStyle[EndStyle], AMiterLimit);
  end;

const
  LFillRule: array[boolean] of tpolyFillMode = (pfNonZero, pfEvenOdd);
var
  TGP: TTransformation;
  Brush, StrokeBrush: TCustomPolygonFiller;
  ClipRoot: TSVGBase;
  LPath: TArrayOfArrayOfFloatPoint;
  Dst: TArrayOfArrayOfFloatPoint;
  LineClosed: boolean;
begin
  if (fPath = nil) then
    Exit;

  LPath := self.fPath.Path;

  if fClipPath = nil then
    CalcClipPath;

  if Assigned(fClipPath) then
  begin
    with TClipper.Create do
      try
        //add multiple contours of existing polygons as subject polygons ...
        AddPaths(LPath);
        //add the single contour of the new polygon as the clipping polygon ...
        AddPaths(fClipPath.Path, ptClip);
        //do the clipping operation (result => Polys) ...
        Execute(ctIntersection, frEvenOdd, LPath);
      finally
        Free;
      end;
  end;

  TGP := GetSVGTransformation(Matrix);
  Brush := GetFillBrush;
  try
    StrokeBrush := GetStrokeBrush;
    try
      if Assigned(Brush) then
        SVGPolyPolygonFS(Graphics, LPath, Brush, LFillRule[self.FillRuleEvenOdd], TGP);


      if Assigned(StrokeBrush) then
      begin
        LineClosed := Assigned(Brush);

        SVGPolyPolylineFS(Graphics, LPath, StrokeBrush, LineClosed,
          GetStrokeWidth(), StrokeLineJoin, StrokeLineCap, StrokeMiterLimit, TGP);
      end;
    finally
      StrokeBrush.Free;
    end;
  finally
    Brush.Free;
    TGP.Free;
  end;
end;

procedure TSVGBase.PaintToPath(Path: TFlattenedPath);
var
  P: TArrayOfArrayOfFloatPoint;
  M: TFloatMatrix;
  i: integer;
begin
  if fPath = nil then
    Exit;
  Path.EndPath(True);
  for i := 0 to Length(fPath.Path) - 1 do
  begin
    Path.Polygon(fPath.path[i]);
  end;
  Path.EndPath(True);
end;

procedure TSVGBase.ReadIn(const Node: TDOMNode);
var
  LRoot: TSVG;
  LStyle: TStyle;
  C: integer;
begin
  inherited;

  LoadLength(Node, 'x', fX);
  LoadLength(Node, 'y', fY);
  LoadLength(Node, 'width', fWidth);
  LoadLength(Node, 'height', fHeight);
  LoadLength(Node, 'rx', fRX);
  LoadLength(Node, 'ry', fRY);

  if (fRX = INHERIT) and (fRY <> INHERIT) then
    fRX := fRY;

  if (fRY = INHERIT) and (fRX <> INHERIT) then
    fRY := fRX;

  LRoot := GetRoot;
  for C := -2 to fClasses.Count do
  begin
    case C of
      -2: LStyle := LRoot.fStyles.GetStyle(fObjectName);
      -1: LStyle := LRoot.fStyles.GetStyle('#' + fID);
      else
      begin
        if C < fClasses.Count then
        begin
          if Assigned(LRoot) then
          begin
            LStyle := LRoot.fStyles.GetStyle('.' + fClasses[C]);
            if LStyle = nil then
              LStyle := LRoot.fStyles.GetStyle(fClasses[C]);
          end
          else
            LStyle := nil;
        end
        else
          LStyle := fStyle;
      end;
    end;

    if Assigned(LStyle) then
      ReadStyle(LStyle);
  end;

  FillColor := GetColor(fFillURI);
  StrokeColor := GetColor(fStrokeURI);
  fFillURI := ParseURI(fFillURI);
  fStrokeURI := ParseURI(fStrokeURI);
  ClipURI := ParseURI(fClipURI);
end;

procedure TSVGBase.Assign(SVG: TSVGObject);
var
  C: integer;
begin
  inherited;

  if SVG is TSVGBase then
  begin
    fFillColor := TSVGBase(SVG).fFillColor;
    fStrokeColor := TSVGBase(SVG).fStrokeColor;
    fFillOpacity := TSVGBase(SVG).fFillOpacity;
    fStrokeOpacity := TSVGBase(SVG).fStrokeOpacity;
    fStrokeWidth := TSVGBase(SVG).fStrokeWidth;
    fStrokeLineJoin := fStrokeLineJoin;
    fStrokeLineCap := TSVGBase(SVG).fStrokeLineCap;
    fStrokeMiterLimit := TSVGBase(SVG).fStrokeMiterLimit;
    fStrokeDashOffset := TSVGBase(SVG).fStrokeDashOffset;
    fStrokeDashArrayCount := TSVGBase(SVG).fStrokeDashArrayCount;

    fFontName := TSVGBase(SVG).fFontName;
    fFontSize := TSVGBase(SVG).fFontSize;
    fFontWeight := TSVGBase(SVG).fFontWeight;
    fFontStyle := TSVGBase(SVG).fFontStyle;
    fTextDecoration := TSVGBase(SVG).fTextDecoration;

    if Assigned(TSVGBase(SVG).fStrokeDashArray) then
    begin
      SetLength(fStrokeDashArray, fStrokeDashArrayCount);
      for C := 0 to fStrokeDashArrayCount - 1 do
        fStrokeDashArray[C] := TSVGBase(SVG).fStrokeDashArray[C];
    end;

    fArrayNone := TSVGBase(SVG).fArrayNone;

    if Assigned(TSVGBase(SVG).fPath) then
      fPath := TSVGBase(SVG).fPath;

    fRX := TSVGBase(SVG).fRX;
    fRY := TSVGBase(SVG).fRY;
    fFillURI := TSVGBase(SVG).fFillURI;
    fStrokeURI := TSVGBase(SVG).fStrokeURI;
    ClipURI := TSVGBase(SVG).fClipURI;
    fLineWidth := TSVGBase(SVG).fLineWidth;
    fFillRule := TSVGBase(SVG).fFillRule;
    fColorInterpolation := TSVGBase(SVG).fColorInterpolation;
    fColorRendering := TSVGBase(SVG).fColorRendering;

    fX := TSVGBase(SVG).X;
    fY := TSVGBase(SVG).Y;
    fWidth := TSVGBase(SVG).Width;
    fHeight := TSVGBase(SVG).Height;
    fWidthAsPixel := TSVGBase(SVG).fWidthAsPixel;
    fHeightAsPixel := TSVGBase(SVG).fHeightAsPixel;
  end;
end;

function TSVGBase.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGBase.Create(AParent);
end;

procedure TSVGBase.ReadStyle(AStyle: TStyle);
var
  Value: string;
  SL: TStringList;

  procedure ConstructFont;
  var
    Bold, Italic: integer;
    FN: string;
  begin
    Bold := Pos('Bold', fFontName);
    Italic := Pos('Italic', fFontName);

    FN := fFontName;

    // Check for Bold
    if Bold <> 0 then
    begin
      fFontName := Copy(FN, 1, Bold - 1) + Copy(FN, Bold + 4, MaxInt);
      if Copy(fFontName, Length(fFontName), 1) = '-' then
        fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
      if IsFontAvailable then
      begin
        AStyle['font-weight'] := 'bold';
        Exit;
      end;
      if Copy(fFontName, Length(fFontName) - 1, 2) = 'MT' then
      begin
        fFontName := Copy(fFontName, 1, Length(fFontName) - 2);
        if Copy(fFontName, Length(fFontName), 1) = '-' then
          fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
        if IsFontAvailable then
        begin
          AStyle['font-weight'] := 'bold';
          Exit;
        end;
      end;
    end;

    // Check for Italic
    if Italic <> 0 then
    begin
      fFontName := Copy(FN, 1, Italic - 1) + Copy(FN, Italic + 6, MaxInt);
      if Copy(fFontName, Length(fFontName), 1) = '-' then
        fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
      if IsFontAvailable then
      begin
        AStyle['font-style'] := 'italic';
        Exit;
      end;
      if Copy(fFontName, Length(fFontName) - 1, 2) = 'MT' then
      begin
        fFontName := Copy(fFontName, 1, Length(fFontName) - 2);
        if Copy(fFontName, Length(fFontName), 1) = '-' then
          fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
        if IsFontAvailable then
        begin
          AStyle['font-style'] := 'italic';
          Exit;
        end;
      end;
    end;

    // Check for Bold and Italic
    if (Bold <> 0) and (Italic <> 0) then
    begin
      fFontName := Copy(FN, 1, Bold - 1) + Copy(FN, Bold + 4, MaxInt);
      if Copy(fFontName, Length(fFontName), 1) = '-' then
        fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
      Italic := Pos('Italic', fFontName);

      fFontName := Copy(fFontName, 1, Italic - 1) + Copy(fFontName, Italic + 6, MaxInt);
      if Copy(fFontName, Length(fFontName), 1) = '-' then
        fFontName := Copy(fFontName, 1, Length(fFontName) - 1);

      if IsFontAvailable then
      begin
        AStyle['font-weight'] := 'bold';
        AStyle['font-style'] := 'italic';
        Exit;
      end;
      if Copy(fFontName, Length(fFontName) - 1, 2) = 'MT' then
      begin
        fFontName := Copy(fFontName, 1, Length(fFontName) - 2);
        if Copy(fFontName, Length(fFontName), 1) = '-' then
          fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
        if IsFontAvailable then
        begin
          AStyle['font-weight'] := 'bold';
          AStyle['font-style'] := 'italic';
          Exit;
        end;
      end;
    end;

    fFontName := FN;
    if Copy(fFontName, Length(fFontName) - 1, 2) = 'MT' then
    begin
      fFontName := Copy(fFontName, 1, Length(fFontName) - 2);
      if Copy(fFontName, Length(fFontName), 1) = '-' then
        fFontName := Copy(fFontName, 1, Length(fFontName) - 1);
      if IsFontAvailable then
        Exit;
    end;

    fFontName := FN;
  end;

begin
  Value := AStyle.Values['stroke-width'];
  if Value <> '' then
    fStrokeWidth := ParseLength(Value);

  Value := AStyle.Values['line-width'];
  if Value <> '' then
    fLineWidth := ParseLength(Value);

  Value := AStyle.Values['opacity'];
  if Value <> '' then
  begin
    fStrokeOpacity := ParsePercent(Value);
    fFillOpacity := fStrokeOpacity;
  end;

  Value := AStyle.Values['stroke-opacity'];
  if Value <> '' then
    fStrokeOpacity := ParsePercent(Value);

  Value := AStyle.Values['fill-opacity'];
  if Value <> '' then
    fFillOpacity := ParsePercent(Value);

  Value := AStyle.Values['fill-rule'];
  if Value = 'evenodd' then
    fFillRuleEvenOdd := True;

  Value := AStyle.Values['color'];
  if Value <> '' then
  begin
    fStrokeURI := Value;
    fFillURI := Value;
  end;

  Value := AStyle.Values['stroke'];
  if Value <> '' then
    fStrokeURI := Value;

  Value := AStyle.Values['fill'];
  if Value <> '' then
    fFillURI := Value;

  Value := AStyle.Values['clip-path'];
  if Value <> '' then
    ClipURI := Value;

  Value := AStyle.Values['stroke-linejoin'];
  if Value <> '' then
    fStrokeLineJoin := Value;

  Value := AStyle.Values['stroke-linecap'];
  if Value <> '' then
    fStrokeLineCap := Value;

  Value := AStyle.Values['stroke-miterlimit'];
  if Value <> '' then
    if not TryStrToTFloat(Value, fStrokeMiterLimit) then
      fStrokeMiterLimit := 0;

  Value := AStyle.Values['stroke-dashoffset'];
  if Value <> '' then
    if not TryStrToTFloat(Value, fStrokeDashOffset) then
      fStrokeDashOffset := 0;

  Value := AStyle.Values['stroke-dasharray'];
  if Value <> '' then
    SetStrokeDashArray(Value);

  Value := AStyle['font-family'];
  if Value <> '' then
  begin
    fFontName := Value;
    if not IsFontAvailable then
      ConstructFont;
  end;

  Value := AStyle['font-weight'];
  if Value <> '' then
    ParseFontWeight(Value);

  Value := AStyle['font-size'];
  if Value <> '' then
    fFontSize := ParseLength(Value);

  Value := AStyle['text-decoration'];
  if Value <> '' then
  begin
    SL := TStringList.Create;
    SL.Delimiter := ' ';
    SL.DelimitedText := Value;

    if SL.IndexOf('underline') > -1 then
    begin
      Exclude(fTextDecoration, tdInherit);
      Include(fTextDecoration, tdUnderLine);
    end;

    if SL.IndexOf('overline') > -1 then
    begin
      Exclude(fTextDecoration, tdInherit);
      Include(fTextDecoration, tdOverLine);
    end;

    if SL.IndexOf('line-through') > -1 then
    begin
      Exclude(fTextDecoration, tdInherit);
      Include(fTextDecoration, tdStrikeOut);
    end;

    if SL.IndexOf('none') > -1 then
      fTextDecoration := [];

    SL.Free;
  end;

  Value := AStyle['font-style'];
  if Value <> '' then
  begin
    if Value = 'normal' then
      fFontStyle := FontNormal;

    if Value = 'italic' then
      fFontStyle := FontItalic;
  end;
end;

procedure TSVGObject.ReadChildren(const Node: TDOMNode);
var
  C: integer;
  SVG: TSVGObject;
  Root: TSVG;
  tag: string;
begin
  for C := 0 to Node.childNodes.Count - 1 do
  begin
    SVG := nil;

    tag := Node.childNodes[C].nodeName;

    if tag = 'g' then
      SVG := TSVGContainer.Create(Self)
    else

    if tag = 'switch' then
      SVG := TSVGSwitch.Create(Self)
    else

    if tag = 'defs' then
      SVG := TSVGDefs.Create(Self)
    else

    if tag = 'use' then
      SVG := TSVGUse.Create(Self)
    else

    if tag = 'rect' then
      SVG := TSVGRect.Create(Self)
    else

    if tag = 'line' then
      SVG := TSVGLine.Create(Self)
    else

    if tag = 'polyline' then
      SVG := TSVGPolyLine.Create(Self)
    else

    if tag = 'polygon' then
      SVG := TSVGPolygon.Create(Self)
    else

    if tag = 'circle' then
      SVG := TSVGEllipse.Create(Self)
    else

    if tag = 'ellipse' then
      SVG := TSVGEllipse.Create(Self)
    else

    if tag = 'path' then
      SVG := TSVGPath.Create(Self)
    else

    if tag = 'image' then
      SVG := TSVGImage.Create(Self)
    else

    if tag = 'clipPath' then
      SVG := TSVGClipPath.Create(Self)
    else

    if tag = 'linearGradient' then
      SVG := TSVGLinearGradient.Create(Self)
    else

    if tag = 'radialGradient' then
      SVG := TSVGRadialGradient.Create(Self)
    else

    if tag = 'symbol' then
      SVG := TSVGSymbol.Create(Self)
    else

    if tag = 'pattern' then
      SVG := TSVGPattern.Create(Self)
    else

    if tag = 'style' then
    begin
      Root := GetRoot;
      Root.ReadStyles(Node.childNodes[C]);
    end;

    if Assigned(SVG) then
      SVG.ReadIn(Node.childNodes[C]);
  end;
end;

procedure TSVGBase.SetClipURI(const Value: string);
begin
  fClipURI := Value;

  CalcClipPath;
end;

procedure TSVGBase.SetStrokeDashArray(const S: string);
var
  C, E: integer;
  SL: TStringList;
  D: TFloat;
begin
  SetLength(fStrokeDashArray, 0);

  fArrayNone := False;
  if Trim(S) = 'none' then
  begin
    fArrayNone := True;
    Exit;
  end;

  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.DelimitedText := S;

  for C := SL.Count - 1 downto 0 do
  begin
    SL[C] := Trim(SL[C]);
    if SL[C] = '' then
      SL.Delete(C);
  end;

  if SL.Count = 0 then
  begin
    SL.Free;
    Exit;
  end;

  if SL.Count mod 2 = 1 then
  begin
    E := SL.Count;
    for C := 0 to E - 1 do
      SL.Add(SL[C]);
  end;

  SetLength(fStrokeDashArray, SL.Count);
  fStrokeDashArrayCount := SL.Count;

  for C := 0 to SL.Count - 1 do
  begin
    if not TryStrToTFloat(SL[C], D) then
      D := 0;
    fStrokeDashArray[C] := D;
  end;

  SL.Free;
end;

function TSVGBase.GetBrush(AURI: string): TCustomPolygonFiller;
var
  Color: integer;
  C32: TColor32;
  Opacity: integer;
  Filler: TSVGObject;
begin
  Result := nil;
  Opacity := Round(255 * FillOpacity);

  if AURI <> '' then
  begin
    Filler := GetRoot.FindByID(AURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self);
  end
  else
  begin
    Color := FillColor;
    if Color >= 0 then
    begin
      C32 := Color32(Color);
      PColor32Entry(@C32)^.A := Opacity;

      Result := TSolidPoligonFiller.Create;
      TSolidPoligonFiller(Result).Color := C32;
    end;
  end;
end;

function TSVGBase.GetFillBrush: TCustomPolygonFiller;
begin
  Result := GetBrush(fFillURI);
end;


function TSVGBase.GetFillColor: integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fFillColor = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := TSVGBase(SVG).fFillColor
  else
    Result := 0;
end;

function TSVGBase.GetStrokeBrush: TCustomPolygonFiller;
var
  Color: integer;
  C32: TColor32;
  Opacity: integer;
  Filler: TSVGObject;
begin
  Result := nil;


  Opacity := Round(255 * StrokeOpacity);

  if fStrokeURI <> '' then
  begin
    Filler := GetRoot.FindByID(fStrokeURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self);
  end
  else
  begin
    Color := StrokeColor;
    if Color >= 0 then
    begin
      C32 := Color32(Color);
      PColor32Entry(@C32)^.A := Opacity;

      Result := TSolidPoligonFiller.Create;
      TSolidPoligonFiller(Result).Color := C32;
    end;
  end;
end;

function TSVGBase.GetStrokeColor: integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeColor = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := TSVGBase(SVG).fStrokeColor
  else
    Result := -2;
end;

function TSVGBase.GetFillOpacity: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fFillOpacity = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := TSVGBase(SVG).fFillOpacity
  else
    Result := 1;

  SVG := fParent;
  while Assigned(SVG) do
  begin
    Result := Result * TSVGBase(SVG).FillOpacity;
    SVG := SVG.fParent;
  end;
end;

function TSVGBase.GetStrokeOpacity: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeOpacity = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := TSVGBase(SVG).fStrokeOpacity
  else
    Result := 1;

  SVG := fParent;
  while Assigned(SVG) do
  begin
    Result := Result * TSVGBase(SVG).StrokeOpacity;
    SVG := SVG.fParent;
  end;
end;

function TSVGBase.GetStrokeWidth: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeWidth = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (SVG is TSVGBase) then
    Result := TSVGBase(SVG).fStrokeWidth
  else
    Result := -2;
end;

function TSVGBase.GetTextDecoration: TTextDecoration;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (tdInherit in TSVGBase(SVG).fTextDecoration) do
    SVG := SVG.fParent;

  if Assigned(SVG) then
    Result := TSVGBase(SVG).fTextDecoration
  else
    Result := [];
end;

function TSVGBase.IsFontAvailable: boolean;
begin
  Result := True;
end;

function TSVGBase.GetClipURI: string;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (SVG is TSVGBase) and (TSVGBase(SVG).fClipURI = '') do
    SVG := SVG.fParent;

  if Assigned(SVG) and (SVG is TSVGBase) then
    Result := TSVGBase(SVG).fClipURI
  else
    Result := '';
end;

function TSVGBase.GetStrokeLineCap: TEndStyle;
var
  SVG: TSVGObject;
begin
  Result := esButt;

  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeLineCap = '') do
    SVG := SVG.fParent;

  if Assigned(SVG) then
  begin
    if TSVGBase(SVG).fStrokeLineCap = 'round' then
      Result := esRound;

    if TSVGBase(SVG).fStrokeLineCap = 'square' then
      Result := esSquare;
  end;
end;

function TSVGBase.GetStrokeLineJoin: TJoinStyle;
var
  SVG: TSVGObject;
begin
  Result := jsMiter;

  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeLineJoin = '') do
    SVG := SVG.fParent;

  if Assigned(SVG) then
  begin
    if TSVGBase(SVG).fStrokeLineJoin = 'round' then
      Result := jsRound;

    if TSVGBase(SVG).fStrokeLineJoin = 'bevel' then
      Result := jsBevel;
  end;
end;

function TSVGBase.GetStrokeMiterLimit: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 4;

  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeMiterLimit = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (TSVGBase(SVG).fStrokeMiterLimit <> INHERIT) then
    Result := TSVGBase(SVG).fStrokeMiterLimit;
end;

function TSVGBase.GetStrokeDashOffset: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 0;

  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeDashOffset = INHERIT) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (TSVGBase(SVG).fStrokeDashOffset <> INHERIT) then
    Result := TSVGBase(SVG).fStrokeDashOffset;
end;

function TSVGBase.GetStrokeDashArray(var ACount: integer): PSingle;
var
  SVG: TSVGObject;
begin
  Result := nil;
  ACount := 0;

  SVG := Self;
  while Assigned(SVG) and (TSVGBase(SVG).fStrokeDashArrayCount = 0) and
    (not TSVGBase(SVG).fArrayNone) do
    SVG := SVG.fParent;

  if Assigned(SVG) and Assigned(TSVGBase(SVG).fStrokeDashArray) and
    (not TSVGBase(SVG).fArrayNone) then
  begin
    Result := @TSVGBase(SVG).fStrokeDashArray;
    ACount := TSVGBase(SVG).fStrokeDashArrayCount;
  end;
end;

function TSVGBase.GetFontName: string;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and ((not (SVG is TSVGBase)) or
      (TSVGBase(SVG).fFontName = '')) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (SVG is TSVGBase) then
    Result := TSVGBase(SVG).fFontName
  else
    Result := 'Arial';
end;

function TSVGBase.GetFontWeight: integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and ((not (SVG is TSVGBase)) or
      (TSVGBase(SVG).fFontWeight = INHERIT)) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (SVG is TSVGBase) then
    Result := TSVGBase(SVG).fFontWeight
  else
    Result := FW_NORMAL;
end;

function TSVGBase.GetFontSize: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and ((not (SVG is TSVGBase)) or
      (TSVGBase(SVG).fFontSize = INHERIT)) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (SVG is TSVGBase) then
    Result := TSVGBase(SVG).fFontSize
  else
    Result := 11;
end;

function TSVGBase.GetFontStyle: integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and ((not (SVG is TSVGBase)) or
      (TSVGBase(SVG).fFontStyle = INHERIT)) do
    SVG := SVG.fParent;

  if Assigned(SVG) and (SVG is TSVGBase) then
    Result := TSVGBase(SVG).fFontStyle
  else
    Result := 0;
end;

procedure TSVGBase.ParseFontWeight(const S: string);
begin
  TryStrToInt(S, fFontWeight);
  if S = 'normal' then
    fFontWeight := FW_NORMAL;

  if S = 'bold' then
    fFontWeight := FW_BOLD;

  if S = 'bolder' then
    fFontWeight := FW_EXTRABOLD;

  if S = 'lighter' then
    fFontWeight := FW_LIGHT;
end;

procedure TSVGBase.ConstructPath;
begin
  FreeAndNil(fPath);
end;

function TSVGBase.GetClipPath: TFlattenedPath;
var
  Path: TSVGObject;
  ClipRoot: TSVGClipPath;
begin
  Result := nil;

  if ClipURI <> '' then
  begin
    Path := GetRoot.FindByID(ClipURI);
    if Path is TSVGClipPath then
      ClipRoot := TSVGClipPath(Path)
    else
      ClipRoot := nil;
    if Assigned(ClipRoot) then
      Result := ClipRoot.GetClipPath;
    Exit;
  end;
end;


{ TSVG }

constructor TSVG.Create;
begin
  inherited;
  fStyles := TStyleList.Create;
  FillChar(fInitialMatrix, SizeOf(fInitialMatrix), 0);
  fDX := 1;
  fDY := 1;
  fCX := 1;
  fCY := 1;
end;

destructor TSVG.Destroy;
begin
  FreeAndNil(fStyles);
  inherited;
end;

procedure TSVG.AssignTo(ADest: TSVG);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveToStream(ms);
    ms.Position := 0;
    ADest.Clear;
    ADest.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TSVG.Clear;
begin
  inherited;

  fSource := '';

  if Assigned(fStyles) then
    fStyles.Clear;

  FillChar(fViewBox, SizeOf(fViewBox), 0);
  FillChar(fInitialMatrix, SizeOf(fInitialMatrix), 0);
  fX := 0;
  fY := 0;
  fWidth := 0;
  fHeight := 0;
  fViewBoxWidthUnits := suNone;
  fViewBoxHeightUnits := suNone;

  fSize := FloatRect(0.0, 0, 0, 0);
  fRealWidth := 0;
  fRealHeight := 0;

  fRX := 0;
  fRY := 0;

  FillColor := -2;
  FillOpacity := 1;
  StrokeColor := -2;
  StrokeWidth := 1;
  StrokeOpacity := 1;

  fAngle := 0;
  FillChar(fAngleMatrix, SizeOf(TFloatMatrix), 0);

  fLineWidth := 1;

  fFileName := '';
end;

procedure TSVG.LoadFromFile(const FileName: string);
var
  St: TFileStream;
begin
  St := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(St);
    fFileName := FileName;
  finally
    St.Free;
  end;
end;

procedure TSVG.LoadFromStream(Stream: TStream);
var
  SL: TStringList;
  XML: TXMLDocument;
  DocNode: TDOMNode;
begin
  SL := TStringList.Create;
  try
    Stream.Position := 0;
    SL.LoadFromStream(Stream);
    Stream.Position := 0;
    Clear;
    fSource := SL.Text;
    try
      ReadXMLFile(XML, Stream);
      if Assigned(XML) then
      begin
        DocNode := XML.documentElement;
        if DocNode.nodeName = 'svg' then
          ReadIn(DocNode)
        else
          fSource := '';
      end
      else
        fSource := '';
    finally
      XML.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TSVG.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSVG.SaveToStream(Stream: TStream);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := fSource;
    SL.SaveToStream(Stream);
  finally
    SL.Free;
  end;
end;

procedure TSVG.Scale(DX: TFloat; DY: TFloat = -1);
begin
  if DY < 0 then
    DY := DX;

  if SameValue(fDX, DX) and SameValue(fDY, DY) then
    Exit;

  fDX := DX;
  fDY := DY;
end;

procedure TSVG.PaintTo(Graphics: TBitmap32; Bounds: TFloatRect;
  Rects: PRectArray; RectCount: integer);
begin
  SetBounds(Bounds);
  Paint(Graphics, Rects, RectCount);
end;

procedure TSVG.PaintTo(Bitmap: TBitmap; Bounds: TFloatRect; Rects: PRectArray;
  RectCount: integer);
var
  Graphics: TBitmap32;
begin
  Graphics := TBitmap32.Create();
  Graphics.Assign(Bitmap);
  PaintTo(Graphics, Bounds, Rects, RectCount);
  Graphics.Free;
end;

procedure TSVG.RenderTo(Bitmap: TBitmap32; const AAngle: TFloat);
var
  w, h, a, s, c: TFloat;
begin
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    exit;
  fRootBounds := FloatRect(Bitmap.BoundsRect);

  w := Min(fWidthAsPixel, fWidth);
  h := Min(fHeightAsPixel, fHeight);
  w := IfThen(fWidth = 0, fRootBounds.Right, w);
  h := IfThen(fHeight = 0, fRootBounds.Bottom, h);
  fDX := fRootBounds.Right / w;
  fDY := fRootBounds.Bottom / h;
  if AAngle <> 0.0 then
  begin
    fAngleMatrix := IdentityMatrix;
    a := DegToRad(AAngle);
    SinCos(a, s, c);
    fAngleMatrix[0, 0] := c;
    fAngleMatrix[1, 0] := -s;
    fAngleMatrix[0, 1] := s;
    fAngleMatrix[1, 1] := c;
    fCX := w * 0.5;
    fCY := h * 0.5;
  end
  else
    FillChar(fAngleMatrix, SizeOf(TFloatMatrix), 0);
  CalculateMatrices;
  Paint(Bitmap, nil, 0);
end;

procedure TSVG.SetSVGOpacity(Opacity: TFloat);
begin
  StrokeOpacity := Opacity;
  FillOpacity := Opacity;
end;

procedure TSVG.SetViewBox(const Value: TFRect);
begin
  fViewBox := Value;
end;

procedure TSVG.SetAngle(Angle: TFloat);
begin
  if SameValue(fAngle, Angle) then
    Exit;
  fAngle := Angle;
end;

procedure TSVG.SetBounds(const Bounds: TFloatRect);
begin
  fRootBounds := Bounds;

  if fWidth = 0 then
    fWidth := fRootBounds.Right;

  if fHeight = 0 then
    fHeight := fRootBounds.Bottom;

  if (fWidth > 0) and (fRootBounds.Right <> -1) then
    fDX := fRootBounds.Right / fWidth;

  if (fHeight > 0) and (fRootBounds.Bottom <> -1) then
    fDY := fRootBounds.Bottom / fHeight;

  CalculateMatrices;
end;

procedure TSVG.Paint(const Graphics: TBitmap32; Rects: PRectArray; RectCount: integer);

  function InBounds(Item: TSVGObject): boolean;
  var
    C: integer;
    Bounds: TBounds;
  begin
    Result := True;
    if RectCount > 0 then
    begin
      for C := 0 to RectCount - 1 do
      begin
        Bounds := Item.ObjectBounds;
        if Intersect(Bounds, Rects^[C]) then
          Exit;
      end;
      Result := False;
    end;
  end;

  function NeedsPainting(Item: TSVGObject): boolean;
  begin
    Result := (Item.Display = 1) and (Item.fStyle.Values['display'] <> 'none') and
      (Item.Visible = 1);
  end;

  procedure PaintItem(const Item: TSVGObject);
  var
    C: integer;
  begin
    if NeedsPainting(Item) then
    begin
      if InBounds(Item) then
        Item.PaintToGraphics(Graphics);
      for C := 0 to Item.Count - 1 do
        PaintItem(Item[C]);
    end;
  end;

begin
  PaintItem(Self);
end;

procedure TSVG.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVG then
  begin
    fRootBounds := TSVG(SVG).fRootBounds;
    fDX := TSVG(SVG).fDX;
    fDY := TSVG(SVG).fDY;
    fCX := TSVG(SVG).fCX;
    fCY := TSVG(SVG).fCY;
    fInitialMatrix := TSVG(SVG).fInitialMatrix;
    fViewBox := TSVG(SVG).fViewBox;
    fSource := TSVG(SVG).Source;
    fSize := TSVG(SVG).fSize;
    fRealWidth := TSVG(SVG).fRealWidth;
    fRealHeight := TSVG(SVG).fRealHeight;
    fViewBoxWidthUnits := TSVG(SVG).fViewBoxWidthUnits;
    fViewBoxHeightUnits := TSVG(SVG).fViewBoxHeightUnits;

    FreeAndNil(fStyles);
    fStyles := TSVG(SVG).fStyles.Clone;
    fFileName := TSVG(SVG).fFileName;
  end;
end;

function TSVG.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVG.Create(AParent);
end;

procedure TSVG.ReadStyles(const Node: TDOMNode);
var
  C: integer;
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    if GetAttrValue(Node, 'type') = 'text/css' then
      SL.Text := Node.TextContent
    else
      for C := 0 to Node.childNodes.Count - 1 do
        if Node.childNodes[C].nodeName = '#cdata-section' then
          SL.Text := Node.childNodes[C].TextContent;

    for C := SL.Count - 1 downto 0 do
    begin
      SL[C] := Trim(SL[C]);
      if SL[C] = '' then
        SL.Delete(C);
    end;
    for C := 0 to SL.Count - 1 do
      fStyles.Add(SL[C]);
  finally
    SL.Free;
  end;
end;

procedure TSVG.CalcCompleteSize;

  function GetLeft(const Bounds: TBounds): TFloat;
  begin
    Result := Min(Bounds.TopLeft.X, Min(Bounds.TopRight.X,
      Min(Bounds.BottomLeft.X, Bounds.BottomRight.X)));
  end;

  function GetTop(const Bounds: TBounds): TFloat;
  begin
    Result := Min(Bounds.TopLeft.Y, Min(Bounds.TopRight.Y,
      Min(Bounds.BottomLeft.Y, Bounds.BottomRight.Y)));
  end;

  function GetRight(const Bounds: TBounds): TFloat;
  begin
    Result := Max(Bounds.TopLeft.X, Max(Bounds.TopRight.X,
      Max(Bounds.BottomLeft.X, Bounds.BottomRight.X)));
  end;

  function GetBottom(const Bounds: TBounds): TFloat;
  begin
    Result := Max(Bounds.TopLeft.Y, Max(Bounds.TopRight.Y,
      Max(Bounds.BottomLeft.Y, Bounds.BottomRight.Y)));
  end;

  procedure Walk(Item: TSVGObject);
  var
    C: integer;
    Left, Top, Right, Bottom, Width, Height: TFloat;
  begin
    Item.CalcObjectBounds;
    Left := GetLeft(Item.fBounds);
    Top := GetTop(Item.fBounds);
    Right := GetRight(Item.fBounds);
    Bottom := GetBottom(Item.fBounds);

    Width := Right - Left;
    Height := Bottom - Top;

    fSize.Right := Max(Width, fSize.Right);
    fSize.Bottom := Max(Height, fSize.Bottom);

    for C := 0 to Item.Count - 1 do
      Walk(Item[C]);
  end;

begin
  Walk(Self);
  fRealWidth := ConvertWidth(fSize.Right, fViewBoxWidthUnits, suPX,
    GetContainerWidth(suPX));
  fRealHeight := ConvertWidth(fSize.Bottom, fViewBoxHeightUnits, suPX,
    GetContainerHeight(suPX));
end;

procedure TSVG.CalcRootMatrix;
var
  ViewBoxMatrix: TFloatMatrix;
  BoundsMatrix: TFloatMatrix;
  ScaleMatrix: TFloatMatrix;
  TranslateMatrix: TFloatMatrix;
begin
  FillChar(ViewBoxMatrix, SizeOf(ViewBoxMatrix), 0);
  ViewBoxMatrix[0, 0] := 1;
  ViewBoxMatrix[1, 1] := 1;
  ViewBoxMatrix[2, 0] := -fViewBox.Left;
  ViewBoxMatrix[2, 1] := -fViewBox.Top;
  ViewBoxMatrix[2, 2] := 1;

  FillChar(BoundsMatrix, SizeOf(BoundsMatrix), 0);
  BoundsMatrix[0, 0] := 1;
  BoundsMatrix[1, 1] := 1;
  BoundsMatrix[2, 0] := fRootBounds.Left;
  BoundsMatrix[2, 1] := fRootBounds.Top;
  BoundsMatrix[2, 2] := 1;

  FillChar(ScaleMatrix, SizeOf(ScaleMatrix), 0);
  ScaleMatrix[0, 0] := fDX;
  ScaleMatrix[1, 1] := fDY;
  ScaleMatrix[2, 2] := 1;

  if fInitialMatrix[2, 2] = 1 then
    fRootMatrix := fInitialMatrix
  else
  begin
    FillChar(fRootMatrix, SizeOf(fRootMatrix), 0);
    fRootMatrix[0, 0] := 1;
    fRootMatrix[1, 1] := 1;
    fRootMatrix[2, 2] := 1;
  end;

  fRootMatrix := Mult(fRootMatrix, BoundsMatrix);
  fRootMatrix := Mult(fRootMatrix, ViewBoxMatrix);
  fRootMatrix := Mult(fRootMatrix, ScaleMatrix);
  if fAngleMatrix[2, 2] = 1 then
  begin
    TranslateMatrix := IdentityMatrix;
    TranslateMatrix[2, 0] := fCX;
    TranslateMatrix[2, 1] := fCY;
    fRootMatrix := Mult(fRootMatrix, TranslateMatrix);
    fRootMatrix := Mult(fRootMatrix, fAngleMatrix);
    TranslateMatrix := IdentityMatrix;
    TranslateMatrix[2, 0] := -fCX;
    TranslateMatrix[2, 1] := -fCY;
    fRootMatrix := Mult(fRootMatrix, TranslateMatrix);
  end;

  if fMatrix[2, 2] = 1 then
    fRootMatrix := Mult(fRootMatrix, fMatrix);
end;

function TSVG.GetContainerHeight(DestUnit: TSVGUnit): TFloat;
begin
  Result := ConvertHeight(fViewBox.Height, fViewBoxHeightUnits, DestUnit, 0);
end;

function TSVG.GetContainerWidth(DestUnit: TSVGUnit): TFloat;
begin
  Result := ConvertWidth(fViewBox.Width, fViewBoxWidthUnits, DestUnit, 0);
end;

function TSVG.Convert(ASize: TFloat; SourceUnit, DestUnit: TSVGUnit;
  ContainerSize: TFloat): TFloat;
var
  SourceFactor, DestFactor: integer;
  h: TFloat;
begin
  if SourceUnit = suNone then
    SourceUnit := suPX;
  if DestUnit = suNone then
    DestUnit := suPX;
  if (SourceUnit = destUnit) then
    Result := ASize
  else
  if SourceUnit = suPercent then
    Result := ASize / 100 * ContainerSize
  else
  if SourceUnit = suEM then
    Result := Convert(ASize * 12, suPT, DestUnit, 0)
  else
  if SourceUnit = suEX then
    Result := Convert(ASize * 6, suPT, DestUnit, 0)
  else
  if DestUnit = suEM then
  begin
    h := ConvertHeight(12, suPT, SourceUnit, GetContainerHeight(DestUnit));
    Result := IfThen(h = 0, 0, ASize / h);
  end
  else
  if destUnit = suEX then
  begin
    h := ConvertHeight(6, suPT, sourceUnit, GetContainerHeight(DestUnit));
    Result := IfThen(h = 0, 0, ASize / h);
  end
  else
  if sourceUnit = suPX then
  begin
    Result := ASize * (InchFactor[destUnit] / 9600);
  end
  else
  if DestUnit = suPX then
  begin
    Result := ASize * (9600 / InchFactor[sourceUnit]);
  end
  else
  begin
    SourceFactor := InchFactor[SourceUnit];
    DestFactor := InchFactor[DestUnit];
    if (SourceFactor = 0) or (DestFactor = 0) then
      Result := 0
    else
      Result := ASize * (DestFactor / SourceFactor);
  end;
end;

function TSVG.ConvertWidth(ax: TFloat; SourceUnit, DestUnit: TSVGUnit;
  ContainerWidth: TFloat): TFloat;
begin
  if SourceUnit = DestUnit then
    Result := ax
  else if SourceUnit = suNone then
    Result := ax * ConvertWidth(1, suPX, DestUnit, ContainerWidth)
  else if SourceUnit = suPercent then
    Result := ax / 100 * ContainerWidth
  else
    Result := Convert(ax, SourceUnit, DestUnit, ContainerWidth);
end;

function TSVG.ConvertHeight(ay: TFloat; SourceUnit, DestUnit: TSVGUnit;
  ContainerHeight: TFloat): TFloat;
begin
  if SourceUnit = DestUnit then
    Result := ay
  else if SourceUnit = suNone then
    Result := ay * ConvertHeight(1, suPX, DestUnit, ContainerHeight)
  else if SourceUnit = suPercent then
    Result := ay / 100 * ContainerHeight
  else
    Result := Convert(ay, SourceUnit, DestUnit, ContainerHeight);
end;

procedure TSVG.ReadIn(const Node: TDOMNode);
var
  LViewBox: string;
  units: TSVGUnit;
begin
  if Node.nodeName <> 'svg' then
    Exit;

  inherited;

  Display := 1;
  Visible := 1;

  fViewBox.Width := fWidth;
  fViewBox.Height := fHeight;

  LViewBox := GetAttrValue(Node, 'viewBox');
  if LViewBox <> '' then
    fViewBox := ParseDRect(LViewBox, fViewBoxWidthUnits, fViewBoxHeightUnits);

  fWidth := fViewBox.Width;
  fHeight := fViewBox.Height;

  ReadChildren(Node);

  DeReferenceUse;

  CalcCompleteSize;

  units := ParseUnit(GetAttrValue(Node, 'width'));
  if units = suPercent then
    fWidth := fSize.Right * 100 / fWidth;
  fWidthAsPixel := ConvertWidth(fWidth, units, suPX, GetContainerWidth(suPX));

  units := ParseUnit(GetAttrValue(Node, 'height'));
  if units = suPercent then
    fHeight := fSize.Bottom * 100 / fHeight;
  fHeightAsPixel := ConvertHeight(fHeight, units, suPX, GetContainerHeight(suPX));
end;

procedure TSVG.DeReferenceUse;
var
  Child: TSVgObject;
begin
  Child := FindByType(TSVGUse);
  while Assigned(Child) do
  begin
    TSVGUse(Child).Construct;
    Child := FindByType(TSVGUse, Child);
  end;
end;

function TSVG.GetStyleValue(const Name, Key: string): string;
var
  LStyle: TStyle;
begin
  Result := '';
  LStyle := fStyles.GetStyle(Name);
  if Assigned(LStyle) then
    Result := LStyle[Key];
end;

{ TSVGContainer }

function TSVGContainer.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGContainer.Create(AParent);
end;

procedure TSVGContainer.ReadIn(const Node: TDOMNode);
begin
  inherited;
  ReadChildren(Node);
end;

{ TSVGSwitch }

function TSVGSwitch.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGSwitch.Create(AParent);
end;

procedure TSVGSwitch.ReadIn(const Node: TDOMNode);
begin
  inherited;
  ReadChildren(Node);
end;

{ TSVGDefs }

function TSVGDefs.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGDefs.Create(AParent);
end;

procedure TSVGDefs.ReadIn(const Node: TDOMNode);
begin
  inherited;
  Display := 0;
  ReadChildren(Node);
end;

{ TSVGUse }

function TSVGUse.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGUse.Create(AParent);
end;

procedure TSVGUse.PaintToGraphics(Graphics: TBitmap32);
begin
end;

procedure TSVGUse.PaintToPath(Path: TFlattenedPath);
var
  UseObject: TSVGBase;
begin
  inherited;

  if fReference <> '' then
  begin
    UseObject := TSVGBase(GetRoot.FindByID(fReference));
    if Assigned(UseObject) then
      UseObject.PaintToPath(Path);
  end;
end;

procedure TSVGUse.Construct;
var
  Container: TSVGContainer;
  SVG, Child: TSVGObject;
  M: TFloatMatrix;
begin
  while Count > 0 do
    GetItem(0).Free;

  SVG := nil;
  if fReference <> '' then
  begin
    if fReference[1] = '#' then
      SVG := GetRoot.FindByID(Copy(fReference, 2, MaxInt));
  end;

  if SVG = nil then
    Exit;

  FillChar(M, SizeOf(M), 0);
  M[0, 0] := 1;
  M[0, 1] := 0;
  M[1, 0] := 0;
  M[1, 1] := 1;
  M[2, 0] := X;
  M[2, 1] := Y;
  M[2, 2] := 1;

  Container := TSVGContainer.Create(Self);
  Container.fObjectName := 'g';
  Container.fMatrix := M;
  SVG := SVG.Clone(Container);

  Child := SVG.FindByType(TSVGUse);
  while Assigned(Child) do
  begin
    TSVGUse(Child).Construct;
    Child := SVG.FindByType(TSVGUse);
  end;
end;

procedure TSVGUse.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGUse then
  begin
    fReference := TSVGUse(SVG).fReference;
  end;
end;

procedure TSVGUse.Clear;
begin
  inherited;
  fReference := '';
end;

procedure TSVGUse.ReadIn(const Node: TDOMNode);
begin
  inherited;
  LoadString(Node, 'xlink:href', fReference);
end;


{ TSVGRect }

procedure TSVGRect.ReadIn(const Node: TDOMNode);
begin
  inherited;

  if fRX > fWidth / 2 then
    fRX := fWidth / 2;

  if fRY > fHeight / 2 then
    fRY := fHeight / 2;

  ConstructPath;
end;

function TSVGRect.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGRect.Create(AParent);
end;

procedure TSVGRect.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  fBounds.TopLeft := Transform(fX - SW, fY - SW);
  fBounds.TopRight := Transform(fX + fWidth + SW, fY - SW);
  fBounds.BottomRight := Transform(fX + fWidth + SW, fY + Height + SW);
  fBounds.BottomLeft := Transform(fX - SW, fY + fHeight + SW);
end;

procedure TSVGRect.ConstructPath;
begin
  inherited;
  fPath := TFlattenedPath.Create;

  if (fRX <= 0) and (fRY <= 0) then
    fPath.Rectangle(FloatRect(fX, fY, fX + fWidth, fY + fHeight))
  else
    fPath.RoundRect(FloatRect(fX, fY, fX + fWidth, fY + fHeight), fRX {, fRY});
end;


{ TSVGLine }

procedure TSVGLine.ReadIn(const Node: TDOMNode);
begin
  inherited;

  LoadLength(Node, 'x1', fX);
  LoadLength(Node, 'y1', fY);
  LoadLength(Node, 'x2', fWidth);
  LoadLength(Node, 'y2', fHeight);

  ConstructPath;
end;

function TSVGLine.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGLine.Create(AParent);
end;

procedure TSVGLine.CalcObjectBounds;
var
  SW: TFloat;
  Left, Top, Right, Bottom: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  Left := Min(X, Width) - SW;
  Top := Min(Y, Height) - SW;
  Right := Max(X, Width) + SW;
  Bottom := Max(Y, Height) + SW;
  fBounds.TopLeft := Transform(Left, Top);
  fBounds.TopRight := Transform(Right, Top);
  fBounds.BottomRight := Transform(Right, Bottom);
  fBounds.BottomLeft := Transform(Left, Bottom);
end;

procedure TSVGLine.ConstructPath;
begin
  inherited;
  fPath := TFlattenedPath.Create;
  fPath.MoveTo(X, Y);
  fPath.LineTo(Width, Height);
  fPath.EndPath(True);
end;


{ TSVGPolyLine }

constructor TSVGPolyLine.Create;
begin
  inherited;
  fPointCount := 0;
end;

function TSVGPolyLine.GetPoints: PArrayOfFloatPoint;
begin
  Result := @fPoints;
end;

procedure TSVGPolyLine.CalcObjectBounds;
var
  Left, Top, Right, Bottom: TFloat;
  C: integer;
  SW: TFloat;
begin
  Left := MaxTFloat;
  Top := MaxTFloat;
  Right := -MaxTFloat;
  Bottom := -MaxTFloat;
  for C := 0 to fPointCount - 1 do
  begin
    if fPoints[C].X < Left then
      Left := fPoints[C].X;

    if fPoints[C].X > Right then
      Right := fPoints[C].X;

    if fPoints[C].Y < Top then
      Top := fPoints[C].Y;

    if fPoints[C].Y > Bottom then
      Bottom := fPoints[C].Y;
  end;

  SW := Max(0, GetStrokeWidth) / 2;
  fBounds.TopLeft := Transform(Left - SW, Top - SW);
  fBounds.TopRight := Transform(Right + SW, Top - SW);
  fBounds.BottomRight := Transform(Right + SW, Bottom + SW);
  fBounds.BottomLeft := Transform(Left - SW, Bottom + SW);
end;

procedure TSVGPolyLine.Clear;
begin
  inherited;

  SetLength(fPoints, 0);
  fPointCount := 0;
end;

procedure TSVGPolyLine.Assign(SVG: TSVGObject);
var
  C: integer;
begin
  inherited;
  if SVG is TSVGPolyLine then
  begin
    fPointCount := TSVGPolyLine(SVG).fPointCount;

    if Assigned(TSVGPolyLine(SVG).fPoints) then
    begin
      SetLength(fPoints, fPointCount);
      for C := 0 to TSVGPolyLine(SVG).fPointCount - 1 do
      begin
        fPoints[C].X := TSVGPolyLine(SVG).fPoints[C].X;
        fPoints[C].Y := TSVGPolyLine(SVG).fPoints[C].Y;
      end;
    end;
  end;
end;

function TSVGPolyLine.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPolyLine.Create(AParent);
end;

procedure TSVGPolyLine.ConstructPoints(const S: string);
var
  SL: TStringList;
  C: integer;
begin
  SL := TStringList.Create;
  SL.Delimiter := ' ';
  SL.DelimitedText := S;

  for C := SL.Count - 1 downto 0 do
    if SL[C] = '' then
      SL.Delete(C);

  if SL.Count mod 2 = 1 then
  begin
    SL.Free;
    Exit;
  end;

  SetLength(fPoints, 0);

  fPointCount := SL.Count div 2;
  SetLength(fPoints, fPointCount);

  for C := 0 to fPointCount - 1 do
  begin
    if not TryStrToTFloat(SL[C * 2], fPoints[C].X) then
      fPoints[C].X := 0;
    if not TryStrToTFloat(SL[C * 2 + 1], fPoints[C].Y) then
      fPoints[C].Y := 0;
  end;

  SL.Free;
end;

procedure TSVGPolyLine.ReadIn(const Node: TDOMNode);
var
  S: string;
begin
  inherited;

  LoadString(Node, 'points', S);

  S := StringReplace(S, ',', ' ', [rfReplaceAll]);
  S := StringReplace(S, '-', ' -', [rfReplaceAll]);
  S := StringReplace(S, 'e -', 'e-', [rfReplaceAll]);

  ConstructPoints(S);

  ConstructPath;
end;

procedure TSVGPolyLine.ConstructPath;
var
  C: integer;
begin
  inherited;
  if fPoints = nil then
    Exit;

  fPath := TFlattenedPath.Create;
  fPath.Polygon(fPoints);
  fPath.EndPath(True);
end;


{ TSVGPolygon }

function TSVGPolygon.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPolygon.Create(AParent);
end;

procedure TSVGPolygon.ConstructPath;
begin
  inherited;

  if fPoints = nil then
    Exit;

  fPath.EndPath(True);
end;


{ TSVGEllipse }


procedure TSVGEllipse.ReadIn(const Node: TDOMNode);
var
  Lrx, Lry: TFloat;
begin
  inherited;

  LoadLength(Node, 'cx', fX);
  LoadLength(Node, 'cy', fY);

  if Node.NodeName = 'circle' then
  begin
    LoadLength(Node, 'r', Lrx);
    Lry := Lrx;
  end
  else
  begin
    LoadLength(Node, 'rx', Lrx);
    LoadLength(Node, 'ry', Lry);
  end;

  fWidth := Lrx * 2;
  fHeight := Lry * 2;

  ConstructPath;
end;

function TSVGEllipse.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGEllipse.Create(AParent);
end;

procedure TSVGEllipse.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  fBounds.TopLeft := Transform(X - Width - SW, Y - Height - SW);
  fBounds.TopRight := Transform(X + Width + SW, Y - Height - SW);
  fBounds.BottomRight := Transform(X + Width + SW, Y + Height + SW);
  fBounds.BottomLeft := Transform(X - Width - SW, Y + Height + SW);
end;

procedure TSVGEllipse.ConstructPath;
begin
  inherited;
  fPath := TFlattenedPath.Create;
  fPath.Ellipse(X, Y, Width / 2, Height / 2);
end;


{ TSVGPath }

procedure TSVGPath.CalcObjectBounds;
var
  C: integer;
  R: TFRect;
  Left, Top, Right, Bottom: TFloat;
  Found: boolean;
  SW: TFloat;
begin
  Left := MaxTFloat;
  Top := MaxTFloat;
  Right := -MaxTFloat;
  Bottom := -MaxTFloat;
  Found := False;

  for C := 0 to Count - 1 do
  begin
    R := TSVGPathElement(Items[C]).GetBounds;
    if (R.Width <> 0) or (R.Height <> 0) then
    begin
      Found := True;
      Left := Min(Left, R.Left);
      Top := Min(Top, R.Top);
      Right := Max(Right, R.Left + R.Width);
      Bottom := Max(Bottom, R.Top + R.Height);
    end;
  end;

  if not Found then
  begin
    Left := 0;
    Top := 0;
    Right := 0;
    Bottom := 0;
  end;

  SW := Max(0, GetStrokeWidth) / 2;
  fBounds.TopLeft := Transform(Left - SW, Top - SW);
  fBounds.TopRight := Transform(Right + SW, Top - SW);
  fBounds.BottomRight := Transform(Right + SW, Bottom + SW);
  fBounds.BottomLeft := Transform(Left - SW, Bottom + SW);
end;

procedure TSVGPath.ConstructPath;
var
  C: integer;
  Element: TSVGPathElement;
begin
  inherited;

  fPath := TFlattenedPath.Create;
  for C := 0 to Count - 1 do
  begin
    Element := TSVGPathElement(Items[C]);
    Element.AddToPath(fPath);
  end;
  fPath.EndPath(True);
end;

function TSVGPath.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGPath.Create(AParent);
end;

procedure TSVGPath.PrepareMoveLineCurveArc(SL: TStringList);
var
  C, D: integer;
  S: ansistring;
begin
  S := SL[0];

  if SL[0] = 'M' then
    S := 'L';
  if SL[0] = 'm' then
    S := 'l';

  D := 0;

  if (S = 'A') or (S = 'a') then
    D := 7;
  if (S = 'C') or (S = 'c') then
    D := 6;
  if (S = 'S') or (S = 's') or (S = 'Q') or (S = 'q') then
    D := 4;
  if (S = 'T') or (S = 't') or (S = 'M') or (S = 'm') or (S = 'L') or (S = 'l') then
    D := 2;
  if (S = 'H') or (S = 'h') or (S = 'V') or (S = 'v') then
    D := 1;

  if (D = 0) or (SL.Count = D + 1) or ((SL.Count - 1) mod D = 1) then
    Exit;

  for C := SL.Count - D downto (D + 1) do
    if (C - 1) mod D = 0 then
      SL.Insert(C, S);
end;

function TSVGPath.SeparateValues(const S: string): TStringList;
var
  C: integer;
  Help: string;
begin
  Help := S;
  Insert(' ', Help, 2);
  Help := StringReplace(Help, '-', ' -', [rfReplaceAll]);
  Help := StringReplace(Help, 'e -', '-', [rfReplaceAll]);

  Result := TStringList.Create;
  Result.Delimiter := ' ';

  Result.DelimitedText := Help;

  for C := Result.Count - 1 downto 0 do
    if Result[C] = '' then
      Result.Delete(C);

  if Result.Count > 0 then
  begin
    if (Result[0] = 'M') or (Result[0] = 'm') or (Result[0] = 'L') or
      (Result[0] = 'l') or (Result[0] = 'H') or (Result[0] = 'h') or
      (Result[0] = 'V') or (Result[0] = 'v') or (Result[0] = 'C') or
      (Result[0] = 'c') or (Result[0] = 'S') or (Result[0] = 's') or
      (Result[0] = 'Q') or (Result[0] = 'q') or (Result[0] = 'T') or
      (Result[0] = 't') or (Result[0] = 'A') or (Result[0] = 'a') then
      PrepareMoveLineCurveArc(Result);

    if (Result[0] = 'Z') or (Result[0] = 'z') then
      while Result.Count > 1 do
        Result.Delete(1);
  end;
end;

function TSVGPath.Split(const S: string): TStringList;
var
  C, D: integer;
  Part: string;
  Help: string;
  SL: TStringList;
  Found: integer;

  function IsID(Ch: widechar): boolean;
  const
    IDs: array [0..19] of widechar = ('M', 'm', 'L', 'l', 'H', 'h', 'V', 'v',
      'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'A', 'a', 'Z', 'z');
  var
    C: integer;
  begin
    Result := True;
    for C := 0 to 19 do
      if Ch = IDs[C] then
        Exit;
    Result := False;
  end;

begin
  Result := TStringList.Create;

  Help := S;
  while Help <> '' do
  begin
    Found := Length(Help) + 1;
    for C := 2 to Length(Help) do
      if IsID(Help[C]) then
      begin
        Found := C;
        Break;
      end;

    Part := Trim(Copy(Help, 1, Found - 1));
    SL := SeparateValues(Part);
    for D := 0 to SL.Count - 1 do
      Result.Add(SL[D]);
    SL.Free;
    Help := Trim(Copy(Help, Found, Length(Help)));
  end;
end;

procedure TSVGPath.ReadIn(const Node: TDOMNode);
var
  S: string;
  SL: TStringList;
  C: integer;

  Element, LastElement: TSVGPathElement;
begin
  inherited;

  LoadString(Node, 'd', S);
  S := StringReplace(S, ',', ' ', [rfReplaceAll]);
  SL := Split(S);

  C := 0;
  LastElement := nil;

  if SL.Count > 0 then
    repeat
      case SL[C][1] of
        'M', 'm': Element := TSVGPathMove.Create(Self);

        'L', 'l': Element := TSVGPathLine.Create(Self);

        'H', 'h', 'V', 'v': Element := TSVGPathLine.Create(Self);

        'C', 'c': Element := TSVGPathCurve.Create(Self);

        'S', 's', 'Q', 'q': Element := TSVGPathCurve.Create(Self);

        'T', 't': Element := TSVGPathCurve.Create(Self);

        'A', 'a': Element := TSVGPathEllipticArc.Create(Self);

        'Z', 'z': Element := TSVGPathClose.Create(Self);

        else
          Element := nil;
      end;

      if Assigned(Element) then
      begin
        Element.Read(SL, C, LastElement);
        LastElement := Element;
      end;
      Inc(C);
    until C = SL.Count;
  SL.Free;

  ConstructPath;
end;


{ TSVGImage }

constructor TSVGImage.Create;
begin
  inherited;
  fStream := nil;
end;

procedure TSVGImage.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  fBounds.TopLeft := Transform(X - SW, Y - SW);
  fBounds.TopRight := Transform(X + Width + SW, Y - SW);
  fBounds.BottomRight := Transform(X + Width + SW, Y + Height + SW);
  fBounds.BottomLeft := Transform(X - SW, Y + Height - SW);
end;

procedure TSVGImage.Clear;
begin
  inherited;
  FreeAndNil(fStream);
  fFileName := '';
end;

procedure TSVGImage.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGImage then
  begin
    fFileName := TSVGImage(SVG).fFileName;
    if Assigned(TSVGImage(SVG).fStream) then
    begin
      fStream := TMemoryStream.Create;
      TSVGImage(SVG).fStream.Position := 0;
      fStream.LoadFromStream(TSVGImage(SVG).fStream);
      fStream.Position := 0;
    end
    else
    begin
      fStream := TMemoryStream.Create;
      fStream.LoadFromFile(fFileName);
      fStream.Position := 0;
    end;
  end;
end;

function TSVGImage.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGImage.Create(AParent);
end;

procedure TSVGImage.PaintToGraphics(Graphics: TBitmap32);
begin

end;

procedure TSVGImage.ReadIn(const Node: TDOMNode);
var
  S: string;

  function IsValid(var S: string): boolean;
  var
    Semicolon: integer;
  begin
    Result := False;
    if Copy(S, 1, 5) <> 'data:' then
      Exit;
    S := Copy(S, 6, MaxInt);
    Semicolon := Pos(';', S);
    if Semicolon = 0 then
      Exit;
    if Copy(S, Semicolon, 8) = ';base64,' then
    begin
      S := Copy(S, Semicolon + 8, MaxInt);
      Result := True;
    end;
  end;

begin
  inherited;

  LoadString(Node, 'xlink:href', S);
end;


{ TSVGClipPath }

procedure TSVGClipPath.ConstructClipPath;

  procedure AddPath(SVG: TSVGBase);
  var
    C: integer;
  begin
    fInClipPath.EndPath(True);
    SVG.PaintToPath(fInClipPath);

    for C := 0 to SVG.Count - 1 do
      AddPath(TSVGBase(SVG[C]));
    fInClipPath.EndPath(True);
  end;

begin
  if Assigned(fInClipPath) then
    FreeAndNil(fInClipPath);
  fInClipPath := TFlattenedPath.Create;
  AddPath(Self);
end;

destructor TSVGClipPath.Destroy;
begin
  if Assigned(fInClipPath) then
    FreeAndNil(fInClipPath);
  inherited;
end;

procedure TSVGClipPath.PaintToPath(Path: TFlattenedPath);
begin
end;

procedure TSVGClipPath.PaintToGraphics(Graphics: TBitmap32);
begin
end;

procedure TSVGClipPath.Clear;
begin
  inherited;
  if Assigned(fInClipPath) then
    FreeAndNil(fInClipPath);
end;

function TSVGClipPath.GetClipPath: TFlattenedPath;
begin
  if not Assigned(fInClipPath) then
    ConstructClipPath;
  Result := fInClipPath;
end;

function TSVGClipPath.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGClipPath.Create(AParent);
end;

procedure TSVGClipPath.ReadIn(const Node: TDOMNode);
begin
  inherited;
  ReadChildren(Node);
  Display := 0;
end;


{ TSVGSymbol }

function TSVGSymbol.New(AParent: TSVGObject): TSVGObject;
begin
  Result := TSVGSymbol.Create(AParent);
end;

procedure TSVGSymbol.ReadIn(const Node: TDOMNode);
var
  LViewBox: string;
  w, h: TSVGUnit;
begin
  inherited;
  LViewBox := GetAttrValue(Node, 'viewBox');
  if LViewBox <> '' then
    fViewBox := ParseDRect(LViewBox, w, h);

  ReadChildren(Node);
end;

procedure TSVGSymbol.SetViewBox(const Value: TFRect);
begin
  fViewBox := Value;
end;


end.




