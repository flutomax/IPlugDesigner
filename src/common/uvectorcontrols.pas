{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uVectorControls;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, Contnrs, SysUtils, ImgList, Graphics, FPImage, Agg_LCL,
  uCommon, uIObject, uGraphics, uTree, uStorage, uIniFileEx;

type

  TEVColor = (
    kBG = 0,         // background: transparent by default
    kFG, kOFF = kFG, // foreground/OFF states
    kPR, kON = kPR,  // pressed/ON states
    kFR,
    // frame: the stroke around a button or knob handle, or border around the outside of the control
    kHL,             // highlight: mouse over and splash click animation
    kSH,             // shadow
    kX1,             // extra1: typically used for indicator tracks on knobs and sliders
    kX2,             // extra2
    kX3              // extra3
    );

  TIVColorSpecArray = array[TEVColor] of TIColor;

const
  DEFAULT_HIDE_CURSOR = True;
  DEFAULT_SHOW_VALUE = True;
  DEFAULT_SHOW_LABEL = True;
  DEFAULT_DRAW_FRAME = True;
  DEFAULT_DRAW_SHADOWS = True;
  DEFAULT_EMBOSS = False;
  DEFAULT_ROUNDNESS = 0;
  DEFAULT_FRAME_THICKNESS = 1;
  DEFAULT_SHADOW_OFFSET = 3;
  DEFAULT_WIDGET_FRAC = 1;
  DEFAULT_WIDGET_ANGLE = 0;

  DEFAULT_GRAPHICS_BGCOLOR = iclGray;
  DEFAULT_BGCOLOR = iclTransparent;
  DEFAULT_FGCOLOR = iclMidGray;
  DEFAULT_PRCOLOR = iclLightGray;
  DEFAULT_FRCOLOR = iclDarkGray;
  DEFAULT_HLCOLOR = iclTranslucent;
  DEFAULT_SHCOLOR = $3C000000;
  DEFAULT_X1COLOR = iclBlack;
  DEFAULT_X2COLOR = iclGreen;
  DEFAULT_X3COLOR = iclBlue;
  DEFAULT_TEXT_FGCOLOR = iclBlack;
  DEFAULT_TEXTENTRY_BGCOLOR = iclWhite;
  DEFAULT_TEXTENTRY_FGCOLOR = iclBlack;

  DEFAULT_COLOR_SPEC: TIVColorSpecArray = (
    DEFAULT_BGCOLOR, // Background
    DEFAULT_FGCOLOR, // OFF/Foreground
    DEFAULT_PRCOLOR, // ON/Pressed
    DEFAULT_FRCOLOR, // Frame
    DEFAULT_HLCOLOR, // Highlight
    DEFAULT_SHCOLOR, // Shadow
    DEFAULT_X1COLOR, // Extra 1
    DEFAULT_X2COLOR, // Extra 2
    DEFAULT_X3COLOR  // Extra 3
    );

  kVColorStrs: array[TEVColor] of string = (
    'bg',
    'fg/off ',
    'pressed/on',
    'frame',
    'highlight',
    'shadow',
    'extra1',
    'extra2',
    'extra3'
    );



type

  TEVShape = (Rectangle, Ellipse, Triangle, EndsRounded, AllRounded);

  { TIVColorSpec }

  TIVColorSpec = class(TIGraphicsObject, ICustomSerialize)
  private
    fColors: TIVColorSpecArray;
    function GetColor(Index: TEVColor): TIColor;
    procedure SetColor(Index: TEVColor; AValue: TIColor);
    procedure CustomSerialize(const Node: ITreeNode);
    procedure CustomDeSerialize(const Node: ITreeNode);
  public
    constructor Create;
    procedure ResetColors;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromIni(ini: TIniFileEx);
    procedure SaveToIni(ini: TIniFileEx);
    property Colors[Index: TEVColor]: TIColor read GetColor write SetColor; default;
  end;

  { TIVStyle }

  TIVStyle = class(TIGraphicsObject)
  private
    fIsDefault: boolean;
    fName: TStyleName;
    fAngle: double;
    fDrawFrame: boolean;
    fDrawShadows: boolean;
    fEmboss: boolean;
    fFrameThickness: double;
    fHideCursor: boolean;
    fRoundness: double;
    fShadowOffset: double;
    fShowLabel: boolean;
    fShowValue: boolean;
    fWidgetFrac: double;
    fColors: TIVColorSpec;
    fLabelFont: TIFont;
    fValueFont: TIFont;
    procedure ParamChanged(Sender: TObject);
    procedure SetAngle(AValue: double);
    procedure SetDrawFrame(AValue: boolean);
    procedure SetDrawShadows(AValue: boolean);
    procedure SetEmboss(AValue: boolean);
    procedure SetFrameThickness(AValue: double);
    procedure SetHideCursor(AValue: boolean);
    procedure SetName(AValue: TStyleName);
    procedure SetRoundness(AValue: double);
    procedure SetShadowOffset(AValue: double);
    procedure SetShowLabel(AValue: boolean);
    procedure SetShowValue(AValue: boolean);
    procedure SetWidgetFrac(AValue: double);
  public
    constructor Create;
    destructor Destroy; override;
    function Clone: TIVStyle;
    procedure Assign(Source: TPersistent); override;
    procedure GetCodeStr(Lst: TStrings);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
  published
    property IsDefault: boolean read fIsDefault write fIsDefault;
    property Name: TStyleName read fName write SetName;
    property HideCursor: boolean read fHideCursor write SetHideCursor;
    property ShowLabel: boolean read fShowLabel write SetShowLabel;
    property ShowValue: boolean read fShowValue write SetShowValue;
    property DrawFrame: boolean read fDrawFrame write SetDrawFrame;
    property DrawShadows: boolean read fDrawShadows write SetDrawShadows;
    property Emboss: boolean read fEmboss write SetEmboss;
    property Roundness: double read fRoundness write SetRoundness;
    property FrameThickness: double read fFrameThickness write SetFrameThickness;
    property ShadowOffset: double read fShadowOffset write SetShadowOffset;
    property WidgetFrac: double read fWidgetFrac write SetWidgetFrac;
    property Angle: double read fAngle write SetAngle;
    property Colors: TIVColorSpec read fColors;
    property LabelFont: TIFont read fLabelFont;
    property ValueFont: TIFont read fValueFont;
  end;

  { TIVStyleCache }

  TIVStyleCache = class(TINonRefInterfacedObject, ICustomSerialize)
  private
    fOwner: TObject;
    fStyles: TObjectList;
    fDefaultStyle: TIVStyle;
    function GetStyle(AIndex: integer): TIVStyle;
    function GetStyleByName(AName: string): TIVStyle;
    function GetStyleIndex(AName: string): integer;
    function GetStylesCount: integer;
    procedure CustomSerialize(const Node: ITreeNode);
    procedure CustomDeSerialize(const Node: ITreeNode);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    procedure AddStyle(aStyle: TIVStyle);
    function StyleExists(AName: string): boolean;
    property StylesCount: integer read GetStylesCount;
    property Styles[AIndex: integer]: TIVStyle read GetStyle; default;
    property StyleIndex[AName: string]: integer read GetStyleIndex;
    property StyleByName[AName: string]: TIVStyle read GetStyleByName;
    property DefaultStyle: TIVStyle read fDefaultStyle;
  end;

  { TIVectorBase }

  TIVectorBase = class(TISpan)
  private
    fStyle: TIVStyle;
    fLabelInWidget: boolean;
    fValueInWidget: boolean;
    fTrackSize: double;
    fValueDisplayFrac: double;
    fWidgetBounds: TIFloatRect;
    fLabelBounds: TIFloatRect;
    fValueBounds: TIFloatRect;
    fLabelText: string;
    fValueText: string;
    fGearing: string;
    fShape: TEVShape;
    fDirection: TDirection;
    fNumStates: integer;
    fValue: double;
    fMinValue: double;
    fMaxValue: double;
    fActionFunction: string;
    fValueIsEditable: boolean;
    function GetStyleName: TStyleName;
    procedure SetDirection(AValue: TDirection);
    procedure SetGearing(AValue: string);
    procedure SetLabelText(AValue: string);
    procedure SetMaxValue(AValue: double);
    procedure SetMinValue(AValue: double);
    procedure SetNumStates(AValue: integer);
    procedure SetStyleName(AValue: TStyleName);
    procedure SetActionFunction(AValue: string);
    procedure SetShape(AValue: TEVShape);
    procedure SetTrackSize(AValue: double);
    procedure SetValue(AValue: double);
    procedure SetValueIsEditable(AValue: boolean);
  protected
    function GetColor(const AIndex: TEVColor): TIColor;
    function GetAdjustedHandleBounds(const ARect: TIFloatRect): TIFloatRect;
    function GetRoundedCornerRadius(const ARect: TIFloatRect): double;
    function MakeRects(const ARect: TIFloatRect;
      AHasHandle: boolean = False): TIFloatRect;
    procedure Resize; override;
    procedure Paint(Canvas: TICanvas); override;
    procedure DrawBackGround(Canvas: TICanvas; const ARect: TIFloatRect); virtual;
    procedure DrawLabel(Canvas: TICanvas); virtual;
    procedure DrawValue(Canvas: TICanvas); virtual;
    procedure DrawWidget(Canvas: TICanvas); virtual;
    procedure DrawPressableShape(Canvas: TICanvas; AShape: TEVShape;
      ARect: TIFloatRect; Pressed: boolean = False); virtual;
    procedure DrawPressableRectangle(Canvas: TICanvas; ARect: TIFloatRect;
      Pressed: boolean; rtl: boolean = False; rtr: boolean = False;
      rbl: boolean = False; rbr: boolean = False);
    procedure DrawPressableEllipse(Canvas: TICanvas; ARect: TIFloatRect;
      Pressed: boolean);
    procedure DrawPressableTriangle(Canvas: TICanvas; ARect: TIFloatRect;
      Pressed: boolean; Angle: double);
    property Direction: TDirection read fDirection write SetDirection;
    property Style: TIVStyle read fStyle;
    property Shape: TEVShape read fShape write SetShape;
    property ActionFunction: string read fActionFunction write SetActionFunction;
    property NumStates: integer read fNumStates write SetNumStates;
    property &Label: string read fLabelText write SetLabelText;
    property TrackSize: double read fTrackSize write SetTrackSize;
    property Gearing: string read fGearing write SetGearing;
    property Value: double read fValue write SetValue;
    property ValueIsEditable: boolean read fValueIsEditable write SetValueIsEditable;
    property MinValue: double read fMinValue write SetMinValue;
    property MaxValue: double read fMaxValue write SetMaxValue;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
  published
    property StyleName: TStyleName read GetStyleName write SetStyleName;
  end;

  { TIVLabelControl }

  TIVLabelControl = class(TIVectorBase)
  private
    procedure SetValueText(AValue: string);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property &Label: string read fValueText write SetValueText;
  end;

  { TIVButtonBaseControl }

  TIVButtonBaseControl = class(TIVectorBase)
  private
    procedure SetLabelInWidget(AValue: boolean);
    procedure SetValueInWidget(AValue: boolean);
    procedure SetValueText(AValue: string);
  protected
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawValue(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    property ValueText: string read fValueText write SetValueText;
    property LabelInButton: boolean read fLabelInWidget write SetLabelInWidget;
    property ValueInButton: boolean read fValueInWidget write SetValueInWidget;
  end;

  { TIVButtonControl }

  TIVButtonControl = class(TIVButtonBaseControl)
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property Shape;
    property ActionFunction;
    property &Label;
    property LabelInButton;
    property ValueInButton;
  end;

  { TIVSwitchControl }

  TIVSwitchControl = class(TIVButtonBaseControl)
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property &Label;
    property ValueText;
    property ParamIdx;
    property ValueInButton;
    property NumStates;
  end;

  { TIVToggleControl }

  TIVToggleControl = class(TIVButtonBaseControl)
  private
    fInitialState: boolean;
    fOffText: string;
    fOnText: string;
    procedure SetInitialState(AValue: boolean);
    procedure SetOffText(AValue: string);
    procedure SetOnText(AValue: string);
  protected
    procedure DrawValue(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property &Label;
    property ParamIdx;
    property OffText: string read fOffText write SetOffText;
    property OnText: string read fOnText write SetOnText;
    property InitialState: boolean read fInitialState write SetInitialState;
  end;

  { TIVSlideSwitchControl }

  TIVSlideSwitchControl = class(TIVButtonBaseControl)
  private
    fInitialState: integer;
    fStartRect: TIFloatRect;
    fEndRect: TIFloatRect;
    fHandleBounds: TIFloatRect;
    procedure SetInitialState(AValue: integer);
  protected
    procedure Resize; override;
    procedure DrawWidget(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property ParamIdx;
    property Direction;
    property NumStates;
    property &Label;
    property InitialState: integer read fInitialState write SetInitialState;
  end;

  { TIVTabSwitchControl }

  TIVTabSwitchControl = class(TIVButtonBaseControl)
  private
    fInitialState: integer;
    fTabLabels: TStringList;
    fButtons: array of TIFloatRect;
    function GetOptions: string;
    procedure SetInitialState(AValue: integer);
    procedure SetOptions(AValue: string);
  protected
    procedure Resize; override;
    procedure Paint(Canvas: TICanvas); override;
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawButton(Canvas: TICanvas; const R: TIFloatRect;
      Pressed, sStart, sEnd: boolean); dynamic;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property ParamIdx;
    property Shape;
    property &Label;
    property Options: string read GetOptions write SetOptions;
    property InitialState: integer read fInitialState write SetInitialState;
  end;

  { TIVRadioButtonControl }

  TIVRadioButtonControl = class(TIVTabSwitchControl)
  private
    fButtonSize: double;
    procedure SetButtonSize(AValue: double);
  protected
    procedure DrawWidget(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ButtonSize: double read fButtonSize write SetButtonSize;
  end;

  { TIVKnobControl }

  TIVKnobControl = class(TIVectorBase)
  private
    fTrackToHandleDistance: double;
    fInnerPointerFrac: double;
    fOuterPointerFrac: double;
    fPointerThickness: double;
    fAngle1: double;
    fAngle2: double;
    fAnchorAngle: double; // for bipolar arc
    procedure SetAnchorAngle(AValue: double);
    procedure SetAngle1(AValue: double);
    procedure SetAngle2(AValue: double);
    procedure SetInnerPointerFrac(AValue: double);
    procedure SetOuterPointerFrac(AValue: double);
    procedure SetPointerThickness(AValue: double);
    procedure SetValueInWidget(AValue: boolean);
  protected
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawIndicatorTrack(Canvas: TICanvas;
      angle, cx, cy, radius: double); dynamic;
    procedure DrawPointer(Canvas: TICanvas; angle, cx, cy, radius: double); dynamic;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property ParamIdx;
    property &Label;
    property Value;
    property ValueIsEditable;
    property Gearing;
    property TrackSize;
    property Angle1: double read fAngle1 write SetAngle1;
    property Angle2: double read fAngle2 write SetAngle2;
    property AnchorAngle: double read fAnchorAngle write SetAnchorAngle;
    property InnerPointerFrac: double read fInnerPointerFrac write SetInnerPointerFrac;
    property OuterPointerFrac: double read fOuterPointerFrac write SetOuterPointerFrac;
    property PointerThickness: double read fPointerThickness write SetPointerThickness;
    property ValueInWidget: boolean read fValueInWidget write SetValueInWidget;
  end;

  { TIVSliderControl }

  TIVSliderControl = class(TIVectorBase)
  private
    fHandleSize: double;
    fHandleInsideTrack: boolean;
    fTrackBounds: TIFloatRect;
    procedure SetHandleInsideTrack(AValue: boolean);
    procedure SetHandleSize(AValue: double);
  protected
    procedure Resize; override;
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawTrack(Canvas: TICanvas; FilledArea: TIFloatRect); dynamic;
    procedure DrawHandle(Canvas: TICanvas; Bounds: TIFloatRect); dynamic;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property ParamIdx;
    property &Label;
    property Value;
    property ValueIsEditable;
    property Gearing;
    property TrackSize;
    property Direction;
    property HandleSize: double read fHandleSize write SetHandleSize;
    property HandleInsideTrack: boolean read fHandleInsideTrack
      write SetHandleInsideTrack;
  end;

  { TIVTrackControlBase }

  TIVTrackControlBase = class(TIVectorBase)
  private
    fNTracks: integer;
    fMaxNTracks: integer;
    fLowParamidx: string;
    fMinTrackValue: double;
    fMaxTrackValue: double;
    fTrackPadding: double;
    fParams: TStringList;
    fValues: TStringList;
    fPeakSize: double;
    fDrawTrackFrame: boolean;
    fTrackBounds: array of TIFloatRect;
    function GetParams: string;
    function GetValues: string;
    procedure SetLowParamidx(AValue: string);
    procedure SetParams(AValue: string);
    procedure SetValues(AValue: string);
    procedure TracksChange;
    procedure SetMaxNTracks(AValue: integer);
    function GetValue(chIdx: integer): double;
  protected
    procedure Resize; override;
    procedure MakeTrackRects(Bounds: TIFloatRect); dynamic;
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawTrack(Canvas: TICanvas; const r: TIFloatRect; chIdx: integer); dynamic;
    procedure DrawTrackBG(Canvas: TICanvas; const r: TIFloatRect; chIdx: integer); dynamic;
    procedure DrawTrackHandle(Canvas: TICanvas; const r: TIFloatRect; chIdx: integer); dynamic;
    procedure DrawPeak(Canvas: TICanvas; const r: TIFloatRect; chIdx: integer); dynamic;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    destructor Destroy; override;
    class function GetObjectName: string; override;
    property MaxNTracks: integer read fMaxNTracks write SetMaxNTracks;
    property LowParamidx: string read fLowParamidx write SetLowParamidx;
    property Params: string read GetParams write SetParams;
    property Values: string read GetValues write SetValues;
  end;

  { TIVRangeSliderControl }

  TIVRangeSliderControl = class(TIVTrackControlBase)
  private
    fHandleSize: double;
    fOnlyHandle: boolean;
    function GetHandleBounds(TrackIdx: integer): TIFloatRect;
    procedure SetHandleSize(AValue: double);
    procedure SetOnlyHandle(AValue: boolean);
  protected
    procedure Paint(Canvas: TICanvas); override;
    procedure MakeTrackRects(Bounds: TIFloatRect); override;
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawTrack(Canvas: TICanvas; const r: TIFloatRect; chIdx: integer); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property Params;
    property Values;
    property &Label;
    property TrackSize;
    property Direction;
    property OnlyHandle: boolean read fOnlyHandle write SetOnlyHandle;
    property HandleSize: double read fHandleSize write SetHandleSize;
  end;

  { TIVGroupControl }

  TIVGroupControl = class(TIVectorBase)
  private
    fLabelOffset: double;
    fLabelPadding: double;
    procedure SetLabelOffset(AValue: double);
  protected
    procedure Resize; override;
    procedure Paint(Canvas: TICanvas); override;
    procedure DrawWidget(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property &Label;
    property LabelOffset: double read fLabelOffset write SetLabelOffset;
  end;

  { TIVPanelControl }

  TIVPanelControl = class(TIVectorBase)
  protected
    procedure Paint(Canvas: TICanvas); override;
    procedure DrawWidget(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property &Label;
  end;

  { TIVNumberBoxControl }

  TIVNumberBoxControl = class(TIVectorBase)
  private
    fTextReadout: TIFloatRect;
    fIncButton: TIFloatRect;
    fDecButton: TIFloatRect;
    fDefaultValue: double;
    fFmtStr: string;
    procedure MakeControlRects;
    procedure SetDefaultValue(AValue: double);
    procedure SetFmtStr(AValue: string);
  protected
    procedure Resize; override;
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawLabel(Canvas: TICanvas); override;
    procedure DrawValue(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property ActionFunction;
    property ParamIdx;
    property &Label;
    property MaxValue;
    property MinValue;
    property Value;
    property DefaultValue: double read fDefaultValue write SetDefaultValue;
    property FmtStr: string read fFmtStr write SetFmtStr;
  end;

  { TIVXYPadControl }

  TIVXYPadControl = class(TIVectorBase)
  private
    fHandleRadius: double;
    fParams: array[0..1] of string;
    fValues: array[0..1] of double;
    function GetParams: string;
    function GetValues: string;
    procedure SetHandleRadius(AValue: double);
    procedure SetParams(AValue: string);
    procedure SetValues(AValue: string);
  protected
    procedure Paint(Canvas: TICanvas); override;
    procedure DrawWidget(Canvas: TICanvas); override;
    procedure DrawTrack(Canvas: TICanvas); dynamic;
    procedure DrawHandle(Canvas: TICanvas; const TrackBounds,
      HandleBounds: TIFloatRect); dynamic;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property &Label;
    property HandleRadius: double read fHandleRadius write SetHandleRadius;
    property Params: string read GetParams write SetParams;
    property Values: string read GetValues write SetValues;
  end;

  { TIVMeterControl }

  TIVMeterControl = class(TIVTrackControlBase)
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property &Label;
    property Direction;
    property MaxNTracks;
    property Values;
  end;

  { TIVMultiSliderControl }

  TIVMultiSliderControl = class(TIVTrackControlBase)
  private
    procedure SetMaxTrackValue(AValue: double);
    procedure SetMinTrackValue(AValue: double);
  protected
    procedure Paint(Canvas: TICanvas); override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject); override;
    class function GetObjectName: string; override;
    function GetCodeStr: string; override;
  published
    property &Label;
    property Direction;
    property Params;
    property Values;
    property LowParamidx;
    property MaxNTracks;
    property MinTrackValue: double read fMinTrackValue write SetMinTrackValue;
    property MaxTrackValue: double read fMaxTrackValue write SetMaxTrackValue;
  end;

  { TIVTestControl }

  TIVTestControl = class(TIVButtonBaseControl)
  private
    fCanvas: TICanvas;
  public
    constructor Create(AStyle: TIVStyle; ABounds: TIFloatRect; ACanvas: TICanvas); reintroduce;
    procedure Paint(MouseDown: boolean); reintroduce;
  end;

implementation

uses
  StrUtils, Math, TypInfo, uIDocument, uFauIcons, uAGGCanvas;

{ TIVColorSpec }

constructor TIVColorSpec.Create;
begin
  inherited Create;
  ResetColors;
end;

procedure TIVColorSpec.ResetColors;
begin
  fColors := DEFAULT_COLOR_SPEC;
end;

procedure TIVColorSpec.Assign(Source: TPersistent);
begin
  if Source is TIVColorSpec then
  begin
    fColors := TIVColorSpec(Source).fColors;
  end
  else
    inherited Assign(Source);
end;

procedure TIVColorSpec.LoadFromIni(ini: TIniFileEx);
var
  i: TEVColor;
begin
  for i := Low(TEVColor) to High(TEVColor) do
    fColors[i] := ini.ReadHex('Colors', GetEnumName(TypeInfo(TEVColor), Ord(i)), iclBlack);
end;

procedure TIVColorSpec.SaveToIni(ini: TIniFileEx);
var
  i: TEVColor;
begin
  for i := Low(TEVColor) to High(TEVColor) do
    ini.WriteHex('Colors', GetEnumName(TypeInfo(TEVColor), Ord(i)), fColors[i]);
end;

function TIVColorSpec.GetColor(Index: TEVColor): TIColor;
begin
  Result := fColors[Index];
end;

procedure TIVColorSpec.SetColor(Index: TEVColor; AValue: TIColor);
begin
  if fColors[Index] = AValue then
    exit;
  Changing;
  fColors[Index] := AValue;
  Changed;
end;

procedure TIVColorSpec.CustomSerialize(const Node: ITreeNode);
var
  NewNode: ITreeNode;
  i: TEVColor;
begin
  NewNode := Node.AddNode('Colors');
  for i := Low(TEVColor) to High(TEVColor) do
    NewNode.AddParam(kVColorStrs[i], fColors[i]);
end;

procedure TIVColorSpec.CustomDeSerialize(const Node: ITreeNode);
var
  NewNode: ITreeNode;
  i: TEVColor;
begin
  NewNode := Node.NodeByName['Colors'];
  if Assigned(NewNode) then
    for i := Low(TEVColor) to High(TEVColor) do
      fColors[i] := Cardinal(NewNode.ParamValueByName[kVColorStrs[i]]);
end;


{ TIVStyle }

constructor TIVStyle.Create;
begin
  inherited Create;
  fIsDefault := False;
  fName := DEFAULT_STYLE_NAME;
  fColors := TIVColorSpec.Create;
  fColors.OnChange := @ParamChanged;
  fLabelFont := TIFont.Create;
  fLabelFont.Size := DEFAULT_TEXT_SIZE + 5;
  fLabelFont.VAlign := TITextVAlign.Top;
  fLabelFont.OnChange := @ParamChanged;
  fValueFont := TIFont.Create;
  fValueFont.VAlign := TITextVAlign.Bottom;
  fValueFont.OnChange := @ParamChanged;
  fShowLabel := DEFAULT_SHOW_LABEL;
  fShowValue := DEFAULT_SHOW_VALUE;
  fHideCursor := DEFAULT_HIDE_CURSOR;
  fDrawFrame := DEFAULT_DRAW_FRAME;
  fDrawShadows := DEFAULT_DRAW_SHADOWS;
  fEmboss := DEFAULT_EMBOSS;
  fRoundness := DEFAULT_ROUNDNESS;
  fFrameThickness := DEFAULT_FRAME_THICKNESS;
  fShadowOffset := DEFAULT_SHADOW_OFFSET;
  fWidgetFrac := DEFAULT_WIDGET_FRAC;
  fAngle := DEFAULT_WIDGET_ANGLE;
end;

destructor TIVStyle.Destroy;
begin
  FreeAndNil(fColors);
  FreeAndNil(fLabelFont);
  FreeAndNil(fValueFont);
  inherited Destroy;
end;

function TIVStyle.Clone: TIVStyle;
begin
  Result := TIVStyle.Create;
  Result.Assign(self);
  Result.fIsDefault := False;
end;

procedure TIVStyle.Assign(Source: TPersistent);
begin
  if Source is TIVStyle then
  begin
    fColors.Assign(TIVStyle(Source).fColors);
    fLabelFont.Assign(TIVStyle(Source).fLabelFont);
    fValueFont.Assign(TIVStyle(Source).fValueFont);
    fIsDefault := TIVStyle(Source).fIsDefault;
    ShowLabel := TIVStyle(Source).fShowLabel;
    ShowValue := TIVStyle(Source).fShowValue;
    HideCursor := TIVStyle(Source).fHideCursor;
    DrawFrame := TIVStyle(Source).fDrawFrame;
    DrawShadows := TIVStyle(Source).fDrawShadows;
    Emboss := TIVStyle(Source).fEmboss;
    Roundness := TIVStyle(Source).fRoundness;
    FrameThickness := TIVStyle(Source).fFrameThickness;
    ShadowOffset := TIVStyle(Source).fShadowOffset;
    WidgetFrac := TIVStyle(Source).fWidgetFrac;
    Angle := TIVStyle(Source).fAngle;
    Name := TIVStyle(Source).fName;
  end
  else
    inherited Assign(Source);
end;

procedure TIVStyle.GetCodeStr(Lst: TStrings);
begin
  Lst.Add(Format(#9'const IVStyle %s {', [Name]));
  Lst.Add(Format(#9#9'%s, // Show label', [BooleanToStr(ShowLabel)]));
  Lst.Add(Format(#9#9'%s, // Show value', [BooleanToStr(ShowValue)]));
  Lst.Add(#9#9'{');
  Lst.Add(Format(#9#9#9'%s, // Background', [IColorStr(Colors[kBG])]));
  Lst.Add(Format(#9#9#9'%s, // Foreground', [IColorStr(Colors[kFG])]));
  Lst.Add(Format(#9#9#9'%s, // Pressed', [IColorStr(Colors[kPR])]));
  Lst.Add(Format(#9#9#9'%s, // Frame', [IColorStr(Colors[kFR])]));
  Lst.Add(Format(#9#9#9'%s, // Highlight', [IColorStr(Colors[kHL])]));
  Lst.Add(Format(#9#9#9'%s, // Shadow', [IColorStr(Colors[kSH])]));
  Lst.Add(Format(#9#9#9'%s, // Extra 1', [IColorStr(Colors[kX1])]));
  Lst.Add(Format(#9#9#9'%s, // Extra 2', [IColorStr(Colors[kX2])]));
  Lst.Add(Format(#9#9#9'%s  // Extra 3', [IColorStr(Colors[kX3])]));
  Lst.Add(#9#9'}, // Colors ');
  Lst.Add(Format(#9#9'%s, // Label text', [ITextStr(LabelFont)]));
  Lst.Add(Format(#9#9'%s, // Value text', [ITextStr(ValueFont)]));
  Lst.Add(Format(#9#9'%s, // Hide cursor', [BooleanToStr(HideCursor)]));
  Lst.Add(Format(#9#9'%s, // Draw frame', [BooleanToStr(DrawFrame)]));
  Lst.Add(Format(#9#9'%s, // Draw shadows', [BooleanToStr(DrawShadows)]));
  Lst.Add(Format(#9#9'%s, // Emboss', [BooleanToStr(Emboss)]));
  Lst.Add(FormatFloat(#9#9'0.0#", // Roundness"', Roundness));
  Lst.Add(FormatFloat(#9#9'0.0#", // Frame thickness"', FrameThickness));
  Lst.Add(FormatFloat(#9#9'0.0#", // Shadow offset"', ShadowOffset));
  Lst.Add(FormatFloat(#9#9'0.0#", // Widget frac"', WidgetFrac));
  Lst.Add(FormatFloat(#9#9'0.0#"  // Angle"', Angle));
  Lst.Add(#9'};');
  Lst.Add('');
end;

procedure TIVStyle.LoadFromFile(const AFileName: string);
var
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(AFileName);
  try
    fName := ini.ReadString('Style', 'Name', DEFAULT_STYLE_NAME);
    fAngle := ini.ReadFloat('Properties', 'Angle', DEFAULT_WIDGET_ANGLE);
    fFrameThickness := ini.ReadFloat('Properties', 'FrameThickness', DEFAULT_FRAME_THICKNESS);
    fRoundness := ini.ReadFloat('Properties', 'Roundness', DEFAULT_ROUNDNESS);
    fShadowOffset := ini.ReadFloat('Properties', 'ShadowOffset', DEFAULT_SHADOW_OFFSET);
    fWidgetFrac := ini.ReadFloat('Properties', 'WidgetFrac', DEFAULT_WIDGET_FRAC);
    fDrawFrame := ini.ReadBool('Properties', 'DrawFrame', DEFAULT_DRAW_FRAME);
    fDrawShadows := ini.ReadBool('Properties', 'DrawShadows', DEFAULT_DRAW_SHADOWS);
    fEmboss := ini.ReadBool('Properties', 'Emboss', DEFAULT_EMBOSS);
    fHideCursor := ini.ReadBool('Properties', 'HideCursor', DEFAULT_HIDE_CURSOR);
    fShowLabel := ini.ReadBool('Properties', 'ShowLabel', DEFAULT_SHOW_LABEL);
    fShowValue := ini.ReadBool('Properties', 'ShowValue', DEFAULT_SHOW_VALUE);
    fColors.LoadFromIni(ini);
    fLabelFont.LoadFromIni(ini, 'LabelFont');
    fValueFont.LoadFromIni(ini, 'ValueFont');
  finally
    ini.Free;
  end;
end;

procedure TIVStyle.SaveToFile(const AFileName: string);
var
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(AFileName);
  try
    ini.WriteString('Style', 'Name', fName);
    ini.WriteFloat('Properties', 'Angle', fAngle);
    ini.WriteFloat('Properties', 'FrameThickness', fFrameThickness);
    ini.WriteFloat('Properties', 'Roundness', fRoundness);
    ini.WriteFloat('Properties', 'ShadowOffset', fShadowOffset);
    ini.WriteFloat('Properties', 'WidgetFrac', fWidgetFrac);
    ini.WriteBool('Properties', 'DrawFrame', fDrawFrame);
    ini.WriteBool('Properties', 'DrawShadows', fDrawShadows);
    ini.WriteBool('Properties', 'Emboss', fEmboss);
    ini.WriteBool('Properties', 'HideCursor', fHideCursor);
    ini.WriteBool('Properties', 'ShowLabel', fShowLabel);
    ini.WriteBool('Properties', 'ShowValue', fShowValue);
    fColors.SaveToIni(ini);
    fLabelFont.SaveToIni(ini, 'LabelFont');
    fValueFont.SaveToIni(ini, 'ValueFont');
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TIVStyle.ParamChanged(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Sender);
end;

procedure TIVStyle.SetName(AValue: TStyleName);
begin
  if fName = AValue then
    exit;
  Changing;
  fName := AValue;
  Changed;
end;

procedure TIVStyle.SetHideCursor(AValue: boolean);
begin
  if fHideCursor = AValue then
    exit;
  Changing;
  fHideCursor := AValue;
  Changed;
end;

procedure TIVStyle.SetShowLabel(AValue: boolean);
begin
  if fShowLabel = AValue then
    exit;
  Changing;
  fShowLabel := AValue;
  Changed;
end;

procedure TIVStyle.SetShowValue(AValue: boolean);
begin
  if fShowValue = AValue then
    exit;
  Changing;
  fShowValue := AValue;
  Changed;
end;

procedure TIVStyle.SetDrawFrame(AValue: boolean);
begin
  if fDrawFrame = AValue then
    exit;
  Changing;
  fDrawFrame := AValue;
  Changed;
end;

procedure TIVStyle.SetDrawShadows(AValue: boolean);
begin
  if fDrawShadows = AValue then
    exit;
  Changing;
  fDrawShadows := AValue;
  Changed;
end;

procedure TIVStyle.SetEmboss(AValue: boolean);
begin
  if fEmboss = AValue then
    exit;
  Changing;
  fEmboss := AValue;
  Changed;
end;

procedure TIVStyle.SetRoundness(AValue: double);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  if fRoundness = AValue then
    exit;
  Changing;
  fRoundness := AValue;
  Changed;
end;

procedure TIVStyle.SetFrameThickness(AValue: double);
begin
  if fFrameThickness = AValue then
    exit;
  Changing;
  fFrameThickness := AValue;
  Changed;
end;

procedure TIVStyle.SetShadowOffset(AValue: double);
begin
  if fShadowOffset = AValue then
    exit;
  Changing;
  fShadowOffset := AValue;
  Changed;
end;

procedure TIVStyle.SetWidgetFrac(AValue: double);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  if fWidgetFrac = AValue then
    exit;
  Changing;
  fWidgetFrac := AValue;
  Changed;
end;

procedure TIVStyle.SetAngle(AValue: double);
begin
  AValue := EnsureRange(AValue, 0.0, 360.0);
  if fAngle = AValue then
    exit;
  Changing;
  fAngle := AValue;
  Changed;
end;


{ TIVStyleCache }

constructor TIVStyleCache.Create(AOwner: TObject);
begin
  fOwner := AOwner;
  fStyles := TObjectList.Create(True);
  fStyles.FPOAttachObserver(fOwner);
  fDefaultStyle := TIVStyle.Create;
  fDefaultStyle.IsDefault := True;
  fStyles.Add(fDefaultStyle);
end;

destructor TIVStyleCache.Destroy;
begin
  fStyles.FPODetachObserver(fOwner);
  FreeAndNil(fStyles);
  inherited Destroy;
end;

procedure TIVStyleCache.Clear;
begin
  fStyles.Clear;
end;

procedure TIVStyleCache.Refresh;
begin
  fDefaultStyle.Assign(StyleByName[DEFAULT_STYLE_NAME]);
end;

function TIVStyleCache.GetStyle(AIndex: integer): TIVStyle;
begin
  if InRange(AIndex, 0, fStyles.Count - 1) then
    Result := TIVStyle(fStyles[AIndex])
  else
    Result := nil;
end;

function TIVStyleCache.GetStyleIndex(AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to StylesCount - 1 do
    if SameText(Styles[i].Name, AName) then
    begin
      Result := i;
      Break;
    end;
end;

function TIVStyleCache.GetStyleByName(AName: string): TIVStyle;
var
  Index: integer;
begin
  Index := StyleIndex[AName];
  if Index > -1 then
    Result := Styles[Index]
  else
    raise EIException.CreateResFmt(@SStyleNotFound, [AName]);
end;

function TIVStyleCache.StyleExists(AName: string): boolean;
begin
  Result := StyleIndex[AName] > -1;
end;

function TIVStyleCache.GetStylesCount: integer;
begin
  Result := fStyles.Count;
end;

procedure TIVStyleCache.AddStyle(aStyle: TIVStyle);
var
  s: TIVStyle;
begin
  s := TIVStyle.Create;
  s.Assign(aStyle);
  fStyles.Add(s);
end;

procedure TIVStyleCache.CustomSerialize(const Node: ITreeNode);
var
  i: integer;
  NewNode, ChildNode: ITreeNode;
begin
  if StylesCount > 0 then
  begin
    NewNode := Node.AddNode('Styles');
    for i := 0 to StylesCount - 1 do
    begin
      ChildNode := NewNode.AddNode('Style');
      TISerializer.SaveToNode(Styles[i], ChildNode);
    end;
  end;
end;

procedure TIVStyleCache.CustomDeSerialize(const Node: ITreeNode);
var
  i: integer;
  NewNode: ITreeNode;
  NewStyle: TIVStyle;
begin
  Clear;
  if Node.NodeIndex['Styles'] < 0 then
    exit;
  NewNode := Node.NodeByName['Styles'];
  for i := 0 to NewNode.NodesCount - 1 do
  begin
    NewStyle := TIVStyle.Create;
    TISerializer.LoadFromNode(NewStyle, NewNode.Nodes[i]);
    AddStyle(NewStyle);
  end;
end;


{ TIVectorBase }

constructor TIVectorBase.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fActionFunction := 'SplashClickActionFunc';
  fGearing := DEFAULT_GEARING;
  fValue := 0;
  fMinValue := 0;
  fMaxValue := 1;
  fStyle := nil;
  if Assigned(ADocument) then
    fStyle := TIDocument(ADocument).StyleCache.DefaultStyle;
  fShape := TEVShape.Rectangle;
  fLabelInWidget := False;
  fValueInWidget := False;
  fTrackSize := 2.0;
  fValueDisplayFrac := 0.66;
  fNumStates := 2;
  fDirection := Horizontal;
end;

procedure TIVectorBase.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawWidget(Canvas);
  DrawLabel(Canvas);
  DrawValue(Canvas);
end;

procedure TIVectorBase.SetStyleName(AValue: TStyleName);
begin
  if fStyle.Name = AValue then
    exit;
  DoChanging(Self);
  try
    fStyle := TIDocument(Document).StyleCache.StyleByName[AValue];
  except
    fStyle := TIDocument(Document).StyleCache.DefaultStyle;
  end;
  DoChanged(Self);
end;

procedure TIVectorBase.SetActionFunction(AValue: string);
begin
  if fActionFunction = AValue then
    exit;
  DoChanging(Self);
  fActionFunction := AValue;
  DoChanged(Self);
end;

procedure TIVectorBase.SetDirection(AValue: TDirection);
begin
  if fDirection = AValue then
    exit;
  DoChanging(Self);
  fDirection := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVectorBase.SetGearing(AValue: string);
begin
  if fGearing = AValue then
    exit;
  DoChanging(Self);
  if AValue.IsEmpty then
    fGearing := DEFAULT_GEARING
  else
    fGearing := AValue;
  DoChanged(Self);
end;

procedure TIVectorBase.SetLabelText(AValue: string);
begin
  if fLabelText = AValue then
    exit;
  DoChanging(Self);
  fLabelText := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVectorBase.SetMaxValue(AValue: double);
begin
  if fMaxValue = AValue then
    exit;
  fMaxValue := Max(fMinValue, fMaxValue);
  DoChanging(Self);
  fMaxValue := AValue;
  fValue := EnsureRange(fValue, fMinValue, fMaxValue);
  DoChanged(Self);
end;

procedure TIVectorBase.SetMinValue(AValue: double);
begin
  if fMinValue = AValue then
    exit;
  fMinValue := Min(fMinValue, fMaxValue);
  DoChanging(Self);
  fMinValue := AValue;
  fValue := EnsureRange(fValue, fMinValue, fMaxValue);
  DoChanged(Self);
end;

procedure TIVectorBase.SetNumStates(AValue: integer);
begin
  AValue := Max(AValue, 2);
  if fNumStates = AValue then
    exit;
  DoChanging(Self);
  fNumStates := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVectorBase.SetShape(AValue: TEVShape);
begin
  if fShape = AValue then
    exit;
  DoChanging(Self);
  fShape := AValue;
  DoChanged(Self);
end;

procedure TIVectorBase.SetTrackSize(AValue: double);
begin
  if fTrackSize = AValue then
    exit;
  DoChanging(Self);
  fTrackSize := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVectorBase.SetValue(AValue: double);
begin
  AValue := EnsureRange(AValue, fMinValue, fMaxValue);
  if fValue = AValue then
    Exit;
  DoChanging(Self);
  fValue := AValue;
  fValueText := FormatFloat('0.0#', fValue);
  Refresh;
  DoChanged(Self);
end;

procedure TIVectorBase.SetValueIsEditable(AValue: boolean);
begin
  if fValueIsEditable = AValue then
    Exit;
  DoChanging(Self);
  fValueIsEditable := AValue;
  DoChanged(Self);
end;

function TIVectorBase.GetStyleName: TStyleName;
begin
  Result := fStyle.Name;
end;

function TIVectorBase.GetColor(const AIndex: TEVColor): TIColor;
begin
  Result := Style.Colors[AIndex];
end;

function TIVectorBase.GetAdjustedHandleBounds(const ARect: TIFloatRect): TIFloatRect;
begin
  Result := ARect;
  with Style do
  begin
    if DrawFrame then
      Result.Inflate(-0.5 * FrameThickness);
    if DrawShadows then
      Result.Alter(ShadowOffset, 0, -ShadowOffset, -ShadowOffset);
  end;
end;

function TIVectorBase.GetRoundedCornerRadius(const ARect: TIFloatRect): double;
begin
  if ARect.Width < ARect.Height then
    Result := Style.Roundness * (ARect.Width / 2.0)
  else
    Result := Style.Roundness * (ARect.Height / 2.0);
end;

function TIVectorBase.MakeRects(const ARect: TIFloatRect;
  AHasHandle: boolean): TIFloatRect;
var
  c: TAggCanvas;
  w, h, vdw: double;
begin
  Result := ARect;
  if Style = nil then
    exit;
  if Assigned(Document) then
    c := TIDocument(Document).Canvas
  else
    if self is TIVTestControl then
      c := (self as TIVTestControl).fCanvas.AggCanvas
    else
      exit;
  if not fLabelInWidget then
  begin
    if Style.ShowLabel and Assigned(c) and (fLabelText.Length > 0) then
    begin
      c.SetupFont(Style.LabelFont);
      h := Style.LabelFont.Size;
      w := c.AggTextWidth(fLabelText);
      fLabelBounds := ARect.GetFromTop(h).GetCentredInside(w, h);
    end
    else
      fLabelBounds := TIFloatRect.Empty;
    if fLabelBounds.Height > 0 then
      Result := ARect.GetReducedFromTop(fLabelBounds.Height);
  end;

  if Style.ShowValue and Assigned(c) and (not fValueInWidget) then
  begin
    h := 0;
    w := 0;
    if fValueText.Length > 0 then
    begin
      c.SetupFont(Style.ValueFont);
      h := Style.ValueFont.Size;
      w := c.AggTextWidth(fValueText);
    end;
    vdw := w * fValueDisplayFrac;

    case Style.ValueFont.VAlign of
      TITextVAlign.Middle:
      begin
        fValueBounds := Result.GetMidVPadded(h / 2.0).GetMidHPadded(vdw);
        fWidgetBounds := Result.GetScaledAboutCentre(Style.WidgetFrac);
      end;
      TITextVAlign.Bottom:
      begin
        fValueBounds := Result.GetFromBottom(h).GetMidHPadded(vdw);
        fWidgetBounds := Result.GetReducedFromBottom(h).GetScaledAboutCentre(
          Style.WidgetFrac);
      end;
      TITextVAlign.Top:
      begin
        fValueBounds := Result.GetFromTop(h).GetMidHPadded(vdw);
        fWidgetBounds := Result.GetReducedFromTop(h).GetScaledAboutCentre(
          Style.WidgetFrac);
      end;
    end;
  end
  else
    fWidgetBounds := Result.GetScaledAboutCentre(Style.WidgetFrac);

  if AHasHandle then
    fWidgetBounds := GetAdjustedHandleBounds(Result).GetScaledAboutCentre(
      Style.WidgetFrac);

  if fLabelInWidget then
    fLabelBounds := fWidgetBounds;

  if fValueInWidget then
    fValueBounds := fWidgetBounds;
end;

procedure TIVectorBase.Resize;
begin
  if osCreating in State then
    exit;
  MakeRects(GetRect);
end;

procedure TIVectorBase.DrawBackGround(Canvas: TICanvas; const ARect: TIFloatRect);
begin
  Canvas.PenWidth := 0;
  Canvas.BrushColor := GetColor(kBG);
  Canvas.DrawRect(ARect);
end;

procedure TIVectorBase.DrawLabel(Canvas: TICanvas);
begin
  if (fLabelBounds.Height = 0) or (not Style.ShowLabel) then
    exit;
  Canvas.GraphicsSettings.Font := Style.LabelFont;
  Canvas.DrawText(fLabelText, fLabelBounds);
end;

procedure TIVectorBase.DrawValue(Canvas: TICanvas);
begin
  if not Style.ShowValue then
    exit;
  Canvas.GraphicsSettings.Font := Style.ValueFont;
  Canvas.DrawText(fValueText, fValueBounds);
end;

procedure TIVectorBase.DrawWidget(Canvas: TICanvas);
begin
  // nothing
end;

procedure TIVectorBase.DrawPressableShape(Canvas: TICanvas; AShape: TEVShape;
  ARect: TIFloatRect; Pressed: boolean);
begin
  case AShape of
    Rectangle: DrawPressableRectangle(Canvas, ARect, Pressed);
    Ellipse: DrawPressableEllipse(Canvas, ARect, Pressed);
    Triangle: DrawPressableTriangle(Canvas, ARect, Pressed, Style.Angle);
    EndsRounded: DrawPressableRectangle(Canvas, ARect, Pressed, True,
        True, False, False);
    AllRounded: DrawPressableRectangle(Canvas, ARect, Pressed, True, True, True, True);
  end;
end;

procedure TIVectorBase.DrawPressableRectangle(Canvas: TICanvas;
  ARect: TIFloatRect; Pressed: boolean; rtl: boolean; rtr: boolean;
  rbl: boolean; rbr: boolean);
var
  RHandle, RCentre, RShadow: TIFloatRect;
  cR, tlr, trr, blr, brr: double;
begin
  RHandle := GetAdjustedHandleBounds(ARect);
  RCentre := RHandle.GetPadded(-Style.ShadowOffset);
  RShadow := RHandle.GetTranslated(Style.ShadowOffset);
  cr := GetRoundedCornerRadius(RHandle);
  tlr := IfThen(rtl, cR);
  trr := IfThen(rtr, cR);
  blr := IfThen(rbl, cR);
  brr := IfThen(rbr, cR);
  Canvas.PenWidth := 0;
  if Pressed then
  begin
    RShadow.ReduceFromRight(Style.ShadowOffset);
    RShadow.ReduceFromBottom(Style.ShadowOffset);
    if Style.Emboss then
    begin
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
      Canvas.BrushColor := GetColor(kSH);
      Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
      Canvas.BrushColor := GetColor(kFG);
      Canvas.DrawRoundedRect(RShadow, tlr, trr, blr, brr);
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawRoundedRect(RCentre, tlr, trr, blr, brr);
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
        Canvas.BrushColor := 0;
        Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
      end;
    end
    else
    begin
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
      end;
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
    end;
  end
  else
  begin
    if Style.DrawShadows then
    begin
      Canvas.BrushColor := GetColor(kSH);
      Canvas.DrawRoundedRect(RShadow, tlr, trr, blr, brr);
    end;
    if Style.Emboss then
    begin
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
      Canvas.BrushColor := GetColor(kSH);
      Canvas.DrawRoundedRect(RShadow, tlr, trr, blr, brr);
      Canvas.BrushColor := GetColor(kFG);
      Canvas.DrawRoundedRect(RCentre, tlr, trr, blr, brr);
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
        Canvas.BrushColor := 0;
        Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
      end;
    end
    else
    begin
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
      end;
      Canvas.BrushColor := GetColor(kFG);
      Canvas.DrawRoundedRect(RHandle, tlr, trr, blr, brr);
    end;
  end;
end;

procedure TIVectorBase.DrawPressableEllipse(Canvas: TICanvas;
  ARect: TIFloatRect; Pressed: boolean);
var
  RCentre, RShadow: TIFloatRect;
begin
  RCentre := ARect.GetPadded(-Style.ShadowOffset);
  RShadow := ARect.GetTranslated(Style.ShadowOffset);
  Canvas.PenWidth := 0;
  if Style.DrawShadows and (not Pressed) then
  begin
    Canvas.BrushColor := GetColor(kSH);
    Canvas.DrawEllipse(RShadow);
  end;
  if Pressed then
  begin
    RShadow.ReduceFromRight(Style.ShadowOffset);
    RShadow.ReduceFromBottom(Style.ShadowOffset);
    if Style.Emboss then
    begin
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawEllipse(ARect);
      Canvas.BrushColor := GetColor(kSH);
      Canvas.DrawEllipse(ARect);
      Canvas.BrushColor := GetColor(kFG);
      Canvas.DrawEllipse(RShadow);
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawEllipse(RCentre);
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
        Canvas.BrushColor := 0;
        Canvas.DrawEllipse(ARect);
      end;
    end
    else
    begin
      Canvas.BrushColor := GetColor(kPR);
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
      end;
      Canvas.DrawEllipse(ARect);
    end;
  end
  else
  begin
    if Style.Emboss then
    begin
      Canvas.BrushColor := GetColor(kPR);
      Canvas.DrawEllipse(ARect);
      Canvas.BrushColor := GetColor(kSH);
      Canvas.DrawEllipse(RShadow);
      Canvas.BrushColor := GetColor(kFG);
      Canvas.DrawEllipse(RCentre);
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
        Canvas.BrushColor := 0;
        Canvas.DrawEllipse(ARect);
      end;
    end
    else
    begin
      Canvas.BrushColor := GetColor(kFG);
      if Style.DrawFrame then
      begin
        Canvas.PenColor := GetColor(kFR);
        Canvas.PenWidth := Style.FrameThickness;
      end;
      Canvas.DrawEllipse(ARect);
    end;

  end;
end;

procedure TIVectorBase.DrawPressableTriangle(Canvas: TICanvas;
  ARect: TIFloatRect; Pressed: boolean; Angle: double);
var
  i: integer;
  theta, s, c: double;
  xT, yT: double;
  RHandle, RCentre: TIFloatRect;
  a, b: array[0..2] of TIFloatPoint;
begin
  Canvas.PenWidth := 0;
  theta := DegToRad(Angle);
  RHandle := GetAdjustedHandleBounds(ARect);
  xT := RHandle.MW;
  yT := RHandle.MH;
  RCentre := RHandle.GetTranslated(-xT, -yT);
  SinCos(theta, s, c);
  a[0].x := RCentre.Left * c - RCentre.Bottom * s + xT;
  a[0].y := RCentre.Left * s + RCentre.Bottom * c + yT;
  a[1].x := RCentre.MW * c - RCentre.Top * s + xT;
  a[1].y := RCentre.MW * s + RCentre.Top * c + yT;
  a[2].x := RCentre.Right * c - RCentre.Bottom * s + xT;
  a[2].y := RCentre.Right * s + RCentre.Bottom * c + yT;
  if Pressed then
  begin
    Canvas.BrushColor := GetColor(kPR);
    if Style.DrawFrame then
    begin
      Canvas.PenColor := GetColor(kFR);
      Canvas.PenWidth := Style.FrameThickness;
    end;
    Canvas.DrawPolygon(@a[0], 3);
  end
  else
  begin
    if Style.DrawShadows then
    begin
      for i := 0 to 2 do
      begin
        b[i].x := a[i].x + Style.ShadowOffset;
        b[i].y := a[i].y + Style.ShadowOffset;
      end;
      Canvas.BrushColor := GetColor(kSH);
      Canvas.DrawPolygon(@b[0], 3);
    end;
    Canvas.BrushColor := GetColor(kFG);
    if Style.DrawFrame then
    begin
      Canvas.PenColor := GetColor(kFR);
      Canvas.PenWidth := Style.FrameThickness;
    end;
    Canvas.DrawPolygon(@a[0], 3);
  end;
end;


{ TIVLabelControl }

constructor TIVLabelControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fValueText := 'Text';
end;

procedure TIVLabelControl.SetValueText(AValue: string);
begin
  if fLabelText = AValue then
    exit;
  DoChanging(Self);
  fValueText := AValue;
  DoChanged(Self);
end;

procedure TIVLabelControl.Paint(Canvas: TICanvas);
var
  r: TIFloatRect;
begin
  r := GetRect;
  GraphicsSettings.Font := Style.ValueFont;
  DrawBackGround(Canvas, r);
  if fValueText <> '' then
  begin
    if Style.DrawShadows then
    begin
      Canvas.FontColor := GetColor(kSH);
      Canvas.DrawText(fValueText, r.GetTranslated(Style.ShadowOffset));
    end;
    Canvas.FontColor := Style.ValueFont.Color;
    Canvas.DrawText(fValueText, r);
  end;
  if Style.DrawFrame then
  begin
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.PenColor := GetColor(kFR);
    Canvas.BrushColor := 0;
    Canvas.DrawRect(r);
  end;
end;

class function TIVLabelControl.GetObjectName: string;
begin
  Result := 'IVLabelControl';
end;

function TIVLabelControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, "%s", %s)', [GetObjectName, IRect(GetRect),
    fValueText, Style.Name]);
end;


{ TIVButtonBaseControl }

constructor TIVButtonBaseControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fLabelInWidget := False;
  fValueInWidget := False;
  fLabelText := 'Text';
  SetBounds(0, 0, 75, 25);
end;

procedure TIVButtonBaseControl.SetLabelInWidget(AValue: boolean);
begin
  if fLabelInWidget = AValue then
    exit;
  DoChanging(Self);
  fLabelInWidget := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVButtonBaseControl.SetValueInWidget(AValue: boolean);
begin
  if fValueInWidget = AValue then
    exit;
  DoChanging(Self);
  fValueInWidget := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVButtonBaseControl.SetValueText(AValue: string);
begin
  if fValueText = AValue then
    exit;
  DoChanging(Self);
  fValueText := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVButtonBaseControl.DrawWidget(Canvas: TICanvas);
begin
  DrawPressableShape(Canvas, fShape, fWidgetBounds);
end;

procedure TIVButtonBaseControl.DrawValue(Canvas: TICanvas);
begin
  if not Style.ShowValue then
    exit;
  Canvas.GraphicsSettings.Font := Style.ValueFont;
  if fValueInWidget then
    Canvas.GraphicsSettings.Font.VAlign := Middle;
  Canvas.DrawText(fValueText, fValueBounds);
end;


{ TIVButtonControl }

constructor TIVButtonControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fLabelInWidget := True;
  fValueInWidget := True;
  fValueText := '';
end;

class function TIVButtonControl.GetObjectName: string;
begin
  Result := 'IVButtonControl';
end;

function TIVButtonControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, "%s", %s, %s, %s, EVShape::%s)',
    [GetObjectName, IRect(GetRect), ActionFunction, &Label,
    Style.Name, BooleanToStr(fLabelInWidget), BooleanToStr(fValueInWidget),
    GetEnumName(TypeInfo(TEVShape), Ord(Shape))]);
end;

{ TIVSwitchControl }

constructor TIVSwitchControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fValueInWidget := True;
  fActionFunction := '';
  fLabelText := '';
  fValueText := 'Value';
end;

class function TIVSwitchControl.GetObjectName: string;
begin
  Result := 'IVSwitchControl';
end;

function TIVSwitchControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format('%s(%s, %s, "%s", %s, %s)', [GetObjectName,
      IRect(GetRect), ParamIdx, &Label, Style.Name, BooleanToStr(fValueInWidget)])
  else
    Result := Format('%s(%s, %s, "%s", %s, %d, %s)',
      [GetObjectName, IRect(GetRect), ActionFunction, &Label,
      Style.Name, NumStates, BooleanToStr(fValueInWidget)]);
end;


{ TIVToggleControl }

constructor TIVToggleControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fInitialState := False;
  fValueInWidget := True;
  fActionFunction := '';
  fLabelText := '';
  fOnText := 'ON';
  fOffText := 'OFF';
  fValueText := fOffText;
end;

class function TIVToggleControl.GetObjectName: string;
begin
  Result := 'IVToggleControl';
end;

function TIVToggleControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format('%s(%s, %s, "%s", %s, "%s", "%s")',
      [GetObjectName, IRect(GetRect), ParamIdx, &Label, Style.Name,
      OffText, fOnText])
  else
    Result := Format('%s(%s, %s, "%s", %s, "%s", "%s", %s)',
      [GetObjectName, IRect(GetRect), ActionFunction, &Label,
      Style.Name, OffText, fOnText, BooleanToStr(InitialState)]);
end;

procedure TIVToggleControl.SetOffText(AValue: string);
begin
  if fOffText = AValue then
    Exit;
  DoChanging(Self);
  fOffText := AValue;
  DoChanged(Self);
end;

procedure TIVToggleControl.SetOnText(AValue: string);
begin
  if fOnText = AValue then
    Exit;
  DoChanging(Self);
  fOnText := AValue;
  DoChanged(Self);
end;

procedure TIVToggleControl.SetInitialState(AValue: boolean);
begin
  if fInitialState = AValue then
    Exit;
  DoChanging(Self);
  fInitialState := AValue;
  DoChanged(Self);
end;

procedure TIVToggleControl.DrawValue(Canvas: TICanvas);
begin
  if not Style.ShowValue then
    exit;
  Canvas.GraphicsSettings.Font := Style.ValueFont;
  if fValueInWidget then
    Canvas.GraphicsSettings.Font.VAlign := Middle;
  Canvas.DrawText(IfThen(fInitialState, OnText, OffText), fValueBounds);
end;

{ TIVSlideSwitchControl }

constructor TIVSlideSwitchControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fActionFunction := 'EmptyClickActionFunc';
  fValueInWidget := True;
  fValueText := '';
  fInitialState := 0;
  fLabelText := '';
end;

class function TIVSlideSwitchControl.GetObjectName: string;
begin
  Result := 'IVSlideSwitchControl';
end;

function TIVSlideSwitchControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format('%s(%s, %s, "%s", %s, %s, EDirection::%s)',
      [GetObjectName, IRect(GetRect), ParamIdx, &Label, Style.Name,
      BooleanToStr(fValueInWidget), GetEnumName(TypeInfo(TDirection),
      Ord(Direction))])
  else
    Result := Format('%s(%s, %s, "%s", %s, %s, EDirection::%s, %d, %d)',
      [GetObjectName, IRect(GetRect), ActionFunction, &Label,
      Style.Name, BooleanToStr(fValueInWidget),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)), NumStates, InitialState]);
end;

procedure TIVSlideSwitchControl.SetInitialState(AValue: integer);
begin
  if fInitialState = AValue then
    exit;
  DoChanging(Self);
  fInitialState := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVSlideSwitchControl.Resize;
begin
  if osCreating in State then
    exit;
  inherited Resize;
  fStartRect := fWidgetBounds.SubRect(fDirection, fNumStates,
    fInitialState mod fNumStates);
  fHandleBounds := fStartRect;
  fEndRect := fWidgetBounds.SubRect(fDirection, fNumStates,
    (fInitialState + 1) mod fNumStates);

  if fValueInWidget then
    fValueBounds := fHandleBounds;
end;

procedure TIVSlideSwitchControl.DrawWidget(Canvas: TICanvas);
var
  cR: double;
begin
  cR := GetRoundedCornerRadius(fHandleBounds);
  Canvas.PenWidth := 0;
  Canvas.BrushColor := GetColor(kSH);
  Canvas.DrawRoundedRect(fWidgetBounds, cR);
  DrawPressableShape(Canvas, Shape, fHandleBounds);
end;


{ TIVTabSwitchControl }

constructor TIVTabSwitchControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fValueInWidget := True;
  fActionFunction := '';
  fLabelText := '';
  fTabLabels := TStringList.Create;
  fTabLabels.CommaText := '1,2,3';
  fInitialState := 0;
end;

destructor TIVTabSwitchControl.Destroy;
begin
  FreeAndNil(fTabLabels);
  fButtons := nil;
  inherited Destroy;
end;

procedure TIVTabSwitchControl.SetOptions(AValue: string);
begin
  AValue := NormalizeOptStr(AValue);
  if AValue = fTabLabels.CommaText then
    exit;
  DoChanging(Self);
  fTabLabels.CommaText := AValue;
  Refresh;
  DoChanged(Self);
end;

function TIVTabSwitchControl.GetOptions: string;
begin
  Result := FormatOptStr(fTabLabels.CommaText);
end;

procedure TIVTabSwitchControl.SetInitialState(AValue: integer);
begin
  if fInitialState = AValue then
    Exit;
  DoChanging(Self);
  fInitialState := AValue;
  DoChanged(Self);
end;

procedure TIVTabSwitchControl.Resize;
var
  i: integer;
begin
  if osCreating in State then
    exit;
  inherited Resize;
  if fTabLabels = nil then
    exit;
  SetLength(fButtons, fTabLabels.Count);
  for i := 0 to fTabLabels.Count - 1 do
  begin
    fButtons[i] := fWidgetBounds.SubRect(fDirection, fTabLabels.Count, i);
  end;
end;

class function TIVTabSwitchControl.GetObjectName: string;
begin
  Result := 'IVTabSwitchControl';
end;

function TIVTabSwitchControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format('%s(%s, %s, {%s}, "%s", %s, EVShape::%s, EDirection::%s)',
      [GetObjectName, IRect(GetRect), ParamIdx, FormatOptCode(fTabLabels),
      &Label, Style.Name, GetEnumName(TypeInfo(TEVShape), Ord(Shape)),
      GetEnumName(TypeInfo(TDirection), Ord(Direction))])
  else
    Result := Format('%s(%s, %s, {%s}, "%s", %s, EVShape::%s, EDirection::%s)',
      [GetObjectName, IRect(GetRect), ActionFunction, FormatOptCode(fTabLabels),
      &Label, Style.Name, GetEnumName(TypeInfo(TEVShape), Ord(Shape)),
      GetEnumName(TypeInfo(TDirection), Ord(Direction))]);
end;

procedure TIVTabSwitchControl.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawLabel(Canvas);
  DrawWidget(Canvas);
end;

procedure TIVTabSwitchControl.DrawWidget(Canvas: TICanvas);
var
  i: integer;
  r: TIFloatRect;
  s: string;
  sStart, sEnd: boolean;
begin
  for i := 0 to fTabLabels.Count - 1 do
  begin
    r := fButtons[i];
    sStart := i = 0;
    sEnd := i = fTabLabels.Count - 1;
    DrawButton(Canvas, r, i = fInitialState, sStart, sEnd);
    s := fTabLabels[i];
    Canvas.GraphicsSettings.Font := Style.ValueFont;
    Canvas.FontAlign := Center;
    Canvas.FontVAlign := Middle;
    Canvas.DrawText(s, r);
  end;
end;

procedure TIVTabSwitchControl.DrawButton(Canvas: TICanvas; const R: TIFloatRect;
  Pressed, sStart, sEnd: boolean);
begin
  case Shape of
    EndsRounded:
      if fDirection = Horizontal then
        DrawPressableRectangle(Canvas, R, Pressed, sStart, sEnd)
      else
        DrawPressableRectangle(Canvas, R, Pressed, sStart, False, sEnd, True);
    AllRounded:
      if fDirection = Horizontal then
        DrawPressableRectangle(Canvas, R, Pressed, True, True)
      else
        DrawPressableRectangle(Canvas, R, Pressed, False, True, False, True);
    else
      DrawPressableShape(Canvas, Shape, R, Pressed);
  end;
end;

{ TIVRadioButtonControl }

constructor TIVRadioButtonControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fButtonSize := 10;
  fShape := Ellipse;
  fDirection := Vertical;
  SetBounds(0, 0, 50, 75);
end;

class function TIVRadioButtonControl.GetObjectName: string;
begin
  Result := 'IVRadioButtonControl';
end;

function TIVRadioButtonControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format('%s(%s, %s, {%s}, "%s", %s, EVShape::%s, EDirection::%s, %s)',
      [GetObjectName, IRect(GetRect), ParamIdx, FormatOptCode(fTabLabels),
      &Label, Style.Name, GetEnumName(TypeInfo(TEVShape), Ord(Shape)),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)), DoubleToStr(ButtonSize)])
  else
    Result := Format('%s(%s, %s, {%s}, "%s", %s, EVShape::%s, EDirection::%s, %s)',
      [GetObjectName, IRect(GetRect), ActionFunction, FormatOptCode(fTabLabels),
      &Label, Style.Name, GetEnumName(TypeInfo(TEVShape), Ord(Shape)),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)), DoubleToStr(ButtonSize)]);
end;

procedure TIVRadioButtonControl.SetButtonSize(AValue: double);
begin
  if fButtonSize = AValue then
    Exit;
  DoChanging(Self);
  fButtonSize := AValue;
  DoChanged(Self);
end;

procedure TIVRadioButtonControl.DrawWidget(Canvas: TICanvas);
var
  i: integer;
  r: TIFloatRect;
  a: double;
  s: string;
begin
  a := fButtonSize * 3;
  for i := 0 to fTabLabels.Count - 1 do
  begin
    r := fButtons[i];
    DrawButton(Canvas, r.GetFromLeft(a).GetCentredInside(fButtonSize),
      i = fInitialState, False, False);
    s := fTabLabels[i];
    r := r.GetFromRight(r.Width - a);
    Canvas.FontColor := IfThen(i = fInitialState, GetColor(kON), GetColor(kX1));
    Canvas.GraphicsSettings.Font := Style.ValueFont;
    Canvas.FontAlign := Near;
    Canvas.FontVAlign := Middle;
    Canvas.DrawText(s, r);
  end;
end;


{ TIVKnobControl }

constructor TIVKnobControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fActionFunction := '';
  fValueText := '0.0';
  fTrackToHandleDistance := 4.0;
  fInnerPointerFrac := 0.1;
  fOuterPointerFrac := 1.0;
  fPointerThickness := 2.5;
  fTrackSize := 2.0;
  fAngle1 := -135.0;
  fAngle2 := 135.0;
  fAnchorAngle := -135.0;
  fShape := Ellipse;
  fValueIsEditable := False;
  fValueInWidget := False;
  fValueText := '0.0';
  SetBounds(0, 0, 50, 50);
end;

class function TIVKnobControl.GetObjectName: string;
begin
  Result := 'IVKnobControl';
end;

procedure TIVKnobControl.SetValueInWidget(AValue: boolean);
begin
  if fValueInWidget = AValue then
    Exit;
  DoChanging(Self);
  fValueInWidget := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVKnobControl.SetInnerPointerFrac(AValue: double);
begin
  if fInnerPointerFrac = AValue then
    Exit;
  DoChanging(Self);
  fInnerPointerFrac := AValue;
  DoChanged(Self);
end;

procedure TIVKnobControl.SetAnchorAngle(AValue: double);
begin
  if fAnchorAngle = AValue then
    Exit;
  DoChanging(Self);
  fAnchorAngle := AValue;
  DoChanged(Self);
end;

procedure TIVKnobControl.SetAngle1(AValue: double);
begin
  if fAngle1 = AValue then
    Exit;
  DoChanging(Self);
  fAngle1 := AValue;
  DoChanged(Self);
end;

procedure TIVKnobControl.SetAngle2(AValue: double);
begin
  if fAngle2 = AValue then
    Exit;
  DoChanging(Self);
  fAngle2 := AValue;
  DoChanged(Self);
end;

procedure TIVKnobControl.SetOuterPointerFrac(AValue: double);
begin
  if fOuterPointerFrac = AValue then
    Exit;
  DoChanging(Self);
  fOuterPointerFrac := AValue;
  DoChanged(Self);
end;

procedure TIVKnobControl.SetPointerThickness(AValue: double);
begin
  if fPointerThickness = AValue then
    Exit;
  DoChanging(Self);
  fPointerThickness := AValue;
  DoChanged(Self);
end;

procedure TIVKnobControl.DrawWidget(Canvas: TICanvas);
var
  wr: double; // The radius out to the indicator track arc
  cx, cy, a: double;
  hb: TIFloatRect;
begin
  if fWidgetBounds.Width > fWidgetBounds.Height then
    wr := fWidgetBounds.Height / 2.0
  else
    wr := fWidgetBounds.Width / 2.0;
  cx := fWidgetBounds.MW;
  cy := fWidgetBounds.MH;
  wr -= fTrackSize / 2.0;
  hb := fWidgetBounds.GetCentredInside((wr - fTrackToHandleDistance) * 2.0);
  a := fAngle1 + fValue * (fAngle2 - fAngle1);
  DrawIndicatorTrack(Canvas, a, cx, cy, wr);
  DrawPressableShape(Canvas, fShape, hb);
  DrawPointer(Canvas, a, cx, cy, hb.Width / 2.0);
end;

procedure TIVKnobControl.DrawIndicatorTrack(Canvas: TICanvas;
  angle, cx, cy, radius: double);
var
  a1, a2: double;
begin
  Canvas.PenColor := GetColor(kX1);
  Canvas.PenWidth := fTrackSize;
  a1 := IfThen(angle >= fAnchorAngle, fAnchorAngle, fAnchorAngle -
    (fAnchorAngle - angle));
  a2 := IfThen(angle >= fAnchorAngle, angle, fAnchorAngle);
  Canvas.DrawArc(cx, cy, radius, a1, a2);
end;

procedure TIVKnobControl.DrawPointer(Canvas: TICanvas; angle, cx, cy, radius: double);
begin
  Canvas.PenColor := GetColor(kFR);
  Canvas.PenWidth := fPointerThickness;
  Canvas.DrawRadialLine(cx, cy, angle, fInnerPointerFrac * radius,
    fOuterPointerFrac * radius);
end;

function TIVKnobControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format(
      '%s(%s, %s, "%s", %s, %s, %s, %s, %s, %s, EDirection::%s, %s, %s)',
      [GetObjectName, IRect(GetRect), ParamIdx, &Label, Style.Name,
      BooleanToStr(ValueIsEditable), BooleanToStr(ValueInWidget),
      DoubleToStr(Angle1), DoubleToStr(Angle2), DoubleToStr(AnchorAngle),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)), Gearing, DoubleToStr(TrackSize)])
  else
    Result := Format(
      '%s(%s, %s, "%s", %s, %s, %s, %s, %s, %s, EDirection::%s, %s, %s)',
      [GetObjectName, IRect(GetRect), ActionFunction, &Label,
      Style.Name, BooleanToStr(ValueIsEditable), BooleanToStr(ValueInWidget),
      DoubleToStr(Angle1), DoubleToStr(Angle2), DoubleToStr(AnchorAngle),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)), Gearing, DoubleToStr(TrackSize)]);
end;

{ TIVSliderControl }

constructor TIVSliderControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fActionFunction := '';
  fValueText := '0.0';
  fShape := Ellipse;
  fHandleSize := 8;
  fDirection := Vertical;
  fHandleInsideTrack := False;
  SetBounds(0, 0, 50, 100);
end;

class function TIVSliderControl.GetObjectName: string;
begin
  Result := 'IVSliderControl';
end;

procedure TIVSliderControl.SetHandleSize(AValue: double);
begin
  if fHandleSize = AValue then
    Exit;
  DoChanging(Self);
  fHandleSize := AValue;
  DoChanged(Self);
end;

procedure TIVSliderControl.SetHandleInsideTrack(AValue: boolean);
begin
  if fHandleInsideTrack = AValue then
    Exit;
  DoChanging(Self);
  fHandleInsideTrack := AValue;
  DoChanged(Self);
end;

procedure TIVSliderControl.Resize;
begin
  if osCreating in State then
    exit;
  inherited Resize;
  if Direction = Vertical then
    fTrackBounds := fWidgetBounds.GetPadded(-fHandleSize).GetMidHPadded(fTrackSize)
  else
    fTrackBounds := fWidgetBounds.GetPadded(-fHandleSize).GetMidVPadded(fTrackSize);
end;

procedure TIVSliderControl.DrawWidget(Canvas: TICanvas);
var
  FilledTrack: TIFloatRect;
  cx, cy, offset: double;
begin
  FilledTrack := fTrackBounds.FracRect(fDirection, Value);
  if TrackSize > 0 then
    DrawTrack(Canvas, FilledTrack);
  offset := IfThen(Style.DrawShadows and (Shape <> Ellipse), Style.ShadowOffset * 0.5);

  if Direction = Vertical then
  begin
    cx := FilledTrack.MW + offset;
    cy := FilledTrack.Top;
  end
  else
  begin
    cx := FilledTrack.Right;
    cy := FilledTrack.MH + offset;
  end;
  if HandleSize > 0 then
    DrawHandle(Canvas, TIFloatRect.Create(cx - HandleSize, cy - HandleSize,
      cx + HandleSize, cy + HandleSize));
end;

procedure TIVSliderControl.DrawTrack(Canvas: TICanvas; FilledArea: TIFloatRect);
var
  extra, cr: double;
  AdjustedTrackBounds, AdjustedFillBounds: TIFloatRect;
begin
  extra := IfThen(fHandleInsideTrack, HandleSize);
  AdjustedTrackBounds := specialize IfThen<TIFloatRect>(Direction = Vertical,
    fTrackBounds.GetVPadded(extra), fTrackBounds.GetHPadded(extra));
  AdjustedFillBounds := specialize IfThen<TIFloatRect>(Direction = Vertical,
    FilledArea.GetVPadded(extra), filledArea.GetHPadded(extra));
  cr := GetRoundedCornerRadius(fTrackBounds);
  Canvas.PenWidth := 0;
  Canvas.BrushColor := GetColor(kSH);
  Canvas.DrawRoundedRect(AdjustedTrackBounds, cr);
  Canvas.BrushColor := GetColor(kX1);
  Canvas.DrawRoundedRect(AdjustedFillBounds, cr);
  if Style.drawFrame then
  begin
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.BrushColor := 0;
    Canvas.DrawRoundedRect(AdjustedTrackBounds, cr);
  end;
end;

procedure TIVSliderControl.DrawHandle(Canvas: TICanvas; Bounds: TIFloatRect);
begin
  DrawPressableShape(Canvas, Shape, Bounds);
end;

function TIVSliderControl.GetCodeStr: string;
begin
  if ActionFunction = '' then
    Result := Format('%s(%s, %s, "%s", %s, %s, EDirection::%s, %s, %s, %s, %s)',
      [GetObjectName, IRect(GetRect), ParamIdx, &Label, Style.Name,
      BooleanToStr(ValueIsEditable), GetEnumName(TypeInfo(TDirection),
      Ord(Direction)), Gearing, DoubleToStr(HandleSize), DoubleToStr(TrackSize),
      BooleanToStr(HandleInsideTrack)])
  else
    Result := Format('%s(%s, %s, "%s", %s, %s, EDirection::%s, %s, %s, %s, %s)',
      [GetObjectName, IRect(GetRect), ActionFunction, &Label,
      Style.Name, BooleanToStr(ValueIsEditable),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)), Gearing,
      DoubleToStr(HandleSize), DoubleToStr(TrackSize), BooleanToStr(HandleInsideTrack)]);
end;

{ TIVTrackControlBase }

constructor TIVTrackControlBase.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fNTracks := 1;
  fPeakSize := 1;
  fMaxNTracks := 1;
  fLowParamidx := kNoParameter;
  fTrackPadding := 0;
  fMinTrackValue := 0;
  fMaxTrackValue := 1.0;
  fDrawTrackFrame := true;
  fParams := TStringList.Create;
  fValues := TStringList.Create;
  fParams.CommaText := kNoParameter;
  fValues.CommaText := '0';
  SetLength(fTrackBounds, 1);
end;

destructor TIVTrackControlBase.Destroy;
begin
  SetLength(fTrackBounds, 0);
  FreeAndNil(fParams);
  FreeAndNil(fValues);
  inherited Destroy;
end;

class function TIVTrackControlBase.GetObjectName: string;
begin
  Result := 'IVTrackControlBase';
end;

procedure TIVTrackControlBase.TracksChange;
begin
  fNTracks := Max(fMaxNTracks, fParams.Count);
  SetLength(fTrackBounds, fNTracks);
  MakeTrackRects(fWidgetBounds);
end;

procedure TIVTrackControlBase.SetParams(AValue: string);
begin
  AValue := NormalizeOptStr(AValue);
  if AValue = fParams.CommaText then
    exit;
  DoChanging(Self);
  if AValue.IsEmpty then
    AValue := kNoParameter;
  fParams.CommaText := AValue;
  fMaxNTracks := 1;
  TracksChange;
  DoChanged(Self);
end;

procedure TIVTrackControlBase.SetValues(AValue: string);
begin
  AValue := NormalizeOptStr(AValue);
  if AValue = fValues.CommaText then
    exit;
  DoChanging(Self);
  if AValue.IsEmpty then
    AValue := '0';
  fValues.CommaText := AValue;
  TracksChange;
  DoChanged(Self);
end;

function TIVTrackControlBase.GetParams: string;
begin
  Result := FormatOptStr(fParams.CommaText);
end;

function TIVTrackControlBase.GetValues: string;
begin
  Result := FormatOptStr(fValues.CommaText);
end;

procedure TIVTrackControlBase.SetLowParamidx(AValue: string);
begin
  if fLowParamidx = AValue then
    Exit;
  DoChanging(Self);
  fLowParamidx := AValue;
  DoChanged(Self);
end;

procedure TIVTrackControlBase.SetMaxNTracks(AValue: integer);
begin
  AValue := Max(1, AValue);
  if fMaxNTracks = AValue then
    Exit;
  DoChanging(Self);
  fMaxNTracks := AValue;
  fParams.CommaText := kNoParameter;
  TracksChange;
  DoChanged(Self);
end;

function TIVTrackControlBase.GetValue(chIdx: integer): double;
begin
  chIdx := EnsureRange(chIdx, 0, fValues.Count - 1);
  result := EnsureRange(StrToFloatDef(fValues[chIdx], 0), 0, 1);
end;

procedure TIVTrackControlBase.Resize;
begin
  if osCreating in State then
    exit;
  inherited Resize;
  MakeTrackRects(fWidgetBounds);
end;

procedure TIVTrackControlBase.MakeTrackRects(Bounds: TIFloatRect);
var
  i: integer;
  ndir: TDirection;
begin
  ndir := specialize IfThen<TDirection>(Direction = Vertical, Horizontal, Vertical);
  for i := 0 to fNTracks - 1 do
    fTrackBounds[i] := Bounds.SubRect(ndir, fNTracks, i).GetPadded(0,
    -fTrackPadding * Ord(Direction), -fTrackPadding * Ord(ndir), -fTrackPadding);
end;

procedure TIVTrackControlBase.DrawWidget(Canvas: TICanvas);
var
  i: integer;
begin
  for i := 0 to fNTracks - 1 do
    DrawTrack(Canvas, fTrackBounds[i], i);
end;

procedure TIVTrackControlBase.DrawTrack(Canvas: TICanvas; const r: TIFloatRect;
  chIdx: integer);
begin
  DrawTrackBG(Canvas, r, chIdx);
  DrawTrackHandle(Canvas, r, chIdx);
  if Style.DrawFrame and fDrawTrackFrame then
  begin
    Canvas.PenColor := GetColor(kFR);
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.BrushColor := 0;
    Canvas.DrawRect(r);
  end;
end;

procedure TIVTrackControlBase.DrawTrackBG(Canvas: TICanvas;
  const r: TIFloatRect; chIdx: integer);
begin
  Canvas.BrushColor := GetColor(kBG);
  Canvas.DrawRect(r);
end;

procedure TIVTrackControlBase.DrawTrackHandle(Canvas: TICanvas;
  const r: TIFloatRect; chIdx: integer);
var
  fr, pr: TIFloatRect;
begin
  fr := r.FracRect(Direction, GetValue(chIdx));
  Canvas.BrushColor := GetColor(kFG);
  Canvas.DrawRect(fr);
  with fr do
    if Direction = Vertical then
      pr := TIFloatRect.Create(Left, Top, Right, Top + fPeakSize)
    else
      pr := TIFloatRect.Create(Right - fPeakSize, Top, Right, Bottom);
  DrawPeak(Canvas, pr, chIdx);
end;

procedure TIVTrackControlBase.DrawPeak(Canvas: TICanvas; const r: TIFloatRect;
  chIdx: integer);
begin
  Canvas.BrushColor := GetColor(kFR);
  Canvas.DrawRect(r);
end;


{ TIVRangeSliderControl }

constructor TIVRangeSliderControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fOnlyHandle := false;
  fDirection := Vertical;
  fHandleSize := 8.0;
  SetBounds(0, 0, 50, 100);
end;

class function TIVRangeSliderControl.GetObjectName: string;
begin
  Result := 'IVRangeSliderControl';
end;

function TIVRangeSliderControl.GetHandleBounds(TrackIdx: integer): TIFloatRect;
var
  cx, cy, offset: double;
  ft: TIFloatRect;
begin
  ft := fTrackBounds[TrackIdx].FracRect(fDirection, GetValue(TrackIdx));
  offset := IfThen(Style.DrawShadows and (Shape <> Ellipse), Style.ShadowOffset * 0.5);
  if Direction = Vertical then
  begin
    cx := ft.MW() + offset;
    cy := ft.Top;
    if (TrackIdx mod 2) <> 0 then
      result := TIFloatRect.Create(cx + fTrackSize, cy - fHandleSize, cx +
        (2.0 * fHandleSize) + fTrackSize, cy + fHandleSize)
    else
      result := TIFloatRect.Create(cx - (2.0 * fHandleSize),
        cy - fHandleSize, cx, cy + fHandleSize);
  end
  else
  begin
    cx := ft.Right;
    cy := ft.MH() + offset;
    if (TrackIdx mod 2) <> 0 then
      result := TIFloatRect.Create(cx - fHandleSize, cy - (2.0 * fHandleSize),
        cx + fHandleSize, cy)
    else
      result := TIFloatRect.Create(cx - fHandleSize, cy + fTrackSize,
        cx + fHandleSize, cy + (2.0 * fHandleSize) + fTrackSize);
  end;
end;

procedure TIVRangeSliderControl.SetHandleSize(AValue: double);
begin
  if fHandleSize = AValue then
    Exit;
  DoChanging(Self);
  fHandleSize := AValue;
  DoChanged(Self);
end;

procedure TIVRangeSliderControl.SetOnlyHandle(AValue: boolean);
begin
  if fOnlyHandle = AValue then
    Exit;
  DoChanging(Self);
  fOnlyHandle := AValue;
  DoChanged(Self);
end;

procedure TIVRangeSliderControl.MakeTrackRects(Bounds: TIFloatRect);
var
  i: integer;
begin
  for i := 0 to fNTracks - 1 do
  begin
    if Direction = Vertical then
      fTrackBounds[i] := Bounds.GetPadded(-fHandleSize).GetMidHPadded(fTrackSize)
    else
      fTrackBounds[i] := Bounds.GetPadded(-fHandleSize).GetMidVPadded(fTrackSize)
  end;
end;

procedure TIVRangeSliderControl.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawWidget(Canvas);
  DrawLabel(Canvas);
end;

procedure TIVRangeSliderControl.DrawWidget(Canvas: TICanvas);
var
  r, f1, f2: TIFloatRect;
  i: integer;
begin
  r := fTrackBounds[0];
  DrawTrackBG(Canvas, r, 0);
  for i := 0 to fNTracks - 2 do
  begin
    f1 := fTrackBounds[i].FracRect(fDirection, GetValue(i));
    f2 := fTrackBounds[i + 1].FracRect(fDirection, GetValue(i + 1));
    Canvas.BrushColor := GetColor(kX1);
    if fDirection = Vertical then
      Canvas.DrawRect(TIFloatRect.Create(f1.Left, IfThen(f1.Top < f2.Top, f1.Top, f2.Top),
        f1.Right, IfThen(f1.Top > f2.Top, f1.Top, f2.Top)))
    else
      Canvas.DrawRect(TIFloatRect.Create(IfThen(f1.Right < f2.Right, f1.Right, f2.Right),
        f1.Top, IfThen(f1.Right > f2.Right, f1.Right, f2.Right), f1.Bottom));
  end;

  if Style.DrawFrame and fDrawTrackFrame then
  begin
    Canvas.BrushColor := 0;
    Canvas.PenColor := GetColor(kFR);
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.DrawRect(r);
  end;
  inherited DrawWidget(Canvas);
end;

procedure TIVRangeSliderControl.DrawTrack(Canvas: TICanvas;
  const r: TIFloatRect; chIdx: integer);
var
  a: double;
begin
  if Direction = Horizontal then
    a := IFThen((chIdx mod 2) <> 0, 180.0)
  else
    a := IFThen((chIdx mod 2) <> 0, 270.0, 90.0);
  DrawPressableTriangle(Canvas, GetHandleBounds(chIdx), false, a);
end;

function TIVRangeSliderControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, {%s}, "%s", %s, EDirection::%s, %s, %s, %s)',
    [GetObjectName, IRect(GetRect), FormatOptCode(fParams, false), &Label,
    Style.Name, GetEnumName(TypeInfo(TDirection), Ord(Direction)),
    BooleanToStr(OnlyHandle), DoubleToStr(HandleSize), DoubleToStr(TrackSize)]);
end;


{ TIVGroupControl }

constructor TIVGroupControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fLabelText := 'Group';
  fLabelOffset := 10;
  fLabelPadding := 10;
  SetBounds(0, 0, 150, 100);
end;

class function TIVGroupControl.GetObjectName: string;
begin
  Result := 'IVGroupControl';
end;

procedure TIVGroupControl.SetLabelOffset(AValue: double);
begin
  if fLabelOffset = AValue then
    Exit;
  DoChanging(Self);
  fLabelOffset := AValue;
  Refresh;
  DoChanged(Self);
end;

procedure TIVGroupControl.Resize;
var
  cr: double;
begin
  if osCreating in State then
    exit;
  inherited Resize;
  fLabelBounds.HPad(fLabelPadding);
  fWidgetBounds.Alter(0, -(fLabelBounds.Height * 0.5) - (Style.FrameThickness * 0.5), 0, 0);
  cr := GetRoundedCornerRadius(fWidgetBounds);
  fLabelBounds.Offset(GetRect.Left - fLabelBounds.Left + Style.FrameThickness + fLabelOffset + cr, 0);
end;

procedure TIVGroupControl.Paint(Canvas: TICanvas);
begin
  DrawWidget(Canvas);
  DrawLabel(Canvas);
end;

procedure TIVGroupControl.DrawWidget(Canvas: TICanvas);
var
  cr, hft, lr, ll: double;
begin
  cr := GetRoundedCornerRadius(fWidgetBounds);
  hft := Style.FrameThickness * 0.5;
  lr := IfThen(fLabelBounds.IsEmpty, GetRect.MW, fLabelBounds.Right);
  ll := IfThen(fLabelBounds.IsEmpty, GetRect.MW, fLabelBounds.Left);
  Canvas.PenWidth := Style.FrameThickness;
  Canvas.PenColor := IfThen(Style.DrawShadows, GetColor(kSH), GetColor(kFR));
  Canvas.DrawGroupboxFrame(fWidgetBounds, ll, lr, cr, hft);
end;

function TIVGroupControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, "%s", %s, %s)',
    [GetObjectName, IRect(GetRect), &Label, DoubleToStr(LabelOffset), Style.Name]);
end;

{ TIVPanelControl }

constructor TIVPanelControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fLabelText := 'Panel';
  SetBounds(0, 0, 150, 100);
end;

class function TIVPanelControl.GetObjectName: string;
begin
  Result := 'IVPanelControl';
end;

procedure TIVPanelControl.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawWidget(Canvas);
  DrawLabel(Canvas);
end;

procedure TIVPanelControl.DrawWidget(Canvas: TICanvas);
begin
  DrawPressableRectangle(Canvas, fWidgetBounds, false);
end;

function TIVPanelControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, "%s", %s)',
    [GetObjectName, IRect(GetRect), &Label, Style.Name]);
end;

{ TIVNumberBoxControl }

constructor TIVNumberBoxControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fTextReadout := TIFloatRect.Empty;
  fIncButton := TIFloatRect.Empty;
  fDecButton := TIFloatRect.Empty;
  fValueInWidget := true;
  fMinValue := 1.0;
  fMaxValue := 100;
  fDefaultValue := 50;
  fValue := fDefaultValue;
  fFmtStr := '%0.0f';
  fActionFunction := DEFAULT_ACT_FUNCTION;
  SetBounds(0, 0, 75, 50);
end;

class function TIVNumberBoxControl.GetObjectName: string;
begin
  Result := 'IVNumberBoxControl';
end;

procedure TIVNumberBoxControl.MakeControlRects;
var
  r: TIFloatRect;
begin
  r := fWidgetBounds;
  fTextReadout :=  r.ReduceFromLeft(r.Width * 0.75);
  r.Pad(-1, 1, 0, 1);
  fIncButton := r.FracRectVertical(0.5, true);
  fDecButton := r.FracRectVertical(0.5, false);
  fIncButton.Inflate(-1);
  fDecButton.Inflate(-1);
end;

procedure TIVNumberBoxControl.SetDefaultValue(AValue: double);
begin
  if fDefaultValue = AValue then
    Exit;
  DoChanging(Self);
  fDefaultValue := AValue;
  DoChanged(Self);
end;

procedure TIVNumberBoxControl.SetFmtStr(AValue: string);
begin
  if fFmtStr = AValue then
    Exit;
  DoChanging(Self);
  fFmtStr := AValue;
  DoChanged(Self);
end;

procedure TIVNumberBoxControl.Resize;
begin
  if osCreating in State then
    exit;
  inherited Resize;
  MakeControlRects;
end;

procedure TIVNumberBoxControl.DrawWidget(Canvas: TICanvas);
begin
  Canvas.PenWidth := Style.FrameThickness;
  Canvas.PenColor := GetColor(kFR);
  Canvas.BrushColor := 0;
  Canvas.DrawRect(fTextReadout);
  Canvas.PenColor := GetColor(kFR);
  Canvas.PenWidth := Style.FrameThickness;
  Canvas.BrushColor := GetColor(kFG);
  Canvas.DrawRect(fIncButton);
  Canvas.DrawRect(fDecButton);
end;

procedure TIVNumberBoxControl.DrawLabel(Canvas: TICanvas);
begin
  Canvas.GraphicsSettings.Font := Style.LabelFont;
  if (fLabelBounds.Height > 0) or Style.ShowLabel then
    Canvas.DrawText(fLabelText, fLabelBounds);
  Canvas.FontAlign := Center;
  Canvas.FontVAlign := Middle;
  Canvas.DrawText('+', fIncButton);
  Canvas.DrawText('-', fDecButton);
end;

procedure TIVNumberBoxControl.DrawValue(Canvas: TICanvas);
begin
  Canvas.GraphicsSettings.Font := Style.ValueFont;
  Canvas.FontAlign := Center;
  Canvas.FontVAlign := Middle;
  fValueText := Format(fFmtStr, [fValue]);
  Canvas.DrawText(fValueText, fTextReadout);
end;

function TIVNumberBoxControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, %s, %s, "%s", %s, %s, %s, %s, "%s")', [GetObjectName,
    IRect(GetRect), ParamIdx, ActionFunction, &Label, Style.Name,
    DoubleToStr(DefaultValue), DoubleToStr(MinValue), DoubleToStr(MaxValue),
    FmtStr])
end;


{ TIVXYPadControl }

constructor TIVXYPadControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fValues[0] := 0;
  fValues[1] := 0;
  fParams[0] := kNoParameter;
  fParams[1] := kNoParameter;
  fHandleRadius := 10;
  fShape := Ellipse;
  SetBounds(0, 0, 100, 100);
end;

class function TIVXYPadControl.GetObjectName: string;
begin
  Result := 'IVXYPadControl';
end;

procedure TIVXYPadControl.SetHandleRadius(AValue: double);
begin
  if fHandleRadius = AValue then
    Exit;
  DoChanging(Self);
  fHandleRadius := AValue;
  DoChanged(Self);
end;

function TIVXYPadControl.GetParams: string;
begin
  Result := Format('%s, %s', [fParams[0], fParams[1]]);
end;

function TIVXYPadControl.GetValues: string;
begin
  Result := Format('%s, %s', [DoubleToStr(fValues[0]), DoubleToStr(fValues[1])]);
end;

procedure TIVXYPadControl.SetParams(AValue: string);
var
  a: TStringDynArray;
begin
  AValue := NormalizeOptStr(AValue);
  if AValue = NormalizeOptStr(GetParams) then
    exit;
  DoChanging(Self);
  if AValue.IsEmpty then
  begin
    fParams[0] := kNoParameter;
    fParams[1] := kNoParameter;
  end else begin
    a := AValue.Split([',']);
    if Length(a) < 2 then
    begin
      fParams[0] := a[0];
      fParams[1] := kNoParameter;
    end else begin
      fParams[0] := a[0];
      fParams[1] := a[1];
    end
  end;
  DoChanged(Self);
end;

procedure TIVXYPadControl.SetValues(AValue: string);
var
  a: TStringDynArray;
begin
  AValue := NormalizeOptStr(AValue);
  if AValue = NormalizeOptStr(GetValues) then
    exit;
  DoChanging(Self);
  if AValue.IsEmpty then
  begin
    fValues[0] := 0;
    fValues[1] := 0;
  end else begin
    a := AValue.Split([',']);
    if Length(a) < 2 then
    begin
      fValues[0] := EnsureRange(StrToFloatDef(a[0], 0), 0, 1);
      fValues[1] := 0;
    end else begin
      fValues[0] := EnsureRange(StrToFloatDef(a[0], 0), 0, 1);
      fValues[1] := EnsureRange(StrToFloatDef(a[1], 0), 0, 1);
    end
  end;
  DoChanged(Self);
end;

procedure TIVXYPadControl.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawLabel(Canvas);
  if Style.DrawFrame then
  begin
    Canvas.BrushColor := 0;
    Canvas.PenColor := GetColor(kFR);
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.DrawRect(fWidgetBounds);
  end;
  DrawWidget(Canvas);
end;

procedure TIVXYPadControl.DrawWidget(Canvas: TICanvas);
var
  tb, hb: TIFloatRect;
  x, y: double;
begin
  DrawTrack(Canvas);
  tb := fWidgetBounds;
  x := fValues[0] * tb.Width;
  y := fValues[1] * tb.Height;
  hb := TIFloatRect.Create(tb.Left + x - fHandleRadius, tb.Bottom - y -
  fHandleRadius, tb.Left + x + fHandleRadius, tb.Bottom - y + fHandleRadius);
  DrawHandle(Canvas, tb, hb);
end;

procedure TIVXYPadControl.DrawTrack(Canvas: TICanvas);
begin
  Canvas.PenWidth := 0.5;
  Canvas.PenColor := GetColor(kSH);
  with fWidgetBounds do
  begin
    Canvas.DrawLine(Left, MH, Right, MH);
    Canvas.DrawLine(MW, Top, MW, Bottom);
  end;
end;

procedure TIVXYPadControl.DrawHandle(Canvas: TICanvas; const TrackBounds,
  HandleBounds: TIFloatRect);
var
  oldclip: TIFloatRect;
begin
  oldclip := Canvas.ClipRect;
  Canvas.ClipRect := TrackBounds.GetPadded(-0.5 * Style.FrameThickness);
  try
    DrawPressableShape(Canvas, fShape, HandleBounds);
  finally
    Canvas.ClipRect := oldclip
  end;
end;

function TIVXYPadControl.GetCodeStr: string;
begin
  Result := Format('%s(%s, {%s}, "%s", %s, %s)',
    [GetObjectName, IRect(GetRect), GetParams, &Label,
    Style.Name, DoubleToStr(HandleRadius)]);
end;

{ TIVMeterControl }

constructor TIVMeterControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fDirection := Vertical;
  SetBounds(0, 0, 50, 100);
end;

class function TIVMeterControl.GetObjectName: string;
begin
  Result := 'IVMeterControl';
end;

procedure TIVMeterControl.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawWidget(Canvas);
  DrawLabel(Canvas);
  if Style.DrawFrame then
  begin
    Canvas.BrushColor := 0;
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.PenColor := GetColor(kFR);
    Canvas.DrawRect(fWidgetBounds);
  end;
end;

function TIVMeterControl.GetCodeStr: string;
begin
  Result := Format('%s<%d>(%s, "%s", %s, EDirection::%s)',
    [GetObjectName, MaxNTracks, IRect(GetRect), &Label, Style.Name,
    GetEnumName(TypeInfo(TDirection), Ord(Direction))]);
end;


{ TIVMultiSliderControl }

constructor TIVMultiSliderControl.Create(AOwner: TIObject; ADocument: TObject);
begin
  inherited Create(AOwner, ADocument);
  fDrawTrackFrame := false;
  fTrackPadding := 1;
  fDirection := Vertical;
  SetBounds(0, 0, 100, 100);
end;

class function TIVMultiSliderControl.GetObjectName: string;
begin
  Result := 'IVMultiSliderControl';
end;

procedure TIVMultiSliderControl.SetMaxTrackValue(AValue: double);
begin
  if fMaxTrackValue = AValue then
    Exit;
  DoChanging(Self);
  fMaxTrackValue := AValue;
  DoChanged(Self);
end;

procedure TIVMultiSliderControl.SetMinTrackValue(AValue: double);
begin
  if fMinTrackValue = AValue then
    Exit;
  DoChanging(Self);
  fMinTrackValue := AValue;
  DoChanged(Self);
end;

procedure TIVMultiSliderControl.Paint(Canvas: TICanvas);
begin
  DrawBackGround(Canvas, GetRect);
  DrawWidget(Canvas);
  DrawLabel(Canvas);
  if Style.DrawFrame then
  begin
    Canvas.BrushColor := 0;
    Canvas.PenWidth := Style.FrameThickness;
    Canvas.PenColor := GetColor(kFR);
    Canvas.DrawRect(fWidgetBounds);
  end;
end;

function TIVMultiSliderControl.GetCodeStr: string;
begin
  if fParams.Count >= fMaxNTracks then
  // Constructs a vector multi slider control with parameters list
    Result := Format('%s<%d>(%s, "%s", %s, {%s}, EDirection::%s, %s, %s)',
      [GetObjectName, fNTracks, IRect(GetRect), &Label, Style.Name,
      FormatOptCode(fParams, false),
      GetEnumName(TypeInfo(TDirection), Ord(Direction)),
      DoubleToStr(MinTrackValue), DoubleToStr(MaxTrackValue)])
  else
  if LowParamidx.IsEmpty or (LowParamidx = kNoParameter)
  or (LowParamidx.Trim = '-1') then
  // Constructs a vector multi slider control that is not linked to parameters
    Result := Format('%s<%d>(%s, "%s", %s, EDirection::%s, %s, %s)',
      [GetObjectName, fNTracks, IRect(GetRect), &Label, Style.Name,
      GetEnumName(TypeInfo(TDirection), Ord(Direction)),
      DoubleToStr(MinTrackValue), DoubleToStr(MaxTrackValue)])
  else
  // Constructs a vector multi slider control that is linked to parameters
    Result := Format('%s<%d>(%s, "%s", %s, %s, EDirection::%s, %s, %s)',
      [GetObjectName, fNTracks, IRect(GetRect), &Label, Style.Name,
      LowParamidx, GetEnumName(TypeInfo(TDirection), Ord(Direction)),
      DoubleToStr(MinTrackValue), DoubleToStr(MaxTrackValue)])
end;

{ TIVTestControl }

constructor TIVTestControl.Create(AStyle: TIVStyle; ABounds: TIFloatRect;
  ACanvas: TICanvas);
begin
  inherited Create(nil, nil);
  fCanvas := ACanvas;
  fStyle := AStyle;
  fShape := AllRounded;
  fLabelText := 'Label';
  fValueText := 'Value';
  with ABounds do
    SetBounds(Left, Top, Width, Height);
  MakeRects(ABounds);
end;

procedure TIVTestControl.Paint(MouseDown: boolean);
begin
  DrawBackGround(fCanvas, GetRect);
  DrawPressableShape(fCanvas, fShape, fWidgetBounds, MouseDown);
  DrawLabel(fCanvas);
  DrawValue(fCanvas);
end;

end.
