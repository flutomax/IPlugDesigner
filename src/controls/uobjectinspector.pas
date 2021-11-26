{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
   This unit defines the TObjectInspectorDlg.
   It uses TOIPropertyGrid and TOIPropertyGridRow which are also defined in this
   unit. The object inspector uses property editors (see TPropertyEditor) to
   display and control properties, thus the object inspector is merely an
   object viewer than an editor. The property editors do the real work.

  ToDo:
   - backgroundcolor=clNone
   - Define Init values
   - Set to init value

   Adapted to IPlug Designer purposes: Vasily Makarov
}

unit uObjectInspector;

{$mode objfpc}{$H+}

interface

uses
  // IMPORTANT: the object inspector is a tool and can be used in other programs
  //            too. Don't put Lazarus IDE specific things here.
  // RTL / FCL
  SysUtils, Types, Classes, TypInfo, Math, FPCanvas,
  // LCL
  LCLPlatformDef, InterfaceBase, LCLType, LCLIntf, Forms, Buttons, Graphics,
  GraphType, StdCtrls, Controls, ComCtrls, ExtCtrls, Menus, Dialogs, Themes,
  LMessages, LCLProc,
  // LazControls
  {$IFnDEF UseOINormalCheckBox} CheckBoxThemed, {$ENDIF}
  TreeFilterEdit, ListFilterEdit,
  // LazUtils
  LazConfigStorage, LazLoggerBase,
  // IdeIntf
  IDEImagesIntf, IDEHelpIntf, ObjInspStrConsts,
  PropEdits, PropEditUtils, OIFavoriteProperties,
  ComponentEditors, ChangeParentDlg, ImgList, FPImage,
  uCommon, uIObject, uIDocument;

const
  OIOptionsFileVersion = 3;

  DefBackgroundColor = clBtnFace;
  DefReferencesColor = clMaroon;
  DefSubPropertiesColor = clGreen;
  DefNameColor = clWindowText;
  DefValueColor = clMaroon;
  DefDefaultValueColor = clWindowText;
  DefValueDifferBackgrndColor = $F0F0FF; // Sort of pink.
  DefReadOnlyColor = clGrayText;
  DefHighlightColor = clHighlight;
  DefHighlightFontColor = clHighlightText;
  DefGutterColor = DefBackgroundColor;
  DefGutterEdgeColor = cl3DShadow;

  DefaultOITypeKinds = [tkUnknown, tkInteger, tkChar, tkEnumeration,
    tkFloat, tkSet,{ tkMethod,}
    tkSString, tkLString, tkAString, tkWString, tkVariant,
    {tkArray, tkRecord,} tkInterface, tkClass, tkObject, tkWChar,
    tkBool, tkInt64, tkQWord, tkUString, tkUChar];

type
  EObjectInspectorException = class(Exception);

  TCustomObjectInspector = class;
  TValueComboBox = class;

  { TOIOptions }

  TOIOptions = class
  private
    FComponentTreeHeight: integer;
    FConfigStore: TConfigStorage;
    FDefaultItemHeight: integer;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FShowComponentTree: boolean;

    FSaveBounds: boolean;
    FLeft: integer;
    FShowGutter: boolean;
    FShowInfoBox: boolean;
    FInfoBoxHeight: integer;
    FShowStatusBar: boolean;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;

    FPropertyNameColor: TColor;
    FSubPropertiesColor: TColor;
    FValueColor: TColor;
    FDefaultValueColor: TColor;
    FValueDifferBackgrndColor: TColor;
    FReadOnlyColor: TColor;
    FReferencesColor: TColor;
    FGridBackgroundColor: TColor;
    FHighlightColor: TColor;
    FHighlightFontColor: TColor;

    FShowHints: boolean;
    FAutoShow: boolean;
    FCheckboxForBoolean: boolean;
    FBoldNonDefaultValues: boolean;
    FDrawGridLines: boolean;
  public
    constructor Create;
    function Load: boolean;
    function Save: boolean;
    procedure AssignTo(AGrid: TCustomObjectInspector);

    property ConfigStore: TConfigStorage read FConfigStore write FConfigStore;

    property SaveBounds: boolean read FSaveBounds write FSaveBounds;
    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property DefaultItemHeight: integer read FDefaultItemHeight
      write FDefaultItemHeight;
    property ShowComponentTree: boolean read FShowComponentTree
      write FShowComponentTree;
    property ComponentTreeHeight: integer read FComponentTreeHeight
      write FComponentTreeHeight;

    property GridBackgroundColor: TColor read FGridBackgroundColor
      write FGridBackgroundColor;
    property SubPropertiesColor: TColor read FSubPropertiesColor
      write FSubPropertiesColor;
    property ReferencesColor: TColor read FReferencesColor write FReferencesColor;
    property ReadOnlyColor: TColor read FReadOnlyColor write FReadOnlyColor;
    property ValueColor: TColor read FValueColor write FValueColor;
    property DefaultValueColor: TColor read FDefaultValueColor write FDefaultValueColor;
    property ValueDifferBackgrndColor: TColor
      read FValueDifferBackgrndColor write FValueDifferBackgrndColor;
    property PropertyNameColor: TColor read FPropertyNameColor write FPropertyNameColor;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property HighlightFontColor: TColor read FHighlightFontColor
      write FHighlightFontColor;
    property GutterColor: TColor read FGutterColor write FGutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write FGutterEdgeColor;

    property ShowHints: boolean read FShowHints write FShowHints;
    property AutoShow: boolean read FAutoShow write FAutoShow;
    property CheckboxForBoolean: boolean read FCheckboxForBoolean
      write FCheckboxForBoolean;
    property BoldNonDefaultValues: boolean read FBoldNonDefaultValues
      write FBoldNonDefaultValues;
    property DrawGridLines: boolean read FDrawGridLines write FDrawGridLines;
    property ShowGutter: boolean read FShowGutter write FShowGutter;
    property ShowStatusBar: boolean read FShowStatusBar write FShowStatusBar;
    property ShowInfoBox: boolean read FShowInfoBox write FShowInfoBox;
    property InfoBoxHeight: integer read FInfoBoxHeight write FInfoBoxHeight;
  end;

  { TOIPropertyGridRow }

  TOIPropertyGridRow = class
  private
    FTop: integer;
    FHeight: integer;
    FLvl: integer;
    FName: string;
    FExpanded: boolean;
    FTree: TCustomObjectInspector;
    FChildCount: integer;
    FPriorBrother, FFirstChild, FLastChild, FNextBrother,
    FParent: TOIPropertyGridRow;
    FEditor: TPropertyEditor;
    FWidgetSets: TLCLPlatforms;

    FIndex: integer;
    LastPaintedValue: string;

    procedure GetLvl;
  public
    constructor Create(PropertyTree: TCustomObjectInspector;
      PropEditor: TPropertyEditor; ParentNode: TOIPropertyGridRow;
      WidgetSets: TLCLPlatforms);
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    function HasChild(Row: TOIPropertyGridRow): boolean;
    procedure WriteDebugReport(const Prefix: string);

    function GetBottom: integer;
    function IsReadOnly: boolean;
    function IsDisabled: boolean;
    procedure MeasureHeight(ACanvas: TCanvas);
    function Sort(const Compare: TListSortCompare): boolean; // true if changed
    function IsSorted(const Compare: TListSortCompare): boolean;
    function Next: TOIPropertyGridRow;
    function NextSkipChilds: TOIPropertyGridRow;

    property Editor: TPropertyEditor read FEditor;
    property Top: integer read FTop write FTop;
    property Height: integer read FHeight write FHeight;
    property Bottom: integer read GetBottom;
    property Lvl: integer read FLvl;
    property Name: string read FName;
    property Expanded: boolean read FExpanded;
    property Tree: TCustomObjectInspector read FTree;
    property Parent: TOIPropertyGridRow read FParent;
    property ChildCount: integer read FChildCount;
    property FirstChild: TOIPropertyGridRow read FFirstChild;
    property LastChild: TOIPropertyGridRow read FLastChild;
    property NextBrother: TOIPropertyGridRow read FNextBrother;
    property PriorBrother: TOIPropertyGridRow read FPriorBrother;
    property Index: integer read FIndex;
  end;

  //----------------------------------------------------------------------------
  TOIPropertyGridState = (
    pgsChangingItemIndex,
    pgsApplyingValue,
    pgsUpdatingEditControl,
    pgsBuildPropertyListNeeded,
    pgsGetComboItemsCalled,
    pgsIdleEnabled,
    pgsCallingEdit,                 // calling property editor Edit
    pgsFocusPropertyEditorDisabled  // by building PropertyList no editor should be focused
    );
  TOIPropertyGridStates = set of TOIPropertyGridState;

  { TCustomObjectInspector }

  TOICustomPropertyGridColumn = (
    oipgcName,
    oipgcValue
    );

  TOILayout = (
    oilHorizontal,
    oilVertical
    );

  TOIQuickEdit = (
    oiqeEdit,
    oiqeShowValue
    );

  TOIPropertyHintEvent = function(Sender: TObject; PointedRow: TOIPropertyGridRow;
    out AHint: string): boolean of object;

  TOIEditorFilterEvent = procedure(Sender: TObject; aEditor: TPropertyEditor;
    var aShow: boolean) of object;

  TCustomObjectInspector = class(TCustomControl, IDocumentObserver, ILayeredDocumentObserver)
  private
    fBackgroundColor: TColor;
    fColumn: TOICustomPropertyGridColumn;
    fGutterColor: TColor;
    fGutterEdgeColor: TColor;
    fHighlightColor: TColor;
    fLayout: TOILayout;
    fOnEditorFilter: TOIEditorFilterEvent;
    fOnOIKeyDown: TKeyEvent;
    fOnPropertyHint: TOIPropertyHintEvent;
    fOnSelectionChange: TNotifyEvent;
    fReferencesColor: TColor;
    fReadOnlyColor: TColor;
    fRowSpacing: integer;
    fShowGutter: boolean;
    fCheckboxForBoolean: boolean;
    fSubPropertiesColor: TColor;
    fChangeStep: integer;
    fCurrentButton: TControl; // nil or ValueButton
    fCurrentEdit: TWinControl;  // nil or ValueEdit or ValueComboBox or ValueCheckBox
    fCurrentEditorLookupRoot: TPersistent;
    fDefaultItemHeight: integer;
    fDragging: boolean;
    fExpandedProperties: TStringList;
    // used to restore expanded state when switching selected component(s)
    fExpandingRow: TOIPropertyGridRow;
    fFavorites: TOIFavoriteProperties;
    fFilter: TTypeKinds;
    fIndent: integer;
    fItemIndex: integer;
    fNameFont, fDefaultValueFont, fValueFont, fHighlightFont: TFont;
    fValueDifferBackgrndColor: TColor;
    fNewComboBoxItems: TStringList;
    fOnModified: TNotifyEvent;
    fRows: TFPList;// list of TOIPropertyGridRow
    fSelection: TPersistentSelectionList;
    fPropertyEditorHook: TPropertyEditorHook;
    fPreferredSplitterX: integer; // best splitter position
    fSplitterX: integer; // current splitter position
    fStates: TOIPropertyGridStates;
    fTopY: integer;
    fDrawHorzGridLines: boolean;
    fActiveRowImages: TLCLGlyphs;
    fFirstClickTime: DWORD;
    fKeySearchText: string;
    fHideClassNames: boolean;
    fPropNameFilter: string;

    // hint stuff
    fLongHintTimer: TTimer;
    fHintManager: THintWindowManager;
    fHintIndex: integer;
    fHintType: TPropEditHint;
    fShowingLongHint: boolean; // last hint was activated by the hinttimer

    ValueEdit: TEdit;
    ValueComboBox: TValueComboBox;
    {$IFnDEF UseOINormalCheckBox}
    ValueCheckBox: TCheckBoxThemed;
    {$ELSE}
    ValueCheckBox: TCheckBox;
    {$ENDIF}
    ValueButton: TSpeedButton;
    fAutoFreeHook: boolean;
    fSaveOnChangeIObject: boolean;
    fDocument: TIDocument;
    procedure ActiveRowImagesGetWidthForPPI(Sender: TCustomImageList;
    {%H-}AImageWidth, {%H-}APPI: integer; var AResultWidth: integer);
    procedure HintMouseLeave(Sender: TObject);
    procedure HintTimer(Sender: TObject);
    procedure ResetLongHintTimer;
    procedure HideHint;
    procedure HintMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    procedure IncreaseChangeStep;
    function GridIsUpdating: boolean;

    function GetRow(Index: integer): TOIPropertyGridRow;
    function GetRowCount: integer;
    procedure ClearRows;
    function GetCurrentEditValue: string;
    procedure SetActiveControl(const AControl: TWinControl);
    procedure SetCheckboxState(NewValue: string);
    procedure SetColumn(const AValue: TOICustomPropertyGridColumn);
    procedure SetCurrentEditValue(const NewValue: string);
    procedure SetDocument(AValue: TIDocument);
    procedure SetDrawHorzGridLines(const AValue: boolean);
    procedure SetFavorites(const AValue: TOIFavoriteProperties);
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetGutterColor(const AValue: TColor);
    procedure SetGutterEdgeColor(const AValue: TColor);
    procedure SetHighlightColor(const AValue: TColor);
    procedure SetItemIndex(NewIndex: integer);
    function IsCurrentEditorAvailable: boolean;

    function GetNameRowHeight: integer;
    // temp solution untill TFont.height returns its actual value

    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetRowSpacing(const AValue: integer);
    procedure SetShowGutter(const AValue: boolean);
    procedure SetSplitterX(const NewValue: integer);
    procedure SetTopY(const NewValue: integer);

    function GetPropNameColor(ARow: TOIPropertyGridRow): TColor;
    function GetTreeIconX(Index: integer): integer;
    function RowRect(ARow: integer): TRect;
    procedure PaintRow(ARow: integer);
    procedure DoPaint(PaintOnlyChangedValues: boolean);

    procedure SetSelection(const ASelection: TPersistentSelectionList);
    procedure SetPropertyEditorHook(NewPropertyEditorHook: TPropertyEditorHook);
    procedure HookGetCheckboxForBoolean(var Value: boolean);

    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s: string);
    procedure ExpandRow(Index: integer);
    procedure ShrinkRow(Index: integer);
    procedure AddSubEditor(PropEditor: TPropertyEditor);
    procedure SortSubEditors(ParentRow: TOIPropertyGridRow);
    function CanExpandRow(Row: TOIPropertyGridRow): boolean;

    procedure SetRowValue(CheckFocus, ForceValue: boolean);
    procedure DoCallEdit(Edit: TOIQuickEdit = oiqeEdit);
    procedure RefreshValueEdit;
    procedure ToggleRow;
    procedure ValueEditDblClick(Sender: TObject);
    procedure ValueControlMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X,{%H-}Y: integer);
    procedure ValueControlMouseMove(Sender: TObject; {%H-}Shift: TShiftState;
    {%H-}X,{%H-}Y: integer);
    procedure ValueEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ValueEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure ValueCheckBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ValueCheckBoxKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ValueCheckBoxExit(Sender: TObject);
    procedure ValueCheckBoxClick(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ValueComboBoxKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ValueComboBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure ValueComboBoxCloseUp(Sender: TObject);
    procedure ValueComboBoxGetItems(Sender: TObject);
    procedure ValueButtonClick(Sender: TObject);
    procedure ValueComboBoxMeasureItem({%H-}Control: TWinControl;
      Index: integer; var AHeight: integer);
    procedure ValueComboBoxDrawItem({%H-}Control: TWinControl;
      Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure OnIdle(Sender: TObject; var {%H-}Done: boolean);
    procedure SetIdleEvent(Enable: boolean);
    procedure OnGridMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var Handled: boolean);

    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetReferences(const AValue: TColor);
    procedure SetSubPropertiesColor(const AValue: TColor);
    procedure SetReadOnlyColor(const AValue: TColor);
    procedure SetValueDifferBackgrndColor(AValue: TColor);
    procedure UpdateScrollBar;
    function FillComboboxItems: boolean; // true if something changed
    function EditorFilter(const AEditor: TPropertyEditor): boolean;
    function GetIObject: TPersistent;
    procedure SetAutoFreeHook(const AValue: boolean);
    procedure SetIObject(const AValue: TPersistent);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;


    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure HandleStandardKeys(var Key: word; Shift: TShiftState); virtual;
    procedure HandleKeyUp(var Key: word; Shift: TShiftState); virtual;
    procedure DoTabKey; virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoSelectionChange;
    procedure ObjectAdded(AObject: TIObject);
    procedure ObjectRemove(AObject: TIObject);
    procedure ObjectChanging(AObject: TIObject);
    procedure ObjectChanged(AObject: TIObject);
    procedure ObjectSelected(AObject: TIObject; Selected: boolean);
    procedure ObjectCommandReceived(AObject: TIObject; var cmd: TICmdMessage);
    procedure ObjectCommandHandled(AObject: TIObject; var cmd: TICmdMessage);
    procedure DocumentResize(NewWidth, NewHeight: double);
    procedure DocumentDestroying;
    procedure LayerAdded(ALayer: TILayer);
    procedure LayerRemove(ALayer: TILayer);
    procedure LayerChanging(ALayer: TILayer);
    procedure LayerChanged(ALayer: TILayer);
    procedure ActiveLayerChanged(ALayer: TILayer);
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateWithParams(AnOwner: TComponent;
      TypeFilter: TTypeKinds; DefItemHeight: integer);
    destructor Destroy; override;
    function InitHints: boolean;
    function CanEditRowValue(CheckFocus: boolean): boolean;
    procedure FocusCurrentEditor;
    procedure SaveChanges;
    function ConsistencyCheck: integer;
    procedure EraseBackground({%H-}DC: HDC); override;
    function GetActiveRow: TOIPropertyGridRow;
    function GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;

    function GetRowByPath(const PropPath: string): TOIPropertyGridRow;
    function GridHeight: integer;
    function RealDefaultItemHeight: integer;
    function MouseToIndex(y: integer; MustExist: boolean): integer;
    function PropertyPath(Index: integer): string;
    function PropertyPath(Row: TOIPropertyGridRow): string;
    function TopMax: integer;
    procedure BuildPropertyList(OnlyIfNeeded: boolean = False;
      FocusEditor: boolean = True);
    procedure Clear;
    procedure Paint; override;
    procedure PropEditLookupRootChange;
    procedure RefreshPropertyValues;
    procedure ScrollToActiveItem;
    procedure ScrollToItem(NewIndex: integer);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure SetCurrentRowValue(const NewValue: string);
    procedure SetItemIndexAndFocus(NewItemIndex: integer;
      WasValueClick: boolean = False);
    procedure InspectObject(AObject: TIObject);

    property BackgroundColor: TColor read fBackgroundColor
      write SetBackgroundColor default DefBackgroundColor;
    property GutterColor: TColor read fGutterColor write SetGutterColor default
      DefGutterColor;
    property GutterEdgeColor: TColor read fGutterEdgeColor
      write SetGutterEdgeColor default DefGutterEdgeColor;
    property HighlightColor: TColor
      read fHighlightColor write SetHighlightColor default DefHighlightColor;
    property ReferencesColor: TColor read fReferencesColor
      write SetReferences default DefReferencesColor;
    property SubPropertiesColor: TColor read fSubPropertiesColor
      write SetSubPropertiesColor default DefSubPropertiesColor;
    property ReadOnlyColor: TColor read fReadOnlyColor
      write SetReadOnlyColor default DefReadOnlyColor;
    property ValueDifferBackgrndColor: TColor
      read fValueDifferBackgrndColor write SetValueDifferBackgrndColor default
      DefValueDifferBackgrndColor;

    property NameFont: TFont read fNameFont write fNameFont;
    property DefaultValueFont: TFont read fDefaultValueFont write fDefaultValueFont;
    property ValueFont: TFont read fValueFont write fValueFont;
    property HighlightFont: TFont read fHighlightFont write fHighlightFont;

    property BorderStyle default bsSingle;
    property Column: TOICustomPropertyGridColumn read fColumn write SetColumn;
    property CurrentEditValue: string read GetCurrentEditValue
      write SetCurrentEditValue;
    property DefaultItemHeight: integer read fDefaultItemHeight
      write fDefaultItemHeight default 0;
    property DrawHorzGridLines: boolean read fDrawHorzGridLines
      write SetDrawHorzGridLines default True;
    property ExpandedProperties: TStringList
      read fExpandedProperties write fExpandedProperties;
    property Indent: integer read fIndent write fIndent;
    property ItemIndex: integer read fItemIndex write SetItemIndex;
    property Layout: TOILayout read fLayout write fLayout default oilHorizontal;
    property OnEditorFilter: TOIEditorFilterEvent
      read fOnEditorFilter write fOnEditorFilter;
    property OnModified: TNotifyEvent read fOnModified write fOnModified;
    property OnOIKeyDown: TKeyEvent read fOnOIKeyDown write fOnOIKeyDown;
    property OnSelectionChange: TNotifyEvent
      read fOnSelectionChange write fOnSelectionChange;
    property OnPropertyHint: TOIPropertyHintEvent
      read fOnPropertyHint write fOnPropertyHint;
    property PropertyEditorHook: TPropertyEditorHook
      read fPropertyEditorHook write SetPropertyEditorHook;
    property RowCount: integer read GetRowCount;
    property Rows[Index: integer]: TOIPropertyGridRow read GetRow;
    property RowSpacing: integer read fRowSpacing write SetRowSpacing;
    property Selection: TPersistentSelectionList read fSelection write SetSelection;
    property ShowGutter: boolean read fShowGutter write SetShowGutter default True;
    property CheckboxForBoolean: boolean read fCheckboxForBoolean
      write fCheckboxForBoolean;
    property PreferredSplitterX: integer read fPreferredSplitterX
      write fPreferredSplitterX default 100;
    property SplitterX: integer read fSplitterX write SetSplitterX default 100;
    property TopY: integer read fTopY write SetTopY default 0;
    property Favorites: TOIFavoriteProperties read fFavorites write SetFavorites;
    property Filter: TTypeKinds read fFilter write SetFilter;
    property HideClassNames: boolean read fHideClassNames write fHideClassNames;
    property PropNameFilter: string read fPropNameFilter write fPropNameFilter;
    property IObject: TPersistent read GetIObject write SetIObject;
    property AutoFreeHook: boolean read fAutoFreeHook write SetAutoFreeHook;
    property SaveOnChangeIObject: boolean read fSaveOnChangeIObject
                                           write fSaveOnChangeIObject
                                           default true;
    property Document: TIDocument read fDocument write SetDocument;
  end;


  { TObjectInspector }

  TObjectInspector = class(TCustomObjectInspector)
  published
    property Align;
    property Anchors;
    property BackgroundColor;
    property BorderStyle;
    property Constraints;
    property DefaultItemHeight;
    property DefaultValueFont;
    property Indent;
    property NameFont;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectionChange;
    property PopupMenu;
    property PreferredSplitterX;
    property SplitterX;
    property Tabstop;
    property ValueFont;
    property Visible;
  end;

  { TValueComboBox }

  TValueComboBox = class(TComboBox)
  public
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
  end;

// the ObjectInspector descendant of the IDE can be found in FormEditingIntf

function dbgs(s: TOIPropertyGridState): string; overload;
function dbgs(States: TOIPropertyGridStates): string; overload;

function GetChangeParentCandidates(PropertyEditorHook: TPropertyEditorHook;
  Selection: TPersistentSelectionList): TFPList;


implementation

uses
  Clipbrd, uGraphics;

{$R obj_inspector.res}

function SortGridRows(Item1, Item2: pointer): integer;
begin
  Result := SysUtils.CompareText(TOIPropertyGridRow(Item1).Name,
    TOIPropertyGridRow(Item2).Name);
end;

function dbgs(s: TOIPropertyGridState): string;
begin
  Result := GetEnumName(TypeInfo(s), Ord(s));
end;

function dbgs(States: TOIPropertyGridStates): string;
var
  s: TOIPropertyGridState;
begin
  Result := '';
  for s in States do
  begin
    if not (s in States) then
      continue;
    if Result <> '' then
      Result += ',';
    Result += dbgs(s);
  end;
  Result := '[' + Result + ']';
end;

function GetChangeParentCandidates(PropertyEditorHook: TPropertyEditorHook;
  Selection: TPersistentSelectionList): TFPList;

  function CanBeParent(Child, Parent: TPersistent): boolean;
  begin
    Result := False;
    if Child = Parent then
      exit;
    if not (Parent is TWinControl) then
      exit;
    if not (Child is TControl) then
      exit;
    if (Child is TWinControl) and (Child = TWinControl(Parent).Parent) then
      exit;
    if not ControlAcceptsStreamableChildComponent(TWinControl(Parent),
      TComponentClass(Child.ClassType), PropertyEditorHook.LookupRoot) then
      exit;
    try
      TControl(Child).CheckNewParent(TWinControl(Parent));
    except
      exit;
    end;
    Result := True;
  end;

  function CanBeParentOfSelection(Parent: TPersistent): boolean;
  var
    i: integer;
  begin
    for i := 0 to Selection.Count - 1 do
      if not CanBeParent(Selection[i], Parent) then
        exit(False);
    Result := True;
  end;

var
  i: integer;
  Candidate: TWinControl;
begin
  Result := TFPList.Create;
  if not (PropertyEditorHook.LookupRoot is TWinControl) then
    exit; // only LCL controls are supported at the moment

  // check if any selected control can be moved
  i := Selection.Count - 1;
  while i >= 0 do
  begin
    if (Selection[i] is TControl) and (TControl(Selection[i]).Owner =
      PropertyEditorHook.LookupRoot) then
      // this one can be moved
      break;
    Dec(i);
  end;
  if i < 0 then
    Exit;

  // find possible new parents
  for i := 0 to TWinControl(PropertyEditorHook.LookupRoot).ComponentCount - 1 do
  begin
    Candidate := TWinControl(TWinControl(PropertyEditorHook.LookupRoot).Components[i]);
    if CanBeParentOfSelection(Candidate) then
      Result.Add(Candidate);
  end;
  if CanBeParentOfSelection(PropertyEditorHook.LookupRoot) then
    Result.Add(PropertyEditorHook.LookupRoot);
end;

function CorrectClassName(editor: TPropertyEditor): string;
var
  p: PTypeInfo;
begin
  result := editor.GetVisualValue;
  // Extract Class name without prefix
  p := editor.GetPropType;
  if Assigned(p) and (p^.Kind = tkClass) and (Pos('(TI', result) = 1) then
    Delete(result, 2, 2);
end;

{ TValueComboBox }

procedure TValueComboBox.CopyToClipboard;
begin
  SendMessage(Handle, LM_COPY, 0, 0);
end;

procedure TValueComboBox.CutToClipboard;
begin
  SendMessage(Handle, LM_CUT, 0, 0);
end;

procedure TValueComboBox.PasteFromClipboard;
begin
  SendMessage(Handle, LM_PASTE, 0, 0);
end;

{ TCustomObjectInspector }

constructor TCustomObjectInspector.CreateWithParams(AnOwner: TComponent;
  TypeFilter: TTypeKinds; DefItemHeight: integer);
var
  Details: TThemedElementDetails;
  Hook: TPropertyEditorHook;
begin
  inherited Create(AnOwner);
  fLayout := oilHorizontal;

  fSelection := TPersistentSelectionList.Create;
  Hook := TPropertyEditorHook.Create(Self);
  fAutoFreeHook:=true;
  fSaveOnChangeIObject:=true;
  PropertyEditorHook := Hook;  // Through property setter.
  fFilter := TypeFilter;
  fItemIndex := -1;
  fStates := [];
  fColumn := oipgcValue;
  fRows := TFPList.Create;
  fExpandingRow := nil;
  fDragging := False;
  fExpandedProperties := TStringList.Create;
  fCurrentEdit := nil;
  fCurrentButton := nil;

  // visible values
  fTopY := 0;
  fSplitterX := 100;
  fPreferredSplitterX := fSplitterX;
  Details := ThemeServices.GetElementDetails(ttGlyphOpened);
  fIndent := ThemeServices.GetDetailSize(Details).cx;

  fBackgroundColor := DefBackgroundColor;
  fReferencesColor := DefReferencesColor;
  fSubPropertiesColor := DefSubPropertiesColor;
  fReadOnlyColor := DefReadOnlyColor;
  fHighlightColor := DefHighlightColor;
  fGutterColor := DefGutterColor;
  fGutterEdgeColor := DefGutterEdgeColor;
  fValueDifferBackgrndColor := DefValueDifferBackgrndColor;

  fNameFont := TFont.Create;
  fNameFont.Color := DefNameColor;
  fValueFont := TFont.Create;
  fValueFont.Color := DefValueColor;
  fDefaultValueFont := TFont.Create;
  fDefaultValueFont.Color := DefDefaultValueColor;
  fHighlightFont := TFont.Create;
  fHighlightFont.Color := DefHighlightFontColor;

  fDrawHorzGridLines := True;
  fShowGutter := True;

  SetInitialBounds(0, 0, 200, 130);
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];
  BorderWidth := 0;
  BorderStyle := bsSingle;

  // create sub components
  ValueEdit := TEdit.Create(Self);
  with ValueEdit do
  begin
    Name := 'ValueEdit';
    Visible := False;
    Enabled := False;
    AutoSize := False;
    SetBounds(0, -30, 80, 25); // hidden
    Parent := Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnDblClick := @ValueEditDblClick;
    OnExit := @ValueEditExit;
    OnChange := @ValueEditChange;
    OnKeyDown := @ValueEditKeyDown;
    OnKeyUp := @ValueEditKeyUp;
    OnMouseUp := @ValueEditMouseUp;
    OnMouseWheel := @OnGridMouseWheel;
  end;

  ValueComboBox := TValueComboBox.Create(Self);
  with ValueComboBox do
  begin
    Name := 'ValueComboBox';
    Sorted := True;
    AutoSelect := True;
    AutoComplete := True;
    Visible := False;
    Enabled := False;
    AutoSize := False;
    SetBounds(0, -30, Width, Height); // hidden
    DropDownCount := 20;
    ItemHeight := MulDiv(17, Screen.PixelsPerInch, 96);
    Parent := Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnDblClick := @ValueEditDblClick;
    OnExit := @ValueComboBoxExit;
    //OnChange:=@ValueComboBoxChange; the on change event is called even,
    // if the user is still editing
    OnKeyDown := @ValueComboBoxKeyDown;
    OnKeyUp := @ValueComboBoxKeyUp;
    OnMouseUp := @ValueComboBoxMouseUp;
    OnGetItems := @ValueComboBoxGetItems;
    OnCloseUp := @ValueComboBoxCloseUp;
    OnMeasureItem := @ValueComboBoxMeasureItem;
    OnDrawItem := @ValueComboBoxDrawItem;
    OnMouseWheel := @OnGridMouseWheel;
  end;

  ValueCheckBox :=
{$IFnDEF UseOINormalCheckBox}
    TCheckBoxThemed.Create(Self);
{$ELSE}
TCheckBox.Create(Self);
{$ENDIF}
  with ValueCheckBox do
  begin
    Name := 'ValueCheckBox';
    Visible := False;
    Enabled := False;
    {$IFnDEF UseOINormalCheckBox}
    AutoSize := False;
    {$ELSE}
    AutoSize := True;    // SetBounds does not work for CheckBox, AutoSize does.
    {$ENDIF}
    Parent := Self;
    Top := -30;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnExit := @ValueCheckBoxExit;
    OnKeyDown := @ValueCheckBoxKeyDown;
    OnKeyUp := @ValueCheckBoxKeyUp;
    OnClick := @ValueCheckBoxClick;
    OnMouseWheel := @OnGridMouseWheel;
  end;

  ValueButton := TSpeedButton.Create(Self);
  with ValueButton do
  begin
    Name := 'ValueButton';
    Visible := False;
    Enabled := False;
    Transparent := False;
    OnClick := @ValueButtonClick;
    Caption := '...';
    SetBounds(0, -30, Width, Height); // hidden
    Parent := Self;
    OnMouseWheel := @OnGridMouseWheel;
  end;

  fHintManager := THintWindowManager.Create;
  fActiveRowImages := TLCLGlyphs.Create(Self);
  fActiveRowImages.Width := 9;
  fActiveRowImages.Height := 9;
  fActiveRowImages.RegisterResolutions([9, 13, 18], [100, 150, 200]);
  fActiveRowImages.OnGetWidthForPPI := @ActiveRowImagesGetWidthForPPI;

  fDefaultItemHeight := DefItemHeight;
  CheckboxForBoolean := True;
  BuildPropertyList;
end;

procedure TCustomObjectInspector.ActiveRowImagesGetWidthForPPI(
  Sender: TCustomImageList;
  AImageWidth, APPI: integer; var AResultWidth: integer);
begin
  if (12 <= AResultWidth) and (AResultWidth <= 16) then
    AResultWidth := 13;
end;

constructor TCustomObjectInspector.Create(TheOwner: TComponent);
begin
  CreateWithParams(TheOwner, AllTypeKinds, 0);
end;

destructor TCustomObjectInspector.Destroy;
var
  a: integer;
begin
  SetIdleEvent(False);
  fItemIndex := -1;
  for a := 0 to fRows.Count - 1 do
    Rows[a].Free;
  FreeAndNil(fRows);
  FreeAndNil(fSelection);
  FreeAndNil(fValueFont);
  FreeAndNil(fDefaultValueFont);
  FreeAndNil(fNameFont);
  FreeAndNil(fHighlightFont);
  FreeAndNil(fExpandedProperties);
  FreeAndNil(fLongHintTimer);
  FreeAndNil(fHintManager);
  FreeAndNil(fNewComboBoxItems);
  if fAutoFreeHook then
    FreeAndNil(fPropertyEditorHook);
  inherited Destroy;
end;

function TCustomObjectInspector.InitHints: boolean;
begin
  if not ShowHint then
    exit(False);

  Result := True;
  if fLongHintTimer = nil then
  begin
    fHintIndex := -1;
    fShowingLongHint := False;
    fLongHintTimer := TTimer.Create(nil);
    fLongHintTimer.Interval := 500;
    fLongHintTimer.Enabled := False;
    fLongHintTimer.OnTimer := @HintTimer;

    fHintManager.OnMouseDown := @HintMouseDown;
    fHintManager.WindowName := 'This_is_a_hint_window';
    fHintManager.HideInterval := 4000;
    fHintManager.AutoHide := True;
  end;
end;

procedure TCustomObjectInspector.UpdateScrollBar;
var
  si: TScrollInfo;
  ATopMax: integer;
begin
  if HandleAllocated then
  begin
    ATopMax := TopMax;
    si.cbSize := SizeOf(si);
    si.fMask := SIF_ALL; //  or SIF_DISABLENOSCROLL
    si.nMin := 0;
    si.nTrackPos := 0;
    si.nMax := ATopMax + ClientHeight - 1;
    if ClientHeight < 2 then
      si.nPage := 1
    else
      si.nPage := ClientHeight - 1;
    if TopY > ATopMax then
      TopY := ATopMax;
    si.nPos := TopY;
    //ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, si, True);
  end;
end;

function TCustomObjectInspector.FillComboboxItems: boolean;
var
  ExcludeUpdateFlag: boolean;
  CurRow: TOIPropertyGridRow;
begin
  Result := False;
  ExcludeUpdateFlag := not (pgsUpdatingEditControl in fStates);
  Include(fStates, pgsUpdatingEditControl);
  ValueComboBox.Items.BeginUpdate;
  try
    CurRow := Rows[fItemIndex];
    if fNewComboBoxItems <> nil then
      fNewComboBoxItems.Clear;
    CurRow.Editor.GetValues(@AddStringToComboBox);
    if fNewComboBoxItems <> nil then
    begin
      fNewComboBoxItems.Sorted := paSortList in CurRow.Editor.GetAttributes;
      if ValueComboBox.Items.Equals(fNewComboBoxItems) then
        exit;
      ValueComboBox.Items.Assign(fNewComboBoxItems);
      //debugln('TCustomObjectInspector.FillComboboxItems "',fNewComboBoxItems.Text,'" Cur="',ValueComboBox.Items.Text,'" ValueComboBox.Items.Count=',dbgs(ValueComboBox.Items.Count));
    end
    else if ValueComboBox.Items.Count = 0 then
    begin
      exit;
    end
    else
    begin
      ValueComboBox.Items.Text := '';
      ValueComboBox.Items.Clear;
      //debugln('TCustomObjectInspector.FillComboboxItems FNewComboBoxItems=nil Cur="',ValueComboBox.Items.Text,'" ValueComboBox.Items.Count=',dbgs(ValueComboBox.Items.Count));
    end;
    Result := True;
    //debugln(['TCustomObjectInspector.FillComboboxItems CHANGED']);
  finally
    FreeAndNil(fNewComboBoxItems);
    ValueComboBox.Items.EndUpdate;
    if ExcludeUpdateFlag then
      Exclude(fStates, pgsUpdatingEditControl);
  end;
end;

procedure TCustomObjectInspector.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
    {$R-}
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or WS_VSCROLL or WS_CLIPCHILDREN;
    {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TCustomObjectInspector.CreateWnd;
begin
  inherited CreateWnd;
  // handle just created, set scrollbar
  UpdateScrollBar;
end;

procedure TCustomObjectInspector.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
    // Scrolls to start / end of the text
    SB_TOP: TopY := 0;
    SB_BOTTOM: TopY := TopMax;
    // Scrolls one line up / down
    SB_LINEDOWN: TopY := TopY + RealDefaultItemHeight div 2;
    SB_LINEUP: TopY := TopY - RealDefaultItemHeight div 2;
    // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopY := TopY + ClientHeight - RealDefaultItemHeight;
    SB_PAGEUP: TopY := TopY - ClientHeight + RealDefaultItemHeight;
    // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: TopY := Msg.Pos;
    // Ends scrolling
    SB_ENDSCROLL: SetCaptureControl(nil); // release scrollbar capture
  end;
end;

function TCustomObjectInspector.DoMouseWheel(Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint): boolean;
var
  H: boolean;
begin
  H := False;
  OnGridMouseWheel(Self, Shift, WheelDelta, MousePos, H);
  Result := True;
end;

procedure TCustomObjectInspector.OnGridMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if Mouse.WheelScrollLines = -1 then
    // -1 : scroll by page
    TopY := TopY - (WheelDelta * (ClientHeight - RealDefaultItemHeight)) div 120
  else
    // scrolling one line -> scroll half an item, see SB_LINEDOWN and SB_LINEUP
    // handler in WMVScroll
    TopY := TopY - (WheelDelta * Mouse.WheelScrollLines * RealDefaultItemHeight) div 240;
  Handled := True;
end;

function TCustomObjectInspector.IsCurrentEditorAvailable: boolean;
begin
  Result := (fCurrentEdit <> nil) and InRange(fItemIndex, 0, fRows.Count - 1);
end;

procedure TCustomObjectInspector.FocusCurrentEditor;
begin
  if (IsCurrentEditorAvailable) and (fCurrentEdit.CanFocus) then
  begin
    fCurrentEdit.SetFocus;
    if (fCurrentEdit is TEdit) then
      (fCurrentEdit as TEdit).SelStart := Length((fCurrentEdit as TEdit).Text);
  end;
end;

function TCustomObjectInspector.ConsistencyCheck: integer;
var
  i: integer;
begin
  for i := 0 to fRows.Count - 1 do
  begin
    if Rows[i] = nil then
    begin
      Result := -1;
      exit;
    end;
    if Rows[i].Index <> i then
    begin
      Result := -2;
      exit;
    end;
    Result := Rows[i].ConsistencyCheck;
    if Result <> 0 then
    begin
      Dec(Result, 100);
      exit;
    end;
  end;
  Result := 0;
end;

procedure TCustomObjectInspector.SetSelection(
  const ASelection: TPersistentSelectionList);
var
  CurRow: TOIPropertyGridRow;
  OldSelectedRowPath: string;
begin
  if ASelection = nil then
    exit;
  if (not ASelection.ForceUpdate) and fSelection.IsEqual(ASelection) then
    exit;

  OldSelectedRowPath := PropertyPath(ItemIndex);
  if fCurrentEdit = ValueEdit then
    ValueEditExit(Self);
  ItemIndex := -1;
  ClearRows;
  fSelection.Assign(ASelection);
  BuildPropertyList;
  CurRow := GetRowByPath(OldSelectedRowPath);
  if CurRow <> nil then
    ItemIndex := CurRow.Index;
  Column := oipgcValue;
  Invalidate;
  DoSelectionChange;
end;

procedure TCustomObjectInspector.SetPropertyEditorHook(
  NewPropertyEditorHook: TPropertyEditorHook);
begin
  if fPropertyEditorHook = NewPropertyEditorHook then
    exit;
  fPropertyEditorHook := NewPropertyEditorHook;
  fPropertyEditorHook.AddHandlerGetCheckboxForBoolean(@HookGetCheckboxForBoolean);
  IncreaseChangeStep;
  SetSelection(fSelection);
end;

procedure TCustomObjectInspector.HookGetCheckboxForBoolean(var Value: boolean);
begin
  Value := fCheckboxForBoolean;
end;

function TCustomObjectInspector.PropertyPath(Index: integer): string;
begin
  if (Index >= 0) and (Index < fRows.Count) then
  begin
    Result := PropertyPath(Rows[Index]);
  end
  else
    Result := '';
end;

function TCustomObjectInspector.PropertyPath(Row: TOIPropertyGridRow): string;
begin
  if Row = nil then
  begin
    Result := '';
    exit;
  end;
  Result := Row.Name;
  Row := Row.Parent;
  while Row <> nil do
  begin
    Result := Row.Name + '.' + Result;
    Row := Row.Parent;
  end;
end;

function TCustomObjectInspector.RealDefaultItemHeight: integer;
begin
  Result := fDefaultItemHeight;
  if (Result <= 0) then
    Result := Scale96ToForm(22);
end;

function TCustomObjectInspector.GetRowByPath(const PropPath: string): TOIPropertyGridRow;
  // searches PropPath. Expands automatically parent rows
var
  CurName: string;
  s, e: integer;
  CurParentRow: TOIPropertyGridRow;
begin
  Result := nil;
  if (PropPath = '') or (fRows.Count = 0) then
    exit;
  CurParentRow := nil;
  s := 1;
  while (s <= length(PropPath)) do
  begin
    e := s;
    while (e <= length(PropPath)) and (PropPath[e] <> '.') do
      Inc(e);
    CurName := uppercase(copy(PropPath, s, e - s));
    s := e + 1;
    // search name in children
    if CurParentRow = nil then
      Result := Rows[0]
    else
      Result := CurParentRow.FirstChild;
    while (Result <> nil) and (uppercase(Result.Name) <> CurName) do
      Result := Result.NextBrother;
    if Result = nil then
    begin
      exit;
    end
    else
    begin
      // expand row
      CurParentRow := Result;
      if s <= length(PropPath) then
        ExpandRow(CurParentRow.Index);
    end;
  end;
  if s <= length(PropPath) then
    Result := nil;
end;

procedure TCustomObjectInspector.SetRowValue(CheckFocus, ForceValue: boolean);

  function GetPropValue(Editor: TPropertyEditor; Index: integer): string;
  var
    PropKind: TTypeKind;
    PropInfo: PPropInfo;
    BoolVal: boolean;
  begin
    Result := '';
    PropInfo := Editor.GetPropInfo;
    PropKind := PropInfo^.PropType^.Kind;
    case PropKind of
      tkInteger, tkInt64:
        Result := IntToStr(Editor.GetInt64ValueAt(Index));
      tkChar, tkWChar, tkUChar:
        Result := char(Editor.GetOrdValueAt(Index));
      tkEnumeration:
        Result := GetEnumName(PropInfo^.PropType, Editor.GetOrdValueAt(Index));
      tkFloat:
        Result := FloatToStr(Editor.GetFloatValueAt(Index));
      tkBool:
      begin
        BoolVal := boolean(Editor.GetOrdValueAt(Index));
        if fCheckboxForBoolean then
          Result := BoolToStr(BoolVal, '(True)', '(False)')
        else
          Result := BoolToStr(BoolVal, 'True', 'False');
      end;
      tkString, tkLString, tkAString, tkUString, tkWString:
        Result := Editor.GetStrValueAt(Index);
      tkSet:
        Result := Editor.GetSetValueAt(Index, True);
      tkVariant:
        if Editor.GetVarValueAt(Index) <> Null then
          Result := Editor.GetVarValueAt(Index)
        else
          Result := '(Null)';
    end;
  end;

var
  CurRow: TOIPropertyGridRow;
  NewValue: string;
  OldExpanded: boolean;
  OldChangeStep: integer;
  APersistent: TPersistent;
  i: integer;
  NewVal: string;
  oldVal: array of string;
  isExcept: boolean;
  prpInfo: PPropInfo;
  Editor: TPropertyEditor;
begin
  //if fItemIndex > -1 then
  //  debugln(['TCustomObjectInspector.SetRowValue A, FItemIndex=',dbgs(fItemIndex),
  //    ', CanEditRowValue=', CanEditRowValue(CheckFocus), ', IsReadOnly=', Rows[fItemIndex].IsReadOnly]);
  if csDestroying in ComponentState then
    exit;
  if not CanEditRowValue(CheckFocus) or Rows[fItemIndex].IsReadOnly then
    exit;

  NewValue := GetCurrentEditValue;
  CurRow := Rows[fItemIndex];
  if length(NewValue) > CurRow.Editor.GetEditLimit then
    NewValue := LeftStr(NewValue, CurRow.Editor.GetEditLimit);

  //DebugLn(['TCustomObjectInspector.SetRowValue Old="',CurRow.Editor.GetVisualValue,'" New="',NewValue,'"']);
  if (CurRow.Editor.GetVisualValue = NewValue) and not ForceValue then
    exit;


  // store old values for undo
  isExcept := False;
  Editor := CurRow.Editor;
  prpInfo := nil;
  if fDocument <> nil then
  begin
    SetLength(oldVal, Editor.PropCount);
    prpInfo := Editor.GetPropInfo;
    if prpInfo <> nil then
    begin
      for i := 0 to Editor.PropCount - 1 do
        oldVal[i] := GetPropValue(Editor, i);
    end;
  end;

  if Assigned(fDocument) then
    fDocument.SelectionPropertyChange(Editor.GetName);

  OldChangeStep := fChangeStep;
  Include(fStates, pgsApplyingValue);
  try
    {$IFNDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}
      //debugln(['TCustomObjectInspector.SetRowValue B ClassName=',CurRow.Editor.ClassName,' Visual="',CurRow.Editor.GetVisualValue,'" NewValue="',NewValue,'" AllEqual=',CurRow.Editor.AllEqual]);
      CurRow.Editor.SetValue(NewValue);
      //debugln(['TCustomObjectInspector.SetRowValue C ClassName=',CurRow.Editor.ClassName,' Visual="',CurRow.Editor.GetVisualValue,'" NewValue="',NewValue,'" AllEqual=',CurRow.Editor.AllEqual]);
    {$IFNDEF DoNotCatchOIExceptions}
    except
      on E: Exception do
      begin
        MessageDlg(oisError, E.Message, mtError, [mbOK], 0);
        isExcept := True;
      end;
    end;
    {$ENDIF}
    if (OldChangeStep <> fChangeStep) then
    begin
      // the selection has changed => CurRow does not exist any more
      exit;
    end;

    // add Undo action on except
    if (fDocument <> nil) then
    begin
      fDocument.SelectionPropertyCommit(not isExcept);
    end;

    // set value in edit control
    SetCurrentEditValue(CorrectClassName(Editor));

    // update volatile sub properties
    if (paVolatileSubProperties in Editor.GetAttributes) and
      ((CurRow.Expanded) or (CurRow.ChildCount > 0)) then
    begin
      OldExpanded := CurRow.Expanded;
      ShrinkRow(fItemIndex);
      if OldExpanded then
        ExpandRow(fItemIndex);
    end;
    //debugln(['TCustomObjectInspector.SetRowValue D ClassName=',CurRow.Editor.ClassName,' Visual="',CurRow.Editor.GetVisualValue,'" NewValue="',NewValue,'" AllEqual=',CurRow.Editor.AllEqual]);
  finally
    Exclude(fStates, pgsApplyingValue);
  end;
  if Assigned(fPropertyEditorHook) then
    fPropertyEditorHook.RefreshPropertyValues;
  if Assigned(fOnModified) then
    fOnModified(Self);
end;

procedure TCustomObjectInspector.DoCallEdit(Edit: TOIQuickEdit);
var
  CurRow: TOIPropertyGridRow;
  OldChangeStep: integer;
begin
  //debugln(['TCustomObjectInspector.DoCallEdit ',dbgs(GetFocus),' ',DbgSName(FindControl(GetFocus))]);
  if not CanEditRowValue(False) then
    exit;

  OldChangeStep := fChangeStep;
  CurRow := Rows[fItemIndex];
  if paDialog in CurRow.Editor.GetAttributes then
  begin
    {$IFnDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}
      //if fSelection.Count > 0 then
      //  DebugLn(['# TCustomObjectInspector.DoCallEdit for ', CurRow.Editor.ClassName,
      //           ', Edit=', Edit=oiqeEdit, ', SelectionCount=', fSelection.Count,
      //           ', SelectionName=', fSelection[0].GetNamePath]);
      Include(fStates, pgsCallingEdit);
      try
        if Edit = oiqeShowValue then
          CurRow.Editor.ShowValue
        else if (fSelection.Count > 0) and (fSelection[0] is TComponent) then
          CurRow.Editor.Edit(TComponent(fSelection[0]))
        else
          CurRow.Editor.Edit;
      finally
        Exclude(fStates, pgsCallingEdit);
      end;
    {$IFnDEF DoNotCatchOIExceptions}
    except
      on E: Exception do
        MessageDlg(oisError, E.Message, mtError, [mbOK], 0);
    end;
    {$ENDIF}
    // CurRow is now invalid, do not access CurRow

    if (OldChangeStep <> fChangeStep) then
    begin
      // the selection has changed => CurRow does not exist any more
      RefreshPropertyValues;
      exit;
    end;
    RefreshValueEdit;       // update value
    Invalidate;             //invalidate changed subproperties
  end;
end;

procedure TCustomObjectInspector.RefreshValueEdit;
var
  CurRow: TOIPropertyGridRow;
  NewValue: string;
begin
  if not GridIsUpdating and IsCurrentEditorAvailable then
  begin
    CurRow := Rows[fItemIndex];
    NewValue := CorrectClassName(CurRow.Editor);
    {$IFDEF LCLCarbon}
    NewValue := StringReplace(NewValue, LineEnding, LineFeedSymbolUTF8, [rfReplaceAll]);
    {$ENDIF}
    SetCurrentEditValue(NewValue);
  end;
end;

procedure TCustomObjectInspector.ValueEditKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key, Shift);
end;

procedure TCustomObjectInspector.ValueEditKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  HandleKeyUp(Key, Shift);
end;

procedure TCustomObjectInspector.ValueEditExit(Sender: TObject);
begin
  SetRowValue(False, False);
end;

procedure TCustomObjectInspector.ValueEditChange(Sender: TObject);
var
  CurRow: TOIPropertyGridRow;
begin
  if (pgsUpdatingEditControl in fStates) or not IsCurrentEditorAvailable then
    exit;
  CurRow := Rows[fItemIndex];
  if paAutoUpdate in CurRow.Editor.GetAttributes then
    SetRowValue(True, True);
end;

procedure TCustomObjectInspector.ValueEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and (Shift = [ssCtrl, ssLeft]) then
    DoCallEdit(oiqeShowValue);
end;

procedure TCustomObjectInspector.ValueCheckBoxKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key, Shift);
end;

procedure TCustomObjectInspector.ValueCheckBoxKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  HandleKeyUp(Key, Shift);
end;

procedure TCustomObjectInspector.ValueCheckBoxExit(Sender: TObject);
begin
  SetRowValue(False, False);
end;

procedure TCustomObjectInspector.ValueCheckBoxClick(Sender: TObject);
begin
  if (pgsUpdatingEditControl in fStates) or not IsCurrentEditorAvailable then
    exit;
  ValueCheckBox.Caption := BoolToStr(ValueCheckBox.Checked, '(True)', '(False)');
  SetRowValue(True, True);
end;

procedure TCustomObjectInspector.ValueComboBoxExit(Sender: TObject);
begin
  if pgsUpdatingEditControl in fStates then
    exit;
  SetRowValue(False, False);
end;

procedure TCustomObjectInspector.ValueComboBoxKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key, Shift);
end;

procedure TCustomObjectInspector.ValueComboBoxKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  HandleKeyUp(Key, Shift);
end;

procedure TCustomObjectInspector.ValueComboBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
  begin
    if (Shift = [ssCtrl, ssLeft]) then
      DoCallEdit(oiqeShowValue)
    else if (fFirstClickTime <> 0) and (GetTickCount <= fFirstClickTime +
      GetDoubleClickTime) and (not ValueComboBox.DroppedDown) then
    begin
      fFirstClickTime := 0;
      ToggleRow;
    end;
  end;
end;

procedure TCustomObjectInspector.ValueButtonClick(Sender: TObject);
begin
  ScrollToActiveItem;
  DoCallEdit;
end;

procedure TCustomObjectInspector.ValueComboBoxMeasureItem(Control: TWinControl;
  Index: integer; var AHeight: integer);
var
  CurRow: TOIPropertyGridRow;
begin
  if (fItemIndex >= 0) and (fItemIndex < fRows.Count) then
  begin
    CurRow := Rows[fItemIndex];
    CurRow.Editor.ListMeasureHeight('Fj', Index, ValueComboBox.Canvas, AHeight);
    AHeight := Max(AHeight, ValueComboBox.ItemHeight);
  end;
end;

procedure TCustomObjectInspector.SetCheckboxState(NewValue: string);
begin
  ValueCheckBox.Caption := NewValue;
  if (NewValue = '') or (NewValue = oisMixed) then
    ValueCheckBox.State := cbGrayed
  else if NewValue = '(True)' then
    ValueCheckBox.State := cbChecked
  // Note: this condition can be removed when the right propedit is used always.
  else if NewValue = '(False)' then
    ValueCheckBox.State := cbUnchecked;
end;

procedure TCustomObjectInspector.SetItemIndex(NewIndex: integer);
var
  NewRow: TOIPropertyGridRow;
  NewValue: string;
  EditorAttributes: TPropertyAttributes;
begin
  {if pgsCallingEdit in fStates then begin
    DumpStack;
    debugln(['TCustomObjectInspector.SetItemIndex ',DbgSName(Self),' ',dbgsname(fCurrentEdit),' ',dbgs(fStates),' GridIsUpdating=',GridIsUpdating,' FItemIndex=',fItemIndex,' NewIndex=',NewIndex]);
  end;}
  if GridIsUpdating or (fItemIndex = NewIndex) then
    exit;
  // save old edit value
  SetRowValue(True, False);

  Include(fStates, pgsChangingItemIndex);
  if (fItemIndex >= 0) and (fItemIndex < fRows.Count) then
    Rows[fItemIndex].Editor.Deactivate;
  if CanFocus then
    SetCaptureControl(nil);

  fItemIndex := NewIndex;
  if fCurrentEdit <> nil then
  begin
    fCurrentEdit.Visible := False;
    fCurrentEdit.Enabled := False;
    fCurrentEdit := nil;
  end;
  if fCurrentButton <> nil then
  begin
    fCurrentButton.Visible := False;
    fCurrentButton.Enabled := False;
    fCurrentButton := nil;
  end;
  fCurrentEditorLookupRoot := nil;
  if (NewIndex >= 0) and (NewIndex < fRows.Count) then
  begin
    NewRow := Rows[NewIndex];
    ScrollToItem(NewIndex);
    if CanFocus then
      NewRow.Editor.Activate;
    EditorAttributes := NewRow.Editor.GetAttributes;
    // or check TIFont
    if (paDialog in EditorAttributes)  then
    begin
      fCurrentButton := ValueButton;
      fCurrentButton.Visible := True;
      //DebugLn(['TCustomObjectInspector.SetItemIndex FCurrentButton.BoundsRect=',dbgs(fCurrentButton.BoundsRect)]);
    end;
    NewValue := CorrectClassName(NewRow.Editor);
    if ((NewRow.Editor is TBoolPropertyEditor) or (NewRow.Editor is
      TSetElementPropertyEditor)) and fCheckboxForBoolean then
    begin
      fCurrentEdit := ValueCheckBox;
      ValueCheckBox.Enabled := not NewRow.IsReadOnly;
      SetCheckboxState(NewValue);
    end
    else if paValueList in EditorAttributes then
    begin
      fCurrentEdit := ValueComboBox;
      if (paCustomDrawn in EditorAttributes) and (paPickList in EditorAttributes) then
        ValueComboBox.Style := csOwnerDrawVariable
      else
      if paCustomDrawn in EditorAttributes then
        ValueComboBox.Style := csOwnerDrawEditableVariable
      else if paPickList in EditorAttributes then
        ValueComboBox.Style := csOwnerDrawFixed
      else
        ValueComboBox.Style := csOwnerDrawEditableFixed;
      ValueComboBox.MaxLength := NewRow.Editor.GetEditLimit;
      ValueComboBox.Sorted := paSortList in NewRow.Editor.GetAttributes;
      ValueComboBox.Enabled := not NewRow.IsReadOnly;
      // Do not fill the items here, because it can be very slow.
      // Just fill in some values and update the values before the combobox popups
      ValueComboBox.Items.Text := NewValue;
      Exclude(fStates, pgsGetComboItemsCalled);
      SetIdleEvent(True);
      ValueComboBox.Text := NewValue;
    end
    else
    begin
      fCurrentEdit := ValueEdit;
      ValueEdit.ReadOnly := NewRow.IsReadOnly;
      ValueEdit.Enabled := True;
      ValueEdit.MaxLength := NewRow.Editor.GetEditLimit;
      ValueEdit.Text := NewValue;
    end;
    AlignEditComponents;
    if fCurrentEdit <> nil then
    begin
      if fPropertyEditorHook <> nil then
        fCurrentEditorLookupRoot := fPropertyEditorHook.LookupRoot;
      if (fCurrentEdit = ValueComboBox) or (fCurrentEdit = ValueEdit) then
      begin
        if NewRow.Editor.AllEqual then
          fCurrentEdit.Color := clWindow
        else
          fCurrentEdit.Color := fValueDifferBackgrndColor;
      end;
      if NewRow.Editor.ValueIsStreamed then
        fCurrentEdit.Font := fValueFont
      else
        fCurrentEdit.Font := fDefaultValueFont;
      fCurrentEdit.Visible := True;
      if (fDragging = False) and fCurrentEdit.Showing and fCurrentEdit.Enabled and
        (not NewRow.IsReadOnly) and CanFocus and (Column = oipgcValue) and
        not (pgsFocusPropertyEditorDisabled in fStates) then
        SetActiveControl(fCurrentEdit);
    end;
    if fCurrentButton <> nil then
      fCurrentButton.Enabled := not NewRow.IsDisabled;
  end;
  //DebugLn(['TCustomObjectInspector.SetItemIndex Vis=',ValueComboBox.Visible,' Ena=',ValueComboBox.Enabled,
  //         ' Items.Count=',ValueComboBox.Items.Count ,' Text=',ValueComboBox.Text]);
  Exclude(fStates, pgsChangingItemIndex);
  DoSelectionChange;
  Invalidate;
end;

function TCustomObjectInspector.GetNameRowHeight: integer;
begin
  Result := Abs(fNameFont.Height);
  if Result = 0 then
    Result := 16;
  Inc(Result, 2); // margin
end;

function TCustomObjectInspector.GetRowCount: integer;
begin
  Result := fRows.Count;
end;

procedure TCustomObjectInspector.BuildPropertyList(OnlyIfNeeded: boolean;
  FocusEditor: boolean);
var
  a: integer;
  CurRow: TOIPropertyGridRow;
  OldSelectedRowPath: string;
begin
  if OnlyIfNeeded and (not (pgsBuildPropertyListNeeded in fStates)) then
    exit;
  Exclude(fStates, pgsBuildPropertyListNeeded);
  if not FocusEditor then
    Include(fStates, pgsFocusPropertyEditorDisabled);
  OldSelectedRowPath := PropertyPath(ItemIndex);
  // unselect
  ItemIndex := -1;
  // clear
  for a := 0 to fRows.Count - 1 do
    Rows[a].Free;
  fRows.Clear;
  // get properties
  if fSelection.Count > 0 then
  begin
    GetPersistentProperties(fSelection, fFilter + [tkClass],
      fPropertyEditorHook, @AddPropertyEditor, @EditorFilter);
  end;
  // sort
  fRows.Sort(@SortGridRows);
  for a := 0 to fRows.Count - 1 do
  begin
    if a > 0 then
      Rows[a].FPriorBrother := Rows[a - 1]
    else
      Rows[a].FPriorBrother := nil;
    if a < fRows.Count - 1 then
      Rows[a].FNextBrother := Rows[a + 1]
    else
      Rows[a].FNextBrother := nil;
  end;
  // set indices and tops
  SetItemsTops;
  // restore expands
  for a := fExpandedProperties.Count - 1 downto 0 do
  begin
    CurRow := GetRowByPath(fExpandedProperties[a]);
    if CurRow <> nil then
      ExpandRow(CurRow.Index);
  end;
  // update scrollbar
  fTopY := 0;
  UpdateScrollBar;
  // reselect
  CurRow := GetRowByPath(OldSelectedRowPath);
  if CurRow <> nil then
    ItemIndex := CurRow.Index;
  Exclude(fStates, pgsFocusPropertyEditorDisabled);
  // paint
  Invalidate;
end;

procedure TCustomObjectInspector.AddPropertyEditor(PropEditor: TPropertyEditor);
var
  NewRow: TOIPropertyGridRow;
  WidgetSets: TLCLPlatforms;
begin
  WidgetSets := [];
  if Favorites <> nil then
  begin
    //debugln('TCustomObjectInspector.AddPropertyEditor A ',PropEditor.GetName);
    if Favorites is TOIRestrictedProperties then
    begin
      WidgetSets := (Favorites as TOIRestrictedProperties).AreRestricted(
        Selection, PropEditor.GetName);
      if WidgetSets = [] then
      begin
        PropEditor.Free;
        Exit;
      end;
    end
    else
    if not Favorites.AreFavorites(Selection, PropEditor.GetName) then
    begin
      PropEditor.Free;
      exit;
    end;
  end;
  if PropEditor is TClassPropertyEditor then
  begin
    (PropEditor as TClassPropertyEditor).SubPropsNameFilter := PropNameFilter;
    (PropEditor as TClassPropertyEditor).SubPropsTypeFilter := fFilter;
    (PropEditor as TClassPropertyEditor).HideClassName := fHideClassNames;
  end;
  NewRow := TOIPropertyGridRow.Create(Self, PropEditor, nil, WidgetSets);
  fRows.Add(NewRow);
  if fRows.Count > 1 then
  begin
    NewRow.FPriorBrother := Rows[fRows.Count - 2];
    NewRow.FPriorBrother.FNextBrother := NewRow;
  end;
end;

procedure TCustomObjectInspector.AddStringToComboBox(const s: string);
begin
  if fNewComboBoxItems = nil then
    fNewComboBoxItems := TStringList.Create;
  fNewComboBoxItems.Add(s);
end;

procedure TCustomObjectInspector.ExpandRow(Index: integer);
var
  a: integer;
  CurPath: string;
  AlreadyInExpandList: boolean;
  ActiveRow: TOIPropertyGridRow;
begin
  // Save ItemIndex
  if ItemIndex <> -1 then
    ActiveRow := Rows[ItemIndex]
  else
    ActiveRow := nil;
  fExpandingRow := Rows[Index];
  if (fExpandingRow.Expanded) or (not CanExpandRow(fExpandingRow)) then
  begin
    fExpandingRow := nil;
    Exit;
  end;
  fExpandingRow.Editor.GetProperties(@AddSubEditor);
  SortSubEditors(fExpandingRow);
  SetItemsTops;
  fExpandingRow.FExpanded := True;
  a := 0;
  CurPath := uppercase(PropertyPath(fExpandingRow.Index));
  AlreadyInExpandList := False;
  while a < fExpandedProperties.Count do
  begin
    if fExpandedProperties[a] = copy(CurPath, 1, length(fExpandedProperties[a])) then
    begin
      if Length(fExpandedProperties[a]) = Length(CurPath) then
      begin
        AlreadyInExpandList := True;
        Inc(a);
      end
      else
        fExpandedProperties.Delete(a);
    end
    else
      Inc(a);
  end;
  if not AlreadyInExpandList then
    fExpandedProperties.Add(CurPath);
  fExpandingRow := nil;
  // restore ItemIndex
  if ActiveRow <> nil then
    fItemIndex := ActiveRow.Index
  else
    fItemIndex := -1;
  UpdateScrollBar;
  Invalidate;
end;

procedure TCustomObjectInspector.ShrinkRow(Index: integer);
var
  CurRow, ARow: TOIPropertyGridRow;
  StartIndex, EndIndex, a: integer;
  CurPath: string;
begin
  CurRow := Rows[Index];
  if (not CurRow.Expanded) then
    Exit;
  // calculate all children (between StartIndex..EndIndex)
  StartIndex := CurRow.Index + 1;
  EndIndex := fRows.Count - 1;
  ARow := CurRow;
  while ARow <> nil do
  begin
    if ARow.NextBrother <> nil then
    begin
      EndIndex := ARow.NextBrother.Index - 1;
      break;
    end;
    ARow := ARow.Parent;
  end;
  if (fItemIndex >= StartIndex) and (fItemIndex <= EndIndex) then
    // current row delete, set new current row
    ItemIndex := 0
  else
  if fItemIndex > EndIndex then
    // adjust current index for deleted rows
    fItemIndex := fItemIndex - (EndIndex - StartIndex + 1);
  for a := EndIndex downto StartIndex do
  begin
    Rows[a].Free;
    fRows.Delete(a);
  end;
  SetItemsTops;
  CurRow.FExpanded := False;
  CurPath := UpperCase(PropertyPath(CurRow.Index));
  a := 0;
  while a < fExpandedProperties.Count do
  begin
    if copy(fExpandedProperties[a], 1, length(CurPath)) = CurPath then
      fExpandedProperties.Delete(a)
    else
      Inc(a);
  end;
  if CurRow.Parent <> nil then
    fExpandedProperties.Add(UpperCase(PropertyPath(CurRow.Parent.Index)));
  UpdateScrollBar;
  Invalidate;
end;

procedure TCustomObjectInspector.AddSubEditor(PropEditor: TPropertyEditor);
var
  NewRow: TOIPropertyGridRow;
  NewIndex: integer;
begin
  if not EditorFilter(PropEditor) then
  begin
    // if some elements of a set is not being shown then free their editor
    // to avoid memory leaks; sine only visible editors will be cleared.
    if PropEditor.ClassType = TSetElementPropertyEditor then
      PropEditor.Free;
    Exit;
  end;


  if PropEditor is TClassPropertyEditor then
  begin
    (PropEditor as TClassPropertyEditor).SubPropsNameFilter := PropNameFilter;
    (PropEditor as TClassPropertyEditor).SubPropsTypeFilter := fFilter;
    (PropEditor as TClassPropertyEditor).HideClassName := fHideClassNames;
  end;
  NewRow := TOIPropertyGridRow.Create(Self, PropEditor, fExpandingRow, []);
  NewIndex := fExpandingRow.Index + 1 + fExpandingRow.ChildCount;
  NewRow.FIndex := NewIndex;
  fRows.Insert(NewIndex, NewRow);
  if NewIndex < fItemIndex then
    Inc(fItemIndex);
  if fExpandingRow.FFirstChild = nil then
    fExpandingRow.FFirstChild := NewRow;
  NewRow.FPriorBrother := fExpandingRow.FLastChild;
  fExpandingRow.FLastChild := NewRow;
  if NewRow.FPriorBrother <> nil then
    NewRow.FPriorBrother.FNextBrother := NewRow;
  Inc(fExpandingRow.FChildCount);
end;

procedure TCustomObjectInspector.SortSubEditors(ParentRow: TOIPropertyGridRow);
var
  Item: TOIPropertyGridRow;
  Index: integer;
  Next: TOIPropertyGridRow;
begin
  if not ParentRow.Sort(@SortGridRows) then
    exit;
  // update fRows
  Item := ParentRow.FirstChild;
  Index := ParentRow.Index + 1;
  Next := ParentRow.NextSkipChilds;
  while (Item <> nil) and (Item <> Next) do
  begin
    fRows[Index] := Item;
    Item.FIndex := Index;
    Item := Item.Next;
    Inc(Index);
  end;
end;

function TCustomObjectInspector.CanExpandRow(Row: TOIPropertyGridRow): boolean;
var
  AnObject: TPersistent;
  ParentRow: TOIPropertyGridRow;
begin
  Result := False;
  if (Row = nil) or (Row.Editor = nil) then
    exit;
  if (not (paSubProperties in Row.Editor.GetAttributes)) then
    exit;
  // check if circling
  if (Row.Editor is TPersistentPropertyEditor) then
  begin
    if (Row.Editor is TInterfacePropertyEditor) then
      AnObject :={%H-}TPersistent(Row.Editor.GetIntfValue)
    else
      AnObject := TPersistent(Row.Editor.GetObjectValue);
    if fSelection.IndexOf(AnObject) >= 0 then
      exit;
    ParentRow := Row.Parent;
    while ParentRow <> nil do
    begin
      if (ParentRow.Editor is TPersistentPropertyEditor) and
        (ParentRow.Editor.GetObjectValue = AnObject) then
        exit;
      ParentRow := ParentRow.Parent;
    end;
  end;
  Result := True;
end;

function TCustomObjectInspector.MouseToIndex(y: integer; MustExist: boolean): integer;
var
  l, r, m: integer;
begin
  l := 0;
  r := fRows.Count - 1;
  Inc(y, fTopY);
  while (l <= r) do
  begin
    m := (l + r) shr 1;
    if Rows[m].Top > y then
    begin
      r := m - 1;
    end
    else if Rows[m].Bottom <= y then
    begin
      l := m + 1;
    end
    else
    begin
      Result := m;
      exit;
    end;
  end;
  if (MustExist = False) and (fRows.Count > 0) then
  begin
    if y < 0 then
      Result := 0
    else
      Result := fRows.Count - 1;
  end
  else
    Result := -1;
end;

function TCustomObjectInspector.GetActiveRow: TOIPropertyGridRow;
begin
  if InRange(ItemIndex, 0, fRows.Count - 1) then
    Result := Rows[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomObjectInspector.SetCurrentRowValue(const NewValue: string);
begin
  if not CanEditRowValue(False) or Rows[fItemIndex].IsReadOnly then
    exit;
  // SetRowValue reads the value from the current edit control and writes it
  // to the property editor
  // -> set the text in the current edit control without changing FLastEditValue
  SetCurrentEditValue(NewValue);
  SetRowValue(False, True);
end;

procedure TCustomObjectInspector.SetItemIndexAndFocus(NewItemIndex: integer;
  WasValueClick: boolean);
begin
  if not InRange(NewItemIndex, 0, fRows.Count - 1) then
    exit;
  ItemIndex := NewItemIndex;
  if fCurrentEdit <> nil then
  begin
    SetActiveControl(fCurrentEdit);
    if (fCurrentEdit is TCustomEdit) then
      TCustomEdit(fCurrentEdit).SelectAll
    {$IFnDEF UseOINormalCheckBox}
    else if (fCurrentEdit is TCheckBoxThemed) and WasValueClick then
      TCheckBoxThemed(fCurrentEdit).Checked := not TCheckBoxThemed(fCurrentEdit).Checked;
    {$ELSE}
    else if (FCurrentEdit is TCheckBox) and WasValueClick then
      TCheckBox(FCurrentEdit).Checked := not TCheckBox(FCurrentEdit).Checked;
    {$ENDIF}
  end;
end;

procedure TCustomObjectInspector.InspectObject(AObject: TIObject);
var
  sel: TPersistentSelectionList;
begin
  if Assigned(AObject) then
  begin
    sel := TPersistentSelectionList.Create;
    try
      sel.ForceUpdate := True;
      sel.Add(AObject);
      Selection := sel;
    finally
      sel.Free;
    end;
  end
  else
    Clear;
end;

function TCustomObjectInspector.CanEditRowValue(CheckFocus: boolean): boolean;
var
  FocusedControl: TWinControl;
begin
  Result := not GridIsUpdating and IsCurrentEditorAvailable and
    (not (pgsCallingEdit in fStates)) and
    ((fCurrentEditorLookupRoot = nil) or (fPropertyEditorHook = nil) or
    (fPropertyEditorHook.LookupRoot = fCurrentEditorLookupRoot));
  if Result and CheckFocus then
  begin
    FocusedControl := FindOwnerControl(GetFocus);
    if (FocusedControl <> nil) and (FocusedControl <> Self) and
      (not IsParentOf(FocusedControl)) then
      Result := False;
  end;
  if Result then
  begin
    {DebugLn(['TCustomObjectInspector.CanEditRowValue',
      ' pgsChangingItemIndex=',pgsChangingItemIndex in fStates,
      ' pgsApplyingValue=',pgsApplyingValue in fStates,
      ' pgsUpdatingEditControl=',pgsUpdatingEditControl in fStates,
      ' FCurrentEdit=',dbgsName(fCurrentEdit),
      ' FItemIndex=',fItemIndex,
      ' FCurrentEditorLookupRoot=',dbgsName(fCurrentEditorLookupRoot),
      ' FPropertyEditorHook.LookupRoot=',dbgsName(fPropertyEditorHook.LookupRoot)
      ]);}
  end;
end;

procedure TCustomObjectInspector.SaveChanges;
begin
  SetRowValue(True, False);
end;

function TCustomObjectInspector.GetHintTypeAt(RowIndex: integer;
  X: integer): TPropEditHint;
var
  IconX: integer;
begin
  Result := pehNone;
  if (RowIndex < 0) or (RowIndex >= RowCount) then
    Exit;
  if SplitterX <= X then
  begin
    if (fCurrentButton <> nil) and (fCurrentButton.Left <= X) then
      Result := pehEditButton
    else
      Result := pehValue;
  end
  else
  begin
    IconX := GetTreeIconX(RowIndex);
    if IconX + Indent > X then
      Result := pehTree
    else
      Result := pehName;
  end;
end;

procedure TCustomObjectInspector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  IconX, Index: integer;
  PointedRow: TOIpropertyGridRow;
  Details: TThemedElementDetails;
  Sz: TSize;
begin
  //ShowMessageDialog('X'+IntToStr(X)+',Y'+IntToStr(Y));
  inherited MouseDown(Button, Shift, X, Y);

  HideHint;

  if Button = mbLeft then
  begin
    fFirstClickTime := GetTickCount;
    if Cursor = crHSplit then
    begin
      fDragging := True;
    end
    else
    begin
      Index := MouseToIndex(Y, False);
      if (Index >= 0) and (Index < fRows.Count) then
      begin
        PointedRow := Rows[Index];
        if CanExpandRow(PointedRow) then
        begin
          IconX := GetTreeIconX(Index);
          if ((X >= IconX) and (X <= IconX + fIndent)) or (ssDouble in Shift) then
          begin
            if PointedRow.Expanded then
              ShrinkRow(Index)
            else
              ExpandRow(Index);
          end;
        end;
        // WasValueClick param is only for Boolean checkboxes, toggled if user
        //  clicks the square. It has no effect for Boolean ComboBox editor.
        Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
        Sz := ThemeServices.GetDetailSize(Details);
        SetItemIndexAndFocus(Index, (X > SplitterX) and (X <= SplitterX + Sz.cx));
        SetCaptureControl(Self);
        Column := oipgcValue;
      end;
    end;
  end;
end;

procedure TCustomObjectInspector.MouseLeave;
begin
  if Assigned(fHintManager) and Assigned(fHintManager.CurHintWindow) and
    fHintManager.CurHintWindow.Visible and not PtInRect(ClientRect,
    ScreenToClient(Mouse.CursorPos)) then
    fHintManager.HideHint;

  inherited MouseLeave;
end;

procedure TCustomObjectInspector.MouseMove(Shift: TShiftState; X, Y: integer);
var
  TheHint: string;
  HintType: TPropEditHint;
  fPropRow: TOIPropertyGridRow;

  procedure DoShow(pt: TPoint); inline;
  var
    HintFont: TFont;
  begin
    if WidgetSet.GetLCLCapability(lcTransparentWindow) = LCL_CAPABILITY_NO then
      Inc(pt.Y, fPropRow.Height);
    if HintType <> pehValue then
      HintFont := Screen.HintFont
    else
    if fPropRow.Editor.ValueIsStreamed then
      HintFont := fValueFont
    else
      HintFont := fDefaultValueFont;
    fHintManager.ShowHint(ClientToScreen(pt), TheHint, False, HintFont);
    if fHintManager.CurHintWindow <> nil then
      fHintManager.CurHintWindow.OnMouseLeave := @HintMouseLeave;
  end;

var
  SplitDistance: integer;
  Index, TextLeft: integer;
begin
  inherited MouseMove(Shift, X, Y);
  SplitDistance := X - SplitterX;
  if fDragging then
  begin
    HideHint;
    if ssLeft in Shift then
    begin
      SplitterX := SplitterX + SplitDistance;
    end
    else
    begin
      EndDragSplitter;
    end;
  end
  else
  begin
    if (abs(SplitDistance) <= 2) then
    begin
      Cursor := crHSplit;
    end
    else
    begin
      Cursor := crDefault;
    end;

    if ssLeft in Shift then
    begin
      Index := MouseToIndex(Y, False);
      SetItemIndexAndFocus(Index);
      SetCaptureControl(Self);
    end;

    // to check if the property text fits in its box, if not show a hint
    if not (ShowHint and InitHints) then
      Exit;
    Index := MouseToIndex(y, False);
    HintType := GetHintTypeAt(Index, x);
    if (Index <> fHintIndex) or (HintType <> fHintType) then
      HideHint;
    ResetLongHintTimer;
    if (Index = -1) or fShowingLongHint or
      (fHintManager.HintIsVisible and (Index = fHintIndex) and (HintType = fHintType)) then
      Exit;

    fHintIndex := Index;
    fHintType := HintType;
    fPropRow := GetRow(Index);
    if HintType = pehName then
    begin
      // Mouse is over property name...
      TheHint := fPropRow.Name;
      TextLeft := BorderWidth + GetTreeIconX(Index) + Indent + 5;
      if (Canvas.TextWidth(TheHint) + TextLeft) >= SplitterX - 2 then
        DoShow(Point(TextLeft - 3, fPropRow.Top - TopY - 1));
    end
    else
    if HintType in [pehValue, pehEditButton] then
    begin
      // Mouse is over property value...
      TheHint := fPropRow.LastPaintedValue;
      if length(TheHint) > 100 then
        TheHint := copy(TheHint, 1, 100) + '...';
      TextLeft := SplitterX + 2;
      if Canvas.TextWidth(TheHint) > (ClientWidth - BorderWidth - TextLeft) then
        DoShow(Point(TextLeft - 3, fPropRow.Top - TopY - 1));
    end;
  end;
end;

procedure TCustomObjectInspector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if fDragging then
    EndDragSplitter;
  SetCaptureControl(nil);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomObjectInspector.KeyDown(var Key: word; Shift: TShiftState);
begin
  HandleStandardKeys(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TCustomObjectInspector.HandleStandardKeys(var Key: word; Shift: TShiftState);
var
  Handled: boolean;

  procedure FindPropertyBySearchText;
  var
    i, IIndex: integer;
  begin
    if Column = oipgcName then
    begin
      fKeySearchText := fKeySearchText + UpCase(Chr(Key));
      if ItemIndex = -1 then
        IIndex := 0
      else
        IIndex := ItemIndex;
      for i := 0 to RowCount - 1 do
        if (Rows[i].Lvl = Rows[IIndex].Lvl) and
          (UpperCase(LeftStr(Rows[i].Name, Length(fKeySearchText))) =
          fKeySearchText) then
        begin
          // Set item index. To go to Value user must hit either Tab or Enter.
          SetItemIndex(i);
          exit;
        end;
      // Left part of phrase not matched, remove added char.
      SetLength(fKeySearchText, Length(fKeySearchText) - 1);
    end;
    Handled := False;
  end;

  procedure HandleUnshifted;
  const
    Page = 20;
  begin
    Handled := True;
    case Key of
      VK_UP: SetItemIndexAndFocus(ItemIndex - 1);
      VK_DOWN: SetItemIndexAndFocus(ItemIndex + 1);
      VK_PRIOR: SetItemIndexAndFocus(Max(ItemIndex - Page, 0));
      VK_NEXT: SetItemIndexAndFocus(Min(ItemIndex + Page, fRows.Count - 1));

      VK_TAB: DoTabKey;

      VK_RETURN:
      begin
        if Column = oipgcName then
          DoTabKey
        else
          SetRowValue(False, True);
        if fCurrentEdit is TCustomEdit then
          TCustomEdit(fCurrentEdit).SelectAll;
      end;

      VK_ESCAPE:
      begin
        RefreshValueEdit;
        fKeySearchText := '';
      end;

      VK_BACK:
      begin
        if (Column = oipgcName) then
          if (fKeySearchText <> '') then
            SetLength(fKeySearchText, Length(fKeySearchText) - 1);
        Handled := False;
      end;

      Ord('A')..Ord('Z'): FindPropertyBySearchText;

      else
        Handled := False;
    end;
  end;

begin
  //writeln('TCustomObjectInspector.HandleStandardKeys ',Key);
  Handled := False;
  if (Shift = []) or (Shift = [ssShift]) then
  begin
    if not (fCurrentEdit is TCustomCombobox) or not
      TCustomCombobox(fCurrentEdit).DroppedDown then
      HandleUnshifted;
  end
  else
  if Shift = [ssCtrl] then
  begin
    case Key of
      VK_RETURN:
      begin
        ToggleRow;
        Handled := True;
      end;
    end;
  end
  else
  if Shift = [ssAlt] then
    case Key of
      VK_LEFT:
      begin
        Handled := (ItemIndex >= 0) and Rows[ItemIndex].Expanded;
        if Handled then
          ShrinkRow(ItemIndex);
      end;

      VK_RIGHT:
      begin
        Handled := (ItemIndex >= 0) and not Rows[ItemIndex].Expanded and
          CanExpandRow(Rows[ItemIndex]);
        if Handled then
          ExpandRow(ItemIndex);
      end;
    end;


  if not Handled and Assigned(OnOIKeyDown) then
  begin
    OnOIKeyDown(Self, Key, Shift);
    Handled := Key = VK_UNKNOWN;
  end;

  //writeln('TCustomObjectInspector.HandleStandardKeys ',Key,' Handled=',Handled);
  if Handled then
    Key := VK_UNKNOWN;
end;

procedure TCustomObjectInspector.HandleKeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key <> VK_UNKNOWN) and Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TCustomObjectInspector.DoTabKey;
begin
  if Column = oipgcValue then
  begin
    Column := oipgcName;
    Self.SetFocus;
  end
  else
  begin
    Column := oipgcValue;
    if fCurrentEdit <> nil then
      fCurrentEdit.SetFocus;
  end;
  fKeySearchText := '';
end;

function TCustomObjectInspector.EditorFilter(const AEditor: TPropertyEditor): boolean;
begin
  Result := IsInteresting(AEditor, fFilter, PropNameFilter);
  if Result and Assigned(OnEditorFilter) then
    OnEditorFilter(Self, AEditor, Result);
end;

function TCustomObjectInspector.GetIObject: TPersistent;
begin
  if PropertyEditorHook <> nil then
    Result := PropertyEditorHook.LookupRoot
  else
    Result := nil;
end;

procedure TCustomObjectInspector.SetAutoFreeHook(const AValue: boolean);
begin
  if fAutoFreeHook = AValue then
    exit;
  fAutoFreeHook := AValue;
end;

procedure TCustomObjectInspector.SetIObject(const AValue: TPersistent);
var
  NewSelection: TPersistentSelectionList;
begin
  if (IObject = AValue) then
  begin
    if ((AValue <> nil) and (Selection.Count = 1) and (Selection[0] = AValue))
    or (AValue = nil) then
      exit;
  end;
  if SaveOnChangeIObject then
    SaveChanges;
  if PropertyEditorHook = nil then
  begin
    fAutoFreeHook := true;
    PropertyEditorHook := TPropertyEditorHook.Create(Self);
  end;
  PropertyEditorHook.LookupRoot := AValue;
  if (AValue = nil) or (Selection.Count <> 1) or (Selection[0] <> AValue) then
  begin
    NewSelection := TPersistentSelectionList.Create;
    try
      if AValue <> nil then
        NewSelection.Add(AValue);
      Selection := NewSelection;
    finally
      NewSelection.Free;
    end;
  end;
end;

procedure TCustomObjectInspector.EraseBackground(DC: HDC);
begin
  // everything is painted, so erasing the background is not needed
end;

procedure TCustomObjectInspector.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateScrollBar;
end;

procedure TCustomObjectInspector.DoSelectionChange;
begin
  if Assigned(fOnSelectionChange) then
    fOnSelectionChange(Self);
end;

procedure TCustomObjectInspector.ObjectAdded(AObject: TIObject);
begin

end;

procedure TCustomObjectInspector.ObjectRemove(AObject: TIObject);
var
  sel: TPersistentSelectionList;
begin
  if fSelection.IndexOf(AObject) >= 0 then
  begin
    sel := TPersistentSelectionList.Create;
    try
      sel.ForceUpdate := True;
      sel.Assign(fSelection);
      sel.Remove(AObject);
      Selection := sel;
    finally
      sel.Free;
    end;
  end;
end;

procedure TCustomObjectInspector.ObjectChanging(AObject: TIObject);
begin

end;

procedure TCustomObjectInspector.ObjectChanged(AObject: TIObject);
begin
  if Assigned(AObject) then
    RefreshPropertyValues;
end;

procedure TCustomObjectInspector.ObjectSelected(AObject: TIObject;
  Selected: boolean);
var
  i: integer;
  sel: TPersistentSelectionList;
begin
  if Selected then
  begin
    if Document.SelectedCount > 1 then
    begin
      sel := TPersistentSelectionList.Create;
      try
        sel.ForceUpdate := True;
        for i := 0 to Document.SelectedCount - 1 do
          sel.Add(Document.Selected[i]);
        Selection := sel;
      finally
        sel.Free;
      end;
    end
    else
      InspectObject(AObject);
  end
  else
    InspectObject(nil);
end;

procedure TCustomObjectInspector.ObjectCommandReceived(AObject: TIObject;
  var cmd: TICmdMessage);
begin

end;

procedure TCustomObjectInspector.ObjectCommandHandled(AObject: TIObject;
  var cmd: TICmdMessage);
begin

end;

procedure TCustomObjectInspector.DocumentResize(NewWidth, NewHeight: double);
begin

end;

procedure TCustomObjectInspector.DocumentDestroying;
begin
  Document := nil;
end;

procedure TCustomObjectInspector.LayerAdded(ALayer: TILayer);
begin

end;

procedure TCustomObjectInspector.LayerRemove(ALayer: TILayer);
begin

end;

procedure TCustomObjectInspector.LayerChanging(ALayer: TILayer);
begin

end;

procedure TCustomObjectInspector.LayerChanged(ALayer: TILayer);
begin

end;

procedure TCustomObjectInspector.ActiveLayerChanged(ALayer: TILayer);
begin

end;

procedure TCustomObjectInspector.HintMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  pos: TPoint;
begin
  if fHintManager.HintIsVisible then
  begin
    pos := ScreenToClient(fHintManager.CurHintWindow.ClientToScreen(Point(X, Y)));
    MouseDown(Button, Shift, pos.X, pos.Y);
  end;
end;

procedure TCustomObjectInspector.HintMouseLeave(Sender: TObject);
begin
  if FindLCLControl(Mouse.CursorPos) <> Self then
    fHintManager.HideHint;
end;

procedure TCustomObjectInspector.EndDragSplitter;
begin
  if fDragging then
  begin
    Cursor := crDefault;
    fDragging := False;
    fPreferredSplitterX := fSplitterX;
    if fCurrentEdit <> nil then
    begin
      SetCaptureControl(nil);
      if Column = oipgcValue then
        fCurrentEdit.SetFocus
      else
        Self.SetFocus;
    end;
  end;
end;

procedure TCustomObjectInspector.SetReadOnlyColor(const AValue: TColor);
begin
  if fReadOnlyColor = AValue then
    Exit;
  fReadOnlyColor := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.SetRowSpacing(const AValue: integer);
begin
  if fRowSpacing = AValue then
    exit;
  fRowSpacing := AValue;
  SetItemsTops;
end;

procedure TCustomObjectInspector.SetShowGutter(const AValue: boolean);
begin
  if fShowGutter = AValue then
    exit;
  fShowGutter := AValue;
  invalidate;
end;

procedure TCustomObjectInspector.SetSplitterX(const NewValue: integer);
var
  AdjustedValue: integer;
begin
  AdjustedValue := NewValue;
  if AdjustedValue > ClientWidth then
    AdjustedValue := ClientWidth;
  if AdjustedValue < 1 then
    AdjustedValue := 1;
  if fSplitterX <> AdjustedValue then
  begin
    fSplitterX := AdjustedValue;
    AlignEditComponents;
    Invalidate;
  end;
end;

procedure TCustomObjectInspector.SetTopY(const NewValue: integer);
var
  NewTopY: integer;
  {$IFDEF WINDOWS}
  r: Types.TRect;
  {$ENDIF}
begin
  NewTopY := TopMax;
  if NewValue < NewTopY then
    NewTopY := NewValue;
  if NewTopY < 0 then
    NewTopY := 0;
  if fTopY <> NewTopY then
  begin
    {$IFDEF WINDOWS}
    r := ClientRect;
    if not ScrollWindowEx(Handle, 0, fTopY - NewTopY, @r, @r, 0, nil,
      SW_INVALIDATE + SW_SCROLLCHILDREN) then
    {$ENDIF}
      Invalidate;
    fTopY := NewTopY;
    UpdateScrollBar;
    AlignEditComponents;
  end;
end;

function TCustomObjectInspector.GetPropNameColor(ARow: TOIPropertyGridRow): TColor;

  function HasWriter(APropInfo: PPropInfo): boolean; inline;
  begin
    Result := Assigned(APropInfo) and Assigned(APropInfo^.SetProc);
  end;

  function HasClass(APropInfo: PPropInfo): boolean; inline;
  begin
    Result := Assigned(APropInfo) and Assigned(APropInfo^.PropType) and
      (APropInfo^.PropType^.Kind = tkClass);
  end;

var
  ParentRow: TOIPropertyGridRow;
  IsObjectSubProperty: boolean;
begin
  // Try to guest if ARow, or one of its parents, is a subproperty
  // of an object (and not an item of a set)
  IsObjectSubProperty := False;
  ParentRow := ARow.Parent;
  while Assigned(ParentRow) do
  begin
    if ParentRow.Editor is TPersistentPropertyEditor then
      IsObjectSubProperty := True;
    ParentRow := ParentRow.Parent;
  end;

  if (ItemIndex <> -1) and (ItemIndex = ARow.Index) then
    Result := fHighlightFont.Color
  else
  if (not HasWriter(ARow.Editor.GetPropInfo)) and (not HasClass(ARow.Editor.GetPropInfo)) then
    Result := fReadOnlyColor
  else
  if ARow.Editor is TPersistentPropertyEditor then
    Result := fReferencesColor
  else
  if IsObjectSubProperty then
    Result := fSubPropertiesColor
  else
    Result := fNameFont.Color;
end;

procedure TCustomObjectInspector.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  //writeln('[TCustomObjectInspector.SetBounds] ',Name,' ',aLeft,',',aTop,',',aWidth,',',aHeight,' Visible=',Visible);
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  if Visible then
  begin
    if not fDragging then
    begin
      if (SplitterX < 5) and (aWidth > 20) then
        SplitterX := 100
      else
        SplitterX := fPreferredSplitterX;
    end;
    AlignEditComponents;
  end;
end;

function TCustomObjectInspector.GetTreeIconX(Index: integer): integer;
begin
  Result := Rows[Index].Lvl * Indent + 2;
end;

function TCustomObjectInspector.TopMax: integer;
begin
  Result := GridHeight - ClientHeight + 2 * integer(BorderWidth);
  if Result < 0 then
    Result := 0;
end;

function TCustomObjectInspector.GridHeight: integer;
begin
  if fRows.Count > 0 then
    Result := Rows[fRows.Count - 1].Bottom
  else
    Result := 0;
end;

procedure TCustomObjectInspector.AlignEditComponents;
var
  RRect, EditCompRect, EditBtnRect: TRect;
begin
  if ItemIndex >= 0 then
  begin
    RRect := RowRect(ItemIndex);
    {.$ifdef LCLGtk2}
    LCLintf.InflateRect(RRect, 0, 1);
    {.$endif}
    EditCompRect := RRect;

    if Layout = oilHorizontal then
      EditCompRect.Left := RRect.Left + SplitterX
    else
    begin
      EditCompRect.Top := RRect.Top + GetNameRowHeight;
      EditCompRect.Left := RRect.Left + GetTreeIconX(ItemIndex) + Indent;
    end;

    if fCurrentButton <> nil then
    begin
      // edit dialog button
      with EditBtnRect do
      begin
        Top := EditCompRect.Top;
        Left := EditCompRect.Right - Scale96ToForm(20);
        Bottom := EditCompRect.Bottom - 1;
        Right := EditCompRect.Right;
        EditCompRect.Right := Left;
      end;
      if fCurrentButton.BoundsRect <> EditBtnRect then
        fCurrentButton.BoundsRect := EditBtnRect;
      //DebugLn(['TCustomObjectInspector.AlignEditComponents FCurrentButton.BoundsRect=',dbgs(fCurrentButton.BoundsRect),' EditBtnRect=',dbgs(EditBtnRect)]);
    end;
    if fCurrentEdit <> nil then
    begin
      // resize the edit component
      if (fCurrentEdit is TEdit) or (fCurrentEdit is TComboBox) then
      begin
        Dec(EditCompRect.Top);
      {$IFDEF UseOINormalCheckBox}
      end
      else if FCurrentEdit is TCheckBox then
      begin
        with EditCompRect do  // Align "normal" CheckBox to the middle vertically
          Inc(Top, (Bottom - Top - ValueCheckBox.Height) div 2);
      {$ELSE}
      end
      else if fCurrentEdit is TCheckBoxThemed then
      begin             // Move right as much as in TPropertyEditor.DrawCheckValue.
        Inc(EditCompRect.Left, CheckBoxThemedLeftOffs);
      {$ENDIF}
      end;
      //debugln('TCustomObjectInspector.AlignEditComponents A ',dbgsName(fCurrentEdit),' ',dbgs(EditCompRect));
      if fCurrentEdit.BoundsRect <> EditCompRect then
        fCurrentEdit.BoundsRect := EditCompRect;
    end;
  end;
end;

procedure TCustomObjectInspector.PaintRow(ARow: integer);
var
  FullRect, NameRect, NameTextRect, NameIconRect, ValueRect: TRect;
  CurRow: TOIPropertyGridRow;

  procedure ClearBackground;
  var
    DrawValuesDiffer: boolean;
  begin
    DrawValuesDiffer := (fValueDifferBackgrndColor <> clNone) and not
      CurRow.Editor.AllEqual;
    if fBackgroundColor <> clNone then
    begin
      Canvas.Brush.Color := fBackgroundColor;
      if DrawValuesDiffer then
        Canvas.FillRect(NameRect)
      else
        Canvas.FillRect(FullRect);
    end;
    if DrawValuesDiffer then
    begin
      // Make the background color darker than what the active edit control has.
      Canvas.Brush.Color := fValueDifferBackgrndColor - $282828;
      Canvas.FillRect(ValueRect);
    end;
    if ShowGutter and (Layout = oilHorizontal) and
      (fGutterColor <> fBackgroundColor) and (fGutterColor <> clNone) then
    begin
      Canvas.Brush.Color := fGutterColor;
      Canvas.FillRect(NameIconRect);
    end;
  end;

  procedure DrawIcon(IconX: integer);
  var
    Details: TThemedElementDetails;
    sz: TSize;
    IconY: integer;
    Res: TScaledImageListResolution;
  begin
    if CurRow.Expanded then
      Details := ThemeServices.GetElementDetails(ttGlyphOpened)
    else
      Details := ThemeServices.GetElementDetails(ttGlyphClosed);
    if CanExpandRow(CurRow) then
    begin
      sz := ThemeServices.GetDetailSize(Details);
      IconY := ((NameRect.Bottom - NameRect.Top - sz.cy) div 2) + NameRect.Top;
      ThemeServices.DrawElement(Canvas.Handle, Details,
        Rect(IconX, IconY, IconX + sz.cx, IconY + sz.cy), nil);
    end
    else
    if (ARow = fItemIndex) then
    begin
      Res := fActiveRowImages.ResolutionForControl[0, Self];

      IconY := ((NameRect.Bottom - NameRect.Top - Res.Height) div 2) + NameRect.Top;
      Res.Draw(Canvas, IconX, IconY, fActiveRowImages.GetImageIndex('pg_active_row'));
    end;
  end;

  procedure DrawName(DrawState: TPropEditDrawState);
  var
    OldFont: TFont;
    NameBgColor: TColor;
  begin
    if (ARow = fItemIndex) and (fHighlightColor <> clNone) then
      NameBgColor := fHighlightColor
    else
      NameBgColor := fBackgroundColor;
    OldFont := Canvas.Font;
    Canvas.Font := fNameFont;
    Canvas.Font.Color := GetPropNameColor(CurRow);
    // set bg color to highlight if needed
    if (NameBgColor <> fBackgroundColor) and (NameBgColor <> clNone) then
    begin
      Canvas.Brush.Color := NameBgColor;
      Canvas.FillRect(NameTextRect);
    end;
    CurRow.Editor.PropDrawName(Canvas, NameTextRect, DrawState);
    Canvas.Font := OldFont;
    if fBackgroundColor <> clNone then // return color back to background
      Canvas.Brush.Color := fBackgroundColor;
  end;

  procedure DrawWidgetsets;
  var
    OldFont: TFont;
    X, Y: integer;
    lclPlatform: TLCLPlatform;
    ImagesRes: TScaledImageListResolution;
  begin
    ImagesRes := IDEImages.Images_16.ResolutionForPPI[0, Font.PixelsPerInch,
      GetCanvasScaleFactor];
    X := NameRect.Right - 2;
    Y := (NameRect.Top + NameRect.Bottom - ImagesRes.Height) div 2;
    OldFont := Canvas.Font;
    Canvas.Font := fNameFont;
    Canvas.Font.Color := clRed;
    for lclPlatform := High(TLCLPlatform) downto Low(TLCLPlatform) do
    begin
      if lclPlatform in CurRow.FWidgetSets then
      begin
        Dec(X, ImagesRes.Width);
        ImagesRes.Draw(Canvas, X, Y,
          IDEImages.LoadImage('issue_' + LCLPlatformDirNames[lclPlatform]));
      end;
    end;
    Canvas.Font := OldFont;
  end;

  procedure DrawValue(DrawState: TPropEditDrawState);
  var
    OldFont: TFont;
    Style: TTextStyle;
    s: string;
  begin
    if ARow <> ItemIndex then
    begin
      OldFont := Canvas.Font;
      if CurRow.Editor.ValueIsStreamed then
        Canvas.Font := fValueFont
      else
        Canvas.Font := fDefaultValueFont;
      s := CorrectClassName(CurRow.Editor);
      FillChar(Style{%H-}, SizeOf(Style), 0);
      with Style do
      begin
        Layout := tlCenter;
        Clipping := True;
        SingleLine := True;
        ExpandTabs := True;
      end;
      Canvas.TextRect(ValueRect, ValueRect.Left + 3, ValueRect.Top, s, Style);
      Canvas.Font := OldFont;
    end;
    CurRow.LastPaintedValue := CurRow.Editor.GetVisualValue;
  end;

  procedure DrawGutterToParent;
  var
    ParentRect: TRect;
    X: integer;
  begin
    if ARow > 0 then
    begin
      ParentRect := RowRect(ARow - 1);
      X := ParentRect.Left + GetTreeIconX(ARow - 1) + Indent + 3;
      if X <> NameIconRect.Right then
      begin
        Canvas.MoveTo(NameIconRect.Right, NameRect.Top - 1 - fRowSpacing);
        Canvas.LineTo(X - 1, NameRect.Top - 1 - fRowSpacing);
      end;
    end;
    // to parent next sibling
    if ARow < fRows.Count - 1 then
    begin
      ParentRect := RowRect(ARow + 1);
      X := ParentRect.Left + GetTreeIconX(ARow + 1) + Indent + 3;
      if X <> NameIconRect.Right then
      begin
        Canvas.MoveTo(NameIconRect.Right, NameRect.Bottom - 1);
        Canvas.LineTo(X - 1, NameRect.Bottom - 1);
      end;
    end;
  end;

var
  IconX: integer;
  DrawState: TPropEditDrawState;
begin
  CurRow := Rows[ARow];
  FullRect := RowRect(ARow);
  NameRect := FullRect;
  ValueRect := FullRect;
  Inc(FullRect.Bottom, fRowSpacing);

  if Layout = oilHorizontal then
  begin
    NameRect.Right := SplitterX;
    ValueRect.Left := SplitterX;
  end
  else
  begin
    NameRect.Bottom := NameRect.Top + GetNameRowHeight;
    ValueRect.Top := NameRect.Bottom;
  end;

  IconX := GetTreeIconX(ARow);
  NameIconRect := NameRect;
  NameIconRect.Right := IconX + Indent;
  NameTextRect := NameRect;
  NameTextRect.Left := NameIconRect.Right;

  if Layout = oilVertical then
    ValueRect.Left := NameTextRect.Left
  else
  begin
    Inc(NameIconRect.Right, 2 + Ord(ShowGutter));
    Inc(NameTextRect.Left, 3 + Ord(ShowGutter));
  end;

  DrawState := [];
  if ARow = fItemIndex then
    Include(DrawState, pedsSelected);

  ClearBackground;      // clear background in one go
  DrawIcon(IconX);      // draw icon
  DrawName(DrawState);  // draw name
  DrawWidgetsets;       // draw widgetsets
  DrawValue(DrawState); // draw value

  with Canvas do
  begin
    // frames

    if Layout = oilHorizontal then
    begin
      // Row Divider
      if DrawHorzGridLines then
      begin
        Pen.Style := psDot;
        Pen.EndCap := pecFlat;
        Pen.Cosmetic := False;
        Pen.Color := cl3DShadow;
        if fRowSpacing <> 0 then
        begin
          MoveTo(NameTextRect.Left, NameRect.Top - 1);
          LineTo(ValueRect.Right, NameRect.Top - 1);
        end;
        MoveTo(NameTextRect.Left, NameRect.Bottom - 1);
        LineTo(ValueRect.Right, NameRect.Bottom - 1);
      end;

      // Split lines between: icon and name, name and value
      Pen.Style := psSolid;
      Pen.Cosmetic := True;
      Pen.Color := cl3DHiLight;
      MoveTo(NameRect.Right - 1, NameRect.Bottom - 1);
      LineTo(NameRect.Right - 1, NameRect.Top - 1 - fRowSpacing);
      Pen.Color := cl3DShadow;
      MoveTo(NameRect.Right - 2, NameRect.Bottom - 1);
      LineTo(NameRect.Right - 2, NameRect.Top - 1 - fRowSpacing);

      // draw gutter line
      if ShowGutter then
      begin
        Pen.Color := GutterEdgeColor;
        MoveTo(NameIconRect.Right, NameRect.Bottom - 1);
        LineTo(NameIconRect.Right, NameRect.Top - 1 - fRowSpacing);
        if CurRow.Lvl > 0 then
          DrawGutterToParent;
      end;
    end
    else
    begin                              // Layout <> oilHorizontal
      Pen.Style := psSolid;
      Pen.Color := cl3DLight;
      MoveTo(ValueRect.Left, ValueRect.Bottom - 1);
      LineTo(ValueRect.Left, NameTextRect.Top);
      LineTo(ValueRect.Right - 1, NameTextRect.Top);
      Pen.Color := cl3DHiLight;
      LineTo(ValueRect.Right - 1, ValueRect.Bottom - 1);
      LineTo(ValueRect.Left, ValueRect.Bottom - 1);

      MoveTo(NameTextRect.Left + 1, NametextRect.Bottom);
      LineTo(NameTextRect.Left + 1, NameTextRect.Top + 1);
      LineTo(NameTextRect.Right - 2, NameTextRect.Top + 1);
      Pen.Color := cl3DLight;
      LineTo(NameTextRect.Right - 2, NameTextRect.Bottom - 1);
      LineTo(NameTextRect.Left + 2, NameTextRect.Bottom - 1);
    end;
  end;
end;

procedure TCustomObjectInspector.DoPaint(PaintOnlyChangedValues: boolean);
var
  a: integer;
  SpaceRect: TRect;
  GutterX: integer;
begin
  BuildPropertyList(True);
  if not PaintOnlyChangedValues then
  begin
    with Canvas do
    begin
      Pen.Color := GutterEdgeColor;
      // draw properties
      for a := 0 to fRows.Count - 1 do
        PaintRow(a);
      // draw unused space below rows
      SpaceRect := Rect(BorderWidth, BorderWidth, ClientWidth -
        BorderWidth + 1, ClientHeight - BorderWidth + 1);
      if fRows.Count > 0 then
        SpaceRect.Top := Rows[fRows.Count - 1].Bottom - fTopY + BorderWidth;
      if fBackgroundColor <> clNone then
      begin
        Brush.Color := fBackgroundColor;
        FillRect(SpaceRect);
      end;

      // draw gutter if needed
      if ShowGutter and (Layout = oilHorizontal) then
      begin
        if fRows.Count > 0 then
          GutterX := RowRect(fRows.Count - 1).Left + GetTreeIconX(fRows.Count - 1)
        else
          GutterX := BorderWidth + 2;
        Inc(GutterX, Indent + 3);
        SpaceRect.Right := GutterX;
        if GutterColor <> clNone then
        begin
          Brush.Color := GutterColor;
          FillRect(SpaceRect);
        end;
        MoveTo(GutterX, SpaceRect.Top);
        LineTo(GutterX, SpaceRect.Bottom);
      end;
      // don't draw border: borderstyle=bsSingle
    end;
  end
  else
  begin
    for a := 0 to fRows.Count - 1 do
    begin
      if Rows[a].Editor.GetVisualValue <> Rows[a].LastPaintedValue then
        PaintRow(a);
    end;
  end;
end;

procedure TCustomObjectInspector.Paint;
begin
  inherited Paint;
  DoPaint(False);
end;

procedure TCustomObjectInspector.RefreshPropertyValues;
begin
  RefreshValueEdit;
  Invalidate;
end;

procedure TCustomObjectInspector.ScrollToActiveItem;
begin
  ScrollToItem(fItemIndex);
end;

procedure TCustomObjectInspector.ScrollToItem(NewIndex: integer);
var
  NewRow: TOIPropertyGridRow;
begin
  if (NewIndex >= 0) and (NewIndex < fRows.Count) then
  begin
    NewRow := Rows[NewIndex];
    if NewRow.Bottom >= TopY + (ClientHeight - 2 * BorderWidth) then
      TopY := NewRow.Bottom - (ClientHeight - 2 * BorderWidth) + 1
    else
    if NewRow.Top < TopY then
      TopY := NewRow.Top;
  end;
end;

procedure TCustomObjectInspector.PropEditLookupRootChange;
begin
  // When the LookupRoot changes, no changes can be stored
  // -> undo the value editor changes
  RefreshValueEdit;
  if PropertyEditorHook <> nil then
    fCurrentEditorLookupRoot := PropertyEditorHook.LookupRoot;
end;

function TCustomObjectInspector.RowRect(ARow: integer): TRect;
const
  ScrollBarWidth = 0;
var
  r: TOIPropertyGridRow;
begin
  r := Rows[ARow];
  if r = nil then
    Exit(Rect(0, 0, 0, 0));
  Result.Left := BorderWidth;
  Result.Top := r.Top - fTopY + BorderWidth;
  Result.Right := ClientWidth - ScrollBarWidth;
  Result.Bottom := r.Bottom - fTopY + BorderWidth;
end;

procedure TCustomObjectInspector.SetItemsTops;
// compute row tops from row heights
// set indices of all rows
var
  a: integer;
begin
  for a := 0 to fRows.Count - 1 do
  begin
    Rows[a].FIndex := a;
    Rows[a].MeasureHeight(Canvas);
  end;
  if fRows.Count > 0 then
    Rows[0].Top := 0;
  for a := 1 to fRows.Count - 1 do
    Rows[a].FTop := Rows[a - 1].Bottom + fRowSpacing;
end;

procedure TCustomObjectInspector.ClearRows;
var
  i: integer;
begin
  IncreaseChangeStep;
  // reverse order to make sure child rows are freed before parent rows
  for i := fRows.Count - 1 downto 0 do
  begin
    //debugln(['TCustomObjectInspector.ClearRows ',i,' ',fRows.Count,' ',dbgs(fRows[i])]);
    Rows[i].Free;
    fRows[i] := nil;
  end;
  fRows.Clear;
  UpdateScrollBar;
end;

function TCustomObjectInspector.GetCurrentEditValue: string;
begin
  if fCurrentEdit = ValueEdit then
  {$IFDEF LCLCarbon}
    Result:=StringReplace(ValueEdit.Text,LineFeedSymbolUTF8,LineEnding,[rfReplaceAll])
  {$ELSE}
    Result:=ValueEdit.Text
  {$ENDIF}
  else if fCurrentEdit = ValueComboBox then
    Result := ValueComboBox.Text
  else if fCurrentEdit = ValueCheckBox then
    Result := ValueCheckBox.Caption
  else
    Result := '';
end;

procedure TCustomObjectInspector.SetActiveControl(const AControl: TWinControl);
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
    F.ActiveControl := AControl;
end;

procedure TCustomObjectInspector.SetColumn(const AValue: TOICustomPropertyGridColumn);
begin
  if fColumn <> AValue then
  begin
    fColumn := AValue;
    // TODO: indication
  end;
end;

procedure TCustomObjectInspector.SetCurrentEditValue(const NewValue: string);
begin
  if fCurrentEdit = ValueEdit then
  {$IFDEF LCLCarbon}
    ValueEdit.Text:=StringReplace(StringReplace(NewValue,#13,LineEnding,[rfReplaceAll]),LineEnding,LineFeedSymbolUTF8,[rfReplaceAll])
  {$ELSE}
    ValueEdit.Text:=NewValue
  {$ENDIF}
  else if fCurrentEdit = ValueComboBox then
  begin
    ValueComboBox.Text := NewValue;
    if ValueComboBox.Style = csOwnerDrawVariable then
      Exclude(fStates, pgsGetComboItemsCalled);
  end
  else if fCurrentEdit = ValueCheckBox then
    SetCheckboxState(NewValue);

  if (fItemIndex >= 0) and (fItemIndex < RowCount) and Assigned(fCurrentEdit) then
  begin
    if Rows[fItemIndex].Editor.ValueIsStreamed then
      fCurrentEdit.Font := fValueFont
    else
      fCurrentEdit.Font := fDefaultValueFont;
  end;
end;

procedure TCustomObjectInspector.SetDocument(AValue: TIDocument);
begin
  if Assigned(fDocument) then
    fDocument.UnRegisterObserver(Self);
  Clear;
  fDocument := AValue;
  if Assigned(fDocument) then
    fDocument.RegisterObserver(Self);
  Invalidate;
end;

procedure TCustomObjectInspector.SetDrawHorzGridLines(const AValue: boolean);
begin
  if fDrawHorzGridLines = AValue then
    Exit;
  fDrawHorzGridLines := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.SetFavorites(const AValue: TOIFavoriteProperties);
begin
  //debugln('TCustomObjectInspector.SetFavorites ',dbgsName(Self));
  if fFavorites = AValue then
    exit;
  fFavorites := AValue;
  BuildPropertyList;
end;

procedure TCustomObjectInspector.SetFilter(const AValue: TTypeKinds);
begin
  if (AValue <> fFilter) then
  begin
    fFilter := AValue;
    BuildPropertyList;
  end;
end;

procedure TCustomObjectInspector.SetGutterColor(const AValue: TColor);
begin
  if fGutterColor = AValue then
    exit;
  fGutterColor := AValue;
  invalidate;
end;

procedure TCustomObjectInspector.SetGutterEdgeColor(const AValue: TColor);
begin
  if fGutterEdgeColor = AValue then
    exit;
  fGutterEdgeColor := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.SetHighlightColor(const AValue: TColor);
begin
  if fHighlightColor = AValue then
    exit;
  fHighlightColor := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.Clear;
begin
  if fCurrentEdit <> nil then
  begin
    fCurrentEdit.Visible := False;
    fCurrentEdit.Enabled := False;
    fCurrentEdit := nil;
  end;
  if fCurrentButton <> nil then
  begin
    fCurrentButton.Visible := False;
    fCurrentButton.Enabled := False;
    fCurrentButton := nil;
  end;
  ClearRows;
  Invalidate;
  DoSelectionChange;
end;

function TCustomObjectInspector.GetRow(Index: integer): TOIPropertyGridRow;
begin
  if InRange(Index, 0, fRows.Count - 1) then
    Result := TOIPropertyGridRow(fRows[Index])
  else
    Result := nil;
end;

procedure TCustomObjectInspector.ValueComboBoxCloseUp(Sender: TObject);
begin
  SetRowValue(False, False);
end;

procedure TCustomObjectInspector.ValueComboBoxGetItems(Sender: TObject);
{ This event is called whenever the widgetset updates the list.
  On gtk the list is updated just before the user popups the list.
  Other widgetsets need the list always, which is bad, as this means collecting
  all items even if the dropdown never happens.
}
var
  CurRow: TOIPropertyGridRow;
  MaxItemWidth, CurItemWidth, i, Cnt: integer;
  ItemValue, CurValue: string;
  NewItemIndex: longint;
  ExcludeUpdateFlag: boolean;
begin
  Include(fStates, pgsGetComboItemsCalled);
  if (fItemIndex >= 0) and (fItemIndex < fRows.Count) then
  begin
    ExcludeUpdateFlag := not (pgsUpdatingEditControl in fStates);
    Include(fStates, pgsUpdatingEditControl);
    ValueComboBox.Items.BeginUpdate;
    try
      CurRow := Rows[fItemIndex];

      // Items
      if not FillComboboxItems then
        exit;

      // Text and ItemIndex
      CurValue := CurRow.Editor.GetVisualValue;
      ValueComboBox.Text := CurValue;
      NewItemIndex := ValueComboBox.Items.IndexOf(CurValue);
      if NewItemIndex >= 0 then
        ValueComboBox.ItemIndex := NewItemIndex;

      // ItemWidth
      MaxItemWidth := ValueComboBox.Width;
      Cnt := ValueComboBox.Items.Count;
      for i := 0 to Cnt - 1 do
      begin
        ItemValue := ValueComboBox.Items[i];
        CurItemWidth := ValueComboBox.Canvas.TextWidth(ItemValue);
        CurRow.Editor.ListMeasureWidth(ItemValue, i, ValueComboBox.Canvas,
          CurItemWidth);
        if MaxItemWidth < CurItemWidth then
          MaxItemWidth := CurItemWidth;
      end;
      ValueComboBox.ItemWidth := MaxItemWidth;
    finally
      ValueComboBox.Items.EndUpdate;
      if ExcludeUpdateFlag then
        Exclude(fStates, pgsUpdatingEditControl);
    end;
  end;
end;

procedure TCustomObjectInspector.ValueComboBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  CurRow: TOIPropertyGridRow;
  ItemValue: string;
  AState: TPropEditDrawState;
  FontColor: TColor;
begin
  if (fItemIndex >= 0) and (fItemIndex < fRows.Count) then
  begin
    CurRow := Rows[fItemIndex];
    if (Index >= 0) and (Index < ValueComboBox.Items.Count) then
      ItemValue := ValueComboBox.Items[Index]
    else
      ItemValue := '';
    AState := [];
    if odSelected in State then
      Include(AState, pedsSelected);
    if odFocused in State then
      Include(AState, pedsFocused);
    if odComboBoxEdit in State then
      Include(AState, pedsInEdit)
    else
      Include(AState, pedsInComboList);

    if not (odBackgroundPainted in State) then
      ValueComboBox.Canvas.FillRect(ARect);

    FontColor := ValueComboBox.Canvas.Font.Color;
    ValueComboBox.Canvas.Font.Assign(fDefaultValueFont);
    if odSelected in State then
      ValueComboBox.Canvas.Font.Color := FontColor
    else
      ValueComboBox.Canvas.Font.Color := clWindowText;
    if CurRow.Editor.HasDefaultValue and (ItemValue = CurRow.Editor.GetDefaultValue) then
      ValueComboBox.Canvas.Font.Style := ValueComboBox.Canvas.Font.Style + [fsItalic];
    CurRow.Editor.ListDrawValue(ItemValue, Index, ValueComboBox.Canvas, ARect, AState);
  end;
end;

procedure TCustomObjectInspector.OnIdle(Sender: TObject; var Done: boolean);
begin
  if (not (pgsGetComboItemsCalled in fStates)) and (fCurrentEdit = ValueComboBox) and
    ValueComboBox.Enabled then
  begin
    ValueComboBoxGetItems(Self);
  end;
end;

procedure TCustomObjectInspector.SetIdleEvent(Enable: boolean);
begin
  if (pgsIdleEnabled in fStates) = Enable then
    exit;
  if Enable then
  begin
    Application.AddOnIdleHandler(@OnIdle);
    Include(fStates, pgsIdleEnabled);
  end
  else
  begin
    Application.RemoveOnIdleHandler(@OnIdle);
    Exclude(fStates, pgsIdleEnabled);
  end;
end;

procedure TCustomObjectInspector.HintTimer(Sender: TObject);
var
  PointedRow: TOIpropertyGridRow;
  Window: TWinControl;
  HintType: TPropEditHint;
  Position, ClientPosition: TPoint;
  Index: integer;
  AHint: string;
  OkToShow: boolean;
begin
  if fLongHintTimer <> nil then
    fLongHintTimer.Enabled := False;
  if (not InitHints) then
    exit;

  Position := Mouse.CursorPos;
  Window := FindLCLWindow(Position);
  if (Window = nil) or ((Window <> Self) and not IsParentOf(Window)) then
    exit;

  ClientPosition := ScreenToClient(Position);
  if ((ClientPosition.X <= 0) or (ClientPosition.X >= Width) or
    (ClientPosition.Y <= 0) or (ClientPosition.Y >= Height)) then
    Exit;

  Index := MouseToIndex(ClientPosition.Y, False);
  // Don't show hint for the selected property.
  if (Index < 0) or (Index >= fRows.Count) or (Index = ItemIndex) then
    Exit;

  PointedRow := Rows[Index];
  if (PointedRow = nil) or (PointedRow.Editor = nil) then
    Exit;

  // Get hint
  OkToShow := True;
  HintType := GetHintTypeAt(Index, ClientPosition.X);
  if (HintType = pehName) and Assigned(OnPropertyHint) then
    OkToShow := OnPropertyHint(Self, PointedRow, AHint)
  else
    AHint := PointedRow.Editor.GetHint(HintType, Position.X, Position.Y);
  // Show hint if all is well.
  if OkToShow and fHintManager.ShowHint(Position, AHint, True, Screen.HintFont) then
  begin
    fHintIndex := Index;
    fHintType := HintType;
    fShowingLongHint := True;
  end;
end;

procedure TCustomObjectInspector.ResetLongHintTimer;
begin
  if (fLongHintTimer = nil) or fShowingLongHint then
    Exit;
  fLongHintTimer.Enabled := False;
  if RowCount > 0 then
    fLongHintTimer.Enabled := not fDragging;
end;

procedure TCustomObjectInspector.HideHint;
begin
  fHintIndex := -1;
  fShowingLongHint := False;
  fHintManager.HideHint;
end;

procedure TCustomObjectInspector.ValueControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  HideHint;
  ScrollToActiveItem;
end;

procedure TCustomObjectInspector.ValueControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  // when the cursor is divider change it to default
  if (Sender as TControl).Parent.Cursor <> crDefault then
    (Sender as TControl).Parent.Cursor := crDefault;
end;

procedure TCustomObjectInspector.IncreaseChangeStep;
begin
  if fChangeStep <> $7fffffff then
    Inc(fChangeStep)
  else
    fChangeStep := -$7fffffff;
end;

function TCustomObjectInspector.GridIsUpdating: boolean;
begin
  Result := (fStates * [pgsChangingItemIndex, pgsApplyingValue,
    pgsBuildPropertyListNeeded] <> []);
end;

procedure TCustomObjectInspector.ToggleRow;
var
  CurRow: TOIPropertyGridRow;
  TypeKind: TTypeKind;
  NewIndex: integer;
begin
  if not CanEditRowValue(False) then
    exit;

  if fLongHintTimer <> nil then
    fLongHintTimer.Enabled := False;

  if (fCurrentEdit = ValueComboBox) then
  begin
    CurRow := Rows[fItemIndex];
    TypeKind := CurRow.Editor.GetPropType^.Kind;
    // Integer (like TImageIndex), Enumeration, Set, Class or Boolean ComboBox
    if TypeKind in [tkInteger, tkEnumeration, tkSet, tkClass, tkBool] then
    begin
      if ValueComboBox.Items.Count = 0 then
        Exit;
      // Pick the next value from list
      if ValueComboBox.ItemIndex < (ValueComboBox.Items.Count - 1) then
      begin
        NewIndex := ValueComboBox.ItemIndex + 1;
        // Go to first object of tkClass. Skip '(none)' which can be in different
        // places depending on widgetset sorting rules.
        if (ValueComboBox.ItemIndex = -1) // Only happen at nil value of tkClass
          and (ValueComboBox.Items[NewIndex] = oisNone) and
          (NewIndex < (ValueComboBox.Items.Count - 1)) then
          Inc(NewIndex);
      end
      else
        NewIndex := 0;
      ValueComboBox.ItemIndex := NewIndex;
      SetRowValue(False, False);
      exit;
    end;
  end;
  DoCallEdit;
end;

procedure TCustomObjectInspector.ValueEditDblClick(Sender: TObject);
begin
  fFirstClickTime := 0;
  ToggleRow;
end;

procedure TCustomObjectInspector.SetBackgroundColor(const AValue: TColor);
begin
  if fBackgroundColor = AValue then
    exit;
  fBackgroundColor := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.SetReferences(const AValue: TColor);
begin
  if fReferencesColor = AValue then
    exit;
  fReferencesColor := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.SetSubPropertiesColor(const AValue: TColor);
begin
  if fSubPropertiesColor = AValue then
    exit;
  fSubPropertiesColor := AValue;
  Invalidate;
end;

procedure TCustomObjectInspector.SetValueDifferBackgrndColor(AValue: TColor);
begin
  if fValueDifferBackgrndColor = AValue then
    Exit;
  fValueDifferBackgrndColor := AValue;
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TOIPropertyGridRow }

constructor TOIPropertyGridRow.Create(PropertyTree: TCustomObjectInspector;
  PropEditor: TPropertyEditor; ParentNode: TOIPropertyGridRow; WidgetSets: TLCLPlatforms);
begin
  inherited Create;
  // tree pointer
  FTree := PropertyTree;
  FParent := ParentNode;
  FNextBrother := nil;
  FPriorBrother := nil;
  FExpanded := False;
  // child nodes
  FChildCount := 0;
  FFirstChild := nil;
  FLastChild := nil;
  // director
  FEditor := PropEditor;
  GetLvl;
  FName := FEditor.GetName;
  FTop := 0;
  FHeight := FTree.RealDefaultItemHeight;
  FIndex := -1;
  LastPaintedValue := '';
  FWidgetSets := WidgetSets;
end;

destructor TOIPropertyGridRow.Destroy;
begin
  //debugln(['TOIPropertyGridRow.Destroy ',fname,' ',dbgs(Pointer(Self))]);
  if FPriorBrother <> nil then
    FPriorBrother.FNextBrother := FNextBrother;
  if FNextBrother <> nil then
    FNextBrother.FPriorBrother := FPriorBrother;
  if FParent <> nil then
  begin
    if FParent.FFirstChild = Self then
      FParent.FFirstChild := FNextBrother;
    if FParent.FLastChild = Self then
      FParent.FLastChild := FPriorBrother;
    Dec(FParent.FChildCount);
  end;
  if FEditor <> nil then
    FEditor.Free;
  inherited Destroy;
end;

function TOIPropertyGridRow.ConsistencyCheck: integer;
var
  OldLvl, RealChildCount: integer;
  AChild: TOIPropertyGridRow;
begin
  if Top < 0 then
    exit(-1);
  if Height < 0 then
    exit(-2);
  if Lvl < 0 then
    exit(-3);
  OldLvl := Lvl;
  GetLvl;
  if Lvl <> OldLvl then
    exit(-4);
  if Name = '' then
    exit(-5);
  if NextBrother <> nil then
  begin
    if NextBrother.PriorBrother <> Self then
      exit(-6);
    if NextBrother.Index < Index + 1 then
      exit(-7);
  end;
  if PriorBrother <> nil then
  begin
    if PriorBrother.NextBrother <> Self then
      exit(-8);
    if PriorBrother.Index > Index - 1 then
      Result := -9;
  end;
  if (Parent <> nil) then
  begin
    // has parent
    if (not Parent.HasChild(Self)) then
      exit(-10);
  end
  else
  begin
    // no parent
  end;
  if FirstChild <> nil then
  begin
    if Expanded then
      if (FirstChild.Index <> Index + 1) then
        exit(-11);
  end
  else
  begin
    if LastChild <> nil then
      exit(-12);
  end;
  RealChildCount := 0;
  AChild := FirstChild;
  while AChild <> nil do
  begin
    if AChild.Parent <> Self then
      exit(-13);
    Inc(RealChildCount);
    AChild := AChild.NextBrother;
  end;
  if RealChildCount <> ChildCount then
    exit(-14);
  Result := 0;
end;

function TOIPropertyGridRow.HasChild(Row: TOIPropertyGridRow): boolean;
var
  ChildRow: TOIPropertyGridRow;
begin
  ChildRow := FirstChild;
  while ChildRow <> nil do
    if ChildRow = Row then
      exit(True);
  Result := False;
end;

procedure TOIPropertyGridRow.WriteDebugReport(const Prefix: string);
var
  i: integer;
  Item: TOIPropertyGridRow;
begin
  DebugLn([Prefix + 'TOIPropertyGridRow.WriteDebugReport ', Name]);
  i := 0;
  Item := FirstChild;
  while Item <> nil do
  begin
    DebugLn([Prefix + '  ', i, ' ', Item.Name]);
    Inc(i);
    Item := Item.NextBrother;
  end;
end;

procedure TOIPropertyGridRow.GetLvl;
var
  n: TOIPropertyGridRow;
begin
  FLvl := 0;
  n := FParent;
  while n <> nil do
  begin
    Inc(FLvl);
    n := n.FParent;
  end;
end;

function TOIPropertyGridRow.GetBottom: integer;
begin
  Result := FTop + FHeight;
  if FTree.Layout = oilVertical then
    Inc(Result, FTree.GetNameRowHeight);
end;

function TOIPropertyGridRow.IsReadOnly: boolean;
begin
  Result := Editor.IsReadOnly or IsDisabled;
end;

function TOIPropertyGridRow.IsDisabled: boolean;
var
  ParentRow: TOIPropertyGridRow;
begin
  Result := False;
  ParentRow := Parent;
  while (ParentRow <> nil) do
  begin
    if paDisableSubProperties in ParentRow.Editor.GetAttributes then
      exit(True);
    ParentRow := ParentRow.Parent;
  end;
end;

procedure TOIPropertyGridRow.MeasureHeight(ACanvas: TCanvas);
begin
  FHeight := FTree.RealDefaultItemHeight;
  Editor.PropMeasureHeight(Name, ACanvas, FHeight);
end;

function TOIPropertyGridRow.Sort(const Compare: TListSortCompare): boolean;
var
  List: TFPList;
  Item: TOIPropertyGridRow;
  i: integer;
begin
  if IsSorted(Compare) then
    exit(False);
  List := TFPList.Create;
  try
    // create a TFPList of the children
    List.Capacity := ChildCount;
    Item := FirstChild;
    while Item <> nil do
    begin
      List.Add(Item);
      Item := Item.NextBrother;
    end;
    // sort the TFPList
    List.Sort(Compare);
    // sort in double linked list
    for i := 0 to List.Count - 1 do
    begin
      Item := TOIPropertyGridRow(List[i]);
      if i = 0 then
      begin
        FFirstChild := Item;
        Item.FPriorBrother := nil;
      end
      else
        Item.FPriorBrother := TOIPropertyGridRow(List[i - 1]);
      if i = List.Count - 1 then
      begin
        FLastChild := Item;
        Item.FNextBrother := nil;
      end
      else
        Item.FNextBrother := TOIPropertyGridRow(List[i + 1]);
    end;
  finally
    List.Free;
  end;
  Result := True;
end;

function TOIPropertyGridRow.IsSorted(const Compare: TListSortCompare): boolean;
var
  Item1: TOIPropertyGridRow;
  Item2: TOIPropertyGridRow;
begin
  if ChildCount < 2 then
    exit(True);
  Item1 := FirstChild;
  while True do
  begin
    Item2 := Item1.NextBrother;
    if Item2 = nil then
      break;
    if Compare(Item1, Item2) > 0 then
      exit(False);
    Item1 := Item2;
  end;
  Result := True;
end;

function TOIPropertyGridRow.Next: TOIPropertyGridRow;
begin
  if fFirstChild <> nil then
    Result := fFirstChild
  else
    Result := NextSkipChilds;
end;

function TOIPropertyGridRow.NextSkipChilds: TOIPropertyGridRow;
begin
  Result := Self;
  while (Result <> nil) do
  begin
    if Result.NextBrother <> nil then
    begin
      Result := Result.NextBrother;
      exit;
    end;
    Result := Result.Parent;
  end;
end;

//==============================================================================


{ TOIOptions }


constructor TOIOptions.Create;
begin
  inherited Create;

  FSaveBounds := False;
  FLeft := 0;
  FTop := 0;
  FWidth := 250;
  FHeight := 400;
  FDefaultItemHeight := 0;
  FShowComponentTree := True;
  FComponentTreeHeight := 160;
  FInfoBoxHeight := 80;

  FGridBackgroundColor := DefBackgroundColor;
  FSubPropertiesColor := DefSubPropertiesColor;
  FValueColor := DefValueColor;
  FDefaultValueColor := DefDefaultValueColor;
  FValueDifferBackgrndColor := DefValueDifferBackgrndColor;
  FReadOnlyColor := DefReadOnlyColor;
  FReferencesColor := DefReferencesColor;
  FPropertyNameColor := DefNameColor;
  FHighlightColor := DefHighlightColor;
  FHighlightFontColor := DefHighlightFontColor;
  FGutterColor := DefGutterColor;
  FGutterEdgeColor := DefGutterEdgeColor;

  FCheckboxForBoolean := True;
  FBoldNonDefaultValues := True;
  FDrawGridLines := True;
  FShowGutter := True;
  FShowStatusBar := True;
  FShowInfoBox := True;
end;

function TOIOptions.Load: boolean;
var
  Path: string;
  FileVersion: integer;

begin
  Result := False;
  if ConfigStore = nil then
    exit;
  try
    Path := 'ObjectInspectorOptions/';
    FileVersion := ConfigStore.GetValue(Path + 'Version/Value', 0);
    FSaveBounds := ConfigStore.GetValue(Path + 'Bounds/Valid', False);
    if FSaveBounds then
    begin
      FLeft := ConfigStore.GetValue(Path + 'Bounds/Left', 0);
      FTop := ConfigStore.GetValue(Path + 'Bounds/Top', 0);
      FWidth := ConfigStore.GetValue(Path + 'Bounds/Width', 250);
      FHeight := ConfigStore.GetValue(Path + 'Bounds/Height', 400);
    end;

    FDefaultItemHeight := ConfigStore.GetValue(Path + 'Bounds/DefaultItemHeight', 0);
    FShowComponentTree := ConfigStore.GetValue(Path + 'ComponentTree/Show/Value', True);
    FComponentTreeHeight := ConfigStore.GetValue(Path + 'ComponentTree/Height/Value', 160);

    FGridBackgroundColor := ConfigStore.GetValue(
      Path + 'Color/GridBackground', DefBackgroundColor);
    FSubPropertiesColor := ConfigStore.GetValue(
      Path + 'Color/SubProperties', DefSubPropertiesColor);
    FValueColor := ConfigStore.GetValue(Path + 'Color/Value', DefValueColor);
    FDefaultValueColor := ConfigStore.GetValue(Path + 'Color/DefaultValue',
      DefDefaultValueColor);
    FValueDifferBackgrndColor :=
      ConfigStore.GetValue(Path + 'Color/ValueDifferBackgrnd', DefValueDifferBackgrndColor);
    FReadOnlyColor := ConfigStore.GetValue(Path + 'Color/ReadOnly', DefReadOnlyColor);
    FReferencesColor := ConfigStore.GetValue(Path + 'Color/References', DefReferencesColor);
    FPropertyNameColor := ConfigStore.GetValue(Path + 'Color/PropertyName', DefNameColor);
    FHighlightColor := ConfigStore.GetValue(Path + 'Color/Highlight', DefHighlightColor);
    FHighlightFontColor := ConfigStore.GetValue(
      Path + 'Color/HighlightFont', DefHighlightFontColor);
    FGutterColor := ConfigStore.GetValue(Path + 'Color/Gutter', DefGutterColor);
    FGutterEdgeColor := ConfigStore.GetValue(Path + 'Color/GutterEdge', DefGutterEdgeColor);

    FShowHints := ConfigStore.GetValue(Path + 'ShowHints', FileVersion >= 3);
    FAutoShow := ConfigStore.GetValue(Path + 'AutoShow', True);
    FCheckboxForBoolean := ConfigStore.GetValue(Path + 'CheckboxForBoolean', True);
    FBoldNonDefaultValues := ConfigStore.GetValue(Path + 'BoldNonDefaultValues', True);
    FDrawGridLines := ConfigStore.GetValue(Path + 'DrawGridLines', True);
    FShowGutter := ConfigStore.GetValue(Path + 'ShowGutter', True);
    FShowStatusBar := ConfigStore.GetValue(Path + 'ShowStatusBar', True);
    FShowInfoBox := ConfigStore.GetValue(Path + 'ShowInfoBox', True);
    FInfoBoxHeight := ConfigStore.GetValue(Path + 'InfoBoxHeight', 80);
  except
    on E: Exception do
    begin
      DebugLn('ERROR: TOIOptions.Load: ', E.Message);
      exit;
    end;
  end;
  Result := True;
end;

function TOIOptions.Save: boolean;
var
  Path: string;
begin
  Result := False;
  if ConfigStore = nil then
    exit;
  try
    Path := 'ObjectInspectorOptions/';
    ConfigStore.SetValue(Path + 'Version/Value', OIOptionsFileVersion);
    ConfigStore.SetDeleteValue(Path + 'Bounds/Valid', FSaveBounds, False);
    if FSaveBounds then
    begin
      ConfigStore.SetValue(Path + 'Bounds/Left', FLeft);
      ConfigStore.SetValue(Path + 'Bounds/Top', FTop);
      ConfigStore.SetValue(Path + 'Bounds/Width', FWidth);
      ConfigStore.SetValue(Path + 'Bounds/Height', FHeight);
    end;

    ConfigStore.SetDeleteValue(Path + 'Bounds/DefaultItemHeight', FDefaultItemHeight, 0);
    ConfigStore.SetDeleteValue(Path + 'ComponentTree/Show/Value', FShowComponentTree, True);
    ConfigStore.SetDeleteValue(Path + 'ComponentTree/Height/Value',
      FComponentTreeHeight, 160);

    ConfigStore.SetDeleteValue(Path + 'Color/GridBackground', FGridBackgroundColor,
      DefBackgroundColor);
    ConfigStore.SetDeleteValue(Path + 'Color/SubProperties', FSubPropertiesColor,
      DefSubPropertiesColor);
    ConfigStore.SetDeleteValue(Path + 'Color/Value', FValueColor, DefValueColor);
    ConfigStore.SetDeleteValue(Path + 'Color/DefaultValue', FDefaultValueColor,
      DefDefaultValueColor);
    ConfigStore.SetDeleteValue(Path + 'Color/ValueDifferBackgrnd',
      FValueDifferBackgrndColor, DefValueDifferBackgrndColor);
    ConfigStore.SetDeleteValue(Path + 'Color/ReadOnly', FReadOnlyColor, DefReadOnlyColor);
    ConfigStore.SetDeleteValue(Path + 'Color/References', FReferencesColor,
      DefReferencesColor);
    ConfigStore.SetDeleteValue(Path + 'Color/PropertyName', FPropertyNameColor,
      DefNameColor);
    ConfigStore.SetDeleteValue(Path + 'Color/Highlight', FHighlightColor, DefHighlightColor);
    ConfigStore.SetDeleteValue(Path + 'Color/HighlightFont', FHighlightFontColor,
      DefHighlightFontColor);
    ConfigStore.SetDeleteValue(Path + 'Color/Gutter', FGutterColor, DefGutterColor);
    ConfigStore.SetDeleteValue(Path + 'Color/GutterEdge', FGutterEdgeColor,
      DefGutterEdgeColor);

    ConfigStore.SetDeleteValue(Path + 'ShowHints', FShowHints, True);
    ConfigStore.SetDeleteValue(Path + 'AutoShow', FAutoShow, True);
    ConfigStore.SetDeleteValue(Path + 'CheckboxForBoolean', FCheckboxForBoolean, True);
    ConfigStore.SetDeleteValue(Path + 'BoldNonDefaultValues', FBoldNonDefaultValues, True);
    ConfigStore.SetDeleteValue(Path + 'DrawGridLines', FDrawGridLines, True);
    ConfigStore.SetDeleteValue(Path + 'ShowGutter', FShowGutter, True);
    ConfigStore.SetDeleteValue(Path + 'ShowStatusBar', FShowStatusBar, True);
    ConfigStore.SetDeleteValue(Path + 'ShowInfoBox', FShowInfoBox, True);
    ConfigStore.SetDeleteValue(Path + 'InfoBoxHeight', FInfoBoxHeight, 80);
  except
    on E: Exception do
    begin
      DebugLn('ERROR: TOIOptions.Save: ', E.Message);
      exit;
    end;
  end;
  Result := True;
end;

procedure TOIOptions.AssignTo(AGrid: TCustomObjectInspector);
begin
  AGrid.BackgroundColor := FGridBackgroundColor;
  AGrid.SubPropertiesColor := FSubPropertiesColor;
  AGrid.ReferencesColor := FReferencesColor;
  AGrid.ReadOnlyColor := FReadOnlyColor;
  AGrid.ValueDifferBackgrndColor := FValueDifferBackgrndColor;
  AGrid.ValueFont.Color := FValueColor;
  if FBoldNonDefaultValues then
    AGrid.ValueFont.Style := [fsBold]
  else
    AGrid.ValueFont.Style := [];
  AGrid.DefaultValueFont.Color := FDefaultValueColor;
  AGrid.NameFont.Color := FPropertyNameColor;
  AGrid.HighlightColor := FHighlightColor;
  AGrid.HighlightFont.Color := FHighlightFontColor;
  AGrid.GutterColor := FGutterColor;
  AGrid.GutterEdgeColor := FGutterEdgeColor;
  AGrid.ShowHint := FShowHints;
  AGrid.DrawHorzGridLines := FDrawGridLines;
  AGrid.ShowGutter := FShowGutter;
  AGrid.CheckboxForBoolean := FCheckboxForBoolean;
end;


end.
