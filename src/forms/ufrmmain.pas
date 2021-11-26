{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmMain;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, Types, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ActnList, StdActns, ExtCtrls, ComCtrls, Contnrs, PropEdits,
  PropEditUtils, GraphPropEdits, ExtendedNotebook, Buttons, uCommon, uIObject,
  uIDocument, uMRUList, uIDesigner, uIMiscControls, uLayersInspector,
  uIniFileEx, uObjectInspector, LCLType, Spin, LMessages, ExtDlgs;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    CmdHelpDonation: TAction;
    CmdHelpAbout: TAction;
    CmdHelpHomepage: TAction;
    CmdToolCalc: TAction;
    CmdFileExport: TAction;
    CmdProjectSVG: TAction;
    CmdToolPasteFauIcon: TAction;
    CmdProjectStyles: TAction;
    CmdToolGenerateSourceCode: TAction;
    CmdToolsPreferences: TAction;
    CmdEditSelectAll: TAction;
    CmdEditRedo: TAction;
    CmdEditUndo: TAction;
    CmdObjectDuplicate: TAction;
    CmdEditPaste: TAction;
    CmdViewGrid: TAction;
    CmdEditCopy: TAction;
    CmdEditCut: TAction;
    CmdEditDelete: TAction;
    CmdLayerVisible: TAction;
    CmdLayerSendBackward: TAction;
    CmdLayerSendToBack: TAction;
    CmdLayerBringForward: TAction;
    CmdLayerBringToFront: TAction;
    CmdLayerLocked: TAction;
    CmdProjectImages: TAction;
    CmdObjectSendBackward: TAction;
    CmdObjectBringForward: TAction;
    CmdObjectSendToBack: TAction;
    CmdObjectBringToFront: TAction;
    CmdObjectUngroupAll: TAction;
    CmdObjectUngroup: TAction;
    CmdObjectGroup: TAction;
    CmdLayerRename: TAction;
    CmdLayerDelete: TAction;
    CmdLayerAdd: TAction;
    BvSplitMain: TBevel;
    CmdSelTool: TAction;
    CmdFileOpen: TAction;
    CmdFileSaveAs: TAction;
    CmdFileSave: TAction;
    CmdFileNew: TAction;
    CmdProjectSettings: TAction;
    CmdViewZoom75: TAction;
    App: TApplicationProperties;
    CmdViewRulers: TAction;
    CmdViewZoomFit: TAction;
    CmdViewZoom25: TAction;
    CmdViewZoom50: TAction;
    CmdViewZoom1600: TAction;
    CmdViewZoom1200: TAction;
    CmdViewZoom1000: TAction;
    CmdViewZoom800: TAction;
    CmdViewZoom600: TAction;
    CmdViewZoom500: TAction;
    CmdViewZoom400: TAction;
    CmdViewZoom300: TAction;
    CmdViewZoom200: TAction;
    CmdViewZoom100: TAction;
    ActionList1: TActionList;
    CmdFileExit: TFileExit;
    EnLayers: TExtendedNotebook;
    EnObj: TExtendedNotebook;
    ILMain: TImageList;
    ILMainD: TImageList;
    ILStatus: TImageList;
    ILIobjects: TImageList;
    LblObjName: TLabel;
    LblPropertyEditor: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    N5: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    msRfTop: TMenuItem;
    miFile: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    msRfBot: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    DlgOpen: TOpenDialog;
    PnEditor: TPanel;
    PnlContent: TPanel;
    PcIObjects: TPageControl;
    PnLeft: TPanel;
    PnlLayers: TPanel;
    PnlPropertyEditor: TPanel;
    PnPropertiesHdr: TPanel;
    PnToolbar: TPanel;
    PnTop: TPanel;
    DldSave: TSaveDialog;
    PmLayers: TPopupMenu;
    PmDesigner: TPopupMenu;
    RlLeft: TImage;
    RlTop: TImage;
    DlgExport: TSavePictureDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    BtnLayerLocked: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TsLayers: TTabSheet;
    TsObjGrid: TTabSheet;
    procedure AppHint(Sender: TObject);
    procedure AppIdle(Sender: TObject; var Done: boolean);
    procedure Button1Click(Sender: TObject);
    procedure CmdEditRedoExecute(Sender: TObject);
    procedure CmdEditRedoUpdate(Sender: TObject);
    procedure CmdEditSelectAllExecute(Sender: TObject);
    procedure CmdEditSelectAllUpdate(Sender: TObject);
    procedure CmdEditUndoExecute(Sender: TObject);
    procedure CmdEditUndoUpdate(Sender: TObject);
    procedure CmdFileExportExecute(Sender: TObject);
    procedure CmdFileNewExecute(Sender: TObject);
    procedure CmdFileOpenExecute(Sender: TObject);
    procedure CmdFileSaveAsExecute(Sender: TObject);
    procedure CmdFileSaveAsUpdate(Sender: TObject);
    procedure CmdFileSaveExecute(Sender: TObject);
    procedure CmdFileSaveUpdate(Sender: TObject);
    procedure CmdHelpAboutExecute(Sender: TObject);
    procedure CmdHelpDonationExecute(Sender: TObject);
    procedure CmdHelpHomepageExecute(Sender: TObject);
    procedure CmdProjectSVGExecute(Sender: TObject);
    procedure CmdToolCalcExecute(Sender: TObject);
    procedure CmdToolPasteFauIconExecute(Sender: TObject);
    procedure CmdToolPasteFauIconUpdate(Sender: TObject);
    procedure CmdProjectSettingsExecute(Sender: TObject);
    procedure CmdProjectImagesExecute(Sender: TObject);
    procedure CmdLayerAddExecute(Sender: TObject);
    procedure CmdLayerAddUpdate(Sender: TObject);
    procedure CmdLayerBringToFrontExecute(Sender: TObject);
    procedure CmdLayerBringToFrontUpdate(Sender: TObject);
    procedure CmdLayerDeleteExecute(Sender: TObject);
    procedure CmdLayerDeleteUpdate(Sender: TObject);
    procedure CmdLayerLockedExecute(Sender: TObject);
    procedure CmdLayerLockedUpdate(Sender: TObject);
    procedure CmdLayerRenameExecute(Sender: TObject);
    procedure CmdLayerRenameUpdate(Sender: TObject);
    procedure CmdLayerSendToBackUpdate(Sender: TObject);
    procedure CmdLayerVisibleExecute(Sender: TObject);
    procedure CmdLayerVisibleUpdate(Sender: TObject);
    procedure CmdObjectBringToFrontExecute(Sender: TObject);
    procedure CmdObjectBringToFrontUpdate(Sender: TObject);
    procedure CmdEditCutExecute(Sender: TObject);
    procedure CmdEditDeleteExecute(Sender: TObject);
    procedure CmdEditDeleteUpdate(Sender: TObject);
    procedure CmdObjectDuplicateExecute(Sender: TObject);
    procedure CmdObjectGroupExecute(Sender: TObject);
    procedure CmdObjectGroupUpdate(Sender: TObject);
    procedure CmdEditPasteExecute(Sender: TObject);
    procedure CmdEditPasteUpdate(Sender: TObject);
    procedure CmdObjectUngroupAllExecute(Sender: TObject);
    procedure CmdObjectUngroupExecute(Sender: TObject);
    procedure CmdObjectUngroupUpdate(Sender: TObject);
    procedure CmdProjectStylesExecute(Sender: TObject);
    procedure CmdSelToolExecute(Sender: TObject);
    procedure CmdSelToolUpdate(Sender: TObject);
    procedure CmdToolGenerateSourceCodeExecute(Sender: TObject);
    procedure CmdToolsPreferencesExecute(Sender: TObject);
    procedure CmdViewGridExecute(Sender: TObject);
    procedure CmdViewGridUpdate(Sender: TObject);
    procedure CmdViewRulersExecute(Sender: TObject);
    procedure CmdViewRulersUpdate(Sender: TObject);
    procedure CmdViewZoom100Execute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PcIObjectsChange(Sender: TObject);
    procedure RlTopResize(Sender: TObject);
    procedure StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    fDesigner: TIDesigner;
    fInCtrl: TFinalEditController;
    fOutCtrl: TOutputController;
    fObjectInspector: TObjectInspector;
    fLayersInspector: TLayersInspector;
    fCursorPt: TPoint;
    fMRUList: TMRUList;
    function GetDocument: TIDocument;
    function EditDocProperties(AValue: TIDocument; undabled: boolean): boolean;
    function CanCloseDocument: boolean;
    procedure DesignerScaleChanged(Sender: TObject);
    procedure DesignerScroll(Sender: TObject);
    procedure DesignerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure DesignerMouseLeave(Sender: TObject);
    procedure DesignerFindObject(Sender: TObject; var AObject: TIObject);
    procedure DesignerObjectPut(Sender: TObject; AObject: TIObject;
      Index, X, Y: integer; var AParent: TIObject; var Pos: TIFloatPoint);
    procedure DesignerBeginSelectRect(Sender: TObject; var AObject: TIObject);
    procedure DesignerEndConstruct(Sender: TObject; AObject: TIObject);
    procedure ObjectInspectorSelectionChange(Sender: TObject);
    procedure DrawRuler;
    procedure SetDocument(AValue: TIDocument);
    procedure CloseDocument;
    procedure SaveDocument;
    procedure SaveDocumentAs;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OpenDocument(aFileName: string);
    procedure RecentFileClick(Sender: TObject; const aFileName: string);
    procedure RegIObject(category: string; AClass: TClass);
    procedure RegIObjects(category: string; Classes: array of string);
    procedure ObjButtonDown(Sender: TObject);
    procedure ResetObjButton;
    procedure FillIObjects;
    procedure ResetConstructObject;
    procedure LayerNameInputCloseQuery(Sender: TObject; const AValue: string;
      var ACanClose: boolean);
    procedure CopyToClipboard(const AsCut: boolean);
    procedure PasteFromClipboard;
  public
    function NewDesign(AWidth, AHeight: double): TIDocument;
    property Document: TIDocument read GetDocument write SetDocument;
    property InCtrl: TFinalEditController read fInCtrl;
    property OutCtrl: TOutputController read fOutCtrl;
    property MRUList: TMRUList read fMRUList;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}
{$R misc.res}

uses
  LCLIntf, Math, ImgList, CalcForm, uFrmDocProperties, uStorage, uFrmInputText,
  uFrmImages, uFrmGenerateCode, uFrmStyles, uFrmFauIcons, uFrmSVG,
  uFrmPreferences, uFrmAbout, uClipboard, uCommands, uGraphics;

var
  Fontdir: string;

procedure Init;

  procedure AddFont(fName: string);
  begin
    fName := Fontdir + fName;
    if not FileExists(fName) then
    begin
      MessageDlg('Error', Format(SFontFileNotFound, [fName]), mtError,
        [mbAbort], '');
      Application.Terminate;
    end;
    AddFontResource(PChar(fName));
  end;

begin
  DefaultFormatSettings.DecimalSeparator := '.';
  Fontdir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName) +
    'fonts');
  if not DirectoryExists(fontdir) then
  begin
    MessageDlg('Error', Format(SFontDirNotExists, [Fontdir]), mtError, [mbAbort], '');
    Application.Terminate;
  end;
  if Screen.Fonts.IndexOf('fontaudio') < 0 then
    AddFont('fontaudio.ttf');
  if Screen.Fonts.IndexOf('Roboto') < 0 then
    AddFont('Roboto-Regular.ttf');
  if Screen.Fonts.IndexOf('forkawesome') < 0 then
    AddFont('forkawesome-webfont.ttf');
end;

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  UnitsPerInch := ScreenInfo.PixelsPerInchX;
  fDesigner := TIDesigner.Create(self);
  fDesigner.Name := 'Designer';
  fDesigner.Align := alClient;
  PnEditor.InsertControl(fDesigner);
  fInCtrl := TFinalEditController.Create(fDesigner);
  fInCtrl.GridSize := 25;
  fInCtrl.OnFindObject := @DesignerFindObject;
  fInCtrl.OnPutObject := @DesignerObjectPut;
  fInCtrl.OnBeginSelectRect := @DesignerBeginSelectRect;
  fInCtrl.OnEndConstruct := @DesignerEndConstruct;
  fOutCtrl := TOutputController.Create(fDesigner);
  fDesigner.InputController := fInCtrl;
  fDesigner.OutputController := fOutCtrl;
  fDesigner.PopupMenu := PmDesigner;
  fDesigner.OnScroll := @DesignerScroll;
  fDesigner.OnScaleChanged := @DesignerScaleChanged;
  fDesigner.OnMouseMove := @DesignerMouseMove;
  fDesigner.OnMouseLeave := @DesignerMouseLeave;
  fObjectInspector := TObjectInspector.CreateWithParams(Self, AllTypeKinds, 21);
  fObjectInspector.Name := 'ObjectInspector';
  fObjectInspector.Parent := PnlPropertyEditor;
  fObjectInspector.Align := alClient;
  fObjectInspector.OnSelectionChange := @ObjectInspectorSelectionChange;
  fLayersInspector := TLayersInspector.Create(self);
  fLayersInspector.Parent := PnlLayers;
  fLayersInspector.Align := alClient;
  fLayersInspector.Color := ColorToRGB(clWindow);
  fLayersInspector.PopupMenu := PmLayers;
  BtnLayerLocked.ImageIndex := 21;
  with RlTop.Picture.Bitmap.Canvas do
  begin
{$IFDEF MSWINDOWS}
    Font.Name := 'Tahoma';
{$ENDIF}
    Font.Size := 7;
  end;
  with RlLeft.Picture.Bitmap.Canvas do
  begin
{$IFDEF MSWINDOWS}
    Font.Name := 'Tahoma';
{$ENDIF}
    Font.Size := 7;
    Font.Orientation := 900;
  end;

  FillIObjects;
  DrawDisabledImagelist(ILMain, ILMainD);
  fMRUList := TMRUList.Create(self);
  fMRUList.MIRecent := miFile;
  fMRUList.TopSepar := msRfTop;
  fMRUList.BotSepar := msRfBot;
  fMRUList.OnRecent := @RecentFileClick;
  LoadSettings;
  Document := NewDesign(640, 480);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  fInCtrl.Free;
  fOutCtrl.Free;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  fMRUList.UpdateRecentFiles;
  ResetObjButton;
  fObjectInspector.InspectObject(nil);
  if ParamCount > 0 then
    OpenDocument(Paramstr(1));
end;

procedure TFrmMain.PcIObjectsChange(Sender: TObject);
begin
  CmdSelTool.Execute;
end;

procedure TFrmMain.LoadSettings;
var
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(IniPath);
  try
    fMRUList.LoadFromIni(ini, 'Recent Files');
    Application.ShowHint := ini.ReadBool(SCommon, 'ShowHints', true);
    OutCtrl.ShowGrid := ini.ReadBool(SGrid, 'ShowGrid', true);
    OutCtrl.GridStyle := TGridStyle(ini.ReadInteger(SGrid, 'GridStyle', 0));
    InCtrl.SnapToGrid := ini.ReadBool(SGrid, 'SnapToGrid', true);
    InCtrl.GridSize := ini.ReadFloat(SGrid, 'GridSize', 10);
    ReplaceRoboto := ini.ReadBool(SCommon, 'ReplaceRoboto', ReplaceRoboto);
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.SaveSettings;
var
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(IniPath);
  try
    fMRUList.SaveToIni(ini, 'Recent Files');
    ini.WriteBool(SCommon, 'ShowHints', Application.ShowHint);
    ini.WriteBool(SCommon, 'ReplaceRoboto', ReplaceRoboto);
    ini.WriteBool(SGrid, 'ShowGrid', OutCtrl.ShowGrid);
    ini.WriteInteger(SGrid, 'GridStyle', Ord(OutCtrl.GridStyle));
    ini.WriteBool(SGrid, 'SnapToGrid', InCtrl.SnapToGrid);
    ini.WriteFloat(SGrid, 'GridSize', InCtrl.GridSize);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.DesignerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  fCursorPt := Point(X, Y);
  DrawRuler;
end;

procedure TFrmMain.DesignerMouseLeave(Sender: TObject);
begin
  DrawRuler;
end;

procedure TFrmMain.DesignerFindObject(Sender: TObject; var AObject: TIObject);
begin
  AObject := Document.ActiveLayer;
end;

procedure TFrmMain.DesignerObjectPut(Sender: TObject; AObject: TIObject;
  Index, X, Y: integer; var AParent: TIObject; var Pos: TIFloatPoint);
var
  ARect: TIFloatRect;
begin
  fInCtrl.RotateMode := False;
  if Document.ActiveLayer = nil then
  begin
    MessageDlg(AppTitle, 'The active layer is not selected.', mtWarning, [mbOK], '');
    Abort;
  end
  else
  begin
    AParent := Document.ActiveLayer;
    ARect := AObject.GetBounds(True);
    Pos.X := Pos.X - (ARect.Right - ARect.Left) / 2 + Index * fInCtrl.GridSize * 2;
    Pos.Y := Pos.Y - (ARect.Bottom - ARect.Top) / 2 + Index * fInCtrl.GridSize * 2;
    if fInCtrl.SnapToGrid then
      Pos := PointToGrid(Pos, fInCtrl.GridSize);
    Pos := AObject.AbsoluteToRelative(Pos);
  end;
end;

procedure TFrmMain.DesignerBeginSelectRect(Sender: TObject; var AObject: TIObject);
begin
  AObject := Document.ActiveLayer;
end;

procedure TFrmMain.DesignerEndConstruct(Sender: TObject; AObject: TIObject);
begin
  fObjectInspector.InspectObject(AObject);
  CmdSelTool.Execute;
end;

procedure TFrmMain.ObjectInspectorSelectionChange(Sender: TObject);
begin
  if Document = nil then
    LblObjName.Caption := 'None'
  else
  if Document.SelectedCount = 0 then
    LblObjName.Caption := 'None'
  else
  if Document.SelectedCount > 1 then
    LblObjName.Caption := Format('%d objects selected', [Document.SelectedCount])
  else
    LblObjName.Caption := Document.Selected[0].GetObjectName;
end;

procedure TFrmMain.CmdSelToolExecute(Sender: TObject);
begin
  ResetConstructObject;
  ResetObjButton;
end;

procedure TFrmMain.CmdSelToolUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document);
end;

procedure TFrmMain.CmdViewGridExecute(Sender: TObject);
begin
  fOutCtrl.ShowGrid := not fOutCtrl.ShowGrid;
end;

procedure TFrmMain.CmdViewGridUpdate(Sender: TObject);
begin
  CmdViewGrid.Enabled := Assigned(Document);
  CmdViewGrid.Checked := CmdViewGrid.Enabled and fOutCtrl.ShowGrid;
end;

procedure TFrmMain.RlTopResize(Sender: TObject);
begin
  with TImage(Sender) do
    Picture.Bitmap.SetSize(Width, Height);
  DrawRuler;
end;

procedure TFrmMain.StatusBarDrawPanel(AStatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  Style: TTextStyle;
begin
  ILStatus.Draw(AStatusBar.Canvas, Rect.Left + 2, Rect.Top + 2, Panel.Index);
  FillChar(Style, SizeOf(Style), 0);
  with Style do
  begin
    Clipping := True;
    SingleLine := True;
    ExpandTabs := True;
  end;
  AStatusBar.Canvas.TextRect(Rect, Rect.Left + 17, Rect.Top + 1, Panel.Text, Style);
end;

procedure TFrmMain.CmdViewRulersExecute(Sender: TObject);
begin
  RlTop.Visible := not RlTop.Visible;
  RlLeft.Visible := RlTop.Visible;
end;

procedure TFrmMain.AppIdle(Sender: TObject; var Done: boolean);
const
  aMod: array[boolean] of string = ('', '*');
var
  lX, lY: double;
begin
  if Assigned(Document) then
  begin
    if Document.Modified then
      StatusBar.Panels[1].Text := 'Modified'
    else
      StatusBar.Panels[1].Text := '';
    if fDesigner.MouseInClient then
    begin
      fDesigner.ScreenToLog(fCursorPt.X, fCursorPt.Y, lX, lY);
      StatusBar.Panels[2].Text := Format('X: %6.1f; Y: %6.1f', [lX, lY]);
    end
    else
      StatusBar.Panels[2].Text := '';
    Caption := Format('%s%s - [%s]', [aMod[Document.Modified], AppTitle,
      Document.FileName]);
  end
  else
  begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
    Caption := AppTitle;
  end;
end;

procedure TFrmMain.Button1Click(Sender: TObject);

begin

end;

procedure TFrmMain.CmdEditRedoExecute(Sender: TObject);
begin
  Document.History.Redo;
end;

procedure TFrmMain.CmdEditRedoUpdate(Sender: TObject);
begin
  if Assigned(Document) then
    Document.History.UpdateRedo(CmdEditRedo)
  else
    CmdEditRedo.Enabled := False;
end;

procedure TFrmMain.CmdEditSelectAllExecute(Sender: TObject);
begin
  Document.SelectedAll;
end;

procedure TFrmMain.CmdEditSelectAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and Assigned(Document.ActiveLayer);
end;

procedure TFrmMain.CmdEditUndoExecute(Sender: TObject);
begin
  Document.History.Undo;
end;

procedure TFrmMain.CmdEditUndoUpdate(Sender: TObject);
begin
  if Assigned(Document) then
    Document.History.UpdateUndo(CmdEditUndo)
  else
    CmdEditUndo.Enabled := False;
end;

procedure TFrmMain.CmdFileExportExecute(Sender: TObject);
begin
  DlgExport.FileName := ExtractFileName(ChangeFileExt(Document.FileName, '.png'));
  if not DlgExport.Execute then
    exit;
  Screen.Cursor := crHourGlass;
  try
    Document.ExportToImage(DlgExport.FileName);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.AppHint(Sender: TObject);
begin
  StatusBar.Panels[3].Text := StringReplace(Application.Hint, LineEnding,
    ' ', [rfReplaceAll]);
end;

procedure TFrmMain.CmdViewRulersUpdate(Sender: TObject);
begin
  CmdViewRulers.Checked := RlTop.Visible;
end;

procedure TFrmMain.CmdViewZoom100Execute(Sender: TObject);
var
  t: integer;
begin
  t := TAction(Sender).Tag;
  if t > 0 then
  begin
    fDesigner.ScaleMode := smManual;
    fDesigner.Scale := t;
  end
  else
  begin
    case t of
      -1: fDesigner.ScaleMode := smFitXY;
      -2: fDesigner.ScaleMode := smFitX;
      -3: fDesigner.ScaleMode := smFitY;
    end;
  end;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanCloseDocument;
  if CanClose then
    CloseDocument;
  SaveSettings;
end;

function TFrmMain.GetDocument: TIDocument;
begin
  Result := fDesigner.Document;
end;

procedure TFrmMain.SetDocument(AValue: TIDocument);
begin
  fDesigner.Document := AValue;
  fLayersInspector.Document := AValue;
  fObjectInspector.Document := AValue;
end;

procedure TFrmMain.DesignerScaleChanged(Sender: TObject);
begin
  DrawRuler;
end;

procedure TFrmMain.DesignerScroll(Sender: TObject);
begin
  DrawRuler;
  RedrawWindow(PnEditor.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or
    RDW_ALLCHILDREN);
end;

procedure TFrmMain.DrawRuler;

  procedure DrawHTick(sX, pX: double; i: integer);
  var
    s: string;
    x: integer;
  begin
    with RlTop.Picture.Bitmap.Canvas do
    begin
      x := Round(sX) + 16;
      if (x > 15) and (fOutCtrl.LargeGridSize > 0) then
      begin
        if i mod fOutCtrl.LargeGridSize = 0 then
        begin
          Pen.Color := clGray;
          MoveTo(x, 11);
          LineTo(x, 15);
          s := IntToStr(Round(pX));
          Dec(x, TextWidth(s) div 2);
          Textout(x, 0, s);
        end
        else
        begin
          Pen.Color := clActiveBorder;
          MoveTo(x, 13);
          LineTo(x, 15);
        end;
      end;
    end;
  end;

  procedure DrawVTick(sY, pY: double; i: integer);
  var
    s: string;
    y: integer;
  begin
    with RlLeft.Picture.Bitmap.Canvas do
    begin
      y := Round(sY);
      if (y > 0) and (fOutCtrl.LargeGridSize > 0) then
      begin
        if i mod fOutCtrl.LargeGridSize = 0 then
        begin
          Pen.Color := clGray;
          MoveTo(11, y);
          LineTo(15, y);
          s := IntToStr(Round(pY));
          Inc(y, TextWidth(s) div 2);
          Textout(0, y, s);
        end
        else
        begin
          Pen.Color := clActiveBorder;
          MoveTo(13, y);
          LineTo(15, y);
        end;
      end;
    end;
  end;

var
  FloatBox, VpR: TIFloatRect;
  p1, p2: TIFloatPoint;
  i: integer;
  sX, sY: double;
begin
  if (not RlTop.Visible) or (not RlLeft.Visible) then
    exit;

  with RlTop.Picture.Bitmap.Canvas do
  begin
    Brush.Color := clWindow;
    FillRect(RlTop.ClientRect);
    Pen.Style := psSolid;
    Pen.Color := clActiveBorder;
    MoveTo(15, 15);
    LineTo(RlTop.Width, 15);
    if Assigned(Document) then
    begin
      VpR := fDesigner.Viewport;
      fDesigner.LogToScreen(VpR.Left, VpR.Top, FloatBox.Left, FloatBox.Top);
      fDesigner.LogToScreen(VpR.Right, VpR.Bottom, FloatBox.Right, FloatBox.Bottom);
      p1 := VpR.TopLeft;
      fDesigner.ScreenToLog(RlTop.Width, RlLeft.Height, p2.X, p2.Y);
      Pen.Color := clGray;
      i := 0;
      while p1.X < p2.X do
      begin
        fDesigner.LogToScreen(p1.X, 0, sX, sY);
        DrawHTick(sX, p1.X, i);
        p1.X += fOutCtrl.StepSize;
        Inc(i);
      end;
      p1.X := VpR.Left;
      fDesigner.ScreenToLog(0, 0, p2.X, p2.Y);
      i := 0;
      while p1.X > p2.X do
      begin
        fDesigner.LogToScreen(p1.X, 0, sX, sY);
        DrawHTick(sX, p1.X, i);
        p1.X -= fOutCtrl.StepSize;
        Dec(i);
      end;
      if fDesigner.MouseInClient then
      begin
        Pen.Color := clRed;
        Pen.Style := psDot;
        MoveTo(fCursorPt.X + 16, 0);
        LineTo(fCursorPt.X + 16, RlTop.Height);
      end;
    end;
  end;

  with RlLeft.Picture.Bitmap.Canvas do
  begin
    Brush.Color := clWindow;
    FillRect(ClipRect);
    Pen.Style := psSolid;
    Pen.Color := clActiveBorder;
    MoveTo(15, 0);
    LineTo(15, RlLeft.Height);
    if Assigned(Document) then
    begin
      Pen.Color := clGray;
      fDesigner.ScreenToLog(RlTop.Width, RlLeft.Height, p2.X, p2.Y);
      i := 0;
      while p1.Y < p2.Y do
      begin
        fDesigner.LogToScreen(0, p1.Y, sX, sY);
        DrawVTick(sY, p1.Y, i);
        p1.Y += fOutCtrl.StepSize;
        Inc(i);
      end;
      p1.Y := VpR.Top;
      fDesigner.ScreenToLog(0, 0, p2.X, p2.Y);
      i := 0;
      while p1.Y > p2.Y do
      begin
        fDesigner.LogToScreen(0, p1.Y, sX, sY);
        DrawVTick(sY, p1.Y, i);
        p1.Y -= fOutCtrl.StepSize;
        Dec(i);
      end;
      if fDesigner.MouseInClient then
      begin
        Pen.Color := clRed;
        Pen.Style := psDot;
        MoveTo(0, fCursorPt.Y);
        LineTo(RlLeft.Width, fCursorPt.Y);
      end;
    end;
  end;
  StatusBar.Panels[0].Text := IntToStr(Round(fDesigner.Scale)) + '%';
end;

procedure TFrmMain.CloseDocument;
begin
  fInCtrl.ClearSelection;
  ResetConstructObject;
  ResetObjButton;
  Document.Free;
  Document := nil;
end;

procedure TFrmMain.SaveDocument;
begin
  if FileExists(Document.FileName) then
  begin
    Screen.Cursor := crHourGlass;
    try
      Document.SaveToFile(Document.FileName);
      fMRUList.AddToRecent(Document.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
  end
  else
    SaveDocumentAs;
end;

procedure TFrmMain.SaveDocumentAs;
begin
  if not DldSave.Execute then
    exit;
  Screen.Cursor := crHourGlass;
  try
    Document.SaveToFile(DldSave.FileName);
    fMRUList.AddToRecent(DldSave.FileName);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.OpenDocument(aFileName: string);
begin
  Screen.Cursor := crHourGlass;
  try
    Document.LoadFromFile(aFileName);
    if FileExists(aFileName) then
      fMRUList.AddToRecent(aFileName);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.RecentFileClick(Sender: TObject; const aFileName: string);
begin
  Application.ProcessMessages;
  if not CanCloseDocument then
    exit;
  CloseDocument;
  Document := NewDesign(640, 480);
  OpenDocument(aFileName);
end;

function TFrmMain.NewDesign(AWidth, AHeight: double): TIDocument;
begin
  Result := TIDocument.Create(AWidth, AHeight, UnitsPerInch, iclWhite);
end;

procedure TFrmMain.CmdProjectSettingsExecute(Sender: TObject);
begin
  EditDocProperties(Document, True);
end;

procedure TFrmMain.CmdProjectImagesExecute(Sender: TObject);
begin
  ShowImages(Document);
end;

procedure TFrmMain.CmdProjectSVGExecute(Sender: TObject);
begin
  ShowSVG(Document);
end;

procedure TFrmMain.CmdToolCalcExecute(Sender: TObject);
var
  dlg: TCustomForm;
begin
  dlg := CreateCalculatorForm(Application, clNormal, 0);
  try
    dlg.Caption := 'Calculator';
    dlg.Position := poOwnerFormCenter;
    dlg.BorderStyle := bsSingle;
    dlg.BorderIcons := [biSystemMenu];
    dlg.ShowModal;
  finally
    dlg.Release;
  end;
end;

procedure TFrmMain.CmdProjectStylesExecute(Sender: TObject);
begin
  ShowStyles(Document);
end;

procedure TFrmMain.CmdFileNewExecute(Sender: TObject);
var
  d: TIDocument;
begin
  if not CanCloseDocument then
    exit;
  d := NewDesign(640, 480);
  try
    if EditDocProperties(d, False) then
    begin
      CloseDocument;
      Document := NewDesign(640, 480);
      Document.CopySettingsFrom(d);
    end;
  finally
    d.Free;
  end;
end;

procedure TFrmMain.CmdFileOpenExecute(Sender: TObject);
begin
  if CanCloseDocument and DlgOpen.Execute then
  begin
    CloseDocument;
    Document := NewDesign(640, 480);
    OpenDocument(DlgOpen.FileName);
  end;
end;

procedure TFrmMain.CmdFileSaveAsExecute(Sender: TObject);
begin
  SaveDocumentAs;
end;

procedure TFrmMain.CmdFileSaveAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document);
end;

procedure TFrmMain.CmdFileSaveExecute(Sender: TObject);
begin
  SaveDocument;
end;

procedure TFrmMain.CmdFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and Document.Modified;
end;

procedure TFrmMain.CmdHelpAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TFrmMain.CmdHelpDonationExecute(Sender: TObject);
begin
  OpenUrl('http://stone-voices.ru/donation/?lang=en');
end;

procedure TFrmMain.CmdToolPasteFauIconExecute(Sender: TObject);
var
  s: string;
  e: TCustomEdit;
begin
  if not ShowFauIcons(s) then
    exit;
  e := ActiveControl as TCustomEdit;
  e.SelText := s;
end;

procedure TFrmMain.CmdToolPasteFauIconUpdate(Sender: TObject);
begin
  CmdToolPasteFauIcon.Enabled := Assigned(Document) and (ActiveControl is TCustomEdit);
end;

function TFrmMain.EditDocProperties(AValue: TIDocument; undabled: boolean): boolean;
var
  cmd: TObject;
begin
  Result := False;
  Application.CreateForm(TFrmDocProperties, FrmDocProperties);
  with FrmDocProperties do
    try
      EdWidth.Value := Trunc(AValue.Width);
      EdHeight.Value := Trunc(AValue.Height);
      EdResolution.Value := Trunc(AValue.UnitsPerInch);
      EdDescription.Text := AValue.Description;
      BgColor := AValue.BgColor;
      PropertyChanged := not undabled;
      Result := ShowModal = mrOk;
      if Result then
      begin
        if undabled then
          cmd := AValue.GetPropertiesCommand;
        AValue.Width := EdWidth.Value;
        AValue.Height := EdHeight.Value;
        AValue.UnitsPerInch := EdResolution.Value;
        AValue.Description := EdDescription.Text;
        AValue.BgColor := BgColor;
        if undabled then
        begin
          TDocumentPropertiesCommand(cmd).Commit(AValue);
          AValue.History.Add(cmd);
        end;
      end;
    finally
      Release;
    end;
end;

function TFrmMain.CanCloseDocument: boolean;
begin
  Result := True;
  if Assigned(Document) and Document.Modified then
  begin
    case MessageDlg(AppTitle, 'Save changes to "' + ExtractFilename(Document.FileName) +
        '" before closing?', mtConfirmation, mbYesNoCancel, '') of
      mrYes: SaveDocument;
      mrCancel: Result := False;
    end;
  end;
end;

procedure TFrmMain.ResetConstructObject;
begin
  if Document = nil then
    exit;
  if Assigned(fInCtrl.ConstructPrototype) then
    fInCtrl.ConstructPrototype.Free;
  fInCtrl.ConstructPrototype := nil;
  fInCtrl.RotateMode := False;
end;

procedure TFrmMain.ObjButtonDown(Sender: TObject);
var
  AClass: TClass;
begin
  AClass := TIObjButton(Sender).ObjClass;
  if AClass = nil then
  begin
    ResetConstructObject;
    exit;
  end;
  fInCtrl.ConstructPrototype := TIObjectClass(AClass).Create(nil, Document);
end;

procedure TFrmMain.ResetObjButton;
var
  Panel: TPanel;
begin
  if PcIObjects.ActivePage = nil then
    Exit;
  Panel := TPanel(PcIObjects.ActivePage.Controls[0]);
  if Panel.ControlCount > 0 then
    TSpeedButton(Panel.Controls[0]).Down := True;
end;

procedure TFrmMain.LayerNameInputCloseQuery(Sender: TObject;
  const AValue: string; var ACanClose: boolean);
begin
  if Trim(AValue) = '' then
  begin
    MessageDlg(AppTitle, 'Layer name can''t be empty.', mtWarning, [mbOK], '');
    ACanClose := False;
  end;
end;

procedure TFrmMain.CmdLayerAddExecute(Sender: TObject);
var
  s: string;
begin
  s := 'Layer ' + IntToStr(Document.LayersCount + 1);
  if not InputText('Enter a new layer name:', @LayerNameInputCloseQuery, s) then
    exit;
  Document.AddLayer(s);
end;

procedure TFrmMain.CmdLayerAddUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document);
end;

procedure TFrmMain.CmdLayerBringToFrontExecute(Sender: TObject);
begin
  Document.SetLayerOrder(TOrder(TAction(Sender).Tag));
end;

procedure TFrmMain.CmdLayerBringToFrontUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and
    Document.CanLayerOrder(TOrder(TAction(Sender).Tag));
end;

procedure TFrmMain.CmdLayerSendToBackUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and Assigned(Document.ActiveLayer) and
    (Document.ActiveLayerIndex > 0);
end;

procedure TFrmMain.CmdLayerVisibleExecute(Sender: TObject);
begin
  Document.ActiveLayer.Visible := not Document.ActiveLayer.Visible;
end;

procedure TFrmMain.CmdLayerVisibleUpdate(Sender: TObject);
begin
  CmdLayerVisible.Enabled := Assigned(Document) and Assigned(Document.ActiveLayer);
  CmdLayerVisible.Checked := CmdLayerVisible.Enabled and Document.ActiveLayer.Visible;
end;

procedure TFrmMain.CmdLayerDeleteExecute(Sender: TObject);
begin
  Document.DeleteLayers(fLayersInspector.SelectedList);
end;

procedure TFrmMain.CmdLayerDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and (fLayersInspector.SelectedCount > 0);
end;

procedure TFrmMain.CmdLayerLockedExecute(Sender: TObject);
begin
  Document.ActiveLayer.Locked := not Document.ActiveLayer.Locked;
end;

procedure TFrmMain.CmdLayerLockedUpdate(Sender: TObject);
begin
  CmdLayerLocked.Enabled := Assigned(Document) and Assigned(Document.ActiveLayer);
  CmdLayerLocked.Checked := CmdLayerLocked.Enabled and Document.ActiveLayer.Locked;
end;

procedure TFrmMain.CmdLayerRenameExecute(Sender: TObject);
var
  s: string;
begin
  s := Document.ActiveLayer.Name;
  if not InputText('Enter a new layer name:', @LayerNameInputCloseQuery, s) then
    exit;
  Document.RenameLayer(s);
end;

procedure TFrmMain.CmdLayerRenameUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and Assigned(Document.ActiveLayer);
end;

procedure TFrmMain.CmdObjectGroupExecute(Sender: TObject);
begin
  Document.GroupSelected;
end;

procedure TFrmMain.CmdObjectGroupUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(Document) and Document.CanGroupSelected;
end;

procedure TFrmMain.CmdObjectUngroupAllExecute(Sender: TObject);
begin
  Document.UnGroupAllSelected;
end;

procedure TFrmMain.CmdObjectUngroupExecute(Sender: TObject);
begin
  Document.UnGroupSelected;
end;

procedure TFrmMain.CmdObjectUngroupUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and Document.CanUnGroupSelected;
end;

procedure TFrmMain.CmdObjectBringToFrontExecute(Sender: TObject);
begin
  Document.SetObjectsOrder(TOrder(TAction(Sender).Tag));
end;

procedure TFrmMain.CmdObjectBringToFrontUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and
    Document.CanObjectsOrder(TOrder(TAction(Sender).Tag));
end;

procedure TFrmMain.PasteFromClipboard;
var
  List: TObjectList;
  i: integer;
begin
  List := TObjectList.Create(False);
  try
    Document.PasteFromClipboard(List);
    fInCtrl.ClearSelection;
    for i := 0 to List.Count - 1 do
      TIObject(List[i]).Selected := True;
  finally
    List.Free;
  end;
end;

procedure TFrmMain.CmdEditPasteExecute(Sender: TObject);
begin
  if ActiveControl is TCustomEdit then
  begin
    TCustomEdit(ActiveControl).PasteFromClipboard;
    exit;
  end;
  if ActiveControl is TValueComboBox then
  begin
    TValueComboBox(ActiveControl).PasteFromClipboard;
    exit;
  end;
  PasteFromClipboard;
end;

procedure TFrmMain.CmdEditPasteUpdate(Sender: TObject);
begin
  if (ActiveControl is TCustomEdit) or (ActiveControl is TValueComboBox) then
    (Sender as TAction).Enabled := Clipboard.HasFormat(CF_TEXT)
  else
    (Sender as TAction).Enabled :=
      Assigned(Document) and Clipboard.HasFormat(CF_IPLUGDESIGNER);
end;

procedure TFrmMain.CopyToClipboard(const AsCut: boolean);
var
  List: TObjectList;
  i: integer;
begin
  List := TObjectList.Create(False);
  try
    for i := 0 to Document.SelectedCount - 1 do
      List.Add(Document.Selected[i]);
    Document.CopyToClipboard(List, AsCut);
    if AsCut then
      fInCtrl.RemoveObjects(List);
  finally
    List.Free;
  end;
end;

procedure TFrmMain.CmdEditCutExecute(Sender: TObject);
begin
  if ActiveControl is TCustomEdit then
  begin
    if TAction(Sender).Tag = 0 then
      TCustomEdit(ActiveControl).CopyToClipboard
    else
      TCustomEdit(ActiveControl).CutToClipboard;
    exit;
  end;
  if ActiveControl is TValueComboBox then
  begin
    if TAction(Sender).Tag = 0 then
      TValueComboBox(ActiveControl).CopyToClipboard
    else
      TValueComboBox(ActiveControl).CutToClipboard;
    exit;
  end;
  CopyToClipboard(TAction(Sender).Tag <> 0);
end;

procedure TFrmMain.CmdEditDeleteExecute(Sender: TObject);
begin
  Document.DeleteSelected;
end;

procedure TFrmMain.CmdEditDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Document) and (Document.SelectedCount > 0);
end;

procedure TFrmMain.CmdObjectDuplicateExecute(Sender: TObject);
begin
  Document.DuplicateSelected;
end;

procedure TFrmMain.CmdToolGenerateSourceCodeExecute(Sender: TObject);
begin
  Application.CreateForm(TFrmGenerateCode, FrmGenerateCode);
  with FrmGenerateCode do
    try
      Generate(Document);
      ShowModal;
    finally
      Release;
    end;
end;

procedure TFrmMain.CmdToolsPreferencesExecute(Sender: TObject);
begin
  ShowPreferences;
end;

procedure TFrmMain.CmdHelpHomepageExecute(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/?lang=en');
end;

procedure TFrmMain.RegIObject(category: string; AClass: TClass);

  function CreateIObjButton(APanel: TPanel): TIObjButton;
  begin
    Result := TIObjButton.Create(APanel);
    Result.GroupIndex := 2;
    Result.Width := 28;
    Result.Height := 28;
    Result.Top := 2;
    Result.Flat := True;
    Result.ShowHint := True;
    Result.Parent := APanel;
    Result.OnClick := @ObjButtonDown;
  end;

  function GetPageOfName(AName: string): TTabSheet;
  var
    z: integer;
  begin
    Result := nil;
    for z := 0 to PcIObjects.PageCount - 1 do
    begin
      if PcIObjects.Pages[z].Caption = AName then
      begin
        Result := PcIObjects.Pages[z];
        Break;
      end;
    end;
  end;

var
  n: integer;
  Panel: TPanel;
  Button: TIObjButton;
  TabSheet: TTabSheet;
  bmp: TCustomBitmap;
  s: string;
begin
  if AClass = nil then
    raise Exception.Create('Can not register IObject in ' + category + '. Class = nil');
  Panel := nil;
  TabSheet := GetPageOfName(category);
  if TabSheet = nil then
  begin
    Panel := TPanel.Create(Self);
    Panel.BevelOuter := bvNone;
    Panel.Left := 0;
    Panel.Top := 0;
    Panel.Align := alClient;
    TabSheet := PcIObjects.AddTabSheet;
    TabSheet.Caption := category;
    Panel.Parent := TabSheet;
    Button := CreateIObjButton(Panel);
    Button.ObjClass := nil;
    Button.Action := CmdSelTool;
    Button.Left := 2;
    Button.Top := 2;
    Button.Parent := Panel;
    ILIobjects.GetBitmap(0, Button.Glyph);
  end
  else
  begin
    Panel := TPanel(TabSheet.Controls[0]);
    n := 1;
    while n < Panel.ControlCount do
    begin
      if TIObjButton(Panel.Controls[n]).ObjClass = AClass then
        raise Exception.Create('The component a ' + AClass.ClassName +
          ' is already registered');
      Inc(n);
    end;
  end;

  Button := CreateIObjButton(Panel);
  if Panel.ControlCount = 1 then
    Button.Left := 35
  else
    Button.Left := 15 + (Panel.ControlCount - 1) * 28 + 2;
  s := TIObjectClass(AClass).GetObjectName;
  Button.Hint := s;
  Button.ObjClass := AClass;
  try
    bmp := GetDefaultGlyph(AClass.ClassName);
  except
    ILIobjects.GetBitmap(1, Button.Glyph);
  end;
  try
    if Assigned(bmp) then
      Button.Glyph.Assign(bmp)
    else
      ILIobjects.GetBitmap(1, Button.Glyph);
  finally
    bmp.Free;
  end;
  if Button.Glyph = nil then
    ILIobjects.GetBitmap(1, Button.Glyph);
end;

procedure TFrmMain.RegIObjects(category: string; Classes: array of string);
var
  i: integer;
  Reg: TIClassList;
begin
  Reg := TIClassList.GetInstance;
  for i := Low(Classes) to High(Classes) do
    RegIObject(category, Reg.FindClassByName(Classes[i]));
end;

procedure TFrmMain.FillIObjects;
begin
  RegIObjects('Design', ['TIText', 'TILine', 'TIPolyline', 'TIRect',
    'TIEllipse', 'TIPolygon', 'TIImage']);
  RegIObjects('Misc Controls', ['TITextControl', 'TIURLControl',
    'TIEditableTextControl', 'TICaptionControl', 'TITextToggleControl',
    'TIPanelControl', 'TICornerResizerControl']);
  RegIObjects('Bitmap Controls', ['TIBButtonControl', 'TIBSwitchControl',
    'TIBKnobControl', 'TIBKnobRotaterControl', 'TIBSliderControl',
    'TIBTextControl']);
  RegIObjects('SVG Controls', ['TISVGButtonControl', 'TISVGKnobControl',
    'TISVGSwitchControl', 'TISVGSliderControl']);
  RegIObjects('IV Controls', ['TIVLabelControl', 'TIVButtonControl',
    'TIVSwitchControl', 'TIVToggleControl', 'TIVTabSwitchControl',
    'TIVNumberBoxControl', 'TIVRadioButtonControl', 'TIVPanelControl',
    'TIVGroupControl', 'TIVSlideSwitchControl', 'TIVKnobControl',
    'TIVSliderControl', 'TIVRangeSliderControl', 'TIVXYPadControl',
    'TIVMultiSliderControl', 'TIVMeterControl', 'TIVKeyboardControl']);
end;

initialization
  Init;

end.
