{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uIDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, Graphics, Agg_LCL, uCommon, uIObject, uAGGCanvas,
  uGraphics, uVectorControls, uHistory;

type

  {$M+}
  TIDocument = class;
  {$M-}
  TILayer = class;

  { IDocumentObserver }

  IDocumentObserver = interface(IInterface)
    ['{B015767B-9A78-4E5E-8007-284306A35124}']
    procedure ObjectAdded(AObject: TIObject);
    procedure ObjectRemove(AObject: TIObject);
    procedure ObjectChanging(AObject: TIObject);
    procedure ObjectChanged(AObject: TIObject);
    procedure ObjectSelected(AObject: TIObject; Selected: boolean);
    procedure ObjectCommandReceived(AObject: TIObject; var cmd: TICmdMessage);
    procedure ObjectCommandHandled(AObject: TIObject; var cmd: TICmdMessage);
    procedure DocumentResize(NewWidth, NewHeight: double);
    procedure DocumentDestroying;
  end;

  ILayeredDocumentObserver = interface(IDocumentObserver)
    ['{981C4818-E43A-4647-8477-F0E49D15C6BC}']
    procedure LayerAdded(ALayer: TILayer);
    procedure LayerRemove(ALayer: TILayer);
    procedure LayerChanging(ALayer: TILayer);
    procedure LayerChanged(ALayer: TILayer);
    procedure ActiveLayerChanged(ALayer: TILayer);
  end;

  IRequestHandler = interface(IInterface)
    ['{E7963DD9-CCC5-490D-849C-4DF93C552983}']
    procedure RequestHandler(AObject: TIObject; var cmd: TICmdMessage);
  end;


  { TILayer }

  TILayer = class(TIBaseLayer)
  protected
    class function GetObjectName: string; override;
  public
    constructor Create(AOwner: TIObject; ADocument: TObject; AName: string); reintroduce;
    function IsBackGround: boolean;
  published
    property Name;
    property Visible;
  end;

  { TIRootLayer }

  TIRootLayer = class(TIBaseLayer)
  private
    fDocument: TIDocument;
  public
    constructor Create(ADocument: TIDocument); reintroduce;
    class function GetObjectName: string; override;
    property Document: TIDocument read fDocument;
  end;


  { TIDocument }

  TIDocument = class(TINonRefInterfacedObject, ISerializeNotify, IFPObserver)
  private
    fLoading: boolean;
    fProcessing: boolean;
    fUnitsPerInch: double;
    fWidth: double;
    fHeight: double;
    fDescription: string;
    fBgColor: TIColor;
    fFileName: string;
    fRootObject: TIRootLayer;
    fNonContentRoot: TIRootLayer;
    fSelectedList: TList;
    fObservers: TList;
    fRequestHandlers: TList;
    fModified: boolean;
    fRequestObject: TIObject;
    fActiveLayer: TILayer;
    fImageCache: TImageCache;
    fSVGCache: TImageCache;
    fStyleCache: TIVStyleCache;
    fDuplicateDelta: double;
    fHistory: THistory;
    fCanvas: TAggCanvas;
    fTempCommand: TObject;
    fOnSelectionChange: TISelectEvent;
    function GetActiveLayerIndex: integer;
    function GetRequestHandlersCount: integer;
    function GetRequestHandler(AIndex: integer): IRequestHandler;
    function GetObserver(AIndex: integer): IDocumentObserver;
    function GetObserversCount: integer;
    function GetSelected(AIndex: integer): TIObject;
    function GetSelectedCount: integer;
    function GetLayer(AIndex: integer): TILayer;
    function GetLayersCount: integer;
    function SelectedInLayer(ALayer: TILayer): boolean;
    procedure SetActiveLayer(const Value: TILayer);
    procedure SetActiveLayerIndex(AValue: integer);
    procedure SetBgColor(AValue: TIColor);
    procedure SetHeight(AValue: double);
    procedure SetUnitsPerInch(AValue: double);
    procedure SetWidth(AValue: double);
  protected
    procedure BeginSave;
    procedure EndSave;
    procedure BeginLoad;
    procedure EndLoad;
    procedure UpdateObservers;
    procedure DocumentDestroying; virtual;
    procedure ObjectAdded(Sender: TObject; AObject: TIObject); virtual;
    procedure ObjectRemove(Sender: TObject; AObject: TIObject); virtual;
    procedure ObjectChanging(Sender: TObject; AObject: TIObject); virtual;
    procedure ObjectChanged(Sender: TObject; AObject: TIObject); virtual;
    procedure ObjectSelected(Sender: TObject; AObject: TIObject;
      Selected: boolean); virtual;
    procedure ObjectCommandReceived(Sender: TObject; AObject: TIObject;
      var cmd: TICmdMessage); virtual;
    procedure ObjectCommandHandled(Sender: TObject; AObject: TIObject;
      var cmd: TICmdMessage); virtual;
    procedure ObjectRequestParams(Sender: TObject; AObject: TIObject;
      var cmd: TICmdMessage); virtual;
    procedure RQSGetUnitsPerInch(var cmd: TICmdMessage); message RQS_GETUNITSPERINCH;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    property RequestObject: TIObject read fRequestObject;
    property RequestHandlersCount: integer read GetRequestHandlersCount;
    property RequestHandlers[AIndex: integer]: IRequestHandler read GetRequestHandler;
    property ObserversCount: integer read GetObserversCount;
    property Observers[AIndex: integer]: IDocumentObserver read GetObserver;
  public
    constructor Create(); overload;
    constructor Create(aWidth, aHeight, aUnitsPerInch: double;
      aBgColor: TIColor; aDescription: string = ''); overload;
    destructor Destroy; override;
    function IndexOfLayer(ALayer: TILayer): integer;
    function GetObjectByID(const AID: QWord): TIObject;
    procedure CopySettingsFrom(AValue: TIDocument);
    procedure RegisterObserver(const AObserver: IDocumentObserver);
    procedure UnRegisterObserver(const AObserver: IDocumentObserver);
    procedure RegisterRequestHandler(const AHandler: IRequestHandler);
    procedure UnRegisterRequestHandler(const AHandler: IRequestHandler);
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure AddLayer(const AName: string);
    procedure RenameLayer(const AName: string);
    procedure DeleteLayers(AList: TObjectList);
    procedure SetObjectsOrder(AOrder: TOrder);
    procedure SetLayerOrder(AOrder: TOrder);
    procedure SelectedAll;
    procedure DeleteSelected;
    procedure DuplicateSelected;
    procedure CopyToClipboard(AList: TObjectList; const AsCut: boolean);
    procedure PasteFromClipboard(AList: TObjectList);
    procedure BeginDrag(AList: TObjectList; hit: cardinal);
    procedure EndDrag(AList: TObjectList);
    procedure UnGroupSelected;
    procedure UnGroupAllSelected;
    procedure SelectionChange;
    procedure SelectionPropertyChange(const PropertyName: string);
    procedure SelectionPropertyCommit(const Success: boolean);
    procedure Refresh;
    procedure ExportToImage(const AFileName: string);
    function GroupSelected: TIGroup;
    function CanGroupSelected: boolean;
    function CanUnGroupSelected: boolean;
    function CanObjectsOrder(AOrder: TOrder): boolean;
    function CanLayerOrder(AOrder: TOrder): boolean;
    function GetPropertiesCommand: TObject;
    function VectorControlsUsed: boolean;
    property FileName: string read fFileName;
    property NonContentRoot: TIRootLayer read fNonContentRoot;
    property SelectedCount: integer read GetSelectedCount;
    property Selected[AIndex: integer]: TIObject read GetSelected;
    property LayersCount: integer read GetLayersCount;
    property Layers[AIndex: integer]: TILayer read GetLayer;
    property ActiveLayer: TILayer read fActiveLayer write SetActiveLayer;
    property Modified: boolean read fModified;
    property History: THistory read fHistory;
    property Canvas: TAggCanvas read fCanvas write fCanvas;
    property OnSelectionChange: TISelectEvent read fOnSelectionChange write fOnSelectionChange;
  published
    // Caches will be first in property order!
    property ImageCache: TImageCache read fImageCache;
    property SVGCache: TImageCache read fSVGCache;
    property StyleCache: TIVStyleCache read fStyleCache;
    property ActiveLayerIndex: integer read GetActiveLayerIndex
      write SetActiveLayerIndex;
    property Width: double read fWidth write SetWidth;
    property Height: double read fHeight write SetHeight;
    property UnitsPerInch: double read fUnitsPerInch write SetUnitsPerInch;
    property Description: string read fDescription write fDescription;
    property BgColor: TIColor read fBgColor write SetBgColor default iclWhite;
    // RootObject will be last in property order!
    property RootObject: TIRootLayer read fRootObject;
  end;

implementation

uses
  Controls, Dialogs, Math, FPWritePNG, uStorage, uTree, uCommands, uClipboard;

function CompareZOrder(Item1, Item2: Pointer): integer;
begin
  Result := (TObject(Item1) as TIObject).GetZOrder -
    (TObject(Item2) as TIObject).GetZOrder;
end;

{ TILayer }

constructor TILayer.Create(AOwner: TIObject; ADocument: TObject; AName: string);
begin
  inherited Create(AOwner, ADocument);
  Name := AName;
end;

function TILayer.IsBackGround: boolean;
begin
  Result := Index = 0;
end;

class function TILayer.GetObjectName: string;
begin
  Result := 'Layer';
end;

{ TIRootLayer }

constructor TIRootLayer.Create(ADocument: TIDocument);
begin
  inherited Create(nil, ADocument);
  fDocument := ADocument;
end;

class function TIRootLayer.GetObjectName: string;
begin
  Result := 'Root';
end;


{ TIDocument }

constructor TIDocument.Create();
begin
  Create(600, 480, UnitsPerInch, clWhite);
end;

constructor TIDocument.Create(aWidth, aHeight, aUnitsPerInch: double;
  aBgColor: TIColor; aDescription: string);
var
  Factory: TIObjectsFactory;
begin
  inherited Create;
  fLoading := true;
  fWidth := aWidth;
  fHeight := aHeight;
  fUnitsPerInch := aUnitsPerInch;
  fBgColor := aBgColor;
  fCanvas := nil;
  fDescription := aDescription;
  fFileName := 'Untitled.ipd';
  fDuplicateDelta := 25;
  fTempCommand := nil;
  Factory := TIObjectsFactory.GetInstance;
  Factory.ResetID;
  fHistory := THistory.Create;
  fRootObject := TIRootLayer.Create(Self);
  fRootObject.OnChildAdded := @ObjectAdded;
  fRootObject.OnChildRemove := @ObjectRemove;
  fRootObject.OnChanging := @ObjectChanging;
  fRootObject.OnChanged := @ObjectChanged;
  fRootObject.OnSelected := @ObjectSelected;
  fRootObject.OnCommandReceived := @ObjectCommandReceived;
  fRootObject.OnCommandHandled := @ObjectCommandHandled;
  fRootObject.OnRequestParams := @ObjectRequestParams;
  fNonContentRoot := TIRootLayer.Create(Self);
  fNonContentRoot.OnRequestParams := @ObjectRequestParams;
  fObservers := TList.Create;
  fRequestHandlers := TList.Create;
  fSelectedList := TList.Create;
  fImageCache := TImageCache.Create(self);
  fSVGCache := TImageCache.Create(self);
  fStyleCache := TIVStyleCache.Create(self);
  fProcessing := True;
  fActiveLayer := TILayer.Create(RootObject, self, 'Layer 1');
  fProcessing := False;
  fLoading := false;
end;

destructor TIDocument.Destroy;
begin
  DocumentDestroying;
  FreeAndNil(fNonContentRoot);
  FreeAndNil(fRootObject);
  FreeAndNil(fSelectedList);
  FreeAndNil(fObservers);
  FreeAndNil(fRequestHandlers);
  FreeAndNil(fStyleCache);
  FreeAndNil(fImageCache);
  FreeAndNil(fSVGCache);
  FreeAndNil(fHistory);
  inherited Destroy;
end;

procedure TIDocument.CopySettingsFrom(AValue: TIDocument);
begin
  fWidth := AValue.Width;
  fHeight := AValue.Height;
  fUnitsPerInch := AValue.UnitsPerInch;
  fBgColor := AValue.BgColor;
  fDescription := AValue.Description;
  UpdateObservers;
end;

function TIDocument.IndexOfLayer(ALayer: TILayer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to LayersCount - 1 do
    if Layers[i] = ALayer then
    begin
      Result := i;
      Break;
    end;
end;

function TIDocument.GetObjectByID(const AID: QWord): TIObject;

  function InernalFind(root: TIObject): TIObject;
  var
    i: integer;
  begin
    Result := nil;
    if root.ID = AID then
      exit(root);
    for i := 0 to root.ChildsCount - 1 do
    begin
      Result := InernalFind(root.Childs[i]);
      if Assigned(Result) then
        break;
    end;
  end;

begin
  Result := InernalFind(fRootObject);
  if Result = nil then
    raise EIException.CreateFmt(SObjectNotFound, [AID]);
end;

procedure TIDocument.DocumentDestroying;
var
  i: integer;
begin
  for i := ObserversCount - 1 downto 0 do
    Observers[i].DocumentDestroying;
end;

procedure TIDocument.UpdateObservers;
var
  i: integer;
begin
  for i := 0 to ObserversCount - 1 do
    Observers[i].DocumentResize(Width, Height);
end;

function TIDocument.GetRequestHandlersCount: integer;
begin
  Result := fRequestHandlers.Count;
end;

function TIDocument.GetActiveLayerIndex: integer;
begin
  Result := IfThen(Assigned(fActiveLayer), fActiveLayer.Index, -1);
end;

function TIDocument.GetRequestHandler(AIndex: integer): IRequestHandler;
begin
  Result := IRequestHandler(fRequestHandlers[AIndex]);
end;

function TIDocument.GetObserver(AIndex: integer): IDocumentObserver;
begin
  Result := IDocumentObserver(fObservers[AIndex]);
end;

function TIDocument.GetObserversCount: integer;
begin
  Result := fObservers.Count;
end;

function TIDocument.GetSelected(AIndex: integer): TIObject;
begin
  Result := TIObject(fSelectedList[AIndex]);
end;

function TIDocument.GetSelectedCount: integer;
begin
  Result := fSelectedList.Count;
end;

function TIDocument.GetLayer(AIndex: integer): TILayer;
begin
  Result := RootObject.Childs[AIndex] as TILayer;
end;

function TIDocument.GetLayersCount: integer;
begin
  Result := RootObject.ChildsCount;
end;

function TIDocument.SelectedInLayer(ALayer: TILayer): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to SelectedCount - 1 do
  begin
    if Assigned(Selected[i]) and (Selected[i].GetLayer = ALayer) then
      Exit(True);
  end;
end;

procedure TIDocument.SetActiveLayer(const Value: TILayer);
var
  i: integer;
  Obs: IDocumentObserver;
  ObsEx: ILayeredDocumentObserver;
begin
  if fActiveLayer = Value then
    exit;
  fActiveLayer := Value;
  for i := 0 to ObserversCount - 1 do
  begin
    Obs := Observers[i];
    if Supports(Obs, ILayeredDocumentObserver, ObsEx) then
      ObsEx.ActiveLayerChanged(Value);
  end;
end;

procedure TIDocument.SetActiveLayerIndex(AValue: integer);
begin
  if InRange(AValue, 0, GetLayersCount - 1) then
    SetActiveLayer(GetLayer(AValue))
  else
    SetActiveLayer(nil);
end;

procedure TIDocument.SetBgColor(AValue: TIColor);
begin
  if fBgColor = AValue then
    Exit;
  fBgColor := AValue;
  fModified := True;
  UpdateObservers;
end;

procedure TIDocument.SetHeight(AValue: double);
begin
  if fHeight = AValue then
    Exit;
  fHeight := AValue;
  fModified := True;
  UpdateObservers;
end;

procedure TIDocument.SetUnitsPerInch(AValue: double);
begin
  if fUnitsPerInch = AValue then
    Exit;
  fUnitsPerInch := AValue;
  fModified := True;
  UpdateObservers;
end;

procedure TIDocument.SetWidth(AValue: double);
begin
  if fWidth = AValue then
    Exit;
  fWidth := AValue;
  fModified := True;
  UpdateObservers;
end;

procedure TIDocument.BeginSave;
begin
  // nothing
end;

procedure TIDocument.EndSave;
begin
  // nothing
end;

procedure TIDocument.BeginLoad;
begin
  ObjectChanging(Self, RootObject);
  fLoading := True;
end;

procedure TIDocument.EndLoad;
begin
  fLoading := False;
  ObjectChanged(Self, RootObject);
end;

procedure TIDocument.ObjectAdded(Sender: TObject; AObject: TIObject);

  procedure ProcessObject(AObject: TIObject);
  var
    i: integer;
  begin
    if AObject.Selected then
      fSelectedList.Add(AObject);
    for i := 0 to AObject.ChildsCount - 1 do
      ProcessObject(AObject.Childs[i]);
  end;

var
  i: integer;
  Obs: IDocumentObserver;
  ObsEx: ILayeredDocumentObserver;
begin
  if AObject.Owner = RootObject then
  begin
    if not (AObject is TILayer) then
      raise EIException.Create(STopLevelObject);
    if (not fHistory.Recovering) and (not fLoading) and (not fProcessing) then
    begin
      fHistory.Add(TLayerCreationCommand.Create(self, AObject));
    end;
    for i := 0 to ObserversCount - 1 do
    begin
      Obs := Observers[i];
      if Supports(Obs, ILayeredDocumentObserver, ObsEx) then
        ObsEx.LayerAdded(TILayer(AObject))
      else
        Obs.ObjectAdded(AObject);
    end;
  end
  else
  begin
    if (not fHistory.Recovering) and (not fLoading) and (not fProcessing) then
    begin
      fHistory.Add(TCreationCommand.Create(self, AObject));
    end;
    ProcessObject(AObject);
    if not fLoading then
    begin
      fModified := True;
      for i := 0 to ObserversCount - 1 do
        Observers[i].ObjectAdded(AObject);
    end;
  end;
end;

procedure TIDocument.ObjectRemove(Sender: TObject; AObject: TIObject);
var
  i: integer;
  Obs: IDocumentObserver;
  ObsEx: ILayeredDocumentObserver;
begin
  if AObject is TILayer then
  begin
    if SelectedInLayer(TILayer(AObject)) then
      fSelectedList.Clear;
    if AObject = ActiveLayer then
      ActiveLayer := nil;
    for i := 0 to ObserversCount - 1 do
    begin
      Obs := Observers[i];
      if Supports(Obs, ILayeredDocumentObserver, ObsEx) then
        ObsEx.LayerRemove(TILayer(AObject))
      else
        Obs.ObjectRemove(AObject);
    end;
    if not fLoading then
      fModified := True;
  end
  else
  begin
    if not fLoading then
    begin
      fModified := True;
      for i := 0 to ObserversCount - 1 do
        Observers[i].ObjectRemove(AObject);
    end;
    for i := SelectedCount - 1 downto 0 do
      if (Selected[i] = AObject) or AObject.Has(Selected[i]) then
        fSelectedList.Remove(Selected[i]);
  end;
end;

procedure TIDocument.ObjectChanging(Sender: TObject; AObject: TIObject);
var
  i: integer;
  Obs: IDocumentObserver;
  ObsEx: ILayeredDocumentObserver;
begin
  if fLoading then
    Exit;
  if AObject is TILayer then
  begin
    for i := 0 to ObserversCount - 1 do
    begin
      Obs := Observers[i];
      if Supports(Obs, ILayeredDocumentObserver, ObsEx) then
        ObsEx.LayerChanging(TILayer(AObject))
      else
        Obs.ObjectChanging(AObject);
    end;
  end
  else
  begin
    for i := 0 to ObserversCount - 1 do
      Observers[i].ObjectChanging(AObject);
  end;
end;

procedure TIDocument.ObjectChanged(Sender: TObject; AObject: TIObject);
var
  i: integer;
  Obs: IDocumentObserver;
  ObsEx: ILayeredDocumentObserver;
begin
  if fLoading then
    Exit;
  if AObject is TILayer then
  begin
    for i := 0 to ObserversCount - 1 do
    begin
      Obs := Observers[i];
      if Supports(Obs, ILayeredDocumentObserver, ObsEx) then
        ObsEx.LayerChanged(TILayer(AObject))
      else
        Obs.ObjectChanged(AObject);
    end;
  end
  else
  begin
    fModified := True;
    for i := 0 to ObserversCount - 1 do
      Observers[i].ObjectChanged(AObject);
  end;
end;

procedure TIDocument.ObjectSelected(Sender: TObject; AObject: TIObject;
  Selected: boolean);
var
  i: integer;
begin
  if Selected then
    fSelectedList.Add(AObject)
  else
    fSelectedList.Remove(AObject);
  if not fLoading then
    for i := 0 to ObserversCount - 1 do
      Observers[i].ObjectSelected(AObject, Selected);
end;

procedure TIDocument.ObjectCommandReceived(Sender: TObject; AObject: TIObject;
  var cmd: TICmdMessage);
var
  i: integer;
begin
  for i := 0 to ObserversCount - 1 do
    Observers[i].ObjectCommandReceived(AObject, cmd);
end;

procedure TIDocument.ObjectCommandHandled(Sender: TObject; AObject: TIObject;
  var cmd: TICmdMessage);
var
  i: integer;
begin
  for i := 0 to ObserversCount - 1 do
    Observers[i].ObjectCommandHandled(AObject, cmd);
end;


procedure TIDocument.ObjectRequestParams(Sender: TObject; AObject: TIObject;
  var cmd: TICmdMessage);
var
  i: integer;
  AHandler: IRequestHandler;
begin
  fRequestObject := AObject;
  try
    Dispatch(cmd);
    if cmd.Result = 0 then
      for i := 0 to RequestHandlersCount - 1 do
      begin
        AHandler := RequestHandlers[i];
        AHandler.RequestHandler(AObject, cmd);
        if cmd.Result <> 0 then
          Break;
      end;
  finally
    fRequestObject := nil;
  end;
end;

procedure TIDocument.RQSGetUnitsPerInch(var cmd: TICmdMessage);
begin
  cmd.Result := 1;
  PExtended(cmd.Param1)^ := UnitsPerInch;
end;

procedure TIDocument.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if fLoading then
    exit;
  ObjectChanged(Self, RootObject);
end;

procedure TIDocument.RegisterObserver(const AObserver: IDocumentObserver);
begin
  if fObservers.IndexOf(Pointer(AObserver)) < 0 then
    fObservers.Add(Pointer(AObserver));
end;

procedure TIDocument.UnRegisterObserver(const AObserver: IDocumentObserver);
begin
  fObservers.Remove(Pointer(AObserver));
end;

procedure TIDocument.RegisterRequestHandler(const AHandler: IRequestHandler);
begin
  if fRequestHandlers.IndexOf(Pointer(AHandler)) < 0 then
    fRequestHandlers.Add(Pointer(AHandler));
end;

procedure TIDocument.UnRegisterRequestHandler(const AHandler: IRequestHandler);
begin
  fRequestHandlers.Remove(Pointer(AHandler));
end;

procedure TIDocument.SaveToFile(const AFileName: string);
begin
  TIBinarySerializer.SaveToFile(self, AFileName);
  fFileName := AFileName;
  fModified := False;
end;

procedure TIDocument.LoadFromFile(const AFileName: string);
begin
  TIBinarySerializer.LoadFromFile(self, AFileName);
  fFileName := AFileName;
  fModified := False;
end;

procedure TIDocument.AddLayer(const AName: string);
begin
  ActiveLayer := TILayer.Create(RootObject, self, AName);
end;

procedure TIDocument.RenameLayer(const AName: string);
begin
  if (not fHistory.Recovering) then
    fHistory.Add(TLayerRenameCommand.Create(self, ActiveLayer.ID, ActiveLayer.Name, AName));
  ActiveLayer.Name := AName;
end;

procedure TIDocument.DeleteLayers(AList: TObjectList);
var
  i: integer;
  lr, slr: TILayer;
  List: TObjectList;
begin
  if MessageDlg(AppTitle, 'Do you want delete selected layer(s)?',
    mtConfirmation, mbYesNo, 0) = mrNo then
    exit;
  slr := nil;
  if SelectedCount > 0 then
  begin
    slr := Selected[0].GetLayer as TILayer;
  end;
  List := TObjectList.Create(False);
  try
    for i := AList.Count - 1 downto 0 do
    begin
      lr := TILayer(AList[i]);
      if lr.IsBackGround then
      begin
        MessageDlg(AppTitle, Format('Unable delete layer "%s". This layer is main layer.',
          [lr.Name]), mtWarning, [mbCancel], 0);
        continue;
      end;
      if lr.Locked then
      begin
        MessageDlg(AppTitle, Format('Unable delete layer "%s". Unlock this layer first.',
          [lr.Name]), mtWarning, [mbCancel], 0);
        exit;
      end;
      List.Add(lr);
    end;

    if List.Count > 0 then
    begin
      if (not fHistory.Recovering) then
        fHistory.Add(TLayerDeleteCommand.Create(self, List));

      for i := 0 to List.Count - 1 do
      begin
        lr := TILayer(List[i]);
        if Assigned(slr) and (lr = slr) then
        begin
          fSelectedList.Clear;
          slr := nil;
        end;
        lr.Free;
      end;
    end;
  finally
    List.Free;
  end;
end;

function TIDocument.CanObjectsOrder(AOrder: TOrder): boolean;
var
  i, omax, omin: integer;
begin
  result := (SelectedCount > 0) and (SelectedCount < Selected[0].Owner.ChildsCount);
  if not result then
    exit;
  omax := Selected[0].Index;
  omin := omax;
  for i := 1 to SelectedCount - 1 do
  begin
    omax := Max(omax, Selected[i].Index);
    omin := Min(omin, Selected[i].Index);
  end;
  case AOrder of
    ordBringToFront, ordBringForward: result := omax < Pred(Selected[0].Owner.ChildsCount);
    ordSendBackward, ordSendToBack: result := omin > 0;
  end;
end;

function TIDocument.CanLayerOrder(AOrder: TOrder): boolean;
begin
  result := Assigned(ActiveLayer);
  if not result then
    exit;
  case AOrder of
    ordBringToFront, ordBringForward: result := ActiveLayer.Index < Pred(fRootObject.ChildsCount);
    ordSendBackward, ordSendToBack: result := ActiveLayer.Index > 0;
  end;
end;

function TIDocument.GetPropertiesCommand: TObject;
begin
  result := TDocumentPropertiesCommand.Create(self);
end;

function TIDocument.VectorControlsUsed: boolean;

  function RecursiveProc(obj: TIObject): boolean;
  var
    i: integer;
  begin
    if Obj is TIVectorBase then
      exit(true);
    for i := 0 to obj.ChildsCount - 1 do
      if RecursiveProc(obj.Childs[i]) then
        exit(true);
    result := false;
  end;

begin
  result := RecursiveProc(RootObject);
end;

procedure TIDocument.SetObjectsOrder(AOrder: TOrder);
var
  i: integer;
  List: TObjectList;
  cmd: TOrderCommand;
begin
  fProcessing := True;
  List := TObjectList.Create(False);
  try
    for i := 0 to SelectedCount - 1 do
      List.Add(Selected[i]);
    cmd := TObjOrderCommand.Create(self, List, AOrder);
    List.Sort(@CompareZOrder);
    if AOrder in [ordBringForward, ordSendToBack] then
      for i := List.Count - 1 downto 0 do
        TIObject(List[i]).SetOrder(AOrder)
    else
      for i := 0 to List.Count - 1 do
        TIObject(List[i]).SetOrder(AOrder);
    cmd.Commit(List);
    fHistory.Add(cmd);
  finally
    List.Free;
    fProcessing := False;
  end;
end;

procedure TIDocument.SetLayerOrder(AOrder: TOrder);
var
  List: TObjectList;
  cmd: TOrderCommand;
begin
  if ActiveLayer = nil then
    exit;
  if ActiveLayer.Locked then
  begin
    MessageDlg(AppTitle, Format(
      'Unable %s layer "%s". Unlock this layer first.',
      [LowerCase(OrderCommandNames[AOrder]), ActiveLayer.Name]),
      mtWarning, [mbCancel], 0);
    exit;
  end;
  fProcessing := True;
  List := TObjectList.Create(False);
  try
    List.Add(ActiveLayer);
    cmd := TLayerOrderCommand.Create(self, List, AOrder);
    ActiveLayer.SetOrder(AOrder);
    cmd.Commit(List);
    fHistory.Add(cmd);
  finally
    List.Free;
    fProcessing := False;
  end;
end;

procedure TIDocument.SelectedAll;
var
  i: integer;
begin
  if ActiveLayer = nil then
    exit;
  for i := 0 to ActiveLayer.ChildsCount - 1 do
    ActiveLayer.Childs[i].Selected := true;
end;

procedure TIDocument.DeleteSelected;
var
  i: integer;
  Obj: TIObject;
  List: TObjectList;
begin
  List := TObjectList.Create(False);
  try
    for i := 0 to SelectedCount - 1 do
      if not Selected[i].Locked then
        List.Add(Selected[i]);
    if (not fHistory.Recovering) then
      fHistory.Add(TDeleteCommand.Create(self, List));
    for i := 0 to List.Count - 1 do
    begin
      Obj := TIObject(List[i]);
      fSelectedList.Remove(Obj);
      Obj.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TIDocument.DuplicateSelected;
var
  Obj: TIObject;
  List, DList: TObjectList;
  p: TIFloatPoint;
  i: integer;

begin
  if SelectedCount = 0 then
    exit;
  fProcessing := True;
  List := TObjectList.Create(False);
  DList := TObjectList.Create(False);
  try
    // Generate list of duplicates
    for i := 0 to SelectedCount - 1 do
      List.Add(Selected[i]);
    // Reset selection
    for i := 0 to List.Count - 1 do
      TIObject(List[i]).Selected := False;
    // Make duplicates
    for i := 0 to List.Count - 1 do
    begin
      Obj := TIObject(List[i]);
      p := Obj.GetSpaceOrigin;
      p.X += fDuplicateDelta;
      p.Y += fDuplicateDelta;
      Obj := Obj.CloneAt(Obj.Owner, p.X, p.Y);
      DList.Add(Obj);
      Obj.Selected := True;
    end;
    if (not fHistory.Recovering) then
      fHistory.Add(TDuplicateCommand.Create(self, DList));
  finally
    List.Free;
    DList.Free;
    fProcessing := False;
  end;
end;

procedure TIDocument.CopyToClipboard(AList: TObjectList; const AsCut: boolean);
begin
  Clipboard.SetObjects(AList);
  if AsCut and (not fHistory.Recovering) then
    fHistory.Add(TCutCommand.Create(self, AList));
end;

procedure TIDocument.PasteFromClipboard(AList: TObjectList);
begin
  Clipboard.GetObjects(ActiveLayer, self, AList);
end;

procedure TIDocument.BeginDrag(AList: TObjectList; hit: cardinal);
begin
  if (not fHistory.Recovering) then
    fHistory.Add(TDragCommand.Create(self, AList, hit));
end;

procedure TIDocument.EndDrag(AList: TObjectList);
begin
  if fHistory.Last is TDragCommand then
    TDragCommand(fHistory.Last).Commit(AList);
end;

procedure TIDocument.UnGroupSelected;
var
  i: integer;
  List: TObjectList;
begin
  fProcessing := True;
  List := TObjectList.Create(False);
  try
    for i := 0 to SelectedCount - 1 do
      if Selected[i] is TIGroup then
        List.Add(Selected[i]);
    if (not fHistory.Recovering) then
      fHistory.Add(TUngroupCommand.Create(self, List));
    for i := 0 to List.Count - 1 do
      TIGroup(List[i]).UnGroup;
  finally
    List.Free;
    fProcessing := False;
  end;
  SelectionChange;
end;

procedure TIDocument.UnGroupAllSelected;
var
  i: integer;
  v: boolean;
  List: TObjectList;
begin
  fProcessing := True;
  try
    List := TObjectList.Create(False);
    try
      for i := 0 to SelectedCount - 1 do
        if Selected[i] is TIGroup then
          List.Add(Selected[i]);
      if (not fHistory.Recovering) then
        fHistory.Add(TUngroupAllCommand.Create(self, List));
    finally
      List.Free;
    end;
    repeat
      for i := 0 to SelectedCount - 1 do
        if Selected[i] is TIGroup then
          TIGroup(Selected[i]).UnGroup;
      v := True;
      for i := 0 to SelectedCount - 1 do
        if Selected[i] is TIGroup then
        begin
          v := False;
          Break;
        end;
    until v;
  finally
    fProcessing := False;
  end;
  SelectionChange;
end;

procedure TIDocument.SelectionChange;
begin
  if Assigned(fOnSelectionChange) then
  begin
    if SelectedCount > 0 then
      fOnSelectionChange(self, Selected[0], true)
    else
      fOnSelectionChange(self, nil, false)
  end;
end;

procedure TIDocument.SelectionPropertyChange(const PropertyName: string);
var
  i: integer;
  List: TObjectList;
begin
  fProcessing := True;
  List := TObjectList.Create(False);
  try
    for i := 0 to SelectedCount - 1 do
      List.Add(Selected[i]);
    fTempCommand := TObjPropertyCommand.Create(self, List, PropertyName);
  finally
    List.Free;
    fProcessing := False;
  end;
end;

procedure TIDocument.SelectionPropertyCommit(const Success: boolean);
var
  i: integer;
  List: TObjectList;
begin
  if Success then
  begin
    List := TObjectList.Create(False);
    try
      for i := 0 to SelectedCount - 1 do
        List.Add(Selected[i]);
      if (not fHistory.Recovering) and Assigned(fTempCommand)
      and (fTempCommand is TObjPropertyCommand) then
      begin
        fHistory.Add(fTempCommand);
        fTempCommand := nil;
      end;
      if fHistory.Last is TObjPropertyCommand then
        TObjPropertyCommand(fHistory.Last).Commit(List);
    finally
      List.Free;
    end;
  end;
end;

procedure TIDocument.Refresh;

  procedure RecursiveProc(obj: TIObject);
  var
    i: integer;
  begin
    if Obj is TIVectorBase then
      Obj.Refresh;
    for i := 0 to obj.ChildsCount - 1 do
      RecursiveProc(obj.Childs[i]);
  end;

begin
  StyleCache.Refresh;
  RecursiveProc(RootObject);
end;

procedure TIDocument.ExportToImage(const AFileName: string);
var
  c: TICanvas;
  r: TIFloatRect;
  wr: TFPWriterPNG;
begin
  r := TIFloatRect.Create(0, 0, Width, Height);
  c := TICanvas.Create(Ceil(Width), Ceil(Height));
  try
    c.ClipRect := r;
    c.AggCanvas.AggClearAll(IColorToAggColor(BgColor));
    RootObject.Draw(c);
    wr := TFPWriterPNG.Create;
    try
      wr.WordSized := False;
      wr.UseAlpha := True;
      c.AggCanvas.Image.SaveToFile(AFileName, wr);
    finally
      wr.Free;
    end;
  finally
    c.Free;
  end;
end;

function TIDocument.GroupSelected: TIGroup;
var
  i: integer;
  List: TObjectList;
begin
 fProcessing := True;
  List := TObjectList.Create(False);
  try
    for i := 0 to SelectedCount - 1 do
      List.Add(Selected[i]);
    Result := TIGroup.Group(List, self);
    List.Clear;
    List.Add(Result);
    if (not fHistory.Recovering) then
      fHistory.Add(TGroupCommand.Create(self, List));
  finally
    List.Free;
    fProcessing := False;
  end;
  SelectionChange;
end;

function TIDocument.CanGroupSelected: boolean;
var
  i: integer;
  List: TObjectList;
begin
  Result := False;
  if SelectedCount > 1 then
  begin
    List := TObjectList.Create(False);
    try
      for i := 0 to SelectedCount - 1 do
        List.Add(Selected[i]);
      Result := TIGroup.CanGroup(List);
    finally
      List.Free;
    end;
  end;
end;

function TIDocument.CanUnGroupSelected: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to SelectedCount - 1 do
    if Selected[i] is TIGroup then
    begin
      Result := True;
      Break;
    end;
end;


end.
