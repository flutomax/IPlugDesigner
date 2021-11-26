{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, TypInfo, variants, uCommon;

type

  { TINameValue }

  TINameValue = class
  private
    fName: string;
    fValue: variant;
  public
    constructor Create(AName: string; AValue: variant);
    property Name: string read fName write fName;
    property Value: variant read fValue write fValue;
  end;

  TINameValueList = class
  private
    fNameValueList: TList;
    function GetCount: integer;
    function GetItems(Index: integer): TINameValue;
    function GetItemByName(Name: string): TINameValue;
    function GetValues(Index: integer): variant;
    function GetValueByName(Name: string): variant;
    procedure SetValues(Index: integer; Value: variant);
    procedure SetValueByName(Name: string; Value: variant);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Name: string; Value: variant): integer;
    function IndexOf(Name: string): integer;
    procedure Delete(Index: integer); overload;
    procedure Delete(Name: string); overload;
    procedure Clear;
    procedure Exchange(Index1, Index2: integer);
    property Count: integer read GetCount;
    property Items[Index: integer]: TINameValue read GetItems; default;
    property ItemByName[Name: string]: TINameValue read GetItemByName;
    property Values[Index: integer]: variant read GetValues write SetValues;
    property ValueByName[Name: string]: variant read GetValueByName write SetValueByName;
  end;

  TIAttributes = class(TObject)
  private
    fAttributes: TINameValueList;
  public
    constructor Create;
    destructor Destroy; override;
    property Attributes: TINameValueList read fAttributes;
  end;

  TIPropertyInfo = class(TIAttributes)
  private
    fPropName: string;
    fPropInfo: PPropInfo;
  public
    constructor Create(PropName: string; PropInfo: PPropInfo);
    property PropName: string read fPropName;
    property PropInfo: PPropInfo read fPropInfo;
  end;

  TIClassInfo = class(TIAttributes)
  private
    fPropList: TList;
    fClassType: TClass;
    function GetPropCount: integer;
    function GetPropInfo(Index: integer): TIPropertyInfo;
    function GetPropInfoByName(Name: string): TIPropertyInfo;
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;
    property PropCount: integer read GetPropCount;
    property PropInfo[Index: integer]: TIPropertyInfo read GetPropInfo;
    property PropInfoByName[Name: string]: TIPropertyInfo read GetPropInfoByName;
    property ClsType: TClass read fClassType;
  end;

  IClassesObserver = interface(IInterface)
    ['{A95D9691-7918-48C2-A7BB-0932BBA0DF75}']
    procedure ClassRegistered(ClassInfo: TIClassInfo);
    procedure ClassUnregistered(ClassInfo: TIClassInfo);
  end;

  { TIClassList }

  TIClassList = class(TISingleton)
  private
    fObservers: TList;
    fClassInfoCache: TList;
    fClassInfoList: TObjectList;
    fCacheMaxSize: integer;
    function GetClassByRef(AClass: TClass): TIClassInfo;
    function GetClass(Index: integer): TIClassInfo;
    function GetClassesCount: integer;
    procedure SetCacheMaxSize(const Value: integer);
    procedure UpdateCacheSize;
    procedure PushInCache(Item: TIClassInfo);
    function FindInList(List: TList; const AttrName: string;
      const AttrValue: variant): TIClassInfo;
  protected
    constructor Create; override;
  public
    destructor Destroy; override;
    function IndexOf(AClass: TClass): integer;
    procedure RegisterClasses(AClasses: array of TClass);
    procedure UnregisterClasses(AClasses: array of TClass);
    procedure RegisterObserver(const Observer: IClassesObserver);
    procedure UnregisterObserver(const Observer: IClassesObserver);
    function FindClassByName(const AName: string): TClass;
    function FindClassByAttr(const AttrName: string;
      const AttrValue: variant): TIClassInfo;
    function GetClassByAttr(const AttrName: string;
      const AttrValue: variant): TIClassInfo;
    property CacheMaxSize: integer read fCacheMaxSize write SetCacheMaxSize;
    property ClassByRef[AClass: TClass]: TIClassInfo read GetClassByRef;
    property Classes[Index: integer]: TIClassInfo read GetClass;
    property ClassesCount: integer read GetClassesCount;
  end;


implementation

uses
  uIObject, uStandartIControls, uVectorControls, uIDocument, uGraphics,
  uIVKeyboardControl, uSVGControls, uBitmapControls;


{ TINameValue }

constructor TINameValue.Create(AName: string; AValue: variant);
begin
  inherited Create;
  fName := AName;
  fValue := AValue;
end;


{ TINameValueList }

function TINameValueList.GetCount: integer;
begin
  Result := fNameValueList.Count;
end;

function TINameValueList.GetItems(Index: integer): TINameValue;
begin
  Result := TINameValue(fNameValueList.Items[Index]);
end;

function TINameValueList.GetItemByName(Name: string): TINameValue;
var
  Index: integer;
begin
  Result := nil;
  Index := IndexOf(Name);
  if Index > -1 then
    Result := Items[Index]
  else
    EIException.CreateResFmt(@SItemNotFound, [Name]);
end;

function TINameValueList.GetValues(Index: integer): variant;
begin
  Result := Items[Index].Value;
end;

function TINameValueList.GetValueByName(Name: string): variant;
var
  Index: integer;
begin
  Index := IndexOf(Name);
  if Index > -1 then
    Result := Values[Index]
  else
    EIException.CreateResFmt(@SItemNotFound, [Name]);
end;

procedure TINameValueList.SetValues(Index: integer; Value: variant);
begin
  Items[Index].Value := Value;
end;

procedure TINameValueList.SetValueByName(Name: string; Value: variant);
var
  Index: integer;
begin
  Index := IndexOf(Name);
  if Index > -1 then
    Values[Index] := Value
  else
    EIException.CreateResFmt(@SItemNotFound, [Name]);
end;

constructor TINameValueList.Create;
begin
  inherited Create;
  fNameValueList := TList.Create;
end;

destructor TINameValueList.Destroy;
begin
  Clear;
  fNameValueList.Free;
  inherited Destroy;
end;

procedure TINameValueList.Exchange(Index1, Index2: integer);
begin
  fNameValueList.Exchange(Index1, Index2);
end;

function TINameValueList.Add(Name: string; Value: variant): integer;
var
  NameValue: TINameValue;
begin
  Result := -1;
  if ItemByName[Name] = nil then
  begin
    NameValue := TINameValue.Create(Name, Value);
    Result := fNameValueList.Add(NameValue);
  end
  else
    EIException.CreateResFmt(@SItemAlreadyExists, [Name]);
end;

function TINameValueList.IndexOf(Name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if AnsiSameText(TINameValue(fNameValueList.Items[i]).Name, Name) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TINameValueList.Delete(Index: integer);
begin
  TObject(fNameValueList[Index]).Free;
  fNameValueList.Delete(Index);
end;

procedure TINameValueList.Delete(Name: string);
var
  Index: integer;
begin
  Index := IndexOf(Name);
  if Index > -1 then
    Delete(Index)
  else
    EIException.CreateResFmt(@SItemNotFound, [Name]);
end;

procedure TINameValueList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TINameValue(fNameValueList.Items[i]).Free;
  fNameValueList.Clear;
end;


{ TIAttributes }

constructor TIAttributes.Create;
begin
  inherited Create;
  fAttributes := TINameValueList.Create;
end;

destructor TIAttributes.Destroy;
begin
  fAttributes.Free;
  inherited Destroy;
end;

{ TIPropertyInfo }

constructor TIPropertyInfo.Create(PropName: string; PropInfo: PPropInfo);
begin
  inherited Create;
  fPropName := PropName;
  fPropInfo := PropInfo;
end;

{ TIClassInfo }

constructor TIClassInfo.Create(AClass: TClass);
var
  LClassInfo: PTypeInfo;
  LClassData: PTypeData;
  LPropList: PPropList;
  LPropInfo: TIPropertyInfo;
  i: integer;
begin
  inherited Create;
  fClassType := AClass;
  fPropList := TList.Create;
  LClassInfo := AClass.ClassInfo;
  LClassData := GetTypeData(LClassInfo);
  if Assigned(LClassData) and (LClassData^.PropCount > 0) then
  begin
    GetMem(LPropList, Sizeof(PPropInfo) * LClassData^.PropCount);
    try
      GetPropInfos(LClassInfo, LPropList);
      for i := 0 to LClassData^.PropCount - 1 do
      begin
        LPropInfo := TIPropertyInfo.Create(LPropList^[i]^.Name, LPropList^[i]);
        fPropList.Add(LPropInfo);
      end;
    finally
      FreeMem(LPropList);
    end;
  end;
end;

destructor TIClassInfo.Destroy;
var
  i: integer;
begin
  for i := 0 to fPropList.Count - 1 do
    TIPropertyInfo(fPropList.Items[i]).Free;
  fPropList.Free;
  inherited Destroy;
end;

function TIClassInfo.GetPropCount: integer;
begin
  Result := fPropList.Count;
end;

function TIClassInfo.GetPropInfo(Index: integer): TIPropertyInfo;
begin
  Result := nil;
  if (Index > -1) and (Index < fPropList.Count) then
    Result := TIPropertyInfo(fPropList.Items[Index]);
end;

function TIClassInfo.GetPropInfoByName(Name: string): TIPropertyInfo;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to PropCount - 1 do
    if AnsiSameText(PropInfo[i].PropName, Name) then
    begin
      Result := PropInfo[i];
      Break;
    end;
end;


{ TIClassList }

constructor TIClassList.Create;
begin
  inherited Create;
  fObservers := TList.Create;
  fClassInfoCache := TList.Create;
  fClassInfoList := TObjectList.Create(True);
  CacheMaxSize := 32;
end;

destructor TIClassList.Destroy;
begin
  fObservers.Free;
  fClassInfoCache.Free;
  fClassInfoList.Free;
  inherited Destroy;
end;

function TIClassList.FindClassByAttr(const AttrName: string;
  const AttrValue: variant): TIClassInfo;
begin
  Result := GetClassByAttr(AttrName, AttrValue);
  if Result = nil then
    raise EIException.Create(SClassNotFound);
end;

function TIClassList.FindInList(List: TList; const AttrName: string;
  const AttrValue: variant): TIClassInfo;
var
  i, Index: integer;
  Item: TIClassInfo;
begin
  Result := nil;
  for i := 0 to List.Count - 1 do
  begin
    Item := TIClassInfo(List[i]);
    Index := Item.Attributes.IndexOf(AttrName);
    if (Index >= 0) and (Item.Attributes.Values[Index] = AttrValue) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TIClassList.GetClass(Index: integer): TIClassInfo;
begin
  Result := TIClassInfo(fClassInfoList.Items[Index]);
end;

function TIClassList.GetClassByAttr(const AttrName: string;
  const AttrValue: variant): TIClassInfo;
begin
  Result := FindInList(fClassInfoCache, AttrName, AttrValue);
  if Result = nil then
    Result := FindInList(fClassInfoList, AttrName, AttrValue);
  if Result <> nil then
    PushInCache(Result);
end;

function TIClassList.GetClassByRef(AClass: TClass): TIClassInfo;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fClassInfoList.Count - 1 do
    if TIClassInfo(fClassInfoList.Items[i]).ClsType = AClass then
    begin
      Result := TIClassInfo(fClassInfoList.Items[i]);
      Break;
    end;
end;

function TIClassList.GetClassesCount: integer;
begin
  Result := fClassInfoList.Count;
end;

function TIClassList.IndexOf(AClass: TClass): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to fClassInfoList.Count - 1 do
    if TIClassInfo(fClassInfoList.Items[i]).ClsType = AClass then
    begin
      Result := i;
      Break;
    end;
end;

function TIClassList.FindClassByName(const AName: string): TClass;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fClassInfoList.Count - 1 do
    if TIClassInfo(fClassInfoList.Items[i]).ClsType.ClassNameIs(AName) then
    begin
      Result := TIClassInfo(fClassInfoList.Items[i]).ClsType;
      Break;
    end;
end;

procedure TIClassList.PushInCache(Item: TIClassInfo);
var
  OldIndex: integer;
begin
  if (fClassInfoCache.Count = 0) or (fClassInfoCache[0] <> Pointer(Item)) then
  begin
    OldIndex := fClassInfoCache.IndexOf(Item);
    if OldIndex < 0 then
    begin
      fClassInfoCache.Insert(0, Item);
      UpdateCacheSize;
    end
    else
      fClassInfoCache.Exchange(0, OldIndex);
  end;
end;

procedure TIClassList.RegisterClasses(AClasses: array of TClass);
var
  i, j: integer;
  Info: TIClassInfo;
begin
  for i := Low(AClasses) to High(AClasses) do
    if IndexOf(AClasses[i]) < 0 then
    begin
      Info := TIClassInfo.Create(AClasses[i]);
      fClassInfoList.Add(Info);
      for j := 0 to fObservers.Count - 1 do
        IClassesObserver(fObservers[j]).ClassRegistered(Info);
    end;
end;

procedure TIClassList.RegisterObserver(const Observer: IClassesObserver);
begin
  if fObservers.IndexOf(Pointer(Observer)) < 0 then
    fObservers.Add(Pointer(Observer));
end;

procedure TIClassList.SetCacheMaxSize(const Value: integer);
begin
  if fCacheMaxSize <> Value then
  begin
    fCacheMaxSize := Value;
    fClassInfoCache.Capacity := Value;
    UpdateCacheSize;
  end;
end;

procedure TIClassList.UnregisterClasses(AClasses: array of TClass);
var
  i, j, Index: integer;
  Info: TIClassInfo;
begin
  for i := Low(AClasses) to High(AClasses) do
  begin
    Index := IndexOf(AClasses[i]);
    if Index >= 0 then
    begin
      Info := TIClassInfo(fClassInfoList[i]);
      fClassInfoCache.Remove(Info);
      try
        for j := 0 to fObservers.Count - 1 do
          IClassesObserver(fObservers[j]).ClassUnregistered(Info);
      finally
        fClassInfoList.Delete(Index);
      end;
    end;
  end;
end;

procedure TIClassList.UnregisterObserver(const Observer: IClassesObserver);
begin
  fObservers.Remove(Pointer(Observer));
end;


procedure TIClassList.UpdateCacheSize;
begin
  if fClassInfoCache.Count > fCacheMaxSize then
    fClassInfoCache.Count := fCacheMaxSize;
end;


procedure RegisterClasses;
var
  Reg: TIClassList;
begin
  Reg := TIClassList.GetInstance;
  Reg.RegisterObserver(TIObjectsFactory.GetInstance);

  Reg.RegisterClasses([TILayer, TIPen, TIBrush, TIFont, TIGroup,
    TIRect, TIEllipse, TIText, TILine, TIPolyline, TIPolygon,
    TIImage, TITextControl, TIURLControl, TIEditableTextControl,
    TICaptionControl, TITextToggleControl, TIPanelControl, TIVLabelControl,
    TICornerResizerControl, TIVButtonControl, TIVSwitchControl, TIVToggleControl,
    TIVSlideSwitchControl, TIVTabSwitchControl, TIVRadioButtonControl,
    TIVKnobControl, TIVSliderControl, TIVRangeSliderControl, TIVGroupControl,
    TIVPanelControl, TIVNumberBoxControl, TIVXYPadControl, TIVMultiSliderControl,
    TIVMeterControl, TIVKeyboardControl, TISVGKnobControl, TISVGButtonControl,
    TISVGSwitchControl, TISVGSliderControl, TIBButtonControl, TIBKnobControl,
    TIBSwitchControl, TIBKnobRotaterControl, TIBSliderControl, TIBTextControl]);
end;

initialization
  RegisterClasses;

end.
