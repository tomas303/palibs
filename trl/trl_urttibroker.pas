unit trl_urttibroker;

{$mode delphi}{$H+}

interface

uses
  trl_irttibroker, TypInfo, SysUtils, Classes, StrUtils, fgl, contnrs;

type

  { ERBException }

  ERBException = class(Exception)
  public
    class procedure UnsupportedDataAccess(const AItemName: string);
    class procedure UnknownDataKind(const AClassName, AItemName: string; const AUnknownKind: integer);
    class procedure UnexpectedDataKind(const AClassName, AItemName: string;
      const AActualKind: integer);
    class procedure Collection_NoCountPropertySpecified(const AItemName: string);
    class procedure EnumerationValueNotFound(const AClassName, AItemName, AEnum: string);
    class procedure NoClassInCreate;
    class procedure NoObjectInCreate;
    class procedure NotInterface;
  end;

  TRBData = class;

  { TRBDataItem }

  TRBDataItem = class(TContainedObject, IRBDataItem)
  private
    //fObject: TObject;
    fParent: TRBData;
    fPropInfo: PPropInfo;
    function GetPropInfo: PPropInfo;
    function GetPropName: string;
  protected
    property PropInfo: PPropInfo read GetPropInfo;
    property PropName: string read GetPropName;
  protected
    // IRBDataItem
    function GetName: string;
    function GetClassName: string;
    function GetIsObject: Boolean;
    function GetIsMemo: Boolean;
    function GetIsID: Boolean;
    function GetIsInterface: Boolean;
    function GetTypeKind: TTypeKind;
    function GetGuid: TGuid;
    function GetAsPersist: string; virtual;
    procedure SetAsPersist(AValue: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsString(AValue: string); virtual;
    function GetAsInteger: integer; virtual;
    procedure SetAsInteger(AValue: integer); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    function GetAsObject: TObject; virtual;
    procedure SetAsObject(AValue: TObject); virtual;
    function GetAsVariant: Variant; virtual;
    procedure SetAsVariant(AValue: Variant); virtual;
    function GetAsClass: TClass; virtual;
    function GetEnumName(AValue: integer): string; virtual;
    function GetEnumNameCount: integer; virtual;
    function GetAsPtrInt: PtrInt; virtual;
    procedure SetAsPtrInt(AValue: PtrInt); virtual;
    function GetAsInterface: IUnknown; virtual;
    procedure SetAsInterface(AValue: IUnknown); virtual;
  public
    constructor Create(AParent: TRBData; const APropInfo: PPropInfo);
    property Name: string read GetName;
    property ClassName: string read GetClassName;
    property IsObject: Boolean read GetIsObject;
    property IsMemo: Boolean read GetIsMemo;
    property IsID: Boolean read GetIsID;
    property IsInterface: Boolean read GetIsInterface;
    property TypeKind: TTypeKind read GetTypeKind;
    property Guid: TGuid read GetGuid;
    property AsPersist: string read GetAsPersist write SetAsPersist;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property AsClass: TClass read GetAsClass;
    property EnumNameCount: integer read GetEnumNameCount;
    property EnumName[AValue: integer]: string read GetEnumName;
    property AsPtrInt: PtrInt read GetAsPtrInt write SetAsPtrInt;
    property AsInterface: IUnknown read GetAsInterface write SetAsInterface;
  end;

  { TRBStringDataItem }

  TRBStringDataItem = class(TRBDataItem)
  protected
    function GetAsPersist: string; override;
    procedure SetAsPersist(AValue: string); override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function GetAsInteger: integer; override;
    procedure SetAsInteger(AValue: integer); override;
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
  end;

  { TRBIntegerDataItem }

  TRBIntegerDataItem = class(TRBDataItem)
  protected
    function GetAsPersist: string; override;
    procedure SetAsPersist(AValue: string); override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function GetAsInteger: integer; override;
    procedure SetAsInteger(AValue: integer); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    function GetAsPtrInt: PtrInt; override;
    procedure SetAsPtrInt(AValue: PtrInt); override;
  end;

 { TRBObjectDataItem }

  TRBObjectDataItem = class(TRBDataItem)
  protected
    function GetAsObject: TObject; override;
    procedure SetAsObject(AValue: TObject); override;
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    function GetAsClass: TClass; override;
  end;

  { TRBEnumerationDataItem }

  TRBEnumerationDataItem = class(TRBDataItem)
  protected
    function GetAsPersist: string; override;
    procedure SetAsPersist(AValue: string); override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function GetEnumName(AValue: integer): string; override;
    function GetEnumNameCount: integer; override;
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
  end;

  { TRBVariantDataItem }

  TRBVariantDataItem = class(TRBDataItem)
  protected
    function GetAsPersist: string; override;
    procedure SetAsPersist(AValue: string); override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function GetAsInteger: integer; override;
    procedure SetAsInteger(AValue: integer); override;
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
  end;

  { TRBPtrIntDataItem }

  TRBPtrIntDataItem = class(TRBDataItem)
  protected
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function GetAsPtrInt: PtrInt; override;
    procedure SetAsPtrInt(AValue: PtrInt); override;
  end;

  { TRBInterfaceDataItem }

  TRBInterfaceDataItem = class(TRBDataItem)
  protected
    function GetAsInterface: IUnknown; override;
    procedure SetAsInterface(AValue: IUnknown); override;
  end;

  { TRBDataItemList }

  TRBDataItemList = class
  private
    fList: TObjectList;
  protected
    function GetCount: LongInt;
    function GetItems(AIndex: integer): IRBDataItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ADataItem: TRBDataItem);
    property Count: integer read GetCount;
    property Items[AIndex: integer]: IRBDataItem read GetItems; default;
  end;

  { TRBData }

  TRBData = class(TInterfacedObject, IRBData)
  private
    fObject: TObject;
    fClass: TClass;
    fDataList: TRBDataItemList;
    fSkipUnsupported: Boolean;
    fPropNameFilter: string;
    function GetClassType: TClass;
    function GetData: TRBDataItemList;
    function GetItemIndex(const AName: string): integer;
    procedure SetUnderObject(AValue: TObject);
    property DataList: TRBDataItemList read GetData;
    procedure FillData;
    function CreateRBData(const APropInfo: PPropInfo): TRBDataItem;
    function SupportedRBData(const APropInfo: PPropInfo): Boolean;
  protected
    function GetClassName: string;
    function GetCount: integer;
    function GetItems(AIndex: integer): IRBDataItem;
    function GetItemByName(const AName: string): IRBDataItem;
    function FindItem(const AName: string): IRBDataItem;
    function FindItemIndex(const AName: string): integer;
    function GetUnderObject: TObject;
  public
    constructor Create(AObject: TObject; ASkipUnsupported: Boolean = False); overload;
    constructor Create(AObject: TObject; const APropNameFilter: string); overload;
    constructor Create(AClass: TClass; ASkipUnsupported: Boolean = False); overload;
    destructor Destroy; override;
    property ClassName: string read GetClassName;
    property ClassType: TClass read GetClassType;
    property Count: integer read GetCount;
    property Items[AIndex: integer]: IRBDataItem read GetItems; default;
    property ItemByName[const AName: string]: IRBDataItem read GetItemByName;
    property ItemIndex[const AName: string]: integer read GetItemIndex;
  published
    property UnderObject: TObject read GetUnderObject write SetUnderObject;
  end;

implementation

{ TRBInterfaceDataItem }

function TRBInterfaceDataItem.GetAsInterface: IUnknown;
begin
  Result := GetInterfaceProp(fParent.fObject, PropInfo);
end;

procedure TRBInterfaceDataItem.SetAsInterface(AValue: IUnknown);
var
  mIntfGuid: TGuid;
  mUnk: IUnknown;
begin
  if AValue = nil then
    SetInterfaceProp(fParent.fObject, PropInfo, AValue)
  else begin
    mIntfGuid := GetTypeData(PropInfo^.PropType)^.GUID;
    if AValue.QueryInterface(mIntfGuid, mUnk) <> 0 then
      raise ERBException.CreateFmt('Value set into interface(%s) doesn''t support target interface(%s)', [GUIDToString(AValue), GUIDToString(mIntfGuid)]);
    SetInterfaceProp(fParent.fObject, PropInfo, mUnk);
  end;
end;

{ TRBPtrIntDataItem }

function TRBPtrIntDataItem.GetAsString: string;
begin
  Result := IntToHex(GetAsPtrInt, SizeOf(PtrInt) * 2);
end;

procedure TRBPtrIntDataItem.SetAsString(AValue: string);
var
  m: Int64;
begin
  HexToBin(pchar(AValue), @m, SizeOf(m));
  SetAsPtrInt(m);
end;

function TRBPtrIntDataItem.GetAsPtrInt: PtrInt;
begin
  Result := GetOrdProp(fParent.fObject, PropInfo);
end;

procedure TRBPtrIntDataItem.SetAsPtrInt(AValue: PtrInt);
begin
  SetOrdProp(fParent.fObject, PropInfo, AValue);
end;

{ TRBVariantDataItem }

function TRBVariantDataItem.GetAsPersist: string;
begin
  Result := GetAsVariant;
end;

procedure TRBVariantDataItem.SetAsPersist(AValue: string);
begin
  SetAsVariant(AValue);
end;

function TRBVariantDataItem.GetAsString: string;
begin
  Result := GetAsVariant;
end;

procedure TRBVariantDataItem.SetAsString(AValue: string);
begin
  SetAsVariant(AValue);
end;

function TRBVariantDataItem.GetAsInteger: integer;
begin
  Result := GetAsVariant;
end;

procedure TRBVariantDataItem.SetAsInteger(AValue: integer);
begin
  SetAsVariant(AValue);
end;

function TRBVariantDataItem.GetAsVariant: Variant;
begin
  Result := GetVariantProp(fParent.fObject, PropInfo);
end;

procedure TRBVariantDataItem.SetAsVariant(AValue: Variant);
begin
  SetVariantProp(fParent.fObject, PropInfo, AValue);
end;

{ TRBEnumerationDataItem }

function TRBEnumerationDataItem.GetAsPersist: string;
begin
  Result := GetAsString;
end;

procedure TRBEnumerationDataItem.SetAsPersist(AValue: string);
begin
  AsString := AValue;
end;

function TRBEnumerationDataItem.GetAsString: string;
begin
  Result := GetEnumProp(fParent.fObject, PropInfo);
end;

procedure TRBEnumerationDataItem.SetAsString(AValue: string);
begin
  if AValue <> '' then
    SetEnumProp(fParent.fObject, PropInfo, AValue);
end;

function TRBEnumerationDataItem.GetEnumName(AValue: integer): string;
begin
  Result := typinfo.GetEnumName(PropInfo^.PropType, AValue);
end;

function TRBEnumerationDataItem.GetEnumNameCount: integer;
begin
  Result := typinfo.GetEnumNameCount(PropInfo^.PropType);
end;

function TRBEnumerationDataItem.GetAsVariant: Variant;
begin
  Result := GetAsString;
end;

procedure TRBEnumerationDataItem.SetAsVariant(AValue: Variant);
begin
  SetAsString(AValue);
end;

{ ERBException }

class procedure ERBException.UnsupportedDataAccess(const AItemName: string);
begin
  raise ERBException.Create('Unsupported access for dataitem ' + AItemName);
end;

class procedure ERBException.UnknownDataKind(const AClassName, AItemName: string; const AUnknownKind: integer);
begin
  raise ERBException.CreateFmt('Unsupported kind (declared via index property %s.%s - %d)', [AClassName, AItemName, AUnknownKind]);
end;

class procedure ERBException.UnexpectedDataKind(const AClassName,
  AItemName: string; const AActualKind: integer);
begin
  raise ERBException.CreateFmt('Unexpected kind in declaration %s.%s - check mask is %d)',
    [AClassName, AItemName, AActualKind]);
end;

class procedure ERBException.Collection_NoCountPropertySpecified(
  const AItemName: string);
begin
  raise ERBException.CreateFmt('For list property %s is not declared counter property ' +
        '(same name with Count suffix', [AItemName]);
end;

class procedure ERBException.EnumerationValueNotFound(const AClassName,
  AItemName, AEnum: string);
begin
  raise ERBException.CreateFmt('Cannot find enumeration value for %s.%s.%s ',
    [AClassName, AItemName, AEnum]);
end;

class procedure ERBException.NoClassInCreate;
begin
  raise ERBException.Create('Empty class when try create RBData');
end;

class procedure ERBException.NoObjectInCreate;
begin
  raise ERBException.Create('Empty object when try create RBData');
end;

class procedure ERBException.NotInterface;
begin
  raise ERBException.Create('Property is not of interface type');
end;

function TRBObjectDataItem.GetAsObject: TObject;
begin
  Result := GetObjectProp(fParent.fObject, PropInfo);
end;

procedure TRBObjectDataItem.SetAsObject(AValue: TObject);
begin
  SetObjectProp(fParent.fObject, PropInfo, AValue);
end;

function TRBObjectDataItem.GetAsVariant: Variant;
var
  mP: Pointer;
begin
  mP := GetAsObject;
  Result := mP;
end;

procedure TRBObjectDataItem.SetAsVariant(AValue: Variant);
var
  mP: Pointer;
begin
  //mP := AValue;
  //SetAsObject(mP);
end;

function TRBObjectDataItem.GetAsClass: TClass;
begin
  Result := GetObjectPropClass(fParent.fClass, PropInfo^.Name);
end;

{ TIRBStringData }

function TRBStringDataItem.GetAsPersist: string;
begin
  Result := AsString;
end;

procedure TRBStringDataItem.SetAsPersist(AValue: string);
begin
  AsString := AValue;
end;

function TRBStringDataItem.GetAsString: string;
begin
  Result := GetStrProp(fParent.fObject, PropInfo);
end;

procedure TRBStringDataItem.SetAsString(AValue: string);
begin
  SetStrProp(fParent.fObject, PropInfo, AValue);
end;

function TRBStringDataItem.GetAsInteger: integer;
begin
  Result := StrToInt(AsString);
end;

procedure TRBStringDataItem.SetAsInteger(AValue: integer);
begin
  AsString := IntToStr(AValue);
end;

function TRBStringDataItem.GetAsVariant: Variant;
begin
  Result := AsString;
end;

procedure TRBStringDataItem.SetAsVariant(AValue: Variant);
begin
  SetAsString(AValue);
end;

{ TIRBIntegerData }

function TRBIntegerDataItem.GetAsPersist: string;
begin
  Result := IfThen(AsInteger = 0, '', AsString);
end;

procedure TRBIntegerDataItem.SetAsPersist(AValue: string);
begin
  if AValue = '' then
    AsInteger := 0
  else
    AsString := AValue;
end;

function TRBIntegerDataItem.GetAsString: string;
begin
  Result := IntToStr(AsInteger);
end;

procedure TRBIntegerDataItem.SetAsString(AValue: string);
begin
  AsInteger := StrToInt(AValue);
end;

function TRBIntegerDataItem.GetAsInteger: integer;
begin
  Result := GetOrdProp(fParent.fObject, PropInfo);
end;

procedure TRBIntegerDataItem.SetAsInteger(AValue: integer);
begin
  SetOrdProp(fParent.fObject, PropInfo, AValue);
end;

function TRBIntegerDataItem.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

procedure TRBIntegerDataItem.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    AsInteger := 1
  else
    AsInteger := 0;
end;

function TRBIntegerDataItem.GetAsVariant: Variant;
begin
  Result := GetAsInteger;
end;

procedure TRBIntegerDataItem.SetAsVariant(AValue: Variant);
begin
  SetAsInteger(AValue);
end;

function TRBIntegerDataItem.GetAsPtrInt: PtrInt;
begin
  Result := GetOrdProp(fParent.fObject, PropInfo);
end;

procedure TRBIntegerDataItem.SetAsPtrInt(AValue: PtrInt);
begin
  SetOrdProp(fParent.fObject, PropInfo, AValue);
end;

function TRBDataItem.GetPropInfo: PPropInfo;
begin
  Result := fPropInfo;
end;

function TRBDataItem.GetAsPtrInt: PtrInt;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetEnumName(AValue: integer): string;
begin
  Result := '';
end;

function TRBDataItem.GetEnumNameCount: integer;
begin
  Result := 0;
end;

function TRBDataItem.GetPropName: string;
begin
  Result := fPropInfo^.Name;
end;

procedure TRBDataItem.SetAsPtrInt(AValue: PtrInt);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsInterface: IUnknown;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsInterface(AValue: IUnknown);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetTypeKind: TTypeKind;
begin
  Result := fPropInfo^.PropType^.Kind;
end;

function TRBDataItem.GetGuid: TGuid;
var
  mP: PInterfaceData;
begin
  if not IsInterface then
    ERBException.NotInterface;
  mP := PInterfaceData(GetTypeData(PropInfo^.PropType));
  Result := mP.GUID;
end;

function TRBDataItem.GetAsPersist: string;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsPersist(AValue: string);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetName: string;
begin
  Result := PropInfo^.Name;
end;

function TRBDataItem.GetClassName: string;
var
  mTypeData: PTypeData;
begin
  if PropInfo^.PropType^.Kind = tkClass then
  begin
    mTypeData := GetTypeData(PropInfo^.PropType);
    Result := mTypeData^.ClassType.ClassName;
  end
  else
    Result := '';
end;

function TRBDataItem.GetIsObject: Boolean;
begin
  Result := TypeKind in [tkClass, tkObject];
end;

function TRBDataItem.GetIsMemo: Boolean;
const
  cMemoStringType = 'TMemoString';
begin
  Result := SameText(cMemoStringType, fPropInfo^.PropType^.Name);
end;

function TRBDataItem.GetIsID: Boolean;
const
  cIDStringType = 'TIDString';
begin
  Result := SameText(cIDStringType, fPropInfo^.PropType^.Name);
end;

function TRBDataItem.GetIsInterface: Boolean;
begin
  Result := TypeKind in [tkInterface, tkInterfaceRaw];
end;

function TRBDataItem.GetAsString: string;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsString(AValue: string);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsInteger: integer;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsInteger(AValue: integer);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsBoolean: Boolean;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsBoolean(AValue: Boolean);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsObject: TObject;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsObject(AValue: TObject);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsVariant: Variant;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsVariant(AValue: Variant);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsClass: TClass;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

constructor TRBDataItem.Create(AParent: TRBData; const APropInfo: PPropInfo);
begin
  inherited Create(AParent);
  fParent := AParent;
  fPropInfo := APropInfo;
end;

{ TIRBDataList }

function TRBDataItemList.GetCount: LongInt;
begin
  Result := fList.Count;
end;

function TRBDataItemList.GetItems(AIndex: integer): IRBDataItem;
begin
  Result := fList[AIndex] as IRBDataItem;
end;

constructor TRBDataItemList.Create;
begin
  fList := TObjectList.Create(True);
end;

destructor TRBDataItemList.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TRBDataItemList.Add(ADataItem: TRBDataItem);
begin
  fList.Add(ADataItem);
end;

{ TRBInstance }

function TRBData.GetData: TRBDataItemList;
begin
  if fDataList = nil then
  begin
    fDataList := TRBDataItemList.Create;
    FillData;
  end;
  Result := fDataList;
end;

function TRBData.GetClassType: TClass;
begin
  if fObject <> nil then
    Result := fObject.ClassType
  else if fClass <> nil then
    Result := fCLass.ClassType
  else
    Result := nil;
end;

function TRBData.GetItemIndex(const AName: string): integer;
begin
  Result := FindItemIndex(AName);
end;

procedure TRBData.SetUnderObject(AValue: TObject);
begin
  FreeAndNil(fDataList);
  fObject := AValue;
  fClass := fObject.ClassType;
end;

procedure TRBData.FillData;
var
  mPropList: PPropList;
  mTypeData: PTypeData;
  i: integer;
begin
  mTypeData := GetTypeData(fClass.ClassInfo);
  if mTypeData^.PropCount > 0 then
  begin
    New(mPropList);
    try
      GetPropInfos(fClass.ClassInfo, mPropList);
      for i := 0 to mTypeData^.PropCount - 1 do
      begin
        if not fSkipUnsupported or SupportedRBData(mPropList^[i]) then
        begin
          if (fPropNameFilter = '') or (fPropNameFilter = mPropList^[i]^.Name) then
            DataList.Add(CreateRBData(mPropList^[i]));
        end;
      end;
    finally
      Dispose(mPropList);
    end;
  end;
end;

function TRBData.CreateRBData(const APropInfo: PPropInfo): TRBDataItem;
begin
  case APropInfo^.PropType^.Kind of
    tkAString:
      Result := TRBStringDataItem.Create(self, APropInfo);
    tkInteger, tkBool:
      Result := TRBIntegerDataItem.Create(self, APropInfo);
    tkClass:
      Result := TRBObjectDataItem.Create(self, APropInfo);
    tkEnumeration:
      Result := TRBEnumerationDataItem.Create(self, APropInfo);
    tkVariant:
      Result := TRBVariantDataItem.Create(self, APropInfo);
    tkClassRef:
      Result := TRBIntegerDataItem.Create(self, APropInfo);
    tkInterface:
      Result := TRBInterfaceDataItem.Create(self, APropInfo);
  else
    raise ERBException.Create('Unsupported data type');
  end;
end;

function TRBData.SupportedRBData(const APropInfo: PPropInfo): Boolean;
begin
  Result := APropInfo^.PropType^.Kind in [tkAString, tkInteger, tkBool, tkClass,
    tkEnumeration, tkVariant, tkClassRef, tkInterface];
end;

function TRBData.GetClassName: string;
begin
  Result := fObject.ClassName;
end;

function TRBData.GetCount: integer;
begin
  Result := DataList.Count;
end;

function TRBData.GetItems(AIndex: integer): IRBDataItem;
begin
  Result := DataList[AIndex];
end;

function TRBData.GetItemByName(const AName: string): IRBDataItem;
begin
  Result := FindItem(AName);
  if Result = nil then
    raise ERBException.Create('Dataitem ' + AName + ' not exists');
end;

function TRBData.FindItem(const AName: string): IRBDataItem;
var
  mIndex: integer;
begin
  Result := nil;
  mIndex := FindItemIndex(AName);
  if mIndex <> -1 then
  begin
    Result := Items[mIndex];
  end;
end;

function TRBData.FindItemIndex(const AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(AName, Items[i].Name) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TRBData.GetUnderObject: TObject;
begin
  Result := fObject;
end;

constructor TRBData.Create(AObject: TObject; ASkipUnsupported: Boolean = False);
begin
  if AObject = nil then
    ERBException.NoObjectInCreate;
  fObject := AObject;
  fClass := fObject.ClassType;
  fSkipUnsupported := ASkipUnsupported;
end;

constructor TRBData.Create(AObject: TObject; const APropNameFilter: string);
begin
  fPropNameFilter := APropNameFilter;
  Create(AObject);
end;

constructor TRBData.Create(AClass: TClass; ASkipUnsupported: Boolean = False);
begin
  if AClass = nil then
    ERBException.NoClassInCreate;
  fClass := AClass;
  fSkipUnsupported := ASkipUnsupported;
end;

destructor TRBData.Destroy;
begin
  FreeAndNil(fDataList);
  inherited Destroy;
end;

end.

