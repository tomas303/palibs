unit rtti_broker_uData;

interface

uses
  rtti_broker_iBroker, TypInfo, SysUtils, Classes, StrUtils, fgl, contnrs;

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
    function FindCounter: IRBDataItem;
    function GetCounter: IRBDataItem;
  protected
    property PropInfo: PPropInfo read GetPropInfo;
    property PropName: string read GetPropName;
    procedure CheckDataKind(AKind: integer);
  protected
    // IRBDataItem
    function GetName: string;
    function GetClassName: string;
    function GetIsObject: Boolean;
    function GetIsList: Boolean;
    function GetIsListCounter: Boolean;
    function GetIsReference: Boolean;
    function GetIsNotStored: Boolean;
    function GetIsMemo: Boolean;
    function GetDataKind: integer;
    function GetTypeKind: TTypeKind;
    function GetListCount: integer;
    procedure SetListCount(ACount: integer);
    function GetAsPersist: string; virtual;
    function GetAsPersistList(AIndex: integer): string; virtual;
    procedure SetAsPersist(AValue: string); virtual;
    procedure SetAsPersistList(AIndex: integer; AValue: string); virtual;
    function GetAsString: string; virtual;
    function GetAsStringList(AIndex: integer): string; virtual;
    procedure SetAsString(AValue: string); virtual;
    procedure SetAsStringList(AIndex: integer; AValue: string); virtual;
    function GetAsInteger: integer; virtual;
    function GetAsIntegerList(AIndex: integer): integer; virtual;
    procedure SetAsInteger(AValue: integer); virtual;
    procedure SetAsIntegerList(AIndex: integer; AValue: integer); virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsBooleanList(AIndex: integer): Boolean; virtual;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsBooleanList(AIndex: integer; AValue: Boolean); virtual;
    function GetAsObject: TObject; virtual;
    function GetAsObjectList(AIndex: integer): TObject; virtual;
    procedure SetAsObject(AValue: TObject); virtual;
    procedure SetAsObjectList(AIndex: integer; AValue: TObject); virtual;
    function GetAsVariant: Variant; virtual;
    function GetAsVariantList(AIndex: integer): Variant; virtual;
    procedure SetAsVariant(AValue: Variant); virtual;
    procedure SetAsVariantList(AIndex: integer; AValue: Variant); virtual;
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
    property IsList: Boolean read GetIsList;
    property IsListCounter: Boolean read GetIsListCounter;
    property IsReference: Boolean read GetIsReference;
    property IsNotStored: Boolean read GetIsNotStored;
    property IsMemo: Boolean read GetIsMemo;
    property DataKind: integer read GetDataKind;
    property TypeKind: TTypeKind read GetTypeKind;
    property ListCount: Integer read GetListCount write SetListCount;
    property AsPersist: string read GetAsPersist write SetAsPersist;
    property AsPersistList[AIndex: integer]: string read GetAsPersistList write SetAsPersistList;
    property AsString: string read GetAsString write SetAsString;
    property AsStringList[AIndex: integer]: string read GetAsStringList write SetAsStringList;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsIntegerList[AIndex: integer]: integer read GetAsIntegerList write SetAsIntegerList;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsBooleanList[AIndex: integer]: Boolean read GetAsBooleanList write SetAsBooleanList;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsObjectList[AIndex: integer]: TObject read GetAsObjectList write SetAsObjectList;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property AsVariantList[AIndex: integer]: variant read GetAsVariantList write SetAsVariantList;
    property AsClass: TClass read GetAsClass;
    property EnumNameCount: integer read GetEnumNameCount;
    property EnumName[AValue: integer]: string read GetEnumName;
    property AsPtrInt: PtrInt read GetAsPtrInt write SetAsPtrInt;
    property AsInterface: IUnknown read GetAsInterface write SetAsInterface;
  end;

  { TRBStringDataItem }

  TRBStringDataItem = class(TRBDataItem, IRBDataText)
  protected
    function GetAsPersist: string; override;
    function GetAsPersistList(AIndex: integer): string; override;
    procedure SetAsPersist(AValue: string); override;
    procedure SetAsPersistList(AIndex: integer; AValue: string); override;
    function GetAsString: string; override;
    function GetAsStringList(AIndex: integer): string; override;
    procedure SetAsString(AValue: string); override;
    procedure SetAsStringList(AIndex: integer; AValue: string); override;
    function GetAsInteger: integer; override;
    function GetAsIntegerList(AIndex: integer): integer; override;
    procedure SetAsInteger(AValue: integer); override;
    procedure SetAsIntegerList(AIndex: integer; AValue: integer); override;
    function GetAsVariant: Variant; override;
    function GetAsVariantList(AIndex: integer): Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetAsVariantList(AIndex: integer; AValue: Variant); override;
  end;

  { TRBIntegerDataItem }

  TRBIntegerDataItem = class(TRBDataItem)
  protected
    function GetAsPersist: string; override;
    function GetAsPersistList(AIndex: integer): string; override;
    procedure SetAsPersist(AValue: string); override;
    procedure SetAsPersistList(AIndex: integer; AValue: string); override;
    function GetAsString: string; override;
    function GetAsStringList(AIndex: integer): string; override;
    procedure SetAsString(AValue: string); override;
    procedure SetAsStringList(AIndex: integer; AValue: string); override;
    function GetAsInteger: integer; override;
    function GetAsCounter: integer;
    function GetAsIntegerList(AIndex: integer): integer; override;
    procedure SetAsInteger(AValue: integer); override;
    procedure SetAsCounter(AValue: integer);
    procedure SetAsIntegerList(AIndex: integer; AValue: integer); override;
    function GetAsBoolean: Boolean; override;
    function GetAsBooleanList(AIndex: integer): Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsBooleanList(AIndex: integer; AValue: Boolean); override;
    function GetAsVariant: Variant; override;
    function GetAsVariantList(AIndex: integer): Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetAsVariantList(AIndex: integer; AValue: Variant); override;
    function GetAsPtrInt: PtrInt; override;
    procedure SetAsPtrInt(AValue: PtrInt); override;
  end;

 { TRBObjectDataItem }

  TRBObjectDataItem = class(TRBDataItem)
  protected
    function GetAsObject: TObject; override;
    function GetAsObjectList(AIndex: integer): TObject; override;
    procedure SetAsObject(AValue: TObject); override;
    procedure SetAsObjectList(AIndex: integer; AValue: TObject); override;
    function GetAsVariant: Variant; override;
    function GetAsVariantList(AIndex: integer): Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetAsVariantList(AIndex: integer; AValue: Variant); override;
    function GetAsClass: TClass; override;
  end;

  { TRBEnumerationDataItem }

  TRBEnumerationDataItem = class(TRBDataItem)
  protected
    function GetAsPersist: string; override;
    function GetAsPersistList(AIndex: integer): string; override;
    procedure SetAsPersist(AValue: string); override;
    procedure SetAsPersistList(AIndex: integer; AValue: string); override;
    function GetAsString: string; override;
    function GetAsStringList(AIndex: integer): string; override;
    procedure SetAsString(AValue: string); override;
    procedure SetAsStringList(AIndex: integer; AValue: string); override;
    function GetEnumName(AValue: integer): string; override;
    function GetEnumNameCount: integer; override;
    function GetAsVariant: Variant; override;
    function GetAsVariantList(AIndex: integer): Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetAsVariantList(AIndex: integer; AValue: Variant); override;
  end;

  { TRBVariantDataItem }

  TRBVariantDataItem = class(TRBDataItem, IRBDataText)
  protected
    function GetAsPersist: string; override;
    function GetAsPersistList(AIndex: integer): string; override;
    procedure SetAsPersist(AValue: string); override;
    procedure SetAsPersistList(AIndex: integer; AValue: string); override;
    function GetAsString: string; override;
    function GetAsStringList(AIndex: integer): string; override;
    procedure SetAsString(AValue: string); override;
    procedure SetAsStringList(AIndex: integer; AValue: string); override;
    function GetAsInteger: integer; override;
    function GetAsIntegerList(AIndex: integer): integer; override;
    procedure SetAsInteger(AValue: integer); override;
    procedure SetAsIntegerList(AIndex: integer; AValue: integer); override;
    function GetAsVariant: Variant; override;
    function GetAsVariantList(AIndex: integer): Variant; override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetAsVariantList(AIndex: integer; AValue: Variant); override;
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
    procedure AssignObject(const ASource, ATarget: TObject);
    procedure Assign(const AData: IRBData);
  public
    constructor Create(AObject: TObject; ASkipUnsupported: Boolean = False);
    constructor Create(AObject: TObject; const APropNameFilter: string);
    constructor Create(AClass: TClass; ASkipUnsupported: Boolean = False);
    destructor Destroy; override;
    property ClassName: string read GetClassName;
    property ClassType: TClass read GetClassType;
    property Count: integer read GetCount;
    property Items[AIndex: integer]: IRBDataItem read GetItems; default;
    property ItemByName[const AName: string]: IRBDataItem read GetItemByName;
    property ItemIndex[const AName: string]: integer read GetItemIndex;
    property UnderObject: TObject read GetUnderObject;
  end;

  { TRBDataList }

  TRBDataList = class(TInterfacedObject, IRBDataList)
    private type
      TInternalList = specialize TFPGInterfacedObjectList<IRBData>;
    private
      fList: TInternalList;
      function RetrieveData(const AObject: TObject): IRBData;
    protected
      function GetCount: integer;
      function GetItems(AIndex: integer): TObject;
      function GetAsData(AIndex: integer): IRBData;
      procedure Add(AObject: TObject);
      procedure Insert(ARow: integer; AObject: TObject);
      procedure AddData(AData: IRBData);
      procedure InsertData(ARow: integer; AData: IRBData);
      procedure Delete(AIndex: integer);
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
      property Items[AIndex: integer]: TObject read GetItems; default;
      property AsData[AIndex: integer]: IRBData read GetAsData;
      property Count: integer read GetCount;
    end;

implementation

{ TRBInterfaceDataItem }

function TRBInterfaceDataItem.GetAsInterface: IUnknown;
begin
  Result := GetInterfaceProp(fParent.fObject, PropInfo);
end;

procedure TRBInterfaceDataItem.SetAsInterface(AValue: IUnknown);
begin
  SetInterfaceProp(fParent.fObject, PropInfo, AValue);
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

function TRBVariantDataItem.GetAsPersistList(AIndex: integer): string;
begin
  Result := GetAsVariantList(AIndex);
end;

procedure TRBVariantDataItem.SetAsPersist(AValue: string);
begin
  SetAsVariant(AValue);
end;

procedure TRBVariantDataItem.SetAsPersistList(AIndex: integer; AValue: string);
begin
  SetAsVariantList(AIndex, AValue);
end;

function TRBVariantDataItem.GetAsString: string;
begin
  Result := GetAsVariant;
end;

function TRBVariantDataItem.GetAsStringList(AIndex: integer): string;
begin
  Result := GetAsVariantList(AIndex);
end;

procedure TRBVariantDataItem.SetAsString(AValue: string);
begin
  SetAsVariant(AValue);
end;

procedure TRBVariantDataItem.SetAsStringList(AIndex: integer; AValue: string);
begin
  SetAsVariantList(AIndex, AValue);
end;

function TRBVariantDataItem.GetAsInteger: integer;
begin
  Result := GetAsVariant;
end;

function TRBVariantDataItem.GetAsIntegerList(AIndex: integer): integer;
begin
  Result := GetAsVariantList(AIndex);
end;

procedure TRBVariantDataItem.SetAsInteger(AValue: integer);
begin
  SetAsVariant(AValue);
end;

procedure TRBVariantDataItem.SetAsIntegerList(AIndex: integer; AValue: integer);
begin
  SetAsVariantList(AIndex, AValue);
end;

function TRBVariantDataItem.GetAsVariant: Variant;
begin
  Result := GetVariantProp(fParent.fObject, PropInfo);
end;

function TRBVariantDataItem.GetAsVariantList(AIndex: integer): Variant;
type
  TGetProcIndex = function(AAccessIndex, APropIndex: integer): variant of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.GetProc;
  mMethod.Data := fParent.fObject;
  Result := TGetProcIndex(mMethod)(AIndex, PropInfo^.Index);
end;

procedure TRBVariantDataItem.SetAsVariant(AValue: Variant);
begin
  SetVariantProp(fParent.fObject, PropInfo, AValue);
end;

procedure TRBVariantDataItem.SetAsVariantList(AIndex: integer; AValue: Variant);
type
  TSetProcIndex = procedure(AAccessIndex, APropIndex: integer; const AValue: variant) of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.SetProc;
  mMethod.Data := fParent.fObject;
  TSetProcIndex(mMethod)(AIndex, PropInfo^.Index, AValue);
end;

{ TRBEnumerationDataItem }

function TRBEnumerationDataItem.GetAsPersist: string;
begin
  Result := GetAsString;
end;

function TRBEnumerationDataItem.GetAsPersistList(AIndex: integer): string;
begin
  Result := AsStringList[AIndex];
end;

procedure TRBEnumerationDataItem.SetAsPersist(AValue: string);
begin
  AsString := AValue;
end;

procedure TRBEnumerationDataItem.SetAsPersistList(AIndex: integer;
  AValue: string);
begin
  AsStringList[AIndex] := AValue;
end;

function TRBEnumerationDataItem.GetAsString: string;
begin
  CheckDataKind(crbNone);
  Result := GetEnumProp(fParent.fObject, PropInfo);
end;

function TRBEnumerationDataItem.GetAsStringList(AIndex: integer): string;
type
  TGetProcIndex = function(AAccessIndex, APropIndex: integer): byte of object;
var
  mMethod : TMethod;
  mEnumValue: byte;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.GetProc;
  mMethod.Data := fParent.fObject;
  mEnumValue := TGetProcIndex(mMethod)(AIndex, PropInfo^.Index);
  Result := typinfo.GetEnumName(PropInfo^.PropType, mEnumValue);
end;

procedure TRBEnumerationDataItem.SetAsString(AValue: string);
begin
  CheckDataKind(crbNone);
  if AValue <> '' then
    SetEnumProp(fParent.fObject, PropInfo, AValue);
end;

procedure TRBEnumerationDataItem.SetAsStringList(AIndex: integer; AValue: string
  );
type
  TSetProcIndex = procedure(AAccessIndex, APropIndex: integer; const AValue: byte) of object;
var
  mMethod : TMethod;
  mPropValue: integer;
begin
  CheckDataKind(crbList);
  if AValue = '' then
    Exit;
  mMethod.Code := PropInfo^.SetProc;
  mMethod.Data := fParent.fObject;
  //
  mPropValue := GetEnumValue(PropInfo^.PropType, AValue);
  if (mPropValue < 0) then
    ERBException.EnumerationValueNotFound(ClassName, PropName, AValue);
  TSetProcIndex(mMethod)(AIndex, PropInfo^.Index, mPropValue);
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

function TRBEnumerationDataItem.GetAsVariantList(AIndex: integer): Variant;
begin
  Result := GetAsStringList(AIndex);
end;

procedure TRBEnumerationDataItem.SetAsVariant(AValue: Variant);
begin
  SetAsString(AValue);
end;

procedure TRBEnumerationDataItem.SetAsVariantList(AIndex: integer;
  AValue: Variant);
begin
  SetAsStringList(AIndex, AValue);
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

function TRBObjectDataItem.GetAsObject: TObject;
begin
  CheckDataKind(crbObject);
  Result := GetObjectProp(fParent.fObject, PropInfo);
end;

function TRBObjectDataItem.GetAsObjectList(AIndex: integer): TObject;
type
  TGetProcIndex = function(AAccessIndex, APropIndex: integer): TObject of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.GetProc;
  mMethod.Data := fParent.fObject;
  Result := TGetProcIndex(mMethod)(AIndex, PropInfo^.Index);
end;

procedure TRBObjectDataItem.SetAsObject(AValue: TObject);
begin
  CheckDataKind(crbObject);
  SetObjectProp(fParent.fObject, PropInfo, AValue);
end;

procedure TRBObjectDataItem.SetAsObjectList(AIndex: integer; AValue: TObject);
type
  TSetProcIndex = procedure(AAccessIndex, APropIndex: integer; AValue: TObject) of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.SetProc;
  mMethod.Data := fParent.fObject;
  TSetProcIndex(mMethod)(AIndex, PropInfo^.Index, AValue);
end;

function TRBObjectDataItem.GetAsVariant: Variant;
var
  mP: Pointer;
begin
  mP := GetAsObject;
  Result := mP;
end;

function TRBObjectDataItem.GetAsVariantList(AIndex: integer): Variant;
begin
  Result := Pointer(GetAsObjectList(AIndex));
end;

procedure TRBObjectDataItem.SetAsVariant(AValue: Variant);
var
  mP: Pointer;
begin
  //mP := AValue;
  //SetAsObject(mP);
end;

procedure TRBObjectDataItem.SetAsVariantList(AIndex: integer; AValue: Variant);
begin
  //SetAsObjectList(AIndex, AValue);
end;

function TRBObjectDataItem.GetAsClass: TClass;
begin
  CheckDataKind(crbObject);
  Result := GetObjectPropClass(fParent.fClass, PropInfo^.Name);
end;

{ TIRBStringData }

function TRBStringDataItem.GetAsPersist: string;
begin
  Result := AsString;
end;

function TRBStringDataItem.GetAsPersistList(AIndex: integer): string;
begin
  Result := AsStringList[AIndex];
end;

procedure TRBStringDataItem.SetAsPersist(AValue: string);
begin
  AsString := AValue;
end;

procedure TRBStringDataItem.SetAsPersistList(AIndex: integer; AValue: string);
begin
  AsStringList[AIndex] := AValue;
end;

function TRBStringDataItem.GetAsString: string;
begin
  //CheckDataKind(crbNone);
  Result := GetStrProp(fParent.fObject, PropInfo);
end;

function TRBStringDataItem.GetAsStringList(AIndex: integer): string;
type
  TGetProcIndex = function(AAccessIndex, APropIndex: integer): string of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.GetProc;
  mMethod.Data := fParent.fObject;
  Result := TGetProcIndex(mMethod)(AIndex, PropInfo^.Index);
end;

procedure TRBStringDataItem.SetAsString(AValue: string);
begin
  //CheckDataKind(crbNone);
  SetStrProp(fParent.fObject, PropInfo, AValue);
end;

procedure TRBStringDataItem.SetAsStringList(AIndex: integer; AValue: string);
type
  TSetProcIndex = procedure(AAccessIndex, APropIndex: integer; const AValue: string) of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.SetProc;
  mMethod.Data := fParent.fObject;
  TSetProcIndex(mMethod)(AIndex, PropInfo^.Index, AValue);
end;

function TRBStringDataItem.GetAsInteger: integer;
begin
  Result := StrToInt(AsString);
end;

function TRBStringDataItem.GetAsIntegerList(AIndex: integer): integer;
begin
  Result := StrToInt(AsString[AIndex]);
end;

procedure TRBStringDataItem.SetAsInteger(AValue: integer);
begin
  AsString := IntToStr(AValue);
end;

procedure TRBStringDataItem.SetAsIntegerList(AIndex: integer; AValue: integer);
begin
  AsStringList[AIndex] := IntToStr(AValue);
end;

function TRBStringDataItem.GetAsVariant: Variant;
begin
  Result := AsString;
end;

function TRBStringDataItem.GetAsVariantList(AIndex: integer): Variant;
begin
  Result := AsStringList[AIndex];
end;

procedure TRBStringDataItem.SetAsVariant(AValue: Variant);
begin
  SetAsString(AValue);
end;

procedure TRBStringDataItem.SetAsVariantList(AIndex: integer; AValue: Variant);
begin
  SetAsStringList(AIndex, AValue);
end;

{ TIRBIntegerData }

function TRBIntegerDataItem.GetAsPersist: string;
begin
  Result := IfThen(AsInteger = 0, '', AsString);
end;

function TRBIntegerDataItem.GetAsPersistList(AIndex: integer): string;
begin
  Result := IfThen(AsIntegerList[AIndex] = 0, '', AsStringList[AIndex]);
end;

procedure TRBIntegerDataItem.SetAsPersist(AValue: string);
begin
  if AValue = '' then
    AsInteger := 0
  else
    AsString := AValue;
end;

procedure TRBIntegerDataItem.SetAsPersistList(AIndex: integer; AValue: string);
begin
  if AValue = '' then
    AsIntegerList[AIndex] := 0
  else
    AsStringList[AIndex] := AValue;
end;

function TRBIntegerDataItem.GetAsString: string;
begin
  Result := IntToStr(AsInteger);
end;

function TRBIntegerDataItem.GetAsStringList(AIndex: integer): string;
begin
  Result := IntToStr(AsIntegerList[AIndex]);
end;

procedure TRBIntegerDataItem.SetAsString(AValue: string);
begin
  AsInteger := StrToInt(AValue);
end;

procedure TRBIntegerDataItem.SetAsStringList(AIndex: integer; AValue: string);
begin
  AsIntegerList[AIndex] := StrToInt(AValue);
end;

function TRBIntegerDataItem.GetAsInteger: integer;
begin
  if IsListCounter then
    Result := GetAsCounter
  else
  begin
    CheckDataKind(crbNone);
    Result := GetOrdProp(fParent.fObject, PropInfo);
  end;
end;

function TRBIntegerDataItem.GetAsCounter: integer;
type
  TGetProcIndex = function(APropIndex: integer): integer of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbListCounter);
  mMethod.Code := PropInfo^.GetProc;
  mMethod.Data := fParent.fObject;
  Result := TGetProcIndex(mMethod)(PropInfo^.Index);
end;

function TRBIntegerDataItem.GetAsIntegerList(AIndex: integer): integer;
type
  TGetProcIndex = function(AAccessIndex, APropIndex: integer): integer of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.GetProc;
  mMethod.Data := fParent.fObject;
  Result := TGetProcIndex(mMethod)(AIndex, PropInfo^.Index);
end;

procedure TRBIntegerDataItem.SetAsInteger(AValue: integer);
begin
  if IsListCounter then
    SetAsCounter(AValue)
  else
  begin
    CheckDataKind(crbNone);
    SetOrdProp(fParent.fObject, PropInfo, AValue);
  end;
end;

procedure TRBIntegerDataItem.SetAsCounter(AValue: integer);
type
  TSetProcIndex = procedure(APropIndex: integer; AValue: integer) of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbListCounter);
  mMethod.Code := PropInfo^.SetProc;
  mMethod.Data := fParent.fObject;
  TSetProcIndex(mMethod)(PropInfo^.Index, AValue);
end;

procedure TRBIntegerDataItem.SetAsIntegerList(AIndex: integer; AValue: integer);
type
  TSetProcIndex = procedure(AAccessIndex, APropIndex: integer; AValue: integer) of object;
var
  mMethod : TMethod;
begin
  CheckDataKind(crbList);
  mMethod.Code := PropInfo^.SetProc;
  mMethod.Data := fParent.fObject;
  TSetProcIndex(mMethod)(AIndex, PropInfo^.Index, AValue);
end;

function TRBIntegerDataItem.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

function TRBIntegerDataItem.GetAsBooleanList(AIndex: integer): Boolean;
begin
  Result := AsIntegerList[AIndex] <> 0;
end;

procedure TRBIntegerDataItem.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    AsInteger := 1
  else
    AsInteger := 0;
end;

procedure TRBIntegerDataItem.SetAsBooleanList(AIndex: integer; AValue: Boolean);
begin
  if AValue then
    AsIntegerList[AIndex] := 1
  else
    AsIntegerList[AIndex] := 0;
end;

function TRBIntegerDataItem.GetAsVariant: Variant;
begin
  Result := GetAsBoolean;
end;

function TRBIntegerDataItem.GetAsVariantList(AIndex: integer): Variant;
begin
  Result := GetAsBooleanList(AIndex);
end;

procedure TRBIntegerDataItem.SetAsVariant(AValue: Variant);
begin
  SetAsBoolean(AValue);
end;

procedure TRBIntegerDataItem.SetAsVariantList(AIndex: integer; AValue: Variant);
begin
  SetAsBooleanList(AIndex, AValue);
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

function TRBDataItem.FindCounter: IRBDataItem;
var
  mCountPropName: string;
begin
  CheckDataKind(crbList);
  mCountPropName := Name + 'Count';
  Result := fParent.FindItem(mCountPropName);
end;

function TRBDataItem.GetCounter: IRBDataItem;
begin
  Result := FindCounter;
  if Result = nil then
    ERBException.Collection_NoCountPropertySpecified(Name);
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

function TRBDataItem.GetDataKind: integer;
begin
  Result := crbNone;
  if ((PropInfo^.PropProcs shr 6) and 1) <> 0 then
  begin
    Result := PropInfo^.Index;
  end;
end;

function TRBDataItem.GetTypeKind: TTypeKind;
begin
  Result := fPropInfo^.PropType^.Kind;
end;

function TRBDataItem.GetListCount: integer;
begin
  Result := GetCounter.AsInteger;
end;

procedure TRBDataItem.SetListCount(ACount: integer);
begin
  GetCounter.AsInteger := ACount;
end;

function TRBDataItem.GetAsPersist: string;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsPersistList(AIndex: integer): string;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsPersist(AValue: string);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsPersistList(AIndex: integer; AValue: string);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.CheckDataKind(AKind: integer);
begin
  //if (AKind = 0) and (DataKind <> 0) then
  //  ERBException.UnexpectedDataKind(ClassName, PropName, AKind);
  //if not ((DataKind and AKind) = AKind) then
  //  ERBException.UnexpectedDataKind(ClassName, PropName, AKind);
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
  //Result := (DataKind and crbObject) = crbObject;
  Result := TypeKind in [tkClass, tkObject];
end;

function TRBDataItem.GetIsList: Boolean;
begin
  Result := (DataKind and crbList) = crbList;
end;

function TRBDataItem.GetIsListCounter: Boolean;
begin
  Result := (DataKind and crbListCounter) = crbListCounter;
end;

function TRBDataItem.GetIsReference: Boolean;
begin
  Result := (DataKind and crbRef) = crbRef;
end;

function TRBDataItem.GetIsNotStored: Boolean;
begin
  Result := (DataKind and crbNotStored) = crbNotStored;
end;

function TRBDataItem.GetIsMemo: Boolean;
begin
  Result := (DataKind and crbMemo) = crbMemo;
end;

function TRBDataItem.GetAsString: string;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsStringList(AIndex: integer): string;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsString(AValue: string);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsStringList(AIndex: integer; AValue: string);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsInteger: integer;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsIntegerList(AIndex: integer): integer;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsInteger(AValue: integer);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsIntegerList(AIndex: integer; AValue: integer);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsBoolean: Boolean;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsBooleanList(AIndex: integer): Boolean;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsBoolean(AValue: Boolean);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsBooleanList(AIndex: integer; AValue: Boolean);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsObject: TObject;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsObjectList(AIndex: integer): TObject;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsObject(AValue: TObject);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsObjectList(AIndex: integer; AValue: TObject);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsVariant: Variant;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

function TRBDataItem.GetAsVariantList(AIndex: integer): Variant;
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsVariant(AValue: Variant);
begin
  ERBException.UnsupportedDataAccess(PropName);
end;

procedure TRBDataItem.SetAsVariantList(AIndex: integer; AValue: Variant);
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

procedure TRBData.AssignObject(const ASource, ATarget: TObject);
var
  mSource, mTarget: IRBData;
begin
  if (ASource = nil) or (ATarget = nil) then
    Exit;
  mSource := TRBData.Create(ASource);
  mTarget := TRBData.Create(ATarget);
  mTarget.Assign(mSource);
end;

procedure TRBData.Assign(const AData: IRBData);
var
  i, j: integer;
begin
  for i := 0 to Count - 1 do begin
    if Items[i].IsList then
    begin
      Items[i].ListCount := AData[i].ListCount;
      for j := 0 to Items[i].ListCount - 1 do
      begin
        if Items[i].IsObject then
        begin
          if Items[i].IsReference then
            Items[i].AsObjectList[j] := AData[i].AsObjectList[j]
          else
            AssignObject(AData[i].AsObjectList[j], Items[i].AsObjectList[j]);
        end
        else
        begin
          Items[i].AsPersistList[j] := AData[i].AsPersistList[j];
        end;
      end;
    end
    else
    if Items[i].IsObject then
    begin
      if Items[i].IsReference then
        Items[i].AsObject := AData[i].AsObject
      else
        AssignObject(AData[i].AsObject, Items[i].AsObject);
    end
    else
    begin
      Items[i].AsPersist := AData[i].AsPersist;
    end;
  end;
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

{ TRBDataList }

function TRBDataList.RetrieveData(const AObject: TObject): IRBData;
var
  m: LongInt;
begin
  if Supports(AObject, IRBData) then
    Result := AObject as IRBData
  else begin
    Result := TRBData.Create(AObject, True);
  end;
end;

function TRBDataList.GetAsData(AIndex: integer): IRBData;
begin
  Result := fList[AIndex];
end;

function TRBDataList.GetCount: integer;
begin
  Result := fList.Count;
end;

function TRBDataList.GetItems(AIndex: integer): TObject;
begin
  Result := fList[AIndex].UnderObject;
end;

procedure TRBDataList.Add(AObject: TObject);
begin
  fList.Add(RetrieveData(AObject));
end;

procedure TRBDataList.Insert(ARow: integer; AObject: TObject);
begin
  fList.Insert(ARow, RetrieveData(AObject));
end;

procedure TRBDataList.AddData(AData: IRBData);
begin
  fList.Add(AData);
end;

procedure TRBDataList.InsertData(ARow: integer; AData: IRBData);
begin
  fList.Insert(ARow, AData);
end;

procedure TRBDataList.Delete(AIndex: integer);
begin
  fList.Delete(AIndex);
end;

procedure TRBDataList.AfterConstruction;
begin
  inherited AfterConstruction;
  fList := TInternalList.Create;
end;

procedure TRBDataList.BeforeDestruction;
begin
  FreeAndNil(fList);
  inherited BeforeDestruction;
end;

end.

