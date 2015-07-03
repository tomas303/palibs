unit rtti_broker_iBroker;

interface

uses
  TypInfo, Classes;

const
  crbNone = 0;
  crbList = 1;
  crbListCounter = 2;
  crbObject = 4;
  crbRef = 8;
  crbNotStored = 16;
  crbMemo = 32;
  crbBlob = 64;

type

  {.$interfaces corba}

    TRBDataType = (rbdtString, rbdtInteger, rbdtClass, rbdtEnumeration);

    { IRBDataItem }

    IRBDataItem = interface
    ['{30206B79-2DC2-4A3C-AD96-70B9617EDD69}']
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
      function GetAsPersist: string;
      function GetAsPersistList(AIndex: integer): string;
      procedure SetAsPersist(AValue: string);
      procedure SetAsPersistList(AIndex: integer; AValue: string);
      function GetAsInteger: integer;
      function GetAsIntegerList(AIndex: integer): integer;
      function GetAsString: string;
      function GetAsStringList(AIndex: integer): string;
      function GetAsBoolean: Boolean;
      function GetAsBooleanList(AIndex: integer): Boolean;
      procedure SetAsInteger(AValue: integer);
      procedure SetAsIntegerList(AIndex: integer; AValue: integer);
      procedure SetAsString(AValue: string);
      procedure SetAsStringList(AIndex: integer; AValue: string);
      procedure SetAsBoolean(AValue: Boolean);
      procedure SetAsBooleanList(AIndex: integer; AValue: Boolean);
      function GetAsObject: TObject;
      function GetAsObjectList(AIndex: integer): TObject;
      procedure SetAsObject(AValue: TObject);
      procedure SetAsObjectList(AIndex: integer; AValue: TObject);
      function GetAsVariant: Variant;
      function GetAsVariantList(AIndex: integer): Variant;
      procedure SetAsVariant(AValue: Variant);
      procedure SetAsVariantList(AIndex: integer; AValue: Variant);
      function GetAsClass: TClass;
      function GetEnumName(AValue: integer): string;
      function GetEnumNameCount: integer;
      function GetAsPtrInt: PtrInt;
      procedure SetAsPtrInt(AValue: PtrInt);
      function GetAsInterface: IUnknown;
      procedure SetAsInterface(AValue: IUnknown);
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

    { IRBData }

    IRBData = interface
    ['{2B5DE8F9-F2FA-4E5A-A0F4-15C87BFB0551}']
      function GetClassName: string;
      function GetClassType: TClass;
      function GetCount: integer;
      function GetItemByName(const AName: string): IRBDataItem;
      function GetItemIndex(const AName: string): integer;
      function GetItems(AIndex: integer): IRBDataItem;
      function FindItem(const AName: string): IRBDataItem;
      function GetUnderObject: TObject;
      procedure Assign(const AData: IRBData);
      property Count: integer read GetCount;
      property ClassName: string read GetClassName;
      property ClassType: TClass read GetClassType;
      property Items[AIndex: integer]: IRBDataItem read GetItems; default;
      property ItemByName[const AName: string]: IRBDataItem read GetItemByName;
      property ItemIndex[const AName: string]: integer read GetItemIndex;
      property UnderObject: TObject read GetUnderObject;
    end;

    { IRBDataText }

    IRBDataText = interface
    ['{1D0EDB6E-3CA5-47DA-952A-A6106F9B8604}']
    end;

    { IRBDataNumber }

    IRBDataNumber = interface
    ['{5B9B3622-D44D-4692-B19D-8553B19AAE97}']
    end;

    { IRBDataOffer }

    IRBDataOffer = interface
    ['{2BF8E862-02A9-4351-BE26-97A119E0EE74}']
      function GetCount: integer;
      function GetName: string;
      property Count: integer read GetCount;
      property Name: string read GetName;
    end;

    { IRBDataList }

    IRBDataList = interface
    ['{CD6765BF-F11E-4FFE-938F-A3D41DFC5307}']
      function GetCount: integer;
      function GetItems(AIndex: integer): TObject;
      function GetAsData(AIndex: integer): IRBData;
      procedure Add(AObject: TObject);
      procedure Insert(ARow: integer; AObject: TObject);
      procedure AddData(AData: IRBData);
      procedure InsertData(ARow: integer; AData: IRBData);
      procedure Delete(AIndex: integer);
      property Items[AIndex: integer]: TObject read GetItems; default;
      property Count: integer read GetCount;
      property AsData[AIndex: integer]: IRBData read GetAsData;
    end;

    IRBDataQuery = interface
    ['{1C4AF71B-C3FB-11E3-B4AE-08002721C44F}']
      function Retrive(const AClass: string): IRBDataList;
    end;

    { IRBFactory }

    IRBFactory = interface
    ['{231BD5BD-DB2B-49DD-9A19-BB9788475F37}']
      procedure RegisterClass(const AClass: TClass);
      function CreateObject(const AClass: string): TObject;
      function FindClass(const AClass: String): TClass;
    end;

    { IRBStore }

    IRBStore = interface
    ['{90F80CF8-362E-479E-A22D-8D7586E5E66C}']
      procedure Save(AData: TObject);
      procedure Save(AData: IRBData);
      function Load(const AClass: string; const AProperty, AValue: string): TObject;
      function LoadList(const AClass: string): IRBDataList;
      procedure Flush;
      procedure Delete(AData: TObject);
      procedure Delete(AData: IRBData);
    end;

    { IRBPersistClassRegister }

    IRBPersistClassRegister = interface
    ['{DBF4A7EB-50EC-4328-B248-5BF998BFADD5}']
      procedure Add(const AClass: TPersistentClass);
      function GetCount: integer;
      function GetItem(AIndex: integer): TPersistentClass;
      property Item[AIndex: integer]: TPersistentClass read GetItem; default;
      property Count: integer read GetCount;
    end;

implementation

end.

