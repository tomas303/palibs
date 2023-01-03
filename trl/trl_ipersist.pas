unit trl_ipersist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker;

const
  cMemoStringType = 'TMemoString';
  cIDStringType = 'TIDString';

type
  TMemoString = type string;
  TIDString = type string;

  { IPersistMoniker }

  IPersistMoniker = interface
  ['{84B31F57-E9FB-47C8-A4D5-2BB150985A9C}']
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
    property Data: IRBData read GetData write SetData;
  end;

  { IPersistMany }

  IPersistMany = interface
  ['{2C47DA7F-FEA7-4E37-965C-82176B12A721}']
    function GetAsInterface(AIndex: integer): IUnknown;
    function GetAsPersistData(AIndex: integer): IRBData;
    function GetAsObject(AIndex: integer): TObject;
    function GetAsPersistDataClass: IRBData;
    function GetAsPersist(AIndex: integer): string;
    function GetAsString(AIndex: integer): string;
    function GetClassName: string;
    function GetCount: integer;
    function GetEnumName(AValue: integer): string;
    function GetEnumNameCount: integer;
    function GetIsInterface: Boolean;
    function GetIsObject: Boolean;
    function GetIsMemo: Boolean;
    function GetIsID: Boolean;
    procedure SetAsInterface(AIndex: integer; AValue: IUnknown);
    procedure SetAsPersistData(AIndex: integer; AValue: IRBData);
    procedure SetAsObject(AIndex: integer; AValue: TObject);
    procedure SetCount(AValue: integer);
    procedure SetAsPersist(AIndex: integer; AValue: string);
    procedure SetAsString(AIndex: integer; AValue: string);
    property Count: integer read GetCount write SetCount;
    property AsPersist[AIndex: integer]: string read GetAsPersist write SetAsPersist;
    property AsString[AIndex: integer]: string read GetAsString write SetAsString;
    property AsObject[AIndex: integer]: TObject read GetAsObject write SetAsObject;
    property AsPersistData[AIndex: integer]: IRBData read GetAsPersistData write SetAsPersistData;
    property AsPersistDataClass: IRBData read GetAsPersistDataClass;
    property AsInterface[AIndex: integer]: IUnknown read GetAsInterface write SetAsInterface;
    procedure Delete(AIndex: integer);
    property EnumNameCount: integer read GetEnumNameCount;
    property EnumName[AValue: integer]: string read GetEnumName;
    property IsObject: Boolean read GetIsObject;
    property IsInterface: Boolean read GetIsInterface;
    property IsMemo: Boolean read GetIsMemo;
    property IsID: Boolean read GetIsID;
    property ClassName: string read GetClassName;
  end;

  { IPersistManyItems }

  IPersistManyItems<TItem> = interface(IPersistMany)
  ['{A08640F4-019D-4F1B-BAC2-6894CA5E0570}']
    function GetItem(AIndex: integer): TItem;
    procedure SetItem(AIndex: integer; AValue: TItem);
    property Item[AIndex: integer]: TItem read GetItem write SetItem; default;
  end;

  { IPersistManyIntegers }

  IPersistManyIntegers = interface(IPersistManyItems<integer>)
  ['{E50D2C49-26BF-4ABB-92B9-7888A63B19A5}']
  end;

  { IPersistManyStrings }

  IPersistManyStrings = interface(IPersistManyItems<string>)
  ['{E50D2C49-26BF-4ABB-92B9-7888A63B19A5}']
  end;

  { TSID }

  TSID = record
  private
    fSID: integer;
  public
    procedure Clear;
    function IsClear: Boolean;
    class operator =(a, b: TSID): Boolean;
    class operator :=(a: integer): TSID;
    class operator :=(a: widestring): TSID;
    class operator Explicit(a: TSID): string;
    class operator Implicit(a: TSID): string;
    class operator Explicit(a: TSID): widestring;
    class operator Implicit(a: TSID): widestring;
    class operator Add(const A: string; const B: TSID): string;
  end;

  { ISIDList }

  ISIDList = interface
  ['{0F1BB78E-6627-43CC-8145-65E81D67AB6C}']
    function GetCount: integer;
    function GetItems(AIndex: integer): TSID;
    procedure SetCount(AValue: integer);
    procedure SetItems(AIndex: integer; AValue: TSID);
    property Count: integer read GetCount write SetCount;
    property Items[AIndex: integer]: TSID read GetItems write SetItems; default;
  end;

  { IPersistFactory }

  IPersistFactory = interface
  ['{66F2248D-87D0-49D8-ACBF-DA19CC862A11}']
    function CreateObject(const AClass: string): IRBData;
    function Create(const AClass: string; const AID: string = ''): TObject; overload;
    function Create(AInterface: TGUID; const AID: string = ''): IUnknown; overload;
  end;

  { IPersistStore }

  IPersistStore = interface
  ['{C306CCBC-5BF5-4109-969F-FFC96D6DDEF3}']
    function New(const AClass: string): IRBData;
    procedure Load(AData: IRBData); overload;
    procedure Save(AData: IRBData);
    procedure Delete(AData: IRBData);
    function Load(const ASID: TSID): IRBData; overload;
    function GetSID(const AData: IRBData): TSID;
    property SID[const AData: IRBData]: TSID read GetSID;
    procedure Open; overload;
    procedure Open(const AFile: string); overload;
    procedure Open(const AStream: TStream); overload;
    procedure Close; overload;
    procedure Close(const AStream: TStream); overload;
    procedure Flush;
    function IsOpened: Boolean;
  end;

  { IPersistRef }

  IPersistRef = interface
  ['{9602E374-F6CE-4D2A-BB31-E2224B4BACE5}']
    function GetClassName: string;
    function GetData: IRBData;
    function GetSID: TSID;
    function GetStore: IPersistStore;
    procedure SetClassName(AValue: string);
    procedure SetData(AValue: IRBData);
    procedure SetSID(AValue: TSID);
    procedure SetStore(AValue: IPersistStore);
    property Data: IRBData read GetData write SetData;
    property Store: IPersistStore read GetStore write SetStore;
    property SID: TSID read GetSID write SetSID;
    property ClassName: string read GetClassName write SetClassName;
  end;

  IPersistRef<TItem> = interface(IPersistRef)
  ['{62132B90-58C8-4E61-AE6A-D00953BAA7BD}']
    function GetItem: TItem;
    procedure SetItem(AValue: TItem);
    property Item: TItem read GetItem write SetItem;
  end;

  { IPersistRefList }

  IPersistRefList = interface
  ['{0264926F-FC8E-4421-9001-0DD67E0E7373}']
    function GetCount: integer;
    function GetData(AIndex: integer): IRBData;
    function GetItems(AIndex: integer): IPersistRef;
    function IndexOfData(const AData: IRBData): integer;
    procedure SetCount(AValue: integer);
    procedure SetItems(AIndex: integer; AValue: IPersistRef);
    procedure Delete(AIndex: integer);
    procedure Insert(AIndex: integer; const AData: IPersistRef);
    property Count: integer read GetCount write SetCount;
    property Items[AIndex: integer]: IPersistRef read GetItems write SetItems; default;
    property Data[AIndex: integer]: IRBData read GetData;
  end;

  { IPersistQuery }

  IPersistQuery = interface
  ['{4A8B3A3E-562B-4E61-9513-8DFBC3CB7BC6}']
    function SelectClass(const AClass: string): IPersistRefList;
  end;

  IPersistStoreDevice = interface
  ['{32674407-3D99-4BF9-8BBE-99DABA186655}']

    procedure Load(const ASID: TSID; AData: IRBData);
    procedure Save(const ASID: TSID; AData: IRBData);
    procedure Delete(const ASID: TSID);

    //function GetSIDSForClass(const AClass: string): array of ASID;
    function NewSID: TSID;
    function GetSIDClass(const ASID: TSID): string;
    procedure Open; overload;
    procedure Open(const AFile: string); overload;
    procedure Open(const AStream: TStream); overload;
    procedure Close; overload;
    procedure Close(const AStream: TStream); overload;
    procedure Flush;
    function GetSIDs(const AClass: string): ISIDList;
    function IsOpened: Boolean;
  end;

  { IPersistManyRefs }

  IPersistManyRefs = interface(IPersistManyItems<IPersistRef>)
  ['{55766680-2912-4680-8B30-8908573E263C}']
    function GetFactory: IPersistFactory;
    procedure SetFactory(AValue: IPersistFactory);
    property Factory: IPersistFactory read GetFactory write SetFactory;
  end;

  IPersistManyRefs<TItem: TObject> = interface(IPersistManyRefs)
  ['{A276B6FA-D582-4071-90A0-EF2AFE861152}']
  end;

implementation

{ TSID }

procedure TSID.Clear;
begin
  fSID := -1;
end;

function TSID.IsClear: Boolean;
begin
  Result := fSID = -1;
end;

class operator TSID. = (a, b: TSID): Boolean;
begin
  Result := a.fSID = b.fSID;
end;

class operator TSID. := (a: integer): TSID;
begin
  Result.fSID := a;
end;

class operator TSID. := (a: widestring): TSID;
begin
  Result.fSID := StrToInt(a);
end;

class operator TSID.Explicit(a: TSID): string;
begin
  Result := IntToStr(a.fSID);
end;

class operator TSID.Implicit(a: TSID): string;
begin
  Result := IntToStr(a.fSID);
end;

class operator TSID.Explicit(a: TSID): widestring;
begin
  Result := IntToStr(a.fSID);
end;

class operator TSID.Implicit(a: TSID): widestring;
begin
  Result := IntToStr(a.fSID);
end;

class operator TSID.Add(const A: string; const B: TSID): string;
begin
  Result := A + string(B);
end;

end.

