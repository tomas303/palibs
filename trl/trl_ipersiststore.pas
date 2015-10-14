unit trl_ipersiststore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker, trl_ipersist;

type

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

  { IPersistFactory }

  IPersistFactory = interface
  ['{66F2248D-87D0-49D8-ACBF-DA19CC862A11}']
    function CreateObject(const AClass: string): IRBData;
  end;

  { IPersistStore }

  IPersistStore = interface
  ['{C306CCBC-5BF5-4109-969F-FFC96D6DDEF3}']
    function New(const AClass: string): IRBData;
    procedure Load(AData: IRBData); overload;
    procedure Save(AData: IRBData);
    procedure Delete(AData: IRBData);
    function Load(const ASID: TSID): IRBData; overload;
    //function LoadList(const AClass: string): IPersistList;
    function GetSID(const AData: IRBData): TSID;
    property SID[const AData: IRBData]: TSID read GetSID;
    procedure Open;
    procedure Close;
    procedure Flush;
  end;

  { IPersistRef }

  IPersistRef = interface
  ['{9602E374-F6CE-4D2A-BB31-E2224B4BACE5}']
    function GetData: IRBData;
    function GetSID: TSID;
    function GetStore: IPersistStore;
    procedure SetData(AValue: IRBData);
    procedure SetSID(AValue: TSID);
    procedure SetStore(AValue: IPersistStore);
    property Data: IRBData read GetData write SetData;
    property Store: IPersistStore read GetStore write SetStore;
    property SID: TSID read GetSID write SetSID;
  end;

  IPersistRef<TItem> = interface(IPersistRef)
  ['{62132B90-58C8-4E61-AE6A-D00953BAA7BD}']
    function GetItem: TItem;
    procedure SetItem(AValue: TItem);
    property Item: TItem read GetItem write SetItem;
  end;

  { IPersistList }

  IPersistList = interface
  ['{0595894D-FA51-4111-9A75-B5EF6C77FBD7}']
    function GetCount: integer;
    function GetItems(AIndex: integer): IRBData;
    function GetMonikers(AIndex: integer): IPersistMoniker;
    procedure SetItems(AIndex: integer; AValue: IRBData);
    procedure SetMonikers(AIndex: integer; AValue: IPersistMoniker);
    property Items[AIndex: integer]: IRBData read GetItems write SetItems; default;
    property Monikers[AIndex: integer]: IPersistMoniker read GetMonikers write SetMonikers;
    property Count: integer read GetCount;
    procedure Delete(AIndex: Integer);
    procedure InsertData(AIndex: Integer; const AData: IRBData);
  end;

  { IPersistQuery }

  IPersistQuery = interface
  ['{4A8B3A3E-562B-4E61-9513-8DFBC3CB7BC6}']
    function Retrive(const AClass: string): IPersistList;
  end;

  IPersistStoreDevice = interface
  ['{32674407-3D99-4BF9-8BBE-99DABA186655}']

    procedure Load(const ASID: TSID; AData: IRBData);
    procedure Save(const ASID: TSID; AData: IRBData);
    procedure Delete(const ASID: TSID);

    //function GetSIDSForClass(const AClass: string): array of ASID;
    function NewSID: TSID;
    function GetSIDClass(const ASID: TSID): string;
    procedure Open;
    procedure Close;
    procedure Flush;

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

