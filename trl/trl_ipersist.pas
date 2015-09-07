unit trl_ipersist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker;

type

  { IRBMany }

  { IPersistMany }

  IPersistMany = interface
  ['{2C47DA7F-FEA7-4E37-965C-82176B12A721}']
    function GetAsIRBData(AIndex: integer): IRBData;
    function GetAsObject(AIndex: integer): TObject;
    function GetCount: integer;
    function GetAsPersist(AIndex: integer): string;
    function GetAsString(AIndex: integer): string;
    procedure SetAsIRBData(AIndex: integer; AValue: IRBData);
    procedure SetAsObject(AIndex: integer; AValue: TObject);
    procedure SetCount(AValue: integer);
    procedure SetAsPersist(AIndex: integer; AValue: string);
    procedure SetAsString(AIndex: integer; AValue: string);
    property Count: integer read GetCount write SetCount;
    property AsPersist[AIndex: integer]: string read GetAsPersist write SetAsPersist;
    property AsString[AIndex: integer]: string read GetAsString write SetAsString;
    property AsObject[AIndex: integer]: TObject read GetAsObject write SetAsObject;
    property AsIRBData[AIndex: integer]: IRBData read GetAsIRBData write SetAsIRBData;
    procedure Delete(AIndex: integer);
  end;

  { IPersistMany }

  IPersistMany<TItem> = interface(IPersistMany)
  ['{A08640F4-019D-4F1B-BAC2-6894CA5E0570}']
    function GetItem(AIndex: integer): TItem;
    procedure SetItem(AIndex: integer; AValue: TItem);
    property Item[AIndex: integer]: TItem read GetItem write SetItem; default;
  end;

  IPersistManyGeneric<TItem> = interface
  ['{CF48D77E-197A-4783-AF7D-181453D8539F}']
    function GetItem(AIndex: integer): TItem;
    procedure SetItem(AIndex: integer; AValue: TItem);
    property Item[AIndex: integer]: TItem read GetItem write SetItem; default;
  end;

  { IPersistManyIntegers }

  IPersistManyIntegers = interface(IPersistMany<integer>)
  ['{E50D2C49-26BF-4ABB-92B9-7888A63B19A5}']
  end;

  { IPersistManyStrings }

  IPersistManyStrings = interface(IPersistMany<string>)
  ['{E50D2C49-26BF-4ABB-92B9-7888A63B19A5}']
  end;

  { IPersistList }

  IPersistList = interface
  ['{2F41D720-0835-4A1E-92CC-DC48206ADB63}']
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

  { IPersistQuery }

  IPersistQuery = interface
  ['{4A8B3A3E-562B-4E61-9513-8DFBC3CB7BC6}']
    function Retrive(const AClass: string): IPersistList;
  end;

  { IPersistStore }

  IPersistStore = interface
  ['{3D56D7D3-955D-4872-9A1E-020645923251}']
    procedure Save(AData: TObject); overload;
    procedure Save(AData: IRBData); overload;
    function Load(const AClass: string; const AProperty, AValue: string): TObject;
    function LoadList(const AClass: string): IPersistList;
    procedure Flush;
    procedure Delete(AData: TObject); overload;
    procedure Delete(AData: IRBData); overload;
  end;

implementation

end.

