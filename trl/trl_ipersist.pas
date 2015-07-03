unit trl_ipersist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker;

type

  { TPersistMany }

  TPersistMany<TItem> = class
  protected
    function GetItem(AIndex: integer): TItem; virtual; abstract;
    procedure SetItem(AIndex: integer; AValue: TItem); virtual; abstract;
    function GetCount: integer; virtual; abstract;
    procedure SetCount(AValue: integer); virtual; abstract;
  public
    property Count: integer read GetCount write SetCount;
    property Item[AIndex: integer]: TItem read GetItem write SetItem;
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

