unit trl_ipersist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker;

type

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
    function GetAsPersistData(AIndex: integer): IRBData;
    function GetAsObject(AIndex: integer): TObject;
    function GetAsPersistDataClass: IRBData;
    function GetAsPersist(AIndex: integer): string;
    function GetAsString(AIndex: integer): string;
    function GetCount: integer;
    function GetEnumName(AValue: integer): string;
    function GetEnumNameCount: integer;
    function GetIsObject: Boolean;
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
    procedure Delete(AIndex: integer);
    property EnumNameCount: integer read GetEnumNameCount;
    property EnumName[AValue: integer]: string read GetEnumName;
    property IsObject: Boolean read GetIsObject;
  end;

  { IPersistMany }

  IPersistMany<TItem> = interface(IPersistMany)
  ['{A08640F4-019D-4F1B-BAC2-6894CA5E0570}']
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

implementation

end.

