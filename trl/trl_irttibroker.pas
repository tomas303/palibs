unit trl_irttibroker;

{$mode objfpc}{$H+}

interface

uses
  TypInfo, Classes;

type

  {.$interfaces corba}

    { IRBDataItem }

    IRBDataItem = interface
    ['{30206B79-2DC2-4A3C-AD96-70B9617EDD69}']
      function GetName: string;
      function GetClassName: string;
      function GetIsObject: Boolean;
      function GetIsInterface: Boolean;
      function GetTypeKind: TTypeKind;
      function GetAsPersist: string;
      procedure SetAsPersist(AValue: string);
      function GetAsInteger: integer;
      function GetAsString: string;
      function GetAsBoolean: Boolean;
      procedure SetAsInteger(AValue: integer);
      procedure SetAsString(AValue: string);
      procedure SetAsBoolean(AValue: Boolean);
      function GetAsObject: TObject;
      procedure SetAsObject(AValue: TObject);
      function GetAsVariant: Variant;
      procedure SetAsVariant(AValue: Variant);
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
      property IsInterface: Boolean read GetIsInterface;
      property TypeKind: TTypeKind read GetTypeKind;
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

implementation

end.

