unit trl_upersist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, fgl, trl_irttibroker, typinfo, trl_urttibroker;

type

  { TPersistManyDataItem }

  TPersistManyDataItem = class(TInterfacedObject, IRBDataItem)
  private
    fPersistMany: IPersistMany;
    fIndex: integer;
  protected
    function GetName: string;
    function GetClassName: string;
    function GetIsObject: Boolean;
    function GetIsMemo: Boolean;
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
  public
    constructor Create(const APersistMany: IPersistMany; AIndex: integer);
  end;

 { TPersistMany }

 TPersistMany<TItem> = class(TInterfacedObject, IPersistMany, IPersistManyItems<TItem>)
  private
    fData: TFPGList<TItem>;
    function GetAsPersistDataClass: IRBData;
  protected
    function GetAsPersist(AIndex: integer): string; virtual; abstract;
    procedure SetAsPersist(AIndex: integer; AValue: string); virtual; abstract;
    function GetAsString(AIndex: integer): string; virtual; abstract;
    procedure SetAsString(AIndex: integer; AValue: string); virtual; abstract;
    function GetAsObject(AIndex: integer): TObject; virtual;
    procedure SetAsObject(AIndex: integer; AValue: TObject); virtual;
    function GetEnumName(AValue: integer): string;
    function GetEnumNameCount: integer;
    function GetIsObject: Boolean;
    function GetIsInterface: Boolean;
    function GetIsMemo: Boolean;
    function GetAsInterface(AIndex: integer): IUnknown; virtual;
    procedure SetAsInterface(AIndex: integer; AValue: IUnknown); virtual;
    function GetClassName: string; virtual;
    //
    function GetAsPersistData(AIndex: integer): IRBData; virtual;
    procedure SetAsPersistData(AIndex: integer; AValue: IRBData); virtual;
    function ItemTypeInfo: PTypeInfo;
    function GetCount: integer;
    function GetItem(AIndex: integer): TItem;
    procedure SetCount(AValue: integer);
    procedure SetItem(AIndex: integer; AValue: TItem);
    property Count: integer read GetCount write SetCount;
    property Item[AIndex: integer]: TItem read GetItem write SetItem; default;
    procedure Delete(AIndex: integer);
    property AsPersist[AIndex: integer]: string read GetAsPersist write SetAsPersist;
    property AsString[AIndex: integer]: string read GetAsString write SetAsString;
    property AsObject[AIndex: integer]: TObject read GetAsObject write SetAsObject;
    property AsInterface[AIndex: integer]: IUnknown read GetAsInterface write SetAsInterface;
    property AsPersistData[AIndex: integer]: IRBData read GetAsPersistData write SetAsPersistData;
    property AsPersistDataClass: IRBData read GetAsPersistDataClass;
    property IsObject: Boolean read GetIsObject;
    property IsInterface: Boolean read GetIsInterface;
    property IsMemo: Boolean read GetIsMemo;
    property ClassName: string read GetClassName;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TPersistManyObjects }

  TPersistManyObjects<TObjItem: TObject> = class(TPersistMany<TObjItem>)
  protected
    function GetAsObject(AIndex: integer): TObject; override;
    procedure SetAsObject(AIndex: integer; AValue: TObject); override;
    function GetAsPersistData(AIndex: integer): IRBData; override;
    procedure SetAsPersistData(AIndex: integer; AValue: IRBData); override;
  end;

  { TPersistManyIntegers }

  TPersistManyIntegers = class(TPersistMany<Integer>, IPersistManyIntegers)
  protected
    function GetAsPersist(AIndex: integer): string; override;
    function GetAsString(AIndex: integer): string; override;
    procedure SetAsPersist(AIndex: integer; AValue: string); override;
    procedure SetAsString(AIndex: integer; AValue: string); override;
  end;

  { TPersistManyStrings }

  TPersistManyStrings = class(TPersistMany<String>, IPersistManyStrings)
  protected
    function GetAsPersist(AIndex: integer): string; override;
    function GetAsString(AIndex: integer): string; override;
    procedure SetAsPersist(AIndex: integer; AValue: string); override;
    procedure SetAsString(AIndex: integer; AValue: string); override;
  end;

implementation

{ TPersistMany<TItem> }

constructor TPersistMany<TItem>.Create;
begin
  fData := TFPGList<TItem>.Create;
end;

destructor TPersistMany<TItem>.Destroy;
begin
  FreeAndNil(fData);
  inherited;
end;

function TPersistMany<TItem>.GetIsObject: Boolean;
begin
  Result := ItemTypeInfo^.Kind in [tkClass, tkObject];
end;

function TPersistMany<TItem>.GetIsInterface: Boolean;
begin
  Result := ItemTypeInfo^.Kind in [tkInterface, tkInterfaceRaw];
end;


function TPersistMany<TItem>.GetIsMemo: Boolean;
begin
  Result := SameText(ItemTypeInfo^.Name, cMemoStringType);
end;

function TPersistMany<TItem>.GetAsInterface(AIndex: integer): IUnknown;
begin
  Result := nil;
end;

function TPersistMany<TItem>.GetAsPersistData(AIndex: integer): IRBData;
begin
  Result := nil;
end;

procedure TPersistMany<TItem>.SetAsPersistData(AIndex: integer; AValue: IRBData
  );
begin
end;

function TPersistMany<TItem>.GetAsPersistDataClass: IRBData;
begin
  Result := TRBData.Create(GetTypeData(ItemTypeInfo)^.ClassType);
end;

function TPersistMany<TItem>.GetClassName: string;
begin
  Result := '';
end;

procedure TPersistMany<TItem>.SetAsInterface(AIndex: integer; AValue: IUnknown);
begin
end;

function TPersistMany<TItem>.GetAsObject(AIndex: integer): TObject;
begin
  Result := nil;
end;

procedure TPersistMany<TItem>.SetAsObject(AIndex: integer; AValue: TObject);
begin
end;

function TPersistMany<TItem>.GetEnumName(AValue: integer): string;
begin
  Result := typinfo.GetEnumName(ItemTypeInfo, AValue);
end;

function TPersistMany<TItem>.GetEnumNameCount: integer;
begin
  Result := typinfo.GetEnumNameCount(ItemTypeInfo);
end;

function TPersistMany<TItem>.ItemTypeInfo: PTypeInfo;
begin
  Result := TypeInfo(TItem);
end;

function TPersistMany<TItem>.GetCount: integer;
begin
  Result := fData.Count;
end;

function TPersistMany<TItem>.GetItem(AIndex: integer): TItem;
begin
  Result := fData[AIndex];
end;

procedure TPersistMany<TItem>.SetCount(AValue: integer);
begin
  fData.Count := AValue;
end;

procedure TPersistMany<TItem>.SetItem(AIndex: integer; AValue: TItem);
begin
  fData[AIndex] := AValue;
end;

procedure TPersistMany<TItem>.Delete(AIndex: integer);
begin
  fData.Delete(AIndex);
end;

{ TPersistManyObjects<TItem> }

function TPersistManyObjects<TObjItem>.GetAsObject(AIndex: integer): TObject;
begin
  if AIndex > Count - 1 then
    Count := AIndex + 1;
  if Item[AIndex] = nil then
    Item[AIndex]:= TObjItem.Create;
  Result := Item[AIndex];
end;

procedure TPersistManyObjects<TObjItem>.SetAsObject(AIndex: integer;
  AValue: TObject);
begin
  if Item[AIndex] <> nil then
    Item[AIndex].Free;
  Item[AIndex] := AValue as TObjItem;
end;

function TPersistManyObjects<TObjItem>.GetAsPersistData(AIndex: integer): IRBData;
begin
  Result := TRBData.Create(AsObject[AIndex]);
end;

procedure TPersistManyObjects<TObjItem>.SetAsPersistData(AIndex: integer; AValue: IRBData);
begin
  AsObject[AIndex] := AValue.UnderObject;
end;

{ TPersistManyDataItem }

function TPersistManyDataItem.GetName: string;
begin

end;

function TPersistManyDataItem.GetClassName: string;
begin
  Result := fPersistMany.ClassName;
end;

function TPersistManyDataItem.GetIsObject: Boolean;
begin
  Result := fPersistMany.IsObject;
end;

function TPersistManyDataItem.GetIsMemo: Boolean;
begin
  Result := fPersistMany.IsMemo;
end;

function TPersistManyDataItem.GetIsInterface: Boolean;
begin
  Result := fPersistMany.IsInterface;
end;

function TPersistManyDataItem.GetTypeKind: TTypeKind;
begin

end;

function TPersistManyDataItem.GetAsPersist: string;
begin
  Result := fPersistMany.AsPersist[fIndex];
end;

procedure TPersistManyDataItem.SetAsPersist(AValue: string);
begin
  fPersistMany.AsPersist[fIndex] := AValue;
end;

function TPersistManyDataItem.GetAsInteger: integer;
begin

end;

function TPersistManyDataItem.GetAsString: string;
begin
  Result := fPersistMany.AsString[fIndex];
end;

function TPersistManyDataItem.GetAsBoolean: Boolean;
begin

end;

procedure TPersistManyDataItem.SetAsInteger(AValue: integer);
begin

end;

procedure TPersistManyDataItem.SetAsString(AValue: string);
begin
  fPersistMany.AsString[fIndex] := AValue;
end;

procedure TPersistManyDataItem.SetAsBoolean(AValue: Boolean);
begin

end;

function TPersistManyDataItem.GetAsObject: TObject;
begin
  Result := fPersistMany.AsObject[fIndex];
end;

procedure TPersistManyDataItem.SetAsObject(AValue: TObject);
begin
  fPersistMany.AsObject[fIndex] := AValue;
end;

function TPersistManyDataItem.GetAsVariant: Variant;
begin

end;

procedure TPersistManyDataItem.SetAsVariant(AValue: Variant);
begin

end;

function TPersistManyDataItem.GetAsClass: TClass;
begin

end;

function TPersistManyDataItem.GetEnumName(AValue: integer): string;
begin
  Result := fPersistMany.EnumName[AValue];
end;

function TPersistManyDataItem.GetEnumNameCount: integer;
begin
  Result := fPersistMany.EnumNameCount;
end;

function TPersistManyDataItem.GetAsPtrInt: PtrInt;
begin

end;

procedure TPersistManyDataItem.SetAsPtrInt(AValue: PtrInt);
begin

end;

function TPersistManyDataItem.GetAsInterface: IUnknown;
begin
  Result := fPersistMany.AsInterface[fIndex];
end;

procedure TPersistManyDataItem.SetAsInterface(AValue: IUnknown);
begin
  fPersistMany.AsInterface[fIndex] := AValue;
end;

constructor TPersistManyDataItem.Create(const APersistMany: IPersistMany;
  AIndex: integer);
begin
  fPersistMany := APersistMany;
  fIndex := AIndex;
end;

{ TPersistManyStrings }

function TPersistManyStrings.GetAsPersist(AIndex: integer): string;
begin
  Result := Item[AIndex];
end;

function TPersistManyStrings.GetAsString(AIndex: integer): string;
begin
  Result := AsPersist[AIndex];
end;

procedure TPersistManyStrings.SetAsPersist(AIndex: integer; AValue: string);
begin
  Item[AIndex] := AValue;
end;

procedure TPersistManyStrings.SetAsString(AIndex: integer; AValue: string);
begin
  AsPersist[AIndex] := AValue;
end;

{ TPersistManyIntegers }

function TPersistManyIntegers.GetAsPersist(AIndex: integer): string;
begin
  Result := IntToStr(Item[AIndex]);
end;

function TPersistManyIntegers.GetAsString(AIndex: integer): string;
begin
  Result := AsPersist[AIndex];
end;

procedure TPersistManyIntegers.SetAsPersist(AIndex: integer; AValue: string);
begin
  if AValue = '' then
    Item[AIndex] := 0
  else
    Item[AIndex] := StrToInt(AValue);
end;

procedure TPersistManyIntegers.SetAsString(AIndex: integer; AValue: string);
begin
  AsPersist[AIndex] := AValue;
end;

end.

