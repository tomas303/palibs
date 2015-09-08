unit trl_upersist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, fgl, trl_irttibroker, typinfo;

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

 TPersistMany<TItem> = class(TInterfacedObject, IPersistMany, IPersistMany<TItem>)
  private
    fData: TFPGList<TItem>;
  protected
    // IRBMany
    function GetAsPersist(AIndex: integer): string; virtual; abstract;
    procedure SetAsPersist(AIndex: integer; AValue: string); virtual; abstract;
    function GetAsString(AIndex: integer): string; virtual; abstract;
    procedure SetAsString(AIndex: integer; AValue: string); virtual; abstract;
    function GetAsIRBData(AIndex: integer): IRBData; virtual; abstract;
    procedure SetAsIRBData(AIndex: integer; AValue: IRBData); virtual; abstract;
    function GetAsObject(AIndex: integer): TObject; virtual; abstract;
    procedure SetAsObject(AIndex: integer; AValue: TObject); virtual; abstract;
    function GetEnumName(AValue: integer): string;
    function GetEnumNameCount: integer;
    //
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
    property AsIRBData[AIndex: integer]: IRBData read GetAsIRBData write SetAsIRBData;
  public
    constructor Create;
    destructor Destroy; override;
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

  { TPersistManyxxx }

  TPersistManyxxx<TItem> = class
  protected
    function GetItem(AIndex: integer): TItem; virtual; abstract;
    procedure SetItem(AIndex: integer; AValue: TItem); virtual; abstract;
    function GetCount: integer; virtual; abstract;
    procedure SetCount(AValue: integer); virtual; abstract;
  public
    property Count: integer read GetCount write SetCount;
    property Item[AIndex: integer]: TItem read GetItem write SetItem;
  end;

  { TMemoryPersistMany }

  TMemoryPersistMany<TItem> = class(TPersistManyXXX<TItem>)
  private
    fData: TFPGList<TItem>;
  protected
    function GetItem(AIndex: integer): TItem; override;
    procedure SetItem(AIndex: integer; AValue: TItem); override;
    function GetCount: integer; override;
    procedure SetCount(AValue: integer); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TMemoryPersistMany<TItem> }

function TMemoryPersistMany<TItem>.GetItem(AIndex: integer): TItem;
begin
  Result := fData[AIndex];
end;

procedure TMemoryPersistMany<TItem>.SetItem(AIndex: integer; AValue: TItem);
begin
  fData[AIndex] := AValue;
end;

function TMemoryPersistMany<TItem>.GetCount: integer;
begin
  Result := fData.Count;
end;

procedure TMemoryPersistMany<TItem>.SetCount(AValue: integer);
begin
  fData.Count := AValue;
end;

constructor TMemoryPersistMany < TItem > .Create;
begin
  fData := TFPGList<TItem>.Create;
end;

destructor TMemoryPersistMany < TItem > .Destroy;
begin
  FreeAndNil(fData);
  inherited Destroy;
end;

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

{ TPersistManyDataItem }

function TPersistManyDataItem.GetName: string;
begin

end;

function TPersistManyDataItem.GetClassName: string;
begin

end;

function TPersistManyDataItem.GetIsObject: Boolean;
begin

end;

function TPersistManyDataItem.GetIsInterface: Boolean;
begin

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

end;

procedure TPersistManyDataItem.SetAsObject(AValue: TObject);
begin

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

end;

procedure TPersistManyDataItem.SetAsInterface(AValue: IUnknown);
begin

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

