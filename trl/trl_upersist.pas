unit trl_upersist;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  Classes, SysUtils, trl_ipersist, fgl, trl_irttibroker, typinfo, trl_urttibroker, trl_usystem;

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
    function GetIsID: Boolean;
    function GetIsInterface: Boolean;
    function GetTypeKind: TTypeKind;
    function GetGuid: TGuid;
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
    function GetIsID: Boolean;
    function GetAsInterface(AIndex: integer): IUnknown; virtual;
    procedure SetAsInterface(AIndex: integer; AValue: IUnknown); virtual;
    function GetClassName: string; virtual;
    //
    function GetAsPersistData(AIndex: integer): IRBData; virtual;
    procedure SetAsPersistData(AIndex: integer; AValue: IRBData); virtual;
    function ItemTypeInfo: PTypeInfo;
    function GetCount: integer;
    function GetItem(AIndex: integer): TItem; virtual;
    procedure SetCount(AValue: integer);
    procedure SetItem(AIndex: integer; AValue: TItem); virtual;
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
    property IsID: Boolean read GetIsID;
    property ClassName: string read GetClassName;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TPersistManyObjects }

  TPersistManyObjects<TObjItem: TObject> = class(TPersistMany<TObjItem>)
  protected
    function GetItem(AIndex: integer): TObjItem; override;
    procedure SetItem(AIndex: integer; AValue: TObjItem); override;
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

  { TDataList_Primitives }

  TDataList_Primitives<T> = class(TInterfacedObject, IDataList, IDataList<T>)
  private type

    { TDataList }

    TDataList = class(TFPGList<TData<T>>)
    end;

  private
    fList: TDataList;
  private
    function GetData(AIndex: Integer): IRBData;
    function GetValue(AIndex: Integer): T;
    function GetCount: Integer;
    procedure Delete(APos: Integer);
    function Insert(APos: Integer): IRBData; overload;
    function Append: IRBData; overload;
    procedure Insert(APos: Integer; const AValue: IRBData); overload;
    procedure Append(const AValue: IRBData); overload;
    procedure Insert(APos: Integer; const AValue: T); overload;
    procedure Append(const AValue: T); overload;
  end;

  { TDataList_Objects }

  TDataList_Objects<T: TPlainObject> = class(TInterfacedObject, IDataList, IDataList<T>)
  private type

    { TDataList }

    TDataList = class(TFPGList<TObjectData<T>>)
    end;

  private
    fList: TDataList;
  private
    function GetData(AIndex: Integer): IRBData;
    function GetValue(AIndex: Integer): T;
    function GetCount: Integer;
    procedure Delete(APos: Integer);
    function Insert(APos: Integer): IRBData; overload;
    function Append: IRBData; overload;
    procedure Insert(APos: Integer; const AValue: IRBData); overload;
    procedure Append(const AValue: IRBData); overload;
    procedure Insert(APos: Integer; const AValue: T); overload;
    procedure Append(const AValue: T); overload;
  end;

  { TPersistDataList_Objects }

  TPersistDataList_Objects<T: TPlainObject> = class(TInterfacedObject, IDataList, IDataList<T>)
  private type

    { TDataList }

    TDataList = class(TFPGList<TPersistData<T>>)
    end;

  private
    fList: TDataList;
  private
    function GetData(AIndex: Integer): IRBData;
    function GetValue(AIndex: Integer): T;
    function GetCount: Integer;
    procedure Delete(APos: Integer);
    function Insert(APos: Integer): IRBData; overload;
    function Append: IRBData; overload;
    procedure Insert(APos: Integer; const AValue: IRBData); overload;
    procedure Append(const AValue: IRBData); overload;
    procedure Insert(APos: Integer; const AValue: T); overload;
    procedure Append(const AValue: T); overload;
  private
    procedure Add(const APeristData: TPersistData<T>);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TPersistDataListBuilder }

  TPersistDataListBuilder<T: TPlainObject> = class(TInterfacedObject, IPersistDataListBuilder, IPersistDataListBuilder<T>)
  private
    fList: TPersistDataList_Objects<T>;
  private
    procedure Add(const ASID: String; const ARBData: IRBData);
    function Build: IDataList<T>;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TPersistMany<TItem> }

procedure TPersistMany<TItem>.AfterConstruction;
begin
  inherited AfterConstruction;
  fData := TFPGList<TItem>.Create;
end;

procedure TPersistMany<TItem>.BeforeDestruction;
begin
  FreeAndNil(fData);
  inherited BeforeDestruction;
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

function TPersistMany<TItem>.GetIsID: Boolean;
begin
  Result := SameText(ItemTypeInfo^.Name, cIDStringType);
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

function TPersistManyObjects<TObjItem>.GetItem(AIndex: integer): TObjItem;
begin
  if AIndex > Count - 1 then
    Count := AIndex + 1;
  Result := inherited GetItem(AIndex);
  if not assigned(Result) then begin
    Result := TObjItem.Create;
    inherited SetItem(AIndex, Result);
  end;
end;

procedure TPersistManyObjects<TObjItem>.SetItem(AIndex: integer; AValue: TObjItem);
begin
  Item[AIndex].Free;
  Item[AIndex] := AValue as TObjItem;
end;

function TPersistManyObjects<TObjItem>.GetAsObject(AIndex: integer): TObject;
begin
  Result := Item[AIndex];
end;

procedure TPersistManyObjects<TObjItem>.SetAsObject(AIndex: integer;
  AValue: TObject);
begin
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

{ TDataList_Primitives }

function TDataList_Primitives<T>.GetData(AIndex: Integer): IRBData;
begin
// later some special implementation ... maybe generic based on T type
  Result := nil;
end;

function TDataList_Primitives<T>.GetValue(AIndex: Integer): T;
begin
  Result := fList[AIndex].Value;
end;

function TDataList_Primitives<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TDataList_Primitives<T>.Delete(APos: Integer);
begin
  fList.Delete(APos);
end;

function TDataList_Primitives<T>.Insert(APos: Integer): IRBData;
begin
  Result := nil;
  fList.Insert(APos, TData<T>.Create(T(Result[0].AsVariant)));
end;

function TDataList_Primitives<T>.Append: IRBData;
begin
  Result := nil;
  fList.Add(TData<T>.Create(T(Result[0].AsVariant)));
end;

procedure TDataList_Primitives<T>.Insert(APos: Integer; const AValue: IRBData);
begin
  fList.Insert(APos, TData<T>.Create(T(AValue[0].AsVariant)));
end;

procedure TDataList_Primitives<T>.Append(const AValue: IRBData);
begin
  fList.Add(TData<T>.Create(T(AValue[0].AsVariant)));
end;

procedure TDataList_Primitives<T>.Insert(APos: Integer; const AValue: T);
begin
  fList.Insert(APos, TData<T>.Create(AValue));
end;

procedure TDataList_Primitives<T>.Append(const AValue: T);
begin
  fList.Add(TData<T>.Create(AValue));
end;

{ TDataList_Objects }

function TDataList_Objects<T>.GetData(AIndex: Integer): IRBData;
begin
  Result := fList[AIndex].RB;
end;

function TDataList_Objects<T>.GetValue(AIndex: Integer): T;
begin
  Result := fList[AIndex].Value;
end;

function TDataList_Objects<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TDataList_Objects<T>.Delete(APos: Integer);
begin
  fList.Delete(APos);
end;

function TDataList_Objects<T>.Insert(APos: Integer): IRBData;
begin
  Result := TRBData.Create(T.Create);
  fList.Insert(APos, TObjectData<T>.Create(Result.UnderObject as T));
end;

function TDataList_Objects<T>.Append: IRBData;
begin
  Result := TRBData.Create(T.Create);
  fList.Add(TObjectData<T>.Create(Result.UnderObject as T));
end;

procedure TDataList_Objects<T>.Insert(APos: Integer; const AValue: IRBData);
begin
  fList.Insert(APos, TObjectData<T>.Create(AValue.UnderObject as T));
end;

procedure TDataList_Objects<T>.Append(const AValue: IRBData);
begin
  fList.Add(TObjectData<T>.Create(AValue.UnderObject as T));
end;

procedure TDataList_Objects<T>.Insert(APos: Integer; const AValue: T);
begin
  fList.Insert(APos, TObjectData<T>.Create(AValue));
end;

procedure TDataList_Objects<T>.Append(const AValue: T);
begin
  fList.Add(TObjectData<T>.Create(AValue));
end;

{ TPersistDataList_Objects }

function TPersistDataList_Objects<T>.GetData(AIndex: Integer): IRBData;
begin
  Result := fList[AIndex].RB;
end;

function TPersistDataList_Objects<T>.GetValue(AIndex: Integer): T;
begin
  Result := fList[AIndex].Value;
end;

function TPersistDataList_Objects<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TPersistDataList_Objects<T>.Delete(APos: Integer);
begin
  fList.Delete(APos);
end;

function TPersistDataList_Objects<T>.Insert(APos: Integer): IRBData;
begin
  Result := TRBData.Create(T.Create);
  fList.Insert(APos, TPersistData<T>.Create(Result.UnderObject as T));
end;

function TPersistDataList_Objects<T>.Append: IRBData;
begin
  Result := TRBData.Create(T.Create);
  fList.Add(TPersistData<T>.Create(Result.UnderObject as T));
end;

procedure TPersistDataList_Objects<T>.Insert(APos: Integer; const AValue: IRBData);
begin
  fList.Insert(APos, TPersistData<T>.Create(AValue.UnderObject as T));
end;

procedure TPersistDataList_Objects<T>.Append(const AValue: IRBData);
begin
  fList.Add(TPersistData<T>.Create(AValue.UnderObject as T));
end;

procedure TPersistDataList_Objects<T>.Insert(APos: Integer; const AValue: T);
begin
  fList.Insert(APos, TPersistData<T>.Create(AValue));
end;

procedure TPersistDataList_Objects<T>.Append(const AValue: T);
begin
  fList.Add(TPersistData<T>.Create(AValue));
end;

procedure TPersistDataList_Objects<T>.Add(const APeristData: TPersistData<T>);
begin
  fList.Add(APeristData);
end;

procedure TPersistDataList_Objects<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fList := TDataList.Create;
end;

procedure TPersistDataList_Objects<T>.BeforeDestruction;
begin
  FreeAndNil(fList);
  inherited BeforeDestruction;
end;

{ TPersistDataListBuilder }

procedure TPersistDataListBuilder<T>.Add(const ASID: String;
  const ARBData: IRBData);
begin
  fList.Add(TPersistData<T>.Create(ASID, ARBData.UnderObject as T));
end;

function TPersistDataListBuilder<T>.Build: IDataList<T>;
begin
  Result := fList as IDataList<T>;
end;

procedure TPersistDataListBuilder<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fList := TPersistDataList_Objects<T>.Create;
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

function TPersistManyDataItem.GetIsID: Boolean;
begin
  Result := fPersistMany.IsID;
end;

function TPersistManyDataItem.GetIsInterface: Boolean;
begin
  Result := fPersistMany.IsInterface;
end;

function TPersistManyDataItem.GetTypeKind: TTypeKind;
begin

end;

function TPersistManyDataItem.GetGuid: TGuid;
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

