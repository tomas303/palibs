unit trl_upersiststore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_ifactory, trl_irttibroker,
  trl_dicontainer, fgl, trl_upersist;

type

  { TSIDList }

  TSIDList = class(TInterfacedObject, ISIDList)
  protected type
    TSIDItems = class(TFPGList<TSID>)
    end;
  private
    fItems: TSIDItems;
  protected
    // ISIDList
    function GetCount: integer;
    function GetItems(AIndex: integer): TSID;
    procedure SetCount(AValue: integer);
    procedure SetItems(AIndex: integer; AValue: TSID);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Count: integer read GetCount write SetCount;
    property Items[AIndex: integer]: TSID read GetItems write SetItems; default;
  end;

  { TPersistFactory }

  TPersistFactory = class(TCustomDIFactory, IPersistFactory)
  protected
    // IPersistFactory
    function CreateObject(const AClass: string): IRBData;
    function Create(const AClass: string; const AID: string = ''): TObject; overload;
    function Create(AInterface: TGUID; const AID: string = ''): IUnknown; overload;
  end;


  { TStoreCache }

  TStoreCache = class(TDIObject)
  private type

    { TDataRecord }

    TDataRecord = record
      fSID: TSID;
      fData: IRBData;
    public
      class function Create(const ASID: TSID; const AData: IRBData): TDataRecord; static;
      procedure Clear;
      class operator =(a, b: TDataRecord): Boolean;
    end;

    TCache = TFPGList<TDataRecord>;

  private
    fCache: TCache;
  protected
    function FindIndex(const ASID: TSID): integer; overload;
    function FindIndex(const AData: IRBData): integer; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const ASID: TSID; const AData: IRBData);
    procedure Remove(const ASID: TSID); overload;
    procedure Remove(const AData: IRBData); overload;
    function FindData(const ASID: TSID): IRBData;
    function FindSID(const AData: IRBData): TSID;
  end;

  { TPersistRef }

  TPersistRef = class(TInterfacedObject, IPersistRef)
  private
    fSID: TSID;
    fStore: IPersistStore;
    fData: IRBData;
    fClassName: string;
  protected
    // IPersistRef
    function GetClassName: string; virtual;
    function GetData: IRBData;
    function GetSID: TSID;
    function GetStore: IPersistStore;
    procedure SetClassName(AValue: string);
    procedure SetData(AValue: IRBData);
    procedure SetSID(AValue: TSID);
    procedure SetStore(AValue: IPersistStore);
    property Data: IRBData read GetData;
    property SID: TSID read GetSID write SetSID;
    property ClassName: string read GetClassName write SetClassName;
  public
    procedure AfterConstruction; override;
  published
    property Store: IPersistStore read GetStore write SetStore;
  end;

  TPersistRef<TItem: TObject> = class(TPersistRef, IPersistRef<TItem>)
  protected
    function GetItem: TItem;
    procedure SetItem(AValue: TItem);
    property Item: TItem read GetItem write SetItem;
  protected
    function GetClassName: string; override;
  end;

  { TPersistRefList }

  TPersistRefList = class(TInterfacedObject, IPersistRefList)
  private type
    TRefListItems = TFPGInterfacedObjectList<IPersistRef>;
  private
    fItems: TRefListItems;
  protected
  // IPersistRefList
    function GetCount: integer;
    function GetData(AIndex: integer): IRBData;
    function GetItems(AIndex: integer): IPersistRef;
    procedure SetCount(AValue: integer);
    procedure SetItems(AIndex: integer; AValue: IPersistRef);
    function IndexOfData(const AData: IRBData): integer;
    procedure Delete(AIndex: integer);
    procedure Insert(AIndex: integer; const AValue: IPersistRef);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Count: integer read GetCount write SetCount;
    property Items[AIndex: integer]: IPersistRef read GetItems write SetItems; default;
    property Data[AIndex: integer]: IRBData read GetData;
  end;


  { TPersistStore }

  TPersistStore = class(TInterfacedObject, IPersistStore, IPersistQuery)
  protected
    fFactory: IPersistFactory;
    fDevice: IPersistStoreDevice;
    fCache: TStoreCache;
  protected
    // IPersistStore
    function New(const AClass: string): IRBData;
    procedure Load(AData: IRBData); overload;
    procedure Save(AData: IRBData);
    procedure Delete(AData: IRBData);
    function Load(const ASID: TSID): IRBData; overload;
    function GetSID(const AData: IRBData): TSID;
    procedure Open; overload;
    procedure Open(const AFile: string); overload;
    procedure Open(const AStream: TStream); overload;
    procedure Close; overload;
    procedure Close(const AStream: TStream); overload;
    procedure Flush;
    function IsOpened: Boolean;
    property SID[const AData: IRBData]: TSID read GetSID;
    // IPersistQuery
    function SelectClass(const AClass: string): IPersistRefList;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property Device: IPersistStoreDevice read fDevice write fDevice;
    property Cache: TStoreCache read fCache write fCache;
  end;

  { TPersistManyRefs }

  TPersistManyRefs = class(TPersistMany<IPersistRef>, IPersistManyRefs)
  private
    fFactory: IPersistFactory;
  protected
    function GetAsInterface(AIndex: integer): IUnknown; override;
    procedure SetAsInterface(AIndex: integer; AValue: IUnknown); override;
    function GetAsPersistData(AIndex: integer): IRBData; override;
    procedure SetAsPersistData(AIndex: integer; AValue: IRBData); override;
  protected
    function GetFactory: IPersistFactory;
    procedure SetFactory(AValue: IPersistFactory);
  published
    property Factory: IPersistFactory read GetFactory write SetFactory;
  end;

  TPersistManyRefs<TItem: TObject> = class(TPersistManyRefs, IPersistManyRefs<TItem>)
  protected
    function GetClassName: string; override;
  end;

implementation

{ TPersistRef<TItem> }

function TPersistRef<TItem>.GetClassName: string;
begin
  Result := TItem.ClassName;
end;

function TPersistRef<TItem>.GetItem: TItem;
begin
  if Data = nil then
    Result := TItem(nil)
  else
    Result := Data.UnderObject as TItem;
end;

procedure TPersistRef<TItem>.SetItem(AValue: TItem);
begin
  Data.UnderObject := AValue;
end;

{ TPersistManyRefs }

function TPersistManyRefs.GetAsInterface(AIndex: integer): IUnknown;
begin
  if AIndex > Count - 1 then
    Count := AIndex + 1;
  if Item[AIndex] = nil then
  begin
    Item[AIndex] := Factory.Create(IPersistRef) as IPersistRef;
    Item[AIndex].ClassName := ClassName;
  end;
  Result := Item[AIndex];
end;

procedure TPersistManyRefs.SetAsInterface(AIndex: integer; AValue: IUnknown);
begin
  Item[AIndex] := AValue as IPersistRef;
end;

function TPersistManyRefs.GetAsPersistData(AIndex: integer): IRBData;
begin
  Result := (AsInterface[AIndex] as IPersistRef).Data;
end;

procedure TPersistManyRefs.SetAsPersistData(AIndex: integer; AValue: IRBData);
begin
  (AsInterface[AIndex] as IPersistRef).Data := AValue;
end;

function TPersistManyRefs.GetFactory: IPersistFactory;
begin
  Result := fFactory;
end;

procedure TPersistManyRefs.SetFactory(AValue: IPersistFactory);
begin
  fFactory := AValue;
end;

function TPersistManyRefs<TItem>.GetClassName: string;
begin
  Result := TItem.ClassName;
end;

{ TSIDList }

function TSIDList.GetCount: integer;
begin
  Result := fItems.Count;
end;

function TSIDList.GetItems(AIndex: integer): TSID;
begin
  Result := fItems[AIndex];
end;

procedure TSIDList.SetCount(AValue: integer);
begin
  fItems.Count := AValue;
end;

procedure TSIDList.SetItems(AIndex: integer; AValue: TSID);
begin
  fItems[AIndex] := AValue;
end;

procedure TSIDList.BeforeDestruction;
begin
  FreeAndNil(fItems);
  inherited BeforeDestruction;
end;

procedure TSIDList.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TSIDItems.Create;
end;

{ TPersistRefList }

function TPersistRefList.GetCount: integer;
begin
  Result := fItems.Count;
end;

function TPersistRefList.GetData(AIndex: integer): IRBData;
begin
  Result := Items[AIndex].Data;
end;

function TPersistRefList.GetItems(AIndex: integer): IPersistRef;
begin
  Result := fItems[AIndex];
end;

procedure TPersistRefList.SetCount(AValue: integer);
begin
  fItems.Count := AValue;
end;

procedure TPersistRefList.SetItems(AIndex: integer; AValue: IPersistRef);
begin
  fItems[AIndex] := AValue;
end;

function TPersistRefList.IndexOfData(const AData: IRBData): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count- 1 do
  begin
    if Data[i] = AData then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TPersistRefList.Delete(AIndex: integer);
begin
  fItems.Delete(AIndex);
end;

procedure TPersistRefList.Insert(AIndex: integer; const AValue: IPersistRef);
begin
  fItems.Insert(AIndex, AValue);
end;

procedure TPersistRefList.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TRefListItems.Create;
end;

procedure TPersistRefList.BeforeDestruction;
begin
  FreeAndNil(fItems);
  inherited BeforeDestruction;
end;

{ TPersistRef }

procedure TPersistRef.SetClassName(AValue: string);
begin
  if AValue <> fClassName then begin
    fData := nil;
    fSID.Clear;
    fClassName := AValue;
  end;
end;

function TPersistRef.GetClassName: string;
begin
  if Data <> nil then
    Result := Data.ClassName
  else
    Result := fClassName;
end;

function TPersistRef.GetData: IRBData;
begin
  if fData = nil then begin
    if not fSID.IsClear then
      fData := fStore.Load(fSID)
    else
    if ClassName <> '' then
      fData := fStore.New(ClassName)
  end;
  Result := fData;
end;

function TPersistRef.GetSID: TSID;
begin
  if (fStore <> nil) and (fData <> nil) then
    Result := fStore.SID[fData]
  else
    Result := fSID;
end;

function TPersistRef.GetStore: IPersistStore;
begin
  Result := fStore;
end;

procedure TPersistRef.SetData(AValue: IRBData);
begin
  fData := AValue;
end;

procedure TPersistRef.SetSID(AValue: TSID);
begin
  if GetSID <> AValue then begin
    fData := nil;
    fClassName := '';
    fSID := AValue;
  end;
end;

procedure TPersistRef.SetStore(AValue: IPersistStore);
begin
  fStore := AValue;
end;

procedure TPersistRef.AfterConstruction;
begin
  inherited AfterConstruction;
  fSID.Clear;
end;

{ TStoreCache.TDataRecord }

class function TStoreCache.TDataRecord.Create(const ASID: TSID; const AData: IRBData): TDataRecord;
begin
  Result.fSID := ASID;
  Result.fData := AData;
end;

procedure TStoreCache.TDataRecord.Clear;
begin
  fSID.Clear;
  fData := nil;
end;

class operator TStoreCache.TDataRecord. = (a, b: TDataRecord): Boolean;
begin
  Result := (a.fSID = b.fSID) and (a.fData = b.fData);
end;

{ TStoreCache }

constructor TStoreCache.Create;
begin
  fCache := TCache.Create;
end;

destructor TStoreCache.Destroy;
begin
  FreeAndNil(fCache);
  inherited Destroy;
end;

function TStoreCache.FindIndex(const ASID: TSID): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to fCache.Count - 1 do
    if fCache[i].fSID = ASID then begin
      Result := i;
      Break;
    end;
end;

function TStoreCache.FindIndex(const AData: IRBData): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to fCache.Count - 1 do
    if fCache[i].fData = AData then begin
      Result := i;
      Break;
    end;
end;

procedure TStoreCache.Add(const ASID: TSID; const AData: IRBData);
var
  mIndex: integer;
begin
  mIndex := FindIndex(AData);
  if mIndex = -1 then begin
    fCache.Add(TDataRecord.Create(ASID, AData));
  end else begin
    raise Exception.create('already in cache');
  end;
end;

procedure TStoreCache.Remove(const ASID: TSID);
var
  mIndex: integer;
begin
  mIndex := FindIndex(ASID);
  if mIndex <> -1 then begin
    fCache[mIndex].Clear;
    fCache.Delete(mIndex);
  end;
end;

procedure TStoreCache.Remove(const AData: IRBData);
var
  mIndex: integer;
begin
  mIndex := FindIndex(AData);
  if mIndex <> -1 then begin
    fCache[mIndex].Clear;
    fCache.Delete(mIndex);
  end;
end;

function TStoreCache.FindData(const ASID: TSID): IRBData;
var
  mIndex: integer;
begin
  Result := nil;
  mIndex := FindIndex(ASID);
  if mIndex <> - 1 then
    Result := fCache[mIndex].fData;
end;

function TStoreCache.FindSID(const AData: IRBData): TSID;
var
  mIndex: integer;
begin
  Result.Clear;
  mIndex := FindIndex(AData);
  if mIndex <> - 1 then
    Result := fCache[mIndex].fSID;
end;

{ TPersistFactory }

function TPersistFactory.CreateObject(const AClass: string): IRBData;
begin
  if AClass = '' then
    raise Exception.Create('Try create IRBData object with empty class');
  Result := Container.Locate(IRBData, AClass);
end;

function TPersistFactory.Create(const AClass: string; const AID: string
  ): TObject;
begin
  Result := Container.Locate(AClass, AID);
end;

function TPersistFactory.Create(AInterface: TGUID; const AID: string): IUnknown;
begin
  Result := Container.Locate(AInterface, AID);
end;

{ TPersistStore }

function TPersistStore.New(const AClass: string): IRBData;
begin
  Result := Factory.CreateObject(AClass);
end;

procedure TPersistStore.Load(AData: IRBData);
var
  mSID: TSID;
begin
  mSID := fCache.FindSID(AData);
  if mSID.IsClear then
    raise exception.Create('cannot load object - not in cache');
  fDevice.Load(mSID, AData);
end;

procedure TPersistStore.Save(AData: IRBData);
var
  mSID: TSID;
begin
  mSID := fCache.FindSID(AData);
  if mSID.IsClear then begin
    mSID := fDevice.NewSID;
    fCache.Add(mSID, AData);
  end;
  fDevice.Save(mSID, AData);
end;

procedure TPersistStore.Delete(AData: IRBData);
var
  mSID: TSID;
begin
  mSID := fCache.FindSID(AData);
  if mSID.IsClear then
    raise exception.Create('delete sid not in cache');
  fDevice.Delete(mSID);
  fCache.Remove(AData);
end;

function TPersistStore.Load(const ASID: TSID): IRBData;
var
  mClass: string;
begin
  Result := nil;
  mClass := fDevice.GetSIDClass(ASID);
  if mClass = '' then
    Exit;
  Result := New(mClass);
  fDevice.Load(ASID, Result);
  fCache.Add(ASID, Result);
end;

function TPersistStore.GetSID(const AData: IRBData): TSID;
begin
  Result := fCache.FindSID(AData);
end;

procedure TPersistStore.Open;
begin
  fDevice.Open;
end;

procedure TPersistStore.Open(const AFile: string);
begin
  fDevice.Open(AFile);
end;

procedure TPersistStore.Open(const AStream: TStream);
begin
  fDevice.Open(AStream);
end;

procedure TPersistStore.Close;
begin
  fDevice.Close;
end;

procedure TPersistStore.Close(const AStream: TStream);
begin
  fDevice.Close(AStream);
end;

procedure TPersistStore.Flush;
begin
  fDevice.Flush;
end;

function TPersistStore.IsOpened: Boolean;
begin
  Result := fDevice.IsOpened;
end;

function TPersistStore.SelectClass(const AClass: string): IPersistRefList;
var
  mSIDs: ISIDList;
  i: integer;
begin
  Result := Factory.Create(IPersistRefList) as IPersistRefList;
  mSIDs := Device.GetSIDs(AClass);
  Result.Count := mSIDs.Count;
  for i := 0 to mSIDs.Count - 1 do
  begin
    Result[i] := Factory.Create(IPersistRef) as IPersistRef;
    Result[i].Store := Self;
    Result[i].SID := mSIDs[i];
  end;
end;

end.

