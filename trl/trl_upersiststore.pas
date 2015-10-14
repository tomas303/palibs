unit trl_upersiststore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersiststore, trl_ifactory, trl_irttibroker,
  trl_dicontainer, fgl;

type

  { TPersistFactory }

  TPersistFactory = class(TDIFactory, IPersistFactory)
  protected
    // IPersistFactory
    function CreateObject(const AClass: string): IRBData;
  end;


  { TStoreCache }

  TStoreCache = class(TDIObject)
  private type

    { TDataRecord }

    TDataRecord = record
      fSID: TSID;
      fData: IRBData;
    public
      function Create(const ASID: TSID; const AData: IRBData): TDataRecord;
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
    function GetData: IRBData;
    function GetSID: TSID;
    function GetStore: IPersistStore;
    procedure SetData(AValue: IRBData);
    procedure SetSID(AValue: TSID);
    procedure SetStore(AValue: IPersistStore);
    property Data: IRBData read GetData;
    property Store: IPersistStore read GetStore write SetStore;
    property SID: TSID read GetSID write SetSID;
  protected
    function GetClassName: string; virtual; abstract;
    property ClassName: string read GetClassName;
  end;

  TPersistRef<TItem: TObject> = class(TPersistRef, IPersistRef<TItem>)
  private
    fItem: TItem;
  protected
    function GetItem: TItem;
    procedure SetItem(AValue: TItem);
    property Item: TItem read GetItem write SetItem;
  protected
    function GetClassName: string; override;
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
    function LoadList(const AClass: string): IPersistList;
    function GetSID(const AData: IRBData): TSID;
    procedure Open;
    procedure Close;
    procedure Flush;
    property SID[const AData: IRBData]: TSID read GetSID;
    // IPersistQuery
    function Retrive(const AClass: string): IPersistList;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property Device: IPersistStoreDevice read fDevice write fDevice;
    property Cache: TStoreCache read fCache write fCache;
  end;

implementation

{ TPersistRef<TItem> }

function TPersistRef<TItem>.GetClassName: string;
begin
  Result := TItem.ClassName;
end;

function TPersistRef<TItem>.GetItem: TItem;
begin
  Result := fItem;
end;

procedure TPersistRef<TItem>.SetItem(AValue: TItem);
begin
  fItem := AValue;
end;

{ TPersistRef }

function TPersistRef.GetData: IRBData;
begin
  if fData = nil then begin
    fData := fStore.Load(fSID);
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
  fSID := AValue;
end;

procedure TPersistRef.SetStore(AValue: IPersistStore);
begin
  fStore := AValue;
end;

{ TStoreCache.TDataRecord }

function TStoreCache.TDataRecord.Create(const ASID: TSID; const AData: IRBData): TDataRecord;
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
  Result := Container.Locate(IRBData, AClass);
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
  fCache.Remove(AData);
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
end;

function TPersistStore.Load(const ASID: TSID): IRBData;
var
  mClass: string;
begin
  mClass := fDevice.GetSIDClass(ASID);
  Result := New(mClass);
  fDevice.Load(ASID, Result);
  fCache.Add(ASID, Result);
end;

function TPersistStore.LoadList(const AClass: string): IPersistList;
begin
  Result := (fDevice as IPersistQuery).Retrive(AClass);
end;

function TPersistStore.GetSID(const AData: IRBData): TSID;
begin
  Result := fCache.FindSID(AData);
end;

procedure TPersistStore.Open;
begin
  fDevice.Open;
end;

procedure TPersistStore.Close;
begin
  fDevice.Close;
end;

procedure TPersistStore.Flush;
begin
  fDevice.Flush;
end;

function TPersistStore.Retrive(const AClass: string): IPersistList;
begin
  Result := (fDevice as IPersistQuery).Retrive(AClass);
end;

end.

