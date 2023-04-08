unit trl_upersist;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  fgl, trl_irttibroker, trl_urttibroker, sysutils,
  trl_pubsub, trl_ipersist, trl_dicontainer, trl_udifactory;

type

  { TPersistFactory }

  TPersistFactory = class(TCustomDIFactory, IPersistFactory)
  protected
    // IPersistFactory
    function CreateObject(const AClass: string): IRBData;
    function Create(const AClass: string; const AID: string = ''): TObject; overload;
    function Create(AInterface: TGUID; const AID: string = ''): IUnknown; overload;
  end;

  { TMiniListEnumerator }

  TMiniListEnumerator = class(TInterfacedObject, IRBDataEnumerator)
  private
    fList: TFPGInterfacedObjectList<IRBData>;
    fIndex: Integer;
  public
    constructor Create(AList: TFPGInterfacedObjectList<IRBData>);
    function MoveNext: Boolean;
    function GetCurrent: IRBData;
    property Current: IRBData read GetCurrent;
  end;


  { TMinilist }

  TMinilist<T: TObject> = class(TInterfacedObject, IMiniList, IMiniList<T>)
  private
    fList: TFPGInterfacedObjectList<IRBData>;
    fPSPersistChannel: IPSPersistChannel;
  private
    function GetCount: Integer;
    function GetField(AIndex: Integer; const AName: String): String;
    procedure SetField(AIndex: Integer; const AName: String; AValue: String);
    function GetList(AIndex: Integer; const AName: String): IMiniList;
    procedure SetList(AIndex: Integer; const AName: String; const AValue: IMiniList);
    function GetData(AIndex: Integer): IRBData;
    function Insert(APos: Integer): IRBData; overload;
    function Append: IRBData; overload;
    procedure Delete(APos: Integer);
    property Count: Integer read GetCount;
    property Field[AIndex: Integer; const AName: String]: String read GetField write SetField;
    property Data[AIndex: Integer]: IRBData read GetData;
    function GetEnumerator: IRBDataEnumerator;
    function PSPersistChannel: IPSPersistChannel;
  public
    constructor Create(const AList: TFPGInterfacedObjectList<IRBData>; const APSPersistChannel: IPSPersistChannel;
      AFactory2: TDIFactory2);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fFactory2: TDIFactory2;
  published
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
  end;


  { TMiniDataList }

  TMiniDataList<T: TObject> = class(TInterfacedObject, IMiniDataList<T>)
  private
    procedure LocateFinished(var Msg); message 'LocateFinished';
  private
    fPSPersistChannel: IPSPersistChannel;
    procedure PSPersistChannelObserver(const AInfo: TPersistInfo);
  private
    fList: TFPGInterfacedObjectList<IRBData>;
    fChanges: TFPGInterfacedObjectList<IRBData>;
    fDeletes: TFPGInterfacedObjectList<IRBData>;
    fNews: TFPGInterfacedObjectList<IRBData>;
  private
    function NewList: IMiniList<T>; overload;
    function NewList(const APredicate: TPersistPredicate): IMiniList<T>; overload;
    function GetEnumerator: IRBDataEnumerator;
    procedure Load;
    procedure Save;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fPubSub: IPubSub;
    fDevice: IPersistStoreDevice;
    fFactory2: TDIFactory2;
  published
    property PubSub: IPubSub read fPubSub write fPubSub;
    property Device: IPersistStoreDevice read fDevice write fDevice;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
  end;


implementation

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

{ TMinilist }

function TMinilist<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TMinilist<T>.GetField(AIndex: Integer; const AName: String): String;
begin
  Result := fList[AIndex].ItemByName[AName].AsString;
end;

procedure TMinilist<T>.SetField(AIndex: Integer; const AName: String; AValue: String);
var
  mData: IRBData;
begin
  mData := fList[AIndex];
  if mData.ItemByName[AName].AsString = AValue then
    Exit;
  mData.ItemByName[AName].AsString := AValue;
  if fPSPersistChannel <> nil then
    fPSPersistChannel.Publish(TPersistInfo.Create(mData, paChange));
end;

function TMinilist<T>.GetList(AIndex: Integer; const AName: String): IMiniList;
begin
  Result := fList[AIndex].ItemByName[AName].AsInterface as IMiniList;
end;

procedure TMinilist<T>.SetList(AIndex: Integer; const AName: String; const AValue: IMiniList);
var
  mData: IRBData;
begin
  mData := fList[AIndex];
  mData.ItemByName[AName].AsInterface := AValue;
  if fPSPersistChannel <> nil then
    fPSPersistChannel.Publish(TPersistInfo.Create(mData, paChange));
end;

function TMinilist<T>.Insert(APos: Integer): IRBData;
begin
  Result := TRBData.Create(fFactory2.LocateC<T>);
  fList.Insert(APos, Result);
  if fPSPersistChannel <> nil then
    fPSPersistChannel.Publish(TPersistInfo.Create(Result, paNew));
end;

function TMinilist<T>.Append: IRBData;
begin
  Result := TRBData.Create(fFactory2.LocateC<T>);
  fList.Add(Result);
  if fPSPersistChannel <> nil then
    fPSPersistChannel.Publish(TPersistInfo.Create(Result, paNew));
end;

procedure TMinilist<T>.Delete(APos: Integer);
var
  mData: IRBData;
begin
  mData := fList[APos];
  fList.Delete(APos);
  if fPSPersistChannel <> nil then
    fPSPersistChannel.Publish(TPersistInfo.Create(mData, paDelete));
end;

function TMinilist<T>.GetData(AIndex: Integer): IRBData;
begin
  Result := fList[AIndex];
end;

function TMinilist<T>.GetEnumerator: IRBDataEnumerator;
begin
  Result := TMiniListEnumerator.Create(fList);
end;

function TMinilist<T>.PSPersistChannel: IPSPersistChannel;
begin
  Result := fPSPersistChannel;
end;

constructor TMinilist<T>.Create(const AList: TFPGInterfacedObjectList<IRBData>; const APSPersistChannel: IPSPersistChannel;
  AFactory2: TDIFactory2);
begin
  inherited Create;
  fList := AList;
  fPSPersistChannel := APSPersistChannel;
  fFactory2 := AFactory2;
end;

procedure TMinilist<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  if fList = nil then
    fList := TFPGInterfacedObjectList<IRBData>.Create;
end;

procedure TMinilist<T>.BeforeDestruction;
begin
  FreeAndNil(fList);
  inherited BeforeDestruction;
end;

{ TMiniDataList }

procedure TMiniDataList<T>.LocateFinished(var Msg);
begin
  fPSPersistChannel := PubSub.Factory.NewDataChannel<TPersistInfo>;
  fPSPersistChannel.Subscribe(PSPersistChannelObserver);
end;

procedure TMiniDataList<T>.PSPersistChannelObserver(const AInfo: TPersistInfo);
var
  i: Integer;
begin
  case AInfo.Action of
    paNew:
      begin
        i := fNews.IndexOf(AInfo.Data);
        if i = -1 then
          fNews.Add(AInfo.Data);
      end;
    paChange:
      begin
        i := fNews.IndexOf(AInfo.Data);
        if i > -1 then
          Exit;
        i := fChanges.IndexOf(AInfo.Data);
        if i = -1 then
          fChanges.Add(AInfo.Data);
      end;
    paDelete:
      begin
        i := fDeletes.IndexOf(AInfo.Data);
        if i = -1 then
          fDeletes.Add(AInfo.Data);
      end;
  end;
end;

function TMiniDataList<T>.NewList: IMiniList<T>;
var
  mList: TFPGInterfacedObjectList<IRBData>;
begin
  mList := TFPGInterfacedObjectList<IRBData>.Create;
  mList.Assign(fList);
  Result := TMiniList<T>.Create(mList, fPSPersistChannel, Factory2);
end;

function TMiniDataList<T>.NewList(const APredicate: TPersistPredicate): IMiniList<T>;
var
  mData: IRBData;
  mList: TFPGInterfacedObjectList<IRBData>;
begin
  mList := TFPGInterfacedObjectList<IRBData>.Create;
  for mData in fList do begin
    if APredicate(mData) then
      mList.Add(mData);
  end;
  Result := TMiniList<T>.Create(mList, fPSPersistChannel, Factory2);
end;

function TMiniDataList<T>.GetEnumerator: IRBDataEnumerator;
begin
  Result := TMiniListEnumerator.Create(fList);
end;

procedure TMiniDataList<T>.Load;
var
  mData: IRBData;
begin
  fList.Clear;
  fChanges.Clear;
  fDeletes.Clear;
  fNews.Clear;
  for mData in Device.Select2(T.ClassName) do begin
    fList.Add(mData);
  end;
end;

procedure TMiniDataList<T>.Save;
var
  mData: IRBData;
begin
  for mData in fDeletes do
    Device.Delete2(mData);
  fDeletes.Clear;
  for mData in fNews do
    Device.Save2(mData);
  fNews.Clear;
  for mData in fChanges do
    Device.Save2(mData);
  fChanges.Clear;
end;

procedure TMiniDataList<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fList := TFPGInterfacedObjectList<IRBData>.Create;
  fChanges := TFPGInterfacedObjectList<IRBData>.Create;
  fDeletes := TFPGInterfacedObjectList<IRBData>.Create;
  fNews := TFPGInterfacedObjectList<IRBData>.Create;
end;

procedure TMiniDataList<T>.BeforeDestruction;
begin
  fPSPersistChannel.Unsubscribe(PSPersistChannelObserver);
  FreeAndNil(fList);
  FreeAndNil(fChanges);
  FreeAndNil(fDeletes);
  FreeAndNil(fNews);
  inherited BeforeDestruction;
end;

{ TMiniListEnumerator }

constructor TMiniListEnumerator.Create(AList: TFPGInterfacedObjectList<IRBData>);
begin
  inherited Create;
  fList := AList;
  fIndex := -1;
end;

function TMiniListEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex <= fList.Count - 1;
end;

function TMiniListEnumerator.GetCurrent: IRBData;
begin
  Result := fList[fIndex];
end;


end.

