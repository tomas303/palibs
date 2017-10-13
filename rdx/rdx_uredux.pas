unit rdx_uredux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rdx_iredux, fgl, trl_iprops, flu_iflux;

type

  { TRdxStore }

  TRdxStore = class(TInterfacedObject, IRdxStore, IFluxDispatcher)
  protected type
    TEvents = specialize TFPGList<TRdxStoreEvent>;
  protected
    fEvents: TEvents;
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAction: IFluxAction);
    // IRdxStore
    procedure Add(const AEvent: TRdxStoreEvent);
    procedure Remove(const AEvent: TRdxStoreEvent);
  protected
    fRdxState: IRdxState;
    fRdxFunc: IRdxFunc;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property RdxState: IRdxState read fRdxState write fRdxState;
    property RdxFunc: IRdxFunc read fRdxFunc write fRdxFunc;
  end;

  { TMapStateToProps }

  TMapStateToProps = class(TInterfacedObject, IMapStateToProps)
  protected type
    TItem = class(TObject)
    protected
      fPath: string;
      fKeys: TStringArray;
      function GetKey(AIndex: integer): string;
      function GetKeyCount: integer;
    public
      constructor Create(const APath: string; AKeys: TStringArray);
      property Path: string read fPath;
      property KeyCount: integer read GetKeyCount;
      property Key[AIndex: integer]: string read GetKey;
    end;
  protected type
    TItems = specialize TFPGObjectList<TItem>;
  protected
    fItems: TItems;
    fKeys: TStringList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    // IMapStateToProps
    function Map(const AProps: IProps): IProps;
    function AddPath(const APath: string; AKeys: TStringArray): IMapStateToProps;
  protected
    fRdxState: IRdxState;
    procedure SetAddKey(const AKey: string);
  published
    property RdxState: IRdxState read fRdxState write fRdxState;
    property AddKey: string write SetAddKey;
  end;

implementation

{ TMapStateToProps.TItem }

function TMapStateToProps.TItem.GetKey(AIndex: integer): string;
begin
  Result := fKeys[AIndex];
end;

function TMapStateToProps.TItem.GetKeyCount: integer;
begin
  Result := Length(fKeys);
end;

constructor TMapStateToProps.TItem.Create(const APath: string;
  AKeys: TStringArray);
begin
  inherited Create;
  fPath := APath;
  fKeys := AKeys;
end;

{ TMapStateToProps }

procedure TMapStateToProps.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TItems.Create;
  fKeys := TStringList.Create;
end;

procedure TMapStateToProps.BeforeDestruction;
begin
  FreeAndNil(fItems);
  FreeAndNil(fKeys);
  inherited BeforeDestruction;
end;

function TMapStateToProps.Map(const AProps: IProps): IProps;
var
  mProp, mKeyProp: IProp;
  mItem: TItem;
  mKey: string;
  i: integer;
begin
  Result := AProps.Clone;
  for mKey in fKeys do
  begin
   mProp := (RdxState as IPropFinder).Find(mKey);
   if mProp <> nil then
     Result.SetProp(mProp.Name, mProp);
  end;
end;

function TMapStateToProps.AddPath(const APath: string; AKeys: TStringArray
  ): IMapStateToProps;
begin
  Result := Self;
  fItems.Add(TItem.Create(APath, AKeys));
end;

procedure TMapStateToProps.SetAddKey(const AKey: string);
begin
  if fKeys.IndexOf(AKey) <> -1 then
    raise Exception.CreateFmt('Key %s for map state already added', [AKey]);
  fKeys.Add(AKey);
end;

{ TRdxStore }

procedure TRdxStore.Dispatch(const AAction: IFluxAction);
var
  mEvent: TRdxStoreEvent;
begin
  RdxState := RdxFunc.Redux(RdxState, AAction);
  if RdxState = nil then
    raise Exception.Create('Redux function returned nil instead of AppState');
  for mEvent in fEvents do begin
    mEvent(RdxState);
  end;
end;

procedure TRdxStore.Add(const AEvent: TRdxStoreEvent);
begin
  if fEvents.IndexOf(AEvent) = -1 then
    fEvents.Add(AEvent);
end;

procedure TRdxStore.Remove(const AEvent: TRdxStoreEvent);
var
  mIndex: integer;
begin
  mIndex := fEvents.IndexOf(AEvent);
  if mIndex <> -1 then
    fEvents.Delete(mIndex);
end;

procedure TRdxStore.AfterConstruction;
begin
  inherited AfterConstruction;
  fEvents := TEvents.Create;
end;

procedure TRdxStore.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  inherited BeforeDestruction;
end;

end.

