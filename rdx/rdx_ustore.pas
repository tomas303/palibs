unit rdx_ustore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, trl_iprops, flu_iflux, trl_igenericaccess;

type

  { TRdxStore }

  TRdxStore = class(TInterfacedObject, IFluxStore, IFluxDispatcher, IPropFinder)
  protected type
    TEvents = specialize TFPGList<TFluxStoreEvent>;
  protected
    fEvents: TEvents;
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAction: IFluxAction);
    // IFluxStore
    procedure Add(const AEvent: TFluxStoreEvent);
    procedure Remove(const AEvent: TFluxStoreEvent);
    // IPropFinder
    function Find(const AID: string): IProp;
  protected
    fRdxState: IFluxState;
    fDispatcher: IFluxDispatcher;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property State: IFluxState read fRdxState write fRdxState;
    property Dispatcher: IFluxDispatcher read fDispatcher write fDispatcher;
  end;

implementation

{ TRdxStore }

procedure TRdxStore.Dispatch(const AAction: IFluxAction);
var
  mEvent: TFluxStoreEvent;
begin
  Dispatcher.Dispatch(AAction);
  for mEvent in fEvents do begin
    mEvent(State);
  end;
end;

procedure TRdxStore.Add(const AEvent: TFluxStoreEvent);
begin
  if fEvents.IndexOf(AEvent) = -1 then
    fEvents.Add(AEvent);
end;

procedure TRdxStore.Remove(const AEvent: TFluxStoreEvent);
var
  mIndex: integer;
begin
  mIndex := fEvents.IndexOf(AEvent);
  if mIndex <> -1 then
    fEvents.Delete(mIndex);
end;

function TRdxStore.Find(const AID: string): IProp;
var
  mProp: IProp;
begin
  mProp := (State as IPropFinder).Find(AID);
  if mProp = nil then
    raise Exception.CreateFmt('Cannot find state with path %s', [AID]);
  Result := mProp;
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

