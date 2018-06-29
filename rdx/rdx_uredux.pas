unit rdx_uredux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, trl_iprops, flu_iflux;

type

  { TRdxStore }

  TRdxStore = class(TInterfacedObject, IFluxStore, IFluxDispatcher)
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
  protected
    fRdxState: IFluxState;
    fRdxFunc: IFluxFunc;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property State: IFluxState read fRdxState write fRdxState;
    property Func: IFluxFunc read fRdxFunc write fRdxFunc;
  end;

implementation

{ TRdxStore }

procedure TRdxStore.Dispatch(const AAction: IFluxAction);
var
  mEvent: TFluxStoreEvent;
begin
  State := Func.Redux(State, AAction);
  if State = nil then
    raise Exception.Create('Redux function returned nil instead of AppState');
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

