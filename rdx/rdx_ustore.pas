unit rdx_ustore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, trl_iprops, flu_iflux, trl_igenericaccess;

type

  { TRdxStore }

  TRdxStore = class(TInterfacedObject, IFluxStore, IFluxDispatcher, IPropFinder, IFluxData)
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAction: IFluxAction);
    // IPropFinder
    function Find(const AID: string): IProp;
    // IFluxData
    function GetData(const AKey: string): IGenericAccess;
  protected
    fRdxState: IFluxState;
    fDispatcher: IFluxDispatcher;
    fData: IFluxData;
  published
    property State: IFluxState read fRdxState write fRdxState;
    property Dispatcher: IFluxDispatcher read fDispatcher write fDispatcher;
    property Data: IFluxData read fData write fData;
  end;

implementation

{ TRdxStore }

procedure TRdxStore.Dispatch(const AAction: IFluxAction);
begin
  Dispatcher.Dispatch(AAction);
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

function TRdxStore.GetData(const AKey: string): IGenericAccess;
begin
  Result := Data[AKey];
end;

end.

