unit rdx_ustoreconnector;

{$mode objfpc}{$H+}

interface

uses
  trl_igenericaccess, trl_itree, flu_iflux, sysutils, trl_iprops,
  trl_idifactory;

type

  { TRdxStoreConnector }

  TRdxStoreConnector = class(TInterfacedObject, IFluxStoreConnector, IFluxData, IFluxDispatcher)
  protected
    // IFluxData
    function GetData(const AKey: string): IGenericAccess;
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAppAction: IFluxAction);
  protected
    fStore: IFluxData;
  published
    property Store: IFluxData read fStore write fStore;
  end;

implementation

{ TRdxStoreConnector }

function TRdxStoreConnector.GetData(const AKey: string): IGenericAccess;
begin
  Result := Store[AKey];
end;

procedure TRdxStoreConnector.Dispatch(const AAppAction: IFluxAction);
begin
  (Store as IFluxDispatcher).Dispatch(AAppAction);
end;

end.

