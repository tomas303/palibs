unit rea_ureafactory;

{$mode delphi}{$H+}

interface

uses
  rea_ireafactory, rea_iflux, trl_iprops , trl_udifactory, rea_idesigncomponent;

type

  { TReaFactory }

  TReaFactory = class(TInterfacedObject, IReaFactory)
  protected
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewProps: IProps;
  private
    fFactory2: TDIFactory2;
  published
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
  end;

implementation

{ TReaFactory }

function TReaFactory.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := Factory2.Locate<IFluxNotifier>(NewProps.SetInt(cAction.ActionID, AActionID));
end;

function TReaFactory.NewProps: IProps;
begin
  Result := Factory2.Locate<IProps>;
end;

end.

