unit rdx_ufunc;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, trl_igenericaccess;

type

  { TRdxFunc }

  TRdxFunc = class(TInterfacedObject, IFluxFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); virtual; abstract;
  protected
    // IFluxFunc
    procedure Execute(const AAction: IFluxAction);
  protected
    fState: IGenericAccess;
  published
    property State: IGenericAccess read fState write fState;
  end;

implementation

{ TRdxFunc }

procedure TRdxFunc.Execute(const AAction: IFluxAction);
begin
  DoExecute(AAction);
end;

end.

