unit rdx_ureg;

{$mode objfpc}{$H+}

interface

uses
  rdx_ireg, trl_dicontainer, flu_iflux, rdx_ufuncdispatcher, trl_iinjector, trl_idifactory,
  trl_iprops, trl_iExecutor;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterDispatcher: TDIReg;
    procedure RegisterCommon;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterDispatcher: TDIReg;
begin
  Result := DIC.Add(TRdxFuncDispatcher, IFluxDispatcher, '', ckSingle);
  Result.InjectProp('Executor', IExecutor);
end;

procedure TReg.RegisterCommon;
begin
  RegisterDispatcher;
end;

end.

