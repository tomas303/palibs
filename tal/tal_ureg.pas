unit tal_ureg;

{$mode objfpc}{$H+}

interface

uses
  tal_ireg, trl_idifactory, trl_dicontainer,
  tal_urealauncher, trl_ilauncher,
  trl_ilog, tal_uwindowlog, flu_iflux,
  trl_iExecutor, trl_imetaelementfactory,
  trl_imetaelement,
  trl_ireconciler, rea_idesigncomponent, rea_irenderer, trl_udifactory;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterReactLauncher: TDIReg;
    function RegisterWindowLog: TDIReg;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterReactLauncher: TDIReg;
begin
  Result := DIC.Add(TReactLauncher, ILauncher);
  Result.InjectProp('Dispatcher', IFluxDispatcher);
  Result.InjectProp('Executor', IExecutor);
  Result.InjectProp('Renderer', IRenderer);
  Result.InjectProp('GUI', IDesignComponentApp);
  Result.InjectProp('Factory2', TDIFactory2);
end;

function TReg.RegisterWindowLog: TDIReg;
begin
  Result := DIC.Add(TWindowLog, ILog, '', ckSingle);
end;

end.

