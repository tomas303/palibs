unit tal_ureg;

{$mode objfpc}{$H+}

interface

uses
  tal_ireg, trl_idifactory, trl_dicontainer,
  tal_ureaapp, rea_iapp, tal_urealauncher, trl_ilauncher,
  trl_ilog, tal_uwindowlog, flu_iflux,
  trl_iExecutor, trl_imetaelementfactory,
  trl_imetaelement,
  trl_ireconciler, rea_idesigncomponent, rea_irenderer;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterReactApp: TDIReg;
    function RegisterWindowLog: TDIReg;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterReactApp: TDIReg;
var
  mReg: TDIReg;
begin
  // todo: think about move this to rea package ... that means move IFluxStore to flu
  mReg := DIC.Add(TReactLauncher, ILauncher);
  mReg.InjectProp('ReactApp', IReactApp);
  //
  Result := DIC.Add(TReactApp, IReactApp, '', ckSingle);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Executor', IExecutor);
end;

function TReg.RegisterWindowLog: TDIReg;
begin
  Result := DIC.Add(TWindowLog, ILog, '', ckSingle);
end;

end.

