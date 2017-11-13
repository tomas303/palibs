unit tal_ureg;

{$mode objfpc}{$H+}

interface

uses
  tal_ireg, trl_idifactory, rea_ireact, rdx_iredux, trl_dicontainer,
  tal_ureaapp, rea_iapp, tal_urealauncher, trl_ilauncher,
  trl_ilog, tal_uwindowlog;

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
  // todo: think about move this to rea package ... that means move IRdxStore to flu
  mReg := DIC.Add(TReactLauncher, ILauncher);
  mReg.InjectProp('ReactApp', IReactApp);
  //
  Result := DIC.Add(TReactApp, IReactApp, '', ckSingle);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('React', IReact);
  Result.InjectProp('AppStore', IRdxStore);
  Result.InjectProp('ElFactory', IMetaElementFactory);
end;

function TReg.RegisterWindowLog: TDIReg;
begin
  Result := DIC.Add(TWindowLog, ILog, '', ckSingle);
end;

end.

