unit rdx_ureg;

{$mode objfpc}{$H+}

interface

uses
  rdx_ireg, trl_dicontainer, flu_iflux, rdx_ufunc, trl_iinjector, trl_idifactory,
  rdx_ustate, trl_iprops, rdx_uredux;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterFunc(const ASubFuncs: array of TClass): TDIReg;
    function RegisterState: TDIReg;
    function RegisterStore: TDIReg;
    procedure RegisterCommon(const ASubFuncs: array of TClass);
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterFunc(const ASubFuncs: array of TClass): TDIReg;
var
  mFuncClass: TClass;
begin
  Result := DIC.Add(TRdxFunc, IFluxFunc, '', ckSingle);
  Result.InjectProp('Injector', IInjector);
  Result.InjectProp('Factory', IDIFactory);
  for mFuncClass in ASubFuncs do begin
    DIC.Add(mFuncClass, IFluxFunc, mFuncClass.ClassName);
    Result.InjectProp('AddFunc', IFluxFunc, mFuncClass.ClassName);
  end;
end;

function TReg.RegisterState: TDIReg;
begin
  Result := DIC.Add(TRdxState, IFluxState, '', ckSingle);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Data', IProps);
end;

function TReg.RegisterStore: TDIReg;
begin
  Result := DIC.Add(TRdxStore, IFluxStore, '', ckSingle);
  Result.InjectProp('RdxState', IFluxState);
  Result.InjectProp('RdxFunc', IFluxFunc);
end;

procedure TReg.RegisterCommon(const ASubFuncs: array of TClass);
begin
  RegisterState;
  RegisterFunc(ASubFuncs);
  RegisterStore;
end;

end.

