unit rdx_ureg;

{$mode objfpc}{$H+}

interface

uses
  rdx_ireg, trl_dicontainer, flu_iflux, rdx_ufuncdispatcher, trl_iinjector, trl_idifactory,
  rdx_ustate, trl_iprops, rdx_ustore, rdx_udata, rdx_ustoreconnector, trl_iExecutor;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterDispatcher(const ASubFuncs: array of string): TDIReg;
    function RegisterState: TDIReg;
    function RegisterFunc(const AClass: TClass): TDIReg;
    function RegisterStore: TDIReg;
    function RegisterData: TDIReg;
    function RegisterStoreConnector: TDIReg;
    procedure RegisterCommon(const ASubFuncs: array of string);
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterDispatcher(const ASubFuncs: array of string): TDIReg;
var
  mFunc: string;
begin
  Result := DIC.Add(TRdxFuncDispatcher, IFluxDispatcher, '', ckSingle);
  Result.InjectProp('Executor', IExecutor);
  for mFunc in ASubFuncs do begin
    Result.InjectProp('AddFunc', IFluxFunc, mFunc);
  end;
end;

function TReg.RegisterState: TDIReg;
begin
  Result := DIC.Add(TRdxState, IFluxState);
  Result.InjectProp('Data', IProps);
end;

function TReg.RegisterFunc(const AClass: TClass): TDIReg;
begin
  Result := DIC.Add(AClass, IFluxFunc, AClass.ClassName);
end;

function TReg.RegisterStore: TDIReg;
begin
  Result := DIC.Add(TRdxStore, IFluxStore, '', ckSingle);
  Result.InjectProp('State', IFluxState);
  Result.InjectProp('Dispatcher', IFluxDispatcher);
  Result.InjectProp('Data', IFluxData);
end;

function TReg.RegisterData: TDIReg;
begin
  Result := DIC.Add(TRdxData, IFluxData);
  Result.InjectProp('Factory', IDIFactory);
end;

function TReg.RegisterStoreConnector: TDIReg;
begin
  Result := DIC.Add(TRdxStoreConnector, IFluxStoreConnector);
  Result.InjectProp('Store', IFluxStore);
end;

procedure TReg.RegisterCommon(const ASubFuncs: array of string);
begin
  RegisterState;
  RegisterDispatcher(ASubFuncs);
  RegisterStore;
  RegisterStoreConnector;
  RegisterData;
end;

end.

