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
    function RegisterStateHub(const AClass: TClass; const AID: string;
      const ASubstateIDs: array of string): TDIReg;
    function RegisterState(const AClass: TClass; const AID: string): TDIReg;
    function RegisterStore: TDIReg;
    procedure RegisterCommon(const ASubstateIDs: array of string;
      const ASubFuncs: array of TClass);
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

function TReg.RegisterStateHub(const AClass: TClass; const AID: string;
  const ASubstateIDs: array of string): TDIReg;
var
  mSubstateID: string;
begin
  Result := DIC.Add(AClass, IFluxState, AID);
  Result.InjectProp('ID', AID);
  Result.InjectProp('Data', IProps);
  for mSubstateID in ASubstateIDs do
    Result.InjectProp('AddState', IFluxState, mSubstateID);
end;

function TReg.RegisterState(const AClass: TClass; const AID: string): TDIReg;
begin
  Result := DIC.Add(AClass, IFluxState, AID, ckSingle);
  Result.InjectProp('ID', AID);
end;

function TReg.RegisterStore: TDIReg;
begin
  Result := DIC.Add(TRdxStore, IFluxStore, '', ckSingle);
  Result.InjectProp('State', IFluxState);
  Result.InjectProp('Func', IFluxFunc);
end;

procedure TReg.RegisterCommon(const ASubstateIDs: array of string;
  const ASubFuncs: array of TClass);
begin
  RegisterStateHub(TRdxState, '', ASubstateIDs);
  RegisterFunc(ASubFuncs);
  RegisterStore;
end;

end.

