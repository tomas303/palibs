unit rdx_ureg;

{$mode objfpc}{$H+}

interface

uses
  rdx_ireg, trl_dicontainer, flu_iflux, rdx_ufuncdispatcher, trl_iinjector, trl_idifactory,
  rdx_ustate, trl_iprops, rdx_ustore, rdx_udata;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterDispatcher(const ASubFuncs: array of string): TDIReg;
    function RegisterStateHub(const AClass: TClass; const AID: string;
      const ASubstateIDs: array of string): TDIReg;
    function RegisterState(const AClass: TClass; const AID: string): TDIReg;
    function RegisterFunc(const AClass: TClass; const AState: string): TDIReg;

    function RegisterStore: TDIReg;
    function RegisterData: TDIReg;
    procedure RegisterCommon(const ASubstateIDs: array of string;
      const ASubFuncs: array of string);
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
  for mFunc in ASubFuncs do begin
    Result.InjectProp('AddFunc', IFluxFunc, mFunc);
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

function TReg.RegisterFunc(const AClass: TClass; const AState: string): TDIReg;
begin
  Result := DIC.Add(AClass, IFluxFunc, AClass.ClassName);
  Result.InjectProp('State', IFluxState, AState);
end;

function TReg.RegisterStore: TDIReg;
begin
  Result := DIC.Add(TRdxStore, IFluxStore, '', ckSingle);
  Result.InjectProp('State', IFluxState);
  Result.InjectProp('Dispatcher', IFluxDispatcher);
end;

function TReg.RegisterData: TDIReg;
begin
  Result := DIC.Add(TRdxData, IFluxData);
  Result.InjectProp('Factory', IDIFactory);
end;

procedure TReg.RegisterCommon(const ASubstateIDs: array of string;
  const ASubFuncs: array of string);
begin
  RegisterStateHub(TRdxState, '', ASubstateIDs);
  RegisterDispatcher(ASubFuncs);
  RegisterStore;
  RegisterData;
end;

end.

