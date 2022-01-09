unit flu_ureg;

{$mode objfpc}{$H+}

interface

uses
  flu_ireg, trl_dicontainer, flu_iflux, flu_uflux, trl_iprops, trl_idifactory;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterAction: TDIReg;
    function RegisterNotifier(const ADispatcher: TGuid; const AID: string = ''): TDIReg;
    function RegisterFunc(const AClass: TClass): TDIReg;
    procedure RegisterCommon;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterAction: TDIReg;
begin
  Result := DIC.Add(TFluxAction, IFluxAction);
  Result.InjectProp('Props', IProps);
end;

function TReg.RegisterNotifier(const ADispatcher: TGuid; const AID: string
  ): TDIReg;
begin
  Result := DIC.Add(TFluxNotifier, IFluxNotifier);
  // asi az pri reactu mozna    mReg.InjectProp('ActionID', cResizeFunc);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Dispatcher', ADispatcher, AID);
end;

function TReg.RegisterFunc(const AClass: TClass): TDIReg;
begin
  Result := DIC.Add(AClass, IFluxFunc, AClass.ClassName);
end;

procedure TReg.RegisterCommon;
begin
  RegisterAction;
  RegisterNotifier(IFluxDispatcher);
end;

end.

