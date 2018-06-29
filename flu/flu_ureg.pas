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
    procedure RegisterCommon(const ADispatcher: TGuid);
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

procedure TReg.RegisterCommon(const ADispatcher: TGuid);
begin
  RegisterAction;
  RegisterNotifier(ADispatcher);
end;

end.

