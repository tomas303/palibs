unit tvl_ureg;

{$mode objfpc}{$H+}

interface

uses
  tvl_ireg, trl_dicontainer,
  tvl_itimer, tvl_utimer;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterTimer(AInterval: integer; const AName: String = ''): TDIReg;
    procedure RegisterCommon;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterTimer(AInterval: integer; const AName: String): TDIReg;
begin
  Result := DIC.Add(TTimer, ITimer, AName);
  Result.InjectProp('Interval', AInterval);
end;

procedure TReg.RegisterCommon;
begin
  RegisterTimer(1000);
end;

end.

