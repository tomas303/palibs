unit tal_ureg;

{$mode objfpc}{$H+}

interface

uses
  tal_ireg, trl_dicontainer, trl_ilog, tal_uwindowlog;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterWindowLog: TDIReg;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterWindowLog: TDIReg;
begin
  Result := DIC.Add(TWindowLog, ILog, '', ckSingle);
end;

end.

