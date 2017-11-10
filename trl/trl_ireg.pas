unit trl_ireg;

{$mode objfpc}{$H+}

interface

uses
  trl_dicontainer;

type
  IReg = interface
  ['{57660B86-1277-47D3-845B-FB81A70203D0}']
    function RegisterSysUtils: TDIReg;
    function RegisterDIOwner: TDIReg;
    function RegisterDIFactory: TDIReg;
    function RegisterInjector: TDIReg;
    function RegisterProps: TDIReg;
    procedure RegisterTreeNodes;
    procedure RegisterCommon;
  end;

implementation

end.

