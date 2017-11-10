unit rdx_ireg;

{$mode objfpc}{$H+}

interface

uses
  trl_dicontainer;

type
  IReg = interface
  ['{07FCB1A3-E4C3-4D73-B6D6-C5AABC8E6904}']
    function RegisterFunc(const ASubFuncs: array of TClass): TDIReg;
    function RegisterState: TDIReg;
    function RegisterStore: TDIReg;
    procedure RegisterCommon(const ASubFuncs: array of TClass);
  end;


implementation

end.

