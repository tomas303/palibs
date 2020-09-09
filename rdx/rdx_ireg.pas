unit rdx_ireg;

{$mode objfpc}{$H+}

interface

uses
  trl_dicontainer;

type
  IReg = interface
  ['{07FCB1A3-E4C3-4D73-B6D6-C5AABC8E6904}']
    function RegisterDispatcher(const ASubFuncs: array of string): TDIReg;

    function RegisterStateHub(const AClass: TClass; const AID: string;
      const ASubstateIDs: array of string): TDIReg;
    function RegisterState(const AClass: TClass; const AID: string): TDIReg;
    function RegisterFunc(const AClass: TClass; const AState: string): TDIReg;

    function RegisterStore: TDIReg;
    function RegisterData: TDIReg;
    function RegisterState: TDIReg;
    function RegisterStoreConnector: TDIReg;
    procedure RegisterCommon(const ASubstateIDs: array of string;
      const ASubFuncs: array of string);
  end;


implementation

end.

