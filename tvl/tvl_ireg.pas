unit tvl_ireg;

{$mode objfpc}{$H+}

interface

uses
  trl_dicontainer;

type
  IReg = interface
  ['{E74195AF-21DD-4BE8-AAAA-2445A350CE43}']
    //function RegisterTimer(AInterval: integer): TDIReg;
    procedure RegisterCommon;
  end;

implementation

end.

