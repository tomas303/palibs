unit flu_ireg;

{$mode objfpc}{$H+}

interface

uses
  trl_dicontainer;

type
  IReg = interface
  ['{1763B6A8-FCDC-4C02-8EEF-22CEDC96B5FE}']
    function RegisterAction: TDIReg;
    function RegisterNotifier(const ADispatcher: TGuid; const AID: string = ''): TDIReg;
    function RegisterFunc(const AClass: TClass): TDIReg;
    procedure RegisterCommon;
  end;

implementation

end.

