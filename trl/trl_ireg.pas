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
    function RegisterDIFactory2: TDIReg;
    function RegisterInjector: TDIReg;
    function RegisterProps: TDIReg;
    procedure RegisterTreeNodes;
    procedure RegisterLink;
    function RegisterExecutor(const AID: string = ''): TDIReg;
    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReconciler: TDIReg;
    function RegisterNexus: TDIReg;
    function RegisterSequence(const AID: string; ACreateKind: TDIRegCreateKind = ckSingle): TDIReg;
    procedure RegisterCommon;
  end;

implementation

end.

