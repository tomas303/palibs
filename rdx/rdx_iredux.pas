unit rdx_iredux;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, trl_iprops, flu_iflux;

type

  { IRdxState }

  IRdxState = interface
  ['{0930F255-E3FB-423E-B8BE-81109F56FDE4}']
    function Props(const AID: string): IProps;
  end;

  { IRdxFunc }

  IRdxFunc = interface
  ['{D5CD4D66-CC4B-4A5E-A206-3D2838BB6CC6}']
    function Redux(const AState: IRdxState; const AAction: IFluxAction): IRdxState;
  end;

  TRdxStoreEvent = procedure(const AAppState: IRdxState) of object;

  { IRdxStore }

  IRdxStore = interface
  ['{3E5DDFF7-63FD-4E14-A913-0A5909A55C7C}']
    procedure Add(const AEvent: TRdxStoreEvent);
    procedure Remove(const AEvent: TRdxStoreEvent);
  end;

implementation

end.

