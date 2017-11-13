unit flu_iflux;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops;

type
  { IFluxAction }

  IFluxAction = interface
  ['{8BFE6073-9272-4051-A3DA-CCFBCC6526CE}']
    function GetID: integer;
    function GetProps: IProps;
    property ID: integer read GetID;
    property Props: IProps read GetProps;
  end;


  { IFluxDispatcher }

  IFluxDispatcher = interface
  ['{935D8630-D358-49BA-977B-E4BF88C804ED}']
    procedure Dispatch(const AAppAction: IFluxAction);
  end;

  TFluxNotifierEvent = procedure(const AProps: IProps) of object;

  { IFluxNotifier }

  IFluxNotifier = interface
  ['{38D6D48A-5E28-43F1-8450-4E05DB2BD750}']
    procedure Notify;
    procedure Add(const AEvent: TFluxNotifierEvent);
    procedure Remove(const AEvent: TFluxNotifierEvent);
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { IFluxState }

  IFluxState = interface
  ['{0930F255-E3FB-423E-B8BE-81109F56FDE4}']
    function Props(const AID: string): IProps;
  end;

  { IFluxFunc }

  IFluxFunc = interface
  ['{D5CD4D66-CC4B-4A5E-A206-3D2838BB6CC6}']
    function Redux(const AState: IFluxState; const AAction: IFluxAction): IFluxState;
  end;

  TFluxStoreEvent = procedure(const AAppState: IFluxState) of object;

  { IFluxStore }

  IFluxStore = interface
  ['{3E5DDFF7-63FD-4E14-A913-0A5909A55C7C}']
    procedure Add(const AEvent: TFluxStoreEvent);
    procedure Remove(const AEvent: TFluxStoreEvent);
  end;

implementation

end.

