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


implementation

end.

