unit flu_iflux;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, trl_igenericaccess;

type
  { IFluxAction }

  IFluxAction = interface
  ['{8BFE6073-9272-4051-A3DA-CCFBCC6526CE}']
    function GetID: integer;
    function GetProps: IProps;
    property ID: integer read GetID;
    property Props: IProps read GetProps;
  end;

  TFluxDispatchEvent = procedure(const AAppAction: IFluxAction) of object;

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



  TStatePath = string;
  TStatePaths = array of TStatePath;

  { IFluxState }

  IFluxState = interface
  ['{0930F255-E3FB-423E-B8BE-81109F56FDE4}']
    function GetID: string;
    property ID: string read GetID;
  end;

  { IFluxFunc }

  IFluxFunc = interface
  ['{D5CD4D66-CC4B-4A5E-A206-3D2838BB6CC6}']
    procedure Execute(const AAction: IFluxAction);
    function RunAsync: Boolean;
    function GetID: integer;
    property ID: integer read GetID;
  end;

  IFluxFuncReg = interface
  ['{B14EE8AB-2FD3-40AD-904B-8E6C111407F2}']
    procedure RegisterFunc(const AFunc: IFluxFunc);
  end;

  { IFluxStore }

  IFluxStore = interface
  ['{3E5DDFF7-63FD-4E14-A913-0A5909A55C7C}']
  end;

  IFluxStoreConnector = interface
  ['{FD468584-9133-4543-8B2E-3D8F312C14D1}']
  end;

  { IFluxData }

  IFluxData = interface
  ['{49ACEFF7-6973-4531-8B97-9D56F1786FA1}']
    function Exists(const AKey: string): Boolean;
    function GetData(const AKey: string): IGenericAccess;
    property Data[const AKey: string]: IGenericAccess read GetData; default;
  end;

implementation

end.

