unit flu_uflux;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, flu_iflux, fgl, trl_idifactory, sysutils;

type

  { TFluxAction }

  TFluxAction = class(TInterfacedObject, IFluxAction)
  protected
    fID: integer;
    fProps: IProps;
  protected
    //IFluxAction
    function GetID: integer;
    function GetProps: IProps;
  published
    property ID: integer read GetID write fID;
    property Props: IProps read GetProps write fProps;
  end;

  { TFluxNotifier }

  {
    Props are collected via notifying, action is generated based on it and
    dispatch by dispatcher
    So add data means add notifier which add to AProps of notify event
  }
  TFluxNotifier = class(TInterfacedObject, IFluxNotifier)
  protected type
    TEvents = specialize TFPGList<TFluxNotifierEvent>;
  protected
    fEvents: TEvents;
  protected
    //IFluxNotifier
    procedure Notify;
    procedure Add(const AEvent: TFluxNotifierEvent);
    procedure Remove(const AEvent: TFluxNotifierEvent);
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fActionID: integer;
    fFactory: IDIFactory;
    fDispatcher: IFluxDispatcher;
    fEnabled: Boolean;
  published
    property ActionID: integer read fActionID write fActionID;
    property Factory: IDIFactory read fFactory write fFactory;
    property Dispatcher: IFluxDispatcher read fDispatcher write fDispatcher;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

{ TFluxAction }

function TFluxAction.GetID: integer;
begin
  Result := fID;
end;

function TFluxAction.GetProps: IProps;
begin
  Result := fProps;
end;

{ TFluxNotifier }

procedure TFluxNotifier.Notify;
var
  mProps: IProps;
  mAction: IFluxAction;
  mEvent: TFluxNotifierEvent;
  m: string;
begin
  if not fEnabled then
    Exit;
  mProps := IProps(Factory.Locate(IProps));
  mProps.SetInt('ID', ActionID);
  mAction := IFluxAction(Factory.Locate(IFluxAction, '', mProps));
  for mEvent in fEvents do begin
    mEvent(mAction.Props);
  end;
  m := (Dispatcher as TObject).ClassName;
  Dispatcher.Dispatch(mAction);
end;

procedure TFluxNotifier.Add(const AEvent: TFluxNotifierEvent);
begin
  if fEvents.IndexOf(AEvent) = -1 then
    fEvents.Add(AEvent);
end;

procedure TFluxNotifier.Remove(const AEvent: TFluxNotifierEvent);
var
  mIndex: integer;
begin
  mIndex := fEvents.IndexOf(AEvent);
  if mIndex <> -1 then
    fEvents.Delete(mIndex);
end;

function TFluxNotifier.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

procedure TFluxNotifier.SetEnabled(AValue: Boolean);
begin
  fEnabled := AValue;
end;

procedure TFluxNotifier.AfterConstruction;
begin
  inherited AfterConstruction;
  fEvents := TEvents.Create;
end;

procedure TFluxNotifier.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  inherited BeforeDestruction;
end;

end.

