unit rea_uflux;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  trl_iprops, rea_iflux, fgl, trl_idifactory, sysutils;

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
  private
    function GetActionID: Integer;
  protected type

    { TFluxNotifierEventRec }

    TFluxNotifierEventRec = record
      Event: TFluxNotifierEvent;
      constructor Create(AEvent: TFluxNotifierEvent);
      class operator =(a,b: TFluxNotifierEventRec): Boolean;
    end;

    TEvents = specialize TFPGList<TFluxNotifierEventRec>;
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
    fActionID: Integer;
    fFactory: IDIFactory;
    fDispatcher: IFluxDispatcher;
    fEnabled: Boolean;
  published
    property ActionID: Integer read GetActionID write fActionID;
    property Factory: IDIFactory read fFactory write fFactory;
    property Dispatcher: IFluxDispatcher read fDispatcher write fDispatcher;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

{ TFluxNotifier.TFluxNotifierEventRec }

constructor TFluxNotifier.TFluxNotifierEventRec.Create(
  AEvent: TFluxNotifierEvent);
begin
  Event := AEvent;
end;

class operator TFluxNotifier.TFluxNotifierEventRec.=(a, b: TFluxNotifierEventRec
  ): Boolean;
var
  ma, mb: TMethod;
begin
  // compare just by a = b do not work, because same method with 2 different objects
  // returns true (don't know reason)
  ma := TMethod(a.Event);
  mb := TMethod(b.Event);
  Result := (ma.Code = mb.Code) and (ma.Data = mb.Data);
end;

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

function TFluxNotifier.GetActionID: Integer;
begin
  Result := fActionID;
end;

procedure TFluxNotifier.Notify;
var
  mProps: IProps;
  mAction: IFluxAction;
  mEvent: TFluxNotifierEventRec;
  m: string;
begin
  if not fEnabled then
    Exit;
  mProps := IProps(Factory.Locate(IProps));
  mProps.SetInt('ID', ActionID);
  mAction := IFluxAction(Factory.Locate(IFluxAction, '', mProps));
  for mEvent in fEvents do begin
    mEvent.Event(mAction.Props);
  end;
  m := (Dispatcher as TObject).ClassName;
  Dispatcher.FluxDispatch(mAction);
end;

procedure TFluxNotifier.Add(const AEvent: TFluxNotifierEvent);
var
  mRec: TFluxNotifierEventRec;
begin
  mRec := TFluxNotifierEventRec.Create(AEvent);
  if fEvents.IndexOf(mRec) = -1 then
    fEvents.Add(mRec);
end;

procedure TFluxNotifier.Remove(const AEvent: TFluxNotifierEvent);
var
  mIndex: integer;
begin
  mIndex := fEvents.IndexOf(TFluxNotifierEventRec.Create(AEvent));
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
  fEnabled := True;
end;

procedure TFluxNotifier.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  inherited BeforeDestruction;
end;

end.

