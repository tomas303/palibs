unit tvl_utimer;

{$mode delphi}{$H+}

interface

uses
  customtimer, fgl, sysutils,
  tvl_itimer, InterfaceBase;

type

  { TTimer }

  TTimer = class(TInterfacedObject, ITimer)
  private
    fObservers: TFPGList<TTimerEvent>;
  protected
    // ITimer
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    procedure Subscribe(ACallback: TTimerEvent);
    procedure Unsubscribe(ACallback: TTimerEvent);
    procedure Restart;
  protected
    fHandle: THandle;
    fEnabled: Boolean;
    procedure TimerHandler;
    procedure KillTimer;
    procedure UpdateTimer;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fInterval: integer;
    procedure SetInterval(AValue: integer);
  published
    property Interval: integer read fInterval write SetInterval;
  end;

implementation

{ TTimer }

procedure TTimer.SetInterval(AValue: integer);
begin
  if fInterval = AValue then Exit;
  fInterval := AValue;
  UpdateTimer;
end;

function TTimer.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

procedure TTimer.SetEnabled(AValue: Boolean);
begin
  fEnabled := AValue;
end;

procedure TTimer.Subscribe(ACallback: TTimerEvent);
begin
  if fObservers.IndexOf(ACallback) = -1 then
    fObservers.Add(ACallback);
end;

procedure TTimer.Unsubscribe(ACallback: TTimerEvent);
begin
  if fObservers.IndexOf(ACallback) <> -1 then
    fObservers.Remove(ACallback);
end;

procedure TTimer.Restart;
begin
  UpdateTimer;
end;

procedure TTimer.TimerHandler;
var
  o: TTimerEvent;
begin
  if Enabled then
    for o in fObservers do
      o();
end;

procedure TTimer.KillTimer;
begin
  if fHandle <> 0 then
    WidgetSet.DestroyTimer(fHandle);
end;

procedure TTimer.UpdateTimer;
begin
  KillTimer;
  fHandle := WidgetSet.CreateTimer(fInterval, TimerHandler);
end;

destructor TTimer.Destroy;
begin
  KillTimer;
  inherited Destroy;
end;

procedure TTimer.AfterConstruction;
begin
  inherited AfterConstruction;
  fObservers := TFPGList<TTimerEvent>.Create;
end;

procedure TTimer.BeforeDestruction;
begin
  FreeAndNil(fObservers);
  inherited BeforeDestruction;
end;

end.

