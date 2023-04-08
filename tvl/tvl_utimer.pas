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
    fInterval: integer;
  protected
    // ITimer
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    procedure Subscribe(ACallback: TTimerEvent);
    procedure Unsubscribe(ACallback: TTimerEvent);
    procedure Restart;
    function GetInterval: integer;
    procedure SetInterval(AValue: integer);
  protected
    fHandle: THandle;
    procedure TimerHandler;
    procedure KillTimer;
    procedure UpdateTimer;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Interval: integer read GetInterval write SetInterval;
  end;

implementation

{ TTimer }

procedure TTimer.SetInterval(AValue: integer);
begin
  if fInterval = AValue then Exit;
  fInterval := AValue;
  if Enabled then
    UpdateTimer;
end;

function TTimer.GetInterval: integer;
begin
  Result := fInterval;
end;

function TTimer.GetEnabled: Boolean;
begin
  Result := fHandle <> 0;
end;

procedure TTimer.SetEnabled(AValue: Boolean);
begin
  if AValue then
    UpdateTimer
  else
    KillTimer;
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
  for o in fObservers do
    o();
end;

procedure TTimer.KillTimer;
var
  h: THandle;
begin
  if fHandle <> 0 then begin
    h := fHandle;
    fHandle := 0;
    WidgetSet.DestroyTimer(h);
  end;
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

