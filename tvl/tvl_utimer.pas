unit tvl_utimer;

{$mode objfpc}{$H+}

interface

uses
  //customtimer
  tvl_itimer, InterfaceBase;

//type
//
//  { TTimer }
//
//  TTimer = class(TInterfacedObject, ITimer)
//  protected
//    // ITimer
//    function GetEnabled: Boolean;
//    procedure SetEnabled(AValue: Boolean);
//    property Enabled: Boolean read GetEnabled write SetEnabled;
//  protected
//    fHandle: THandle;
//    fEnabled: Boolean;
//    procedure TimerHandler;
//    procedure KillTimer;
//    procedure UpdateTimer;
//  public
//    destructor Destroy; override;
//  protected
//    fInterval: integer;
//    fNotifier: IFluxNotifier;
//    procedure SetInterval(AValue: integer);
//    procedure SetNotifier(AValue: IFluxNotifier);
//  published
//    property Interval: integer read fInterval write SetInterval;
//    property Notifier: IFluxNotifier read fNotifier write SetNotifier;
//  end;

implementation

//{ TTimer }
//
//procedure TTimer.SetInterval(AValue: integer);
//begin
//  if fInterval = AValue then Exit;
//  fInterval := AValue;
//  UpdateTimer;
//end;
//
//procedure TTimer.SetNotifier(AValue: IFluxNotifier);
//begin
//  if fNotifier = AValue then Exit;
//  fNotifier := AValue;
//  UpdateTimer;
//end;
//
//function TTimer.GetEnabled: Boolean;
//begin
//  Result := fEnabled;
//end;
//
//procedure TTimer.SetEnabled(AValue: Boolean);
//begin
//  fEnabled := AValue;
//end;
//
//procedure TTimer.TimerHandler;
//begin
//  if Enabled and Assigned(Notifier) then
//    Notifier.Notify;
//end;
//
//procedure TTimer.KillTimer;
//begin
//  if fHandle <> 0 then
//    WidgetSet.DestroyTimer(fHandle);
//end;
//
//procedure TTimer.UpdateTimer;
//begin
//  KillTimer;
//  fHandle := WidgetSet.CreateTimer(fInterval, @TimerHandler);
//end;
//
//destructor TTimer.Destroy;
//begin
//  KillTimer;
//  inherited Destroy;
//end;

end.

