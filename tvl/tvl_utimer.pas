(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
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

