unit tvl_itimer;

{$mode delphi}{$H+}

interface

uses
  Classes;

type

  TTimerEvent = procedure of object;

  { ITimer }

  ITimer = interface
  ['{FF523B59-BA98-4170-8B72-7643C06A0744}']
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    procedure Subscribe(ACallback: TTimerEvent);
    procedure Unsubscribe(ACallback: TTimerEvent);
    procedure Restart;
    function GetInterval: integer;
    procedure SetInterval(AValue: integer);
    property Interval: integer read GetInterval write SetInterval;
  end;

implementation

end.

