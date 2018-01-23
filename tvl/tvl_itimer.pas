unit tvl_itimer;

{$mode objfpc}{$H+}

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
  end;

implementation

end.

