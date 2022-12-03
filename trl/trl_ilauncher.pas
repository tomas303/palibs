unit trl_ilauncher;

{$mode objfpc}{$H+}

interface

uses
  trl_pubsub;

type

  ELaunchStop = class(EPubSubUnhandled);

  ILauncher = interface
  ['{7D91208A-C492-4814-B7B9-04AD990C5029}']
    procedure Launch;
  end;

implementation

end.

