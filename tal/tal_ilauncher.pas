unit tal_ilauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  ILauncher = interface
  ['{7D91208A-C492-4814-B7B9-04AD990C5029}']
    procedure Launch;
  end;

  IMainForm = interface
  ['{325D6DA1-5E5C-4F4E-97CB-164CE7FAB760}']
    procedure StartUp;
    procedure ShutDown;
  end;


implementation

end.

