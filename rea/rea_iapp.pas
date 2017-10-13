unit rea_iapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { IAppLogic }

  IReactApp = interface
  ['{7C88A40C-9356-4302-AE61-BCAC7A12FDD5}']
    procedure StartUp;
    procedure ShutDown;
  end;

implementation

end.

