unit rea_ulauncher;

{$mode objfpc}{$H+}

interface

uses
  tal_ilauncher, forms, rea_iapp;

type

  { TReactLauncher }

  TReactLauncher = class(TInterfacedObject, ILauncher)
  protected
    // ILauncher
    procedure Launch;
  public
    procedure AfterConstruction; override;
  protected
    fReactApp: IReactApp;
  published
    property ReactApp: IReactApp read fReactApp write fReactApp;
  end;

implementation

{ TReactLauncher }

procedure TReactLauncher.Launch;
begin
  ReactApp.StartUp;
  try
    Application.Run;
  finally
    ReactApp.ShutDown;
  end;
end;

procedure TReactLauncher.AfterConstruction;
begin
  inherited AfterConstruction;
  // since mainform could be subject of dependency injection and as such could
  // be created during create of hierarchy of object, call to inititialization
  // is put here(at least on windows must be called before main form is created)
  Application.Initialize;
end;

end.

