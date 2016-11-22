unit tal_uguilauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tal_ilauncher, Forms;

type

  { TGUILauncher }

  TGUILauncher = class(TInterfacedObject, ILauncher)
  private
    fMainForm: IMainForm;
  protected
    // ILauncher
    procedure Launch;
  public
    procedure AfterConstruction; override;
  published
    property MainForm: IMainForm read fMainForm write fMainForm;
  end;

implementation

{ TGUILauncher }

procedure TGUILauncher.Launch;
begin
  MainForm.StartUp;
  try
    Application.Run;
  finally
    MainForm.ShutDown;
  end;
end;

procedure TGUILauncher.AfterConstruction;
begin
  inherited AfterConstruction;
  // since mainform could be subject of dependency injection and as such could
  // be created during create of hierarchy of object, call to inititialization
  // is put here(at least on windows must be called before main form is created)
  Application.Initialize;
end;

end.

