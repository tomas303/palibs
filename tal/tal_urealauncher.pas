unit tal_urealauncher;

{$mode objfpc}{$H+}

interface

uses
  trl_ilauncher, forms, trl_iExecutor, flu_iflux, Classes;

type

  { TReactLauncher }

  TReactLauncher = class(TInterfacedObject, ILauncher)
  private
    procedure KeyDownBeforeHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StartUp;
    procedure ShutDown;
  protected
    // ILauncher
    procedure Launch;
  public
    procedure AfterConstruction; override;
  protected
    fExecutor: IExecutor;
  published
    property Executor: IExecutor read fExecutor write fExecutor;
  end;

implementation

{ TReactLauncher }

procedure TReactLauncher.KeyDownBeforeHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //todo notifier for hotkeys
end;

procedure TReactLauncher.StartUp;
begin
  Application.AddOnKeyDownBeforeHandler(@KeyDownBeforeHandler);
end;

procedure TReactLauncher.ShutDown;
begin
  Application.RemoveOnKeyDownBeforeHandler(@KeyDownBeforeHandler);
  Application.Terminate;
end;

procedure TReactLauncher.Launch;
begin
  StartUp;
  try
    Executor.ExecuteLoop;
  finally
    ShutDown;
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

