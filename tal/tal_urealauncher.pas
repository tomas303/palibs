unit tal_urealauncher;

{$mode delphi}{$H+}

interface

uses
  trl_ilauncher, forms, trl_iExecutor, flu_iflux, Classes,
  rea_irenderer, rea_urenderer,
  tal_uwinfunc,
  rea_idesigncomponent,
  trl_udifactory, trl_iprops;

type

  { TReactLauncher }

  TReactLauncher = class(TInterfacedObject, ILauncher)
  private
    function NewProps: IProps;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    procedure KeyDownBeforeHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StartUp;
    procedure ShutDown;
  protected
    // ILauncher
    procedure Launch;
  public
    procedure AfterConstruction; override;
  protected
    fFluxDispatcher: IFluxDispatcher;
    fExecutor: IExecutor;
    fRenderer: IRenderer;
    fGUI: IDesignComponentApp;
    fFactory2: TDIFactory2;
  published
    property FluxDispatcher: IFluxDispatcher read fFluxDispatcher write fFluxDispatcher;
    property Executor: IExecutor read fExecutor write fExecutor;
    property Renderer: IRenderer read fRenderer write fRenderer;
    property GUI: IDesignComponentApp read fGUI write fGUI;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
  end;

implementation

{ TReactLauncher }

function TReactLauncher.NewProps: IProps;
begin
  Result := Factory2.Locate<IProps>;
end;

function TReactLauncher.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := Factory2.Locate<IFluxNotifier>(
    NewProps
    .SetInt(cAction.ActionID, AActionID)
    .SetIntf(cAction.Dispatcher, FluxDispatcher)
  );
end;

procedure TReactLauncher.KeyDownBeforeHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //todo notifier for hotkeys
end;

procedure TReactLauncher.StartUp;
begin
  Application.AddOnKeyDownBeforeHandler(KeyDownBeforeHandler);
  FluxDispatcher.RegisterFunc(TRenderFunc.Create(cNotifyRender, GUI, Renderer));
  NewNotifier(cNotifyRender).Notify;
  FluxDispatcher.RegisterFunc(TProcessMessagesFunc.Create(cNotifyProcessMessages, NewNotifier(cNotifyProcessMessages)));
  NewNotifier(cNotifyProcessMessages).Notify;
end;

procedure TReactLauncher.ShutDown;
begin
  Application.RemoveOnKeyDownBeforeHandler(KeyDownBeforeHandler);
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

