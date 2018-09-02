unit tal_ureaapp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rea_ireact, trl_uprops,
  trl_idifactory, flu_iflux, rea_iapp,
  trl_iExecutor,
  Forms,  // later remove and make Application accessible through interface
  LMessages,
  trl_imetaelementfactory;

type

  { TReactApp }

  TReactApp = class(TInterfacedObject, IReactApp)
  protected
    // IReactApp
    procedure StartUp;
    procedure ShutDown;
  protected
    procedure AppStoreChanged(const AAppState: IFluxState);
    procedure IdleHandler(Sender: TObject; var Done: Boolean);
  protected
    fFactory: IDIFactory;
    fRootComponent: IReactComponent;
    fAppStore: IFluxStore;
    fElFactory: IMetaElementFactory;
    fExecutor: IExecutor;
    fReact: IReact;
  published
    property Factory: IDIFactory read fFactory write FFactory;
    property RootComponent: IReactComponent read fRootComponent write fRootComponent;
    property AppStore: IFluxStore read fAppStore write fAppStore;
    property ElFactory: IMetaElementFactory read fElFactory write fElFactory;
    property Executor: IExecutor read fExecutor write fExecutor;
    property React: IReact read fReact write fReact;
  end;

implementation

{ TReactApp }

procedure TReactApp.StartUp;
var
  mAction: IFluxAction;
begin
  Application.AddOnIdleHandler(@IdleHandler);
  AppStore.Add(@AppStoreChanged);
  mAction := IFluxAction(Factory.Locate(IFluxAction, '', TProps.New.SetInt('ID', 0)));
  (AppStore as IFluxDispatcher).Dispatch(mAction);
  React.Render(RootComponent);
end;

procedure TReactApp.ShutDown;
begin
  Application.RemoveOnIdleHandler(@IdleHandler);
  AppStore.Remove(@AppStoreChanged);
end;

procedure TReactApp.AppStoreChanged(const AAppState: IFluxState);
begin
  React.RenderAsync(RootComponent);
end;

procedure TReactApp.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  Executor.ExecuteAll;
end;

end.

