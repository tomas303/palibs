unit tal_ureaapp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rea_ireact, rdx_iredux, trl_uprops,
  trl_idifactory, flu_iflux, rea_iapp;

type

  { TReactApp }

  TReactApp = class(TInterfacedObject, IReactApp)
  protected
    // IReactApp
    procedure StartUp;
    procedure ShutDown;
  protected
    fFactory: IDIFactory;
    fReact: IReact;
    fAppStore: IRdxStore;
    fElFactory: IMetaElementFactory;
  protected
    procedure AppStoreChanged(const AAppState: IRdxState);
  published
    property Factory: IDIFactory read fFactory write FFactory;
    property React: IReact read fReact write fReact;
    property AppStore: IRdxStore read fAppStore write fAppStore;
    property ElFactory: IMetaElementFactory read fElFactory write fElFactory;
  end;

implementation

{ TReactApp }

procedure TReactApp.StartUp;
var
  mAction: IFluxAction;
begin
  AppStore.Add(@AppStoreChanged);
  mAction := IFluxAction(Factory.Locate(IFluxAction, '', TProps.New.SetInt('ID', 0)));
  (AppStore as IFluxDispatcher).Dispatch(mAction);
  React.Render(ElFactory.CreateElement(IAppComposite));
end;

procedure TReactApp.ShutDown;
begin
  AppStore.Remove(@AppStoreChanged);
end;

procedure TReactApp.AppStoreChanged(const AAppState: IRdxState);
begin
  // for now synchronous change, what all will be rendered will be decided by
  // react componenets itself
  React.Rerender;
end;

end.

