unit tal_ureaapp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rea_ireact, trl_uprops,
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
    //fReact: IReact;
    fRootComponent: IReactComponent;
    fAppStore: IFluxStore;
    fElFactory: IMetaElementFactory;
  protected
    procedure AppStoreChanged(const AAppState: IFluxState);
  published
    property Factory: IDIFactory read fFactory write FFactory;
    //property React: IReact read fReact write fReact;
    property RootComponent: IReactComponent read fRootComponent write fRootComponent;
    property AppStore: IFluxStore read fAppStore write fAppStore;
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
  //React.Render(ElFactory.CreateElement(IReactComponentApp));

  RootComponent.Render(ElFactory.CreateElement(GUID_NULL));
  RootComponent.Bit.Render;
end;

procedure TReactApp.ShutDown;
begin
  AppStore.Remove(@AppStoreChanged);
end;

procedure TReactApp.AppStoreChanged(const AAppState: IFluxState);
begin
  // for now synchronous change, what all will be rendered will be decided by
  // react componenets itself
  //React.Rerender;
  // in some react description was that react somehow store changed nodes and rerendere only this(or those)
  RootComponent.Render(ElFactory.CreateElement(GUID_NULL));
  RootComponent.Bit.Render;
end;

end.

