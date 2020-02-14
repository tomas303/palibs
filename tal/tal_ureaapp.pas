unit tal_ureaapp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rea_ireact, trl_uprops,
  trl_idifactory, flu_iflux, rea_iapp,
  trl_iExecutor,
  trl_ilog,
  Forms,  // later remove and make Application accessible through interface
  LMessages, LCLType, LCLIntf,
  trl_imetaelementfactory,
  rea_ibrace, rea_ibits, trl_imetaelement, Classes,
  rea_idesigncomponent, rea_irenderer,
  trl_ireconciler;

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
    procedure KeyDownBeforeHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    fHeadBit: IBit;
    fHeadEl: IMetaElement;
    procedure Render;
  protected
    fLog: ILog;
    fFactory: IDIFactory;
    //fRootComponent: IReactComponent;
    fAppStore: IFluxStore;
    fElFactory: IMetaElementFactory;
    //fExecutor: IExecutor;
    //fReact: IReact;

    //fHead: IBrace;
    //fHeadProvider: IMetaElementProvider;
    fReconciler: IReconciler;
    fAppComponent: IDesignComponentApp;
    fRenderer: IRenderer;
  published
    property Log: ILog read fLog write fLog;
    property Factory: IDIFactory read fFactory write FFactory;
    //property RootComponent: IReactComponent read fRootComponent write fRootComponent;
    property AppStore: IFluxStore read fAppStore write fAppStore;
    property ElFactory: IMetaElementFactory read fElFactory write fElFactory;
    //property Executor: IExecutor read fExecutor write fExecutor;
    //property React: IReact read fReact write fReact;

    //property Head: IBrace read fHead write fHead;
    //property HeadProvider: IMetaElementProvider read fHeadProvider write fHeadProvider;
    property Reconciler: IReconciler read fReconciler write fReconciler;
    property AppComponent: IDesignComponentApp read fAppComponent write fAppComponent;
    property Renderer: IRenderer read fRenderer write fRenderer;
  end;

implementation

{ TReactApp }

procedure TReactApp.StartUp;
var
  mAction: IFluxAction;
begin
  Application.AddOnIdleHandler(@IdleHandler);
  Application.AddOnKeyDownBeforeHandler(@KeyDownBeforeHandler);
  AppStore.Add(@AppStoreChanged);
  mAction := IFluxAction(Factory.Locate(IFluxAction, '', TProps.New.SetInt('ID', 0)));
  (AppStore as IFluxDispatcher).Dispatch(mAction);
  //React.Render(RootComponent);
  //fHeadBit := Head.Refresh(HeadProvider.ProvideMetaElement);
  //fHeadBit.Render;
  Render;
end;

procedure TReactApp.ShutDown;
begin
  Application.RemoveOnIdleHandler(@IdleHandler);
  Application.RemoveOnKeyDownBeforeHandler(@KeyDownBeforeHandler);
  AppStore.Remove(@AppStoreChanged);
end;

procedure TReactApp.AppStoreChanged(const AAppState: IFluxState);
begin
  //React.RenderAsync(RootComponent);
  //fHeadBit := Head.Refresh(HeadProvider.ProvideMetaElement);
  //fHeadBit.Render;
  Render;
end;

procedure TReactApp.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  //Executor.ExecuteAll;
end;

procedure TReactApp.KeyDownBeforeHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_L) and (Shift = [ssCtrl])
  then begin
    Log.Visible := not Log.Visible;
    Render;
  end;
end;

procedure TReactApp.Render;
var
  mNewEl: IMetaElement;
  mNew: IUnknown;
  mNewBit: IBit;
  m: string;
begin
  mNewEl := Renderer.Render(AppComponent);
  //mNewBit := Reconciler.Reconcile(fHeadEl, fHeadBit, mNewEl) as IBit;
  mNew := Reconciler.Reconcile(fHeadEl, fHeadBit, mNewEl);
  m := (mNew as TObject).ClassName;
  mNewBit := mNew as IBit;
  fHeadEl := mNewEl;
  fHeadBit := mNewBit;
  fHeadBit.Render;
end;

end.

