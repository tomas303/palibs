unit rea_urenderer;

{$mode objfpc}{$H+}

interface

uses
  rea_irenderer, rea_idesigncomponent, trl_ilog, trl_imetaelement, trl_idifactory,
  trl_itree, trl_iprops, flu_iflux, rea_ibits, trl_ireconciler;

type

  { TRenderFunc }

  TRenderFunc = class(TInterfacedObject, IFluxFunc)
  private
    fHeadBit: IBit;
    fHeadEl: IMetaElement;
    fRenderer: IRenderer;
    procedure Render;
  protected
    procedure Execute(const AAction: IFluxAction);
    function RunAsync: Boolean;
    function GetID: integer;
  protected
    fReconciler: IReconciler;
    fenderer: IRenderer;
    fAppComponent: IDesignComponentApp;
  published
    property Reconciler: IReconciler read fReconciler write fReconciler;
    property Renderer: IRenderer read fRenderer write fRenderer;
    property AppComponent: IDesignComponentApp read fAppComponent write fAppComponent;
  end;

  { TRenderer }

  TRenderer = class(TInterfacedObject, IRenderer)
  protected
    function GetChildren(const AElement: IMetaElement): TMetaElementArray;
    function EmptyChildren: TMetaElementArray;
    function RenderElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
  protected
    // IRenderer
    function Render(const AComponent: IDesignComponent): IMetaElement;
  protected
    fLog: ILog;
    fFactory: IDIFactory;
  published
    property Log: ILog read fLog write fLog;
    property Factory: IDIFactory read fFactory write fFactory;
  end;

implementation

{ TRenderFunc }

procedure TRenderFunc.Render;
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

procedure TRenderFunc.Execute(const AAction: IFluxAction);
begin
  Render;
end;

function TRenderFunc.RunAsync: Boolean;
begin
  Result := True;
end;

function TRenderFunc.GetID: integer;
begin
  Result := cFuncRender;
end;

{ TRenderer }

function TRenderer.GetChildren(const AElement: IMetaElement): TMetaElementArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, (AElement as INode).Count);
  for i := 0 to (AElement as INode).Count - 1 do
    Result[i] := (AElement as INode).Child[i] as IMetaElement;
end;

function TRenderer.EmptyChildren: TMetaElementArray;
begin
  Result := nil;
  SetLength(Result, 0);
end;

function TRenderer.RenderElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
var
  mEl, mEndEl, mNewEl: IMetaElement;
begin
  Result := RenderChain(AElement, AParentProps);
  for mEl in Result do
  begin
    //mEndEl := RenderChain(Result, mEl);
    //(Result as INode).ExchangeChild(mEl as INode, mEndEl as INode);
    mNewEl := RenderElement(mEl, Result.Props.Clone);
    (Result as INode).ExchangeChild(mEl as INode, mNewEl as INode);
  end;
end;

function TRenderer.RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
var
  mComponent: IDesignComponent;
  mNewEl: IMetaElement;
  mChildren: TMetaElementArray;
begin
  Result := AElement;
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    mChildren := GetChildren(Result);
    mNewEl := mComponent.Compose(AParentProps.Clone, mChildren);
    Result := mNewEl;
  end;
end;

function TRenderer.Render(const AComponent: IDesignComponent): IMetaElement;
var
  mEl: IMetaElement;
  m: string;
begin
  mEl := AComponent.Compose(IProps(Factory.Locate(IProps)), EmptyChildren);
  Result := RenderElement(mEl, mEl.Props);
  m := (Result as ILogSupport).LogInfo;
  Log.DebugLn(m);
end;

end.

