unit rea_urenderer;

{$mode objfpc}{$H+}

interface

uses
  rea_irenderer, rea_idesigncomponent, trl_ilog, trl_imetaelement, trl_idifactory,
  trl_itree, trl_iprops, rea_iflux, rea_ibits, trl_ireconciler, sysutils,
  trl_imetaelementfactory, classes, strutils;

type

  { TRenderer }

  TRenderer = class(TInterfacedObject, IRenderer)
  private
    fHeadBit: IBit;
    fHeadEl: IMetaElement;
  protected
    function GetChildren(const AElement: IMetaElement): TMetaElementArray;
    function EmptyChildren: TMetaElementArray;
    function ExpandElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function ExpandElement2(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function Expand(const AElement: IMetaElement): IMetaElement;
    procedure Info(const AElement: IMetaElement; AInfo: TStrings; ALevel: Integer);
    procedure InfoNode(const ANode: INode; AInfo: TStrings; ALevel: Integer);
  protected
    // IRenderer
    procedure Render(const AElement: IMetaElement);
  protected
    fLog: ILog;
    fFactory: IDIFactory;
    fElementFactory: IMetaElementFactory;
    fReconciler: IReconciler;
  published
    property Log: ILog read fLog write fLog;
    property Factory: IDIFactory read fFactory write fFactory;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Reconciler: IReconciler read fReconciler write fReconciler;
  end;

  { TRenderFunc }

  TRenderFunc = class(TInterfacedObject, IFluxFunc)
  private
    fID: integer;
    fComponent: IDesignComponent;
    fRenderer: IRenderer;
  protected
    procedure Execute(const AAction: IFluxAction);
    function GetID: integer;
  public
    constructor Create(AID: integer; AComponent: IDesignComponent; ARenderer: IRenderer);
  end;

implementation

{ TRenderFunc }

procedure TRenderFunc.Execute(const AAction: IFluxAction);
var
  mEl: IMetaElement;
begin
  mEl := fComponent.Compose(nil, []);
  if mEl = nil then begin
    raise exception.create('render func nil element');
  end;
  fRenderer.Render(mEl);
end;

function TRenderFunc.GetID: integer;
begin
  Result := fID;
end;

constructor TRenderFunc.Create(AID: integer; AComponent: IDesignComponent; ARenderer: IRenderer);
begin
  inherited Create;
  fID := AID;
  fComponent := AComponent;
  fRenderer := ARenderer;
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

function TRenderer.ExpandElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
var
  mEl, mEndEl, mNewEl: IMetaElement;
  mo: tobject;
  mcn: string;
begin
  Result := RenderChain(AElement, AParentProps);
  mcn := (Result as tobject).classname;
  for mEl in Result do
  begin
    //mEndEl := RenderChain(Result, mEl);
    //(Result as INode).ExchangeChild(mEl as INode, mEndEl as INode);
    mNewEl := ExpandElement(mEl, Result.Props.Clone);
    (Result as INode).ExchangeChild(mEl as INode, mNewEl as INode);
  end;
end;

function TRenderer.ExpandElement2(const AElement: IMetaElement;
  const AParentProps: IProps): IMetaElement;
var
  mEl, mNewEl: IMetaElement;
  mChildren: TMetaElementArray;
  mComponent: IDesignComponent;
begin
  for mEl in AElement do
  begin
    //mEndEl := RenderChain(Result, mEl);
    //(Result as INode).ExchangeChild(mEl as INode, mEndEl as INode);
    mNewEl := ExpandElement2(mEl, AElement.Props.Clone);
    SetLength(mChildren, Length(mChildren) + 1);
    mChildren[High(mChildren)] := mNewEl;
  end;
  Result := ElementFactory.CreateElement(AElement.Guid, AElement.Props, mChildren);
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    mNewEl := mComponent.Compose(AParentProps.Clone, GetChildren(Result));
    Result := mNewEl;
  end;
end;

function TRenderer.RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
var
  mComponent: IDesignComponent;
  mNewEl: IMetaElement;
  mChildren: TMetaElementArray;
  mc:string;
begin
  Result := AElement;
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    mChildren := GetChildren(Result);
    mNewEl := mComponent.Compose(AParentProps.Clone, mChildren);
    Result := mNewEl;
  end;

  mc := (result as tobject).ClassName;
    if pos('Design', mc) > 0 then
    Factory.CanLocateAs(Result.Guid, IDesignComponent);

end;

function TRenderer.Expand(const AElement: IMetaElement): IMetaElement;
var
  m: string;
begin
  Result := ExpandElement(AElement, AElement.Props);
  m := (Result as ILogSupport).LogInfo;
  Log.DebugLn(m);
end;

procedure TRenderer.Info(const AElement: IMetaElement; AInfo: TStrings; ALevel: Integer);
var
  mO: TObject;
  mEl: IMetaElement;
begin
  mO := IUnknown(Factory.Locate(AElement.Guid, AElement.TypeID, AElement.Props)) as tobject;
  AInfo.Add(DupeString('-', ALevel) +  mO.ClassName);
  for mEl in AElement do begin
    Info(mEl, AInfo, ALevel + 2);
  end;
end;

procedure TRenderer.InfoNode(const ANode: INode; AInfo: TStrings;
  ALevel: Integer);
var
  mN: INode;
begin
  AInfo.Add(DupeString('-', ALevel) + (ANode as TObject).ClassName);
  for mN in ANode do begin
    InfoNode(mN, AInfo, ALevel + 2);
  end;
end;

procedure TRenderer.Render(const AElement: IMetaElement);
var
  mNewEl: IMetaElement;
  mNew: IUnknown;
  mNewBit: IBit;
begin
  //mNewEl := Expand(AElement);
  mNewEl := AElement;
  mNew := Reconciler.Reconcile(fHeadEl, fHeadBit, mNewEl);
  mNewBit := mNew as IBit;
  fHeadEl := mNewEl;
  fHeadBit := mNewBit;
  fHeadBit.Render;
end;

end.

