unit rea_urenderer;

{$mode objfpc}{$H+}

interface

uses
  rea_irenderer, rea_idesigncomponent, trl_ilog, trl_imetaelement, trl_idifactory,
  trl_itree, trl_iprops, flu_iflux, rea_ibits, trl_ireconciler;

type

  { TRenderer }

  TRenderer = class(TInterfacedObject, IRenderer)
  private
    fHeadBit: IBit;
    fHeadEl: IMetaElement;
  protected
    function GetChildren(const AElement: IMetaElement): TMetaElementArray;
    function EmptyChildren: TMetaElementArray;
    function RenderElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function Expand(const AElement: IMetaElement): IMetaElement;
  protected
    // IRenderer
    procedure Render(const AElement: IMetaElement);
  protected
    fLog: ILog;
    fFactory: IDIFactory;
    fReconciler: IReconciler;
  published
    property Log: ILog read fLog write fLog;
    property Factory: IDIFactory read fFactory write fFactory;
    property Reconciler: IReconciler read fReconciler write fReconciler;
  end;

implementation

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

function TRenderer.Expand(const AElement: IMetaElement): IMetaElement;
var
  m: string;
begin
  Result := RenderElement(AElement, AElement.Props);
  m := (Result as ILogSupport).LogInfo;
  Log.DebugLn(m);
end;

procedure TRenderer.Render(const AElement: IMetaElement);
var
  mNewEl: IMetaElement;
  mNew: IUnknown;
  mNewBit: IBit;
  m: string;
begin
  mNewEl := Expand(AElement);
  mNew := Reconciler.Reconcile(fHeadEl, fHeadBit, mNewEl);
  m := (mNew as TObject).ClassName;
  mNewBit := mNew as IBit;
  fHeadEl := mNewEl;
  fHeadBit := mNewBit;
  fHeadBit.Render;
end;

end.

