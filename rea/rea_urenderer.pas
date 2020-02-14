unit rea_urenderer;

{$mode objfpc}{$H+}

interface

uses
  rea_irenderer, rea_idesigncomponent, trl_ilog, trl_imetaelement, trl_idifactory,
  trl_itree, trl_iprops;

type

  { TRenderer }

  TRenderer = class(TInterfacedObject, IRenderer)
  protected
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

{ TRenderer }

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
  mNewEl, mEl: IMetaElement;
  mNode: INode;
begin
  Result := AElement;
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    mNewEl := mComponent.Compose(AParentProps.Clone);
    for mEl in Result do
      (mNewEl as INode).AddChild(mEl as INode);
    Result := mNewEl;
  end;
end;

function TRenderer.Render(const AComponent: IDesignComponent): IMetaElement;
var
  mEl: IMetaElement;
  m: string;
begin
  mEl := AComponent.Compose(IProps(Factory.Locate(IProps)));
  Result := RenderElement(mEl, mEl.Props);
  m := (Result as ILogSupport).LogInfo;
  Log.DebugLn(m);
end;

end.

