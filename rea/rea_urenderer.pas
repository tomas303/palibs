unit rea_urenderer;

{$mode objfpc}{$H+}

interface

uses
  rea_irenderer, rea_idesigncomponent, trl_ilog, trl_imetaelement, trl_idifactory,
  trl_itree;

type

  { TRenderer }

  TRenderer = class(TInterfacedObject, IRenderer)
  protected
    function Render(const AParent: IMetaElement; const AComponent: IDesignComponent): IMetaElement;
    function RenderChain(const AParent: IMetaElement; const AElement: IMetaElement): IMetaElement;
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

function TRenderer.Render(const AParent: IMetaElement;
  const AComponent: IDesignComponent): IMetaElement;
var
  mEl, mEndEl: IMetaElement;
begin
  Result := RenderChain(AParent, AComponent.Compose(AParent));
  for mEl in Result do
  begin
    mEndEl := RenderChain(Result, mEl);
    (Result as INode).ExchangeChild(mEl as INode, mEndEl as INode);
  end;
end;

function TRenderer.RenderChain(const AParent: IMetaElement; const AElement: IMetaElement): IMetaElement;
var
  mComponent: IDesignComponent;
begin
  Result := AElement;
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    Result := mComponent.Compose(Result);
  end;
end;

function TRenderer.Render(const AComponent: IDesignComponent): IMetaElement;
begin
  Result := Render(nil, AComponent);
end;

end.

