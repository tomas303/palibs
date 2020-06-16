unit rea_udesigncomponent;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, trl_usystem, trl_imetaelement, rea_ireact, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, flu_iflux, trl_ilog, trl_igenericaccess,
  sysutils;

type

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewState(const APath: string = ''): IGenericAccessRO;
  protected
    function DoCompose(const AProps: IProps): IMetaElement; virtual; abstract;
  protected
    // IDesignComponent = interface
    function Compose(const AProps: IProps): IMetaElement;
  protected
    // INode
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure ExchangeChild(const AFromNode, AToNode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    function GetNodeEnumerator: INodeEnumerator;
    function INode.GetEnumerator = GetNodeEnumerator;
  protected
    fLog: ILog;
    fElementFactory: IMetaElementFactory;
    fFactory: IDIFactory;
    fNode: INode;
    fState: IGenericAccessRO;
  published
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Node: INode read fNode write fNode;
    property State: IGenericAccessRO read fState write fState;
  end;

  { TDesignComponentForm }

  TDesignComponentForm = class(TDesignComponent, IDesignComponentForm)
  protected
    function DoCompose(const AProps: IProps): IMetaElement; override;
  end;

  { TDesignComponentEdit }

  TDesignComponentEdit = class(TDesignComponent, IDesignComponentEdit)
  protected
    function DoCompose(const AProps: IProps): IMetaElement; override;
  end;

  { TDesignComponentButton }

  TDesignComponentButton = class(TDesignComponent, IDesignComponentButton)
  protected
    function DoCompose(const AProps: IProps): IMetaElement; override;
  end;

  { TDesignComponentHeader }

  TDesignComponentHeader = class(TDesignComponent, IDesignComponentHeader)
  protected
    function DoCompose(const AProps: IProps): IMetaElement; override;
  end;


implementation

{ TDesignComponentHeader }

function TDesignComponentHeader.DoCompose(const AProps: IProps): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.Title, cProps.MMWidth, cProps.MMHeight,
    cProps.Border, cProps.BorderColor, cProps.FontColor, cProps.Transparent]);
  Result := ElementFactory.CreateElement(IStripBit, mProps);
end;

{ TDesignComponentButton }

function TDesignComponentButton.DoCompose(const AProps: IProps): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight, cProps.Color, cProps.Text, cProps.ClickNotifier]);
  Result := ElementFactory.CreateElement(IButtonBit, mProps);
end;

{ TDesignComponentEdit }

function TDesignComponentEdit.DoCompose(const AProps: IProps): IMetaElement;
var
  mTitle: IProp;
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight]);
  Result := ElementFactory.CreateElement(IStripBit, mProps);
  mTitle := SelfProps.PropByName[cProps.Title];
  if mTitle <> nil then
    (Result as INode).AddChild(ElementFactory.CreateElement(ITextBit, NewProps.SetProp(cProps.Text, mTitle)) as INode);
  (Result as INode).AddChild(ElementFactory.CreateElement(IEditBit,
    NewProps
    .SetProp(cProps.Text, SelfProps.PropByName[cProps.Value])
    .SetProp(cProps.OnTextNotifier, SelfProps.PropByName[cProps.OnTextNotifier])
    ) as INode);
end;

{ TDesignComponentForm }

function TDesignComponentForm.DoCompose(const AProps: IProps): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Title, cProps.Layout, cProps.Color,
    cProps.SizeNotifier, cProps.MoveNotifier, cProps.ActivateNotifier]);
  if mProps.PropByName[cProps.Color] = nil then
    mProps.SetProp(AProps.PropByName[cProps.Color]);
  { depend on size notifier above - will take info from storage, not from here
  mProps.SetInt(cProps.MMWidth, MMWidth);
  mProps.SetInt(cProps.MMHeight, MMHeight);
  mProps.SetInt(cProps.MMLeft, MMLeft);
  mProps.SetInt(cProps.MMTop, MMTop);
  }

  mProps.SetInt(cProps.MMLeft, State.AsInt('Left'));
  mProps.SetInt(cProps.MMTop, State.AsInt('Top'));
  mProps.SetInt(cProps.MMWidth, State.AsInt('Width'));
  mProps.SetInt(cProps.MMHeight, State.AsInt('Height'));

  Result := ElementFactory.CreateElement(IFormBit, mProps);
end;

{ TDesignComponentMainForm }

{ TDesignComponent }

function TDesignComponent.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

function TDesignComponent.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '', NewProps.SetInt('ActionID', AActionID)));
end;

function TDesignComponent.NewState(const APath: string): IGenericAccessRO;
var
  mStore: IFluxStore;
  mProp: IProp;
begin
  mStore := IFluxStore(Factory.Locate(IFluxStore));
  mProp := (mStore as IPropFinder).Find(APath);
  if mprop = nil then
    raise Exception.Create('state isnil');
  Result := mProp.AsInterface as IGenericAccessRO;
end;

function TDesignComponent.Compose(const AProps: IProps): IMetaElement;
begin
  Result := DoCompose(AProps);
end;

procedure TDesignComponent.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TDesignComponent.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TDesignComponent.ExchangeChild(const AFromNode, AToNode: INode);
begin
  Node.ExchangeChild(AFromNode, AToNode);
end;

procedure TDesignComponent.Insert(const AIndex: integer; const ANode: INode);
begin
  Node.Insert(AIndex, ANode);
end;

procedure TDesignComponent.Delete(const AIndex: integer);
begin
  Node.Delete(AIndex);
end;

function TDesignComponent.Count: integer;
begin
  Result := Node.Count;
end;

function TDesignComponent.GetChild(const AIndex: integer): INode;
begin
  Result := Node[AIndex];
end;

function TDesignComponent.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

end.

