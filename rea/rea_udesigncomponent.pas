unit rea_udesigncomponent;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, trl_usystem, trl_imetaelement, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, flu_iflux, trl_ilog, trl_igenericaccess,
  sysutils;

type

  { TDesignComponentFunc }

  TDesignComponentFunc = class(TInterfacedObject, IFluxFunc)
  private
    fState: IGenericAccess;
  protected
    procedure Execute(const AAction: IFluxAction);
    procedure DoExecute(const AAction: IFluxAction); virtual; abstract;
  public
    constructor Create(const AState: IGenericAccess);
  end;

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
    function NewAction(AActionID: integer): IFluxAction;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewState(const APath: string): IGenericAccessRO;
    function NewState: IGenericAccessRO;
  protected
    procedure DoInitValues; virtual;
    function DoCompose(const AProps: IProps): IMetaElement; virtual; abstract;
  protected
    // IDesignComponent = interface
    function Compose(const AProps: IProps): IMetaElement;
    procedure InitValues; override;
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
    fDataPath: string;
    fStoreConnector: IFluxData;
    fFluxFuncReg: IFluxFuncReg;
  published
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Node: INode read fNode write fNode;
    property State: IGenericAccessRO read fState write fState;
    property DataPath: string read fDataPath write fDataPath;
    property StoreConnector: IFluxData read fStoreConnector write fStoreConnector;
    property FluxFuncReg: IFluxFuncReg read fFluxFuncReg write fFluxFuncReg;
  end;

  { TSizeFunc }

  TSizeFunc = class(TDesignComponentFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  { TMoveFunc }

  TMoveFunc = class(TDesignComponentFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;


  { TDesignComponentForm }

  TDesignComponentForm = class(TDesignComponent, IDesignComponentForm)
  protected
    procedure DoInitValues; override;
    function DoCompose(const AProps: IProps): IMetaElement; override;
  protected
    fSizeNotifier: IFluxNotifier;
    fMoveNotifier: IFluxNotifier;
  published
    property SizeNotifier: IFluxNotifier read fSizeNotifier write fSizeNotifier;
    property MoveNotifier: IFluxNotifier read fMoveNotifier write fMoveNotifier;
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

{ TDesignComponentFunc }

procedure TDesignComponentFunc.Execute(const AAction: IFluxAction);
begin
  DoExecute(AAction);
end;

constructor TDesignComponentFunc.Create(const AState: IGenericAccess);
begin
  inherited Create;
  fState := AState;
end;

{ TMoveFunc }

procedure TMoveFunc.DoExecute(const AAction: IFluxAction);
begin
  if AAction.ID = -2 then begin
    fState.SetInt('Left', AAction.Props.AsInt('MMLeft'));
    fState.SetInt('Top', AAction.Props.AsInt('MMTop'));
  end;
end;

{ TSizeFunc }

procedure TSizeFunc.DoExecute(const AAction: IFluxAction);
begin
  if AAction.ID = -1 then begin
    fState.SetInt('Width', AAction.Props.AsInt('MMWidth'));
    fState.SetInt('Height', AAction.Props.AsInt('MMHeight'));
  end;
end;

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

procedure TDesignComponentForm.DoInitValues;
begin
  inherited DoInitValues;
  if fSizeNotifier = nil then begin
    fSizeNotifier := NewNotifier(-1);
    FluxFuncReg.RegisterFunc(TSizeFunc.Create(fState as IGenericAccess));
  end;
  if fMoveNotifier = nil then begin
    fMoveNotifier := NewNotifier(-2);
    FluxFuncReg.RegisterFunc(TMoveFunc.Create(fState as IGenericAccess));
  end;
  if State.AsInt('Width') = 0 then
    (State as IGenericAccess).SetInt('Width', 400);
  if State.AsInt('Height') = 0 then
    (State as IGenericAccess).SetInt('Height', 200);
end;

function TDesignComponentForm.DoCompose(const AProps: IProps): IMetaElement;
var
  mProps: IProps;
  mw: integer;
begin
  mProps := SelfProps.Clone([cProps.Title, cProps.Layout, cProps.Color,
    {cProps.SizeNotifier, cProps.MoveNotifier,} cProps.ActivateNotifier]);
  if mProps.PropByName[cProps.Color] = nil then
    mProps.SetProp(AProps.PropByName[cProps.Color]);
  { depend on size notifier above - will take info from storage, not from here
  mProps.SetInt(cProps.MMWidth, MMWidth);
  mProps.SetInt(cProps.MMHeight, MMHeight);
  mProps.SetInt(cProps.MMLeft, MMLeft);
  mProps.SetInt(cProps.MMTop, MMTop);
  }

  mProps.SetIntf(cProps.SizeNotifier, SizeNotifier);
  mProps.SetIntf(cProps.MoveNotifier, MoveNotifier);


  mProps.SetInt(cProps.MMLeft, State.AsInt('Left'));
  mProps.SetInt(cProps.MMTop, State.AsInt('Top'));
  mw := State.AsInt('Left');
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

function TDesignComponent.NewAction(AActionID: integer): IFluxAction;
var
  mProps: IProps;
begin
  mProps := NewProps;
  mProps
    .SetInt('ID', AActionID)
    .SetIntf('State', State);
  Result := IFluxAction(Factory.Locate(IFluxAction, '', mProps));
end;

function TDesignComponent.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '',
    NewProps
    .SetInt('ActionID', AActionID)
  ));
end;

function TDesignComponent.NewState(const APath: string): IGenericAccessRO;
begin
  Result := StoreConnector.Data[APath] as IGenericAccessRO;
end;

function TDesignComponent.NewState: IGenericAccessRO;
begin
  Result := NewState(DataPath);
end;

procedure TDesignComponent.DoInitValues;
begin
  if fState = nil then
    fState := NewState;
end;

function TDesignComponent.Compose(const AProps: IProps): IMetaElement;
begin
  Result := DoCompose(AProps);
end;

procedure TDesignComponent.InitValues;
begin
  inherited InitValues;
  DoInitValues;
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

