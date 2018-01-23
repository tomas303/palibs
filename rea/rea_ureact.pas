unit rea_ureact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rea_ireact, trl_iprops, rea_ibits,
  trl_itree, trl_idifactory, trl_irttibroker,
  trl_uprops, trl_udifactory,
  trl_ilog, trl_iinjector, rea_ilayout,
  graphics, flu_iflux,
  rea_imaps, fgl, typinfo,
  trl_iExecutor;

type

  { TMetaElementEnumerator }

  TMetaElementEnumerator = class(TInterfacedObject, IMetaElementEnumerator)
  protected
    fNodeEnumerator: INodeEnumerator;
  protected
    // IMetaElementEnumerator
    function MoveNext: Boolean;
    function GetCurrent: IMetaElement;
    property Current: IMetaElement read GetCurrent;
  public
    constructor Create(const ANodeEnumerator: INodeEnumerator);
  end;

  { TMetaElement }

  TMetaElement = class(TInterfacedObject, IMetaElement, INode)
  protected
    // IMetaElement
    fTypeGuid: string;
    fTypeID: string;
    fProps: IProps;
    function Guid: TGuid;
    function GetTypeGuid: string;
    function GetTypeID: string;
    function GetProps: IProps;
    function Info: string;
    function GetMetaElementEnumerator: IMetaElementEnumerator;
    function IMetaElement.GetEnumerator = GetMetaElementEnumerator;
  published
    property TypeGuid: string read GetTypeGuid write fTypeGuid;
    property TypeID: string read GetTypeID write fTypeID;
    property Props: IProps read GetProps write fProps;
  protected
    // INode
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    function GetNodeEnumerator: INodeEnumerator;
    function INode.GetEnumerator = GetNodeEnumerator;
  protected
    fNode: INode;
  published
    property Node: INode read fNode write fNode;
  end;

  { TReactComponentMachinery }

  TReactComponentMachinery = class(TInterfacedObject, IReactComponentMachinery)
  protected
    procedure RenderChildren(const AElement: IMetaElement);
    function Bit: IBit;
  protected
    procedure DoRenderChildren(const AElement: IMetaElement); virtual; abstract;
    function DoGetBit: IBit; virtual; abstract;
  protected
    fFactory: IDIFactory;
    fLog: ILog;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property Log: ILog read fLog write fLog;
  end;

  { TReactComponentMachineryMiddle }

  TReactComponentMachineryMiddle = class(TReactComponentMachinery, IReactComponentMachineryMiddle)
  protected
    fComponent: IReactComponent;
    procedure DoRenderChildren(const AElement: IMetaElement); override;
    function DoGetBit: IBit; override;
  published
    property Component: IReactComponent read fComponent write fComponent;
  end;

  { TReactComponentMachineryLeaf }

  TReactComponentMachineryLeaf = class(TReactComponentMachinery, IReactComponentMachineryLeaf)
  protected type
    TMiddles = specialize TFPGInterfacedObjectList<IReactComponent>;
  protected
    fMiddles: TMiddles;
    function GetMiddles: TMiddles;
    property Middles: TMiddles read GetMiddles;
    procedure ProcessChildren(const ABit: IBit; const AElement: IMetaElement);
  protected
    procedure DoRenderChildren(const AElement: IMetaElement); override;
    function DoGetBit: IBit; override;
  public
    destructor Destroy; override;
  protected
    fBit: IBit;
  published
    property Bit: IBit read fBit write fBit;
  end;

  { TReactComponent }

  TReactComponent = class(TInterfacedObject, IReactComponent, INode)
  protected type

    { TFD }

    // todo - move it to extra unit and regiter
    TFD = class(TInterfacedObject, IFluxDispatcher)
    protected
      // IFluxDispatcher
      procedure Dispatch(const AAppAction: IFluxAction);
    protected
      fOnDispatch: TFluxDispatchEvent;
    published
      property OnDispatch: TFluxDispatchEvent read fOnDispatch write fOnDispatch;
    end;

  protected
    fMachinery: IReactComponentMachinery;
    fRenderProps: IProps;
    fRenderParent: IMetaElement;
    fIsDirty: Boolean;
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; virtual; abstract;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewNotifier(const AActionID: integer; const ADispatecher: IFluxDispatcher): IFluxNotifier;
    function NewProps: IProps;
    function NewEventDispatcher(ADispatchEvent: TFluxDispatchEvent): IFluxDispatcher;
    procedure SetDirty(const AValue: Boolean);
  protected
    // IReactComponent
    procedure Render;
    procedure Render(const AProps: IProps; const AParentElement: IMetaElement);
    function GetBit: IBit;
    property Bit: IBit read GetBit;
    function IsDirty: Boolean;
  protected
    // INode
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    function GetNodeEnumerator: INodeEnumerator;
    function INode.GetEnumerator = GetNodeEnumerator;
  protected
    fLog: ILog;
    fNode: INode;
    fReconciliator: IReconciliator;
    fFactory: IDIFactory;
    fElementFactory: IMetaElementFactory;
    fExecutor: IExecutor;
    fReact: IReact;
  published
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
    property Reconciliator: IReconciliator read fReconciliator write fReconciliator;
    property Factory: IDIFactory read fFactory write fFactory;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Executor: IExecutor read fExecutor write fExecutor;
    property React: IReact read fReact write fReact;
  end;

  { TReactComponentForm }

  TReactComponentForm = class(TReactComponent, IReactComponentForm)
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  protected
    fActionResize: integer;
  published
    property ActionResize: integer read fActionResize write fActionResize;
  end;

  { TReactComponentMainForm }

  TReactComponentMainForm = class(TReactComponent, IReactComponentMainForm, IFluxDispatcher)
  protected const
    cActionSize = 1;
    cActionMove = 2;
    cActionActivate = 3;
  protected
    fTop: integer;
    fLeft: integer;
    fWidth: integer;
    fHeight: integer;
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAppAction: IFluxAction);
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentEdit }

  TReactComponentEdit = class(TReactComponent, IReactComponentEdit)
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentEdits }

  TReactComponentEdits = class(TReactComponent, IReactComponentEdits)
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentButton }

  TReactComponentButton = class(TReactComponent, IReactComponentButton)
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  protected
    fActionClick: integer;
  published
    property ActionClick: integer read fActionClick write fActionClick;
  end;

  { TReactComponentButtons }

  TReactComponentButtons = class(TReactComponent, IReactComponentButtons)
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentHeader }

  TReactComponentHeader = class(TReactComponent, IReactComponentHeader)
  protected
    function ComposeElement(const AProps: IProps; const AParentElement: IMetaElement): IMetaElement; override;
  end;


  { TMetaElementFactory }

  TMetaElementFactory = class(TDIFactory, IMetaElementFactory)
  protected
    //IMetaElementFactory
    function CreateElement(const ATypeGuid: TGuid): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AChildren: array of IMetaElement): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps;
      const AChildren: array of IMetaElement): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AChildren: array of IMetaElement): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps;
      const AChildren: array of IMetaElement): IMetaElement;
  protected
    fLog: ILog;
  published
    property Log: ILog read fLog write fLog;
  end;

  { TReconciliator }

  TReconciliator = class(TInterfacedObject, IReconciliator)
  protected
    // setup diff of props between AOldElement / ANewElement to ABit
    function EqualizeProps(var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
    // elements exists in both old and new structure or only in old structure
    function EqualizeOriginalChildren(const AComponent: IReactComponent; var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
    // elements exists only in new structure
    function EqualizeNewChildren(const AComponent: IReactComponent; var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
    function Equalize(const AComponent: IReactComponent; var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
  protected
    // IReconciliator
    function Reconciliate(const AComponent: IReactComponent; var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
  protected
    fLog: ILog;
    //fElementFactory: IMetaElementFactory;
    fInjector: IInjector;
  published
    property Log: ILog read fLog write fLog;
    //property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Injector: IInjector read fInjector write fInjector;
  end;

  { TRenderExecute }

  TRenderExecute = class(TInterfacedObject, IExecute)
  protected
    // IExecute
    procedure Execute;
  protected
    fComponent: IReactComponent;
  published
    property Component: IReactComponent read fComponent write fComponent;
  end;

  { TReact }

  TReact = class(TInterfacedObject, IReact)
  protected
    // IReact
    procedure Render(const AComponent: IReactComponent);
    procedure RenderAsync(const AComponent: IReactComponent);
  protected
    fLog: ILog;
    fExecutor: IExecutor;
    fElFactory: IMetaElementFactory;
    fFactory: IDIFactory;
  published
    property Log: ILog read fLog write fLog;
    property Executor: IExecutor read fExecutor write fExecutor;
    property ElFactory: IMetaElementFactory read fElFactory write fElFactory;
    property Factory: IDIFactory read fFactory write fFactory;
  end;

implementation

{ TReactComponent.TFD }

procedure TReactComponent.TFD.Dispatch(const AAppAction: IFluxAction);
begin
  OnDispatch(AAppAction);
end;

{ TRenderExecute }

procedure TRenderExecute.Execute;
begin
  if Component.IsDirty then
  begin
    Component.Render;
    Component.Bit.Render;
  end;
end;

{ TReact }

procedure TReact.Render(const AComponent: IReactComponent);
begin
  AComponent.Render;
  AComponent.Bit.Render;
end;

procedure TReact.RenderAsync(const AComponent: IReactComponent);
var
  mE: IExecute;
begin
  mE := IUnknown(Factory.Locate(IExecute, 'TRenderExecute',
     TProps.New.SetIntf('Component', AComponent))) as IExecute;
  Executor.Add(mE);
end;

{ TReactComponentMachinery }

procedure TReactComponentMachinery.RenderChildren(const AElement: IMetaElement);
begin
  DoRenderChildren(AElement);
end;

function TReactComponentMachinery.Bit: IBit;
begin
  Result := DoGetBit;
end;

{ TReactComponentMachineryLeaf }

function TReactComponentMachineryLeaf.GetMiddles: TMiddles;
begin
  if fMiddles = nil then
    fMiddles := TMiddles.Create;
  Result := fMiddles;
end;

destructor TReactComponentMachineryLeaf.Destroy;
begin
  FreeAndNil(fMiddles);
  inherited Destroy;
end;

procedure TReactComponentMachineryLeaf.ProcessChildren(const ABit: IBit;
  const AElement: IMetaElement);
var
  mNew: IUnknown;
  mElement: IMetaElement;
  mChildEl: IMetaElement;
  mChildBit: IBit;
  mChildComponent: IReactComponent;
begin

  // AProps - in case of bit there should be mainly props already published on BIT
  // and so setted when BIT was created
  // But when some not publish prop exist, here will be place where to process it

  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  for mChildEl in AElement do
  begin
    mNew := IUnknown(Factory.Locate(mChildEl.Guid, mChildEl.TypeID, mChildEl.Props.Clone));
    if Supports(mNew, IBit, mChildBit) then
    begin
      // what render to IBit will be use directly
      (ABit as INode).AddChild(mChildBit as INode);
      ProcessChildren(mChildBit, AElement);
    end
    else
    if Supports(mNew, IReactComponent, mChildComponent) then
    begin
      // what render to IReactComponent need to be first rendered and its Result is used
      mChildComponent.Render(mChildEl.Props, AElement);
      (ABit as INode).AddChild(mChildComponent.Bit as INode);
      Middles.Add(mChildComponent);
    end
    else
    begin
      raise exception.create('todo');
    end;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TReactComponentMachineryLeaf.DoRenderChildren(const AElement: IMetaElement);
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  ProcessChildren(Bit, AElement);
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReactComponentMachineryLeaf.DoGetBit: IBit;
begin
  Result := fBit;
end;

{ TReactComponentMachineryMiddle }

procedure TReactComponentMachineryMiddle.DoRenderChildren(const AElement: IMetaElement);
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  Component.Render(AElement.Props, AElement);
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReactComponentMachineryMiddle.DoGetBit: IBit;
begin
  Result := Component.Bit;
end;

{ TReactComponentMainForm }

procedure TReactComponentMainForm.Dispatch(const AAppAction: IFluxAction);
begin
  case AAppAction.ID of
    cActionMove:
      begin
        fLeft := AAppAction.Props.AsInt('Left');
        fTop := AAppAction.Props.AsInt('Top');
      end;
    cActionSize:
      begin
        fWidth := AAppAction.Props.AsInt('Width');
        fHeight := AAppAction.Props.AsInt('Height');
        SetDirty(True);
      end;
    cActionActivate:
      begin
        if IsDirty then
          React.RenderAsync(Self);
      end;
  end;
end;

function TReactComponentMainForm.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
begin
  AProps.SetIntf('SizeNotifier', NewNotifier(cActionSize, NewEventDispatcher(@Dispatch)));
  AProps.SetIntf('MoveNotifier', NewNotifier(cActionMove, NewEventDispatcher(@Dispatch)));
  AProps.SetIntf('ActivateNotifier', NewNotifier(cActionActivate, NewEventDispatcher(@Dispatch)));

  if fWidth > 0 then
    AProps.SetInt('Width', fWidth);
  if fHeight > 0 then
    AProps.SetInt('Height', fHeight);

  if fLeft > 0 then
    AProps.SetInt('Left', fLeft);
  if fTop > 0 then
    AProps.SetInt('Top', fTop);

  AProps.SetInt('Color', AParentElement.Props.AsInt('Color'));


  Result := ElementFactory.CreateElement(IMainFormBit, AProps);
  for mChild in AParentElement do begin
    (Result as INode).AddChild(mChild as INode);
  end;
end;

{ TReactComponentButtons }

function TReactComponentButtons.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
var
  i: integer;
  mButtons: IProps;
  mButton: IProps;
begin
  Result := ElementFactory.CreateElement(IStripBit, TProps.New.SetInt('Layout', AProps.AsInt('Layout')));
  mButtons := AProps.AsIntf('Buttons') as IProps;
  for i := 0 to mButtons.Count - 1 do
  begin
    mButton := mButtons.AsIntf(i) as IProps;
    (Result as INode).AddChild(
      ElementFactory.CreateElement(IReactComponentButton,
        TProps.New
        .SetStr('Caption', mButton.AsStr('Caption'))
        .SetInt('ActionClick', mButton.AsInt('ActionClick'))
        .SetInt('Place', cPlace.FixFront)
        .SetInt('MMWidth', 100)
        .SetInt('MMHeight', 22)
        ) as INode);
  end;
end;

{ TReactComponentButton }

function TReactComponentButton.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
begin
  if ActionClick <> 0 then
  begin
    AProps.SetIntf('ClickNotifier', NewNotifier(ActionClick));
  end;
  if AProps.AsBool('ParentColor') then
    AProps.SetInt('Color', AParentElement.Props.AsInt('Color'));
  Result := ElementFactory.CreateElement(IButtonBit, AProps);
end;

{ TReactComponentEdits }

function TReactComponentEdits.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
var
  mTitles, mValues: TStringArray;
  mTitle: String;
  i: integer;
begin
  // maybe add support for array ... as generic? probably with new fpc sources
  mTitles := AProps.AsStr('Titles').Split('|');
  mValues := AProps.AsStr('Values').Split('|');
  Result := ElementFactory.CreateElement(IStripBit, AProps{.SetInt('Layout', uiLayoutVertical)});
  for i := 0 to High(mValues) do begin
    if i > High(mTitles) then
      mTitle := ''
    else
      mTitle := mTitles[i];
    (Result as INode).AddChild(
      ElementFactory.CreateElement(IReactComponentEdit,
        TProps.New
        .SetStr('Title', mTitle)
        .SetStr('Value', mValues[i])
        .SetInt('Place', cPlace.FixFront)
        .SetInt('MMHeight', 22)
        ) as INode);
  end;
end;

{ TReactComponentEdit }

function TReactComponentEdit.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
var
  mTitle, mValue: string;
begin
  // can make it aswell as property .... then there will be values from create time
  mTitle := AProps.AsStr('Title');
  mValue := AProps.AsStr('Value');
  {
  Result := ElementFactory.CreateElement(IStripBit,
    AProps.SetInt('Layout', 0),
    [ElementFactory.CreateElement(ITextBit, TProps.New.SetStr('Text', mTitle)),
     ElementFactory.CreateElement(IEditBit, TProps.New.SetStr('Text', mValue)))
     ]
   );
  }
  Result := ElementFactory.CreateElement(IStripBit, AProps.SetInt('Layout', cLayout.Horizontal));
  if mTitle <> '' then
    (Result as INode).AddChild(ElementFactory.CreateElement(ITextBit, TProps.New.SetStr('Text', mTitle)) as INode);
  (Result as INode).AddChild(ElementFactory.CreateElement(IEditBit, TProps.New.SetStr('Text', mValue)) as INode);
end;

{ TReactComponentHeader }

function TReactComponentHeader.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
  m: string;
begin

  m:=AProps.Info;

  Result := ElementFactory.CreateElement(IStripBit, AProps);
  for mChild in AParentElement do
    (Result as INode).AddChild(mChild as INode);
end;

{ TReactComponent }

procedure TReactComponent.Render;
var
  mElement: IMetaElement;
  mNew: IUnknown;
  mBit: IBit;
  mComponent: IReactComponent;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});

  if fRenderProps = nil then
    fRenderProps := TProps.Create;
  if fRenderParent = nil then
    fRenderParent := ElementFactory.CreateElement(GUID_NULL);
  mElement := ComposeElement(fRenderProps, fRenderParent);

  // tady by melo byt reconciliation - nejspis jen konkretni uzel bez deti a prebrat
  // vysledek, na zaklade nej to tady rozstrelit, zatim vzdy nove

  mNew := IUnknown(Factory.Locate(mElement.Guid, mElement.TypeID, mElement.Props));
  if Supports(mNew, IBit, mBit) then
  begin
    fMachinery := IUnknown(Factory.Locate(IReactComponentMachineryLeaf, '',
      TProps.New.SetIntf('Bit', mBit)))
      as IReactComponentMachinery;
  end
  else
  if Supports(mNew, IReactComponent, mComponent) then
  begin
    fMachinery := IUnknown(Factory.Locate(IReactComponentMachineryMiddle, '',
      TProps.New.SetIntf('Component', mComponent)))
      as IReactComponentMachinery;
  end
  else
  begin
    raise Exception.Create('bad element declaration');
  end;
  fMachinery.RenderChildren(mElement);
  SetDirty(False);
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TReactComponent.Render(const AProps: IProps; const AParentElement: IMetaElement);
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  fRenderProps := AProps.Clone;
  fRenderParent := AParentElement;
  Render;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReactComponent.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '', TProps.New.SetInt('ActionID', AActionID)));
end;

function TReactComponent.NewNotifier(const AActionID: integer;
  const ADispatecher: IFluxDispatcher): IFluxNotifier;
begin
  Result := IFluxNotifier(
    Factory.Locate(IFluxNotifier, '',
      TProps.New
      .SetInt('ActionID', AActionID)
      .SetIntf('Dispatcher', ADispatecher)
      )
    );
end;

function TReactComponent.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

function TReactComponent.NewEventDispatcher(ADispatchEvent: TFluxDispatchEvent): IFluxDispatcher;
var
  mFD: TFD;
begin
  // this is necessary, put self as IFluxDispatcher lock interfaces, even taking
  // weak reference cause problems, because FluxDispatcher is connected in BIT and
  // during connectiong / disconnection references are knocked down and so object
  // is destroyed
  mFD := TFD.Create;
  mFD.OnDispatch := ADispatchEvent;
  Result := mFD;
end;

procedure TReactComponent.SetDirty(const AValue: Boolean);
begin
  fIsDirty := AValue;
end;

function TReactComponent.GetBit: IBit;
begin
  Result := fMachinery.Bit;
end;

function TReactComponent.IsDirty: Boolean;
begin
  Result := fIsDirty;
end;

procedure TReactComponent.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TReactComponent.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TReactComponent.Insert(const AIndex: integer; const ANode: INode);
begin
  Node.Insert(AIndex, ANode);
end;

procedure TReactComponent.Delete(const AIndex: integer);
begin
  Node.Delete(AIndex);
end;

function TReactComponent.Count: integer;
begin
  Result := Node.Count;
end;

function TReactComponent.GetChild(const AIndex: integer): INode;
begin
  Result := Node.Child[AIndex];
end;

function TReactComponent.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

{ TReactComponentForm }

function TReactComponentForm.ComposeElement(const AProps: IProps;
  const AParentElement: IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
begin
  if ActionResize <> 0 then
  begin
    AProps.SetIntf('ResizeNotifier', NewNotifier(ActionResize));
  end;
  Result := ElementFactory.CreateElement(IFormBit, AProps);
  for mChild in AParentElement do begin
    (Result as INode).AddChild(mChild as INode);
  end;
end;

{ TReconciliator }

function TReconciliator.Equalize(const AComponent: IReactComponent;
  var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
var
  mRender: Boolean;
begin
  Result := EqualizeProps(ABit, AOldElement, ANewElement);
  if EqualizeOriginalChildren(AComponent, ABit, AOldElement, ANewElement) then
    Result := True;
  if EqualizeNewChildren(AComponent, ABit, AOldElement, ANewElement) then
    Result := True;
end;

function TReconciliator.EqualizeNewChildren(const AComponent: IReactComponent; var ABit: IBit;
  const AOldElement, ANewElement: IMetaElement): Boolean;
var
  i: integer;
  mNewBit: IBit;
  mNewEl: IMetaElement;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  Result := False;
  for i := (AOldElement as INode).Count to (ANewElement as INode).Count - 1 do begin
    mNewBit := nil;
    mNewEl := (ANewElement as INode).Child[i] as IMetaElement;
    Result := Reconciliate(AComponent, mNewBit, nil, mNewEl);
    if mNewBit <> nil then begin
      (ABit as INode).AddChild(mNewBit as INode);
    end;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReconciliator.EqualizeOriginalChildren(const AComponent: IReactComponent;
  var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
var
  i: integer;
  mRemoved: integer;
  mBit: IBit;
  mNewBit: IBit;
  mOldEl: IMetaElement;
  mNewEl: IMetaElement;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  Result := False;
  mRemoved := 0;
  for i := 0 to (AOldElement as INode).Count - 1 do begin
    mBit := (ABit as INode).Child[i - mRemoved] as IBit;
    mOldEl := (AOldElement as INode).Child[i] as IMetaElement;
    if i <= (ANewElement as INode).Count - 1 then begin
      mNewEl := (ANewElement as INode).Child[i] as IMetaElement;
    end
    else
      mNewEl := nil;
    mNewBit := mBit;
    Result := Reconciliate(AComponent, mNewBit, mOldEl, mNewEl);
    if mNewBit <> mBit then begin
      (ABit as INode).Delete(i - mRemoved);
      if mNewBit <> nil then begin
        (ABit as INode).Insert(i - mRemoved, mNewBit as INode);
        dec(mRemoved);
      end;
      inc(mRemoved);
    end;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReconciliator.EqualizeProps(var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
var
  mDiffProps: IProps;
  mRender: Boolean;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  Result := False;
  mDiffProps := ANewElement.Props.Diff(AOldElement.Props);
  if mDiffProps.Count > 0 then begin
    Injector.Write(ABit as TObject, mDiffProps);
    Result := True;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReconciliator.Reconciliate(const AComponent: IReactComponent;
  var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  //Result := False;
  //if (AOldElement = nil) and (ANewElement = nil) then begin
  //  ABit := nil;
  //  Log.DebugLn('both nil');
  //end else
  //if (AOldElement <> nil) and (ANewElement = nil) then begin
  //  ABit := nil;
  //  Log.DebugLn(AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to nil');
  //  Result := True;
  //end else
  //if (AOldElement = nil) and (ANewElement <> nil) then begin
  //  ABit := ReactFactory.New(ANewElement, AComponent) as IBit;
  //  Log.DebugLn('from nil to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
  //  Result := True;
  //end else
  //if (AOldElement.TypeGuid <> ANewElement.TypeGuid) or (AOldElement.TypeID <> ANewElement.TypeID) then begin
  //  ABit := ReactFactory.New(ANewElement, AComponent) as IBit;
  //  Log.DebugLn('from ' + AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
  //  Result := True;
  //end else begin
  //  Result := Equalize(AComponent, ABit, AOldElement, ANewElement);
  //end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

{ TMetaElementEnumerator }

function TMetaElementEnumerator.MoveNext: Boolean;
begin
  Result := fNodeEnumerator.MoveNext;
end;

function TMetaElementEnumerator.GetCurrent: IMetaElement;
begin
  Result := fNodeEnumerator.Current as IMetaElement;
end;

constructor TMetaElementEnumerator.Create(const ANodeEnumerator: INodeEnumerator
  );
begin
  fNodeEnumerator := ANodeEnumerator;
end;

{ TMetaElementFactory }

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid
  ): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', TProps.New, []);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const AProps: IProps): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', AProps, []);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const AChildren: array of IMetaElement): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', TProps.New, AChildren);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', AProps, AChildren);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, ATypeID, TProps.New, []);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; const AProps: IProps): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, ATypeID, AProps, []);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; const AChildren: array of IMetaElement): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, ATypeID, TProps.New, AChildren);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
  mRB: IRBData;
  mP: Pointer;
begin
  mP := Locate(IMetaElement, '',
    TProps.New
    .SetStr('TypeGuid', GUIDToString(ATypeGuid))
    .SetStr('TypeID', ATypeID)
    .SetIntf('Props', AProps));
  Result := IMetaElement(mP);
  for mChild in AChildren do begin
    (Result as INode).AddChild(mChild as INode);
  end;
end;

{ TMetaElement }

function TMetaElement.Guid: TGuid;
begin
  Result := StringToGUID(fTypeGuid);
end;

function TMetaElement.GetTypeGuid: string;
begin
  Result := fTypeGuid;
end;

function TMetaElement.GetTypeID: string;
begin
  Result := fTypeID;
end;

function TMetaElement.GetProps: IProps;
begin
  Result := fProps;
end;

function TMetaElement.Info: string;
var
  mChild: INode;
  mChildEl: IMetaElement;
begin
  Result := TypeGuid + '(' + TypeID + ')';
  for mChild in Node do
  begin
    mChildEl := mChild as IMetaElement;
    Result := Result + LineEnding + '->' +  mChildEl.Info;
  end;
end;

function TMetaElement.GetMetaElementEnumerator: IMetaElementEnumerator;
begin
  Result := TMetaElementEnumerator.Create(GetNodeEnumerator);
end;

procedure TMetaElement.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TMetaElement.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TMetaElement.Insert(const AIndex: integer; const ANode: INode);
begin
  Node.Insert(AIndex, ANode);
end;

procedure TMetaElement.Delete(const AIndex: integer);
begin
  Node.Delete(AIndex);
end;

function TMetaElement.Count: integer;
begin
  Result := Node.Count;
end;

function TMetaElement.GetChild(const AIndex: integer): INode;
begin
  Result := Node[AIndex];
end;

function TMetaElement.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

end.

