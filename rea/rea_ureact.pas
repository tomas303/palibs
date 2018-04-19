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
  trl_iExecutor, Math;

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
    fTypeGuid: string;
    fTypeID: string;
    fProps: IProps;
  protected
    // IMetaElement
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
    // IReactComponentMachinery
    procedure RenderChildren(const AElement: IMetaElement);
    function Bit: IBit;
    procedure ChangeProps(const AProps: IProps);
  protected
    procedure DoRenderChildren(const AElement: IMetaElement); virtual; abstract;
    function DoGetBit: IBit; virtual; abstract;
    procedure DoChangeProps(const AProps: IProps); virtual; abstract;
  protected
    fFactory: IDIFactory;
    fLog: ILog;
    fInjector: IInjector;
    fElementFactory: IMetaElementFactory;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property Log: ILog read fLog write fLog;
    property Injector: IInjector read fInjector write fInjector;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
  end;

  { TReactComponentMachineryMiddle }

  TReactComponentMachineryMiddle = class(TReactComponentMachinery, IReactComponentMachineryMiddle)
  protected
    fComponent: IReactComponent;
    procedure DoRenderChildren(const AElement: IMetaElement); override;
    function DoGetBit: IBit; override;
    procedure DoChangeProps(const AProps: IProps); override;
  published
    property Component: IReactComponent read fComponent write fComponent;
  end;

  { TReactComponentMachineryLeaf }

  TReactComponentMachineryLeaf = class(TReactComponentMachinery, IReactComponentMachineryLeaf)
  protected type
    TMiddles = specialize TFPGInterfacedObjectList<IReactComponent>;
  protected
    fMiddles: TMiddles;
    fElement: IMetaElement;
    function GetMiddles: TMiddles;
    property Middles: TMiddles read GetMiddles;
    function IndexOfMiddle(const ABit: IBit): integer;
  protected
    procedure RemoveChild(const AParentBit: IBit; AIndex: integer);
    procedure AddChild(const AParentBit: IBit; const AChildElement: IMetaElement);
    procedure DiffChild(const AParentBit: IBit; const ANewChildElement, AOldChildElement: IMetaElement; AIndex: integer);
    procedure ProcessChildren(const ABit: IBit; const AOldElement, ANewElement: IMetaElement);
  protected
    procedure DoRenderChildren(const AElement: IMetaElement); override;
    function DoGetBit: IBit; override;
    procedure DoChangeProps(const AProps: IProps); override;
  public
    destructor Destroy; override;
  protected
    fBit: IBit;
  published
    property Bit: IBit read fBit write fBit;
  end;

  { TDynaObject }

  TDynaObject = class(TInterfacedObject)
  public
    class function newinstance : tobject;override;
  protected
    fSelfProps: IProps;
  published
    property SelfProps: IProps read fSelfProps;
  end;

  { TReactComponent }

  TReactComponent = class(TDynaObject, IReactComponent, INode)
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
    fElement: IMetaElement;
    fRenderParent: IMetaElement;
    fIsDirty: Boolean;
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; virtual; abstract;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewNotifier(const AActionID: integer; const ADispatecher: IFluxDispatcher): IFluxNotifier;
    function NewProps: IProps;
    function NewEventDispatcher(ADispatchEvent: TFluxDispatchEvent): IFluxDispatcher;
    procedure SetDirty(const AValue: Boolean);
    function NewMachinery(const AElement: IMetaElement): IReactComponentMachinery;
    function Reconciliate(const AMachinery: IReactComponentMachinery; const AOldElement, ANewElement: IMetaElement): IReactComponentMachinery;
  protected
    // IReactComponent
    procedure Render;
    procedure Render(const AParentElement: IMetaElement);
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
    fFactory: IDIFactory;
    fElementFactory: IMetaElementFactory;
    fExecutor: IExecutor;
    fReact: IReact;
  published
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
    property Factory: IDIFactory read fFactory write fFactory;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Executor: IExecutor read fExecutor write fExecutor;
    property React: IReact read fReact write fReact;
  end;

  { TReactComponentForm }

  TReactComponentForm = class(TReactComponent, IReactComponentForm)
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
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
    fMMTop: integer;
    fMMLeft: integer;
    fMMWidth: integer;
    fMMHeight: integer;
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAppAction: IFluxAction);
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentEdit }

  TReactComponentEdit = class(TReactComponent, IReactComponentEdit)
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentEdits }

  TReactComponentEdits = class(TReactComponent, IReactComponentEdits)
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentButton }

  TReactComponentButton = class(TReactComponent, IReactComponentButton)
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
  protected
    fActionClick: integer;
  published
    property ActionClick: integer read fActionClick write fActionClick;
  end;

  { TReactComponentButtons }

  TReactComponentButtons = class(TReactComponent, IReactComponentButtons)
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
  end;

  { TReactComponentHeader }

  TReactComponentHeader = class(TReactComponent, IReactComponentHeader)
  protected
    function ComposeElement(const AParentElement: IMetaElement): IMetaElement; override;
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

{ TDynaObject }

class function TDynaObject.newinstance: tobject;
begin
  Result := inherited newinstance;
  (Result as TDynaObject).fSelfProps := TProps.Create;
end;

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

procedure TReactComponentMachinery.ChangeProps(const AProps: IProps);
begin
  DoChangeProps(AProps);
end;

{ TReactComponentMachineryLeaf }

function TReactComponentMachineryLeaf.GetMiddles: TMiddles;
begin
  if fMiddles = nil then
    fMiddles := TMiddles.Create;
  Result := fMiddles;
end;

function TReactComponentMachineryLeaf.IndexOfMiddle(const ABit: IBit): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Middles.Count - 1 do
  begin
    if Middles[i].Bit = ABit then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TReactComponentMachineryLeaf.RemoveChild(const AParentBit: IBit; AIndex: integer);
var
  mInd: integer;
  mChildBit: IBit;
begin
  mChildBit := (AParentBit as INode).Child[AIndex] as IBit;
  mInd := IndexOfMiddle(mChildBit);
  if mInd <> -1 then
    Middles.Delete(mInd);
  (AParentBit as INode).RemoveChild(mChildBit as INode);
end;

procedure TReactComponentMachineryLeaf.AddChild(const AParentBit: IBit; const AChildElement: IMetaElement);
var
  mNew: IUnknown;
  mElement: IMetaElement;
  mChildEl: IMetaElement;
  mChildBit: IBit;
  mChildComponent: IReactComponent;
begin
  mNew := IUnknown(Factory.Locate(AChildElement.Guid, AChildElement.TypeID, AChildElement.Props.Clone));
  if Supports(mNew, IBit, mChildBit) then
  begin
    // what render to IBit will be use directly
    (AParentBit as INode).AddChild(mChildBit as INode);
    ProcessChildren(mChildBit, nil, AChildElement);
  end
  else
  if Supports(mNew, IReactComponent, mChildComponent) then
  begin
    // what render to IReactComponent need to be first rendered and its Result is used
    mChildComponent.Render(AChildElement);
    (AParentBit as INode).AddChild(mChildComponent.Bit as INode);
    Middles.Add(mChildComponent);
  end
  else
  begin
    raise exception.create('todo');
  end;
end;

procedure TReactComponentMachineryLeaf.DiffChild(const AParentBit: IBit;
  const ANewChildElement, AOldChildElement: IMetaElement; AIndex: integer);
var
  mDiffProps: IProps;
  mInd: integer;
  mChildBit: IBit;
begin
  mDiffProps := ANewChildElement.Props.Diff(AOldChildElement.Props);
  if mDiffProps.Count > 0 then
  begin
    mChildBit := (AParentBit as INode).Child[AIndex] as IBit;
    mInd := IndexOfMiddle(mChildBit);
    if mInd = -1 then
    begin
      Injector.Write(mChildBit as TObject, mDiffProps);
    end
    else
    begin
      Injector.Write(Middles[mInd] as TObject, mDiffProps);
    end;
  end;
end;

destructor TReactComponentMachineryLeaf.Destroy;
begin
  FreeAndNil(fMiddles);
  inherited Destroy;
end;

procedure TReactComponentMachineryLeaf.ProcessChildren(const ABit: IBit;
  const AOldElement, ANewElement: IMetaElement);
var
  mOldChildEl: IMetaElement;
  mNewChildEl: IMetaElement;
  i: Integer;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  for i := 0 to Min((AOldElement as INode).Count, (ANewElement as INode).Count) - 1 do
  begin
    mOldChildEl := (AOldElement as INode).Child[i] as IMetaElement;
    mNewChildEl := (ANewElement as INode).Child[i] as IMetaElement;
    if (mOldChildEl.TypeGuid <> mNewChildEl.TypeGuid) or (mOldChildEl.TypeID <> mNewChildEl.TypeID) then
    begin
      // change of type
      RemoveChild(ABit, i);
      AddChild(ABit, mNewChildEl);
    end
    else
    begin
      // same type - process difference
      DiffChild(ABit, mNewChildEl, mOldChildEl, i);
    end;
  end;
  if (ANewElement as INode).Count > (AOldElement as INode).Count then
  begin
    // add new ones
    for i := (AOldElement as INode).Count to (ANewElement as INode).Count - 1 do
    begin
      mNewChildEl := (ANewElement as INode).Child[i] as IMetaElement;
      AddChild(ABit, mNewChildEl);
    end;
  end
  else
  begin
    // remove old ones
    for i := (ANewElement as INode).Count to (AOldElement as INode).Count - 1 do
    begin
      RemoveChild(ABit, (ANewElement as INode).Count);
    end;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TReactComponentMachineryLeaf.DoRenderChildren(const AElement: IMetaElement);
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  if fElement = nil then
    fElement := ElementFactory.CreateElement(GUID_NULL);
  ProcessChildren(Bit, fElement, AElement);
  fElement := AElement;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReactComponentMachineryLeaf.DoGetBit: IBit;
begin
  Result := fBit;
end;

procedure TReactComponentMachineryLeaf.DoChangeProps(const AProps: IProps);
begin
  Injector.Write(Bit as TObject, AProps);
end;

{ TReactComponentMachineryMiddle }

procedure TReactComponentMachineryMiddle.DoRenderChildren(const AElement: IMetaElement);
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  Component.Render(AElement);
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReactComponentMachineryMiddle.DoGetBit: IBit;
begin
  Result := Component.Bit;
end;

procedure TReactComponentMachineryMiddle.DoChangeProps(const AProps: IProps);
begin
  Injector.Write(Component as TObject, AProps);
end;

{ TReactComponentMainForm }

procedure TReactComponentMainForm.Dispatch(const AAppAction: IFluxAction);
begin
  case AAppAction.ID of
    cActionMove:
      begin
        fMMLeft := AAppAction.Props.AsInt('MMLeft');
        fMMTop := AAppAction.Props.AsInt('MMTop');
      end;
    cActionSize:
      begin
        fMMWidth := AAppAction.Props.AsInt('MMWidth');
        fMMHeight := AAppAction.Props.AsInt('MMHeight');
        SetDirty(True);
      end;
    cActionActivate:
      begin
        if IsDirty then
          React.RenderAsync(Self);
      end;
  end;
end;

function TReactComponentMainForm.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
  mProps: IProps;
  m: string;
begin
  mProps := NewProps;
  //mProps := SelfProps.Clone;
  mProps.SetIntf('SizeNotifier', NewNotifier(cActionSize, NewEventDispatcher(@Dispatch)));
  mProps.SetIntf('MoveNotifier', NewNotifier(cActionMove, NewEventDispatcher(@Dispatch)));
  mProps.SetIntf('ActivateNotifier', NewNotifier(cActionActivate, NewEventDispatcher(@Dispatch)));
  // todo: later samewhat unify - better in global tree - first time initialize with begin
  // or saved values, from this point will be actualized by handler
  if fMMWidth > 0 then
    mProps.SetInt('MMWidth', fMMWidth);
  if fMMHeight > 0 then
    mProps.SetInt('MMHeight', fMMHeight);
  if fMMLeft > 0 then
    mProps.SetInt('MMLeft', fMMLeft);
  if fMMTop > 0 then
    mProps.SetInt('MMTop', fMMTop);
  //
  mProps.SetInt('Color', AParentElement.Props.AsInt('Color'));
  Result := ElementFactory.CreateElement(IMainFormBit, mProps);
  m := mProps.Info;
  for mChild in AParentElement do begin
    (Result as INode).AddChild(mChild as INode);
  end;
end;

{ TReactComponentButtons }

function TReactComponentButtons.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  i: integer;
  mButtons: IProps;
  mButton: IProps;
begin
  Result := ElementFactory.CreateElement(IStripBit, TProps.New.SetInt('Layout', SelfProps.AsInt('Layout')));
  mButtons := SelfProps.AsIntf('Buttons') as IProps;
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

function TReactComponentButton.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  mProps: IProps;
begin
  mProps := NewProps;
  if ActionClick <> 0 then
  begin
    mProps.SetIntf('ClickNotifier', NewNotifier(ActionClick));
  end;
  if SelfProps.AsBool('ParentColor') then
    mProps.SetInt('Color', AParentElement.Props.AsInt('Color'));
  Result := ElementFactory.CreateElement(IButtonBit, mProps);
end;

{ TReactComponentEdits }

function TReactComponentEdits.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  mTitles, mValues: TStringArray;
  mTitle: String;
  i: integer;
begin
  // maybe add support for array ... as generic? probably with new fpc sources
  mTitles := SelfProps.AsStr('Titles').Split('|');
  mValues := SelfProps.AsStr('Values').Split('|');
  Result := ElementFactory.CreateElement(IStripBit, NewProps{.SetInt('Layout', uiLayoutVertical)});
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

function TReactComponentEdit.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  mTitle, mValue: string;
begin
  // can make it aswell as property .... then there will be values from create time
  mTitle := SelfProps.AsStr('Title');
  mValue := SelfProps.AsStr('Value');
  {
  Result := ElementFactory.CreateElement(IStripBit,
    AProps.SetInt('Layout', 0),
    [ElementFactory.CreateElement(ITextBit, TProps.New.SetStr('Text', mTitle)),
     ElementFactory.CreateElement(IEditBit, TProps.New.SetStr('Text', mValue)))
     ]
   );
  }
  Result := ElementFactory.CreateElement(IStripBit, NewProps.SetInt('Layout', cLayout.Horizontal));
  if mTitle <> '' then
    (Result as INode).AddChild(ElementFactory.CreateElement(ITextBit, NewProps.SetStr('Text', mTitle)) as INode);
  (Result as INode).AddChild(ElementFactory.CreateElement(IEditBit, NewProps.SetStr('Text', mValue)) as INode);
end;

{ TReactComponentHeader }

function TReactComponentHeader.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, NewProps);
  for mChild in AParentElement do
    (Result as INode).AddChild(mChild as INode);
end;

{ TReactComponent }

procedure TReactComponent.Render;
var
  mElement: IMetaElement;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  if fRenderParent = nil then
    fRenderParent := ElementFactory.CreateElement(GUID_NULL);
  mElement := ComposeElement(fRenderParent);
  fMachinery := Reconciliate(fMachinery, fElement, mElement);
  fElement := mElement;
  fMachinery.RenderChildren(fElement);
  SetDirty(False);
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TReactComponent.Render(const AParentElement: IMetaElement);
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
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
  //Result := IProps(Factory.Locate(IProps));
  Result := SelfProps.Clone;
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

function TReactComponent.NewMachinery(const AElement: IMetaElement
  ): IReactComponentMachinery;
var
  mNew: IUnknown;
  mBit: IBit;
  mComponent: IReactComponent;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  mNew := IUnknown(Factory.Locate(AElement.Guid, AElement.TypeID, AElement.Props));
  if Supports(mNew, IBit, mBit) then
  begin
    Result := IUnknown(Factory.Locate(IReactComponentMachineryLeaf, '',
      TProps.New.SetIntf('Bit', mBit)))
      as IReactComponentMachinery;
  end
  else
  if Supports(mNew, IReactComponent, mComponent) then
  begin
    Result := IUnknown(Factory.Locate(IReactComponentMachineryMiddle, '',
      TProps.New.SetIntf('Component', mComponent)))
      as IReactComponentMachinery;
  end
  else
  begin
    raise Exception.Create('bad element declaration');
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReactComponent.Reconciliate(
  const AMachinery: IReactComponentMachinery; const AOldElement,
  ANewElement: IMetaElement): IReactComponentMachinery;
var
  mDiffProps: IProps;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  if (AOldElement = nil) and (ANewElement = nil) then begin
    Log.DebugLn('both nil');
    Result := nil;
  end else
  if (AOldElement <> nil) and (ANewElement = nil) then begin
    Log.DebugLn(AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to nil');
    Result := nil;
  end else
  if (AOldElement = nil) and (ANewElement <> nil) then begin
    Log.DebugLn('from nil to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := NewMachinery(ANewElement);
  end else
  if (AOldElement.TypeGuid <> ANewElement.TypeGuid) or (AOldElement.TypeID <> ANewElement.TypeID) then begin
    Log.DebugLn('from ' + AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := NewMachinery(ANewElement);
  end else begin
    Log.DebugLn('equalize props');
    mDiffProps := ANewElement.Props.Diff(AOldElement.Props);
    if mDiffProps.Count > 0 then
      AMachinery.ChangeProps(mDiffProps);
    Result := AMachinery;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
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

function TReactComponentForm.ComposeElement(const AParentElement: IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
  mProps: IProps;
begin
  mProps := NewProps;
  if ActionResize <> 0 then
  begin
    mProps.SetIntf('ResizeNotifier', NewNotifier(ActionResize));
  end;
  Result := ElementFactory.CreateElement(IFormBit, mProps);
  for mChild in AParentElement do begin
    (Result as INode).AddChild(mChild as INode);
  end;
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

