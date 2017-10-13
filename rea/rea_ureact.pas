unit rea_ureact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rea_ireact, trl_iprops, rea_ibits,
  trl_itree, trl_idifactory, trl_irttibroker,
  trl_uprops, trl_udifactory,
  trl_ilog, trl_iinjector, rdx_iredux, rea_ilayout,
  graphics, flu_iflux;

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
  public
    destructor Destroy; override;
  protected
    fNode: INode;
  published
    property Node: INode read fNode write fNode;
  end;

  { TComposite }

  TComposite = class(TInterfacedObject, IComposite)
  protected
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewProps: IProps;
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; virtual; abstract;
  protected
    // IComposite
    function CreateElement(const ASourceElement: IMetaElement): IMetaElement;
  protected
    fFactory: IDIFactory;
    fElementFactory: IMetaElementFactory;
    fMapStateToProps: IMapStateToProps;
    fLog: ILog;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property MapStateToProps: IMapStateToProps read fMapStateToProps write fMapStateToProps;
    property Log: ILog read fLog write fLog;
  end;

  { TFormComposite }

  TFormComposite = class(TComposite, IFormComposite)
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; override;
  protected
    fActionResize: integer;
  published
    property ActionResize: integer read fActionResize write fActionResize;
  end;

  { TEditComposite }

  TEditComposite = class(TComposite, IEditComposite)
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; override;
  end;

  { TEditsComposite }

  TEditsComposite = class(TComposite, IEditsComposite)
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; override;
  end;

  { TButtonComposite }

  TButtonComposite = class(TComposite, IButtonComposite)
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; override;
  protected
    fActionClick: integer;
  published
    property ActionClick: integer read fActionClick write fActionClick;
  end;

  { TButtonsComposite }

  TButtonsComposite = class(TComposite, IButtonsComposite)
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; override;
  end;

  { THeaderComposite }

  THeaderComposite = class(TComposite, IHeaderComposite)
  protected
    function ComposeElement(const AProps: IProps; const AChildren: array of IMetaElement): IMetaElement; override;
  end;

  { TReactComponent }

  TReactComponent = class(TInterfacedObject, IReactComponent, INode)
  protected
    // later remove setting from interface and made it part of on demand injection
    // in place, where react component is created
    fElement: IMetaElement;
    fComposite: IComposite;
    fBit: IBit;
  protected
    // IReactComponent
    procedure Rerender(const AUpperComponent: IReactComponent);
    //procedure AddComposite(const AComposite: IComposite);
    procedure ResetData(const AElement: IMetaElement; const AComposite: IComposite;
      const ABit: IBit);
    function GetElement: IMetaElement;
    property Element: IMetaElement read GetElement;
    function GetComposite: IComposite;
    property Composite: IComposite read GetComposite;
    function GetBit: IBit;
    property Bit: IBit read GetBit;
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
    fReactFactory: IReactFactory;
  published
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
    property Reconciliator: IReconciliator read fReconciliator write fReconciliator;
    property ReactFactory: IReactFactory read fReactFactory write fReactFactory;
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

  { TReactFactory }

  TReactFactory = class(TDIFactory, IReactFactory)
  protected
    function MakeBit(const AMetaElement: IMetaElement; const AComponent: IReactComponent): IBit;
    procedure MakeChildren(const AParentElement: INode; const AParentInstance: INode;
      const AComponent: IReactComponent);
    function GetChildrenAsArray(const AParentElement: INode): TMetaElementArray;
  protected
    // IReactFactory
    function New(const AMetaElement: IMetaElement; const AComponent: IReactComponent): IBit;
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
    fReactFactory: IReactFactory;
    fInjector: IInjector;
  published
    property Log: ILog read fLog write fLog;
    //property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property ReactFactory: IReactFactory read fReactFactory write fReactFactory;
    property Injector: IInjector read fInjector write fInjector;
  end;

  { TReact }

  TReact = class(TInterfacedObject, IReact)
  protected
    fTopBit: IBit;
    fTopElement: IMetaElement;
  protected
    //IReact
    procedure Render(const AElement: IMetaElement);
    procedure Rerender;
  protected
    fLog: ILog;
    fReactFactory: IReactFactory;
    fInjector: IInjector;
    fReconciliator: IReconciliator;
    fRootComponent: IReactComponent;
  published
    property Log: ILog read fLog write fLog;
    property ReactFactory: IReactFactory read fReactFactory write fReactFactory;
    property Injector: IInjector read fInjector write fInjector;
    property Reconciliator: IReconciliator read fReconciliator write fReconciliator;
    property RootComponent: IReactComponent read fRootComponent write fRootComponent;
  end;

implementation

{ TButtonsComposite }

function TButtonsComposite.ComposeElement(const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
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
      ElementFactory.CreateElement(IButtonComposite,
        TProps.New
        .SetStr('Caption', mButton.AsStr('Caption'))
        .SetInt('ActionClick', mButton.AsInt('ActionClick'))
        .SetInt('Place', cPlace.FixFront)
        .SetInt('MMWidth', 100)
        .SetInt('MMHeight', 22)
        ) as INode);
  end;
end;

{ TButtonComposite }

function TButtonComposite.ComposeElement(const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
begin
  if ActionClick <> 0 then
  begin
    AProps.SetIntf('ClickNotifier', NewNotifier(ActionClick));
  end;
  Result := ElementFactory.CreateElement(IButtonBit, AProps);
end;

{ TEditsComposite }

function TEditsComposite.ComposeElement(const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
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
      ElementFactory.CreateElement(IEditComposite,
        TProps.New
        .SetStr('Title', mTitle)
        .SetStr('Value', mValues[i])
        .SetInt('Place', cPlace.FixFront)
        .SetInt('MMHeight', 22)
        ) as INode);
  end;
end;

{ TEditComposite }

function TEditComposite.ComposeElement(const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
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

{ THeaderComposite }

function THeaderComposite.ComposeElement(const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, AProps);
  for mChild in AChildren do
    (Result as INode).AddChild(mChild as INode);
end;

{ TReactComponent }

procedure TReactComponent.Rerender(const AUpperComponent: IReactComponent);
var
  mNewBit: IBit;
  mNewElement: IMetaElement;
  mChildNode: INode;
  mChildComponent: IReactComponent;
begin
  //if composite.shouldupdate ... and later througs all, maybe via composites
  if Composite <> nil then
  begin
    Log.DebugLn('rerendering %s', [(Composite as TObject).ClassName]);
    mNewBit := Bit;
    mNewElement := Composite.CreateElement(Element);
    if Element <> nil then
      Log.DebugLn('ELEMENT: %s', [Element.Info]);
    if mNewElement <> nil then
     Log.DebugLn('NEW ELEMENT: %s', [mNewElement.Info]);
    if Reconciliator.Reconciliate(self, mNewBit, Element, mNewElement) then
    begin
      if Bit <> mNewBit  then begin
        fBit := mNewBit;
      end;
      Bit.Render;
      fElement := mNewElement;
    end
    else
    begin
      for mChildNode in Node do begin
        mChildComponent := mChildNode as IReactComponent;
        mChildComponent.Rerender(Self);
      end;
    end;
  end
  else
  begin
    for mChildNode in Node do begin
      mChildComponent := mChildNode as IReactComponent;
      mChildComponent.Rerender(Self);
    end;
  end;
end;

procedure TReactComponent.ResetData(const AElement: IMetaElement;
  const AComposite: IComposite; const ABit: IBit);
begin
  fElement := AElement;
  fComposite := AComposite;
  fBit := ABit;
end;

function TReactComponent.GetElement: IMetaElement;
begin
  Result := fElement;
end;

function TReactComponent.GetComposite: IComposite;
begin
  Result := fComposite;
end;

function TReactComponent.GetBit: IBit;
begin
  Result := fBit;
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

{ TReactFactory }

function TReactFactory.MakeBit(const AMetaElement: IMetaElement;
  const AComponent: IReactComponent): IBit;
var
  mNew: IUnknown;
  mComposite: IComposite;
  mElement: IMetaElement;
  mComponent: IReactComponent;
  mc: integer;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  mc:=AMetaElement.Props.Count;
  mNew := IUnknown(Container.Locate(AMetaElement.Guid, AMetaElement.TypeID, AMetaElement.Props));
  if Supports(mNew, IComposite, mComposite) then
  begin
    mComponent := IReactComponent(Container.Locate(IReactComponent));
    (AComponent as INode).AddChild(mComponent as INode);
    mElement := mComposite.CreateElement(AMetaElement);
    Result := MakeBit(mElement, mComponent);
    mComponent.ResetData(mElement, mComposite, Result);
  end
  else
  if Supports(mNew, IBit, Result) then
  begin
    MakeChildren(AMetaElement as INode, Result as INode, AComponent);
  end
  else
  begin
    raise Exception.Create('unsupported element definition');
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TReactFactory.MakeChildren(const AParentElement: INode;
  const AParentInstance: INode; const AComponent: IReactComponent);
var
  mChild: IBit;
  mChildNode: INode;
  mChildElement: IMetaElement;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  for mChildNode in AParentElement do begin
    mChildElement := mChildNode as IMetaElement;
    mChild := New(mChildElement, AComponent);
    AParentInstance.AddChild(mChild as INode);
  end;
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
end;

function TReactFactory.GetChildrenAsArray(const AParentElement: INode): TMetaElementArray;
var
  mChildNode: INode;
  mChildElement: IMetaElement;
  i: integer;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  SetLength(Result, (AParentElement as INode).Count);
  for i := 0 to (AParentElement as INode).Count - 1 do begin
    Result[i] := (AParentElement as INode).Child[i] as IMetaElement;
  end;
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
end;

function TReactFactory.New(const AMetaElement: IMetaElement;
  const AComponent: IReactComponent): IBit;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  Result := MakeBit(AMetaElement, AComponent);
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

{ TFormComposite }

function TFormComposite.ComposeElement(const AProps: IProps;
  const AChildren: array of IMetaElement): IMetaElement;
var
  mChild: IMetaElement;
begin
  if ActionResize <> 0 then
  begin
    AProps.SetIntf('ResizeNotifier', NewNotifier(ActionResize));
  end;
  Result := ElementFactory.CreateElement(IFormBit, AProps);
  for mChild in AChildren do begin
    (Result as INode).AddChild(mChild as INode);
  end;
end;

{ TComposite }

function TComposite.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '', TProps.New.SetInt('ActionID', AActionID)));
end;

function TComposite.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

function TComposite.CreateElement(const ASourceElement: IMetaElement): IMetaElement;
var
  mProps: IProps;
  mChildren: TMetaElementArray;
  i: integer;
begin
  if MapStateToProps <> nil then
  begin
    Log.DebugLn('map props %s info %s', [ClassName, ASourceElement.Props.Info]);
    mProps := MapStateToProps.Map(ASourceElement.Props);
  end
  else
  begin
    Log.DebugLn('clone props %s info %s', [ClassName, ASourceElement.Props.Info]);
    mProps := ASourceElement.Props.Clone;
  end;
  SetLength(mChildren, (ASourceElement as INode).Count);
  for i := 0 to (ASourceElement as INode).Count - 1 do begin
    mChildren[i] := (ASourceElement as INode).Child[i] as IMetaElement;
  end;
  Result := ComposeElement(mProps, mChildren);
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
  Result := False;
  if (AOldElement = nil) and (ANewElement = nil) then begin
    ABit := nil;
    Log.DebugLn('both nil');
  end else
  if (AOldElement <> nil) and (ANewElement = nil) then begin
    ABit := nil;
    Log.DebugLn(AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to nil');
    Result := True;
  end else
  if (AOldElement = nil) and (ANewElement <> nil) then begin
    ABit := ReactFactory.New(ANewElement, AComponent) as IBit;
    Log.DebugLn('from nil to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := True;
  end else
  if (AOldElement.TypeGuid <> ANewElement.TypeGuid) or (AOldElement.TypeID <> ANewElement.TypeID) then begin
    ABit := ReactFactory.New(ANewElement, AComponent) as IBit;
    Log.DebugLn('from ' + AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := True;
  end else begin
    Result := Equalize(AComponent, ABit, AOldElement, ANewElement);
  end;
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

{ TReact }

procedure TReact.Render(const AElement: IMetaElement);
var
  mNewTopBit: IBit;
  mBit: IBit;
begin
  mBit := ReactFactory.New(AElement, RootComponent);
  RootComponent.ResetData(AElement, nil, mBit);
  RootComponent.Bit.Render;
end;

procedure TReact.Rerender;
begin
  RootComponent.Rerender(nil);
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

destructor TMetaElement.Destroy;
begin
  inherited Destroy;
end;

function TMetaElement.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

end.

