unit rea_udesigncomponent;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, rea_udesigncomponentdata, trl_usystem, trl_imetaelement, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, rea_iflux, trl_ilog,
  sysutils, rea_ilayout, Graphics, LCLType, fgl, trl_isequence, rea_irenderer,
  trl_udifactory, rea_istyles, trl_pubsub;

type

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewComposeProps: IProps; virtual;
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; virtual; abstract;
    procedure AddChildren(const AElement: IMetaElement; const AChildren: TMetaElementArray);
    procedure ComposeChildren(const AParentEl: IMetaElement);
    procedure DoStartingValues; virtual;
    function StyleForeColor(const APropName: String): TColor;
    function StyleBackColor(const APropName: String): TColor;
    function StyleSuppleColor(const APropName: String): TColor;
  protected
    // IDesignComponent = interface
    function Compose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
  public
    procedure AfterConstruction; override;
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
    fID: string;
    fLog: ILog;
    fElementFactory: IMetaElementFactory;
    fFactory: IDIFactory;
    fFactory2: TDIFactory2;
    fNode: INode;
    fStyle: IStyle;
    fStyleKind: Integer;
    fPubSub: IPubSub;
  published
    property ID: string read fID write fID;
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property Node: INode read fNode write fNode;
    property Style: IStyle read fStyle write fStyle;
    property StyleKind: Integer read fStyleKind write fStyleKind;
    property PubSub: IPubSub read fPubSub write fPubSub;
  end;

  { TDesignComponentForm }

  TDesignComponentForm = class(TDesignComponent, IDesignComponentForm)
  protected
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TFormData;
    fSizeNotifier: IFluxNotifier;
    fMoveNotifier: IFluxNotifier;
    fCloseQueryNotifier: IFluxNotifier;
  published
    property Data: TFormData read fData write fData;
    property SizeNotifier: IFluxNotifier read fSizeNotifier write fSizeNotifier;
    property MoveNotifier: IFluxNotifier read fMoveNotifier write fMoveNotifier;
    property CloseQueryNotifier: IFluxNotifier read fCloseQueryNotifier write fCloseQueryNotifier;
  end;

  { TDesignComponentEdit }

  TDesignComponentEdit = class(TDesignComponent, IDesignComponentEdit)
  protected
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TEditData;
    fTextChangedNotifier: IFluxNotifier;
    fKeyDownNotifier: IFluxNotifier;
  published
    property Data: TEditData read fData write fData;
    property TextChangedNotifier: IFluxNotifier read fTextChangedNotifier write fTextChangedNotifier;
    property KeyDownNotifier: IFluxNotifier read fKeyDownNotifier write fKeyDownNotifier;
  end;

  { TDesignComponentButton }

  TDesignComponentButton = class(TDesignComponent, IDesignComponentButton)
  private
    function NewOuterProps: IProps;
  protected
    procedure DoStartingValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  { TDesignComponentStrip }

  TDesignComponentStrip = class(TDesignComponent, IDesignComponentStrip)
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  { TDesignComponentBox }

  TDesignComponentBox = class(TDesignComponent)
  private
    procedure MakeChildren(const AParentEl: IMetaElement);
  protected
    function BoxLayout: Integer; virtual; abstract;
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  { TDesignComponentHBox }

  TDesignComponentHBox = class(TDesignComponentBox, IDesignComponentHBox)
  protected
    function BoxLayout: Integer; override;
  public
    procedure AfterConstruction; override;
  end;

  { TDesignComponentVBox }

  TDesignComponentVBox = class(TDesignComponentBox, IDesignComponentVBox)
  protected
    function BoxLayout: Integer; override;
  end;

  { TDesignComponentText }

  TDesignComponentText = class(TDesignComponent, IDesignComponentText)
  protected
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  { TDesignComponentGrid }

  TDesignComponentGrid = class(TDesignComponent, IDesignComponentGrid)
  private
    function ColProps(Row, Col: integer): IProps;
    function RowProps(Row: integer): IProps;
    function MakeRow(Row: integer): TMetaElementArray;
    function MakeGrid: TMetaElementArray;
    function LaticeColProps: IProps;
    function LaticeRowProps: IProps;
    function Latice(AElements: TMetaElementArray; ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
  protected
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TGridData;
    fEdTextChangedNotifier: IFluxNotifier;
    fEdKeyDownNotifier: IFluxNotifier;
    fLaticeColColor: integer;
    fLaticeRowColor: integer;
    fLaticeColSize: integer;
    fLaticeRowSize: integer;
  published
    property Data: TGridData read fData write fData;
    property EdTextChangedNotifier: IFluxNotifier read fEdTextChangedNotifier write fEdTextChangedNotifier;
    property EdKeyDownNotifier: IFluxNotifier read fEdKeyDownNotifier write fEdKeyDownNotifier;
    property LaticeColColor: integer read fLaticeColColor write fLaticeColColor;
    property LaticeRowColor: integer read fLaticeRowColor write fLaticeRowColor;
    property LaticeColSize: integer read fLaticeColSize write fLaticeColSize;
    property LaticeRowSize: integer read fLaticeRowSize write fLaticeRowSize;
  end;

  { TDesignComponentPager }

  TDesignComponentPager = class(TDesignComponent, IDesignComponentPager)
  private
    function RenderPage(const APageElement: IMetaElement): IMetaElement;
    function MakeSwitch: IMetaElement;
    function MakeBody: IMetaElement;
  protected
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TPagerData;
    fSwitchEdge: Integer;
    fSwitchSize: Integer;
  published
    property Data: TPagerData read fData write fData;
    property SwitchEdge: Integer read fSwitchEdge write fSwitchEdge;
    property SwitchSize: Integer read fSwitchSize write fSwitchSize;
  end;

  { TDesignComponentFrame }

  TDesignComponentFrame = class(TDesignComponent, IDesignComponentFrame)
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

implementation

{ TDesignComponentHBox }

function TDesignComponentHBox.BoxLayout: Integer;
begin
  Result := cLayout.Horizontal;
end;

procedure TDesignComponentHBox.AfterConstruction;
begin
  inherited AfterConstruction;
end;

{ TDesignComponentVBox }

function TDesignComponentVBox.BoxLayout: Integer;
begin
  Result := cLayout.Vertical;
end;

{ TDesignComponentBox }

procedure TDesignComponentBox.MakeChildren(const AParentEl: IMetaElement);
var
  i: Integer;
  mNode: INode;
  mEl: IMetaElement;
  mStrip: IMetaElement;
  mStripProps: IProps;
begin
  mStripProps := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetBool(cProps.Transparent, False)
    .SetInt(cProps.Color, AParentEl.Props.AsInt(cProps.Color))
    .SetInt(cProps.FontColor, AParentEl.Props.AsInt(cProps.FontColor))
    .SetInt(cProps.BorderColor, AParentEl.Props.AsInt(cProps.BorderColor))
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.MMWidth, SelfProps.AsInt(cProps.BoxLaticeSize))
    .SetInt(cProps.MMHeight, SelfProps.AsInt(cProps.BoxLaticeSize));
  mStrip := ElementFactory.CreateElement(IStripBit, mStripProps);
  (AParentEl as INode).AddChild(mStrip as INode);
  for i := 0 to Count - 1 do begin
    mEl := (GetChild(i) as IDesignComponent).Compose(nil, nil);
    (AParentEl as INode).AddChild(mEl as INode);
    mStrip := ElementFactory.CreateElement(IStripBit, mStripProps);
    (AParentEl as INode).AddChild(mStrip as INode);
  end;
end;

function TDesignComponentBox.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetInt(cProps.Layout, BoxLayout);
end;

function TDesignComponentBox.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    MakeChildren(Result);
end;

{ TDesignComponentFrame }

function TDesignComponentFrame.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    ComposeChildren(Result);
end;

{ TDesignComponentText }

function TDesignComponentText.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result.SetStr(cProps.Text, self.SelfProps.AsStr(cProps.Text));
end;

function TDesignComponentText.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(ITextBit, NewComposeProps);
end;

{ TDesignComponentEdit }

function TDesignComponentEdit.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetStr(cProps.Text, SelfProps.AsStr(cProps.Text))
    .SetBool(cProps.Focused, SelfProps.AsBool(cProps.Focused))
    .SetBool(cProps.Flat, SelfProps.AsBool(cProps.Flat))
    .SetIntf(cEdit.PSTextChannel, SelfProps.AsIntf(cEdit.PSTextChannel))
    .SetIntf(cEdit.PSKeyDownChannel, SelfProps.AsIntf(cEdit.PSKeyDownChannel));
end;

function TDesignComponentEdit.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IEditBit, NewComposeProps);
end;

{ TDesignComponentGrid }

function TDesignComponentGrid.ColProps(Row, Col: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetStr(cProps.Text, Data[Row, Col])
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.TextColor, SelfProps.AsInt(cProps.TextColor))
    .SetInt(cProps.MMWidth, SelfProps.AsInt(cProps.ColMMWidth));
  if Row mod 2 = 1 then
    mProp := SelfProps.PropByName[cProps.ColOddColor]
  else
    mProp := SelfProps.PropByName[cProps.ColEvenColor];
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger);
end;

function TDesignComponentGrid.RowProps(Row: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.Layout, cLayout.Horizontal)
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.MMHeight, SelfProps.AsInt(cProps.RowMMHeight));
  if Row mod 2 = 1 then begin
    mProp := SelfProps.PropByName[cProps.RowOddColor];
  end else begin
    mProp := SelfProps.PropByName[cProps.RowEvenColor];
  end;
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger).SetBool(cProps.Transparent, False);
end;

function TDesignComponentGrid.MakeRow(Row: integer): TMetaElementArray;
var
  i: integer;
  mProps: IProps;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, Data.ColCount);
  for i := 0 to Data.ColCount - 1 do
    if (Row = Data.CurrentRow) and (i = Data.CurrentCol) then begin
      mProps := ColProps(Row, i);
      mProps
        .SetObject(cProps.Data, Data.EditData)
        .SetBool(cProps.Flat, True)
        .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
        //.SetIntf(cProps.TextChangedNotifier, EdTextChangedNotifier)
        //.SetIntf(cProps.KeyDownNotifier, EdKeyDownNotifier)
        ;
      Result[i] := ElementFactory.CreateElement(IDesignComponentEdit, mProps);
    end else begin
      Result[i] := ElementFactory.CreateElement(ITextBit, ColProps(Row, i));
    end;
end;

function TDesignComponentGrid.MakeGrid: TMetaElementArray;
var
  i: integer;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, Data.RowCount);
  for i := 0 to Data.RowCount - 1 do begin
    Result[i] := ElementFactory.CreateElement(IStripBit, RowProps(i), Latice(MakeRow(i), IStripBit, LaticeColProps));
  end;
  Result := Latice(Result, IStripBit, LaticeRowProps);
end;

function TDesignComponentGrid.LaticeColProps: IProps;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetBool(cProps.Transparent, False)
    .SetInt(cProps.Color, LaticeColColor)
    .SetInt(cProps.MMWidth, LaticeColSize);
end;

function TDesignComponentGrid.LaticeRowProps: IProps;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetBool(cProps.Transparent, False)
    .SetInt(cProps.Color, LaticeRowColor)
    .SetInt(cProps.MMHeight, LaticeRowSize);
end;

function TDesignComponentGrid.Latice(AElements: TMetaElementArray;
  ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Length(AElements) * 2 + 1);
  //Result[0] := ElementFactory.CreateElement(IStripBit, LaticeColProps);
  Result[0] := ElementFactory.CreateElement(ALaticeEl, ALaticeProps);
  for i := 0 to Length(AElements) - 1 do begin
    Result[i * 2 + 1] := AElements[i];
    Result[i * 2 + 2] := ElementFactory.CreateElement(ALaticeEl, ALaticeProps);
  end;
end;

function TDesignComponentGrid.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
   .SetInt(cProps.Place, cPlace.Elastic)
   .SetInt(cProps.Layout, cLayout.Vertical);
end;

function TDesignComponentGrid.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(
    IStripBit, NewComposeProps, MakeGrid);
end;

{ TDesignComponentForm }

function TDesignComponentForm.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  {
  Result
  .SetInt(cProps.MMLeft, Data.Left)
  .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
  .SetInt(cProps.MMTop, Data.Top)
  .SetInt(cProps.MMWidth, Data.Width)
  .SetInt(cProps.MMHeight, Data.Height)
  .SetIntf(cForm.ActivateNotifier, SelfProps.AsIntf(cForm.ActivateNotifier))
  .SetIntf(cForm.SizeNotifier, SizeNotifier)
  .SetIntf(cForm.MoveNotifier, MoveNotifier)
  .SetIntf(cForm.CloseQueryNotifier, CloseQueryNotifier);
  }
  Result
  .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
  .SetInt(cProps.MMLeft, SelfProps.AsInt(cProps.MMLeft))
  .SetInt(cProps.MMTop, SelfProps.AsInt(cProps.MMTop))
  .SetInt(cProps.MMWidth, SelfProps.AsInt(cProps.MMWidth))
  .SetInt(cProps.MMHeight, SelfProps.AsInt(cProps.MMHeight))
  .SetIntf(cForm.PSCloseChannel, SelfProps.AsIntf(cForm.PSCloseChannel))
  .SetIntf(cForm.PSSizeChannel, SelfProps.AsIntf(cForm.PSSizeChannel))
  .SetIntf(cForm.PSPositionChannel, SelfProps.AsIntf(cForm.PSPositionChannel))
  .SetIntf(cForm.PSActivateChannel, SelfProps.AsIntf(cForm.PSActivateChannel))

end;

function TDesignComponentForm.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IFormBit, NewComposeProps);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    ComposeChildren(Result);
end;

{ TDesignComponentPager }

function TDesignComponentPager.RenderPage(const APageElement: IMetaElement): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, [APageElement]);
end;

function TDesignComponentPager.MakeSwitch: IMetaElement;
var
  i: integer;
  mSwitch: TMetaElementArray;
  mProps: IProps;
  mText: String;
  mSwitchDC: IDesignComponent;
  mSwitchFactory: IDesignComponentPagerSwitchFactory;
  mFontDirection: Integer;
begin
  case SwitchEdge of
    cEdge.Left:
      mFontDirection := cFontDirection.VertLeft;
    cEdge.Right:
      mFontDirection := cFontDirection.VertRight;
    else
      mFontDirection := cFontDirection.Horizontal;
  end;
  SetLength(mSwitch, Count);
  for i := 0 to Count - 1 do
  begin
    mText := (GetChild(i) as TDynaObject).SelfProps.AsStr(cProps.Caption);
    mSwitchFactory := IDesignComponentPagerSwitchFactory(Factory.Locate(IDesignComponentPagerSwitchFactory));
    mSwitchDC := mSwitchFactory.New(
      NewProps
        .SetObject(cPager.PagerData, Data)
        .SetInt(cPager.PageIndex, i)
        .SetInt(cProps.FontDirection, mFontDirection)
        .SetStr(cProps.Text, mText)
    );
    mSwitch[i] := mSwitchDC.Compose(nil, nil);
  end;
  mProps := NewProps;
  case SwitchEdge of
    cEdge.Left, cEdge.Right:
      mProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.MMWidth, SwitchSize);
    cEdge.Top, cEdge.Bottom:
      mProps.SetInt(cProps.Layout, cLayout.Horizontal).SetInt(cProps.MMHeight, SwitchSize);
  end;
  mProps.SetInt(cProps.Place, cPlace.FixFront);
  Result := ElementFactory.CreateElement(IStripBit, mProps, mSwitch);
end;

function TDesignComponentPager.MakeBody: IMetaElement;
var
  mActual: IMetaElement;
begin
  mActual := (GetChild(Data.ActiveIndex) as IDesignComponent).Compose(nil, nil);
  Result := ElementFactory.CreateElement(
    IStripBit,
    NewProps
      .SetInt(cProps.Layout, cLayout.Overlay),
    [mActual]);
end;

function TDesignComponentPager.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  case SwitchEdge of
    cEdge.Left, cEdge.Right:
      Result.SetInt(cProps.Layout, cLayout.Horizontal);
    cEdge.Top, cEdge.Bottom:
      Result.SetInt(cProps.Layout, cLayout.Vertical);
  end;
end;

function TDesignComponentPager.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mChildren: TMetaElementArray;
begin
  case SwitchEdge of
    cEdge.Left, cEdge.Top:
      mChildren := [MakeSwitch, MakeBody];
    cEdge.Right, cEdge.Bottom:
      mChildren := [MakeBody,MakeSwitch];
  end;
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps, mChildren);
end;

{ TDesignComponentStrip }

function TDesignComponentStrip.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps, AChildren);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    ComposeChildren(Result);
end;

{ TDesignComponentButton }

function TDesignComponentButton.NewOuterProps: IProps;
begin
  Result := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight])
    .SetInt(cProps.Border, 4)
    .SetInt(cProps.BorderColor, clGray);
end;

procedure TDesignComponentButton.DoStartingValues;
begin
  inherited DoStartingValues;
  SelfProps.SetInt(cProps.FontDirection, cFontDirection.Horizontal);
end;

function TDesignComponentButton.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetInt(cProps.FontDirection, SelfProps.AsInt(cProps.FontDirection))
    .SetStr(cProps.Text, SelfProps.AsStr(cProps.Text))
    .SetIntf(cButton.PSClickChannel, SelfProps.AsIntf(cButton.PSClickChannel));
end;

function TDesignComponentButton.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IDesignComponentFrame,
    NewOuterProps,
    [ElementFactory.CreateElement(IButtonBit, NewComposeProps)]);
end;

{ TDesignComponent }

function TDesignComponent.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

function TDesignComponent.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '', NewProps.SetInt(cAction.ActionID, AActionID)));
end;

function TDesignComponent.NewComposeProps: IProps;
begin
  {
  Result := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.Title,
    cProps.MMWidth, cProps.MMHeight, cProps.Border])
    .SetInt(cProps.Color, StyleBackColor(cProps.Color))
    .SetInt(cProps.FontColor, StyleForeColor(cProps.FontColor))
    .SetInt(cProps.TextColor, StyleForeColor(cProps.TextColor))
    .SetInt(cProps.BorderColor, StyleSuppleColor(cProps.BorderColor));
}
  Result := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.Title,
    cProps.MMWidth, cProps.MMHeight, cProps.Border,
    cProps.Color, cProps.FontColor, cProps.TextColor, cProps.BorderColor]);
end;

procedure TDesignComponent.AddChildren(const AElement: IMetaElement;
  const AChildren: TMetaElementArray);
var
  mEl: IMetaElement;
begin
  for mEl in AChildren do
    (AElement as INode).AddChild(mEl as INode);
end;

procedure TDesignComponent.ComposeChildren(const AParentEl: IMetaElement);
var
  i: Integer;
  mNode: INode;
  mEl: IMetaElement;
begin
  for i := 0 to Count - 1 do begin
    mEl := (GetChild(i) as IDesignComponent).Compose(nil, nil);
    (AParentEl as INode).AddChild(mEl as INode);
  end;
end;

procedure TDesignComponent.DoStartingValues;
begin
  SelfProps
  .SetInt(cProps.Layout, cLayout.Horizontal)
  .SetInt(cProps.Place, cPlace.Elastic)
  .SetInt(cProps.Border, 0)
  .SetStr(cProps.Text, '')
  .SetBool(cProps.Transparent, False)
  .SetInt(cProps.Color, clFuchsia)
  .SetInt(cProps.FontColor, clOlive)
  .SetInt(cProps.BorderColor, clAqua);
end;

function TDesignComponent.StyleForeColor(const APropName: String): TColor;
begin
  if StyleKind = cStyleKind.None then
    Result := SelfProps.AsInt(APropName)
  else
    Result := Style.Get(StyleKind).ForeColor;
end;

function TDesignComponent.StyleBackColor(const APropName: String): TColor;
begin
  if StyleKind = cStyleKind.None then
    Result := SelfProps.AsInt(APropName)
  else
    Result := Style.Get(StyleKind).BackColor;
end;

function TDesignComponent.StyleSuppleColor(const APropName: String): TColor;
begin
  if StyleKind = cStyleKind.None then
    Result := SelfProps.AsInt(APropName)
  else
    Result := Style.Get(StyleKind).SuppleColor;
end;

function TDesignComponent.Compose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := DoCompose(AProps, AChildren);
end;

procedure TDesignComponent.AfterConstruction;
begin
  inherited AfterConstruction;
  DoStartingValues;
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

