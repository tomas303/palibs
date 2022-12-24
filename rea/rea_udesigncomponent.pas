unit rea_udesigncomponent;

{$mode delphi}{$H+}

interface

uses
  rea_idesigncomponent, trl_usystem, trl_imetaelement, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, trl_ilog,
  sysutils, rea_ilayout, Graphics, LCLType, fgl, trl_isequence, rea_irenderer,
  trl_udifactory, trl_pubsub;

type

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
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
    fPubSub: IPubSub;
  published
    property ID: string read fID write fID;
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property Node: INode read fNode write fNode;
    property PubSub: IPubSub read fPubSub write fPubSub;
  end;

  { TDesignComponentForm }

  TDesignComponentForm = class(TDesignComponent, IDesignComponentForm)
  private
    fPSSizeChannel: IPSSizeChannel;
    procedure PSSizeChannelObserver(const AData: TSizeData);
    function PSSizeChannel: IPSSizeChannel;
  private
    fPSPositionChannel: IPSPositionChannel;
    procedure PSPositionChannelObserver(const AData: TPositionData);
    function PSPositionChannel: IPSPositionChannel;
  private
    fPSCloseChannel: IPSCloseChannel;
    function PSCloseChannel: IPSCloseChannel;
  private
    fPSActivateChannel: IPSActivateChannel;
    function PSActivateChannel: IPSActivateChannel;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fPSGUIChannel: IPSGUIChannel;
    fMMLeft: Integer;
    fMMTop: Integer;
    fMMWidth: Integer;
    fMMHeight: Integer;
  published
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
    property MMLeft: Integer read fMMLeft write fMMLeft;
    property MMTop: Integer read fMMTop write fMMTop;
    property MMWidth: Integer read fMMWidth write fMMWidth;
    property MMHeight: Integer read fMMHeight write fMMHeight;
  end;

  { TDesignComponentEdit }

  TDesignComponentEdit = class(TDesignComponent, IDesignComponentEdit)
  private
    fPSTextChannel: IPSTextChannel;
    procedure PSTextChannelObserver(const AValue: String);
  private
    fPSKeyDownChannel: IPSKeyChannel;
  protected
    function PSTextChannel: IPSTextChannel;
    function PSKeyDownChannel: IPSKeyChannel;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fText: String;
    procedure SetText(AText: String);
  published
    property Text: String read fText write SetText;
  end;

  { TDesignComponentButton }

  TDesignComponentButton = class(TDesignComponent, IDesignComponentButton)
  private
    fPSClickChannel: IPSClickChannel;
    function PSClickChannel: IPSClickChannel;
  private
    function NewOuterProps: IProps;
  protected
    procedure InitValues; override;
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
  private type
    TMatrix = array of array of string;
  private
    fData: TMatrix;
    fCurrentRow: Integer;
    fCurrentCol: Integer;
    fEdit: IDesignComponentEdit;
    procedure PSTextChannelObserver(const AValue: String);
    procedure PSKeyDownChannelObserver(const AValue: TKeyData);
    procedure Move(AXDelta, AYDelta: Integer);
  private
    function ColProps(Row, Col: integer): IProps;
    function RowProps(Row: integer): IProps;
    function MakeRow(Row: integer): TMetaElementArray;
    function MakeGrid: TMetaElementArray;
    function LaticeColProps: IProps;
    function LaticeRowProps: IProps;
    function Latice(AElements: TMetaElementArray; ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fPSGUIChannel: IPSGUIChannel;
    fRowCount: Integer;
    fColCount: Integer;
    fLaticeColColor: integer;
    fLaticeRowColor: integer;
    fLaticeColSize: integer;
    fLaticeRowSize: integer;
    procedure SetRowCount(AValue: Integer);
    procedure SetColCount(AValue: Integer);
  published
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
    property RowCount: Integer read fRowCount write SetRowCount;
    property ColCount: Integer read fColCount write SetColCount;
    property LaticeColColor: integer read fLaticeColColor write fLaticeColColor;
    property LaticeRowColor: integer read fLaticeRowColor write fLaticeRowColor;
    property LaticeColSize: integer read fLaticeColSize write fLaticeColSize;
    property LaticeRowSize: integer read fLaticeRowSize write fLaticeRowSize;
  end;

  { TDesignComponentPager }

  TDesignComponentPager = class(TDesignComponent, IDesignComponentPager)
  private type

    { TClickLink }

    TClickLink = class
    strict private
      fIndex: Integer;
      fPSClickChannel: IPSClickChannel;
      fPSPagerChannel: IPSPagerChannel;
      procedure PSClickChannelObserver;
    public
      constructor Create(AIndex: Integer; const APSPagerChannel: IPSPagerChannel;
        const APSClickChannel: IPSClickChannel);
    end;

  private
    fIndex: Integer;
    fPSPagerChannel: IPSPagerChannel;
    fClickLinks: TFPGObjectList<TClickLink>;
    procedure PSPagerChannelObserver(const AIndex: Integer);
  private
    function RenderPage(const APageElement: IMetaElement): IMetaElement;
    function MakeSwitch: IMetaElement;
    function MakeBody: IMetaElement;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fSwitchEdge: Integer;
    fSwitchSize: Integer;
    fPSGUIChannel: IPSGUIChannel;
  published
    property SwitchEdge: Integer read fSwitchEdge write fSwitchEdge;
    property SwitchSize: Integer read fSwitchSize write fSwitchSize;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

  { TDesignComponentFrame }

  TDesignComponentFrame = class(TDesignComponent, IDesignComponentFrame)
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

implementation

{ TDesignComponentPager.TClickLink }

procedure TDesignComponentPager.TClickLink.PSClickChannelObserver;
begin
  fPSPagerChannel.Publish(fIndex);
end;

constructor TDesignComponentPager.TClickLink.Create(AIndex: Integer;
  const APSPagerChannel: IPSPagerChannel; const APSClickChannel: IPSClickChannel);
begin
  fIndex := AIndex;
  fPSPagerChannel := APSPagerChannel;
  fPSClickChannel := APSClickChannel;
  fPSClickChannel.Subscribe(PSClickChannelObserver);
end;

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

procedure TDesignComponentEdit.PSTextChannelObserver(const AValue: String);
begin
  Text := AValue;
end;

function TDesignComponentEdit.PSTextChannel: IPSTextChannel;
begin
  Result := fPSTextChannel;
end;

function TDesignComponentEdit.PSKeyDownChannel: IPSKeyChannel;
begin
  Result := fPSKeyDownChannel;
end;

procedure TDesignComponentEdit.InitValues;
begin
  inherited InitValues;
  fPSTextChannel := PubSub.Factory.NewDataChannel<String>;
  fPSTextChannel.Subscribe(PSTextChannelObserver);
  fPSKeyDownChannel := PubSub.Factory.NewDataChannel<TKeyData>;
end;

function TDesignComponentEdit.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetStr(cProps.Text, Text)
    .SetInt(cProps.MMWidth, SelfProps.AsInt(cProps.MMWidth))
    .SetInt(cProps.MMHeight, SelfProps.AsInt(cProps.MMHeight))
    .SetBool(cProps.Focused, SelfProps.AsBool(cProps.Focused))
    .SetBool(cProps.Flat, SelfProps.AsBool(cProps.Flat))
    .SetIntf(cEdit.PSTextChannel, PSTextChannel)
    .SetIntf(cEdit.PSKeyDownChannel, PSKeyDownChannel);
end;

function TDesignComponentEdit.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IEditBit, NewComposeProps);
end;

procedure TDesignComponentEdit.SetText(AText: String);
begin
  if AText <> fText then begin
    fText := AText;
    fPSTextChannel.Publish(fText);
  end;
end;

{ TDesignComponentGrid }

procedure TDesignComponentGrid.PSTextChannelObserver(const AValue: String);
begin
  fData[fCurrentRow, fCurrentCol] := AValue;
end;

procedure TDesignComponentGrid.PSKeyDownChannelObserver(const AValue: TKeyData);
begin
  case AValue.ControlKey of
    ckTab: if AValue.Shift then Move(-1, 0) else Move(1, 0);
    ckUp: Move(0, -1);
    ckDown: Move(0, 1);
  end;
end;

procedure TDesignComponentGrid.Move(AXDelta, AYDelta: Integer);
var
  mNew: Integer;
begin
  mNew := fCurrentCol + AXDelta;
  if (mNew >= 0) and (mNew < ColCount) then
    fCurrentCol := mNew;
  mNew := fCurrentRow + AYDelta;
  if (mNew >= 0) and (mNew < RowCount) then
    fCurrentRow := mNew;
  fEdit.PSTextChannel.Publish(fData[fCurrentRow, fCurrentCol]);
  PSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

function TDesignComponentGrid.ColProps(Row, Col: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetStr(cProps.Text, fData[Row, Col])
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.TextColor, SelfProps.AsInt(cProps.TextColor))
    .SetInt(cProps.MMWidth, SelfProps.AsInt(cGrid.ColMMWidth));
  if Row mod 2 = 1 then
    mProp := SelfProps.PropByName[cGrid.ColOddColor]
  else
    mProp := SelfProps.PropByName[cGrid.ColEvenColor];
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
    .SetInt(cProps.MMHeight, SelfProps.AsInt(cGrid.RowMMHeight));
  if Row mod 2 = 1 then begin
    mProp := SelfProps.PropByName[cGrid.RowOddColor];
  end else begin
    mProp := SelfProps.PropByName[cGrid.RowEvenColor];
  end;
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger).SetBool(cProps.Transparent, False);
end;

function TDesignComponentGrid.MakeRow(Row: integer): TMetaElementArray;
var
  i: integer;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, ColCount);
  for i := 0 to ColCount - 1 do
    if (Row = fCurrentRow) and (i = fCurrentCol) then begin
      Result[i] := fEdit.Compose(nil, [])
    end else begin
      Result[i] := ElementFactory.CreateElement(ITextBit, ColProps(Row, i));
    end;
end;

function TDesignComponentGrid.MakeGrid: TMetaElementArray;
var
  i: integer;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, RowCount);
  for i := 0 to RowCount - 1 do begin
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
  Result[0] := ElementFactory.CreateElement(ALaticeEl, ALaticeProps);
  for i := 0 to Length(AElements) - 1 do begin
    Result[i * 2 + 1] := AElements[i];
    Result[i * 2 + 2] := ElementFactory.CreateElement(ALaticeEl, ALaticeProps);
  end;
end;

procedure TDesignComponentGrid.InitValues;
begin
  inherited InitValues;
  fEdit := Factory2.Locate<IDesignComponentEdit>(
    NewProps
    .SetBool(cProps.Flat, True)
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetBool(cEdit.Focused, True)
  );
  fEdit.PSTextChannel.Subscribe(PSTextChannelObserver);
  fEdit.PSKeyDownChannel.Subscribe(PSKeyDownChannelObserver);
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

procedure TDesignComponentGrid.SetRowCount(AValue: Integer);
begin
  if fRowCount = AValue then Exit;
  fRowCount := AValue;
  SetLength(fData, RowCount, ColCount);
end;

procedure TDesignComponentGrid.SetColCount(AValue: Integer);
begin
  if fColCount = AValue then Exit;
  fColCount := AValue;
  SetLength(fData, RowCount, ColCount);
end;

{ TDesignComponentForm }

function TDesignComponentForm.PSCloseChannel: IPSCloseChannel;
begin
  Result := fPSCloseChannel;
end;

procedure TDesignComponentForm.PSSizeChannelObserver(const AData: TSizeData);
begin
  fMMWidth := AData.Width;
  fMMHeight := AData.Height;
  if PSGUIChannel <> nil then
    fPSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

function TDesignComponentForm.PSSizeChannel: IPSSizeChannel;
begin
  Result := fPSSizeChannel;
end;

procedure TDesignComponentForm.PSPositionChannelObserver(
  const AData: TPositionData);
begin
  fMMLeft := AData.Left;
  fMMTop := AData.Top;
end;

function TDesignComponentForm.PSPositionChannel: IPSPositionChannel;
begin
  Result := fPSPositionChannel;
end;

function TDesignComponentForm.PSActivateChannel: IPSActivateChannel;
begin
  Result := fPSActivateChannel;
end;

procedure TDesignComponentForm.InitValues;
begin
  inherited InitValues;
  fPSSizeChannel := PubSub.Factory.NewDataChannel<TSizeData>;
  fPSSizeChannel.Subscribe(PSSizeChannelObserver);
  fPSPositionChannel := PubSub.Factory.NewDataChannel<TPositionData>;
  fPSPositionChannel.Subscribe(PSPositionChannelObserver);
  fPSCloseChannel := PubSub.Factory.NewChannel;
  fPSActivateChannel := PubSub.Factory.NewChannel;
end;

function TDesignComponentForm.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
  .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
  .SetInt(cProps.MMLeft, MMLeft)
  .SetInt(cProps.MMTop, MMTop)
  .SetInt(cProps.MMWidth, MMWidth)
  .SetInt(cProps.MMHeight, MMHeight)
  .SetIntf(cForm.PSCloseChannel, PSCloseChannel)
  .SetIntf(cForm.PSSizeChannel, PSSizeChannel)
  .SetIntf(cForm.PSPositionChannel, PSPositionChannel)
  .SetIntf(cForm.PSActivateChannel, PSActivateChannel)
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

procedure TDesignComponentPager.PSPagerChannelObserver(const AIndex: Integer);
begin
  if fIndex <> AIndex then begin
    fIndex := AIndex;
    PSGUIChannel.Debounce(TGUIData.Create(gaRender));
  end;
end;

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
  mSwitchDC: IDesignComponentButton;
  mClickLink: TClickLink;
begin
  fClickLinks.Clear;
  SetLength(mSwitch, Count);
  for i := 0 to Count - 1 do
  begin
    mText := (GetChild(i) as TDynaObject).SelfProps.AsStr(cProps.Caption);
    mSwitchDC :=  Factory2.Locate<IDesignComponentButton>(NewProps
      .SetStr(cButton.Text, mText)
    );
    mClickLink := TClickLink.Create(i, fPSPagerChannel, mSwitchDC.PSClickChannel);
    fClickLinks.Add(mClickLink);
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
  mActual := (GetChild(fIndex) as IDesignComponent).Compose(nil, nil);
  Result := ElementFactory.CreateElement(
    IStripBit,
    NewProps
      .SetInt(cProps.Layout, cLayout.Overlay),
    [mActual]);
end;

procedure TDesignComponentPager.InitValues;
begin
  inherited InitValues;
  fPSPagerChannel :=  PubSub.Factory.NewDataChannel<Integer>;
  fPSPagerChannel.Subscribe(PSPagerChannelObserver);
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
      mChildren := [MakeBody, MakeSwitch];
  end;
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps, mChildren);
end;

procedure TDesignComponentPager.AfterConstruction;
begin
  inherited AfterConstruction;
  fClickLinks := TFPGObjectList<TClickLink>.Create(True);
end;

procedure TDesignComponentPager.BeforeDestruction;
begin
  fPSPagerChannel.Unsubscribe(PSPagerChannelObserver);
  FreeAndNil(fClickLinks);
  inherited BeforeDestruction;
end;

{ TDesignComponentStrip }

function TDesignComponentStrip.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mc:integer;
begin
  mc := SelfProps.AsInt(cProps.Color);
  Result := ElementFactory.CreateElement(IStripBit,
    NewComposeProps
      .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
      .SetStr(cProps.Caption, SelfProps.AsStr(cProps.Caption))
      .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color)),
    AChildren);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    ComposeChildren(Result);
end;

{ TDesignComponentButton }

function TDesignComponentButton.PSClickChannel: IPSClickChannel;
begin
  Result := fPSClickChannel;
end;

function TDesignComponentButton.NewOuterProps: IProps;
begin
  Result := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight])
    .SetInt(cProps.Border, 4)
    .SetInt(cProps.BorderColor, clGray);
end;

procedure TDesignComponentButton.InitValues;
begin
  inherited InitValues;
  fPSClickChannel := PubSub.Factory.NewChannel;
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
    .SetIntf(cButton.PSClickChannel, PSClickChannel);
end;

function TDesignComponentButton.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  {
  Result := ElementFactory.CreateElement(IDesignComponentFrame,
    NewOuterProps,
    [ElementFactory.CreateElement(IButtonBit, NewComposeProps)]);
  }
  Result := ElementFactory.CreateElement(IButtonBit, NewComposeProps);
end;

{ TDesignComponent }

function TDesignComponent.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
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
    {cProps.MMWidth, cProps.MMHeight,} cProps.Border,
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
  Result := SelfProps.AsInt(APropName)
end;

function TDesignComponent.StyleBackColor(const APropName: String): TColor;
begin
  Result := SelfProps.AsInt(APropName)
end;

function TDesignComponent.StyleSuppleColor(const APropName: String): TColor;
begin
  Result := SelfProps.AsInt(APropName)
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

