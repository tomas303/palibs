unit rea_udesigncomponent;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, rea_udesigncomponentdata, trl_usystem, trl_imetaelement, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, flu_iflux, trl_ilog,
  sysutils, rea_ilayout, Graphics, LCLType, fgl, trl_isequence, rea_irenderer;

type

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
    function NewAction(AActionID: integer): IFluxAction;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; virtual; abstract;
    procedure AddChildren(const AElement: IMetaElement; const AChildren: TMetaElementArray);
    procedure ComposeChildren(const AParentEl: IMetaElement);
    procedure DoStartingValues; virtual;
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
    fNode: INode;
  published
    property ID: string read fID write fID;
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Node: INode read fNode write fNode;
  end;

  { TDesignComponentForm }

  TDesignComponentForm = class(TDesignComponent, IDesignComponentForm)
  protected
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
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TEditData;
    fAskNotifier: IFluxNotifier; //todo .... probably remove
    fTextChangedNotifier: IFluxNotifier;
    fKeyDownNotifier: IFluxNotifier;
  published
    property Data: TEditData read fData write fData;
    property AskNotifier: IFluxNotifier read fAskNotifier write fAskNotifier;
    property TextChangedNotifier: IFluxNotifier read fTextChangedNotifier write fTextChangedNotifier;
    property KeyDownNotifier: IFluxNotifier read fKeyDownNotifier write fKeyDownNotifier;
  end;

  { TDesignComponentButton }

  TDesignComponentButton = class(TDesignComponent, IDesignComponentButton)
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  { TDesignComponentHeader }

  TDesignComponentHeader = class(TDesignComponent, IDesignComponentHeader)
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  { TDesignComponentGrid }

  TDesignComponentGrid = class(TDesignComponent, IDesignComponentGrid)
  private
   function ColProps(Row, Col: integer): IProps;
   function RowProps(Row: integer): IProps;
   function GridProps: IProps;
   function MakeRow(Row: integer): TMetaElementArray;
   function MakeGrid: TMetaElementArray;
   function LaticeColProps: IProps;
   function LaticeRowProps: IProps;
   function Latice(AElements: TMetaElementArray; ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
  protected
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
    function MakeProps: IProps;
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TPagerData;
    fSwitchEdge: Integer;
    fSwitchSize: Integer;
    fSwitchFactory: IDesignComponentFactory;
  published
    property Data: TPagerData read fData write fData;
    property SwitchEdge: Integer read fSwitchEdge write fSwitchEdge;
    property SwitchSize: Integer read fSwitchSize write fSwitchSize;
    property SwitchFactory: IDesignComponentFactory read fSwitchFactory write fSwitchFactory;
  end;

  { TDesignComponentLabelEdit }

  TDesignComponentLabelEdit = class(TDesignComponent, IDesignComponentLabelEdit)
  private
    function MakeProps: IProps;
    function MakeLabeledEdits(AChildren: TMetaElementArray): TMetaElementArray;
    function NewLabeledEdit(const ASource: IMetaElement): IMetaElement;
    function NewLabeledProps(const ASource: IMetaElement): IProps;
    function NewEdit(ATypeID: TGuid): IMetaElement;
    function NewText(const ACaption: String; APlace: Integer; ATextWidth: Integer): IMetaElement;
    function TextWidth(const ASource: IMetaElement): Integer;
  protected
    procedure DoStartingValues; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fCaption: String;
    fCaptionEdge: Integer;
  published
    property Caption: String read fCaption write fCaption;
    property CaptionEdge: Integer read fCaptionEdge write fCaptionEdge;
  end;

implementation

{ TDesignComponentEdit }

function TDesignComponentEdit.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight]);
  mProps
    .SetStr('ID', ID)
    .SetStr('Text', Data.Text)
    //.SetIntf('AskNotifier', AskNotifier)
    .SetIntf(cProps.TextChangedNotifier, TextChangedNotifier)
    .SetIntf(cProps.KeyDownNotifier, KeyDownNotifier)
    .SetBool('Focused', Data.Focused)
    .SetBool('Flat', SelfProps.AsBool('Flat'));
  Result := ElementFactory.CreateElement(IEditBit, mProps);
end;

{ TDesignComponentGrid }

function TDesignComponentGrid.ColProps(Row, Col: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt('Place', cPlace.Elastic)
    .SetStr('Text', Data[Row, Col])
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
    .SetInt('Place', cPlace.FixFront)
    .SetInt('Layout', cLayout.Horizontal)
    .SetInt(cProps.MMHeight, SelfProps.AsInt(cProps.RowMMHeight));
  if Row mod 2 = 1 then begin
    mProp := SelfProps.PropByName[cProps.RowOddColor];
  end else begin
    mProp := SelfProps.PropByName[cProps.RowEvenColor];
  end;
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger).SetBool('Transparent', False);
end;

function TDesignComponentGrid.GridProps: IProps;
var
  mProp: IProp;
begin
  Result := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.MMWidth, cProps.MMHeight]);
  Result
   .SetInt('Place', cPlace.Elastic)
   .SetInt('Layout', cLayout.Vertical);
  mProp := SelfProps.PropByName[cProps.Color];
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger).SetBool('Transparent', False)
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
        .SetObject('Data', Data.EditData)
        .SetBool('Flat', True)
        .SetIntf('TextChangedNotifier', EdTextChangedNotifier)
        .SetIntf('KeyDownNotifier', EdKeyDownNotifier);
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
    .SetInt('Place', cPlace.FixFront)
    .SetBool('Transparent', False)
    .SetInt('Color', LaticeColColor)
    .SetInt('Width', LaticeColSize);
end;

function TDesignComponentGrid.LaticeRowProps: IProps;
begin
  Result := NewProps
    .SetInt('Place', cPlace.FixFront)
    .SetBool('Transparent', False)
    .SetInt('Color', LaticeRowColor)
    .SetInt('Height', LaticeRowSize);
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

function TDesignComponentGrid.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(
    IStripBit, GridProps, MakeGrid);
end;

{ TDesignComponentForm }

function TDesignComponentForm.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Title, cProps.Layout, cProps.Color, cProps.ActivateNotifier]);
  mProps
    .SetIntf(cProps.SizeNotifier, SizeNotifier)
    .SetIntf(cProps.MoveNotifier, MoveNotifier)
    .SetIntf(cProps.CloseQueryNotifier, CloseQueryNotifier)
    .SetInt(cProps.MMLeft, Data.Left)
    .SetInt(cProps.MMTop, Data.Top)
    .SetInt(cProps.MMWidth, Data.Width)
    .SetInt(cProps.MMHeight, Data.Height);
  Result := ElementFactory.CreateElement(IFormBit, mProps);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    ComposeChildren(Result);
end;

{ TDesignComponentLabelEdit }

function TDesignComponentLabelEdit.MakeProps: IProps;
var
  mProp: IProp;
begin
  Result := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.Height, cProps.Width,
    cProps.Color, cProps.Transparent]);
end;

function TDesignComponentLabelEdit.MakeLabeledEdits(AChildren: TMetaElementArray): TMetaElementArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Length(AChildren));
  for i := 0 to High(Result) do
    Result[i] := NewLabeledEdit(AChildren[i]);
end;

function TDesignComponentLabelEdit.NewLabeledEdit(const ASource: IMetaElement): IMetaElement;
begin
  case CaptionEdge of
    IDesignComponentLabelEditHelper.CaptionEdgeLeft,
    IDesignComponentLabelEditHelper.CaptionEdgeTop:
      begin
        Result := ElementFactory.CreateElement(IStripBit,
          NewLabeledProps(ASource),
          [
            NewText(ASource.Props.AsStr(cProps.Caption), cPlace.FixFront, TextWidth(ASource)),
            NewEdit(ASource.Guid)
          ]);
      end;
    IDesignComponentLabelEditHelper.CaptionEdgeRight,
    IDesignComponentLabelEditHelper.CaptionEdgeBottom:
      begin
        Result := ElementFactory.CreateElement(IStripBit,
          NewLabeledProps(ASource),
          [
            NewEdit(ASource.Guid),
            NewText(ASource.Props.AsStr(cProps.Caption), cPlace.FixBack, TextWidth(ASource))
          ]);
      end;
  end;
end;

function TDesignComponentLabelEdit.NewLabeledProps(const ASource: IMetaElement): IProps;
var
  mProp: IProp;
begin
  Result := NewProps;
  mProp := ASource.Props.PropByName[cProps.Color];
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger).SetBool(cProps.Transparent, False);
  case CaptionEdge of
    IDesignComponentLabelEditHelper.CaptionEdgeLeft,
    IDesignComponentLabelEditHelper.CaptionEdgeRight:
      Result
        .SetInt(cProps.Layout, cLayout.Horizontal)
        .SetInt(cProps.Place, cPlace.FixFront)
        .SetInt(cProps.Width, SelfProps.AsInt(cProps.PairWidth))
        .SetInt(cProps.Height, cEditHeight);
    IDesignComponentLabelEditHelper.CaptionEdgeTop,
    IDesignComponentLabelEditHelper.CaptionEdgeBottom:
      Result
        .SetInt(cProps.Layout, cLayout.Vertical)
        .SetInt(cProps.Place, cPlace.FixFront)
        .SetInt(cProps.Width, SelfProps.AsInt(cProps.PairWidth))
        .SetInt(cProps.Height, 2 * cEditHeight);
  end;
end;

function TDesignComponentLabelEdit.NewEdit(ATypeID: TGuid): IMetaElement;
begin
  Result := ElementFactory.CreateElement(
      ATypeID,
      NewProps
        .SetInt(cProps.Place, cPlace.Elastic)
        .SetInt(cProps.Height, cEditHeight)
        .SetStr(cProps.Text, 'xxx')
    );
end;

function TDesignComponentLabelEdit.NewText(const ACaption: String;
  APlace: Integer; ATextWidth: Integer): IMetaElement;
begin
  Result := ElementFactory.CreateElement(
    ITextBit,
    NewProps
      .SetStr(cProps.Text, ACaption)
      .SetInt(cProps.Place, APlace)
      .SetInt(cProps.Width, ATextWidth)
      .SetInt(cProps.Height, cEditHeight)
  );
end;

function TDesignComponentLabelEdit.TextWidth(const ASource: IMetaElement
  ): Integer;
var
  mProp: IProp;
begin
  mProp := ASource.Props.PropByName[cProps.CaptionWidth];
  if mProp = nil then
    mProp := SelfProps.PropByName[cProps.CaptionWidth];
  if mProp <> nil then
    Result := mProp.AsInteger
  else
    Result := cCaptionWidth;
end;

procedure TDesignComponentLabelEdit.DoStartingValues;
begin
  inherited DoStartingValues;
  CaptionEdge := IDesignComponentLabelEdit.CaptionEdgeLeft;
end;

function TDesignComponentLabelEdit.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, MakeProps, MakeLabeledEdits(AChildren));
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
begin
  SetLength(mSwitch, Count);
  for i := 0 to Count - 1 do
  begin
    mText := (GetChild(i) as TDynaObject).SelfProps.AsStr(cProps.Caption);
    mSwitchDC := SwitchFactory.New(
      NewProps
        .SetInt('PageIndex', i)
        .SetStr(cProps.Text, mText)
        .SetObject('PagerData', Data)
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
      .SetInt('Layout', cLayout.Overlay),
    [mActual]);
end;

function TDesignComponentPager.MakeProps: IProps;
begin
  Result := NewProps;
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
  Result := ElementFactory.CreateElement(IStripBit, MakeProps, mChildren);
end;

{ TDesignComponentHeader }

function TDesignComponentHeader.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.Title, cProps.MMWidth, cProps.MMHeight,
    cProps.Border, cProps.BorderColor, cProps.FontColor, cProps.Transparent, cProps.Color]);
  Result := ElementFactory.CreateElement(IStripBit, mProps, AChildren);
  if AChildren <> nil then
    AddChildren(Result, AChildren)
  else
    ComposeChildren(Result);
end;

{ TDesignComponentButton }

function TDesignComponentButton.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight, cProps.Color, cProps.Text, cProps.ClickNotifier]);
  Result := ElementFactory.CreateElement(IButtonBit, mProps);
end;

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
    .SetInt('ID', AActionID);
  Result := IFluxAction(Factory.Locate(IFluxAction, '', mProps));
end;

function TDesignComponent.NewNotifier(const AActionID: integer): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '',
    NewProps
    .SetInt('ActionID', AActionID)
  ));
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

