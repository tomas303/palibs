unit rea_udesigncomponent;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, trl_usystem, trl_imetaelement, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, flu_iflux, trl_ilog, trl_igenericaccess,
  sysutils, rea_ilayout, Graphics, LCLType, fgl, trl_isequence, rea_irenderer;

type

  { TDesignComponentFunc }

  TDesignComponentFunc = class(TInterfacedObject, IFluxFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); virtual; abstract;
  protected
    // IFluxFunc
    procedure Execute(const AAction: IFluxAction);
    function RunAsync: Boolean;
    function GetID: integer;
  public
    constructor Create(AID: integer; const AState: IGenericAccess);
  protected
    fID: integer;
    fState: IGenericAccess;
  published
    property ID: integer read fID write fID;
    property State: IGenericAccess read fState write fState;
  end;

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
    function NewAction(AActionID: integer): IFluxAction;
    function NewNotifier(const AActionID: integer): IFluxNotifier; overload;
    function NewNotifier(const AFunc: IFluxFunc): IFluxNotifier; overload;
    function NewState(const APath: string): IGenericAccessRO;
    procedure AddFuncNotifier(const AState: IGenericAccess; const AFunc: IFluxFunc; const ANotifierName: string);
  protected
    procedure DoInitValues; virtual;
    procedure DoInitState(const AState: IGenericAccess); virtual;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; virtual; abstract;
    procedure AddChildren(const AElement: IMetaElement; const AChildren: TMetaElementArray);
    procedure DoStartingValues; virtual;
  protected
    // IDesignComponent = interface
    function Compose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
    procedure InitValues; override;
    procedure InitState(const AState: IGenericAccess);
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
    fLog: ILog;
    fElementFactory: IMetaElementFactory;
    fFactory: IDIFactory;
    fNode: INode;
    fState: IGenericAccessRO;
    fDataPath: string;
    fStoreConnector: IFluxData;
    fFluxFuncReg: IFluxFuncReg;
    fFuncSequence: ISequence;
  published
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Node: INode read fNode write fNode;
    property State: IGenericAccessRO read fState write fState;
    property DataPath: string read fDataPath write fDataPath;
    property StoreConnector: IFluxData read fStoreConnector write fStoreConnector;
    property FluxFuncReg: IFluxFuncReg read fFluxFuncReg write fFluxFuncReg;
    property FuncSequence: ISequence read fFuncSequence write fFuncSequence;
  end;

  { TSizeFunc }

  TSizeFunc = class(TDesignComponentFunc)
  private
    fRenderNotifier: IFluxNotifier;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(AID: integer; const AState: IGenericAccess; const ARenderNotifier: IFluxNotifier);
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
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fSizeNotifier: IFluxNotifier;
    fMoveNotifier: IFluxNotifier;
    fCloseQueryNotifier: IFluxNotifier;
  published
    property SizeNotifier: IFluxNotifier read fSizeNotifier write fSizeNotifier;
    property MoveNotifier: IFluxNotifier read fMoveNotifier write fMoveNotifier;
    property CloseQueryNotifier: IFluxNotifier read fCloseQueryNotifier write fCloseQueryNotifier;
  end;

  { TTextChangedFunc }

  TTextChangedFunc = class(TDesignComponentFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  { TKeyDownFunc }

  TKeyDownFunc = class(TDesignComponentFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  { TDesignComponentEdit }

  TDesignComponentEdit = class(TDesignComponent, IDesignComponentEdit)
  protected
    procedure DoInitValues; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fTextChangedNotifier: IFluxNotifier;
    fKeyDownNotifier: IFluxNotifier;
  published
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

  { IGridData }

  IGridData = interface
  ['{B3B7B1B3-A738-4A44-BB6D-53E09BDAF8A9}']
    function GetValue(X, Y: integer): string;
    procedure SetValue(X, Y: integer; AValue: string);
    procedure CheckDimensions(AHorizontalCount, AVerticalCount: integer);
    property Value[X, Y: integer]: string read GetValue write SetValue; default;
  end;

  { TGridData }

  TGridData = class(TInterfacedObject, IGridData)
  private type
    TMatrix = array of array of string;
  private
    fData: TMatrix;
    fDataHCount: integer;
    fDataVCount: integer;
    function GetValue(X, Y: integer): string;
    procedure SetValue(X, Y: integer; AValue: string);
  public
    procedure CheckDimensions(AHorizontalCount, AVerticalCount: integer);
    property Value[X, Y: integer]: string read GetValue write SetValue; default;
  end;

  { TGridFunc }

  TGridFunc = class(TDesignComponentFunc)
  protected
    fRenderNotifier: IFluxNotifier;
    fEdState: IGenericAccess;
    function GetHorizontalCount: integer;
    function GetPosX: integer;
    function GetPosY: integer;
    function GetVerticalCount: integer;
    function GetData: IGridData;
    procedure SetPosX(AValue: integer);
    procedure SetPosY(AValue: integer);
    property HorizontalCount: integer read GetHorizontalCount;
    property VerticalCount: integer read GetVerticalCount;
    property PosX: integer read GetPosX write SetPosX;
    property PosY: integer read GetPosY write SetPosY;
    property Data: IGridData read GetData;
  public
    constructor Create(AID: integer; const AState, AEdState: IGenericAccess; const ARenderNotifier: IFluxNotifier);
  end;

  { TGridEdTextChangedFunc }

  TGridEdTextChangedFunc = class(TGridFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;


  { TGridEdKeyDownFunc }

  TGridEdKeyDownFunc = class(TGridFunc)
  private
    fBrowseMode: Boolean;
    procedure PositionMove;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    procedure AfterConstruction; override;
  end;

  { TDesignComponentGrid }

  TDesignComponentGrid = class(TDesignComponent, IDesignComponentGrid)
  private
    fEdTextChangedNotifier: IFluxNotifier;
    fEdKeyDownNotifier: IFluxNotifier;
    fEdState: IGenericAccessRO;
    function RowProps(X, Y: integer): IProps;
    function ColProps(ANr: integer): IProps;
    function GridProps: IProps;
    function MakeRow(ARowNr: integer): TMetaElementArray;
    function MakeGrid: TMetaElementArray;
    function LaticeColProps: IProps;
    function LaticeRowProps: IProps;
    function Latice(AElements: TMetaElementArray; ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
  protected
    procedure DoStartingValues; override;
    procedure DoInitValues; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fHorizontalCount: integer;
    fVerticalCount: integer;
    fLaticeColColor: integer;
    fLaticeRowColor: integer;
    fLaticeColSize: integer;
    fLaticeRowSize: integer;
  published
    property HorizontalCount: integer read fHorizontalCount write fHorizontalCount;
    property VerticalCount: integer read fVerticalCount write fVerticalCount;
    property LaticeColColor: integer read fLaticeColColor write fLaticeColColor;
    property LaticeRowColor: integer read fLaticeRowColor write fLaticeRowColor;
    property LaticeColSize: integer read fLaticeColSize write fLaticeColSize;
    property LaticeRowSize: integer read fLaticeRowSize write fLaticeRowSize;
  end;

  { TDesignComponentPager }

  TDesignComponentPager = class(TDesignComponent, IDesignComponentPager)
  public type

    { TTabChangedFunc }

    TTabChangedFunc = class(TDesignComponentFunc)
    private
      fSwitchElement: IMetaElement;
      fRenderNotifier: IFluxNotifier;
      function GetActualElement: IMetaElement;
      procedure SetActualElement(AValue: IMetaElement);
    protected
      procedure DoExecute(const AAction: IFluxAction); override;
    public
      constructor Create(AID: integer; const AState: IGenericAccess; const ASwitchElement: IMetaElement;
        const ARenderNotifier: IFluxNotifier);
      property ActualElement: IMetaElement read GetActualElement write SetActualElement;
    end;

  private
    function RenderPage(const APageElement: IMetaElement): IMetaElement;
    function MakeSwitch(const AChildren: TMetaElementArray): IMetaElement;
    function MakeBody(const AChildren: TMetaElementArray): IMetaElement;
    function MakeProps: IProps;
  protected
    procedure DoStartingValues; override;
    procedure DoInitValues; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
    function GetActualElement: IMetaElement;
    property ActualElement: IMetaElement read GetActualElement;
  protected
    fSwitchEdge: Integer;
    fSwitchSize: Integer;
  published
    property SwitchEdge: Integer read fSwitchEdge write fSwitchEdge;
    property SwitchSize: Integer read fSwitchSize write fSwitchSize;
  end;

implementation

{ TDesignComponentPager.TTabChangedFunc }

procedure TDesignComponentPager.TTabChangedFunc.SetActualElement(
  AValue: IMetaElement);
begin
  fState.SetIntf('SwitchElement', AValue);
end;

function TDesignComponentPager.TTabChangedFunc.GetActualElement: IMetaElement;
begin
  Result := fState.AsIntf('SwitchElement') as IMetaElement;
end;

procedure TDesignComponentPager.TTabChangedFunc.DoExecute(
  const AAction: IFluxAction);
begin
  ActualElement := fSwitchElement;
  fRenderNotifier.Notify;
end;

constructor TDesignComponentPager.TTabChangedFunc.Create(AID: integer;
  const AState: IGenericAccess; const ASwitchElement: IMetaElement; const ARenderNotifier: IFluxNotifier);
begin
  inherited Create(AID, AState);
  fSwitchElement := ASwitchElement;
  fRenderNotifier := ARenderNotifier;
end;

{ TDesignComponentPager }

function TDesignComponentPager.RenderPage(const APageElement: IMetaElement): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, [APageElement]);
end;

function TDesignComponentPager.MakeSwitch(const AChildren: TMetaElementArray): IMetaElement;
var
  i: integer;
  mSwitch: TMetaElementArray;
  mProps: IProps;
begin
  SetLength(mSwitch, Length(AChildren));
  for i := 0 to High(AChildren) do
  begin
    mSwitch[i] := ElementFactory.CreateElement(
      IDesignComponentButton,
      NewProps
        .SetStr(cProps.Text, AChildren[i].Props.AsStr(cProps.Caption))
        .SetInt(cProps.Place, cPlace.Elastic)
        .SetIntf(cProps.ClickNotifier,
          NewNotifier(
            TTabChangedFunc.Create(
              FuncSequence.Next,
              State as IGenericAccess,
              AChildren[i],
              NewNotifier(cFuncRender))))
    );
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

function TDesignComponentPager.MakeBody(const AChildren: TMetaElementArray
  ): IMetaElement;
var
  mActual: IMetaElement;
begin
  mActual := ActualElement;
  if (mActual = nil) and (Length(AChildren) > 0) then
    mActual := AChildren[0];

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

procedure TDesignComponentPager.DoStartingValues;
begin
  inherited DoStartingValues;
  SwitchEdge := IDesignComponentPager.SwitchEdgeTop;
  SwitchSize := 100;
end;

procedure TDesignComponentPager.DoInitValues;
begin
  inherited DoInitValues;
end;

function TDesignComponentPager.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mChildren: TMetaElementArray;
begin
  case SwitchEdge of
    cEdge.Left, cEdge.Top:
      mChildren := [MakeSwitch(AChildren), MakeBody(AChildren)];
    cEdge.Right, cEdge.Bottom:
      mChildren := [MakeBody(AChildren),MakeSwitch(AChildren)];
  end;
  Result := ElementFactory.CreateElement(IStripBit, MakeProps, mChildren);
end;

function TDesignComponentPager.GetActualElement: IMetaElement;
begin
  Result := State.AsIntf('SwitchElement') as IMetaElement;
end;

{ TGridData }

function TGridData.GetValue(X, Y: integer): string;
begin
  Result := fData[X, Y];
end;

procedure TGridData.SetValue(X, Y: integer; AValue: string);
begin
  fData[X, Y] := AValue;
end;

procedure TGridData.CheckDimensions(AHorizontalCount,
  AVerticalCount: integer);
begin
  if (fDataHCount <> AHorizontalCount)
     or (fDataVCount <> AVerticalCount)
  then begin
    fDataHCount := AHorizontalCount;
    fDataVCount := AVerticalCount;
    SetLength(fData, fDataHCount, fDataVCount);
  end;
end;

{ TGridFunc }

function TGridFunc.GetData: IGridData;
begin
  Result := fState.AsIntf('GridData') as IGridData;
  if Result = nil then begin
    Result := TGridData.Create;
    fState.SetIntf('GridData', Result);
  end;
end;

function TGridFunc.GetHorizontalCount: integer;
begin
  Result := fState.AsInt('HorizontalCount');
end;

function TGridFunc.GetPosX: integer;
begin
  Result := fState.AsInt('PosX');
end;

function TGridFunc.GetPosY: integer;
begin
  Result := fState.AsInt('PosY');
end;

function TGridFunc.GetVerticalCount: integer;
begin
  Result := fState.AsInt('VerticalCount');
end;

procedure TGridFunc.SetPosX(AValue: integer);
begin
  fState.SetInt('PosX', AValue);
end;

procedure TGridFunc.SetPosY(AValue: integer);
begin
  fState.SetInt('PosY', AValue);
end;

constructor TGridFunc.Create(AID: integer; const AState,
  AEdState: IGenericAccess; const ARenderNotifier: IFluxNotifier);
begin
  inherited Create(AID, AState);
  fEdState := AEdState;
  fRenderNotifier := ARenderNotifier;
end;

{ TGridEdKeyDownFunc }

procedure TGridEdKeyDownFunc.PositionMove;
begin
  Data.CheckDimensions(HorizontalCount, VerticalCount);
  fEdState.SetStr('Text', Data[PosX, PosY]);
  fRenderNotifier.Notify;
end;

procedure TGridEdKeyDownFunc.DoExecute(const AAction: IFluxAction);
begin
  case AAction.Props.AsInt('CharCode') of
    VK_ESCAPE:
      fBrowseMode := True;
    VK_RETURN:
      fBrowseMode := False;
    VK_LEFT:
      if fBrowseMode and (PosX > 0) then begin
        PosX := PosX - 1;
        PositionMove;
      end;
    VK_RIGHT:
      if fBrowseMode and (PosX < HorizontalCount - 1) then begin
        PosX := PosX + 1;
        PositionMove;
      end;
    VK_UP:
      if fBrowseMode and (PosY > 0) then begin
        PosY := PosY - 1;
        PositionMove;
      end;
    VK_DOWN:
      if fBrowseMode and (PosY < VerticalCount - 1) then begin
        PosY := PosY + 1;
        PositionMove;
      end;
  end;
  fEdState.SetBool('Focused', AAction.Props.AsBool('Focused'));
end;

procedure TGridEdKeyDownFunc.AfterConstruction;
begin
  inherited AfterConstruction;
  fBrowseMode := True;
end;

{ TGridEdTextChangedFunc }

procedure TGridEdTextChangedFunc.DoExecute(const AAction: IFluxAction);
begin
  // pole posx posy ... tam se bude taky ukladat
  //fEdState.SetStr('Text', AAction.Props.AsStr('Text'));
  Data.CheckDimensions(HorizontalCount, VerticalCount);
  Data[PosX, PosY] := AAction.Props.AsStr('Text');
end;

{ TKeyDownFunc }

procedure TKeyDownFunc.DoExecute(const AAction: IFluxAction);
begin
  fState.SetInt('CharCode', AAction.Props.AsInt('CharCode'));
  fState.SetInt('KeyData', AAction.Props.AsInt('KeyData'));
end;

{ TTextChangedFunc }

procedure TTextChangedFunc.DoExecute(const AAction: IFluxAction);
begin
  fState.SetStr('Text', AAction.Props.AsStr('Text'));
end;

{ TDesignComponentGrid }

function TDesignComponentGrid.RowProps(X, Y: integer): IProps;
var
  mProp: IProp;
  mData: IGridData;
  mText: string;
begin
  mData := fState.AsIntf('GridData') as IGridData;
  if mData <> nil then
    mText := mData[X, Y];
  Result := NewProps
    .SetInt('Place', cPlace.Elastic)
    .SetStr('Text', mText)
    .SetInt(cProps.MMWidth, SelfProps.AsInt(cProps.ColMMWidth));
  if X mod 2 = 1 then
    mProp := SelfProps.PropByName[cProps.ColOddColor]
  else
    mProp := SelfProps.PropByName[cProps.ColEvenColor];
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger);
end;

function TDesignComponentGrid.ColProps(ANr: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt('Place', cPlace.FixFront)
    .SetInt('Layout', cLayout.Horizontal)
    .SetInt(cProps.MMHeight, SelfProps.AsInt(cProps.RowMMHeight));
  if ANr mod 2 = 1 then begin
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

function TDesignComponentGrid.MakeRow(ARowNr: integer): TMetaElementArray;
var
  i: integer;
  mProps: IProps;
  mposx,mposy:integer;
begin
  mposx := State.AsInt('PosX');
  mposy := State.AsInt('PosY');
  Result := TMetaElementArray.Create;
  SetLength(Result, HorizontalCount);
  for i := 0 to HorizontalCount - 1 do
    if (ARowNr = State.AsInt('PosY')) and (i = State.AsInt('PosX')) then begin
      mProps := RowProps(i, ARowNr);
      mProps
        .SetBool('Flat', True)
        .SetIntf('State', fEdState)
        .SetIntf('TextChangedNotifier', fEdTextChangedNotifier)
        .SetIntf('KeyDownNotifier', fEdKeyDownNotifier);
        //.SetStr('Text', fEdState.AsStr('Text'));
      Result[i] := ElementFactory.CreateElement(IDesignComponentEdit, mProps);
    end else begin
      Result[i] := ElementFactory.CreateElement(ITextBit, RowProps(i, ARowNr));
    end;
end;

function TDesignComponentGrid.MakeGrid: TMetaElementArray;
var
  i: integer;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, VerticalCount);
  for i := 0 to VerticalCount - 1 do begin
    Result[i] := ElementFactory.CreateElement(IStripBit, ColProps(i), Latice(MakeRow(i), IStripBit, LaticeColProps));
  end;
  Result := Latice(Result, IStripBit, LaticeRowProps);
end;

procedure TDesignComponentGrid.DoStartingValues;
begin
  inherited DoStartingValues;
  LaticeColColor := clBlack;
  LaticeRowColor := clBlack;
  LaticeColSize := 1;
  LaticeRowSize := 1;
end;

procedure TDesignComponentGrid.DoInitValues;
var
  mFunc: IFluxFunc;
begin
  inherited DoInitValues;
  fEdState := NewState(DataPath + '/Ed');
  (State as IGenericAccess).SetInt('HorizontalCount', HorizontalCount);
  (State as IGenericAccess).SetInt('VerticalCount', VerticalCount);
  //
  FuncSequence.Next;
  fEdTextChangedNotifier := State.AsIntf('EdTextChangedNotifier') as IFluxNotifier;
  if fEdTextChangedNotifier = nil then begin
    mFunc := TGridEdTextChangedFunc.Create(FuncSequence.Next, fState as IGenericAccess, fEdState as IGenericAccess, NewNotifier(cFuncRender));
    fEdTextChangedNotifier := NewNotifier(mFunc.ID);
    FluxFuncReg.RegisterFunc(mFunc);
    (State as IGenericAccess).SetIntf('EdTextChangedNotifier', fEdTextChangedNotifier);
  end;

  fEdKeyDownNotifier := State.AsIntf('EdKeyDownNotifier') as IFluxNotifier;
  if fEdKeyDownNotifier = nil then begin
    mFunc := TGridEdKeyDownFunc.Create(FuncSequence.Next, fState as IGenericAccess, fEdState as IGenericAccess, NewNotifier(cFuncRender));
    fEdKeyDownNotifier := NewNotifier(mFunc.ID);
    FluxFuncReg.RegisterFunc(mFunc);
    (State as IGenericAccess).SetIntf('EdKeyDownNotifier', fEdKeyDownNotifier);
  end;

end;

function TDesignComponentGrid.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(
    IStripBit, GridProps, MakeGrid);
end;

{ TDesignComponentFunc }

procedure TDesignComponentFunc.Execute(const AAction: IFluxAction);
begin
  DoExecute(AAction);
end;

function TDesignComponentFunc.RunAsync: Boolean;
begin
  Result := False;
end;

function TDesignComponentFunc.GetID: integer;
begin
  Result := fID;
end;

constructor TDesignComponentFunc.Create(AID: integer;
  const AState: IGenericAccess);
begin
  inherited Create;
  fID := AID;
  fState := AState;
end;

{ TMoveFunc }

procedure TMoveFunc.DoExecute(const AAction: IFluxAction);
begin
  fState.SetInt('Left', AAction.Props.AsInt('MMLeft'));
  fState.SetInt('Top', AAction.Props.AsInt('MMTop'));
end;

{ TSizeFunc }

procedure TSizeFunc.DoExecute(const AAction: IFluxAction);
begin
  fState.SetInt('Width', AAction.Props.AsInt('MMWidth'));
  fState.SetInt('Height', AAction.Props.AsInt('MMHeight'));
  fRenderNotifier.Notify;
end;

constructor TSizeFunc.Create(AID: integer; const AState: IGenericAccess;
  const ARenderNotifier: IFluxNotifier);
begin
  inherited Create(AID, AState);
  fRenderNotifier := ARenderNotifier;
end;

{ TDesignComponentHeader }

function TDesignComponentHeader.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.Title, cProps.MMWidth, cProps.MMHeight,
    cProps.Border, cProps.BorderColor, cProps.FontColor, cProps.Transparent]);
  Result := ElementFactory.CreateElement(IStripBit, mProps);
end;

{ TDesignComponentButton }

function TDesignComponentButton.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mProps: IProps;
begin
  mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight, cProps.Color, cProps.Text, cProps.ClickNotifier]);
  Result := ElementFactory.CreateElement(IButtonBit, mProps);
end;

{ TDesignComponentEdit }

function TDesignComponentEdit.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mTitle: IProp;
  mProps: IProps;
begin
  //mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight]);
  //Result := ElementFactory.CreateElement(IStripBit, mProps);
  //mTitle := SelfProps.PropByName[cProps.Title];
  //if mTitle <> nil then
  //  (Result as INode).AddChild(ElementFactory.CreateElement(ITextBit, NewProps.SetProp(cProps.Text, mTitle)) as INode);
  //(Result as INode).AddChild(ElementFactory.CreateElement(IEditBit,
  //  NewProps
  //  .SetProp(cProps.Text, SelfProps.PropByName[cProps.Value])
  //  .SetProp(cProps.OnTextNotifier, SelfProps.PropByName[cProps.OnTextNotifier])
  //  ) as INode);
  mProps := SelfProps.Clone([cProps.Place, cProps.MMWidth, cProps.MMHeight]);
  mProps
    .SetStr('Text', State.AsStr('Text'))
    .SetIntf('TextChangedNotifier', TextChangedNotifier)
    .SetIntf('KeyDownNotifier', KeyDownNotifier)
    .SetBool('Focused', State.AsBool('Focused'))
    .SetBool('Flat', SelfProps.AsBool('Flat'));
  Result := ElementFactory.CreateElement(IEditBit, mProps);
end;

procedure TDesignComponentEdit.DoInitValues;
var
  mFunc: IFluxFunc;
begin
  inherited DoInitValues;
  if fTextChangedNotifier = nil then begin
    mFunc := TTextChangedFunc.Create(FuncSequence.Next, fState as IGenericAccess);
    fTextChangedNotifier := NewNotifier(mFunc.ID);
    FluxFuncReg.RegisterFunc(mFunc);
  end;
  if fKeyDownNotifier = nil then begin
    mFunc := TKeyDownFunc.Create(FuncSequence.Next, fState as IGenericAccess);
    fKeyDownNotifier := NewNotifier(FuncSequence.Next);
    FluxFuncReg.RegisterFunc(mFunc);
  end;
end;

{ TDesignComponentForm }

procedure TDesignComponentForm.DoInitValues;
var
  mFunc: IFluxFunc;
begin
  inherited DoInitValues;
  fSizeNotifier := State.AsIntf('SizeNotifier') as IFluxNotifier;
  if fSizeNotifier = nil then begin
    mFunc := TSizeFunc.Create(FuncSequence.Next, fState as IGenericAccess, NewNotifier(cFuncRender));
    fSizeNotifier := NewNotifier(mFunc.ID);
    FluxFuncReg.RegisterFunc(mFunc);
    (State as IGenericAccess).SetIntf('SizeNotifier', fSizeNotifier);
  end;
  fMoveNotifier := State.AsIntf('MoveNotifier') as IFluxNotifier;
  if fMoveNotifier = nil then begin
    mFunc := TMoveFunc.Create(FuncSequence.Next, fState as IGenericAccess);
    fMoveNotifier := NewNotifier(mFunc.ID);
    FluxFuncReg.RegisterFunc(mFunc);
    (State as IGenericAccess).SetIntf('MoveNotifier', fMoveNotifier);
  end;
  if State.AsInt('Width') = 0 then
    (State as IGenericAccess).SetInt('Width', 400);
  if State.AsInt('Height') = 0 then
    (State as IGenericAccess).SetInt('Height', 200);
end;

function TDesignComponentForm.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
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
  mProps.SetIntf(cProps.CloseQueryNotifier, CloseQueryNotifier);


  mProps.SetInt(cProps.MMLeft, State.AsInt('Left'));
  mProps.SetInt(cProps.MMTop, State.AsInt('Top'));
  mw := State.AsInt('Left');
  mProps.SetInt(cProps.MMWidth, State.AsInt('Width'));
  mProps.SetInt(cProps.MMHeight, State.AsInt('Height'));

  Result := ElementFactory.CreateElement(IFormBit, mProps);
  AddChildren(Result, AChildren);
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

function TDesignComponent.NewNotifier(const AFunc: IFluxFunc): IFluxNotifier;
begin
  FluxFuncReg.RegisterFunc(AFunc);
  Result := NewNotifier(AFunc.ID);
end;

function TDesignComponent.NewState(const APath: string): IGenericAccessRO;
begin
  Result := StoreConnector.Data[APath] as IGenericAccessRO;
end;

procedure TDesignComponent.AddFuncNotifier(const AState: IGenericAccess; const AFunc: IFluxFunc;
  const ANotifierName: string);
begin
  FluxFuncReg.RegisterFunc(AFunc);
  AState.SetIntf(ANotifierName, NewNotifier(AFunc.ID));
end;

procedure TDesignComponent.DoInitValues;
var
  mInit: Boolean;
begin
  if fState = nil then begin
    mInit := not StoreConnector.Exists(DataPath);
    fState := NewState(DataPath);
    if mInit then
       InitState(fState as IGenericAccess);
  end;
end;

procedure TDesignComponent.DoInitState(const AState: IGenericAccess);
begin

end;

procedure TDesignComponent.AddChildren(const AElement: IMetaElement;
  const AChildren: TMetaElementArray);
var
  mEl: IMetaElement;
begin
  for mEl in AChildren do
    (AElement as INode).AddChild(mEl as INode);
end;

procedure TDesignComponent.DoStartingValues;
begin

end;

function TDesignComponent.Compose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := DoCompose(AProps, AChildren);
end;

procedure TDesignComponent.InitValues;
begin
  inherited InitValues;
  DoInitValues;
end;

procedure TDesignComponent.InitState(const AState: IGenericAccess);
begin
  DoInitState(AState);
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

