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
    fID: string;
    fLog: ILog;
    fElementFactory: IMetaElementFactory;
    fFactory: IDIFactory;
    fNode: INode;
    fState: IGenericAccessRO;
    fDataPath: string;
    fStoreConnector: IFluxData;
    fFluxFuncReg: IFluxFuncReg;
    fFuncSequence: ISequence;
    function GetState: IGenericAccessRO;
  published
    property ID: string read fID write fID;
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Node: INode read fNode write fNode;
    property State: IGenericAccessRO read GetState write fState;
    property DataPath: string read fDataPath write fDataPath;
    property StoreConnector: IFluxData read fStoreConnector write fStoreConnector;
    property FluxFuncReg: IFluxFuncReg read fFluxFuncReg write fFluxFuncReg;
    property FuncSequence: ISequence read fFuncSequence write fFuncSequence;
  end;

  { TFormData }

  TFormData = class
  private
    fLeft: Integer;
    fTop: Integer;
    fWidth: Integer;
    fHeight: Integer;
  published
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
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

  { TEditData }

  TEditData = class
  private
    fText: String;
    fFocused: Boolean;
  published
    property Text: String read fText write fText;
    property Focused: Boolean read fFocused write fFocused;
  end;

  { TDesignComponentEdit }

  TDesignComponentEdit = class(TDesignComponent, IDesignComponentEdit)
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  protected
    fData: TEditData;
    fAskNotifier: IFluxNotifier;
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

  { IGridData }

  IGridData = interface
  ['{B3B7B1B3-A738-4A44-BB6D-53E09BDAF8A9}']
    function GetValue(X, Y: integer): string;
    procedure SetValue(X, Y: integer; AValue: string);
    procedure CheckDimensions(AHorizontalCount, AVerticalCount: integer);
    property Value[X, Y: integer]: string read GetValue write SetValue; default;
  end;

  { TGridData }

  TGridData = class
  private type
   TMatrix = array of array of string;
  private
   fData: TMatrix;
   fEditData: TEditData;
   fDataRow: integer;
   fProvider: IGridDataProvider;
   fColCount: Integer;
   fRowCount: Integer;
   fCurrentRow: Integer;
   fCurrentCol: Integer;
   fBrowseMode: Boolean;
   function GetValue(Row, Col: Integer): string;
   procedure SetColCount(AValue: Integer);
   procedure SetCurrentCol(AValue: Integer);
   procedure SetCurrentRow(AValue: Integer);
   procedure SetRowCount(AValue: Integer);
   procedure SetValue(Row, Col: Integer; AValue: string);
   procedure Move(ADelta: Integer);
   procedure ReadDataRow(ARow: Integer);
   procedure ClearDataRow(ARow: Integer);
   procedure SynchronizeEditText;
  public
   constructor Create(const AProvider: IGridDataProvider);
   procedure BeforeDestruction; override;
   procedure ReadData;
   property ColCount: Integer read fColCount write SetColCount;
   property RowCount: Integer read fRowCount write SetRowCount;
   property CurrentRow: Integer read fCurrentRow write SetCurrentRow;
   property CurrentCol: Integer read fCurrentCol write SetCurrentCol;
   property BrowseMode: Boolean read fBrowseMode write fBrowseMode;
   property Value[Row, Col: Integer]: string read GetValue write SetValue; default;
   property EditData: TEditData read fEditData;
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
    procedure DoInitValues; override;
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
    .SetIntf('AskNotifier', AskNotifier)
    .SetIntf('TextChangedNotifier', TextChangedNotifier)
    .SetIntf('KeyDownNotifier', KeyDownNotifier)
    .SetBool('Focused', Data.Focused)
    .SetBool('Flat', SelfProps.AsBool('Flat'));
  Result := ElementFactory.CreateElement(IEditBit, mProps);
end;

{ TGridData }

function TGridData.GetValue(Row, Col: Integer): string;
begin
  Result := fData[Row, Col];
end;

procedure TGridData.SetColCount(AValue: Integer);
begin
  if fColCount = AValue then Exit;
  fColCount := AValue;
end;

procedure TGridData.SetCurrentCol(AValue: Integer);
begin
  if fCurrentCol = AValue then Exit;
  if (AValue >= Low(fData[CurrentRow])) or (AValue <= High(fData[CurrentRow])) then begin
    fCurrentCol := AValue;
    SynchronizeEditText;
  end;
end;

procedure TGridData.SetCurrentRow(AValue: Integer);
begin
  if fCurrentRow = AValue then Exit;
  if (AValue >= Low(fData)) or (AValue <= High(fData)) then begin
    fCurrentRow := AValue;
    SynchronizeEditText;
  end;
end;

procedure TGridData.SetRowCount(AValue: Integer);
begin
  if fRowCount=AValue then Exit;
  fRowCount:=AValue;
end;

procedure TGridData.SetValue(Row, Col: Integer; AValue: string);
begin
  fData[Row, Col] := AValue;
  Move(Row - fDataRow);
  fProvider[Col] := AValue;
end;

procedure TGridData.Move(ADelta: Integer);
var
  i: integer;
begin
  if ADelta > 0 then begin
    for i := 1 to ADelta do begin
      fProvider.Next;
      Inc(fDataRow);
    end;
  end else if ADelta < 0 then begin
    for i := ADelta to -1 do begin
      fProvider.Prev;
      Dec(fDataRow);
    end;
  end;
end;

procedure TGridData.ReadData;
var
  i: integer;
  mMoved: Boolean;
begin
  SetLength(fData, RowCount, ColCount);
  Move(-fDataRow);
  if fProvider.IsEmpty then begin
    for i := 0 to RowCount - 1 do begin
      ClearDataRow(i);
    end;
  end else begin
    for i := 0 to RowCount - 1 do begin
      if mMoved then begin
        ReadDataRow(fDataRow);
        mMoved := fProvider.Next;
        if mMoved then
          Inc(fDataRow);
      end else begin
        ClearDataRow(i);
      end;
    end;
  end;
  SynchronizeEditText;
end;

procedure TGridData.ReadDataRow(ARow: Integer);
var
  i: integer;
begin
  for i := 0 to ColCount - 1 do
    Value[ARow, i] := fProvider[i];
end;

procedure TGridData.ClearDataRow(ARow: Integer);
var
  i: integer;
begin
  for i := 0 to ColCount - 1 do
    Value[ARow, i] := '';
end;

procedure TGridData.SynchronizeEditText;
begin
  EditData.Text := Value[CurrentRow, CurrentCol];
  EditData.Focused := True;
end;

constructor TGridData.Create(const AProvider: IGridDataProvider);
begin
  fProvider := AProvider;
  fBrowseMode := True;
  fEditData := TEditData.Create;
end;

procedure TGridData.BeforeDestruction;
begin
  FreeAndNil(fEditData);
  inherited BeforeDestruction;
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
  AddChildren(Result, AChildren);
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
        .SetStr(cProps.DataPath, DataPath)
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

procedure TDesignComponentLabelEdit.DoInitValues;
begin
  inherited DoInitValues;
end;

function TDesignComponentLabelEdit.DoCompose(const AProps: IProps;
  const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, MakeProps, MakeLabeledEdits(AChildren));
end;

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

{ TDesignComponentFunc }

procedure TDesignComponentFunc.Execute(const AAction: IFluxAction);
begin
  DoExecute(AAction);
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
  if (fState = nil) and (DataPath <> '') then begin
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

function TDesignComponent.GetState: IGenericAccessRO;
begin
  if fState = nil then
     Raise Exception.Create('State is not initialized - maybe DataPath missing');
  Result := fState;
end;

end.

