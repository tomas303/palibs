unit rea_udataconnector;

{$mode objfpc}{$H+}

interface

uses
  rea_idataconnector, rea_idesigncomponent, rea_udesigncomponentdata,
  rea_udesigncomponentfunc, rea_iflux, rea_ireafactory, trl_isequence;

type

  { TDataConnector }

  TDataConnector = class(TInterfacedObject, IDataConnector)
  protected
    procedure Connect(const AProvider: IGridDataProvider; AData: TGridData; AIndexes: array of Integer);
    procedure Connect(const AProvider: IGridDataProvider; AData: TEditData; AIndex: Integer);
  protected
    fFluxDispatcher: IFluxDispatcher;
    fReaFactory: IReaFactory;
    fSequence: ISequence;
  published
    property FluxDispatcher: IFluxDispatcher read fFluxDispatcher write fFluxDispatcher;
    property ReaFactory: IReaFactory read fReaFactory write fReaFactory;
    property Sequence: ISequence read fSequence write fSequence;
  end;

  { TDataToGUIFunc }

  TDataToGUIFunc = class(TDesignComponentFunc)
  private
    fProvider: IGridDataProvider;
    fProviderIndex: Integer;
    fData: TEditData;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(const AProvider: IGridDataProvider; AData: TEditData; AIndex: Integer);
  end;

  { TGUIToDataFunc }

  TGUIToDataFunc = class(TDesignComponentFunc)
  private
    fProvider: IGridDataProvider;
    fProviderIndex: Integer;
    fData: TEditData;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(const AProvider: IGridDataProvider; AData: TEditData; AIndex: Integer);
  end;

  { TGridDataToGUIFunc }

  TGridDataToGUIFunc = class(TDesignComponentFunc)
  private
    fProvider: IGridDataProvider;
    fIndexes: array of Integer;
    fData: TGridData;
    function Between(const ANumber, ALower, AUpper: Integer): Boolean;
    function MoveProvider(ADelta: Integer): Integer;
    function ReadRows: Integer;
    procedure ReadDataRow(ARow: Integer);
    procedure ClearDataRow(ARow: Integer);
    procedure ReadData(const ADeltaMove: Integer);
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(const AProvider: IGridDataProvider; AData: TGridData; AIndexes: array of Integer);
  end;

  { TGUIToGridDataFunc }

  TGUIToGridDataFunc = class(TDesignComponentFunc)
  private
    fProvider: IGridDataProvider;
    fIndexes: array of Integer;
    fData: TGridData;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(const AProvider: IGridDataProvider; AData: TGridData; AIndexes: array of Integer);
  end;

implementation

{ TGUIToGridDataFunc }

procedure TGUIToGridDataFunc.DoExecute(const AAction: IFluxAction);
begin
  fProvider.Value[fIndexes[fData.CurrentCol]] := fData.EditData.Text;
end;

constructor TGUIToGridDataFunc.Create(const AProvider: IGridDataProvider;
  AData: TGridData; AIndexes: array of Integer);
begin
  inherited Create(AData.EditData.ChangedNotifier.ActionID);
  fData := AData;
  fProvider := AProvider;
  fIndexes := Copy(AIndexes);
end;

{ TGUIToDataFunc }

procedure TGUIToDataFunc.DoExecute(const AAction: IFluxAction);
begin
  fProvider.Value[fProviderIndex] := fData.Text;
end;

constructor TGUIToDataFunc.Create(const AProvider: IGridDataProvider;
  AData: TEditData; AIndex: Integer);
begin
  inherited Create(AData.ChangedNotifier.ActionID);
  fData := AData;
  fProvider := AProvider;
  fProviderIndex := AIndex;
end;

{ TGridDataToGUIFunc }

function TGridDataToGUIFunc.Between(const ANumber, ALower, AUpper: Integer): Boolean;
begin
  Result := (ANumber >= ALower) and (ANumber <= AUpper);
end;

function TGridDataToGUIFunc.MoveProvider(ADelta: Integer): Integer;
begin
  Result := 0;
  if ADelta > 0 then begin
    while ADelta > 0 do
    begin
      if not fProvider.Next then
        Break;
      Inc(Result);
      Dec(ADelta);
    end;
  end else begin
    while ADelta < 0 do
    begin
      if not fProvider.Prev then
        Break;
      Inc(Result);
      Inc(ADelta);
    end;
  end;
end;

function TGridDataToGUIFunc.ReadRows: Integer;
var
  i: integer;
begin
  i := 0;
  Result := -1;
  if not fProvider.IsEmpty then begin
    Result := 0;
    repeat
      ReadDataRow(i);
      Inc(i);
      Inc(Result);
    until (i > fData.RowCount - 1) or not fProvider.Next;
  end;
  while i <= fData.RowCount - 1 do begin
    ClearDataRow(i);
    Inc(i);
  end;
end;

procedure TGridDataToGUIFunc.ReadDataRow(ARow: Integer);
var
  i: integer;
begin
  for i := 0 to fData.ColCount - 1 do
    fData.Value[ARow, i] := fProvider[fIndexes[i]];
end;

procedure TGridDataToGUIFunc.ClearDataRow(ARow: Integer);
var
  i: integer;
begin
  for i := 0 to fData.ColCount - 1 do
    fData.Value[ARow, i] := '';
end;

procedure TGridDataToGUIFunc.ReadData(const ADeltaMove: Integer);
var
  mBookmark: IInterface;
  mMovedRows: Integer;
begin
  mBookmark := fProvider.NewBookmark;
  try
    if fData.DataRow = -1 then begin
      if not fProvider.IsEmpty then begin
        fData.LastDataRow := ReadRows;
        fData.DataRow := 0;
        fData.CurrentRow := fData.DataRow;
      end;
    end
    else if Between(fData.DataRow + ADeltaMove, 0, fData.RowCount - 1) then begin
      fData.DataRow := fData.DataRow + ADeltaMove;
      fData.CurrentRow := fData.DataRow;
    end
    else begin
      mMovedRows := MoveProvider(-fData.DataRow);
      fData.DataRow := mMovedRows;
      fData.LastDataRow := ReadRows;
      fData.CurrentRow := fData.DataRow;
    end;
  finally
    fProvider.GotoBookmark(mBookmark);
  end;
end;

procedure TGridDataToGUIFunc.DoExecute(const AAction: IFluxAction);
begin
  fProvider.Silent := True;
  try
    ReadData(AAction.Props.AsInt('deltamove'));
  finally
    fProvider.Silent := False;
  end;
end;

constructor TGridDataToGUIFunc.Create(const AProvider: IGridDataProvider; AData: TGridData; AIndexes: array of Integer);
begin
  inherited Create(AProvider.MoveActionID);
  fData := AData;
  fProvider := AProvider;
  fIndexes := Copy(AIndexes);
end;

{ TDataToGUIFunc }

procedure TDataToGUIFunc.DoExecute(const AAction: IFluxAction);
begin
  fData.Text := fProvider.Value[fProviderIndex];
end;

constructor TDataToGUIFunc.Create(const AProvider: IGridDataProvider; AData: TEditData; AIndex: Integer);
begin
  inherited Create(AProvider.MoveActionID);
  fData := AData;
  fProvider := AProvider;
  fProviderIndex := AIndex;
end;

{ TDataConnector }

procedure TDataConnector.Connect(const AProvider: IGridDataProvider;
  AData: TGridData; AIndexes: array of Integer);
var
  mFunc: IFluxFunc;
begin
  mFunc := TGridDataToGUIFunc.Create(AProvider, AData, [0,1]);
  FluxDispatcher.RegisterFunc(mFunc);
  AData.EditData.ChangedNotifier := ReaFactory.NewNotifier(Sequence.Next);
  mFunc := TGUIToGridDataFunc.Create(AProvider, AData, [0,1]);
  FluxDispatcher.RegisterFunc(mFunc);
end;

procedure TDataConnector.Connect(const AProvider: IGridDataProvider;
  AData: TEditData; AIndex: Integer);
var
  mFunc: IFluxFunc;
begin
  mFunc := TDataToGUIFunc.Create(AProvider, AData, AIndex);
  FluxDispatcher.RegisterFunc(mFunc);
  AData.ChangedNotifier := ReaFactory.NewNotifier(Sequence.Next);
  mFunc := TGUIToDataFunc.Create(AProvider, AData, AIndex);
  FluxDispatcher.RegisterFunc(mFunc);
end;

end.

