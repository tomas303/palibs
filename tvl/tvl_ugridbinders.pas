unit tvl_ugridbinders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, controls, LCLProc, LCLType,
  lmessages, StdCtrls, MaskEdit, trl_irttibroker, trl_ipersist, tvl_udatabinders,
  trl_ipersiststore;

type

  { IGridData }

  IGridData = interface
    procedure DeleteRow(ARow: integer);
    procedure InsertRow(ARow: integer);
    procedure EditRow(ARow: integer);
    function GetColumnHeader(ACol: integer): string;
    function GetDataColCount: integer;
    function GetDataItemClass(ACol: integer): IRBDataItem;
    function GetDataRowCount: integer;
    function GetDataItem(ACol, ARow: integer): IRBDataItem;
    property DataItem[ACol, ARow: integer]: IRBDataItem read GetDataItem;
    property DataItemClass[ACol: integer]: IRBDataItem read GetDataItemClass;
    property DataRowCount: integer read GetDataRowCount;
    property DataColCount: integer read GetDataColCount;
    property ColumnHeader[ACol: integer]: string read GetColumnHeader;
    function IndexOfCol(const AName: string): integer;
  end;

  { TGridBinder }

  TGridBinder = class

  protected type

    { TEditorSupport }

    TEditorSupport = class
    private
      fEditor: TWinControl;
      fOldWndProc: TWndMethod;
      fGrid: TCustomGrid;
      fCol, fRow:Integer;
      fff: integer;
    protected
      procedure EditorWndProc(var TheMessage: TLMessage);
      procedure KeyDown(var Key : Word; Shift : TShiftState);
      procedure EditingDone;
      function GetRect: TRect;
      procedure SetGrid(var Msg: TGridMessage);
      procedure SetPos(var Msg: TGridMessage);
      procedure GetGrid(var Msg: TGridMessage);
    public
      constructor Create(AEditor: TWinControl);
    end;

    { TOfferEditor }

    TOfferEditor = class(TCustomComboBox)
    private
      fSupport: TEditorSupport;
    protected
      procedure KeyDown(var Key : Word; Shift : TShiftState); override;
      procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
      procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
      procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
      procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
      procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure EditingDone; override;
    end;

    { TTextEditor }

    TTextEditor = class(TCustomMaskEdit)
    private
      fSupport: TEditorSupport;
    protected
      procedure KeyDown(var Key : Word; Shift : TShiftState); override;
      procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
      procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
      procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
      procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
      procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure EditingDone; override;
    end;

  private
    fGrid: TCustomDrawGrid;
    fGridData: IGridData;
    fDataQuery: IPersistQuery;
    fCellEditor: TWinControl;
    fCellBinder: TEditBinder;
  protected
    function CreateEditor(const ADataItem: IRBDataItem): TWinControl;
    function CreateBinder(const ADataItem: IRBDataItem): TEditBinder;
    procedure BindColumns;
    function GetCellDataItem(ACellCol, ACellRow: integer): IRBDataItem;
    function GetCellDataItemText(ACellCol, ACellRow: integer): string;
    function GetGrid: TCustomDrawGrid;
    property Grid: TCustomDrawGrid read GetGrid;
  protected
    // events
    procedure DrawCellEventHandler(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
    procedure GetEditTextHandler(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnSelectEditorHandler(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OnColRowInsertedHandler(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OnColRowDeletedHandler(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OnEditingDoneHandler(Sender: TObject);
  public
    destructor Destroy; override;
    procedure Bind(AGrid: TCustomDrawGrid; AGridData: IGridData; ADataQuery: IPersistQuery);
    procedure DataToControl;
    property CellDataItem[ACellCol, ACellRow: integer]: IRBDataItem read GetCellDataItem;
  end;

implementation

type

  { TGridHelper }

  TGridHelper = class helper for TCustomGrid
  private
    function GetH_FastEditing: boolean;
    procedure SetH_FastEditing(AValue: boolean);
  public
    procedure H_EditorTextChanged(const aCol,aRow: Integer; const aText:string);
    function  H_EditorIsReadOnly: boolean;
    procedure H_SetEditText(ACol, ARow: Longint; const Value: string);
    procedure H_KeyDown(var Key : Word; Shift : TShiftState);
    procedure H_DoOPDeleteColRow(IsColumn: Boolean; index: Integer);
    procedure H_DoOPInsertColRow(IsColumn: Boolean; index: Integer);
    property H_FastEditing: boolean read GetH_FastEditing write SetH_FastEditing;
  end;

  { TControlHelper }

  TControlHelper = class helper for TControl
  private
    function GetH_OnEditingDone: TNotifyEvent;
    procedure SetH_OnEditingDone(AValue: TNotifyEvent);
  public
    property H_OnEditingDone: TNotifyEvent read GetH_OnEditingDone write SetH_OnEditingDone;
  end;

  { TCustomDrawGridHelper }

  TCustomDrawGridHelper = class helper for TCustomDrawGrid
  public
    procedure DefaultDrawWithText(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState; aText: String);
  end;

{ TCustomDrawGridHelper }

procedure TCustomDrawGridHelper.DefaultDrawWithText(aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState; aText: String);
begin
  DefaultDrawCell(aCol, aRow, aRect, aState);
  DrawCellText(aCol, aRow, aRect, aState, aText);
end;


{ TGridHelper }

function TGridHelper.GetH_FastEditing: boolean;
begin
  Result := FastEditing;
end;

procedure TGridHelper.SetH_FastEditing(AValue: boolean);
begin
  FastEditing := AValue;
end;

procedure TGridHelper.H_EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
begin
  EditorTextChanged(aCol, aRow, aText);
end;

function TGridHelper.H_EditorIsReadOnly: boolean;
begin
  Result := EditorIsReadOnly;
end;

procedure TGridHelper.H_SetEditText(ACol, ARow: Longint; const Value: string);
begin
  SetEditText(ACol, ARow, Value);
end;

procedure TGridHelper.H_KeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TGridHelper.H_DoOPDeleteColRow(IsColumn: Boolean; index: Integer);
begin
  DoOPDeleteColRow(IsColumn, index);
end;

procedure TGridHelper.H_DoOPInsertColRow(IsColumn: Boolean; index: Integer);
begin
  DoOPInsertColRow(IsColumn, index);
end;

{ TControlHelper }

function TControlHelper.GetH_OnEditingDone: TNotifyEvent;
begin
  Result := OnEditingDone;
end;

procedure TControlHelper.SetH_OnEditingDone(AValue: TNotifyEvent);
begin
  OnEditingDone := AValue;
end;

{ TGridBinder.TTextEditor }

procedure TGridBinder.TTextEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  fSupport.KeyDown(Key, Shift);
end;

procedure TGridBinder.TTextEditor.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  m: TRect;
begin
  m := fSupport.GetRect;
  inherited DoSetBounds(m.Left, m.Top, m.Right - m.Left + 1, m.Bottom - m.Top + 1);
end;

procedure TGridBinder.TTextEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  fSupport.SetGrid(Msg);
end;

procedure TGridBinder.TTextEditor.msg_SetPos(var Msg: TGridMessage);
begin
  fSupport.SetPos(Msg);
end;

procedure TGridBinder.TTextEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  fSupport.GetGrid(Msg);
end;

procedure TGridBinder.TTextEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
end;

constructor TGridBinder.TTextEditor.Create(AOwner: TComponent);
begin
  fSupport := TEditorSupport.Create(Self);
  inherited Create(AOwner);
  AutoSize := false;
  BorderStyle := bsNone;
end;

destructor TGridBinder.TTextEditor.Destroy;
begin
  FreeAndNil(fSupport);
  inherited Destroy;
end;

procedure TGridBinder.TTextEditor.EditingDone;
begin
  inherited EditingDone;
  fSupport.EditingDone;
end;

{ TGridBinder.TEditorSupport }

procedure TGridBinder.TEditorSupport.EditorWndProc(var TheMessage: TLMessage
  );
begin
  case TheMessage.Msg of
    LM_KEYDOWN:
      begin
        Exit;
      end;
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      begin
        if (fGrid <> nil) and (fGrid.H_EditorIsReadOnly) then
          exit;
      end;
  end;
  fOldWndProc(TheMessage);
end;

procedure TGridBinder.TEditorSupport.KeyDown(var Key: Word;
  Shift: TShiftState);

  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;

  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      FGrid.H_KeyDown(Key, shift);
  end;

  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := FGrid.H_FastEditing
    else
      Result := False;
  end;

  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or FGrid.H_EditorIsReadOnly then
      Key := 0;
  end;

var
  IntSel: boolean;
begin
  case Key of
    VK_DELETE:
       CheckEditingKey;
    VK_BACK:
      CheckEditingKey;
    VK_UP:
      doGridKeyDown;
    VK_DOWN:
      begin
        EditingDone;
        doGridKeyDown;
      end;
    VK_LEFT, VK_RIGHT:;
      //if GetFastEntry then begin
        {
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
        if not IntSel then begin}
    VK_END, VK_HOME:
      ;
    else
      doEditorKeyDown;

  end;
end;

procedure TGridBinder.TEditorSupport.EditingDone;
begin
  if fGrid <> nil then
    fGrid.EditingDone;
end;

function TGridBinder.TEditorSupport.GetRect: TRect;
begin
  if (FGrid <> nil) and (fCol >= 0) and (fRow >= 0) then
    Result := FGrid.CellRect(fCol, fRow)
  else begin
    Result := Bounds(fEditor.Left, fEditor.Top, fEditor.Width, fEditor.Height);
  end;
  //InflateRect(Result, -2, -2);
  Result.Top := Result.Top;
  Result.Left := Result.Left;
  Result.Bottom := Result.Bottom - 2;
  Result.Right := Result.Right - 2;
end;

procedure TGridBinder.TEditorSupport.SetGrid(var Msg: TGridMessage);
begin
  fGrid := Msg.Grid;
  Msg.Options := EO_AUTOSIZE or EO_SELECTALL {or EO_HOOKKEYPRESS or EO_HOOKKEYUP};
end;

procedure TGridBinder.TEditorSupport.SetPos(var Msg: TGridMessage);
begin
  fCol := Msg.Col;
  fRow := Msg.Row;
end;

procedure TGridBinder.TEditorSupport.GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TGridBinder.TEditorSupport.Create(AEditor: TWinControl);
begin
  fEditor := AEditor;
  fOldWndProc := fEditor.WindowProc;
  fEditor.WindowProc := @EditorWndProc;
end;

{ TGridBinder.TOfferEditor }

procedure TGridBinder.TOfferEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  fSupport.KeyDown(Key, Shift);
end;

procedure TGridBinder.TOfferEditor.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  m: TRect;
begin
  m := fSupport.GetRect;
  inherited DoSetBounds(m.Left, m.Top, m.Right - m.Left + 1, m.Bottom - m.Top + 1);
end;

procedure TGridBinder.TOfferEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  fSupport.SetGrid(Msg);
end;

procedure TGridBinder.TOfferEditor.msg_SetPos(var Msg: TGridMessage);
begin
  fSupport.SetPos(Msg);
end;

procedure TGridBinder.TOfferEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  fSupport.GetGrid(Msg);
end;

procedure TGridBinder.TOfferEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
end;

constructor TGridBinder.TOfferEditor.Create(AOwner: TComponent);
begin
  fSupport := TEditorSupport.Create(Self);
  inherited Create(AOwner);
  AutoSize := false;
  BorderStyle := bsNone;
end;

destructor TGridBinder.TOfferEditor.Destroy;
begin
  FreeAndNil(fSupport);
  inherited Destroy;
end;

procedure TGridBinder.TOfferEditor.EditingDone;
begin
  inherited EditingDone;
  fSupport.EditingDone;
end;

{ TGridBinder }

function TGridBinder.GetGrid: TCustomDrawGrid;
begin
  Result := fGrid;
end;

function TGridBinder.GetCellDataItem(ACellCol, ACellRow: integer): IRBDataItem;
var
  mColDelta, mRowDelta: integer;
begin
  if ACellRow = 0 then
    Result := nil
  else
    Result := fGridData.DataItem[Grid.Columns[ACellCol].Tag, ACellRow - 1];
end;

function TGridBinder.GetCellDataItemText(ACellCol, ACellRow: integer): string;
var
  mDItem: IRBDataItem;
begin
  Result := '';
  if ACellRow = 0 then  // header
    Exit;
  mDItem := CellDataItem[ACellCol, ACellRow];
  if mDItem <> nil then
  begin
    if mDItem.IsObject then
    begin
      Result := '[Object]';
    end
    else
    //if mDItem.IsList then
    //begin
    //  Result := '[List]';
    //end
    //else
    begin
      Result := mDItem.AsString;
    end;
  end;
end;

function TGridBinder.CreateEditor(const ADataItem: IRBDataItem): TWinControl;
begin
  Result := nil;
  //if ADataItem.IsObject and ADataItem.IsReference then
  //begin
  //  Result := TOfferEditor.Create(Grid);
  //end
  //else
  begin
    if ADataItem.EnumNameCount > 0 then
    begin
      Result := TOfferEditor.Create(Grid);
    end
    else
    begin
      Result := TTextEditor.Create(Grid);
    end;
  end;
end;

function TGridBinder.CreateBinder(const ADataItem: IRBDataItem): TEditBinder;
begin
  Result := nil;
  //if ADataItem.IsObject and ADataItem.IsReference then
  //begin
  //  Result := TOfferObjectRefBinder.Create;
  //end
  //else
  if ADataItem.EnumNameCount > 0 then
  begin
    Result := TOfferEnumBinder.Create;
  end
  else
  begin
    Result := TTextBinder.Create;
  end;
end;

procedure TGridBinder.OnColRowDeletedHandler(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
  i: integer;
begin
  if not IsColumn then
  begin
    for i := sIndex to tIndex do
      fGridData.DeleteRow(i);
  end;
end;

procedure TGridBinder.OnColRowInsertedHandler(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
  i: integer;
begin
  if not IsColumn then
  begin
    for i := sIndex to tIndex do
    begin
      fGridData.InsertRow(i - 1);
    end;
  end;
end;

procedure TGridBinder.OnEditingDoneHandler(Sender: TObject);
begin
  fGridData.EditRow(Grid.Row - 1);
end;

procedure TGridBinder.OnSelectEditorHandler(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  mOldEd: TWinControl;
  mDataItem: IRBDataItem;
begin
  mOldEd := fCellEditor;
  FreeAndNil(fCellBinder);
  mDataItem := CellDataItem[aCol, ARow];
  fCellEditor := CreateEditor(mDataItem);
  fCellBinder := CreateBinder(mDataItem);
  //if mDataItem.IsList then
  //  fCellBinder.Bind(fCellEditor, mDataItem, aRow - 1, fDataQuery)
  //else
    fCellBinder.Bind(fCellEditor, mDataItem, fDataQuery);
  Editor := fCellEditor;
  mOldEd.Free;
end;

procedure TGridBinder.OnKeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Shift = [ssCtrl]) then
  begin
    if Grid.Row > 1 then
    begin
      Grid.H_DoOPDeleteColRow(False, Grid.Row);
    end;
    Key := 0;
  end else
  if (Key = VK_INSERT) and (Shift = [ssCtrl]) then
  begin
    Grid.H_DoOPInsertColRow(False, Grid.Row);
    Grid.Row := Grid.Row - 1;
    Key := 0;
  end else
    inherited;
end;

procedure TGridBinder.DrawCellEventHandler(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  mData: string;
  mDItem: IRBDataItem;
begin
  if fGridData = nil then
    mData := ''
  else
    mData := GetCellDataItemText(ACol, ARow);
  TCustomDrawGrid(Sender).DefaultDrawWithText(aCol, aRow, aRect, aState, mData);
end;

procedure TGridBinder.GetEditTextHandler(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  Value := GetCellDataItemText(ACol, ARow);
end;

procedure TGridBinder.BindColumns;
var
  i: integer;
  mCol: TGridColumn;
  mName: string;
begin
  if Grid.Columns.Enabled then begin
    for i := 0 to Grid.Columns.Count - 1 do begin
      // bind name via PickList
      mName := Trim(Grid.Columns[i].PickList.Text);
      Grid.Columns[i].Tag := fGridData.IndexOfCol(mName);
      Grid.Columns[i].PickList.Text := '';
    end;
  end else begin
    for i := 0 to fGridData.DataColCount - 1 do begin
      mCol := Grid.Columns.Add;
      mCol.Title.Caption := fGridData.ColumnHeader[i];
      mCol.Tag := i;
    end;
  end;
end;

destructor TGridBinder.Destroy;
begin
  inherited Destroy;
end;

procedure TGridBinder.Bind(AGrid: TCustomDrawGrid; AGridData: IGridData; ADataQuery: IPersistQuery);
var
  mData: IRBData;
  i: integer;
begin
  fGrid := AGrid;
  fGridData := AGridData;
  fDataQuery := ADataQuery;
  // grid layout (top row for captions)
  Grid.RowCount := 1 + fGridData.DataRowCount;
  if not Grid.Columns.Enabled then
    Grid.ColCount := fGridData.DataColCount;
  Grid.FixedRows := 1;
  Grid.FixedCols := 0;
  // process columns (join against columns in data)
  BindColumns;
  // events
  Grid.AutoFillColumns := True;
  Grid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, {goAutoAddRows, goAutoAddRowsSkipContentCheck,} goTabs, {goAlwaysShowEditor,} goSmoothScroll];
  Grid.OnColRowInserted := @OnColRowInsertedHandler;
  Grid.OnColRowDeleted := @OnColRowDeletedHandler;
  Grid.OnSelectEditor := @OnSelectEditorHandler;
  Grid.OnKeyDown := @OnKeyDownHandler;
  Grid.H_OnEditingDone := @OnEditingDoneHandler;
  Grid.DefaultDrawing := True;
  Grid.OnDrawCell := @DrawCellEventHandler;
  Grid.OnGetEditText := @GetEditTextHandler;
end;

procedure TGridBinder.DataToControl;
var
  mRow, mCol, mRowDelta: integer;
  mDataItem: IRBDataItem;
begin
  //mRowDelta := Grid.FixedRows;
  //for mRow := 0 to fGridData.DataRowCount - 1 do
  //begin
  //  for mCol := 0 to fGridData.DataColCount - 1 do
  //  begin
  //    mDataItem := fGridData.DataItem[mCol, mRow];
  //    if mDataItem.IsObject then
  //    begin
  //      Grid.Cells[mCol, mRow + mRowDelta] := '[Object]';
  //    end
  //    else
  //    if mDataItem.IsList then
  //    begin
  //      Grid.Cells[mCol, mRow + mRowDelta] := '[List]';
  //    end
  //    else
  //    begin
  //      Grid.Cells[mCol, mRow + mRowDelta] := fGridData.DataItem[mCol, mRow].AsString;
  //    end;
  //  end;
  //end;
end;

end.

