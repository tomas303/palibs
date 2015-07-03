unit rtti_idebinder_uTallyBinders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, grids, controls, fgl,
  rtti_idebinder_iBindings, rtti_broker_uData, StdCtrls,
  rtti_idebinder_uGridBinders;


type

  EListBinder = class(Exception)

  end;

  { TTallyBinder }

  TTallyBinder = class(TInterfacedObject, IRBTallyBinder)
  private
    fControl: TWinControl;
    fContext: IRBBinderContext;
    fClass: TClass;
    fClassData: IRBData;
    fDataList: IRBDataList;
  protected
    property Control: TWinControl read fControl;
    property DataList: IRBDataList read fDataList;
    property ClassData: IRBData read fClassData;
    procedure BindControl; virtual; abstract;
    procedure RefreshData; virtual; abstract;
    function GetCurrentListIndex: integer; virtual; abstract;
  public
    //procedure DataToControl; virtual; abstract;
    procedure Bind(const AListControl: TWinControl; const AContext: IRBBinderContext;
      const AClass: TClass);
    procedure Reload;
    function GetCurrentData: IRBData;
    class function New(const AListControl: TWinControl; const AContext: IRBBinderContext;
      const AClass: TClass): IRBTallyBinder;
  end;


  TGridColumnBinder = class
  private
    fColumn: TGridColumn;
    fDataList: IRBDataList;
    fDataItemIndex: integer;
    fDataItemName: string;
    fClassData: IRBData;
  protected
    property Column: TGridColumn read fColumn;
    procedure ResetDataItemIndex;
  public
    function GetData(const ARow: integer): string;
    procedure Bind(const AColumn: TGridColumn; const ADataList: IRBDataList; AClassData: IRBData);
  end;

  TGridColumnBinders = specialize TFPGObjectList<TGridColumnBinder>;

  { TCustomDrawGridHelper }

  TCustomDrawGridHelper = class helper for TCustomDrawGrid
  public
    procedure DefaultDrawWithText(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState; aText: String);
  end;

  { TDrawGridBinder }

  TDrawGridBinder = class(TTallyBinder)
  private
    fColumnBinders: TGridColumnBinders;
    function GetGrid: TCustomDrawGrid;
  protected
    procedure ResetColumnBinders;
    procedure DrawCellEventHandler(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
    property Grid: TCustomDrawGrid read GetGrid;
    procedure BindControl; override;
    procedure RefreshData; override;
    function GetCurrentListIndex: integer; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TDrawGridBinder2 }

  TDrawGridBinder2 = class(TTallyBinder, IGridData)
  protected
    // IGridData
    procedure DeleteRow(ARow: integer);
    procedure InsertRow(ARow: integer);
    procedure EditRow(ARow: integer);
    function GetDataColCount: integer;
    function GetDataItemClass(ACol: integer): IRBDataItem;
    function GetDataRowCount: integer;
    function GetDataItem(ACol, ARow: integer): IRBDataItem;
    function GetColumnHeader(ACol: integer): string;
    function IndexOfCol(const AName: string): integer;
  private
    fGES: TGridBinder;
  protected
    procedure BindControl; override;
    procedure RefreshData; override;
    function GetCurrentListIndex: integer; override;
  protected
    function GetGrid: TCustomDrawGrid;
    property Grid: TCustomDrawGrid read GetGrid;
  public
    destructor Destroy; override;
  end;

  { TListBoxBinder }

  TListBoxBinder = class(TTallyBinder)
  private
    fDataItemIndex: integer;
    function GetListBox: TCustomListBox;
  protected
    procedure RefreshData; override;
    property ListBox: TCustomListBox read GetListBox;
    procedure BindControl; override;
    function GetCurrentListIndex: integer; override;
  end;

  { TComboBoxBinder }

  TComboBoxBinder = class(TTallyBinder)
  private
    fDataItemIndex: integer;
    function GetComboBox: TCustomComboBox;
  protected
    procedure RefreshData; override;
    property ComboBox: TCustomComboBox read GetComboBox;
    procedure BindControl; override;
    function GetCurrentListIndex: integer; override;
  end;

implementation

{ TDrawGridBinder2 }

procedure TDrawGridBinder2.DeleteRow(ARow: integer);
var
  mData: IRBData;
begin
  mData := GetCurrentData;
  if mData <> nil then begin
    fContext.DataStore.Delete(mData);
    fContext.DataStore.Flush;
  end;
  DataList.Delete(ARow);
end;

function TDrawGridBinder2.GetDataColCount: integer;
begin
  Result := fClassData.Count;
end;

function TDrawGridBinder2.GetDataItemClass(ACol: integer): IRBDataItem;
begin
  Result := fClassData[ACol];
end;

function TDrawGridBinder2.GetDataRowCount: integer;
begin
  Result := DataList.Count;
end;

procedure TDrawGridBinder2.InsertRow(ARow: integer);
var
  mData: IRBData;
begin
  mData := ClassData.ClassType.Create as IRBData;
  DataList.InsertData(ARow, mData);
end;

procedure TDrawGridBinder2.EditRow(ARow: integer);
var
  mData: IRBData;
begin
  mData := GetCurrentData;
  if mData <> nil then begin
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
  end;
end;


function TDrawGridBinder2.GetDataItem(ACol, ARow: integer): IRBDataItem;
begin
  Result := DataList.AsData[ARow][ACol];
end;

function TDrawGridBinder2.GetColumnHeader(ACol: integer): string;
begin
  Result := ClassData.Items[ACol].Name;
end;

function TDrawGridBinder2.IndexOfCol(const AName: string): integer;
begin
  Result := fClassData.ItemIndex[AName];
end;

function TDrawGridBinder2.GetGrid: TCustomDrawGrid;
begin
  Result := inherited Control as TCustomDrawGrid;
end;

procedure TDrawGridBinder2.BindControl;
begin
  FreeAndNil(fGES);
  fGES := TGridBinder.Create;
end;

procedure TDrawGridBinder2.RefreshData;
begin
  fGES.Bind(Grid, Self, fContext.DataQuery);
  fGES.DataToControl;
end;

function TDrawGridBinder2.GetCurrentListIndex: integer;
begin
  Result := Grid.Row - Grid.FixedRows;
end;

destructor TDrawGridBinder2.Destroy;
begin
  FreeAndNil(fGES);
  inherited Destroy;
end;

{ TComboBoxBinder }

function TComboBoxBinder.GetComboBox: TCustomComboBox;
begin
  Result := inherited Control as TCustomComboBox;
end;

procedure TComboBoxBinder.RefreshData;
var
  i: integer;
begin
  ComboBox.Clear;
  for i := 0 to DataList.Count - 1 do
  begin
    ComboBox.Items.Add(DataList.AsData[i][fDataItemIndex].AsString);
  end;
end;

procedure TComboBoxBinder.BindControl;
begin
  fDataItemIndex := 0;
  if (ComboBox.Items.Count > 0) then
  begin
    fDataItemIndex := ClassData.ItemIndex[ComboBox.Items[0]];
    if fDataItemIndex = -1 then
      fDataItemIndex := 0;
  end;
end;

function TComboBoxBinder.GetCurrentListIndex: integer;
begin
  Result := ComboBox.ItemIndex;
end;

{ TListBoxBinder }

function TListBoxBinder.GetListBox: TCustomListBox;
begin
  Result := inherited Control as TCustomListBox;
end;

procedure TListBoxBinder.RefreshData;
var
  i: integer;
begin
  ListBox.Clear;
  for i := 0 to DataList.Count - 1 do
  begin
    ListBox.Items.Add(DataList.AsData[i][fDataItemIndex].AsString);
  end;
end;

procedure TListBoxBinder.BindControl;
begin
  fDataItemIndex := 0;
  if (ListBox.Count > 0) then
  begin
    fDataItemIndex := ClassData.ItemIndex[ListBox.Items[0]];
    if fDataItemIndex = -1 then
      fDataItemIndex := 0;
  end;
end;

function TListBoxBinder.GetCurrentListIndex: integer;
begin
  Result := ListBox.ItemIndex;
end;

{ TCustomDrawGridHelper }

procedure TCustomDrawGridHelper.DefaultDrawWithText(aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState; aText: String);
begin
  DefaultDrawCell(aCol, aRow, aRect, aState);
  DrawCellText(aCol, aRow, aRect, aState, aText);
end;

{ TGridColumnBinder }

procedure TGridColumnBinder.ResetDataItemIndex;
var
  mItemName: string;
begin
  fDataItemIndex := -1;
  fDataItemName := '';
  if Column.PickList.Count > 0 then
  begin
    mItemName := Column.PickList[0];
    fDataItemIndex := fClassData.ItemIndex[mItemName];
    fDataItemName := mItemName;
  end;
end;

function TGridColumnBinder.GetData(const ARow: integer): string;
begin
  if fDataItemIndex = -1 then
    Result := ''
  else begin
    if (ARow >= 0) and (ARow < fDataList.Count) then
      Result := fDataList.AsData[ARow][fDataItemIndex].AsString
    else
      Result := '';
  end;
end;

procedure TGridColumnBinder.Bind(const AColumn: TGridColumn;
  const ADataList: IRBDataList; AClassData: IRBData);
begin
  fColumn := AColumn;
  fDataList := ADataList;
  fClassData := AClassData;
  ResetDataItemIndex;
end;

{ TDrawGridBinder }

function TDrawGridBinder.GetGrid: TCustomDrawGrid;
begin
  Result := inherited Control as TCustomDrawGrid;
end;

procedure TDrawGridBinder.ResetColumnBinders;
var
  i, mc: integer;
  mBinder: TGridColumnBinder;
begin
  fColumnBinders.Clear;
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    mBinder := TGridColumnBinder.Create;
    mBinder.Bind(Grid.Columns[i], DataList, ClassData);
    fColumnBinders.Add(mBinder);
  end;
  mc := fColumnBinders.Count;
end;

procedure TDrawGridBinder.DrawCellEventHandler(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  mData: string;
  mc: integer;
begin
  mc := fColumnBinders.Count;
  if (ACol < 0) or (ACol > mc - 1) then
    Exit;
  mData := fColumnBinders[aCol].GetData(aRow - 1);
  //DataList.AsData[aRow - 1][aCol].AsString := '??';
  TCustomDrawGrid(Sender).DefaultDrawWithText(aCol, aRow, aRect, aState, mData);
end;

procedure TDrawGridBinder.BindControl;
begin
  //RefreshData;
  Grid.DefaultDrawing := True;
  Grid.OnDrawCell := @DrawCellEventHandler;
end;

procedure TDrawGridBinder.RefreshData;
begin
  Grid.RowCount := Grid.FixedRows + DataList.Count;
  ResetColumnBinders;
end;

function TDrawGridBinder.GetCurrentListIndex: integer;
begin
  Result := Grid.Row - Grid.FixedRows;
end;

procedure TDrawGridBinder.AfterConstruction;
begin
  inherited AfterConstruction;
  fColumnBinders := TGridColumnBinders.Create;
end;

procedure TDrawGridBinder.BeforeDestruction;
begin
  FreeAndNil(fColumnBinders);
  inherited BeforeDestruction;
end;

{ TTallyBinder }

procedure TTallyBinder.Bind(const AListControl: TWinControl; const AContext: IRBBinderContext;
  const AClass: TClass);
begin
  fControl := AListControl;
  //fDataList := ADataList;
  fContext := AContext;
  fClass := AClass;
  fClassData := TRBData.Create(fClass, True);
  BindControl;
  Reload;
end;

procedure TTallyBinder.Reload;
begin
  fDataList := fContext.DataQuery.Retrive(fClass.ClassName);
  RefreshData;
end;

function TTallyBinder.GetCurrentData: IRBData;
var
  mIndex: integer;
begin
  mIndex := GetCurrentListIndex;
  if mIndex = -1 then
    Result := nil
  else
    Result := fDataList.AsData[mIndex];
end;

class function TTallyBinder.New(const AListControl: TWinControl; const AContext: IRBBinderContext;
  const AClass: TClass): IRBTallyBinder;
begin
  if AListControl is TCustomDrawGrid then
    //Result := TDrawGridBinder.Create
    Result := TDrawGridBinder2.Create
  else
  if AListControl is TCustomListBox then
      Result := TListBoxBinder.Create
  else
  if AListControl is TCustomComboBox then
      Result := TComboBoxBinder.Create
  else
    raise EListBinder.CreateFmt('For %s not existis binder', [AListControl.ClassName]);
  Result.Bind(AListControl, AContext, AClass);
end;

end.

