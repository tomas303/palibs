unit tvl_udatabinders;

interface

uses
  Classes, SysUtils, trl_irttibroker, Controls, StdCtrls, ExtCtrls, fgl,
  Graphics, Grids, MaskEdit, lmessages, LCLProc, LCLType,
  Menus, SynEdit, trl_ipersist, trl_upersist, tvl_messages, lclintf, messages;

type

  TBinderChangeEvent = procedure of object;

  TBinderChangeEvents = specialize TFPGList<TBinderChangeEvent>;

  { TEditBinder }

  TEditBinder = class
  private
    fControl: TWinControl;
    fDataItem: IRBDataItem;
    fDataQuery: IPersistQuery;
    fChangeEvents: TBinderChangeEvents;
  protected
    property DataQuery: IPersistQuery read fDataQuery;
    procedure BindControl; virtual; abstract;
    procedure UnbindControl; virtual;
    procedure NotifyChangeEvents;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DataToControl; virtual; abstract;
    procedure Bind(const AControl: TWinControl; const ADataItem: IRBDataItem;
      const ADataQuery: IPersistQuery);
    procedure RegisterChangeEvent(AEvent: TBinderChangeEvent);
    procedure UnregisterChangeEvent(AEvent: TBinderChangeEvent);
    property Control: TWinControl read fControl;
    property DataItem: IRBDataItem read fDataItem;
  end;

  { TTextBinder }

  TTextBinder = class(TEditBinder)
  private
    function GetControl: TCustomEdit;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    property Control: TCustomEdit read GetControl;
  public
    procedure DataToControl; override;
  end;

  { TMemoBinder }

  TMemoBinder = class(TEditBinder)
  private
    function GetControl: TCustomSynEdit;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    property Control: TCustomSynEdit read GetControl;
  public
    procedure DataToControl; override;
  end;

  { TBoolBinder }

  TBoolBinder = class(TEditBinder)
  private
    function GetControl: TCustomCheckBox;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    property Control: TCustomCheckBox read GetControl;
  public
    procedure DataToControl; override;
  end;

  { TOfferBinder }

  TOfferBinder = class(TEditBinder)
  private
    fKeyDownItemIndex: integer;
    fOldWndProc: TWndMethod;
    function GetControl: TCustomComboBox;
    procedure OnChangeHandler(Sender: TObject);
    procedure ControlWndProc(var TheMessage: TLMessage);
  protected
    procedure FillOffer; virtual; abstract;
    procedure OfferToData; virtual; abstract;
    procedure DataToOffer; virtual; abstract;
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    property Control: TCustomComboBox read GetControl;
  public
    procedure DataToControl; override;
  end;

  { TOfferObjectRefBinder }

  TOfferObjectRefBinder = class(TOfferBinder)
  private
    fOffer: IPersistList;
  protected
    procedure FillOffer; override;
    procedure OfferToData; override;
    procedure DataToOffer; override;
  protected
    procedure FillControlItems;
  end;

  { TOfferEnumBinder }

  TOfferEnumBinder = class(TOfferBinder)
  protected
    procedure FillOffer; override;
    procedure OfferToData; override;
    procedure DataToOffer; override;
  end;

  { TListBinder }

  TListBinder = class(TEditBinder)
  protected type

    { TGridEditorSupport }

    TGridEditorSupport = class
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
      fSupport: TGridEditorSupport;
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
      fSupport: TGridEditorSupport;
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
    fCellEditor: TWinControl;
    fCellValueData: IRBData;
    fCellBinder: TEditBinder;
    fObjectData: IRBData;
    fOldEd: TWinControl;
    fOldWndProc: TWndMethod;
    procedure ControlWndProc(var TheMessage: TLMessage);
    function GetAsMany: IPersistMany;
    function GetControl: TCustomStringGrid;
    procedure FillRowFromObject(ARow: integer; AObjectData: IRBData);
    procedure EmptyRow(ARow: integer);
    function CreateEditor(const ADataItem: IRBDataItem): TWinControl;
    function CreateBinder(const ADataItem: IRBDataItem): TEditBinder;
    procedure OnColRowDeletedHandler(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OnColRowInsertedHandler(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OnEditingDoneHandler(Sender: TObject);
    procedure OnSelectEditorHandler(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    property Control: TCustomStringGrid read GetControl;
    property AsMany: IPersistMany read GetAsMany;
  public
    procedure DataToControl; override;
  end;

implementation

type

  { TControlHelper }

  TControlHelper = class helper for TControl
  private
    function GetH_OnEditingDone: TNotifyEvent;
    procedure SetH_OnEditingDone(AValue: TNotifyEvent);
  public
    property H_OnEditingDone: TNotifyEvent read GetH_OnEditingDone write SetH_OnEditingDone;
  end;

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
    property H_FastEditing: boolean read GetH_FastEditing write SetH_FastEditing;
  end;

  { TCustomComboBoxHelper }

  TCustomComboBoxHelper = class helper for TCustomComboBox
  private
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(AValue: TNotifyEvent);
  public
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
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

{ TMemoBinder }

function TMemoBinder.GetControl: TCustomSynEdit;
begin
  Result := inherited Control as TCustomSynEdit;
end;

procedure TMemoBinder.OnChangeHandler(Sender: TObject);
begin
  DataItem.AsString := Control.Text;
  NotifyChangeEvents;
end;

procedure TMemoBinder.BindControl;
begin
  Control.OnChange := @OnChangeHandler;
end;

procedure TMemoBinder.DataToControl;
begin
  Control.Text := DataItem.AsString;
end;

{ TBoolBinder }

function TBoolBinder.GetControl: TCustomCheckBox;
begin
  Result := inherited Control as TCustomCheckBox;
end;

procedure TBoolBinder.OnChangeHandler(Sender: TObject);
begin
  DataItem.AsBoolean := Control.State = cbChecked;
  NotifyChangeEvents;
end;

procedure TBoolBinder.BindControl;
begin
  Control.OnChange := @OnChangeHandler;
end;

procedure TBoolBinder.DataToControl;
begin
  if DataItem.AsBoolean then
    Control.State := cbChecked
  else
    Control.State := cbUnchecked;
end;

{ TOfferEnumBinder }

procedure TOfferEnumBinder.FillOffer;
var
  i: integer;
begin
  Control.Items.Clear;
  for i := 0 to DataItem.EnumNameCount - 1 do
  begin
    Control.Items.Add(DataItem.EnumName[i]);
  end;
end;

procedure TOfferEnumBinder.OfferToData;
begin
  if Control.ItemIndex <> -1 then
  begin
    DataItem.AsString := Control.Items[Control.ItemIndex];
  end;
end;

procedure TOfferEnumBinder.DataToOffer;
var
  i: integer;
begin
  for i := 0 to Control.Items.Count - 1 do
  begin
    if DataItem.AsString = Control.Items[i] then
    begin
      Control.ItemIndex := i;
      Exit;
    end;
  end;
  Control.ItemIndex := -1;
end;

{ TListBinder.TTextEditor }

procedure TListBinder.TTextEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  fSupport.KeyDown(Key, Shift);
end;

procedure TListBinder.TTextEditor.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  m: TRect;
begin
  m := fSupport.GetRect;
  inherited DoSetBounds(m.Left, m.Top, m.Right - m.Left + 1, m.Bottom - m.Top + 1);
end;

procedure TListBinder.TTextEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  fSupport.SetGrid(Msg);
end;

procedure TListBinder.TTextEditor.msg_SetPos(var Msg: TGridMessage);
begin
  fSupport.SetPos(Msg);
end;

procedure TListBinder.TTextEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  fSupport.GetGrid(Msg);
end;

procedure TListBinder.TTextEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
end;

constructor TListBinder.TTextEditor.Create(AOwner: TComponent);
begin
  fSupport := TGridEditorSupport.Create(Self);
  inherited Create(AOwner);
  AutoSize := false;
  BorderStyle := bsNone;
end;

destructor TListBinder.TTextEditor.Destroy;
begin
  FreeAndNil(fSupport);
  inherited Destroy;
end;

procedure TListBinder.TTextEditor.EditingDone;
begin
  inherited EditingDone;
  fSupport.EditingDone;
end;

{ TListBinder.TGridEditorSupport }

procedure TListBinder.TGridEditorSupport.EditorWndProc(var TheMessage: TLMessage
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

procedure TListBinder.TGridEditorSupport.KeyDown(var Key: Word;
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

procedure TListBinder.TGridEditorSupport.EditingDone;
begin
  if fGrid <> nil then
    fGrid.EditingDone;
end;

function TListBinder.TGridEditorSupport.GetRect: TRect;
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

procedure TListBinder.TGridEditorSupport.SetGrid(var Msg: TGridMessage);
begin
  fGrid := Msg.Grid;
  Msg.Options := EO_AUTOSIZE or EO_SELECTALL {or EO_HOOKKEYPRESS or EO_HOOKKEYUP};
end;

procedure TListBinder.TGridEditorSupport.SetPos(var Msg: TGridMessage);
begin
  fCol := Msg.Col;
  fRow := Msg.Row;
end;

procedure TListBinder.TGridEditorSupport.GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TListBinder.TGridEditorSupport.Create(AEditor: TWinControl);
begin
  fEditor := AEditor;
  fOldWndProc := fEditor.WindowProc;
  fEditor.WindowProc := @EditorWndProc;
end;

{ TListBinder.TOfferEditor }

procedure TListBinder.TOfferEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  fSupport.KeyDown(Key, Shift);
end;

procedure TListBinder.TOfferEditor.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  m: TRect;
begin
  m := fSupport.GetRect;
  inherited DoSetBounds(m.Left, m.Top, m.Right - m.Left + 1, m.Bottom - m.Top + 1);
end;

procedure TListBinder.TOfferEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  fSupport.SetGrid(Msg);
end;

procedure TListBinder.TOfferEditor.msg_SetPos(var Msg: TGridMessage);
begin
  fSupport.SetPos(Msg);
end;

procedure TListBinder.TOfferEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  fSupport.GetGrid(Msg);
end;

procedure TListBinder.TOfferEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
end;

constructor TListBinder.TOfferEditor.Create(AOwner: TComponent);
begin
  fSupport := TGridEditorSupport.Create(Self);
  inherited Create(AOwner);
  AutoSize := false;
  BorderStyle := bsNone;
end;

destructor TListBinder.TOfferEditor.Destroy;
begin
  FreeAndNil(fSupport);
  inherited Destroy;
end;

procedure TListBinder.TOfferEditor.EditingDone;
begin
  inherited EditingDone;
  fSupport.EditingDone;
end;

{ TOfferObjectRefBinder }

procedure TOfferObjectRefBinder.FillOffer;
begin
  fOffer := DataQuery.Retrive(DataItem.ClassName);
  Control.Items.Clear;
  FillControlItems;
end;

procedure TOfferObjectRefBinder.OfferToData;
var
  mOfferIndex: NativeInt;
begin
  if Control.ItemIndex <> -1 then
  begin
    mOfferIndex := NativeInt(Control.Items.Objects[Control.ItemIndex]);
    DataItem.AsObject := fOffer[mOfferIndex]
  end;
end;

procedure TOfferObjectRefBinder.DataToOffer;
var
  i: integer;
  mO, mOffErO: TObject;
begin
  mO := DataItem.AsObject;
  for i := 0 to fOffer.Count - 1 do begin
    mOffErO := fOffer[i];
    if mOfferO = mO then
    begin
      Control.ItemIndex := i;
      Exit;
    end;
  end;
  Control.ItemIndex := -1;
end;

procedure TOfferObjectRefBinder.FillControlItems;
var
  i: NativeInt;
  mData: IRBData;
begin
  for i := 0 to fOffer.Count - 1 do
  begin
    mData := fOffer.AsData[i];
    if not SameText(mData[0].Name, 'id') or (mData.Count = 1) then
      Control.Items.AddObject(fOffer.AsData[i][0].AsString, TObject(i))
    else
      Control.Items.AddObject(fOffer.AsData[i][1].AsString, TObject(i));
  end;
end;

{ TCustomComboBoxHelper }

function TCustomComboBoxHelper.GetOnChange: TNotifyEvent;
begin
  Result := inherited OnChange;
end;

procedure TCustomComboBoxHelper.SetOnChange(AValue: TNotifyEvent);
begin
  inherited OnChange := AValue;
end;

{ TOfferBinder }

function TOfferBinder.GetControl: TCustomComboBox;
begin
  Result := inherited Control as TCustomComboBox;
end;

procedure TOfferBinder.OnChangeHandler(Sender: TObject);
begin
  OfferToData;
  NotifyChangeEvents;
end;

procedure TOfferBinder.ControlWndProc(var TheMessage: TLMessage);
begin
  case TheMessage.Msg of
    LM_KEYDOWN:
      fKeyDownItemIndex := Control.ItemIndex;
    LM_KEYUP:
      if fKeyDownItemIndex <> Control.ItemIndex then begin
        // key can cause autcomplete and via that change of ItemIndex
        // and this is not reported via OnChange event
        OnChangeHandler(Control);
      end;
  end;
  fOldWndProc(TheMessage);
end;

procedure TOfferBinder.BindControl;
begin
  FillOffer;
  Control.OnChange := @OnChangeHandler;
  fOldWndProc := Control.WindowProc;
  Control.WindowProc := @ControlWndProc;
  Control.AutoComplete := True;
end;

procedure TOfferBinder.UnbindControl;
begin
  Control.OnChange := nil;
  Control.WindowProc := fOldWndProc;
  inherited UnbindControl;
end;

procedure TOfferBinder.DataToControl;
begin
  DataToOffer;
end;

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

{ TListBinder }

function TListBinder.GetControl: TCustomStringGrid;
begin
  Result := inherited Control as TCustomStringGrid;
end;

procedure TListBinder.ControlWndProc(var TheMessage: TLMessage);
begin
  case TheMessage.Msg of
    TVLM_GRIDSETPOS:
      begin
        if TheMessage.WParam <> -1 then
          Control.Row := TheMessage.WParam;
        if TheMessage.LParam <> -1 then
          Control.Col := TheMessage.LParam;
      end;
    else
      fOldWndProc(TheMessage);
  end;
end;

function TListBinder.GetAsMany: IPersistMany;
begin
  if DataItem.IsObject then
    Result := DataItem.AsObject as IPersistMany
  else
  if DataItem.IsInterface then
    Result := DataItem.AsInterface as IPersistMany
  else
    raise Exception.Create('not object nor interface property for TLIST, unable retrieve IPersistMany');
end;

procedure TListBinder.FillRowFromObject(ARow: integer; AObjectData: IRBData);
var
  i: integer;
begin
  if AObjectData = nil then
    EmptyRow(ARow)
  else begin
    if Control.ColCount <> AObjectData.Count then
    begin
      Control.ColCount := AObjectData.Count;
    end;
    for i := 0 to AObjectData.Count - 1 do
    begin
      if AObjectData[i].IsObject then
      begin
        Control.Cells[i, ARow] := '[Object]';
      end
      else
      //if AObjectData[i].IsList then
      //begin
      //  Control.Cells[i, ARow] := '[List]';
      //end
      //else
      begin
        Control.Cells[i, ARow] := AObjectData[i].AsString;
      end;
    end;
  end;
end;

procedure TListBinder.EmptyRow(ARow: integer);
var
  i: integer;
begin
  for i := 0 to Control.ColCount - 1 do
    Control.Cells[i, ARow] := '';
end;

function TListBinder.CreateEditor(const ADataItem: IRBDataItem): TWinControl;
begin
  Result := nil;
  //if ADataItem.IsObject and ADataItem.IsReference then
  //begin
  //  Result := TOfferEditor.Create(Control);
  //end
  //else
  begin
    if ADataItem.EnumNameCount > 0 then
    begin
      Result := TOfferEditor.Create(Control);
    end
    else
    begin
      Result := TTextEditor.Create(Control);
    end;
  end;
end;

function TListBinder.CreateBinder(const ADataItem: IRBDataItem): TEditBinder;
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

procedure TListBinder.OnColRowDeletedHandler(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
  mFrom, mCount: integer;
begin
  if AsMany.Count = 0 then
    Exit;
  mFrom := sIndex - 1;
  mCount := tIndex - sIndex + 1;
  while (mCount > 0) and (mFrom <= AsMany.Count - 1) do
  begin
    AsMany.Delete(mFrom);
    Dec(mCount);
  end;
end;

procedure TListBinder.OnColRowInsertedHandler(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  AsMany.Count := AsMany.Count + 1;
  PostMessage(Control.Handle, TVLM_GRIDSETPOS, -1, Control.FixedCols);
end;

procedure TListBinder.OnEditingDoneHandler(Sender: TObject);
begin
  if fCellBinder = nil then
    Exit;
  if fCellBinder.DataItem = nil then
    Exit;
  if fCellBinder.DataItem.IsObject then
    DataToControl
  else
    Control.Cells[Control.Col, Control.Row] := fCellBinder.DataItem.AsString;
end;

procedure TListBinder.OnSelectEditorHandler(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  mDataItem: IRBDataItem;
begin
  fOldEd.Free;
  fOldEd := fCellEditor;
  FreeAndNil(fCellBinder);
  if aRow >= Control.FixedRows then begin
    mDataItem := TPersistManyDataItem.Create(AsMany, aRow - 1);
    if mDataItem.IsObject then begin
      fObjectData := AsMany.AsPersistData[aRow - 1];
      mDataItem := fObjectData[aCol];
    end;
    fCellEditor := CreateEditor(mDataItem);
    fCellBinder := CreateBinder(mDataItem);
    fCellBinder.Bind(fCellEditor, mDataItem, fDataQuery);
  end
  else
    fCellEditor := nil;
  Editor := fCellEditor;
end;

procedure TListBinder.OnKeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Shift = [ssCtrl]) then
  begin
    if Control.Row >= Control.FixedRows then
      Control.H_DoOPDeleteColRow(False, Control.Row);
    Key := 0;
  end else
    inherited;
end;

procedure TListBinder.BindControl;
var
  mData: IRBData;
  i: integer;
begin
  fOldWndProc := Control.WindowProc;
  Control.WindowProc := @ControlWndProc;
  Control.RowCount := 1 + AsMany.Count;
  Control.FixedRows := 1;
  Control.FixedCols := 0;
  if AsMany.IsObject then
  begin
    mData := AsMany.AsPersistDataClass;
    Control.ColCount := mData.Count;
    for i := 0 to mData.Count - 1 do
    begin
      Control.Cells[i,0] := mData[i].Name;
    end;
  end
  else
  begin
    Control.ColCount := 1;
    Control.Cells[0,0] := DataItem.Name;
  end;
  // on the end, otherway are called when assign collcount - rowcount
  Control.AutoFillColumns := True;
  Control.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAutoAddRows, goTabs, {goAlwaysShowEditor,} goSmoothScroll];
  Control.OnColRowInserted := @OnColRowInsertedHandler;
  Control.OnColRowDeleted := @OnColRowDeletedHandler;
  Control.OnSelectEditor := @OnSelectEditorHandler;
  Control.OnKeyDown := @OnKeyDownHandler;
  Control.H_OnEditingDone := @OnEditingDoneHandler;
end;

procedure TListBinder.UnbindControl;
begin
  fOldEd.Free;
  Control.WindowProc := fOldWndProc;
  inherited UnbindControl;
end;

procedure TListBinder.DataToControl;
var
  i: integer;
begin
  for i := 0 to AsMany.Count - 1 do
  begin
    if AsMany.IsObject then
    begin
      FillRowFromObject(i + 1, AsMany.AsPersistData[i]);
    end
    else
    begin
      Control.Cells[0, i + 1] := AsMany.AsString[i];
    end;
  end;
end;

{ TTextBinder }

function TTextBinder.GetControl: TCustomEdit;
begin
  Result := inherited Control as TCustomEdit;
end;

procedure TTextBinder.OnChangeHandler(Sender: TObject);
begin
  DataItem.AsString := Control.Text;
  NotifyChangeEvents;
end;

procedure TTextBinder.BindControl;
begin
  Control.OnChange := @OnChangeHandler;
end;

procedure TTextBinder.DataToControl;
begin
  Control.Text := DataItem.AsString
end;

{ TEditBinder }

procedure TEditBinder.UnbindControl;
begin
end;

procedure TEditBinder.NotifyChangeEvents;
var
  mEvent: TBinderChangeEvent;
begin
  for mEvent in fChangeEvents do
    mEvent;
end;

constructor TEditBinder.Create;
begin
  fChangeEvents := TBinderChangeEvents.Create;
end;

destructor TEditBinder.Destroy;
begin
  UnbindControl;
  FreeAndNil(fChangeEvents);
  inherited Destroy;
end;

procedure TEditBinder.Bind(const AControl: TWinControl;
  const ADataItem: IRBDataItem; const ADataQuery: IPersistQuery);
begin
  fControl := AControl;
  fDataItem := ADataItem;
  fDataQuery := ADataQuery;
  BindControl;
  DataToControl;
end;

procedure TEditBinder.RegisterChangeEvent(AEvent: TBinderChangeEvent);
var
  mIndex: integer;
begin
  mIndex := fChangeEvents.IndexOf(AEvent);
  if mIndex = -1 then
    fChangeEvents.Add(AEvent);
end;

procedure TEditBinder.UnregisterChangeEvent(AEvent: TBinderChangeEvent);
var
  mIndex: integer;
begin
  mIndex := fChangeEvents.IndexOf(AEvent);
  if mIndex <> -1 then
    fChangeEvents.Delete(mIndex);
end;

end.

