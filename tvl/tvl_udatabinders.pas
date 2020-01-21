unit tvl_udatabinders;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker, Controls, StdCtrls, ExtCtrls, fgl,
  Graphics, Grids, MaskEdit, lmessages, LCLProc, LCLType,
  Menus, SynEdit, trl_ipersist, trl_upersist, tvl_messages, lclintf, messages,
  Forms, EditBtn, tvl_ucontrolbinder, ComCtrls, tvl_ibindings;

type

  { TEditBinder }

  TEditBinder = class(TControlBinder)
  private
    fDataItem: IRBDataItem;
    fChangeEvents: TBinderChangeEvents;
    function GetControl: TWinControl;
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    procedure NotifyChangeEvents;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DataToControl; virtual; abstract;
    procedure ControlToData; virtual; abstract;
    procedure Bind(const AControl: TWinControl; const ADataItem: IRBDataItem); reintroduce;
    procedure RegisterChangeEvent(AEvent: TBinderChangeEvent);
    procedure UnregisterChangeEvent(AEvent: TBinderChangeEvent);
    property Control: TWinControl read GetControl;
    property DataItem: IRBDataItem read fDataItem;
  end;

  { TTextBinder }

  TTextBinder = class(TEditBinder)
  private
    function GetControl: TCustomEdit;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    property Control: TCustomEdit read GetControl;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
  end;

  { TTextBtnBinder }

  TTextBtnBinder = class(TEditBinder)
  private
    function GetControl: TCustomEditButton;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    property Control: TCustomEditButton read GetControl;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
  end;

  { TMemoBinder }

  TMemoBinder = class(TEditBinder)
  private
    function GetControl: TCustomSynEdit;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    property Control: TCustomSynEdit read GetControl;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
  end;

  { TBoolBinder }

  TBoolBinder = class(TEditBinder)
  private
    function GetControl: TCustomCheckBox;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    property Control: TCustomCheckBox read GetControl;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
  end;

  { TOfferBinder }

  TOfferBinder = class(TEditBinder)
  private
    fKeyDownText: string;
    function GetControl: TCustomComboBox;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure FillOffer; virtual; abstract;
    procedure OfferToData; virtual; abstract;
    procedure DataToOffer; virtual; abstract;
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
    property Control: TCustomComboBox read GetControl;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
  end;

  { TOfferRefBinder }

  TOfferRefBinder = class(TOfferBinder)
  private
    fOffer: IPersistRefList;
    function GetAsRef: IPersistRef;
    function GetItemForCombo(AData: IRBData): string;
  protected
    procedure FillOffer; override;
    procedure OfferToData; override;
    procedure DataToOffer; override;
  protected
    procedure FillControlItems;
    property AsRef: IPersistRef read GetAsRef;
  end;

  { TOfferEnumBinder }

  TOfferEnumBinder = class(TOfferBinder)
  protected
    procedure FillOffer; override;
    procedure OfferToData; override;
    procedure DataToOffer; override;
  end;

  { TTabSheetBinder }

  TTabSheetBinder = class(TEditBinder)
  private
    function GetControl: TCustomPage;
  protected
    property Control: TCustomPage read GetControl;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
  end;

  { TListBinder }

  TListBinder = class(TEditBinder)
  protected type

    { TGridEditorSupport }

    TGridEditorSupport = class
    protected type

      TOnGetEditorSupportGrid = function: TCustomGrid of object;

      { TSupportBinder }

      TSupportBinder = class(TControlBinder)
      protected
        fOnEditorSupportGrid: TOnGetEditorSupportGrid;
        procedure DoControlWndProc(var TheMessage: TLMessage); override;
      public
        property OnEditorSupportGrid: TOnGetEditorSupportGrid read fOnEditorSupportGrid write fOnEditorSupportGrid;
      end;

    private
      fEditor: TWinControl;
      fGrid: TCustomGrid;
      fCol, fRow:Integer;
      fSupportBinder: TSupportBinder;
    protected
      procedure KeyDown(var Key : Word; Shift : TShiftState);
      procedure EditingDone;
      function GetRect: TRect;
      procedure SetGrid(var Msg: TGridMessage);
      procedure SetPos(var Msg: TGridMessage);
      procedure GetGrid(var Msg: TGridMessage);
      function GetEditorSupportGridEvent: TCustomGrid;
    public
      constructor Create(AEditor: TWinControl);
      destructor Destroy; override;
      property Col: integer read fCol;
      property Row: integer read fRow;
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
    fCol, fRow: Integer;
    fOfferEditor: TOfferEditor;
    fTextEditor: TTextEditor;
    function GetAsMany: IPersistMany;
    function GetControl: TCustomStringGrid;
    procedure FillRowFromObject(ARow: integer; AObjectData: IRBData);
    procedure EmptyRow(ARow: integer);
    function CreateEditor(const ADataItem: IRBDataItem): TWinControl;
    function CreateBinder(const ADataItem: IRBDataItem): TEditBinder;
    procedure NilEditor;
    procedure OnColRowDeletedHandler(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OnColRowInsertedHandler(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OnEditingDoneHandler(Sender: TObject);
    procedure OnSelectEditorHandler(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnSelectionHandler(Sender: TObject; aCol, aRow: Integer);
  protected
    procedure BindControl; override;
    procedure UnbindControl; override;
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
    property Control: TCustomStringGrid read GetControl;
    property AsMany: IPersistMany read GetAsMany;
  protected
    procedure DispatchTextToEditor;
    procedure SetRowAutoInsertToFalse;
    function GetOfferEditor: TOfferEditor;
    function GetTextEditor: TTextEditor;
    property OfferEditor: TOfferEditor read GetOfferEditor;
    property TextEditor: TTextEditor read GetTextEditor;
  public
    procedure DataToControl; override;
    procedure ControlToData; override;
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

  { TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
  public
    function H_FindNextControl(CurrentControl: TWinControl; GoForward,
      CheckTabStop, CheckParent: Boolean): TWinControl;
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
    function  H_GetEditText(ACol, ARow: Longint): string;
    procedure H_SetOptions(const AValue: TGridOptions);
  end;

  { TCustomComboBoxHelper }

  TCustomComboBoxHelper = class helper for TCustomComboBox
  private
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(AValue: TNotifyEvent);
  public
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

{ TTabSheetBinder }

function TTabSheetBinder.GetControl: TCustomPage;
begin
  Result := inherited Control as TCustomPage;
end;

procedure TTabSheetBinder.DataToControl;
begin
  Control.Caption := DataItem.AsString;
end;

procedure TTabSheetBinder.ControlToData;
begin
  // for now leave readonly
end;

{ TListBinder.TGridEditorSupport.TSupportBinder }

procedure TListBinder.TGridEditorSupport.TSupportBinder.DoControlWndProc(
  var TheMessage: TLMessage);
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
        if (OnEditorSupportGrid <> nil) and (OnEditorSupportGrid.H_EditorIsReadOnly) then
          exit;
      end;
  end;
  inherited DoControlWndProc(TheMessage);
end;

{ TTextBtnBinder }

function TTextBtnBinder.GetControl: TCustomEditButton;
begin
  Result := inherited Control as TCustomEditButton;
end;

procedure TTextBtnBinder.OnChangeHandler(Sender: TObject);
begin
  DataItem.AsString := Control.Text;
  NotifyChangeEvents;
end;

procedure TTextBtnBinder.BindControl;
begin
  inherited;
  Control.OnChange := OnChangeHandler;
end;

procedure TTextBtnBinder.UnbindControl;
begin
  Control.OnChange := nil;
  inherited UnbindControl;
end;

procedure TTextBtnBinder.DataToControl;
begin
  Control.Text := DataItem.AsString
end;

procedure TTextBtnBinder.ControlToData;
begin
  DataItem.AsString := Control.Text;
end;

{ TWinControlHelper }

function TWinControlHelper.H_FindNextControl(CurrentControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
begin
  Result := FindNextControl(CurrentControl, GoForward, CheckTabStop, CheckParent);
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
  inherited;
  Control.OnChange := OnChangeHandler;
end;

procedure TMemoBinder.UnbindControl;
begin
  Control.OnChange := nil;
  inherited UnbindControl;
end;

procedure TMemoBinder.DataToControl;
begin
  Control.Text := DataItem.AsString;
end;

procedure TMemoBinder.ControlToData;
begin
  DataItem.AsString := Control.Text;
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
  inherited;
  Control.OnChange := OnChangeHandler;
end;

procedure TBoolBinder.UnbindControl;
begin
  Control.OnChange := nil;
  inherited UnbindControl;
end;

procedure TBoolBinder.DataToControl;
begin
  if DataItem.AsBoolean then
    Control.State := cbChecked
  else
    Control.State := cbUnchecked;
end;

procedure TBoolBinder.ControlToData;
begin
  DataItem.AsBoolean := Control.State = cbChecked;
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
  if fSupport <> nil then
    fSupport.KeyDown(Key, Shift);
end;

procedure TListBinder.TTextEditor.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  m: TRect;
begin
  if fSupport <> nil then begin
    m := fSupport.GetRect;
    inherited DoSetBounds(m.Left, m.Top, m.Right - m.Left + 1, m.Bottom - m.Top + 1);
  end else
    inherited;
end;

procedure TListBinder.TTextEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  if fSupport <> nil then
    fSupport.SetGrid(Msg);
end;

procedure TListBinder.TTextEditor.msg_SetPos(var Msg: TGridMessage);
begin
  if fSupport <> nil then
    fSupport.SetPos(Msg);
end;

procedure TListBinder.TTextEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  if fSupport <> nil then
    fSupport.GetGrid(Msg);
end;

procedure TListBinder.TTextEditor.msg_SetValue(var Msg: TGridMessage);
var
  mSkipEnd: Boolean;
begin
  mSkipEnd := Text = '';
  Text := Msg.Value;
  if mSkipEnd then
    SelStart := Length(Text) + 1;
end;

constructor TListBinder.TTextEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSupport := TGridEditorSupport.Create(Self);
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
  if fSupport <> nil then
    fSupport.EditingDone;
end;

{ TListBinder.TGridEditorSupport }

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

function TListBinder.TGridEditorSupport.GetEditorSupportGridEvent: TCustomGrid;
begin
  Result := fGrid;
end;

constructor TListBinder.TGridEditorSupport.Create(AEditor: TWinControl);
begin
  fEditor := AEditor;
  fSupportBinder := TSupportBinder.Create;
  fSupportBinder.OnEditorSupportGrid := GetEditorSupportGridEvent;
  fSupportBinder.Bind(fEditor);
end;

destructor TListBinder.TGridEditorSupport.Destroy;
begin
  fSupportBinder.Unbind;
  FreeAndNil(fSupportBinder);
  inherited Destroy;
end;

{ TListBinder.TOfferEditor }

procedure TListBinder.TOfferEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if not DroppedDown then
     if fSupport <> nil then
        fSupport.KeyDown(Key, Shift);
end;

procedure TListBinder.TOfferEditor.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  m: TRect;
begin
  if fSupport <> nil then begin
    m := fSupport.GetRect;
    inherited DoSetBounds(m.Left, m.Top, m.Right - m.Left + 1, m.Bottom - m.Top + 1);
  end else
    inherited;
end;

procedure TListBinder.TOfferEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  if fSupport <> nil then
    fSupport.SetGrid(Msg);
end;

procedure TListBinder.TOfferEditor.msg_SetPos(var Msg: TGridMessage);
begin
  if fSupport <> nil then
    fSupport.SetPos(Msg);
end;

procedure TListBinder.TOfferEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  if fSupport <> nil then
    fSupport.GetGrid(Msg);
end;

procedure TListBinder.TOfferEditor.msg_SetValue(var Msg: TGridMessage);
var
  mSkipEnd: Boolean;
begin
  mSkipEnd := Text = '';
  Text := Msg.Value;
  if mSkipEnd then
    SelStart := Length(Text) + 1;
end;

constructor TListBinder.TOfferEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSupport := TGridEditorSupport.Create(Self);
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
  if fSupport <> nil then
    fSupport.EditingDone;
end;

{ TOfferRefBinder }

function TOfferRefBinder.GetAsRef: IPersistRef;
begin
  Result := DataItem.AsInterface as IPersistRef;
end;

function TOfferRefBinder.GetItemForCombo(AData: IRBData): string;
begin
  if not SameText(AData[0].Name, 'id') or (AData.Count = 1) then
    Result := AData[0].AsString
  else
    Result := AData[1].AsString;
end;

procedure TOfferRefBinder.FillOffer;
begin
  fOffer := (AsRef.Store as IPersistQuery).SelectClass(AsRef.ClassName);
  Control.Items.Clear;
  FillControlItems;
end;

procedure TOfferRefBinder.OfferToData;
var
  mOfferIndex: NativeInt;
  i: integer;
  mData: IRBData;
  mText: string;
begin
  if Control.ItemIndex <> -1 then
  begin
    mOfferIndex := NativeInt(Control.Items.Objects[Control.ItemIndex]);
    AsRef.SID := fOffer[mOfferIndex].SID;
  end else begin
    mText := Control.Text;
    for i := 0 to fOffer.Count - 1 do
    begin
      mData := fOffer[i].Data;
      if GetItemForCombo(mData) = mText then
      begin
        AsRef.SID := fOffer[i].SID;
      end;
    end;
  end;
end;

procedure TOfferRefBinder.DataToOffer;
var
  i: integer;
begin
  Control.ItemIndex := -1;
  for i := 0 to fOffer.Count - 1 do
  begin
    if AsRef.SID = fOffer[i].SID then
    begin
      Control.ItemIndex := i;
      Break;
    end;
  end;
end;

procedure TOfferRefBinder.FillControlItems;
var
  i: NativeInt;
  mData: IRBData;
begin
  for i := 0 to fOffer.Count - 1 do
  begin
    mData := fOffer[i].Data;
    Control.Items.AddObject(GetItemForCombo(mData), TObject(i))
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

procedure TOfferBinder.BindControl;
begin
  inherited;
  FillOffer;
  Control.OnChange := OnChangeHandler;
  Control.AutoComplete := True;
end;

procedure TOfferBinder.UnbindControl;
begin
  Control.OnChange := nil;
  inherited UnbindControl;
end;

procedure TOfferBinder.DoControlWndProc(var TheMessage: TLMessage);
var
  mMsg: TLMKey;
  mLMCmd: TLMCommand absolute TheMessage;
begin
  case TheMessage.Msg of
    CN_Command:
      case mLMCmd.NotifyCode of
        CBN_CLOSEUP:
          begin
            // when binded around combo in grid, by this is Grid.FRowAutoInserted
            // reset to false ... otherwise adding of new row is prevented
            // (do not now why I did it, but edit combo in grid has second binder,
            // which prevent LM_KEYDOWN message to go further)
            mMsg.Msg := CN_KEYDOWN;
            mMsg.CharCode := VK_RETURN;
            mMsg.KeyData := 0;
            Control.Dispatch(mMsg);
          end;
      end;
  end;
  inherited;
  case TheMessage.Msg of
    LM_KEYDOWN, CN_KEYDOWN:
      fKeyDownText := Control.Text;
    LM_KEYUP, CN_KEYUP:
      if fKeyDownText <> Control.Text then begin
        // key can cause autcomplete and via that change of ItemIndex
        // and this is not reported via OnChange event
        OnChangeHandler(Control);
      end;
  end;
end;

procedure TOfferBinder.DataToControl;
begin
  DataToOffer;
end;

procedure TOfferBinder.ControlToData;
begin
  OfferToData;
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

function TGridHelper.H_GetEditText(ACol, ARow: Longint): string;
begin
  Result := GetEditText(ACol, ARow);
end;

procedure TGridHelper.H_SetOptions(const AValue: TGridOptions);
begin
  Options := AValue;
end;

{ TListBinder }

function TListBinder.GetControl: TCustomStringGrid;
begin
  Result := inherited Control as TCustomStringGrid;
end;

function TListBinder.GetAsMany: IPersistMany;
var
  m: boolean;
  mm: string;
begin
  mm := self.ClassName;
  m := DataItem.IsObject;
  if m then
    Result := DataItem.AsObject as IPersistMany
  else
  if DataItem.IsInterface then
    Result := DataItem.AsInterface as IPersistMany
  else
    raise Exception.Create('not object nor interface property for TLIST, unable retrieve IPersistMany');
  if Result = nil then
    raise Exception.Create(DataItem.Name + ' is not assigned');
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
  if Supports(AsMany, IPersistManyRefs) then
  begin
    Result := OfferEditor;
  end
  else
  begin
    if ADataItem.EnumNameCount > 0 then
    begin
      Result := OfferEditor;
    end
    else
    begin
      Result := TextEditor;
    end;
  end;
end;

function TListBinder.CreateBinder(const ADataItem: IRBDataItem): TEditBinder;
begin
  Result := nil;
  if Supports(AsMany, IPersistManyRefs) then
  begin
    Result := TOfferRefBinder.Create;
  end
  else
  if ADataItem.EnumNameCount > 0 then
  begin
    Result := TOfferEnumBinder.Create;
  end
  else
  begin
    Result := TTextBinder.Create;
  end;
end;

procedure TListBinder.NilEditor;
begin
  if Control.Editor = fCellEditor then
    Control.Editor := nil;
  fCellEditor := nil;
end;

function TListBinder.GetOfferEditor: TOfferEditor;
begin
  if fOfferEditor = nil then
    fOfferEditor := TOfferEditor.Create(Control);
  Result := fOfferEditor;
end;

function TListBinder.GetTextEditor: TTextEditor;
begin
  if fTextEditor = nil then
    fTextEditor := TTextEditor.Create(Control);
  Result := fTextEditor;
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
  if fCellBinder.DataItem.IsInterface and Supports(fCellBinder.DataItem.AsInterface, IPersistRef) then
  begin
    Control.Cells[Control.Col, Control.Row] := (fCellBinder.DataItem.AsInterface as IPersistRef).Data[0].AsString;
  end
  else
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
  FreeAndNil(fCellBinder);
  if aRow >= Control.FixedRows then begin
    mDataItem := TPersistManyDataItem.Create(AsMany, aRow - 1);
    if mDataItem.IsObject then begin
      fObjectData := AsMany.AsPersistData[aRow - 1];
      mDataItem := fObjectData[aCol];
    end;
    NilEditor;
    fCellEditor := CreateEditor(mDataItem);
    fCellBinder := CreateBinder(mDataItem);
    fCellBinder.Bind(fCellEditor, mDataItem);
  end
  else begin
    fCellBinder.Unbind;
    NilEditor;
  end;
  Editor := fCellEditor;
  fCol := aCol;
  fRow :=  aRow;
end;

procedure TListBinder.OnKeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  mNextControl: TWinControl;
  mParentF: TCustomForm;
begin
  if (Key = VK_DELETE) and (Shift = [ssCtrl]) then
  begin
    if Control.Row >= Control.FixedRows then
      Control.H_DoOPDeleteColRow(False, Control.Row);
    Key := 0;
  end
  else if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    mParentF := GetParentForm(Control);
    if mParentF <> nil then
    begin
      mNextControl := mParentF.H_FindNextControl(Control, True, True, False);
      if mNextControl <> nil then begin
        mNextControl.SetFocus;
        Key := 0;
      end
    end;
  end;
  inherited;
end;

procedure TListBinder.OnSelectionHandler(Sender: TObject; aCol, aRow: Integer);
begin
  if (fCol <> aCol) or (fRow <> aRow) then
    FreeAndNil(fCellBinder);
end;

procedure TListBinder.BindControl;
var
  mData: IRBData;
  i: integer;
begin
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
  Control.OnColRowInserted := OnColRowInsertedHandler;
  Control.OnColRowDeleted := OnColRowDeletedHandler;
  Control.OnSelectEditor := OnSelectEditorHandler;
  Control.OnKeyDown := OnKeyDownHandler;
  Control.H_OnEditingDone := OnEditingDoneHandler;
end;

procedure TListBinder.UnbindControl;
begin
  Control.OnColRowInserted := nil;
  Control.OnColRowDeleted := nil;
  Control.OnSelectEditor := nil;
  Control.OnKeyDown := nil;
  Control.H_OnEditingDone := nil;
  NilEditor;
  FreeAndNil(fCellBinder);
  FreeAndNil(fOfferEditor);
  FreeAndNil(fTextEditor);
  inherited UnbindControl;
end;

procedure TListBinder.DoControlWndProc(var TheMessage: TLMessage);
begin
  case TheMessage.Msg of
    TVLM_GRIDSETPOS:
      begin
        if TheMessage.WParam <> -1 then
          Control.Row := TheMessage.WParam;
        if TheMessage.LParam <> -1 then
          Control.Col := TheMessage.LParam;
      end;
    LM_KEYDOWN:
      begin
        inherited;
        if (TLMKey(TheMessage).CharCode = VK_V)
           and ([ssModifier] = MsgKeyDataToShiftState(TLMKey(TheMessage).KeyData))
        then begin
          // in case of paste editor could be hidden, so clipboard
          // is inserted into grid cell
          DispatchTextToEditor;
          SetRowAutoInsertToFalse;
        end;
      end;
    else
      inherited;
  end;
end;

procedure TListBinder.DispatchTextToEditor;
var
  msg: TGridMessage;
begin
  if fCellEditor = nil then
    Exit;
  msg.LclMsg.msg := GM_SETVALUE;
  msg.Grid := Control;
  msg.Col := Control.Col;
  msg.Row := Control.Row;
  msg.Value := Control.H_GetEditText(Fcol, FRow);
  fCellEditor.Dispatch(msg);
end;

procedure TListBinder.SetRowAutoInsertToFalse;
var
  mOptions: TGridOptions;
begin
  // will cause set of fRowAutoInsert to false (because after paste from clipoboard
  // is not set to false and auto row insert do not work otherwise)
  if goAutoAddRowsSkipContentCheck in Control.Options then
  begin
    mOptions := Control.Options - [goAutoAddRowsSkipContentCheck];
    Control.H_SetOptions(mOptions);
    mOptions := Control.Options + [goAutoAddRowsSkipContentCheck];
    Control.H_SetOptions(mOptions);
  end
  else
  begin
    mOptions := Control.Options + [goAutoAddRowsSkipContentCheck];
    Control.H_SetOptions(mOptions);
    mOptions := Control.Options - [goAutoAddRowsSkipContentCheck];
    Control.H_SetOptions(mOptions);
  end;
end;

procedure TListBinder.DataToControl;
var
  i: integer;
  mData: IRBData;
begin
  // possible change of rowcount
  UnbindControl;
  BindControl;
  for i := 0 to AsMany.Count - 1 do
  begin
    if Supports(AsMany, IPersistManyRefs) then
    begin
      mData := (AsMany.AsInterface[i] as IPersistRef).Data;
      if mData <> nil then
        Control.Cells[0, i + 1] := mData[0].AsString
      else
        Control.Cells[0, i + 1] := '';
    end
    else
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

procedure TListBinder.ControlToData;
begin
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
  inherited;
  Control.OnChange := OnChangeHandler;
end;

procedure TTextBinder.UnbindControl;
begin
  Control.OnChange := nil;
  inherited UnbindControl;
end;

procedure TTextBinder.DataToControl;
begin
  Control.Text := DataItem.AsString
end;

procedure TTextBinder.ControlToData;
begin
  DataItem.AsString := Control.Text;
end;

{ TEditBinder }

function TEditBinder.GetControl: TWinControl;
begin
  Result := inherited Control as TWinControl;
end;

procedure TEditBinder.BindControl;
begin
  inherited;
end;

procedure TEditBinder.UnbindControl;
begin
  ControlToData;
  inherited;
end;

procedure TEditBinder.NotifyChangeEvents;
var
  mEvent: TBinderChangeEvent;
begin
  for mEvent in fChangeEvents do
    mEvent(DataItem, Control);
end;

constructor TEditBinder.Create;
begin
  fChangeEvents := TBinderChangeEvents.Create;
end;

destructor TEditBinder.Destroy;
begin
  FreeAndNil(fChangeEvents);
  inherited Destroy;
end;

procedure TEditBinder.Bind(const AControl: TWinControl;
  const ADataItem: IRBDataItem);
begin
  fDataItem := ADataItem;
  inherited Bind(AControl);
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

