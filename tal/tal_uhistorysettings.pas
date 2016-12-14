unit tal_uhistorysettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, tal_ihistorysettings, Controls, StdCtrls, Forms,
  trl_irttibroker, trl_ipersist, trl_upersist, ExtCtrls, SynEditMiscClasses;

type

  THistoryDataPosition = class
  private
    fID: string;
    fTop: integer;
    fLeft: integer;
    fWidth: integer;
    fHeight: integer;
  published
    property ID: string read fID write fID;
    property Top: integer read fTop write fTop;
    property Left: integer read fLeft write fLeft;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
  end;

  IPersistManyTHistoryDataPositions = interface(IPersistManyItems<THistoryDataPosition>)
  ['{CC058F28-C055-4E71-A890-E912642D9AD4}']
  end;

  TPersistManyTHistoryDataPositions = class(TPersistManyObjects<THistoryDataPosition>, IPersistManyTHistoryDataPositions)
  end;

  { THistoryDataTexts }

  THistoryDataTexts = class
  private
    fID: string;
    fTexts: IPersistManyStrings;
  public
    procedure AfterConstruction; override;
  published
     property ID: string read fID write fID;
     property Texts: IPersistManyStrings read fTexts;
  end;

  IPersistManyTHistoryDataTexts = interface(IPersistManyItems<THistoryDataTexts>)
  ['{2D2B4EB2-920B-493B-AFA3-402BCDB0495F}']
  end;

  TPersistManyTHistoryDataTexts = class(TPersistManyObjects<THistoryDataTexts>, IPersistManyTHistoryDataTexts)
  end;

  { THistoryDataCheckBoxStates }

  THistoryDataCheckBoxStates = class
  private
    fID: string;
    fCheckBoxState: TCheckBoxState;
  published
     property ID: string read fID write fID;
     property CheckBoxState: TCheckBoxState read fCheckBoxState write fCheckBoxState;
  end;

  IPersistManyTHistoryDataCheckBoxStates = interface(IPersistManyItems<THistoryDataCheckBoxStates>)
  ['{1CDE4E3A-C21B-4EB5-8D26-0940A7E18F98}']
  end;

  TPersistManyTHistoryDataCheckBoxStates = class(TPersistManyObjects<THistoryDataCheckBoxStates>, IPersistManyTHistoryDataCheckBoxStates)
  end;

  { THistoryDataIntegers }

  THistoryDataIntegers = class
  private
    fID: string;
    fIntegers: IPersistManyIntegers;
  public
    procedure AfterConstruction; override;
  published
     property ID: string read fID write fID;
     property Integers: IPersistManyIntegers read fIntegers;
  end;

  IPersistManyTHistoryDataIntegers = interface(IPersistManyItems<THistoryDataIntegers>)
  ['{466318BC-50E3-4555-809C-628650475215}']
  end;

  TPersistManyTHistoryDataIntegers = class(TPersistManyObjects<THistoryDataIntegers>, IPersistManyTHistoryDataIntegers)
  end;

  THistoryData = class
  private
    fID: string;
    fPositions: IPersistManyTHistoryDataPositions;
    fTexts: IPersistManyTHistoryDataTexts;
    fCheckBoxStates: IPersistManyTHistoryDataCheckBoxStates;
    fIntegers: IPersistManyTHistoryDataIntegers;
  public
    procedure AfterConstruction; override;
  published
    property ID: string read fID write fID;
    property Positions: IPersistManyTHistoryDataPositions read fPositions;
    property Texts: IPersistManyTHistoryDataTexts read fTexts;
    property CheckBoxStates: IPersistManyTHistoryDataCheckBoxStates read fCheckBoxStates;
    property Integers: IPersistManyTHistoryDataIntegers read fIntegers;
  end;

  { TControlVisitor }

  TControlVisitor = class
  protected const
    cMaxTexts = 10;
  protected
    function GetPosition(const AID: string; const APositions: IPersistManyTHistoryDataPositions;
      ACanCreate: Boolean): THistoryDataPosition;
    function GetTexts(const AID: string; const ATexts: IPersistManyTHistoryDataTexts;
      ACanCreate: Boolean): THistoryDataTexts;
    function GetCheckBoxState(const AID: string; const ACheckBoxStates: IPersistManyTHistoryDataCheckBoxStates;
      ACanCreate: Boolean): THistoryDataCheckBoxStates;
    function GetIntegers(const AID: string; const AIntegers: IPersistManyTHistoryDataIntegers;
      ACanCreate: Boolean): THistoryDataIntegers;
  public
    procedure Visit(const AHistory: IRBData; AControl: TControl); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomComboBox); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomForm); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomSplitter); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomCheckBox); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TSynEditBase); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomEdit); virtual; overload;
  end;

  { TControlSaveVisitor }

  TControlSaveVisitor = class(TControlVisitor)
  public
    procedure Visit(const AHistory: IRBData; AControl: TCustomComboBox); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomForm); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomSplitter); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomCheckBox); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TSynEditBase); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomEdit); override; overload;
  end;

  { TControlLoadVisitor }

  TControlLoadVisitor = class(TControlVisitor)
  public
    procedure Visit(const AHistory: IRBData; AControl: TCustomComboBox); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomForm); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomSplitter); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomCheckBox); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TSynEditBase); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomEdit); override; overload;
  end;

  { THistorySettings }

  THistorySettings = class(TInterfacedObject, IHistorySettings)
  protected
    fStore: IPersistStore;
  protected
    procedure DispatchVisit(AControl: TControl; AHistory: IRBData;
      AVisitor: TControlVisitor);
    procedure AcceptVisitor(AControl: TWinControl; AHistory: IRBData;
      AVisitor: TControlVisitor; AInside: Boolean = True); overload;
    procedure AcceptVisitor(AControl: TControl; AHistory: IRBData;
      AVisitor: TControlVisitor; AInside: Boolean = True); overload;
    function GetHistoryData(const AID: string): IRBData;
  protected
    // IHistorySettings
    procedure Load(const ATopControl: TWinControl; AInside: Boolean = True);
    procedure Save(const ATopControl: TWinControl; AInside: Boolean = True);
  published
    property Store: IPersistStore read fStore write fStore;
  end;

implementation

{ THistoryDataIntegers }

procedure THistoryDataIntegers.AfterConstruction;
begin
  inherited AfterConstruction;
  fIntegers := TPersistManyIntegers.Create;
end;

{ THistoryDataCheckBoxStates }

{ TControlSaveVisitor }

procedure TControlSaveVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomComboBox);
var
  i: integer;
  mTexts: THistoryDataTexts;
  mComboText: string;
begin
  mTexts := GetTexts(AControl.Name,
    AHistory.ItemByName['Texts'].AsInterface as IPersistManyTHistoryDataTexts,
    True);
  mComboText := AControl.Text;
  i := 0;
  while i <= mTexts.Texts.Count - 1 do
   if SameText(mComboText, mTexts.Texts[i]) then
     mTexts.Texts.Delete(i)
   else
     inc(i);
  mTexts.Texts.Count := mTexts.Texts.Count + 1;
  mTexts.Texts[mTexts.Texts.Count - 1] := mComboText;
  while mTexts.Texts.Count > cMaxTexts do
    mTexts.Texts.Delete(0);
end;

procedure TControlSaveVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomForm);
var
  i: integer;
  mPosition: THistoryDataPosition;
begin
  mPosition := GetPosition(AControl.Name,
    AHistory.ItemByName['Positions'].AsInterface as IPersistManyTHistoryDataPositions,
    True);
  mPosition.Left := AControl.Left;
  mPosition.Top := AControl.Top;
  mPosition.Width := AControl.Width;
  mPosition.Height := AControl.Height;
end;

procedure TControlSaveVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomSplitter);
var
  mIntegers: THistoryDataIntegers;
begin
  mIntegers := GetIntegers(AControl.Name,
    AHistory.ItemByName['Integers'].AsInterface as IPersistManyTHistoryDataIntegers,
    True);
  mIntegers.Integers.Count := 1;
  mIntegers.Integers[0] := AControl.GetSplitterPosition;
end;

procedure TControlSaveVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomCheckBox);
var
  mCheckBoxStates: THistoryDataCheckBoxStates;
begin
  mCheckBoxStates := GetCheckBoxState(AControl.Name,
    AHistory.ItemByName['CheckBoxStates'].AsInterface as IPersistManyTHistoryDataCheckBoxStates,
    True);
  mCheckBoxStates.CheckBoxState := AControl.State;
end;

procedure TControlSaveVisitor.Visit(const AHistory: IRBData;
  AControl: TSynEditBase);
var
  mTexts: THistoryDataTexts;
begin
  mTexts := GetTexts(AControl.Name,
    AHistory.ItemByName['Texts'].AsInterface as IPersistManyTHistoryDataTexts,
    True);
  mTexts.Texts.Count := 1;
  mTexts.Texts[0] := AControl.Lines.Text;
end;

procedure TControlSaveVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomEdit);
var
  mTexts: THistoryDataTexts;
begin
  mTexts := GetTexts(AControl.Name,
    AHistory.ItemByName['Texts'].AsInterface as IPersistManyTHistoryDataTexts,
    True);
  mTexts.Texts.Count := 1;
  mTexts.Texts[0] := AControl.Text;
end;

{ TControlLoadVisitor }

procedure TControlLoadVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomComboBox);
var
  i: integer;
  mTexts: THistoryDataTexts;
  mComboText: string;
begin
  mTexts := GetTexts(AControl.Name,
    AHistory.ItemByName['Texts'].AsInterface as IPersistManyTHistoryDataTexts,
    False);
  if mTexts = nil then
    Exit;
  AControl.Clear;
  for i := 0 to mTexts.Texts.Count - 1 do
    AControl.Items.Insert(0, mTexts.Texts[i]);
  if AControl.Items.Count > 0 then
    AControl.ItemIndex := 0;
end;

procedure TControlLoadVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomForm);
var
  i: integer;
  mPosition: THistoryDataPosition;
begin
  mPosition := GetPosition(AControl.Name,
    AHistory.ItemByName['Positions'].AsInterface as IPersistManyTHistoryDataPositions,
    False);
  if mPosition = nil then
    Exit;
  AControl.Left := mPosition.Left;
  AControl.Top := mPosition.Top;
  AControl.Width := mPosition.Width;
  AControl.Height := mPosition.Height;
end;

procedure TControlLoadVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomSplitter);
var
  mIntegers: THistoryDataIntegers;
begin
  mIntegers := GetIntegers(AControl.Name,
    AHistory.ItemByName['Integers'].AsInterface as IPersistManyTHistoryDataIntegers,
    False);
  if mIntegers = nil then
    Exit;
  if mIntegers.Integers.Count > 0 then
    AControl.SetSplitterPosition(mIntegers.Integers[0]);
end;

procedure TControlLoadVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomCheckBox);
var
  mCheckBoxStates: THistoryDataCheckBoxStates;
begin
  mCheckBoxStates := GetCheckBoxState(AControl.Name,
    AHistory.ItemByName['CheckBoxStates'].AsInterface as IPersistManyTHistoryDataCheckBoxStates,
    False);
  if mCheckBoxStates = nil then
    Exit;
  AControl.State := mCheckBoxStates.CheckBoxState;
end;

procedure TControlLoadVisitor.Visit(const AHistory: IRBData;
  AControl: TSynEditBase);
var
  mTexts: THistoryDataTexts;
begin
  mTexts := GetTexts(AControl.Name,
    AHistory.ItemByName['Texts'].AsInterface as IPersistManyTHistoryDataTexts,
    False);
  if mTexts = nil then
    Exit;
  if mTexts.Texts.Count > 0 then
    AControl.Lines.Text := mTexts.Texts[0];
end;

procedure TControlLoadVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomEdit);
var
  mTexts: THistoryDataTexts;
begin
  mTexts := GetTexts(AControl.Name,
    AHistory.ItemByName['Texts'].AsInterface as IPersistManyTHistoryDataTexts,
    False);
  if mTexts = nil then
    Exit;
  if mTexts.Texts.Count > 0 then
    AControl.Text := mTexts.Texts[0];
end;

{ THistoryData }

procedure THistoryData.AfterConstruction;
begin
  inherited AfterConstruction;
  fPositions := TPersistManyTHistoryDataPositions.Create;
  fTexts := TPersistManyTHistoryDataTexts.Create;
  fCheckBoxStates := TPersistManyTHistoryDataCheckBoxStates.Create;
  fIntegers := TPersistManyTHistoryDataIntegers.Create;
end;

{ THistoryDataTexts }

procedure THistoryDataTexts.AfterConstruction;
begin
  inherited AfterConstruction;
  fTexts := TPersistManyStrings.Create;
end;

{ THistorySettings }

procedure THistorySettings.DispatchVisit(AControl: TControl;
  AHistory: IRBData; AVisitor: TControlVisitor);
begin
  // because of helpers do not support virtual methods, dispatch to visitor methods
  // need to be done here staticaly
  if AControl is TCustomForm then
    AVisitor.Visit(AHistory, AControl as TCustomForm)
  else if AControl is TCustomComboBox then
    AVisitor.Visit(AHistory, AControl as TCustomComboBox)
  else if AControl is TCustomSplitter then
    AVisitor.Visit(AHistory, AControl as TCustomSplitter)
  else if AControl is TCustomCheckBox then
    AVisitor.Visit(AHistory, AControl as TCustomCheckBox)
  else if AControl is TSynEditBase then
    AVisitor.Visit(AHistory, AControl as TSynEditBase)
  else if AControl is TCustomEdit then
    AVisitor.Visit(AHistory, AControl as TCustomEdit)
  else
    AVisitor.Visit(AHistory, AControl);
end;

procedure THistorySettings.AcceptVisitor(AControl: TWinControl; AHistory: IRBData;
  AVisitor: TControlVisitor; AInside: Boolean = True);
var
  i: integer;
begin
  DispatchVisit(AControl, AHistory, AVisitor);
  if AInside then
    for i := 0 to AControl.ControlCount - 1 do
      AcceptVisitor(AControl.Controls[i], AHistory, AVisitor);
end;

procedure THistorySettings.AcceptVisitor(AControl: TControl;
  AHistory: IRBData; AVisitor: TControlVisitor; AInside: Boolean = True);
begin
  if AControl is TWinControl then
    AcceptVisitor(AControl as TWinControl, AHistory, AVisitor, AInside)
  else
    DispatchVisit(AControl, AHistory, AVisitor);
end;

function THistorySettings.GetHistoryData(const AID: string): IRBData;
var
  mList: IPersistRefList;
  i: integer;
begin
  Result := nil;
  mList := (fStore as IPersistQuery).SelectClass('THistoryData');
  for i := 0 to mList.Count - 1 do begin
    if mList.Data[i].ItemByName['ID'].AsString = AID then begin
      Result := mList.Data[i];
      Break;
    end;
  end;
  if Result = nil then begin
    Result := fStore.New('THistoryData');
    Result.ItemByName['ID'].AsString := AID;
  end;
end;

procedure THistorySettings.Load(const ATopControl: TWinControl; AInside: Boolean = True);
var
  mLoadVisitor: TControlLoadVisitor;
  mHistoryData: IRBData;
begin
  Store.Open;
  try
    mHistoryData := GetHistoryData(ATopControl.Name);
    mLoadVisitor := TControlLoadVisitor.Create;
    try
      AcceptVisitor(ATopControl, mHistoryData, mLoadVisitor, AInside);
    finally
      mLoadVisitor.Free;
    end;
  finally
    Store.Close;
  end;
end;

procedure THistorySettings.Save(const ATopControl: TWinControl; AInside: Boolean = True);
var
  mSaveVisitor: TControlSaveVisitor;
  mHistoryData: IRBData;
begin
  Store.Open;
  try
    mHistoryData := GetHistoryData(ATopControl.Name);
    mSaveVisitor := TControlSaveVisitor.Create;
    try
      AcceptVisitor(ATopControl, mHistoryData, mSaveVisitor, AInside);
    finally
      mSaveVisitor.Free;
    end;
    Store.Save(mHistoryData);
  finally
    Store.Close;
  end;
end;

{ TControlVisitor }

function TControlVisitor.GetPosition(const AID: string;
  const APositions: IPersistManyTHistoryDataPositions; ACanCreate: Boolean): THistoryDataPosition;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to APositions.Count - 1 do
    if APositions.Item[i].ID = AID then begin
      Result := APositions.Item[i];
      Break;
    end;
  if (Result = nil) and ACanCreate then begin
    APositions.Count := APositions.Count + 1;
    APositions.Item[APositions.Count - 1].ID := AID;
    Result := APositions.Item[APositions.Count - 1];
  end;
end;

function TControlVisitor.GetTexts(const AID: string;
  const ATexts: IPersistManyTHistoryDataTexts; ACanCreate: Boolean
  ): THistoryDataTexts;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ATexts.Count - 1 do
    if ATexts.Item[i].ID = AID then begin
      Result := ATexts.Item[i];
      Break;
    end;
  if (Result = nil) and ACanCreate then begin
    ATexts.Count := ATexts.Count + 1;
    ATexts.Item[ATexts.Count - 1].ID := AID;
    Result := ATexts.Item[ATexts.Count - 1];
  end;
end;

function TControlVisitor.GetCheckBoxState(const AID: string;
  const ACheckBoxStates: IPersistManyTHistoryDataCheckBoxStates; ACanCreate: Boolean
  ): THistoryDataCheckBoxStates;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ACheckBoxStates.Count - 1 do
    if ACheckBoxStates.Item[i].ID = AID then begin
      Result := ACheckBoxStates.Item[i];
      Break;
    end;
  if (Result = nil) and ACanCreate then begin
    ACheckBoxStates.Count := ACheckBoxStates.Count + 1;
    ACheckBoxStates.Item[ACheckBoxStates.Count - 1].ID := AID;
    Result := ACheckBoxStates.Item[ACheckBoxStates.Count - 1];
  end;
end;

function TControlVisitor.GetIntegers(const AID: string;
  const AIntegers: IPersistManyTHistoryDataIntegers; ACanCreate: Boolean
  ): THistoryDataIntegers;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to AIntegers.Count - 1 do
    if AIntegers.Item[i].ID = AID then begin
      Result := AIntegers.Item[i];
      Break;
    end;
  if (Result = nil) and ACanCreate then begin
    AIntegers.Count := AIntegers.Count + 1;
    AIntegers.Item[AIntegers.Count - 1].ID := AID;
    Result := AIntegers.Item[AIntegers.Count - 1];
  end;
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TControl);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TCustomComboBox);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TCustomForm);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomSplitter);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData;
  AControl: TCustomCheckBox);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TSynEditBase
  );
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TCustomEdit);
begin
end;

end.

