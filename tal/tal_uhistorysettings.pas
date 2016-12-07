unit tal_uhistorysettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, tal_ihistorysettings, Controls, StdCtrls, Forms,
  trl_irttibroker, trl_ipersist, trl_upersist;

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

  THistoryData = class
  private
    fID: string;
    fPositions: IPersistManyTHistoryDataPositions;
    fTexts: IPersistManyTHistoryDataTexts;
  public
    procedure AfterConstruction; override;
  published
    property ID: string read fID write fID;
    property Positions: IPersistManyTHistoryDataPositions read fPositions;
    property Texts: IPersistManyTHistoryDataTexts read fTexts;
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
  public
    procedure Visit(const AHistory: IRBData; AControl: TControl); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomComboBox); virtual; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomForm); virtual; overload;
  end;

  { TControlSaveVisitor }

  TControlSaveVisitor = class(TControlVisitor)
  public
    procedure Visit(const AHistory: IRBData; AControl: TCustomComboBox); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomForm); override; overload;
  end;

  { TControlLoadVisitor }

  TControlLoadVisitor = class(TControlVisitor)
  public
    procedure Visit(const AHistory: IRBData; AControl: TCustomComboBox); override; overload;
    procedure Visit(const AHistory: IRBData; AControl: TCustomForm); override; overload;
  end;

  { THistorySettings }

  THistorySettings = class(TInterfacedObject, IHistorySettings)
  protected
    fStore: IPersistStore;
  protected
    procedure DispatchVisit(AControl: TControl; AHistory: IRBData;
      AVisitor: TControlVisitor);
    procedure AcceptVisitor(AControl: TWinControl; AHistory: IRBData;
      AVisitor: TControlVisitor); overload;
    procedure AcceptVisitor(AControl: TControl; AHistory: IRBData;
      AVisitor: TControlVisitor); overload;
    function GetHistoryData(const AID: string): IRBData;
  protected
    // IHistorySettings
    procedure Load(const ATopControl: TWinControl);
    procedure Save(const ATopControl: TWinControl);
  published
    property Store: IPersistStore read fStore write fStore;
  end;

implementation

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

{ THistoryData }

procedure THistoryData.AfterConstruction;
begin
  inherited AfterConstruction;
  fPositions := TPersistManyTHistoryDataPositions.Create;
  fTexts := TPersistManyTHistoryDataTexts.Create;
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
  else
    AVisitor.Visit(AHistory, AControl);
end;

procedure THistorySettings.AcceptVisitor(AControl: TWinControl; AHistory: IRBData;
  AVisitor: TControlVisitor);
var
  i: integer;
begin
  DispatchVisit(AControl, AHistory, AVisitor);
  for i := 0 to AControl.ControlCount - 1 do
    AcceptVisitor(AControl.Controls[i], AHistory, AVisitor);
end;

procedure THistorySettings.AcceptVisitor(AControl: TControl;
  AHistory: IRBData; AVisitor: TControlVisitor);
begin
  if AControl is TWinControl then
    AcceptVisitor(AControl as TWinControl, AHistory, AVisitor)
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

procedure THistorySettings.Load(const ATopControl: TWinControl);
var
  mLoadVisitor: TControlLoadVisitor;
  mHistoryData: IRBData;
begin
  Store.Open;
  try
    mHistoryData := GetHistoryData(ATopControl.Name);
    mLoadVisitor := TControlLoadVisitor.Create;
    try
      AcceptVisitor(ATopControl, mHistoryData, mLoadVisitor);
    finally
      mLoadVisitor.Free;
    end;
  finally
    Store.Close;
  end;
end;

procedure THistorySettings.Save(const ATopControl: TWinControl);
var
  mSaveVisitor: TControlSaveVisitor;
  mHistoryData: IRBData;
begin
  Store.Open;
  try
    mHistoryData := GetHistoryData(ATopControl.Name);
    mSaveVisitor := TControlSaveVisitor.Create;
    try
      AcceptVisitor(ATopControl, mHistoryData, mSaveVisitor);
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

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TControl);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TCustomComboBox);
begin
end;

procedure TControlVisitor.Visit(const AHistory: IRBData; AControl: TCustomForm);
begin
end;

end.

