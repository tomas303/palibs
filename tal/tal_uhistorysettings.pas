unit tal_uhistorysettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, tal_ihistorysettings, Controls, StdCtrls, Forms,
  trl_irttibroker, trl_ipersist, trl_upersist, ExtCtrls, SynEditMiscClasses;

type

  { THistoryData }

  THistoryData = class
  private
    fName: string;
    fID: string;
  published
    property Name: string read fName write fName;
    property ID: string read fID write fID;
  end;

  { THistoryDataPosition }

  THistoryDataPosition = class(THistoryData)
  private
    fTop: integer;
    fLeft: integer;
    fWidth: integer;
    fHeight: integer;
  published
    property Top: integer read fTop write fTop;
    property Left: integer read fLeft write fLeft;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
  end;


  { THistoryDataTexts }

  THistoryDataTexts = class(THistoryData)
  private
    fTexts: IPersistManyStrings;
  public
    procedure AfterConstruction; override;
  published
     property Texts: IPersistManyStrings read fTexts;
  end;

  { THistoryDataCheckBoxState }

  THistoryDataCheckBoxState = class(THistoryData)
  private
    fCheckBoxState: TCheckBoxState;
  published
     property CheckBoxState: TCheckBoxState read fCheckBoxState write fCheckBoxState;
  end;

  { THistoryDataIntegers }

  THistoryDataIntegers = class(THistoryData)
  private
    fIntegers: IPersistManyIntegers;
  public
    procedure AfterConstruction; override;
  published
     property Integers: IPersistManyIntegers read fIntegers;
  end;

  { THistoryDataMemo }

  THistoryDataMemo = class(THistoryData)
  private
    fMemo: TMemoString;
  published
     property Memo: TMemoString read fMemo write fMemo;
  end;

  { TControlVisitor }

  TControlVisitor = class
  protected type
    TFinder<TDATA: THistoryData> = class
    public
      // temporarily instead of GetHistoryData
      class function GetHistoryData(const AStore: IPersistStore; const AName, AID: string; ACanCreate: Boolean): IRBData;
    end;
  protected const
    cMaxTexts = 10;
  protected
    fStore: IPersistStore;
  protected
    // it cause internal error when use TDATA type(like classname)
    //function GetHistoryData<TDATA: THistoryData>(const AName, AID: string; ACanCreate: Boolean): TDATA;
  public
    constructor Create(AStore: IPersistStore);
    procedure Visit(AControl: TControl; const AID: string); virtual; abstract; overload;
    procedure Visit(AControl: TCustomComboBox; const AID: string); virtual; abstract; overload;
    procedure Visit(AControl: TCustomForm; const AID: string); virtual; abstract; overload;
    procedure Visit(AControl: TCustomSplitter; const AID: string); virtual; abstract; overload;
    procedure Visit(AControl: TCustomCheckBox; const AID: string); virtual; abstract; overload;
    procedure Visit(AControl: TSynEditBase; const AID: string); virtual; abstract; overload;
    procedure Visit(AControl: TCustomEdit; const AID: string); virtual; abstract; overload;
  end;

  { TControlSaveVisitor }

  TControlSaveVisitor = class(TControlVisitor)
  public
    procedure Visit(AControl: TControl; const AID: string); override; overload;
    procedure Visit(AControl: TCustomComboBox; const AID: string); override; overload;
    procedure Visit(AControl: TCustomForm; const AID: string); override; overload;
    procedure Visit(AControl: TCustomSplitter; const AID: string); override; overload;
    procedure Visit(AControl: TCustomCheckBox; const AID: string); override; overload;
    procedure Visit(AControl: TSynEditBase; const AID: string); override; overload;
    procedure Visit(AControl: TCustomEdit; const AID: string); override; overload;
  end;

  { TControlLoadVisitor }

  TControlLoadVisitor = class(TControlVisitor)
  public
    procedure Visit(AControl: TControl; const AID: string); override; overload;
    procedure Visit(AControl: TCustomComboBox; const AID: string); override; overload;
    procedure Visit(AControl: TCustomForm; const AID: string); override; overload;
    procedure Visit(AControl: TCustomSplitter; const AID: string); override; overload;
    procedure Visit(AControl: TCustomCheckBox; const AID: string); override; overload;
    procedure Visit(AControl: TSynEditBase; const AID: string); override; overload;
    procedure Visit(AControl: TCustomEdit; const AID: string); override; overload;
  end;

  { THistorySettings }

  THistorySettings = class(TInterfacedObject, IHistorySettings)
  protected
    fStore: IPersistStore;
  protected
    procedure DispatchVisit(AControl: TControl; const AID: string;
      AVisitor: TControlVisitor);
    procedure AcceptVisitor(AControl: TWinControl; AVisitor: TControlVisitor;
      const AID: string; AInside: Boolean = True); overload;
    procedure AcceptVisitor(AControl: TControl; AVisitor: TControlVisitor;
      const AID: string; AInside: Boolean = True); overload;
    function GetHistoryData(const AID: string): IRBData;
  protected
    // IHistorySettings
    procedure Load(const ATopControl: TWinControl; AInside: Boolean = True); overload;
    procedure Load(const ATopControl: TWinControl; const AID: string; AInside: Boolean = True); overload;
    procedure Save(const ATopControl: TWinControl; AInside: Boolean = True); overload;
    procedure Save(const ATopControl: TWinControl; const AID: string; AInside: Boolean = True); overload;
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

{ TControlSaveVisitor }

procedure TControlSaveVisitor.Visit(AControl: TControl; const AID: string);
begin
end;

procedure TControlSaveVisitor.Visit(AControl: TCustomComboBox; const AID: string);
var
  i: integer;
  mData: IRBData;
  mTexts: THistoryDataTexts;
  mComboText: string;
begin
  mData := TFinder<THistoryDataTexts>.GetHistoryData(fStore, AControl.Name, AID, True);
  mTexts := mData.UnderObject as THistoryDataTexts;
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
  fStore.Save(mData);
end;

procedure TControlSaveVisitor.Visit(AControl: TCustomForm; const AID: string);
var
  mData: IRBData;
  mPosition: THistoryDataPosition;
begin
  mData := TFinder<THistoryDataPosition>.GetHistoryData(fStore, AControl.Name, AID, True);
  mPosition := mData.UnderObject as THistoryDataPosition;
  mPosition.Left := AControl.Left;
  mPosition.Top := AControl.Top;
  mPosition.Width := AControl.Width;
  mPosition.Height := AControl.Height;
  fStore.Save(mData);
end;

procedure TControlSaveVisitor.Visit(AControl: TCustomSplitter; const AID: string);
var
  mData: IRBData;
  mIntegers: THistoryDataIntegers;
begin
  mData := TFinder<THistoryDataIntegers>.GetHistoryData(fStore, AControl.Name, AID, True);
  mIntegers := mData.UnderObject as THistoryDataIntegers;
  mIntegers.Integers.Count := 1;
  if AControl.ResizeAnchor in [akLeft, akRight] then
    mIntegers.Integers[0] := Round(AControl.GetSplitterPosition * 10000 / AControl.Parent.Width)
  else
    mIntegers.Integers[0] := Round(AControl.GetSplitterPosition * 10000 / AControl.Parent.Height);
  fStore.Save(mData);
end;

procedure TControlSaveVisitor.Visit(AControl: TCustomCheckBox; const AID: string);
var
  mData: IRBData;
  mCheckBoxState: THistoryDataCheckBoxState;
begin
  mData := TFinder<THistoryDataCheckBoxState>.GetHistoryData(fStore, AControl.Name, AID, True);
  mCheckBoxState := mData.UnderObject as THistoryDataCheckBoxState;
  mCheckBoxState.CheckBoxState := AControl.State;
  fStore.Save(mData);
end;

procedure TControlSaveVisitor.Visit(AControl: TSynEditBase; const AID: string);
var
  mData: IRBData;
  mMemo: THistoryDataMemo;
begin
  mData := TFinder<THistoryDataMemo>.GetHistoryData(fStore, AControl.Name, AID, True);
  mMemo := mData.UnderObject as THistoryDataMemo;
  mMemo.Memo := AControl.Lines.Text;
  fStore.Save(mData);
end;

procedure TControlSaveVisitor.Visit(AControl: TCustomEdit; const AID: string);
var
  mData: IRBData;
  mTexts: THistoryDataTexts;
begin
  mData := TFinder<THistoryDataTexts>.GetHistoryData(fStore, AControl.Name, AID, True);
  mTexts := mData.UnderObject as THistoryDataTexts;
  mTexts.Texts.Count := 1;
  mTexts.Texts[0] := AControl.Text;
  fStore.Save(mData);
end;

{ TControlLoadVisitor }

procedure TControlLoadVisitor.Visit(AControl: TControl; const AID: string);
begin
end;

procedure TControlLoadVisitor.Visit(AControl: TCustomComboBox; const AID: string);
var
  mData: IRBData;
  i: integer;
  mTexts: THistoryDataTexts;
begin
  mData := TFinder<THistoryDataTexts>.GetHistoryData(fStore, AControl.Name, AID, False);
  if mData = nil then
    Exit;
  mTexts := mData.UnderObject as THistoryDataTexts;
  AControl.Clear;
  for i := 0 to mTexts.Texts.Count - 1 do
    AControl.Items.Insert(0, mTexts.Texts[i]);
  if AControl.Items.Count > 0 then
    AControl.ItemIndex := 0;
end;

procedure TControlLoadVisitor.Visit(AControl: TCustomForm; const AID: string);
var
  mData: IRBData;
  mPosition: THistoryDataPosition;
begin
  mData := TFinder<THistoryDataPosition>.GetHistoryData(fStore, AControl.Name, AID, False);
  if mData = nil then
    Exit;
  mPosition := mData.UnderObject as THistoryDataPosition;
  AControl.Left := mPosition.Left;
  AControl.Top := mPosition.Top;
  AControl.Width := mPosition.Width;
  AControl.Height := mPosition.Height;
end;

procedure TControlLoadVisitor.Visit(AControl: TCustomSplitter; const AID: string);
var
  mData: IRBData;
  mIntegers: THistoryDataIntegers;
begin
  mData := TFinder<THistoryDataIntegers>.GetHistoryData(fStore, AControl.Name, AID, False);
  if mData = nil then
    Exit;
  mIntegers := mData.UnderObject as THistoryDataIntegers;
  if mIntegers.Integers.Count > 0 then begin
    if AControl.ResizeAnchor in [akLeft, akRight] then begin
      AControl.SetSplitterPosition((mIntegers.Integers[0] * AControl.Parent.Width) DIV 10000);
      if AControl.GetSplitterPosition < 0 then
        AControl.SetSplitterPosition(0)
      else if AControl.GetSplitterPosition > AControl.Parent.Width - 1 then
        AControl.SetSplitterPosition(AControl.Parent.Width - 1);
    end
    else
    begin
      AControl.SetSplitterPosition((mIntegers.Integers[0] * AControl.Parent.Height) DIV 10000);
      if AControl.GetSplitterPosition < 0 then
        AControl.SetSplitterPosition(0)
      else if AControl.GetSplitterPosition > AControl.Parent.Height - 1 then
        AControl.SetSplitterPosition(AControl.Parent.Height - 1);
    end;
  end;
end;

procedure TControlLoadVisitor.Visit(AControl: TCustomCheckBox; const AID: string);
var
  mData: IRBData;
  mCheckBoxState: THistoryDataCheckBoxState;
begin
  mData := TFinder<THistoryDataCheckBoxState>.GetHistoryData(fStore, AControl.Name, AID, False);
  if mData = nil then
    Exit;
  mCheckBoxState := mData.UnderObject as THistoryDataCheckBoxState;
  AControl.State := mCheckBoxState.CheckBoxState;
end;

procedure TControlLoadVisitor.Visit(AControl: TSynEditBase; const AID: string);
var
  mData: IRBData;
  mMemo: THistoryDataMemo;
begin
  mData := TFinder<THistoryDataMemo>.GetHistoryData(fStore, AControl.Name, AID, False);
  if mData = nil then
    Exit;
  mMemo := mData.UnderObject as THistoryDataMemo;
  AControl.Lines.Text := mMemo.Memo;
end;

procedure TControlLoadVisitor.Visit(AControl: TCustomEdit; const AID: string);
var
  mData: IRBData;
  mTexts: THistoryDataTexts;
begin
  mData := TFinder<THistoryDataTexts>.GetHistoryData(fStore, AControl.Name, AID, False);
  if mData = nil then
    Exit;
  mTexts := mData.UnderObject as THistoryDataTexts;
  if mTexts.Texts.Count > 0 then
    AControl.Text := mTexts.Texts[0];
end;

{ THistoryDataTexts }

procedure THistoryDataTexts.AfterConstruction;
begin
  inherited AfterConstruction;
  fTexts := TPersistManyStrings.Create;
end;

{ THistorySettings }

procedure THistorySettings.DispatchVisit(AControl: TControl;
  const AID: string; AVisitor: TControlVisitor);
begin
  // because of helpers do not support virtual methods, dispatch to visitor methods
  // need to be done here staticaly
  if AControl is TCustomForm then
    AVisitor.Visit(AControl as TCustomForm, AID)
  else if AControl is TCustomComboBox then
    AVisitor.Visit(AControl as TCustomComboBox, AID)
  else if AControl is TCustomSplitter then
    AVisitor.Visit(AControl as TCustomSplitter, AID)
  else if AControl is TCustomCheckBox then
    AVisitor.Visit(AControl as TCustomCheckBox, AID)
  else if AControl is TSynEditBase then
    AVisitor.Visit(AControl as TSynEditBase, AID)
  else if AControl is TCustomEdit then
    AVisitor.Visit(AControl as TCustomEdit, AID)
  else
    AVisitor.Visit(AControl, AID);
end;

procedure THistorySettings.AcceptVisitor(AControl: TWinControl;
  AVisitor: TControlVisitor; const AID: string; AInside: Boolean = True);
var
  i: integer;
begin
  DispatchVisit(AControl, AID, AVisitor);
  if AInside then
    for i := 0 to AControl.ControlCount - 1 do
      AcceptVisitor(AControl.Controls[i], AVisitor, AID, AInside);
end;

procedure THistorySettings.AcceptVisitor(AControl: TControl;
  AVisitor: TControlVisitor; const AID: string; AInside: Boolean = True);
begin
  if AControl is TWinControl then
    AcceptVisitor(AControl as TWinControl, AVisitor, AID, AInside)
  else
    DispatchVisit(AControl, AID, AVisitor);
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
begin
  Load(ATopControl, '',AInside);
end;

procedure THistorySettings.Load(const ATopControl: TWinControl;
  const AID: string; AInside: Boolean);
var
  mLoadVisitor: TControlLoadVisitor;
begin
  Store.Open;
  try
    mLoadVisitor := TControlLoadVisitor.Create(Store);
    try
      AcceptVisitor(ATopControl, mLoadVisitor, AID, AInside);
    finally
      mLoadVisitor.Free;
    end;
  finally
    Store.Close;
  end;
end;

procedure THistorySettings.Save(const ATopControl: TWinControl; AInside: Boolean = True);
begin
  Save(ATopControl, '', AInside);
end;

procedure THistorySettings.Save(const ATopControl: TWinControl;
  const AID: string; AInside: Boolean);
var
  mSaveVisitor: TControlSaveVisitor;
begin
  Store.Open;
  try
    mSaveVisitor := TControlSaveVisitor.Create(Store);
    try
      AcceptVisitor(ATopControl, mSaveVisitor, AID, AInside);
    finally
      mSaveVisitor.Free;
    end;
  finally
    Store.Close;
  end;
end;

{ TControlVisitor }

//function TControlVisitor.GetHistoryData<TDATA>(const AName, AID: string; ACanCreate: Boolean): TDATA;
//var
//  mList: IPersistRefList;
//  i: integer;
//begin
//  Result := nil;
//  mList := (fStore as IPersistQuery).SelectClass(''{TDATA.ClassName});
//  for i := 0 to mList.Count - 1 do begin
//    if (mList.Data[i].ItemByName['Name'].AsString = AName)
//      and (mList.Data[i].ItemByName['ID'].AsString = AID)
//    then begin
//      Result := mList.Data[i].UnderObject as TDATA;
//      Break;
//    end;
//  end;
//  if (Result = nil) and ACanCreate then begin
//    Result := fStore.New({TDATA.ClassName}'').UnderObject as TDATA;
//    //Result.Name := AName;
//    //Result.ID := AID;
//  end;
//end;

constructor TControlVisitor.Create(AStore: IPersistStore);
begin
  fStore := AStore;
end;

class function TControlVisitor.TFinder<TDATA: THistoryData>.GetHistoryData(const AStore: IPersistStore;
  const AName, AID: string; ACanCreate: Boolean): IRBData;
var
  mList: IPersistRefList;
  i: integer;
begin
  Result := nil;
  mList := (AStore as IPersistQuery).SelectClass(TDATA.ClassName);
  for i := 0 to mList.Count - 1 do begin
    if (mList.Data[i].ItemByName['Name'].AsString = AName)
      and (mList.Data[i].ItemByName['ID'].AsString = AID)
    then begin
      Result := mList.Data[i];
      Break;
    end;
  end;
  if (Result = nil) and ACanCreate then begin
    Result := AStore.New(TDATA.ClassName);
    Result.ItemByName['Name'].AsString := AName;
    Result.ItemByName['ID'].AsString := AID;
  end;
end;

end.

