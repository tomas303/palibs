(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit rea_udesigncomponent;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  rea_idesigncomponent, rea_ichannels,
  trl_usystem, trl_imetaelement, trl_imetaelementfactory,
  trl_iprops, rea_ibits, trl_itree, trl_idifactory, trl_ilog,
  sysutils, rea_ilayout, Graphics, LCLType, fgl,
  trl_udifactory, trl_pubsub, tvl_itimer;

type

  { TMorph }

  TMorph = class(TInterfacedObject, IMorph)
  private
    function NewProps: IProps;
  private
    function WrapInStrip(const AComponent: IDesignComponent; ASize: Integer; APlace: Integer): IDesignComponent;
    function NewPage(const ACaption: String; ALayout: Integer; const AComponents: TArray<IDesignComponent>): IDesignComponent;
    function StickLabel(const AComponent: IDesignComponent; const ACaption: String; AEdge: Integer; ASize: Integer): IDesignComponent;
    function WrapUp(const AComponent: IDesignComponent; AHeight: Integer): IDesignComponent; overload;
    function WrapUp(const AComponent: IDesignComponent; AHeight: Integer; const ACaption: String; ACaptionWidth: Integer): IDesignComponent; overload;
    function WrapUpElastic(const AComponent: IDesignComponent; const ACaption: String; ACaptionWidth: Integer): IDesignComponent;
  protected
    fFactory2: TDIFactory2;
  published
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
  end;

  { TDesignComponent }

  TDesignComponent = class(TDynaObject, IDesignComponent, INode)
  protected
    function NewProps: IProps;
    function NewComposeProps: IProps; virtual;
  protected
    function DoCompose: IMetaElement; virtual; abstract;
    procedure AddChildren(const AElement: IMetaElement; const AChildren: TMetaElementArray);
    procedure ComposeChildren(const AParentEl: IMetaElement);
    procedure DoStartingValues; virtual;
  protected
    // IDesignComponent = interface
    function Compose: IMetaElement;
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
    fFactory2: TDIFactory2;
    fNode: INode;
    fPubSub: IPubSub;
    fMorph: IMorph;
  published
    property ID: string read fID write fID;
    property Log: ILog read fLog write fLog;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Factory: IDIFactory read fFactory write fFactory;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property Node: INode read fNode write fNode;
    property PubSub: IPubSub read fPubSub write fPubSub;
    property Morph: IMorph read fMorph write fMorph;
  end;

  { TDesignComponentForm }

  TDesignComponentForm = class(TDesignComponent, IDesignComponentForm)
  private
    fPSSizeChannel: IPSSizeChannel;
    procedure PSSizeChannelObserver(const AData: TSizeData);
    function PSSizeChannel: IPSSizeChannel;
  private
    fPSPositionChannel: IPSPositionChannel;
    procedure PSPositionChannelObserver(const AData: TPositionData);
    function PSPositionChannel: IPSPositionChannel;
  private
    fPSCloseChannel: IPSCloseChannel;
    function PSCloseChannel: IPSCloseChannel;
  private
    fPSActivateChannel: IPSActivateChannel;
    function PSActivateChannel: IPSActivateChannel;
  private
    fPSKeyDownChannel: IPSKeyChannel;
    function PSKeyDownChannel: IPSKeyChannel;
  protected
    procedure DoStartingValues; override;
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  protected
    fPSGUIChannel: IPSGUIChannel;
    fMMLeft: Integer;
    fMMTop: Integer;
    fMMWidth: Integer;
    fMMHeight: Integer;
  published
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
    property MMLeft: Integer read fMMLeft write fMMLeft;
    property MMTop: Integer read fMMTop write fMMTop;
    property MMWidth: Integer read fMMWidth write fMMWidth;
    property MMHeight: Integer read fMMHeight write fMMHeight;
  end;

  { TDesignComponentEdit }

  TDesignComponentEdit = class(TDesignComponent, IDesignComponentEdit)
  private
    fPSTextChannel: IPSTextChannel;
    procedure PSTextChannelObserver(const AValue: String);
  private
    fPSKeyDownChannel: IPSKeyChannel;
  private
    fIsFocused: Boolean;
    fPSFocusChannel: IPSFocusChannel;
    procedure PSFocusChannelObserver(const AValue: TFocusData);
  private
    fPSSecretChannel: IPSSecretChannel;
    procedure PSSecretChannelObserver(const AValue: TSecretData);
  protected
    function PSTextChannel: IPSTextChannel;
    function PSKeyDownChannel: IPSKeyChannel;
    function PSFocusChannel: IPSFocusChannel;
    function PSSecretChannel: IPSSecretChannel;
  protected
    procedure DoStartingValues; override;
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  protected
    fText: String;
    function GetText: String;
    procedure SetText(AText: String);
  published
    property Text: String read GetText write SetText;
  end;

  { TDesignComponentMemo }

  TDesignComponentMemo = class(TDesignComponent, IDesignComponentMemo)
  private
    fPSTextChannel: IPSTextChannel;
    procedure PSTextChannelObserver(const AValue: String);
  private
    fPSKeyDownChannel: IPSKeyChannel;
  private
    fIsFocused: Boolean;
    fPSFocusChannel: IPSFocusChannel;
    procedure PSFocusChannelObserver(const AValue: TFocusData);
  protected
    function PSTextChannel: IPSTextChannel;
    function PSKeyDownChannel: IPSKeyChannel;
    function PSFocusChannel: IPSFocusChannel;
  protected
    procedure DoStartingValues; override;
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  protected
    fText: String;
    function GetText: String;
    procedure SetText(AText: String);
  published
    property Text: String read GetText write SetText;
  end;

  { TDesignComponentButton }

  TDesignComponentButton = class(TDesignComponent, IDesignComponentButton)
  private
    fPSClickChannel: IPSClickChannel;
    function PSClickChannel: IPSClickChannel;
  private
    function NewOuterProps: IProps;
  protected
    procedure InitValues; override;
    procedure DoStartingValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  public
    procedure BeforeDestruction; override;
  end;

  { TDesignComponentStrip }

  TDesignComponentStrip = class(TDesignComponent, IDesignComponentStrip)
  protected
    function DoCompose: IMetaElement; override;
  end;

  { TDesignComponentBox }

  TDesignComponentBox = class(TDesignComponent)
  private
    procedure MakeChildren(const AParentEl: IMetaElement);
  protected
    function BoxLayout: Integer; virtual; abstract;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  end;

  { TDesignComponentHBox }

  TDesignComponentHBox = class(TDesignComponentBox, IDesignComponentHBox)
  protected
    function BoxLayout: Integer; override;
  public
    procedure AfterConstruction; override;
  end;

  { TDesignComponentVBox }

  TDesignComponentVBox = class(TDesignComponentBox, IDesignComponentVBox)
  protected
    function BoxLayout: Integer; override;
  end;

  { TDesignComponentText }

  TDesignComponentText = class(TDesignComponent, IDesignComponentText)
  private
    fPSTextChannel: IPSTextChannel;
    procedure PSTextChannelObserver(const AValue: String);
  protected
    function PSTextChannel: IPSTextChannel;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  protected
    fText: String;
    function GetText: String;
    procedure SetText(AText: String);
  published
    property Text: String read GetText write SetText;
  end;

  { TDesignComponentGrid }

  TDesignComponentGrid = class(TDesignComponent, IDesignComponentGrid)
  private type

    { TShiftInfo }

    TShiftInfo = record
    strict private
      fFirst: Integer;
      fLast: Integer;
      fDistance: Integer;
    public
      constructor Create(AFirst, ALast, ADistance: Integer);
      property First: Integer read fFirst;
      property Last: Integer read fLast;
      property Distance: Integer read fDistance;
    end;

  private
    fPSGridCmdMoveChannel: IPSGridCmdMoveChannel;
    function PSGridCmdMoveChannel: IPSGridCmdMoveChannel;
  private
    fPSGridCmdInfoChannel: IPSGridCmdInfoChannel;
    function PSGridCmdInfoChannel: IPSGridCmdInfoChannel;
  private
    fPSGridCmdRowChannel: IPSGridCmdRowChannel;
    function PSGridCmdRowChannel: IPSGridCmdRowChannel;
  private
    fPSGridCmdFieldChannel: IPSGridCmdFieldChannel;
    function PSGridCmdFieldChannel: IPSGridCmdFieldChannel;
  private
    fPSGridRecordChannel: IPSGridRecordChannel;
    procedure PSGridRecordChannelObserver(const AData: TGridRecord);
    function PSGridRecordChannel: IPSGridRecordChannel;
  private
    fPSGridMoverChannel: IPSGridMoverChannel;
    procedure PSGridMoverChannelObserver(const AData: TGridMover);
    function PSGridMoverChannel: IPSGridMoverChannel;
  private
    fPSLayoutChannel: IPSLayoutChannel;
    procedure PSLayoutChannelObserver(const AValue: TLayoutData);
  private
    fPSSecretChannel: IPSSecretChannel;
    function PSSecretChannel: IPSSecretChannel;
    procedure PSSecretChannelObserver(const AValue: TSecretData);
  private type
    TMatrix = array of array of String;
    TColsHideText = array of Boolean;
  private
    fSourceRow: Integer;
    fData: TMatrix;
    fCurrentRow: Integer;
    fCurrentCol: Integer;
    fEdit: IDesignComponentEdit;
    fEditFocused: Boolean;
    fColsHideText: TColsHideText;
    procedure PSTextChannelObserver(const AValue: String);
    procedure PSKeyDownChannelObserver(const AValue: TKeyData);
    procedure PSFocusChannelObserver(const AValue: TFocusData);
    procedure MoveVertically(ADelta: Integer);
    procedure MoveHorizontally(ADelta: Integer);
    procedure CopyRow(AFrom, ATo: Integer);
    procedure ShiftRows(AInfo: TShiftInfo);
    procedure ChangeRowData(ARow: Integer; AData: TArray<String>);
    procedure EraseRowData(ARow: Integer);
    procedure InsertRecord;
    procedure DeleteRecord;
    procedure SentGUIRender;
    procedure SentEditChange;
    procedure SentRows(AFrom, ATo: Integer);
  private
    function ColProps(Row, Col: integer): IProps;
    function RowProps(Row: integer): IProps;
    function MakeRow(Row: integer): TMetaElementArray;
    function MakeGrid: TMetaElementArray;
    function LaticeColProps: IProps;
    function LaticeRowProps: IProps;
    function Latice(AElements: TMetaElementArray; ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
    function ComposeEdit: IMetaElement;
    function RenderText(Row, Col: integer): String;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  protected
    fPSGUIChannel: IPSGUIChannel;
    fRowCount: Integer;
    fColCount: Integer;
    fLaticeColColor: integer;
    fLaticeRowColor: integer;
    fLaticeColSize: integer;
    fLaticeRowSize: integer;
    procedure SetRowCount(AValue: Integer);
    procedure SetColCount(AValue: Integer);
  published
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
    property RowCount: Integer read fRowCount write SetRowCount;
    property ColCount: Integer read fColCount write SetColCount;
    property LaticeColColor: Integer read fLaticeColColor write fLaticeColColor;
    property LaticeRowColor: Integer read fLaticeRowColor write fLaticeRowColor;
    property LaticeColSize: Integer read fLaticeColSize write fLaticeColSize;
    property LaticeRowSize: Integer read fLaticeRowSize write fLaticeRowSize;
  end;

  { TDesignComponentPager }

  TDesignComponentPager = class(TDesignComponent, IDesignComponentPager)
  private
    fIndex: Integer;
    fPSPagerChannel: IPSPagerChannel;
    fSwitch: TArray<IDesignComponent>;
    procedure PSPagerChannelObserver(const AIndex: Integer);
  private
    function RenderPage(const APageElement: IMetaElement): IMetaElement;
    function MakeSwitch: IMetaElement;
    function MakeBody: IMetaElement;
    function NewSwitch: TArray<IDesignComponent>;
    function NewSwitchFunc(AIndex: Integer): TPubSubNewData<Integer>;
  protected
    procedure InitValues; override;
    function NewComposeProps: IProps; override;
    function DoCompose: IMetaElement; override;
  public
    procedure BeforeDestruction; override;
  protected
    fSwitchEdge: Integer;
    fSwitchSize: Integer;
    fPSGUIChannel: IPSGUIChannel;
  published
    property SwitchEdge: Integer read fSwitchEdge write fSwitchEdge;
    property SwitchSize: Integer read fSwitchSize write fSwitchSize;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

  { TDesignComponentFilter }

  TDesignComponentFilter = class(TDesignComponent, IDesignComponentFilter)
  strict private
    fFilterText, fLastTriggeredText: String;
    fFilterEdit: IDesignComponentEdit;
    fTimer: ITimer;
    fPSTextFilterChannel: IPSTextFilterChannel;
    procedure TimerObserver;
    procedure FilterEditTextChannelObserver(const AText: String);
    function PSTextFilterChannel: IPSTextFilterChannel;
    function PSFocusChannel: IPSFocusChannel;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
  public
    destructor Destroy; override;
  protected
    fInterval: Integer;
  published
    property Interval: Integer read fInterval write fInterval;
  end;



implementation

{ TDesignComponentFilter }

procedure TDesignComponentFilter.TimerObserver;
begin
  if fLastTriggeredText <> fFilterText then begin
    fLastTriggeredText := fFilterText;
    fPSTextFilterChannel.Publish(fLastTriggeredText);
  end;
end;

procedure TDesignComponentFilter.FilterEditTextChannelObserver(const AText: String);
begin
  fFilterText := AText;
  fTimer.Restart;
end;

function TDesignComponentFilter.PSTextFilterChannel: IPSTextFilterChannel;
begin
  Result := fPSTextFilterChannel;
end;

function TDesignComponentFilter.PSFocusChannel: IPSFocusChannel;
begin
  Result := fFilterEdit.PSFocusChannel;
end;

procedure TDesignComponentFilter.InitValues;
begin
  inherited InitValues;
  fPSTextFilterChannel := PubSub.Factory.NewDataChannel<String>;
  fTimer := Factory2.Locate<ITimer>(NewProps.SetInt('Interval', fInterval));
  fTimer.Subscribe(TimerObserver);
  fFilterEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cEdit.ID, ID)
    .SetBool(cEdit.Flat, SelfProps.AsBool(cEdit.Flat))
    .SetBool(cEdit.TabStop, SelfProps.AsBool(cEdit.TabStop))
    .SetBool(cEdit.Transparent, SelfProps.AsBool(cEdit.Transparent))
    .SetInt(cEdit.Color, SelfProps.AsInt(cEdit.Color))
    );
  fFilterEdit.PSTextChannel.Subscribe(FilterEditTextChannelObserver);
  fTimer.Enabled := True;
end;

function TDesignComponentFilter.DoCompose: IMetaElement;
begin
  Result := fFilterEdit.Compose;
end;

destructor TDesignComponentFilter.Destroy;
begin
  inherited Destroy;
end;

{ TMorph }

function TMorph.NewProps: IProps;
begin
  Result := Factory2.Locate<IProps>;
end;

function TMorph.WrapInStrip(const AComponent: IDesignComponent; ASize: Integer;
  APlace: Integer): IDesignComponent;
begin
  Result := Factory2.Locate<IDesignComponentStrip>(NewProps
      .SetInt(cProps.Place, APlace)
      .SetInt(cProps.PlaceSize, ASize)
      .SetBool(cProps.Transparent, True)
    );
    (Result as INode).AddChild(AComponent as INode);
end;

function TMorph.NewPage(const ACaption: String; ALayout: Integer;
  const AComponents: TArray<IDesignComponent>): IDesignComponent;
var
  mDC: IDesignComponent;
begin
  Result := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetBool(cProps.Transparent, True)
    .SetStr(cProps.Caption, ACaption)
    .SetInt(cProps.Layout, ALayout)
  );
  for mDC in AComponents do begin
    (Result as INode).AddChild(mDC as INode);
  end;
end;

function TMorph.StickLabel(const AComponent: IDesignComponent;
  const ACaption: String; AEdge: Integer; ASize: Integer): IDesignComponent;

  function GetLayout: Integer;
  begin
    case AEdge of
      cEdge.Left, cEdge.Right: Result := cLayout.Horizontal;
      cEdge.Top, cEdge.Bottom: Result := cLayout.Vertical;
    else
      raise Exception.Create('unsupported caption edge');
    end;
  end;

  function GetLabelPlace: Integer;
  begin
    case AEdge of
      cEdge.Left, cEdge.Top: Result := cPlace.FixFront;
      cEdge.Right, cEdge.Bottom: Result := cPlace.FixBack;
    else
      raise Exception.Create('unsupported caption edge');
    end;
  end;

  function NewLabel: IDesignComponent;
  begin
    Result := Factory2.Locate<IDesignComponentText>(NewProps
      .SetInt(cProps.Place, GetLabelPlace)
      .SetInt(cProps.PlaceSize, ASize)
      .SetStr(cProps.Text, ACaption)
    );
  end;

begin
  Result := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Layout, GetLayout)
    .SetBool(cProps.Transparent, True)
  );
  case AEdge of
    cEdge.Left, cEdge.Top: begin
      (Result as INode).AddChild(NewLabel as INode);
      (Result as INode).AddChild(AComponent as INode);
    end;
    cEdge.Right, cEdge.Bottom: begin
      (Result as INode).AddChild(AComponent as INode);
      (Result as INode).AddChild(NewLabel as INode);
    end;
  else
    raise Exception.Create('unsupported caption edge');
  end;
end;

function TMorph.WrapUp(const AComponent: IDesignComponent; AHeight: Integer
  ): IDesignComponent;
begin
  Result := WrapInStrip(AComponent, AHeight, cPlace.FixFront);
end;

function TMorph.WrapUp(const AComponent: IDesignComponent; AHeight: Integer;
  const ACaption: String; ACaptionWidth: Integer): IDesignComponent;
begin
  Result := WrapInStrip(
    StickLabel(AComponent, ACaption, cEdge.Left, ACaptionWidth),
    AHeight, cPlace.FixFront
  );
end;

function TMorph.WrapUpElastic(const AComponent: IDesignComponent;
  const ACaption: String; ACaptionWidth: Integer
  ): IDesignComponent;
begin
  Result := WrapInStrip(
    StickLabel(AComponent, ACaption, cEdge.Left, ACaptionWidth),
    0, cPlace.Elastic
  );
end;

{ TDesignComponentGrid.TShiftInfo }

constructor TDesignComponentGrid.TShiftInfo.Create(AFirst, ALast,
  ADistance: Integer);
begin
  fFirst := AFirst;
  fLast := ALast;
  fDistance := ADistance;
end;

{ TDesignComponentHBox }

function TDesignComponentHBox.BoxLayout: Integer;
begin
  Result := cLayout.Horizontal;
end;

procedure TDesignComponentHBox.AfterConstruction;
begin
  inherited AfterConstruction;
end;

{ TDesignComponentVBox }

function TDesignComponentVBox.BoxLayout: Integer;
begin
  Result := cLayout.Vertical;
end;

{ TDesignComponentBox }

procedure TDesignComponentBox.MakeChildren(const AParentEl: IMetaElement);
var
  i: Integer;
  mNode: INode;
  mEl: IMetaElement;
  mStrip: IMetaElement;
  mStripProps: IProps;
begin
  mStripProps := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetBool(cProps.Transparent, AParentEl.Props.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, AParentEl.Props.AsInt(cProps.Color))
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.PlaceSize, SelfProps.AsInt(cProps.BoxLaticeSize));
  mStrip := ElementFactory.CreateElement(IStripBit, mStripProps);
  (AParentEl as INode).AddChild(mStrip as INode);
  for i := 0 to Count - 1 do begin
    mEl := (GetChild(i) as IDesignComponent).Compose;
    (AParentEl as INode).AddChild(mEl as INode);
    mStrip := ElementFactory.CreateElement(IStripBit, mStripProps);
    (AParentEl as INode).AddChild(mStrip as INode);
  end;
end;

function TDesignComponentBox.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps
    .SetInt(cProps.Layout, BoxLayout)
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent));
end;

function TDesignComponentBox.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps);
  MakeChildren(Result);
end;

{ TDesignComponentText }

procedure TDesignComponentText.PSTextChannelObserver(const AValue: String);
begin
  fText := AValue;
end;

function TDesignComponentText.PSTextChannel: IPSTextChannel;
begin
  Result := fPSTextChannel;
end;

procedure TDesignComponentText.InitValues;
begin
  inherited InitValues;
  fPSTextChannel := PubSub.Factory.NewDataChannel<String>;
  fPSTextChannel.Subscribe(PSTextChannelObserver);
end;

function TDesignComponentText.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result.SetStr(cProps.Text, Text);
end;

function TDesignComponentText.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(ITextBit, NewComposeProps);
end;

function TDesignComponentText.GetText: String;
begin
  Result := fText;
end;

procedure TDesignComponentText.SetText(AText: String);
begin
  if AText <> fText then begin
    fText := AText;
    if fPSTextChannel <> nil then
      fPSTextChannel.Publish(fText);
  end;
end;

{ TDesignComponentEdit }

procedure TDesignComponentEdit.PSTextChannelObserver(const AValue: String);
begin
  fText := AValue;
end;

procedure TDesignComponentEdit.PSSecretChannelObserver(const AValue: TSecretData);
begin
  if AValue.Visible then
    SelfProps.SetStr(cEdit.PasswordChar, #0)
  else
    SelfProps.SetStr(cEdit.PasswordChar, '*');
end;

procedure TDesignComponentEdit.PSFocusChannelObserver(const AValue: TFocusData);
begin
  fIsFocused := AValue.Focused;
end;

function TDesignComponentEdit.PSTextChannel: IPSTextChannel;
begin
  Result := fPSTextChannel;
end;

function TDesignComponentEdit.PSKeyDownChannel: IPSKeyChannel;
begin
  Result := fPSKeyDownChannel;
end;

function TDesignComponentEdit.PSFocusChannel: IPSFocusChannel;
begin
  Result := fPSFocusChannel;
end;

function TDesignComponentEdit.PSSecretChannel: IPSSecretChannel;
begin
  Result := fPSSecretChannel;
end;

function TDesignComponentEdit.GetText: String;
begin
  Result := fText;
end;

procedure TDesignComponentEdit.DoStartingValues;
begin
  inherited DoStartingValues;
  SelfProps.SetBool(cProps.Flat, True);
  SelfProps.SetBool(cEdit.TabStop, True);
  SelfProps.SetStr(cEdit.PasswordChar, '');
end;

procedure TDesignComponentEdit.InitValues;
begin
  inherited InitValues;
  fPSTextChannel := PubSub.Factory.NewDataChannel<String>;
  fPSTextChannel.Subscribe(PSTextChannelObserver);
  fPSKeyDownChannel := PubSub.Factory.NewDataChannel<TKeyData>;
  fPSFocusChannel := PubSub.Factory.NewDataChannel<TFocusData>;
  fPSFocusChannel.Subscribe(PSFocusChannelObserver);
  fPSSecretChannel := PubSub.Factory.NewDataChannel<TSecretData>;
  fPSSecretChannel.Subscribe(PSSecretChannelObserver);
end;

function TDesignComponentEdit.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetStr(cEdit.Text, Text)
    .SetBool(cEdit.Focused, fIsFocused)
    .SetBool(cEdit.Flat, SelfProps.AsBool(cEdit.Flat))
    .SetBool(cEdit.TabStop, SelfProps.AsBool(cEdit.TabStop))
    .SetStr(cEdit.PasswordChar, SelfProps.AsStr(cEdit.PasswordChar))
    .SetIntf(cEdit.PSTextChannel, PSTextChannel)
    .SetIntf(cEdit.PSKeyDownChannel, PSKeyDownChannel)
    .SetIntf(cEdit.PSFocusChannel, PSFocusChannel)
end;

function TDesignComponentEdit.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IEditBit, NewComposeProps);
end;

procedure TDesignComponentEdit.SetText(AText: String);
begin
  if AText <> fText then begin
    fText := AText;
    fPSTextChannel.Publish(fText);
  end;
end;

{ TDesignComponentMemo }

procedure TDesignComponentMemo.PSTextChannelObserver(const AValue: String);
begin
  fText := AValue;
end;

procedure TDesignComponentMemo.PSFocusChannelObserver(const AValue: TFocusData);
begin
  fIsFocused := AValue.Focused;
end;

function TDesignComponentMemo.PSTextChannel: IPSTextChannel;
begin
  Result := fPSTextChannel;
end;

function TDesignComponentMemo.PSKeyDownChannel: IPSKeyChannel;
begin
  Result := fPSKeyDownChannel;
end;

function TDesignComponentMemo.PSFocusChannel: IPSFocusChannel;
begin
  Result := fPSFocusChannel;
end;

function TDesignComponentMemo.GetText: String;
begin
  Result := fText;
end;

procedure TDesignComponentMemo.DoStartingValues;
begin
  inherited DoStartingValues;
  SelfProps.SetBool(cProps.Flat, True);
end;

procedure TDesignComponentMemo.InitValues;
begin
  inherited InitValues;
  fPSTextChannel := PubSub.Factory.NewDataChannel<String>;
  fPSTextChannel.Subscribe(PSTextChannelObserver);
  fPSKeyDownChannel := PubSub.Factory.NewDataChannel<TKeyData>;
  fPSFocusChannel := PubSub.Factory.NewDataChannel<TFocusData>;
  fPSFocusChannel.Subscribe(PSFocusChannelObserver);
end;

function TDesignComponentMemo.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetStr(cProps.Text, Text)
    .SetBool(cProps.Focused, fIsFocused)
    .SetBool(cProps.Flat, SelfProps.AsBool(cProps.Flat))
    .SetIntf(cEdit.PSTextChannel, PSTextChannel)
    .SetIntf(cEdit.PSKeyDownChannel, PSKeyDownChannel)
    .SetIntf(cEdit.PSFocusChannel, PSFocusChannel)
end;

function TDesignComponentMemo.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IMemoBit, NewComposeProps);
end;

procedure TDesignComponentMemo.SetText(AText: String);
begin
  if AText <> fText then begin
    fText := AText;
    fPSTextChannel.Publish(fText);
  end;
end;

{ TDesignComponentGrid }

function TDesignComponentGrid.PSGridCmdMoveChannel: IPSGridCmdMoveChannel;
begin
  Result := fPSGridCmdMoveChannel;
end;

function TDesignComponentGrid.PSGridCmdInfoChannel: IPSGridCmdInfoChannel;
begin
  Result := fPSGridCmdInfoChannel;
end;

function TDesignComponentGrid.PSGridCmdRowChannel: IPSGridCmdRowChannel;
begin
  Result := fPSGridCmdRowChannel;
end;

function TDesignComponentGrid.PSGridCmdFieldChannel: IPSGridCmdFieldChannel;
begin
  Result := fPSGridCmdFieldChannel;
end;

procedure TDesignComponentGrid.PSGridRecordChannelObserver(
  const AData: TGridRecord);
begin
  if AData.Data.HasValue then begin
    ChangeRowData(AData.Pos + fSourceRow, AData.Data.Value);
    SentGUIRender;
  end else begin
    EraseRowData(AData.Pos + fSourceRow);
    if AData.Pos < 0 then begin
      if fCurrentRow > 0 then begin
        Dec(fCurrentRow);
        fPSGridMoverChannel.Debounce(TGridMover.New);
      end;
    end;
  end;
end;

function TDesignComponentGrid.PSGridRecordChannel: IPSGridRecordChannel;
begin
  Result := fPSGridRecordChannel;
end;

procedure TDesignComponentGrid.PSGridMoverChannelObserver(const AData: TGridMover);
var
  mShiftInfo: TShiftInfo;
begin
  if AData.Delta.HasValue then begin
    if fSourceRow = fCurrentRow then begin
      if AData.Delta.Value < 0 then begin
        mShiftInfo := TShiftInfo.Create(0, RowCount - 1 + AData.Delta.Value, -AData.Delta.Value);
        ShiftRows(mShiftInfo);
        SentRows(0, -AData.Delta.Value - 1);
      end
      else if AData.Delta.Value > 0 then begin
        mShiftInfo := TShiftInfo.Create(AData.Delta.Value, RowCount - 1, -AData.Delta.Value);
        ShiftRows(mShiftInfo);
        SentRows(RowCount - AData.Delta.Value, RowCount - 1);
      end;
    end else begin
      fSourceRow := fSourceRow + AData.Delta.Value;
      fCurrentRow := fSourceRow;
      SentEditChange;
    end;
    SentGUIRender;
  end else begin
    fSourceRow := fCurrentRow;
    SentRows(0, RowCount - 1);
  end;
end;

function TDesignComponentGrid.PSGridMoverChannel: IPSGridMoverChannel;
begin
  Result := fPSGridMoverChannel;
end;

procedure TDesignComponentGrid.PSLayoutChannelObserver(const AValue: TLayoutData);
var
  mNewRowCount: Integer;
begin
  if SelfProps.AsInt(cProps.Place) in [cPlace.FixFront, cPlace.FixMiddle, cPlace.FixBack] then
    Exit;
  mNewRowCount := (AValue.Height - LaticeRowSize) div (SelfProps.AsInt(cGrid.RowMMHeight) + LaticeRowSize);
  if mNewRowCount <= 0 then
    Exit;
  if RowCount <> mNewRowCount then begin
    RowCount := mNewRowCount;
    fSourceRow := fCurrentRow;
    SentRows(0, RowCount - 1);
    SentEditChange;
  end;
end;

function TDesignComponentGrid.PSSecretChannel: IPSSecretChannel;
begin
  Result := fPSSecretChannel;
end;

procedure TDesignComponentGrid.PSSecretChannelObserver(const AValue: TSecretData);
begin
  fColsHideText[AValue.Info] := not AValue.Visible;
  if fCurrentCol = AValue.Info then
    fEdit.PSSecretChannel.Publish(TSecretData.Create(not fColsHideText[fCurrentCol]));
end;

procedure TDesignComponentGrid.PSTextChannelObserver(const AValue: String);
begin
  if fData[fCurrentRow, fCurrentCol] <> AValue then begin
    fData[fCurrentRow, fCurrentCol] := AValue;
    PSGridCmdFieldChannel.Publish(TGridCmdField.Create(fCurrentCol, AValue));
  end;
end;

procedure TDesignComponentGrid.PSKeyDownChannelObserver(const AValue: TKeyData);
begin
  case AValue.ControlKey of
    ckTab: if AValue.Shift then MoveHorizontally(-1) else MoveHorizontally(1);
    ckUp: MoveVertically(-1);
    ckDown: MoveVertically(1);
    ckPgUp: MoveVertically(-fRowCount);
    ckPgDown: MoveVertically(fRowCount);
    ckInsert: if AValue.Ctrl then InsertRecord;
    ckDelete: if AValue.Ctrl then DeleteRecord;
  end;
end;

procedure TDesignComponentGrid.PSFocusChannelObserver(const AValue: TFocusData);
begin
  if fEditFocused <> AValue.Focused then begin
    fEditFocused := AValue.Focused;
    if fEditFocused and (ColCount > 1) and (fCurrentCol = ColCount - 1) and (AValue.Source <> Self) then
      MoveHorizontally(-MaxInt);
  end;
end;

procedure TDesignComponentGrid.MoveVertically(ADelta: Integer);
var
  mNewPos: Integer;
begin
  mNewPos := fCurrentRow + ADelta;
  if mNewPos < 0 then begin
    fPSGridCmdMoveChannel.Publish(TGridCmdMove.Create(ADelta));
  end else if mNewPos > RowCount - 1 then begin
    fPSGridCmdMoveChannel.Publish(TGridCmdMove.Create(ADelta));
  end else begin
    fCurrentRow := mNewPos;
    SentEditChange;
    fPSGridCmdMoveChannel.Publish(TGridCmdMove.Create(ADelta));
  end;
end;

procedure TDesignComponentGrid.MoveHorizontally(ADelta: Integer);
var
  mNewPos: Integer;
begin
  mNewPos := fCurrentCol + ADelta;
  if mNewPos < 0 then
    mNewPos := 0
  else if mNewPos > ColCount - 1 then
    mNewPos := ColCount - 1;
  if (ADelta < 0) and fEditFocused then
    fEdit.PSFocusChannel.Publish(TFocusData.Create(Self, True))
  else if (ADelta > 0) and fEditFocused and (fCurrentCol < ColCount - 1) then
    fEdit.PSFocusChannel.Publish(TFocusData.Create(Self, True));
  fCurrentCol := mNewPos;
  SentEditChange;
  SentGUIRender;
end;

procedure TDesignComponentGrid.CopyRow(AFrom, ATo: Integer);
var
  i: integer;
begin
  for i := 0 to ColCount - 1 do
    fData[ATo, i] := fData[AFrom, i];
  if ATo = fCurrentRow then
    SentEditChange;
end;

procedure TDesignComponentGrid.ShiftRows(AInfo: TShiftInfo);
var
  i: integer;
begin
  if AInfo.Distance < 0 then begin
    for i := AInfo.First to AInfo.Last do
      CopyRow(i, i + AInfo.Distance);
  end else if AInfo.Distance > 0 then begin
    for i := AInfo.Last downto AInfo.First do
      CopyRow(i, i + AInfo.Distance);
  end;
end;

procedure TDesignComponentGrid.ChangeRowData(ARow: Integer; AData: TArray<String>);
var
  i: Integer;
begin
  Assert(Length(AData) = ColCount);
  for i := 0 to ColCount - 1 do begin
    if fData[ARow, i] <> AData[i] then begin
      fData[ARow, i] := AData[i];
      if (ARow = fCurrentRow) and (i = fCurrentCol) then
        SentEditChange;
    end;
  end;

end;

procedure TDesignComponentGrid.EraseRowData(ARow: Integer);
var
  i: Integer;
begin
  for i := 0 to ColCount - 1 do begin
    if fData[ARow, i] <> '---' then begin
      fData[ARow, i] := '---';
      if (ARow = fCurrentRow) and (i = fCurrentCol) then
        SentEditChange;
    end;
  end;
end;

procedure TDesignComponentGrid.InsertRecord;
begin
  PSGridCmdRowChannel.Publish(TGridCmdRow.Create(fCurrentRow - fSourceRow, cmdNew));
  MoveHorizontally(-MaxInt);
  fEdit.PSFocusChannel.Publish(TFocusData.Create(Self, True));
end;

procedure TDesignComponentGrid.DeleteRecord;
begin
  PSGridCmdRowChannel.Publish(TGridCmdRow.Create(fCurrentRow - fSourceRow, cmdDelete));
end;

procedure TDesignComponentGrid.SentGUIRender;
begin
  PSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

procedure TDesignComponentGrid.SentEditChange;
begin
  fEdit.PSSecretChannel.Publish(TSecretData.Create(not fColsHideText[fCurrentCol]));
  if fEdit.Text <> fData[fCurrentRow, fCurrentCol] then begin
    fEdit.PSTextChannel.Publish(fData[fCurrentRow, fCurrentCol]);
  end;
end;

procedure TDesignComponentGrid.SentRows(AFrom, ATo: Integer);
begin
  fPSGridCmdInfoChannel.Publish(TGridCmdInfo.Create(AFrom - fSourceRow, ATo - fSourceRow));
end;

function TDesignComponentGrid.ColProps(Row, Col: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetStr(cProps.Text, RenderText(Row, Col))
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.TextColor, SelfProps.AsInt(cProps.TextColor));
  if Row mod 2 = 1 then
    mProp := SelfProps.PropByName[cGrid.ColOddColor]
  else
    mProp := SelfProps.PropByName[cGrid.ColEvenColor];
  if mProp <> nil then
    Result.SetInt(cProps.Color, mProp.AsInteger);
end;

function TDesignComponentGrid.RowProps(Row: integer): IProps;
var
  mProp: IProp;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.Layout, cLayout.Horizontal)
    .SetInt(cProps.Border, 0)
    .SetInt(cProps.PlaceSize, SelfProps.AsInt(cGrid.RowMMHeight));
  if (Row = fCurrentRow) then begin
    mProp := SelfProps.PropByName[cGrid.FocusRowColor];
  end
  else if Row mod 2 = 1 then begin
    mProp := SelfProps.PropByName[cGrid.RowOddColor];
  end else begin
    mProp := SelfProps.PropByName[cGrid.RowEvenColor];
  end;
  if mProp = nil then begin
    mProp := SelfProps.PropByName[cGrid.Color];
  end;
  if mProp <> nil then begin
    Result.SetInt(cProps.Color, mProp.AsInteger).SetBool(cProps.Transparent, False);
  end;
end;

function TDesignComponentGrid.MakeRow(Row: integer): TMetaElementArray;
var
  i: integer;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, ColCount);
  for i := 0 to ColCount - 1 do
    if (Row = fCurrentRow) and (i = fCurrentCol) then begin
      Result[i] := ComposeEdit
    end else begin
      Result[i] := ElementFactory.CreateElement(ITextBit, ColProps(Row, i));
    end;
end;

function TDesignComponentGrid.MakeGrid: TMetaElementArray;
var
  i: integer;
begin
  Result := TMetaElementArray.Create;
  SetLength(Result, RowCount);
  for i := 0 to RowCount - 1 do begin
    Result[i] := ElementFactory.CreateElement(IStripBit, RowProps(i), Latice(MakeRow(i), IStripBit, LaticeColProps));
  end;
  Result := Latice(Result, IStripBit, LaticeRowProps);
end;

function TDesignComponentGrid.LaticeColProps: IProps;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetBool(cProps.Transparent, False)
    .SetInt(cProps.Color, LaticeColColor)
    .SetInt(cProps.PlaceSize, LaticeColSize);
end;

function TDesignComponentGrid.LaticeRowProps: IProps;
begin
  Result := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetBool(cProps.Transparent, False)
    .SetInt(cProps.Color, LaticeRowColor)
    .SetInt(cProps.PlaceSize, LaticeRowSize);
end;

function TDesignComponentGrid.Latice(AElements: TMetaElementArray;
  ALaticeEl: TGuid; ALaticeProps: IProps): TMetaElementArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Length(AElements) * 2 + 1);
  Result[0] := ElementFactory.CreateElement(ALaticeEl, ALaticeProps);
  for i := 0 to Length(AElements) - 1 do begin
    Result[i * 2 + 1] := AElements[i];
    Result[i * 2 + 2] := ElementFactory.CreateElement(ALaticeEl, ALaticeProps);
  end;
end;

function TDesignComponentGrid.ComposeEdit: IMetaElement;
var
  mEdit: IDesignComponent;
begin
  mEdit := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cEdit.Border, SelfProps.AsInt(cGrid.EditBorder))
    .SetInt(cEdit.BorderColor, SelfProps.AsInt(cGrid.EditBorderColor))
  );
  (mEdit as INode).AddChild(fEdit as INode);
  Result := mEdit.Compose;
end;

function TDesignComponentGrid.RenderText(Row, Col: integer): String;
begin
  Result := fData[Row, Col];
  if (Result <> '---') and fColsHideText[Col] then
    Result := '< secret >';
end;

procedure TDesignComponentGrid.InitValues;
var
  mProp: IProp;
  mEdColor: TColor;
begin
  inherited InitValues;
  mProp := SelfProps.PropByName[cGrid.FocusRowColor];
  if mProp = nil then
    mProp := SelfProps.PropByName[cGrid.Color];
  if mProp = nil then
    mEdColor := clFuchsia
  else
    mEdColor:= mProp.AsInteger;
  fEdit := Factory2.Locate<IDesignComponentEdit>(
    NewProps
    .SetBool(cEdit.Flat, True)
    .SetInt(cEdit.Color, mEdColor)
  );
  fEdit.PSTextChannel.Subscribe(PSTextChannelObserver);
  fEdit.PSKeyDownChannel.Subscribe(PSKeyDownChannelObserver);
  fEdit.PSFocusChannel.Subscribe(PSFocusChannelObserver);
  fPSGridCmdMoveChannel := PubSub.Factory.NewDataChannel<TGridCmdMove>;
  fPSGridCmdInfoChannel := PubSub.Factory.NewDataChannel<TGridCmdInfo>;
  fPSGridCmdRowChannel := PubSub.Factory.NewDataChannel<TGridCmdRow>;
  fPSGridCmdFieldChannel := PubSub.Factory.NewDataChannel<TGridCmdField>;
  fPSGridRecordChannel := PubSub.Factory.NewDataChannel<TGridRecord>;
  fPSGridRecordChannel.Subscribe(PSGridRecordChannelObserver);
  fPSGridMoverChannel := PubSub.Factory.NewDataChannel<TGridMover>;
  fPSGridMoverChannel.Subscribe(PSGridMoverChannelObserver);
  fPSLayoutChannel := PubSub.Factory.NewDataChannel<TLayoutData>;
  fPSLayoutChannel.Subscribe(PSLayoutChannelObserver);
  fPSSecretChannel := PubSub.Factory.NewDataChannel<TSecretData>;
  fPSSecretChannel.Subscribe(PSSecretChannelObserver);
end;

function TDesignComponentGrid.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
   .SetInt(cProps.Place, cPlace.Elastic)
   .SetInt(cProps.Layout, cLayout.Vertical)
   .SetIntf(cGrid.PSLayoutChannel, fPSLayoutChannel);
end;

function TDesignComponentGrid.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(
    IStripBit, NewComposeProps, MakeGrid);
end;

procedure TDesignComponentGrid.SetRowCount(AValue: Integer);
begin
  if fRowCount = AValue then Exit;
  fRowCount := AValue;
  SetLength(fData, RowCount, ColCount);
end;

procedure TDesignComponentGrid.SetColCount(AValue: Integer);
begin
  if fColCount = AValue then Exit;
  fColCount := AValue;
  SetLength(fData, RowCount, ColCount);
  SetLength(fColsHideText, ColCount);
end;

{ TDesignComponentForm }

function TDesignComponentForm.PSCloseChannel: IPSCloseChannel;
begin
  Result := fPSCloseChannel;
end;

procedure TDesignComponentForm.PSSizeChannelObserver(const AData: TSizeData);
begin
  fMMWidth := AData.Width;
  fMMHeight := AData.Height;
  if PSGUIChannel <> nil then
    fPSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

function TDesignComponentForm.PSSizeChannel: IPSSizeChannel;
begin
  Result := fPSSizeChannel;
end;

procedure TDesignComponentForm.PSPositionChannelObserver(
  const AData: TPositionData);
begin
  fMMLeft := AData.Left;
  fMMTop := AData.Top;
end;

function TDesignComponentForm.PSPositionChannel: IPSPositionChannel;
begin
  Result := fPSPositionChannel;
end;

function TDesignComponentForm.PSActivateChannel: IPSActivateChannel;
begin
  Result := fPSActivateChannel;
end;

function TDesignComponentForm.PSKeyDownChannel: IPSKeyChannel;
begin
  Result := fPSKeyDownChannel;
end;

procedure TDesignComponentForm.DoStartingValues;
begin
  inherited DoStartingValues;
  // when lcl work with WM_SIZE message, flag wcfBoundsRealized must be set, otherways message didn't update forms size
  // flag wcfBoundsRealized is set in TWinControl.RealizeBounds but only in case sizes differs
  MMLeft := 0;
  MMTop := 0;
  MMWidth := 200;
  MMHeight := 200;
end;

procedure TDesignComponentForm.InitValues;
begin
  inherited InitValues;
  fPSSizeChannel := PubSub.Factory.NewDataChannel<TSizeData>;
  fPSSizeChannel.Subscribe(PSSizeChannelObserver);
  fPSPositionChannel := PubSub.Factory.NewDataChannel<TPositionData>;
  fPSPositionChannel.Subscribe(PSPositionChannelObserver);
  fPSCloseChannel := PubSub.Factory.NewChannel;
  fPSActivateChannel := PubSub.Factory.NewChannel;
  fPSKeyDownChannel := PubSub.Factory.NewDataChannel<TKeyData>;
end;

function TDesignComponentForm.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
  .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
  .SetIntf(cForm.PSCloseChannel, PSCloseChannel)
  .SetIntf(cForm.PSSizeChannel, PSSizeChannel)
  .SetIntf(cForm.PSPositionChannel, PSPositionChannel)
  .SetIntf(cForm.PSActivateChannel, PSActivateChannel)
  .SetIntf(cForm.PSKeyDownChannel, PSKeyDownChannel)
end;

function TDesignComponentForm.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IFormBit, NewComposeProps);
  ComposeChildren(Result);
end;

{ TDesignComponentPager }

procedure TDesignComponentPager.PSPagerChannelObserver(const AIndex: Integer);
begin
  if fIndex <> AIndex then begin
    fIndex := AIndex;
    PSGUIChannel.Debounce(TGUIData.Create(gaRender));
  end;
end;

function TDesignComponentPager.RenderPage(const APageElement: IMetaElement): IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit, [APageElement]);
end;

function TDesignComponentPager.MakeSwitch: IMetaElement;
var
  i: integer;
  mSwitch: TMetaElementArray;
  mProps: IProps;
begin
  fSwitch := NewSwitch;
  SetLength(mSwitch, Count);
  for i := 0 to Count - 1 do
  begin
    mSwitch[i] := fSwitch[i].Compose;
  end;
  mProps := NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.PlaceSize, SwitchSize);
  case SwitchEdge of
    cEdge.Left, cEdge.Right:
      mProps.SetInt(cProps.Layout, cLayout.Vertical);
    cEdge.Top, cEdge.Bottom:
      mProps.SetInt(cProps.Layout, cLayout.Horizontal);
  end;
  Result := ElementFactory.CreateElement(IStripBit, mProps, mSwitch);
end;

function TDesignComponentPager.MakeBody: IMetaElement;
var
  mActual: IMetaElement;
begin
  mActual := (GetChild(fIndex) as IDesignComponent).Compose;
  Result := ElementFactory.CreateElement(
    IStripBit,
    NewProps
      .SetInt(cProps.Layout, cLayout.Overlay),
    [mActual]);
end;

function TDesignComponentPager.NewSwitch: TArray<IDesignComponent>;
var
  i: Integer;
  mText: String;
  mButton: IDesignComponentButton;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    mText := (GetChild(i) as TDynaObject).SelfProps.AsStr(cProps.Caption);
    mButton := Factory2.Locate<IDesignComponentButton>(NewProps
      .SetInt(cButton.Color, SelfProps.AsInt(cButton.Color))
      .SetBool(cButton.Transparent, SelfProps.AsBool(cButton.Transparent))
      .SetStr(cButton.Text, mText)
    );
    PubSub.Factory.NewNonDataToDataBridge<Integer>(
      mButton.PSClickChannel,
      fPSPagerChannel,
      NewSwitchFunc(i)
    );
    Result[i] := mButton;
  end;
end;

function TDesignComponentPager.NewSwitchFunc(AIndex: Integer): TPubSubNewData<
  Integer>;
begin
  Result := function (): Integer
  begin
    Result := AIndex;
  end
end;

procedure TDesignComponentPager.InitValues;
begin
  inherited InitValues;
  fPSPagerChannel :=  PubSub.Factory.NewDataChannel<Integer>;
  fPSPagerChannel.Subscribe(PSPagerChannelObserver);
end;

function TDesignComponentPager.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  case SwitchEdge of
    cEdge.Left, cEdge.Right:
      Result.SetInt(cProps.Layout, cLayout.Horizontal);
    cEdge.Top, cEdge.Bottom:
      Result.SetInt(cProps.Layout, cLayout.Vertical);
  end;
end;

function TDesignComponentPager.DoCompose: IMetaElement;
var
  mChildren: TMetaElementArray;
begin
  case SwitchEdge of
    cEdge.Left, cEdge.Top:
      mChildren := [MakeSwitch, MakeBody];
    cEdge.Right, cEdge.Bottom:
      mChildren := [MakeBody, MakeSwitch];
  end;
  Result := ElementFactory.CreateElement(IStripBit, NewComposeProps, mChildren);
end;

procedure TDesignComponentPager.BeforeDestruction;
begin
  fPSPagerChannel.Unsubscribe(PSPagerChannelObserver);
  inherited BeforeDestruction;
end;

{ TDesignComponentStrip }

function TDesignComponentStrip.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IStripBit,
    NewComposeProps
      .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
      .SetStr(cProps.Caption, SelfProps.AsStr(cProps.Caption))
      .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color)));
  ComposeChildren(Result);
end;

{ TDesignComponentButton }

function TDesignComponentButton.PSClickChannel: IPSClickChannel;
begin
  Result := fPSClickChannel;
end;

function TDesignComponentButton.NewOuterProps: IProps;
begin
  Result := SelfProps.Clone([cProps.Place, cProps.PlaceSize])
    .SetInt(cProps.Border, 4)
    .SetInt(cProps.BorderColor, clGray);
end;

procedure TDesignComponentButton.InitValues;
begin
  inherited InitValues;
  fPSClickChannel := PubSub.Factory.NewChannel;
end;

procedure TDesignComponentButton.DoStartingValues;
begin
  inherited DoStartingValues;
  SelfProps.SetInt(cButton.FontDirection, cFontDirection.Horizontal);
  SelfProps.SetInt(cButton.FocusColor, clYellow);
end;

function TDesignComponentButton.NewComposeProps: IProps;
begin
  Result := inherited NewComposeProps;
  Result
    .SetInt(cButton.Color, SelfProps.AsInt(cButton.Color))
    .SetInt(cButton.Place, cPlace.Elastic)
    .SetInt(cButton.FontDirection, SelfProps.AsInt(cButton.FontDirection))
    .SetInt(cButton.FocusColor, SelfProps.AsInt(cButton.FocusColor))
    .SetStr(cButton.Text, SelfProps.AsStr(cButton.Text))
    .SetIntf(cButton.PSClickChannel, PSClickChannel);
end;

function TDesignComponentButton.DoCompose: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IButtonBit, NewComposeProps);
end;

procedure TDesignComponentButton.BeforeDestruction;
begin
  PubSub.Factory.DropChannel(fPSClickChannel);
  inherited BeforeDestruction;
end;

{ TDesignComponent }

function TDesignComponent.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

function TDesignComponent.NewComposeProps: IProps;
begin
  Result := SelfProps.Clone([cProps.Layout, cProps.Place, cProps.PlaceSize,
    cProps.Title, cProps.Border,
    cProps.Color, cProps.FontColor, cProps.TextColor, cProps.BorderColor]);
end;

procedure TDesignComponent.AddChildren(const AElement: IMetaElement;
  const AChildren: TMetaElementArray);
var
  mEl: IMetaElement;
begin
  for mEl in AChildren do
    (AElement as INode).AddChild(mEl as INode);
end;

procedure TDesignComponent.ComposeChildren(const AParentEl: IMetaElement);
var
  i: Integer;
  mEl: IMetaElement;
begin
  for i := 0 to Count - 1 do begin
    mEl := (GetChild(i) as IDesignComponent).Compose;
    (AParentEl as INode).AddChild(mEl as INode);
  end;
end;

procedure TDesignComponent.DoStartingValues;
begin
  SelfProps
  .SetInt(cProps.Layout, cLayout.Horizontal)
  .SetInt(cProps.Place, cPlace.Elastic)
  .SetInt(cProps.Border, 0)
  .SetStr(cProps.Text, '')
  .SetBool(cProps.Transparent, True)
  .SetInt(cProps.Color, clWhite)
  .SetInt(cProps.FontColor, clBlack)
  .SetInt(cProps.BorderColor, clBlack);
end;

function TDesignComponent.Compose: IMetaElement;
begin
  Result := DoCompose;
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

