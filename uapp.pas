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
unit uapp;

{$mode delphi}{$H+}{$M+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  graphics, tal_uapp, typinfo,
  rea_idesigncomponent, rea_udesigncomponent, rea_ilayout,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher,
  trl_urttibroker, trl_irttibroker,
  trl_ipersist,
  trl_upersistxml,
  rea_idata, sysutils,
  trl_usystem, trl_upersist,
  trl_udifactory,
  tvl_itimer;

type
  { TApp }

  TApp = class(TALApp)
  private
    procedure RegisterPersist;
  protected
    procedure RegisterAppServices; override;
  end;

  { TAppSettings }

  TAppSettings = class
  private
    fID: String;
    fTop: Integer;
    fLeft: Integer;
    fWidth: Integer;
    fHeight: Integer;
  published
    [PersistIDAttribute, PersistAUTOAttribute]
    property ID: String read fID write fID;
    property Top: Integer read fTop write fTop;
    property Left: Integer read fLeft write fLeft;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
  end;

  { TTag }

  TTag = class
  private
    fVal: String;
  published
    property Val: String read fVal write fVal;
  end;

  IListTags = interface(IMiniList<TTag>)
  ['{4D154971-854E-44C0-844A-B5B86A038B1E}']
  end;

  TListTags = class(TMiniList<TTag>, IListTags)
  end;

  { TPerson }

  TPerson = class
  private
    fID: String;
    fName: String;
    fSurename: String;
    fTags: IListTags;
  published
    [PersistIDAttribute, PersistAUTOAttribute]
    property ID: String read fID write fID;
    property Name: String read fName write fName;
    property Surename: String read fSurename write fSurename;
    property Tags: IListTags read fTags write fTags;
  end;

  IDataListPersons = interface(IMiniDataList<TPerson>)
  ['{55465231-6ED6-41A0-B0B3-AE76D5E060F4}']
  end;

  TDataListTPersons = class(TMiniDataList<TPerson>, IDataListPersons)
  end;

  IPSFilterChannel = IPubSubDataChannel<String>;

  IGUIFilter = interface(IDesignComponent)
  ['{C3211214-1580-4F7C-95C2-634E8E7C3689}']
    function PSFilterChannel: IPSFilterChannel;
    function GetFilterEdit: IDesignComponentEdit;
    property FilterEdit: IDesignComponentEdit read GetFilterEdit;
  end;

  { TGUIFilter }

  TGUIFilter = class(TDesignComponent, IGUIFilter)
  strict private
    fFilterText, fLastTriggeredText: String;
    fFilterEdit: IDesignComponentEdit;
    fTimer: ITimer;
    fPSFilterChannel: IPSFilterChannel;
    procedure TimerObserver;
    procedure FilterEditTextChannelObserver(const AText: String);
    function PSFilterChannel: IPSFilterChannel;
    function GetFilterEdit: IDesignComponentEdit;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
  public
    destructor Destroy; override;
  end;

  { IGUIPersons }

  IGUIPersons = interface(IDesignComponent)
  ['{15239075-7E14-4C90-B824-8C1F7E93131A}']
    function GetNameEdit: IDesignComponentEdit;
    function GetSurenameEdit: IDesignComponentEdit;
    function GetGrid: IDesignComponentGrid;
    function GetIDText: IDesignComponentText;
    function GetTags: IDesignComponentGrid;
    property NameEdit: IDesignComponentEdit read GetNameEdit;
    property SurenameEdit: IDesignComponentEdit read GetSurenameEdit;
    property Grid: IDesignComponentGrid read GetGrid;
    property IDText: IDesignComponentText read GetIDText;
    property Tags: IDesignComponentGrid read GetTags;
  end;

  { TGUIPersons }

  TGUIPersons = class(TDesignComponent, IGUIPersons)
  private const
    cRowSize = 30;
  private
    fNameEdit: IDesignComponentEdit;
    fSurenameEdit: IDesignComponentEdit;
    fGrid: IDesignComponentGrid;
    fIDText: IDesignComponentText;
    fTags: IDesignComponentGrid;
    function ComposeEditRow(const ACaption: String; AEdit: IDesignComponent): IDesignComponent;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
    function GetNameEdit: IDesignComponentEdit;
    function GetSurenameEdit: IDesignComponentEdit;
    function GetGrid: IDesignComponentGrid;
    function GetIDText: IDesignComponentText;
    function GetTags: IDesignComponentGrid;
  protected
    fPSGUIChannel: IPSGUIChannel;
  published
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

  { IGUICommands }

  IGUICommands = interface(IDesignComponent)
  ['{2001CC71-14E1-4BAC-866F-9295FE27DCBA}']
    function GetFirst: IDesignComponentButton;
    function GetLast: IDesignComponentButton;
    function GetNext: IDesignComponentButton;
    function GetPrior: IDesignComponentButton;
    property Next: IDesignComponentButton read GetNext;
    property Prior: IDesignComponentButton read GetPrior;
    property First: IDesignComponentButton read GetFirst;
    property Last: IDesignComponentButton read GetLast;
  end;

  { TGUICommands }

  TGUICommands = class(TDesignComponent, IGUICommands)
  private
    fNext, fPrior, fFirst, fLast: IDesignComponentButton;
    function GetFirst: IDesignComponentButton;
    function GetLast: IDesignComponentButton;
    function GetNext: IDesignComponentButton;
    function GetPrior: IDesignComponentButton;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
  end;

  { TGUI }

  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    fPersons: IGUIPersons;
    fCommands: IGUICommands;
    fFilter: IGUIFilter;
    fForm: IDesignComponentForm;
    fDataConnector: IDataConnector;
    fTagsConnector: IDataConnector;
    fPersonsData: IMiniDataList<TPerson>;
    function NewForm(const ADCs: TArray<IDesignComponent>): IDesignComponentForm;
    function NewPager(const APages: TArray<IDesignComponent>): IDesignComponent;
    function NewLogButton: IDesignComponentButton;
    function NewSaveButton: IDesignComponentButton;
  private
    fAppSettings: IRBData;
  private
    procedure PSSizeObserver(const AValue: TSizeData);
    procedure PSPositionObserver(const AValue: TPositionData);
    procedure PSCloseProgramObserver;
    procedure PSShowLogObserver;
    procedure PSSaveDataObserver;
    procedure PSFilterChannelObserver(const AValue: String);
  private
    function GetAppSettings: IRBData;
    procedure PublishAppSettings;
    procedure CreateComponents;
    procedure CreateDataConnectors;
  protected
    function DoCompose: IMetaElement; override;
    procedure InitValues; override;
  protected
    fStoreDevice: IPersistStoreDevice;
    fPersistFactory: IPersistFactory;
    fPSGUIChannel: IPSGUIChannel;
  published
    property StoreDevice: IPersistStoreDevice read fStoreDevice write fStoreDevice;
    property PersistFactory: IPersistFactory read fPersistFactory write fPersistFactory;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

implementation

{ TGUIFilter }

procedure TGUIFilter.TimerObserver;
begin
  if fLastTriggeredText <> fFilterText then begin
    fLastTriggeredText := fFilterText;
    fPSFilterChannel.Publish(fLastTriggeredText);
  end;
end;

procedure TGUIFilter.FilterEditTextChannelObserver(const AText: String);
begin
  fFilterText := AText;
  fTimer.Restart;
end;

procedure TGUIFilter.InitValues;
begin
  inherited InitValues;
  fPSFilterChannel := PubSub.Factory.NewDataChannel<String>;
  fTimer := Factory2.Locate<ITimer>('filter');
  fTimer.Subscribe(TimerObserver);
  fFilterEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'filter_edit')
    .SetBool(cProps.Flat, True)
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    );
  fFilterEdit.PSTextChannel.Subscribe(FilterEditTextChannelObserver);
  fTimer.Enabled := True;
end;

function TGUIFilter.DoCompose: IMetaElement;
begin
  Result := Morph.WrapUp(fFilterEdit, 50).Compose;
end;

function TGUIFilter.PSFilterChannel: IPSFilterChannel;
begin
  Result := fPSFilterChannel;
end;

function TGUIFilter.GetFilterEdit: IDesignComponentEdit;
begin
  Result := fFilterEdit;
end;

destructor TGUIFilter.Destroy;
begin
  fTimer.Unsubscribe(TimerObserver);
  fFilterEdit.PSTextChannel.Unsubscribe(FilterEditTextChannelObserver);
  inherited Destroy;
end;

{ TGUICommands }

function TGUICommands.GetFirst: IDesignComponentButton;
begin
  Result := fFirst;
end;

function TGUICommands.GetLast: IDesignComponentButton;
begin
  Result := fLast;
end;

function TGUICommands.GetNext: IDesignComponentButton;
begin
  Result := fNext;
end;

function TGUICommands.GetPrior: IDesignComponentButton;
begin
  Result := fPrior;
end;

procedure TGUICommands.InitValues;
begin
  inherited InitValues;
  fNext := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetStr(cProps.ID, 'next')
    .SetStr(cProps.Text, 'next'));
  fPrior := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetStr(cProps.ID, 'prior')
    .SetStr(cProps.Text, 'prior'));
  fFirst := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetStr(cProps.ID, 'first')
    .SetStr(cProps.Text, 'first'));
  fLast := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetStr(cProps.ID, 'last')
    .SetStr(cProps.Text, 'last'));
end;

function TGUICommands.DoCompose: IMetaElement;
var
  mB: IDesignComponent;
begin
  mB := Factory2.Locate<IDesignComponentHBox>(NewProps
    .SetInt(cProps.Layout, cLayout.Horizontal)
    .SetInt(cProps.BoxLaticeSize, 5)
    .SetBool(cProps.Transparent, True));
  (mB as INode).AddChild(fFirst as INode);
  (mB as INode).AddChild(fPrior as INode);
  (mB as INode).AddChild(fNext as INode);
  (mB as INode).AddChild(fLast as INode);
  Result := mB.Compose;
end;

{ TGUIPersons }

function TGUIPersons.ComposeEditRow(const ACaption: String;
  AEdit: IDesignComponent): IDesignComponent;
begin
  Result := Morph.WrapInStrip(
    Morph.StickLabel(AEdit, ACaption, cEdge.Left, 100),
    cRowSize, cPlace.FixFront
  );
end;

procedure TGUIPersons.InitValues;
begin
  inherited InitValues;
  fNameEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'person_name')
    .SetBool(cProps.Flat, True)
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    );
  fSurenameEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'person_surename')
    .SetBool(cProps.Flat, True)
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    );
  fGrid := Factory2.Locate<IDesignComponentGrid>(NewComposeProps
    .SetStr(cProps.ID, 'person_grid')
    .SetIntf('PSGUIChannel', PSGUIChannel)
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetInt(cGrid.RowCount, 5)
    .SetInt(cGrid.ColCount, 2)
    .SetInt(cGrid.RowMMHeight, 25)
    .SetInt(cGrid.LaticeColColor, clBlack)
    .SetInt(cGrid.LaticeRowColor, clBlack)
    .SetInt(cGrid.LaticeColSize, 2)
    .SetInt(cGrid.LaticeRowSize, 2)
    );
  fIDText := Factory2.Locate<IDesignComponentText>(NewComposeProps
    .SetStr(cProps.ID, 'person_id')
    .SetBool(cProps.Flat, True)
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
  );
  fTags := Factory2.Locate<IDesignComponentGrid>(NewComposeProps
    .SetStr(cProps.ID, 'tags_grid')
    .SetIntf('PSGUIChannel', PSGUIChannel)
    .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
    .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
    .SetInt(cGrid.RowCount, 5)
    .SetInt(cGrid.ColCount, 1)
    .SetInt(cGrid.RowMMHeight, 25)
    .SetInt(cGrid.LaticeColColor, clBlack)
    .SetInt(cGrid.LaticeRowColor, clBlack)
    .SetInt(cGrid.LaticeColSize, 1)
    .SetInt(cGrid.LaticeRowSize, 1)
    );
end;

function TGUIPersons.DoCompose: IMetaElement;
var
  mBEdit, mBGridEdit, mBMain: IDesignComponent;
begin
  mBEdit := Factory2.Locate<IDesignComponentVBox>(NewComposeProps
    .SetBool(cProps.Transparent, True)
  );
  (mBEdit as INode).AddChild(ComposeEditRow('Name', fNameEdit) as INode);
  (mBEdit as INode).AddChild(ComposeEditRow('Surename', fSurenameEdit) as INode);
  (mBEdit as INode).AddChild(ComposeEditRow('ID', fIDText) as INode);
  (mBEdit as INode).AddChild(Morph.WrapInStrip(Morph.StickLabel(fTags, 'Tags', cEdge.Left, 100), cRowSize * 5 , cPlace.Elastic) as INode);

  mBGridEdit := Factory2.Locate<IDesignComponentHBox>(NewComposeProps
    .SetInt(cProps.BoxLaticeSize, 10)
    .SetBool(cProps.Transparent, True)
  );
  (mBGridEdit as INode).AddChild(fGrid as INode);
  (mBGridEdit as INode).AddChild(mBEdit as INode);
  mBMain := Factory2.Locate<IDesignComponentVBox>(NewComposeProps
    .SetInt(cProps.BoxLaticeSize, 5)
    .SetBool(cProps.Transparent, True)
  );
  (mBMain as INode).AddChild(mBGridEdit as INode);
  Result := mBMain.Compose;
end;

function TGUIPersons.GetNameEdit: IDesignComponentEdit;
begin
  Result := fNameEdit;
end;

function TGUIPersons.GetSurenameEdit: IDesignComponentEdit;
begin
  Result := fSurenameEdit;
end;

function TGUIPersons.GetGrid: IDesignComponentGrid;
begin
  Result := fGrid;
end;

function TGUIPersons.GetIDText: IDesignComponentText;
begin
  Result := fIDText;
end;

function TGUIPersons.GetTags: IDesignComponentGrid;
begin
  Result := fTags;
end;

{ TGUI }

function TGUI.NewForm(const ADCs: TArray<IDesignComponent>): IDesignComponentForm;
var
  mDC: IDesignComponent;
begin
  Result := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.ID, 'mainform')
    .SetIntf('PSGUIChannel', fPSGUIChannel)
    .SetStr(cProps.Caption, 'Demo app')
    .SetInt(cProps.Color, clLtGray)
    );
  Result.PSSizeChannel.Subscribe(PSSizeObserver);
  Result.PSPositionChannel.Subscribe(PSPositionObserver);
  Result.PSCloseChannel.Subscribe(PSCloseProgramObserver);
  for mDC in ADCs do begin
    (Result as INode).AddChild(mDC as INode);
  end;
end;

function TGUI.NewPager(const APages: TArray<IDesignComponent>): IDesignComponent;
var
  mPage: IDesignComponent;
begin
  Result := Factory2.Locate<IDesignComponentPager>(NewProps
   .SetIntf('PSGUIChannel', fPSGUIChannel)
   .SetInt(cPager.SwitchEdge, cEdge.Top)
   .SetInt(cPager.SwitchSize, 25)
   .SetInt(cProps.Color, clLtGray)
  );
  for mPage in APages do begin
    (Result as INode).AddChild(mPage as INode);
  end;
end;

function TGUI.NewLogButton: IDesignComponentButton;
begin
  Result := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.PlaceSize, 20)
    .SetStr(cProps.Text, 'SHOW LOG')
  );
  Result.PSClickChannel.Subscribe(PSShowLogObserver);
end;

function TGUI.NewSaveButton: IDesignComponentButton;
begin
  Result := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.PlaceSize, 20)
    .SetStr(cProps.Text, 'SAVE')
  );
  Result.PSClickChannel.Subscribe(PSSaveDataObserver);
end;

procedure TGUI.CreateComponents;
var
  mPager: IDesignComponent;
begin
  fFilter := Factory2.Locate<IGUIFilter>(NewProps
   .SetInt(cProps.Color, clLime)
  );
  fFilter.PSFilterChannel.Subscribe(PSFilterChannelObserver);
  fPersons := Factory2.Locate<IGUIPersons>(NewProps
   .SetIntf('PSGUIChannel', fPSGUIChannel)
   .SetInt(cProps.Color, clCream)
  );
  fCommands := Factory2.Locate<IGUICommands>(NewProps
   .SetInt(cProps.Color, clLtGray)
   .SetBool(cProps.Transparent, False)
  );
  mPager := NewPager([
    Morph.NewPage('Persons', cLayout.Vertical, [fFilter, fPersons, Morph.WrapInStrip(fCommands, 25, cPlace.FixBack)]),
    Morph.NewPage('Test', cLayout.Vertical, [NewLogButton, NewSaveButton])
  ]);
  fForm := NewForm([mPager]);
end;

procedure TGUI.CreateDataConnectors;
begin
  fDataConnector := Factory2.Locate<IDataConnector>('TStoreConnector');
  fDataConnector.RegisterEdit('Name', fPersons.NameEdit);
  fDataConnector.RegisterEdit('Surename', fPersons.SurenameEdit);
  fDataConnector.RegisterGrid(TArray<String>.Create('Name', 'Surename'), fPersons.Grid, TPerson);
  fDataConnector.RegisterText('ID', fPersons.IDText);

  fTagsConnector := Factory2.Locate<IDataConnector>('TStoreConnector');
  fTagsConnector.RegisterGrid(TArray<String>.Create('Val'), fPersons.Tags, TTag);
  fDataConnector.RegisterConnector('Tags', fTagsConnector);

  fDataConnector.RegisterCommand(fCommands.First.PSClickChannel, TCommand.CreateFirst);
  fDataConnector.RegisterCommand(fCommands.Last.PSClickChannel, TCommand.CreateLast);
  fDataConnector.RegisterCommand(fCommands.Next.PSClickChannel, TCommand.CreateNext);
  fDataConnector.RegisterCommand(fCommands.Prior.PSClickChannel, TCommand.CreatePrior);
end;

procedure TGUI.PSSizeObserver(const AValue: TSizeData);
begin
  fAppSettings.ItemByName['Width'].AsInteger := AValue.Width;
  fAppSettings.ItemByName['Height'].AsInteger := AValue.Height;
end;

procedure TGUI.PSPositionObserver(const AValue: TPositionData);
begin
  fAppSettings.ItemByName['Left'].AsInteger := AValue.Left;
  fAppSettings.ItemByName['Top'].AsInteger := AValue.Top;
end;

procedure TGUI.PSCloseProgramObserver;
begin
  fPersonsData.Save;
  StoreDevice.Save2(fAppSettings);
  StoreDevice.Close;
  raise ELaunchStop.Create('');
end;

procedure TGUI.PSShowLogObserver;
begin
  Log.Visible := not Log.Visible;
end;

procedure TGUI.PSSaveDataObserver;
begin
  fPersonsData.Save;
  StoreDevice.Save2(fAppSettings);
  StoreDevice.Flush;
end;

function FilterData(const ALowerText: String; const x: IRBData): Boolean;
var
  i: Integer;
  mList: IMiniList;
  mData: IRBData;
begin
  Result := False;
  for i := 0 to x.Count - 1 do begin
    if x.Items[i].IsObject then begin
      Result := FilterData(ALowerText, TRBData.Create(x.Items[i].AsObject));
    end
    else if x.Items[i].IsInterface then begin
      if Supports(x.Items[i].AsInterface, IMiniList, mList) then begin
        for mData in mList do begin
          Result := FilterData(ALowerText, mData);
          if Result then
            Break;
        end;
      end;
    end
    else begin
      Result := Pos(ALowerText, x.Items[i].AsString.ToLower) > 0;
    end;
    if Result then
      Break;
  end;
end;

procedure TGUI.PSFilterChannelObserver(const AValue: String);
begin
  if AValue = '' then
    fDataConnector.PSListChangeChannel.Publish(TListChange.New(fPersonsData.NewList))
  else
    fDataConnector.PSListChangeChannel.Publish(TListChange.New(
      fPersonsData.NewList(
        function(const x: IRBData): Boolean
        begin
          Result := FilterData(AValue.ToLower, x);
        end
      )
    ));
  PSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

function TGUI.GetAppSettings: IRBData;
var
  mEnum: IRBDataEnumerator;
begin
  mEnum := StoreDevice.Select2(TAppSettings.ClassName).GetEnumerator;
  if mEnum.MoveNext then
    Result := mEnum.Current
  else begin
    Result := PersistFactory.Create(IRBData, TAppSettings.ClassName) as IRBData;
    Result.ItemByName['Width'].AsInteger := 600;
    Result.ItemByName['Height'].AsInteger := 200;
    Result.ItemByName['Left'].AsInteger := 300;
    Result.ItemByName['Top'].AsInteger := 400;
  end;
end;

procedure TGUI.PublishAppSettings;
begin
  fForm.PSSizeChannel.Debounce(TSizeData.Create(Self, fAppSettings.ItemByName['Width'].AsInteger, fAppSettings.ItemByName['Height'].AsInteger));
  fForm.PSPositionChannel.Debounce(TPositionData.Create(Self, fAppSettings.ItemByName['Left'].AsInteger, fAppSettings.ItemByName['Top'].AsInteger));
end;

function TGUI.DoCompose: IMetaElement;
begin
  Result := fForm.Compose;
end;

procedure TGUI.InitValues;
begin
  inherited InitValues;
  StoreDevice.Open('/root/demosettings.xml');
  CreateComponents;
  CreateDataConnectors;
  fAppSettings := GetAppSettings;
  PublishAppSettings;
  fPersonsData := Factory2.Locate<IDataListPersons>;
  fPersonsData.Load;
  fDataConnector.PSListChangeChannel.Publish(TListChange.New(fPersonsData.NewList));
 end;

{ TApp }

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TRBData, IRBData);

  RegisterDataClass(DIC, TAppSettings);
  RegisterDataClass(DIC, TPerson);
  RegisterDataClass(DIC, TTag);

  mReg := DIC.Add(TDataListTPersons, IDataListPersons);
  mReg.InjectProp('PubSub', IPubSub);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Factory2', TDIFactory2);
  mReg := DIC.Add(TListTags, IListTags);
  mReg.InjectProp('Factory2', TDIFactory2);


  mReg := DIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer);

  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'xml', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
end;

procedure TApp.RegisterAppServices;
var
  mReg: TDIReg;
begin
  inherited RegisterAppServices;
  RegisterPersist;
  RegReact.RegisterPubSubLauncher;
  RegApps.RegisterWindowLog;
  RegReact.RegisterCommon;
  RegVL.RegisterTimer(300, 'filter');

  mReg := RegReact.RegisterDesignComponent(TGUI, IDesignComponentApp);
  mReg.InjectProp('StoreDevice', IPersistStoreDevice, 'xml');
  mReg.InjectProp('PersistFactory', IPersistFactory);
  mReg := RegReact.RegisterDesignComponent(TGUIPersons, IGUIPersons);
  mReg := RegReact.RegisterDesignComponent(TGUICommands, IGUICommands);
  mReg := RegReact.RegisterDesignComponent(TGUIFilter, IGUIFilter);
end;

end.

