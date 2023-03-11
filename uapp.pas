unit uapp;

{$mode delphi}{$H+}{$M+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  graphics, tal_uapp, typinfo, rtti,
  rea_idesigncomponent, rea_udesigncomponent, rea_ilayout,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher,
  trl_urttibroker, trl_irttibroker,
  trl_upersiststore, trl_ipersist,
  trl_upersistxml,
  rea_idata, sysutils,
  trl_usystem, trl_upersist,
  trl_udifactory,
  trl_iminipersist, trl_uminipersist;

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
    fTop: Integer;
    fLeft: Integer;
    fWidth: Integer;
    fHeight: Integer;
  published
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

  { TPerson }

  TPerson = class(TPlainObject)
  private
    fID: String;
    fName: String;
    fSurename: String;
//    fTags: IMiniList<TTag>;
  published
    [PersistIDAttribute, PersistAUTOAttribute]
    property ID: String read fID write fID;
    property Name: String read fName write fName;
    property Surename: String read fSurename write fSurename;
//    property Tags: IMiniList<TTag> read fTags write fTags;
  end;

  IDataListPersons = IMiniDataList<TPerson>;

  { IGUIPersons }

  IGUIPersons = interface(IDesignComponent)
  ['{15239075-7E14-4C90-B824-8C1F7E93131A}']
    function GetNameEdit: IDesignComponentEdit;
    function GetSurenameEdit: IDesignComponentEdit;
    function GetGrid: IDesignComponentGrid;
    function GetIDText: IDesignComponentText;
    property NameEdit: IDesignComponentEdit read GetNameEdit;
    property SurenameEdit: IDesignComponentEdit read GetSurenameEdit;
    property Grid: IDesignComponentGrid read GetGrid;
    property IDText: IDesignComponentText read GetIDText;
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
    function ComposeEditRow(const ACaption: String; AEdit: IDesignComponent): IDesignComponent;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
    function GetNameEdit: IDesignComponentEdit;
    function GetSurenameEdit: IDesignComponentEdit;
    function GetGrid: IDesignComponentGrid;
    function GetIDText: IDesignComponentText;
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
    fForm: IDesignComponentForm;
    fDataConnector: IDataConnector;
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
  private
    function GetAppSettings: IRBData;
    procedure PublishAppSettings;
    procedure CreateComponents;
    procedure CreateDataConnectors;
  protected
    function DoCompose: IMetaElement; override;
    procedure InitValues; override;
  protected
    //fStore: IPersistStore;
    fPersistFactory: IPersistFactory;
    fPSGUIChannel: IPSGUIChannel;
  published
    //property Store: IPersistStore read fStore write fStore;
    property PersistFactory: IPersistFactory read fPersistFactory write fPersistFactory;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

implementation

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
  fPersons := Factory2.Locate<IGUIPersons>(NewProps
   .SetIntf('PSGUIChannel', fPSGUIChannel)
   .SetInt(cProps.Color, clCream)
  );
  fCommands := Factory2.Locate<IGUICommands>(NewProps
   .SetInt(cProps.Color, clLtGray)
   .SetBool(cProps.Transparent, False)
  );
  mPager := NewPager([
    Morph.NewPage('Persons', cLayout.Vertical, [fPersons, Morph.WrapInStrip(fCommands, 25, cPlace.FixBack)]),
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
  //Store.Save(fAppSettings);
  //Store.Close;
  raise ELaunchStop.Create('');
end;

procedure TGUI.PSShowLogObserver;
begin
  Log.Visible := not Log.Visible;
end;

procedure TGUI.PSSaveDataObserver;
begin
  fPersonsData.Save;
end;

function TGUI.GetAppSettings: IRBData;
//var
//  mList: IPersistRefList;
begin
  //mList := (Store as IPersistQuery).SelectClass(TAppSettings.ClassName);
  //if mList.Count = 0 then
  //begin
  //  Result := PersistFactory.Create(IRBData, TAppSettings.ClassName) as IRBData;
  //  Result.ItemByName['Width'].AsInteger := 600;
  //  Result.ItemByName['Height'].AsInteger := 200;
  //  Result.ItemByName['Left'].AsInteger := 300;
  //  Result.ItemByName['Top'].AsInteger := 400;
  //end
  //else
  //begin
  //  Result := mList.Data[0];
  //end;
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
  //Store.Open('/root/demosettings.xml');
  CreateComponents;
  CreateDataConnectors;
  fAppSettings := GetAppSettings;
  PublishAppSettings;
  fPersonsData := Factory2.Locate<IDataListPersons>;
  fPersonsData.Load;
  fDataConnector.ConnectList(fPersonsData.NewList);
 end;

{ TApp }

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TRBData, IRBData);
  //
  //mReg := DIC.Add(TSIDList, ISIDList);
  //
  //mReg := DIC.Add(TPersistRef, IPersistRef);
  //mReg.InjectProp('Store', IPersistStore);
  //
  //mReg := DIC.Add(TPersistManyRefs, IPersistManyRefs);
  //mReg.InjectProp('Store', IPersistStore);
  //
  //mReg := DIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(DIC, TAppSettings);
  RegisterDataClass(DIC, TPerson);
  RegisterDataClass(DIC, TTag);

  //mReg := DIC.Add(TPersistDataList_Objects<TPerson>, IDataList<TPerson>);
  //mReg := DIC.Add(TDataList_Primitives<String>, IDataList<String>);


  //mReg := DIC.Add(TPersistRef<TPerson>, IPersistRef, TPerson.ClassName);
  //mReg.InjectProp('Store', IPersistStore);


  mReg := DIC.Add(TMiniDataList<TPerson>, IDataListPersons);
  mReg.InjectProp('PubSub', IPubSub);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');

  mReg := DIC.Add(TMiniList<TPerson>, IMiniList<TPerson>);
  mReg := DIC.Add(TMiniList<TTag>, IMiniList<TTag>);


  //
  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'xml', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := DIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer);
  //
  //mReg := DIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  //mReg.InjectProp('Factory', IPersistFactory);
  //mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  //mReg.InjectProp('Cache', TStoreCache);
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
  mReg := RegReact.RegisterDesignComponent(TGUI, IDesignComponentApp);
  //mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('PersistFactory', IPersistFactory);
  mReg := RegReact.RegisterDesignComponent(TGUIPersons, IGUIPersons);
  mReg := RegReact.RegisterDesignComponent(TGUICommands, IGUICommands);
end;

end.

