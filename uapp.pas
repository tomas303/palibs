unit uapp;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  graphics, tal_uapp,
  rea_idesigncomponent, rea_udesigncomponent, rea_ilayout,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher,
  trl_urttibroker, trl_irttibroker,
  trl_upersiststore, trl_ipersist,
  trl_upersistxml,
  rea_idata, sysutils;

type
  { TApp }

  TApp = class(TALApp)
  private
    procedure RegisterPersist;
  protected
    procedure RegisterAppServices; override;
  end;

  { TGUI }

  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    fForm: IDesignComponentForm;
    fPager: IDesignComponent;
    fPage1, fPage2, fPage3: IDesignComponent;
    fPersonsNameEdit: IDesignComponentEdit;
    fPersonsSurenameEdit: IDesignComponentEdit;
    fPersonsGrid: IDesignComponentGrid;
    fPersonsDC: IDesignComponent;
    fPersonsEditStrip: IDesignComponentStrip;
    fCommandsStrip: IDesignComponentStrip;
    fNext, fPrior, fFirst, fLast: IDesignComponentButton;
    fDataConnector: IDataConnector;
    procedure CreateForm;
    procedure CreatePager;
    procedure CreateCommandsStrip;
    procedure CreatePersonsEditStrip;
    procedure CreatePersonsGrid;
    procedure CreatePersonsStrip;
    procedure CreateComponents;
  private
    fAppSettings: IRBData;
    fPSGUIChannel: IPSGUIChannel;
  private
    procedure PSSizeObserver(const AValue: TSizeData);
    procedure PSPositionObserver(const AValue: TPositionData);
    procedure PSCloseProgramObserver;
  private
    function GetAppSettings: IRBData;
    procedure PublishAppSettings;
    function GetPersons: IPersistRefList;
    procedure CreateDataConnectors;
  protected
    function PSGUIChannel: IPSGUIChannel;
    function DoCompose: IMetaElement; override;
    procedure InitValues; override;
  protected
    fStore: IPersistStore;
    fPersistFactory: IPersistFactory;
  published
    property Store: IPersistStore read fStore write fStore;
    property PersistFactory: IPersistFactory read fPersistFactory write fPersistFactory;
  end;

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


  { TPerson }

  TPerson = class
  private
    fName: String;
    fSurename: String;
  published
    property Name: String read fName write fName;
    property Surename: String read fSurename write fSurename;
  end;

implementation

{ TGUI }

procedure TGUI.CreateForm;
begin
  fForm := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.ID, 'mainform')
    .SetIntf('PSGUIChannel', fPSGUIChannel)
    .SetStr(cProps.Caption, 'Demo app')
    .SetInt(cProps.Color, clSilver)
    );
  fForm.PSSizeChannel.Subscribe(PSSizeObserver);
  fForm.PSPositionChannel.Subscribe(PSPositionObserver);
  fForm.PSCloseChannel.Subscribe(PSCloseProgramObserver);
end;

procedure TGUI.CreatePager;
begin
  fPager := Factory2.Locate<IDesignComponentPager>(NewProps
   .SetIntf('PSGUIChannel', fPSGUIChannel)
   .SetInt(cPager.SwitchEdge, cEdge.Top)
   .SetInt(cPager.SwitchSize, 25)
  );

  fPage1 := Factory2.Locate<IDesignComponentStrip>(NewProps.SetStr(cProps.Caption, 'red').SetInt(cProps.Color, clRed).SetBool('Transparent', False));
  (fPage1 as INode).AddChild(fPersonsDC as INode);
  fPage2 := Factory2.Locate<IDesignComponentStrip>(NewProps.SetStr(cProps.Caption, 'blue').SetInt(cProps.Color, clBlue).SetBool('Transparent', False));
  //(fPage2 as INode).AddChild(fPersonsNameEdit as INode);
  //(fPage2 as INode).AddChild(fPersonsSurenameEdit as INode);
  fPage3 := Factory2.Locate<IDesignComponentStrip>(NewProps.SetStr(cProps.Caption, 'lime').SetInt(cProps.Color, clLime).SetBool('Transparent', False));
  //(fPage3 as INode).AddChild(fPersonsDC as INode);

  (fPager as INode).AddChild(fPage1 as INode);
  (fPager as INode).AddChild(fPage2 as INode);
  (fPager as INode).AddChild(fPage3 as INode);

  (fForm as INode).AddChild(fPager as INode);

end;

procedure TGUI.CreateCommandsStrip;
begin
  fNext := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetStr(cProps.ID, 'next')
    .SetStr(cProps.Text, 'next'));
  fPrior := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetStr(cProps.ID, 'prior')
    .SetStr(cProps.Text, 'prior'));
  fFirst := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetStr(cProps.ID, 'first')
    .SetStr(cProps.Text, 'first'));
  fLast := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetStr(cProps.ID, 'last')
    .SetStr(cProps.Text, 'last'));

  fCommandsStrip := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.MMWidth, 50)
    .SetInt(cProps.MMHeight, 50)
    .SetInt(cProps.Layout, cLayout.Horizontal)
    .SetBool('Transparent', True));
  (fCommandsStrip as INode).AddChild(fFirst as INode);
  (fCommandsStrip as INode).AddChild(fPrior as INode);
  (fCommandsStrip as INode).AddChild(fNext as INode);
  (fCommandsStrip as INode).AddChild(fLast as INode);
end;

procedure TGUI.CreatePersonsEditStrip;
begin
  fPersonsNameEdit := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetStr(cProps.ID, 'name')
    .SetInt(cProps.Color, clAqua)
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    .SetBool(cProps.Flat, True)
    );
  fPersonsSurenameEdit := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetStr(cProps.ID, 'surename')
    .SetInt(cProps.Color, clSkyBlue)
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    .SetBool(cProps.Flat, True)
    );
  fPersonsEditStrip := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.MMHeight, 60)
    .SetInt(cProps.Layout, cLayout.Vertical)
    .SetBool('Transparent', True));
  (fPersonsEditStrip as INode).AddChild(fPersonsNameEdit as INode);
  (fPersonsEditStrip as INode).AddChild(fPersonsSurenameEdit as INode);
end;

procedure TGUI.CreatePersonsGrid;
begin
  fPersonsGrid := Factory2.Locate<IDesignComponentGrid>(NewProps
    .SetIntf('PSGUIChannel', fPSGUIChannel)
    .SetInt(cGrid.RowCount, 5)
    .SetInt(cGrid.ColCount, 2)
    .SetInt(cGrid.MMWidth, 200)
    .SetInt(cGrid.MMHeight, 500)
    .SetInt(cGrid.RowMMHeight, 25)
    .SetInt(cGrid.ColMMWidth, 25)
    .SetInt(cGrid.LaticeColColor, clBlack)
    .SetInt(cGrid.LaticeRowColor, clBlack)
    .SetInt(cGrid.LaticeColSize, 2)
    .SetInt(cGrid.LaticeRowSize, 2)
    );
end;

procedure TGUI.CreatePersonsStrip;
var
  mStrip1, mStrip2: IDesignComponentStrip;
  mH: IDesignComponent;
begin
  fPersonsDC := Factory2.Locate<IDesignComponentVBox>(NewProps
    .SetInt(cProps.BoxLaticeSize, 5)
    .SetInt(cProps.Color, clGray)
    .SetBool('Transparent', True));
  mH := Factory2.Locate<IDesignComponentHBox>(NewProps
    .SetInt(cProps.BoxLaticeSize, 5)
    .SetInt(cProps.Color, clGray)
    .SetBool('Transparent', True));
  (fPersonsDC as INode).AddChild(mH as INode);

  mStrip1 := Factory2.Locate<IDesignComponentStrip>(NewProps
      .SetInt(cProps.Place, cPlace.Elastic)
      .SetInt(cProps.Layout, cLayout.Overlay)
      .SetInt(cProps.MMWidth, 100)
      .SetInt(cProps.Color, clGreen)
      .SetBool('Transparent', False));
  (mStrip1 as INode).AddChild(fPersonsGrid as INode);

  mStrip2 := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetInt(cProps.Layout, cLayout.Vertical)
    .SetInt(cProps.MMWidth, 50)
    .SetInt(cProps.Color, clPurple)
    .SetBool('Transparent', False));
  (mStrip2 as INode).AddChild(fPersonsEditStrip as INode);
  (mStrip2 as INode).AddChild(fCommandsStrip as INode);

  (mH as INode).AddChild(mStrip1 as INode);
  (mH as INode).AddChild(mStrip2 as INode);


end;

procedure TGUI.CreateComponents;
begin
  CreateForm;
  CreatePersonsGrid;
  CreatePersonsEditStrip;
  CreateCommandsStrip;
  CreatePersonsStrip;
  CreatePager;
end;

procedure TGUI.CreateDataConnectors;
begin
  fDataConnector := Factory2.Locate<IDataConnector>('TStoreConnector', NewProps.SetIntf('List', GetPersons));
  fDataConnector.RegisterEdit('Name', fPersonsNameEdit);
  fDataConnector.RegisterEdit('Surename', fPersonsSurenameEdit);
  fDataConnector.RegisterGrid(TArray<String>.Create('Name', 'Surename'), fPersonsGrid, TPerson);
  fDataConnector.RegisterCommand(fFirst.PSClickChannel, TCommand.CreateFirst);
  fDataConnector.RegisterCommand(fLast.PSClickChannel, TCommand.CreateLast);
  fDataConnector.RegisterCommand(fNext.PSClickChannel, TCommand.CreateNext);
  fDataConnector.RegisterCommand(fPrior.PSClickChannel, TCommand.CreatePrior);
end;

function TGUI.GetPersons: IPersistRefList;
var
  mPerson: IRBData;
  i: integer;
begin
  Result := (Store as IPersistQuery).SelectClass(TPerson.ClassName);
  if Result.Count = 0 then
  begin
    //mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
    //mPerson.ItemByName['Name'].AsString := 'John';
    //mPerson.ItemByName['Surename'].AsString := 'Doe';
    //Store.Save(mPerson);
    //mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
    //mPerson.ItemByName['Name'].AsString := 'Jim';
    //mPerson.ItemByName['Surename'].AsString := 'Beam';
    //Store.Save(mPerson);
    //mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
    //mPerson.ItemByName['Name'].AsString := 'Anthony';
    //mPerson.ItemByName['Surename'].AsString := 'Hopkins';
    //Store.Save(mPerson);
    for i := 1 to 14 do begin
      mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
      mPerson.ItemByName['Name'].AsString := 'Name ' + i.ToString;
      mPerson.ItemByName['Surename'].AsString := 'Surename ' + i.ToString;
      Store.Save(mPerson);
    end;
    Result := (Store as IPersistQuery).SelectClass(TPerson.ClassName);
  end;
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
  Store.Save(fAppSettings);
  Store.Close;
  raise ELaunchStop.Create('');
end;

function TGUI.GetAppSettings: IRBData;
var
  mList: IPersistRefList;
begin
  mList := (Store as IPersistQuery).SelectClass(TAppSettings.ClassName);
  if mList.Count = 0 then
  begin
    Result := PersistFactory.Create(IRBData, TAppSettings.ClassName) as IRBData;
    Result.ItemByName['Width'].AsInteger := 600;
    Result.ItemByName['Height'].AsInteger := 200;
    Result.ItemByName['Left'].AsInteger := 300;
    Result.ItemByName['Top'].AsInteger := 400;
  end
  else
  begin
    Result := mList.Data[0];
  end;
end;

procedure TGUI.PublishAppSettings;
begin
  fForm.PSSizeChannel.Publish(TSizeData.Create(Self, fAppSettings.ItemByName['Width'].AsInteger, fAppSettings.ItemByName['Height'].AsInteger));
  fForm.PSPositionChannel.Publish(TPositionData.Create(Self, fAppSettings.ItemByName['Left'].AsInteger, fAppSettings.ItemByName['Top'].AsInteger));
end;

function TGUI.PSGUIChannel: IPSGUIChannel;
begin
  Result := fPSGUIChannel;
end;

function TGUI.DoCompose: IMetaElement;
begin
  Result := fForm.Compose;
end;

procedure TGUI.InitValues;
begin
  inherited InitValues;
  fPSGUIChannel := PubSub.Factory.NewDataChannel<TGUIData>;
  Store.Open('/root/demosettings.xml');
  CreateComponents;
  CreateDataConnectors;
  fAppSettings := GetAppSettings;
  PublishAppSettings;
 end;

{ TApp }

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TRBData, IRBData);
  //
  mReg := DIC.Add(TSIDList, ISIDList);
  //
  mReg := DIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(DIC, TAppSettings);
  RegisterDataClass(DIC, TPerson);

  mReg := DIC.Add(TPersistRef<TPerson>, IPersistRef, TPerson.ClassName);
  mReg.InjectProp('Store', IPersistStore);

  //
  mReg := DIC.Add(TStoreCache);
  //
  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := DIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer);
  //
  mReg := DIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
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
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('PersistFactory', IPersistFactory);
end;

end.

