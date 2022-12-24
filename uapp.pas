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
  rea_idata;

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
    fS1, fS2, fS3: IDesignComponent;
    fPager: IDesignComponent;
    fGrid: IDesignComponentGrid;
    fDataConnector: IDataConnector;
    fSPersons, fSData, fSCommands: IDesignComponentStrip;
    fEditName, fEditSurename: IDesignComponentEdit;
    fNext, fPrior, fFirst, fLast: IDesignComponentButton;
    procedure CreateComponents;
    function GetPersons: IPersistRefList;
  private
    fAppSettings: IRBData;
    fPSGUIChannel: IPSGUIChannel;
    procedure PSNameObserver(const AValue: String);
    procedure PSSurenameObserver(const AValue: String);
    procedure PSSizeObserver(const AValue: TSizeData);
    procedure PSPositionObserver(const AValue: TPositionData);
  private
    procedure BeforeClose;
    procedure CloseProgram;
  protected
    function PSGUIChannel: IPSGUIChannel;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
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
    fName: String;
    fSurename: String;
  published
    property Top: Integer read fTop write fTop;
    property Left: Integer read fLeft write fLeft;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
    property Name: String read fName write fName;
    property Surename: String read fSurename write fSurename;
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

  { TStoreDataConnector }

  TStoreDataConnector =  class(TInterfacedObject, IDataConnector)
  private
    procedure PublishData;
  private
    fActualData: IRBData;
    fActualIndex: integer;
    fList: IPersistRefList;
    fStore: IPersistStore;
    fPubSub: IPubSub;
    fPSFieldDataChannel: IPSFieldDataChannel;
    fPSCommandDataChannel: IPSCommandDataChannel;
    fPSGUIChannel: IPSGUIChannel;
    procedure PSFieldDataChannelObserver(const AData: TFieldData);
    procedure PSCommandDataChannelObserver(const AData: TCommandData);
  protected
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSCommandDataChannel: IPSCommandDataChannel;
  public
    constructor Create(const APubSub: IPubSub; const AStore: IPersistStore; const AList: IPersistRefList;
      const APSGUIChannel: IPSGUIChannel);
    procedure BeforeDestruction; override;
  end;

implementation

{ TStoreDataConnector }

procedure TStoreDataConnector.PublishData;
var
  i: integer;
begin
  if fActualData = nil then
    Exit;
  for i := 0 to fActualData.Count - 1 do begin
    fPSFieldDataChannel.Publish(TFieldData.Create(
      fActualData.Items[i].Name,
      fActualData.Items[i].AsString
    ));
  end;
end;

procedure TStoreDataConnector.PSFieldDataChannelObserver(const AData: TFieldData
  );
begin
  if fActualData = nil then
    Exit;
  fActualData.ItemByName[AData.Name].AsString := AData.Value;
  fStore.Save(fActualData);
end;

procedure TStoreDataConnector.PSCommandDataChannelObserver(
  const AData: TCommandData);
begin
  case AData.Action of
    cdaNext:
      begin
        if fActualIndex < fList.Count -1 then begin
          inc(fActualIndex);
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
    cdaPrior:
      begin
        if fActualIndex > 0 then begin
          dec(fActualIndex);
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
    cdaFirst:
      begin
        if fList.Count > 0 then begin
          fActualIndex := 0;
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
    cdaLast:
      begin
        if fList.Count > 0 then begin
          fActualIndex := fList.Count - 1;
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
  end;
end;

function TStoreDataConnector.PSFieldDataChannel: IPSFieldDataChannel;
begin
  Result := fPSFieldDataChannel;
end;

function TStoreDataConnector.PSCommandDataChannel: IPSCommandDataChannel;
begin
  Result := fPSCommandDataChannel;
end;

constructor TStoreDataConnector.Create(const APubSub: IPubSub;
  const AStore: IPersistStore;
  const AList: IPersistRefList;
  const APSGUIChannel: IPSGUIChannel);
begin
  fPubSub := APubSub;
  fStore := AStore;
  fList := AList;
  fPSGUIChannel := APSGUIChannel;
  fPSFieldDataChannel := fPubSub.Factory.NewDataChannel<TFieldData>;
  fPSFieldDataChannel.Subscribe(PSFieldDataChannelObserver);
  fPSCommandDataChannel := fPubSub.Factory.NewDataChannel<TCommandData>;
  fPSCommandDataChannel.Subscribe(PSCommandDataChannelObserver);
  if fList.Count > 0 then
    fActualData := fList.Data[0];
  PublishData;
end;

procedure TStoreDataConnector.BeforeDestruction;
begin
  fPSFieldDataChannel.Unsubscribe(PSFieldDataChannelObserver);
  fPSCommandDataChannel.Unsubscribe(PSCommandDataChannelObserver);
  inherited BeforeDestruction;
end;

{ TGUI }

procedure TGUI.CreateComponents;
begin
  fForm := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.ID, 'mainform')
    .SetIntf('PSGUIChannel', fPSGUIChannel)
    .SetStr(cProps.Caption, 'Demo app')
    .SetInt(cProps.Color, clSilver)
    );

  fEditName := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetStr(cProps.ID, 'name')
    .SetInt(cProps.Color, clAqua)
    .SetInt(cProps.MMWidth, 50)
    .SetInt(cProps.MMHeight, 50)
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    );
  fEditSurename := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetStr(cProps.ID, 'surename')
    .SetInt(cProps.Color, clSkyBlue)
    .SetInt(cProps.MMWidth, 50)
    .SetInt(cProps.MMHeight, 50)
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    );

  fSPersons := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetInt(cProps.Layout, cLayout.Vertical)
    //.SetInt(cProps.MMWidth, 50)
    //.SetInt(cProps.MMHeight, 50)
    .SetBool('Transparent', True));
  fSData := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetInt(cProps.Layout, cLayout.Vertical)
    //.SetInt(cProps.MMWidth, 50)
    //.SetInt(cProps.MMHeight, 60)
    .SetBool('Transparent', True));
  fSCommands := Factory2.Locate<IDesignComponentStrip>(NewProps
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetInt(cProps.Layout, cLayout.Horizontal)
    //.SetInt(cProps.MMWidth, 50)
    //.SetInt(cProps.MMHeight, 50)
    .SetBool('Transparent', True));

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


  (fSData as INode).AddChild(fEditName as INode);
  (fSData as INode).AddChild(fEditSurename as INode);
  (fSCommands as INode).AddChild(fFirst as INode);
  (fSCommands as INode).AddChild(fPrior as INode);
  (fSCommands as INode).AddChild(fNext as INode);
  (fSCommands as INode).AddChild(fLast as INode);

  (fSPersons as INode).AddChild(fSData as INode);
  (fSPersons as INode).AddChild(fSCommands as INode);



  fGrid := Factory2.Locate<IDesignComponentGrid>(NewProps
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

  fPager := Factory2.Locate<IDesignComponentPager>(NewProps
   .SetIntf('PSGUIChannel', fPSGUIChannel)
   .SetInt(cPager.SwitchEdge, cEdge.Top)
   .SetInt(cPager.SwitchSize, 25)
  );

  fS1 := Factory2.Locate<IDesignComponentStrip>(NewProps.SetStr(cProps.Caption, 'red').SetInt(cProps.Color, clRed).SetBool('Transparent', False));
  (fS1 as INode).AddChild(fGrid as INode);
  fS2 := Factory2.Locate<IDesignComponentStrip>(NewProps.SetStr(cProps.Caption, 'blue').SetInt(cProps.Color, clBlue).SetBool('Transparent', False));
  //(fS2 as INode).AddChild(fEditName as INode);
  //(fS2 as INode).AddChild(fEditSurename as INode);
  fS3 := Factory2.Locate<IDesignComponentStrip>(NewProps.SetStr(cProps.Caption, 'lime').SetInt(cProps.Color, clLime).SetBool('Transparent', False));
  (fS3 as INode).AddChild(fSPersons as INode);

  (fPager as INode).AddChild(fS1 as INode);
  (fPager as INode).AddChild(fS2 as INode);
  (fPager as INode).AddChild(fS3 as INode);

  (fForm as INode).AddChild(fPager as INode);

end;

function TGUI.GetPersons: IPersistRefList;
var
  mPerson: IRBData;
begin
  Result := (Store as IPersistQuery).SelectClass(TPerson.ClassName);
  if Result.Count = 0 then
  begin
    mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
    mPerson.ItemByName['Name'].AsString := 'John';
    mPerson.ItemByName['Surename'].AsString := 'Doe';
    Store.Save(mPerson);
    mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
    mPerson.ItemByName['Name'].AsString := 'Jim';
    mPerson.ItemByName['Surename'].AsString := 'Beam';
    Store.Save(mPerson);
    mPerson := PersistFactory.Create(IRBData, TPerson.ClassName) as IRBData;
    mPerson.ItemByName['Name'].AsString := 'Anthony';
    mPerson.ItemByName['Surename'].AsString := 'Hopkins';
    Store.Save(mPerson);
  end;
  Result := (Store as IPersistQuery).SelectClass(TPerson.ClassName);
end;

procedure TGUI.PSNameObserver(const AValue: String);
begin
  fAppSettings.ItemByName['Name'].AsString := AValue;
end;

procedure TGUI.PSSurenameObserver(const AValue: String);
begin
  fAppSettings.ItemByName['Surename'].AsString := AValue;
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

procedure TGUI.BeforeClose;
begin
  Store.Save(fAppSettings);
  Store.Close;
end;

procedure TGUI.CloseProgram;
begin
  BeforeClose;
  raise ELaunchStop.Create('');
end;

function TGUI.PSGUIChannel: IPSGUIChannel;
begin
  Result := fPSGUIChannel;
end;

function TGUI.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray
  ): IMetaElement;
begin
  Result := fForm.Compose(nil, []);
end;

procedure TGUI.InitValues;
var
  mList: IPersistRefList;
  mListPersons: IPersistRefList;
begin
  inherited InitValues;

  fPSGUIChannel := PubSub.Factory.NewDataChannel<TGUIData>;

  Store.Open('/root/demosettings.xml');
  mList := (Store as IPersistQuery).SelectClass(TAppSettings.ClassName);
  if mList.Count = 0 then
  begin
    fAppSettings := PersistFactory.Create(IRBData, TAppSettings.ClassName) as IRBData;
    fAppSettings.ItemByName['Name'].AsString := 'John';
    fAppSettings.ItemByName['Surename'].AsString := 'Doe';
    fAppSettings.ItemByName['Width'].AsInteger := 600;
    fAppSettings.ItemByName['Height'].AsInteger := 200;
    fAppSettings.ItemByName['Left'].AsInteger := 300;
    fAppSettings.ItemByName['Top'].AsInteger := 400;
  end
  else
  begin
    fAppSettings := mList.Data[0];
  end;



  CreateComponents;

  fForm.PSSizeChannel.Publish(TSizeData.Create(Self, fAppSettings.ItemByName['Width'].AsInteger, fAppSettings.ItemByName['Height'].AsInteger));
  fForm.PSSizeChannel.Subscribe(PSSizeObserver);
  fForm.PSPositionChannel.Publish(TPositionData.Create(Self, fAppSettings.ItemByName['Left'].AsInteger, fAppSettings.ItemByName['Top'].AsInteger));
  fForm.PSPositionChannel.Subscribe(PSPositionObserver);
  fForm.PSCloseChannel.Subscribe(CloseProgram);

  fEditName.PSTextChannel.Subscribe(PSNameObserver);
  fEditSurename.PSTextChannel.Subscribe(PSSurenameObserver);

  fEditName.PSTextChannel.Publish(fAppSettings.ItemByName['Name'].AsString);
  fEditSurename.PSTextChannel.Publish(fAppSettings.ItemByName['Surename'].AsString);

  {
  PubSub.Factory.NewDataBridge<String, String>(
    fEditName.PSTextChannel,
    fEditSurename.PSTextChannel,
    function (const AData: String): String
    begin
      Result := '!' + AData + '!';
    end);
   }

   fDataConnector := TStoreDataConnector.Create(PubSub, Store, GetPersons, fPSGUIChannel);
   PubSub.Factory.NewDuplexDataBridge<String, TFieldData>(
     fEditName.PSTextChannel,
     fDataConnector.PSFieldDataChannel,
     function (const AData: String): TFieldData
     begin
       Result := TFieldData.Create('Name', AData);
     end,
     function (const AData: TFieldData): String
     begin
       if AData.Name = 'Name' then
         Result := AData.Value
       else
         raise EPubSubBridgeNoWay.Create('');
     end);
   PubSub.Factory.NewDuplexDataBridge<String, TFieldData>(
     fEditSurename.PSTextChannel,
     fDataConnector.PSFieldDataChannel,
     function (const AData: String): TFieldData
     begin
       Result := TFieldData.Create('Surename', AData);
     end,
     function (const AData: TFieldData): String
     begin
       if AData.Name = 'Surename' then
         Result := AData.Value
       else
         raise EPubSubBridgeNoWay.Create('');
     end);
   PubSub.Factory.NewNonDataToDataBridge<TCommandData>(
     fFirst.PSClickChannel,
     fDataConnector.PSCommandDataChannel,
     function: TCommandData
     begin
       Result := TCommandData.Create(cdaFirst);
     end);
   PubSub.Factory.NewNonDataToDataBridge<TCommandData>(
     fLast.PSClickChannel,
     fDataConnector.PSCommandDataChannel,
     function: TCommandData
     begin
       Result := TCommandData.Create(cdaLast);
     end);
   PubSub.Factory.NewNonDataToDataBridge<TCommandData>(
     fNext.PSClickChannel,
     fDataConnector.PSCommandDataChannel,
     function: TCommandData
     begin
       Result := TCommandData.Create(cdaNext);
     end);
   PubSub.Factory.NewNonDataToDataBridge<TCommandData>(
     fPrior.PSClickChannel,
     fDataConnector.PSCommandDataChannel,
     function: TCommandData
     begin
       Result := TCommandData.Create(cdaPrior);
     end);
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

