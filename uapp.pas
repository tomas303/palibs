unit uapp;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}

interface

uses
  graphics, tal_uapp,
  rea_idesigncomponent, rea_udesigncomponent,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher,
  trl_urttibroker, trl_irttibroker,
  trl_upersiststore, trl_ipersist,
  trl_upersistxml;

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
    fAppSettings: IRBData;
    fNameChannel: IPSTextChannel;
    fSurenameChannel: IPSTextChannel;
    fCloseChannel: IPSCloseChannel;
    fPSSizeChannel: IPSSizeChannel;
    fPSPositionChannel: IPSPositionChannel;
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

implementation

{ TGUI }

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
  fPSGUIChannel.Publish(TGUIData.Create(gaRender));
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
var
  mFF: IDesignComponentFormFactory;
  mFE: IDesignComponentEditFactory;
  mF, mEditName, mEditSurename: IDesignComponent;
begin
  mF := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.Caption, 'Demo app')
    .SetInt(cProps.Color, clGreen)
    .SetInt(cProps.MMLeft, fAppSettings.ItemByName['Left'].AsInteger)
    .SetInt(cProps.MMTop, fAppSettings.ItemByName['Top'].AsInteger)
    .SetInt(cProps.MMWidth, fAppSettings.ItemByName['Width'].AsInteger)
    .SetInt(cProps.MMHeight, fAppSettings.ItemByName['Height'].AsInteger)
    .SetIntf(cForm.PSCloseChannel, fCloseChannel)
    .SetIntf(cForm.PSSizeChannel, fPSSizeChannel)
    .SetIntf(cForm.PSPositionChannel, fPSPositionChannel)
    );
  mEditName := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetInt(cProps.Color, clRed)
    .SetInt(cProps.MMWidth, 50)
    .SetStr(cProps.Text, 'first')
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    .SetIntf(cEdit.PSTextChannel, fNameChannel)
    );
  mEditSurename := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetInt(cProps.Color, clBlue)
    .SetInt(cProps.MMWidth, 50)
    .SetStr(cProps.Text, 'second')
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    .SetIntf(cEdit.PSTextChannel, fSurenameChannel)
    );
  (mF as INode).AddChild(mEditName as INode);
  (mF as INode).AddChild(mEditSurename as INode);
  Result := mF.Compose(nil, []);
end;

procedure TGUI.InitValues;
var
  mList: IPersistRefList;
begin
  inherited InitValues;
  fNameChannel := PubSub.Factory.NewDataChannel<String>;
  fNameChannel.Subscribe(PSNameObserver);

  fSurenameChannel := PubSub.Factory.NewDataChannel<String>;
  fSurenameChannel.Subscribe(PSSurenameObserver);

  fCloseChannel := PubSub.Factory.NewChannel;
  fCloseChannel.Subscribe(CloseProgram);

  fPSSizeChannel := PubSub.Factory.NewDataChannel<TSizeData>;
  fPSSizeChannel.Subscribe(PSSizeObserver);

  fPSPositionChannel := PubSub.Factory.NewDataChannel<TPositionData>;
  fPSPositionChannel.Subscribe(PSPositionObserver);

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
  fNameChannel.Publish(fAppSettings.ItemByName['Name'].AsString);
  fSurenameChannel.Publish(fAppSettings.ItemByName['Surename'].AsString);
  fPSSizeChannel.Publish(TSizeData.Create(Self, fAppSettings.ItemByName['Width'].AsInteger, fAppSettings.ItemByName['Height'].AsInteger));
  fPSPositionChannel.Publish(TPositionData.Create(Self, fAppSettings.ItemByName['Left'].AsInteger, fAppSettings.ItemByName['Top'].AsInteger));
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

