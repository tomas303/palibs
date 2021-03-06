unit tal_uapp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_dicontainer, trl_irttibroker, trl_ipersist,
  tal_ilauncher,
  trl_urttibroker,
  trl_isysutils, trl_usysutils,
  trl_upersiststore,
  tal_uhistorysettings, tal_ihistorysettings;

type

  { TALApp }

  TALApp = class
  private
    fDIC: TDIContainer;
    fDataFile: string;
    fSettingsFile: string;
    fLogFile: string;
  protected
    property DIC: TDIContainer read fDIC write fDIC;
    property DataFile: string read fDataFile write fDataFile;
    property SettingsFile: string read fSettingsFile write fSettingsFile;
    property LogFile: string read fLogFile write fLogFile;
  protected
    procedure InjectPersistRef(const AItem: IRBDataItem; ADIC: TDICustomContainer);
    procedure RegisterDataClass(ADIC: TDIContainer; AClass: TClass);
    procedure RegisterHistorySettings(ADIC: TDIContainer; const AStoreID: string = '');
    procedure RegisterPersistCommon(ADIC: TDIContainer);
  protected
    procedure SetUpDataFile;
    procedure RegisterSystemServices;
    procedure RegisterAppServices; virtual;
    procedure BeforeLaunch; virtual;
    procedure AfterLaunch; virtual;
  protected
    procedure Setup;
    procedure RegisterServices;
    procedure Launch;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Go;
  end;

implementation

{ TALApp }

procedure TALApp.InjectPersistRef(const AItem: IRBDataItem; ADIC: TDICustomContainer);
var
  mSysUtils: ISysUtils;
begin
  if AItem.IsInterface and Supports(AItem.AsInterface, IPersistRef) then
  begin
    // IPersistRef need resolve data via Store
    (AItem.AsInterface as IPersistRef).Store := ADIC.Locate(IPersistStore);
  end
  else
  if AItem.IsInterface and Supports(AItem.AsInterface, IPersistManyRefs) then
  begin
    // need to create IPersistRef members
    (AItem.AsInterface as IPersistManyRefs).Factory := ADIC.Locate(IPersistFactory);
  end;
  if AItem.IsID then begin
    mSysUtils := DIC.Locate(ISysUtils);
    AItem.AsString := mSysUtils.NewGID;
  end;
end;

procedure TALApp.RegisterDataClass(ADIC: TDIContainer; AClass: TClass);
var
  mReg: TDIReg;
begin
  // persist class
  mReg := ADIC.Add(AClass);
  mReg.InjectProp('', InjectPersistRef);
  // data envelop for persist class
  mReg := ADIC.Add(TRBData, IRBData, AClass.ClassName);
  mReg.InjectProp('UnderObject', AClass);
end;

procedure TALApp.RegisterHistorySettings(ADIC: TDIContainer; const AStoreID: string = '');
var
  mReg: TDIReg;
begin
  mReg := ADIC.Add(THistorySettings, IHistorySettings);
  mReg.InjectProp('Store', IPersistStore, AStoreID);
  //
  RegisterDataClass(ADIC, THistoryDataPosition);
  RegisterDataClass(ADIC, THistoryDataTexts);
  RegisterDataClass(ADIC, THistoryDataCheckBoxState);
  RegisterDataClass(ADIC, THistoryDataIntegers);
  RegisterDataClass(ADIC, THistoryDataMemo);
  RegisterDataClass(ADIC, THistoryData);
end;

procedure TALApp.RegisterPersistCommon(ADIC: TDIContainer);
var
  mReg: TDIReg;
begin
  mReg := ADIC.Add(TStoreCache);
  //
  mReg := ADIC.Add(TRBData, IRBData);
  //
  mReg := ADIC.Add(TSIDList, ISIDList);
  //
  mReg := ADIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := ADIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := ADIC.Add(TPersistRefList, IPersistRefList);
  //
  mReg := ADIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice);
  mReg.InjectProp('Cache', TStoreCache);
end;

procedure TALApp.SetUpDataFile;
var
  mAppDir, mSubdir, mExt: string;
begin
  if Paramcount > 0 then
    mAppDir := ParamStr(1)
  else
  begin
    mSubdir := '.' + ExtractFileName(ParamStr(0));
    mExt := ExtractFileExt(ParamStr(0));
    mSubDir := copy(mSubDir, 1, Length(mSubdir) - Length(mExt));
    {$IFDEF UNIX}
    mAppDir := GetEnvironmentVariable('HOME') + PathDelim + mSubdir + PathDelim;
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mAppDir := GetEnvironmentVariable('APPDATA') + PathDelim + mSubdir + PathDelim;
    {$ENDIF WINDOWS}
  end;
  if not DirectoryExists(mAppDir) then
  begin
    if not ForceDirectories(mAppDir) then
      raise Exception.Create('Cannot create directory ' + mAppDir);
  end;
  fDataFile := mAppDir + 'data.xml';
  fSettingsFile := mAppDir + 'settings.xml';
  fLogFile := mAppDir + 'log.txt';
end;

procedure TALApp.RegisterSystemServices;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TSysUtils, ISysUtils);
end;

procedure TALApp.Setup;
begin
  SetUpDataFile;
end;

procedure TALApp.RegisterServices;
begin
  RegisterSystemServices;
  RegisterAppServices;
end;

procedure TALApp.Launch;
var
  mLauncher: ILauncher;
begin
  BeforeLaunch;
  mLauncher := DIC.Locate(ILauncher);
  mLauncher.Launch;
  AfterLaunch;
end;

procedure TALApp.RegisterAppServices;
begin
end;

procedure TALApp.BeforeLaunch;
begin
end;

procedure TALApp.AfterLaunch;
begin
end;

constructor TALApp.Create;
begin
  fDIC := TDIContainer.Create;
end;

destructor TALApp.Destroy;
begin
  FreeAndNil(fDIC);
  inherited Destroy;
end;

class procedure TALApp.Go;
var
  mApp: TALApp;
begin
  mApp := Create;
  try
    mApp.Setup;
    mApp.RegisterServices;
    mApp.Launch;
  finally
    mApp.Free;
  end;
end;

end.

