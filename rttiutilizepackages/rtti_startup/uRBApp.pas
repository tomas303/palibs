unit uRBApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, controls,
  rtti_serializer_uXmlStore, rtti_serializer_uFactory, rtti_broker_iBroker,
  rtti_idebinder_iBindings, rtti_idebinder_uDesigner, iStart,
  rtti_broker_uPersistClassRegister;

type

  { TRBApp }

  TRBApp = class(TInterfacedObject, IApp, IAppContext_kvyhozeni)
  private
    fSerialFactory: IRBFactory;
    fConfig, fData: IRBStore;
    fDesigner: IRBDesigner;
    fMainF: TForm;
    fStorageFile: string;
    fControlClasses: IRBPersistClassRegister;
  protected
    // IMainContext
    function GetConfig: IRBStore;
    function GetData: IRBStore;
    function GetClassFactory: IRBFactory;
    function GetDataQuery: IRBDataQuery;
    function GetDesigner: IRBDesigner;
    function GetBinderContext: IRBBinderContext;
    function GetControlClasses: IRBPersistClassRegister;
    function PrepareHomeDir(const AHomeSubdir: string): string;
    function ConfigFile: string;
    function ConifgFileName: string; virtual;
    function DefaultDataFile: string;
  public
    procedure OpenConfig;
    function OpenData: IRBStore; virtual;
    procedure RegisterDataClasses(const AClasses: array of TClass);
    procedure RegisterControlClasses(const ACtlClasses: array of TControlClass);
    procedure Run(const AFormClass: TFormClass);
    property ClassFactory: IRBFactory read GetClassFactory;
    property Config: IRBStore read GetConfig;
    property DataQuery: IRBDataQuery read GetDataQuery;
    property Designer: IRBDesigner read GetDesigner;
    property ControlClasses: IRBPersistClassRegister read GetControlClasses;
    property BinderContext: IRBBinderContext read GetBinderContext;
  end;

implementation

{ TRBApp }

function TRBApp.GetConfig: IRBStore;
begin
  if fConfig = nil then
    raise Exception.Create('Config store not available');
  Result := fConfig;
end;

function TRBApp.GetData: IRBStore;
begin
  if fData = nil then
    fData := OpenData;
  if fData = nil then
    raise Exception.Create('Data store not available');
  Result := fData;
end;

function TRBApp.GetClassFactory: IRBFactory;
begin
  if fSerialFactory = nil then
    fSerialFactory := TSerialFactory.Create;
  Result := fSerialFactory;
end;

function TRBApp.GetDataQuery: IRBDataQuery;
begin
  Result := Config as IRBDataQuery;
end;

function TRBApp.GetDesigner: IRBDesigner;
begin
  if fDesigner = nil then begin
    fDesigner := TRBDesigner.Create;
  end;
  Result := fDesigner;
end;

function TRBApp.GetBinderContext: IRBBinderContext;
begin
  //Result := Self;
end;

function TRBApp.GetControlClasses: IRBPersistClassRegister;
begin
  if fControlClasses = nil then
    fControlClasses := TRBPersistClassRegister.Create;
  Result := fControlClasses;
end;

function TRBApp.PrepareHomeDir(const AHomeSubdir: string): string;
begin
{$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + AHomeSubdir + PathDelim;
{$ENDIF UNIX}
{$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + AHomeSubdir + PathDelim;
{$ENDIF WINDOWS}
  if not DirectoryExists(Result) then
  begin
    if not ForceDirectories(Result) then
      raise Exception.Create('Cannot create directory ' + Result);
  end;
end;

function TRBApp.ConfigFile: string;
begin
  Result := PrepareHomeDir('.' + ExtractFileName(ParamStr(0))) + ConifgFileName;
end;

function TRBApp.ConifgFileName: string;
begin
  Result := 'config.xml';
end;

function TRBApp.DefaultDataFile: string;
begin
  Result := PrepareHomeDir('.' + ExtractFileName(ParamStr(0))) + 'data.xml';
end;

procedure TRBApp.OpenConfig;
begin
  if fConfig <> nil then
    fConfig.Flush;
  fConfig := TXmlStore.Create(ClassFactory, ConfigFile);
end;

function TRBApp.OpenData: IRBStore;
begin
  Result := TXmlStore.Create(ClassFactory, DefaultDataFile);
end;

procedure TRBApp.RegisterDataClasses(const AClasses: array of TClass);
var
  m: TClass;
begin
  for m in AClasses do
    ClassFactory.RegisterClass(m);
end;

procedure TRBApp.RegisterControlClasses(
  const ACtlClasses: array of TControlClass);
var
  m: TControlClass;
begin
  for m in ACtlClasses do
    ControlClasses.Add(m);
end;

procedure TRBApp.Run(const AFormClass: TFormClass);
begin
  OpenConfig;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(AFormClass, fMainF);
  Designer.Bind(fMainF, Config, ClassFactory, DataQuery, ControlClasses);
  //if Supports(fMainF, IStartContextConnectable) then
  //  (fMainF as IStartContextConnectable).Connect(self);
  //Application.ShowMainForm := False;
  Application.Run;
end;

end.

