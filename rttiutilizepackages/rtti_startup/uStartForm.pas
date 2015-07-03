unit uStartForm;

{$mode objfpc}{$H+}

interface

uses
  forms, sysutils, classes, controls,
  iStart, rtti_serializer_uXmlStore,
  rtti_serializer_uFactory, rtti_broker_iBroker, rtti_idebinder_iBindings,
  rtti_idebinder_uDesigner;

type
   EMain = class(Exception)

   end;

  { TStartForm }

  TStartForm = class(TComponent, IStartContext, IRBBinderContext)
  private
    fSerialFactory: IRBFactory;
    fDataStore: IRBStore;
    fDesigner: IRBDesigner;
    fMainF: TForm;
  protected
    // IMainContext
    function GetDataStore: IRBStore;
    function GetSerialFactory: IRBFactory;
    function GetDataQuery: IRBDataQuery;
    function GetDesigner: IRBDesigner;
    function GetBinderContext: IRBBinderContext;
  protected
    function PrepareHomeDir(const AHomeSubdir: string): string;
    function OpenDataStore(const AFile: string; ACanCreate: Boolean): Boolean;
    procedure InitData(const AStorage: string = '');
    procedure StartUp;
    procedure ShutDown;
    procedure RegisterClasses(const AClasses: array of TClass);
    procedure Go(const AFormClass: TFormClass; const ACtlClasses: array of TControlClass);
  public
    class procedure Run(const AFormClass: TFormClass;
      const AClasses: array of TClass;
      const ACtlClasses: array of TControlClass;
      AStorage: string = '');
    property SerialFactory: IRBFactory read GetSerialFactory;
    property DataStore: IRBStore read GetDataStore;
    property DataQuery: IRBDataQuery read GetDataQuery;
    property Designer: IRBDesigner read GetDesigner;
  end;

implementation

{ TStartForm }

function TStartForm.GetDesigner: IRBDesigner;
begin
  Result := fDesigner;
end;

function TStartForm.GetBinderContext: IRBBinderContext;
begin
  Result := Self;
end;

function TStartForm.GetDataStore: IRBStore;
begin
  Result := fDataStore;
end;

function TStartForm.GetSerialFactory: IRBFactory;
begin
  Result := fSerialFactory;
end;

function TStartForm.GetDataQuery: IRBDataQuery;
begin
  Result := fDataStore as IRBDataQuery;
end;

function TStartForm.PrepareHomeDir(const AHomeSubdir: string): string;
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
      raise EMain('Cannot create directory ' + Result);
  end;
end;

function TStartForm.OpenDataStore(const AFile: string; ACanCreate: Boolean): Boolean;
begin
  Result := False;
  if FileExists(AFile) or ACanCreate then
  begin
    fDataStore := TXmlStore.Create(fSerialFactory, AFile);
    Result := True;
  end;
end;

procedure TStartForm.InitData(const AStorage: string = '');
var
  mStorage: string;
begin
  fSerialFactory := TSerialFactory.Create;
  mStorage := AStorage;
  if mStorage = '' then
    mStorage := PrepareHomeDir('.' + ExtractFileName(ParamStr(0))) + 'data.xml';
  if not OpenDataStore(mStorage, True) then
    raise EMain.Create('Cannot oper or create ' + mStorage);
end;

procedure TStartForm.StartUp;
begin
end;

procedure TStartForm.ShutDown;
begin
  fDataStore.Flush;
end;

procedure TStartForm.RegisterClasses(const AClasses: array of TClass);
var
  i: integer;
begin
  for i := 0 to Length(AClasses) - 1 do
  begin
    fSerialFactory.RegisterClass(AClasses[i]);
  end;
end;

procedure TStartForm.Go(const AFormClass: TFormClass; const ACtlClasses: array of TControlClass);
begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(AFormClass, fMainF);
  fDesigner := TRBDesigner.Create;
  fDesigner.Bind(fMainF, fDataStore, fSerialFactory, DataQuery, {ACtlClasses}nil);
  if Supports(fMainF, IStartContextConnectable) then
    (fMainF as IStartContextConnectable).Connect(self);
  //Application.ShowMainForm := False;
  Application.Run;
end;

class procedure TStartForm.Run(const AFormClass: TFormClass;
  const AClasses: array of TClass;
  const ACtlClasses: array of TControlClass;
  AStorage: string = '');
var
  m: TStartForm;
begin
   m := TStartForm.Create(nil);
  try
    m.StartUp;
    try
      m.InitData(AStorage);
      m.RegisterClasses(AClasses);
      m.Go(AFormClass, ACtlClasses);
    finally
      m.ShutDown;
    end;
  finally
    m.Free;
  end;
end;

end.


