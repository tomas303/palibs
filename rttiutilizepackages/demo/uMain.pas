unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, rtti_serializer_iManager, rtti_serializer_uXmlStore,
  rtti_serializer_uFactory, SysUtils,
  uPerson,
  //uIDECode,
  fMain, iMain, classes;

type

  { TMain }

  TMain = class(TComponent, IMainContext)
  private
    fSerialFactory: ISerialFactory;
    fDataStore: ISerialStore;
    //fIDE: TIDECode;
    fMainF: TMainForm;
  protected
    // IMainContext
    function GetDataStore: ISerialStore;
    function GetSerialFactory: ISerialFactory;
  protected
    procedure StartUp;
    procedure ShutDown;
    procedure Go;
  public
    class procedure Run;
    property SerialFactory: ISerialFactory read GetSerialFactory;
    property DataStore: ISerialStore read GetDataStore;
  end;

implementation

{ TMain }

function TMain.GetDataStore: ISerialStore;
begin
  Result := fDataStore;
end;

function TMain.GetSerialFactory: ISerialFactory;
begin
  Result := fSerialFactory;
end;

procedure TMain.StartUp;
begin
  fSerialFactory := TSerialFactory.Create;
  fDataStore := TXmlStore.Create(fSerialFactory, './store/demo.xml');
  fSerialFactory.RegisterClass(TPerson);
  //fIDE := TIDECode.Create;
  //fIDE.AttachStore(fDataStore);
  //fIDE.AttachFactory(fSerialFactory);
end;

procedure TMain.ShutDown;
begin
  fDataStore.Flush;
  //FreeAndNil(fIDE);
end;

procedure TMain.Go;
begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, fMainF);
  fMainF.AttachMainContext(self);
  //fIDE.AttachForm(fMainF);
  fMainF.Show;
  Application.Run;
end;

class procedure TMain.Run;
var
  m: TMain;
begin
  m := TMain.Create(nil);
  try
    m.StartUp;
    try
      m.Go;
    finally
      m.ShutDown;
    end;
  finally
    m.Free;
  end;
end;

end.

