unit tvl_uapplaunch;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, tvl_iedit;

type

  { IGUILaunched }

  IGUIKicker = interface
  ['{24C8F00C-52FA-403A-B07F-63C9BE78B747}']
    procedure StartUp;
    procedure ShutDown;
    function GetMainForm: IMainForm;
    property MainForm: IMainForm read GetMainForm;
  end;

  { TGUILauncher }

  TGUILauncher = class
  private
    fKicker: IGUIKicker;
  protected
    procedure CloseMainFormHandler(Sender: TObject; var CloseAction: TCloseAction);
  public
    procedure Launch;
    procedure AfterConstruction; override;
  published
    property Kicker: IGUIKicker read fKicker write fKicker;
  end;

implementation

{ TGUILauncher }

procedure TGUILauncher.CloseMainFormHandler(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if CloseAction in [caHide, caFree] then
  begin
    Kicker.ShutDown;
  end;
end;

procedure TGUILauncher.Launch;
begin
  Kicker.MainForm.ConnectCloseHandler(CloseMainFormHandler);
  Kicker.StartUp;
  Application.Run;
end;

procedure TGUILauncher.AfterConstruction;
begin
  // since mainform could be subject of dependency injection and as such could
  // be created during create of hierarchy of object, call to inititialization
  // is put here(at least on windows must be called before main form is created)
  Application.Initialize;
end;

end.

