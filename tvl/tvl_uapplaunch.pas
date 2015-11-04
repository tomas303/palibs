unit tvl_uapplaunch;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { IGUILaunched }

  IGUIKicker = interface
  ['{24C8F00C-52FA-403A-B07F-63C9BE78B747}']
    procedure Start;
    function GetMainForm: TForm;
    procedure SetMainForm(AValue: TForm);
    property MainForm: TForm read GetMainForm write SetMainForm;
  end;

  { TGUILauncher }

  TGUILauncher = class
  private
    fKicker: IGUIKicker;
  protected
    procedure CloseMainFormHandler(Sender: TObject; var CloseAction: TCloseAction);
  public
    procedure Launch;
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
    Application.Terminate;
  end;
end;

procedure TGUILauncher.Launch;
begin
  Application.Initialize;
  Kicker.MainForm.AddHandlerClose(CloseMainFormHandler);
  Kicker.Start;
  Application.Run;
end;

end.

