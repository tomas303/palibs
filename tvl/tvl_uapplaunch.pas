unit tvl_uapplaunch;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TGUILauncher }

  TGUILauncher = class
  private
    fMainForm: TForm;
  protected
    procedure CloseMainFormHandler(Sender: TObject; var CloseAction: TCloseAction);
  public
    procedure Launch;
  published
    property MainForm: TForm read fMainForm write fMainForm;
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
  MainForm.AddHandlerClose(CloseMainFormHandler);
  MainForm.Visible := True;
  Application.Run;
end;

end.

