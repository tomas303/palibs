program reatests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GuiTestRunner;

{$R *.res}

begin
  //trl_dicontainer_tests.RegisterTests;
  RequireDerivedFormResource := True;
  Application.Initialize;
  RunRegisteredTests;
end.

