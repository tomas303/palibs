program trltests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GuiTestRunner, trl_dicontainer_tests, trl_umetaelement_tests,
  trl_umetaelementfactory_tests, trl_uinterfacedownedobject_tests;

{$R *.res}

begin
  trl_dicontainer_tests.RegisterTests;
  trl_uinterfacedownedobject_tests.RegisterTests;
  trl_umetaelement_tests.RegisterTests;
  trl_umetaelementfactory_tests.RegisterTests;
  RequireDerivedFormResource := True;
  Application.Initialize;
  RunRegisteredTests;
end.

