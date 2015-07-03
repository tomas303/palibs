program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, fEdit, uPerson, uIDECode, uMain, fMain, iMain
  { you can add units after this };

{$R *.res}

begin
  TMain.Run;
end.

