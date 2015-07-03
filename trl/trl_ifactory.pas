unit trl_ifactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 { IFactory }

  IFactory = interface
  ['{231BD5BD-DB2B-49DD-9A19-BB9788475F37}']
    procedure RegisterClass(const AClass: TClass);
    function CreateObject(const AClass: string): TObject;
  end;

implementation

end.

