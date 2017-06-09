unit trl_iinjector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_iprops;

type

  IInjector = interface
  ['{E30E4F4A-B1EA-41BC-950F-4BB828FCE441}']
    procedure Write(AInstance: TObject; const AProps: IProps);
    procedure Read(AInstance: TObject; const AProps: IProps);
  end;

implementation

end.

