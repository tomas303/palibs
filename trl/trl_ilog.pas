unit trl_ilog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  ILog = interface
  ['{5F619DCC-AC09-4617-B9F5-7789E36FA39C}']
    procedure DebugLn(const s: string = ''); overload;
    procedure DebugLn(const S: String; Args: array of const); overload;
    procedure DebugLnEnter(const s: string = ''); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s: string = ''); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
  end;

implementation

end.

