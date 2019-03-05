unit trl_ilog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  ILogSupport = interface
  ['{BCAA7B90-8D8A-46CB-8F72-B1FB29A3D399}']
    function LogInfo: string;
  end;

  { ILog }

  ILog = interface
  ['{5F619DCC-AC09-4617-B9F5-7789E36FA39C}']
    procedure DebugLn(const s: string = ''); overload;
    procedure DebugLn(const S: String; Args: array of const); overload;
    procedure DebugLnEnter(const s: string = ''); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s: string = ''); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

implementation

end.

