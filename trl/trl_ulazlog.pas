unit trl_ulazlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_ilog, LazLogger;

type

  { TLazLog }

  TLazLog = class(TInterfacedObject, ILog)
  protected
    fLog: TLazLoggerFile;
    function GetCloseLogFileBetweenWrites: Boolean;
    function GetLogName: String;
    function GetUseStdOut: Boolean;
    procedure SetCloseLogFileBetweenWrites(AValue: Boolean);
    procedure SetLogName(AValue: String);
    procedure SetUseStdOut(AValue: Boolean);
  protected
    // ILog
    procedure DebugLn(const s: string = ''); overload;
    procedure DebugLn(const S: String; Args: array of const); overload;
    procedure DebugLnEnter(const s: string = ''); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s: string = ''); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property  LogName: String read GetLogName write SetLogName;
    property  UseStdOut: Boolean read GetUseStdOut write SetUseStdOut;
    property  CloseLogFileBetweenWrites: Boolean read GetCloseLogFileBetweenWrites write SetCloseLogFileBetweenWrites;
  end;

implementation

{ TLazLog }

function TLazLog.GetCloseLogFileBetweenWrites: Boolean;
begin
  Result := fLog.CloseLogFileBetweenWrites;
end;

function TLazLog.GetLogName: String;
begin
  Result := fLog.LogName;
end;

function TLazLog.GetUseStdOut: Boolean;
begin
  Result := fLog.UseStdOut;
end;

procedure TLazLog.SetCloseLogFileBetweenWrites(AValue: Boolean);
begin
  fLog.CloseLogFileBetweenWrites := AValue;
end;

procedure TLazLog.SetLogName(AValue: String);
begin
  fLog.LogName := AValue;
end;

procedure TLazLog.SetUseStdOut(AValue: Boolean);
begin
  fLog.UseStdOut := AValue;
end;

procedure TLazLog.DebugLn(const s: string);
begin
  fLog.DebugLn(s);
end;

procedure TLazLog.DebugLn(const S: String; Args: array of const);
begin
  fLog.DebugLn(S, Args);
end;

procedure TLazLog.DebugLnEnter(const s: string);
begin
  fLog.DebugLnEnter(s);
end;

procedure TLazLog.DebugLnEnter(s: string; Args: array of const);
begin
  fLog.DebugLnEnter(s, Args);
end;

procedure TLazLog.DebugLnExit(const s: string);
begin
  fLog.DebugLnExit(s);
end;

procedure TLazLog.DebugLnExit(s: string; Args: array of const);
begin
  fLog.DebugLnExit(s, Args);
end;

constructor TLazLog.Create;
begin
  fLog := TLazLoggerFile.Create;
end;

destructor TLazLog.Destroy;
begin
  FreeAndNil(fLog);
  inherited Destroy;
end;

end.

