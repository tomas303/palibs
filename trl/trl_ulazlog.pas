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
    function AddTimeStamp(const s: string): string;
  protected
    // ILog
    procedure DebugLn(const s: string = ''); overload;
    procedure DebugLn(const s: String; Args: array of const); overload;
    procedure DebugLnEnter(const s: string = ''); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s: string = ''); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  public
    procedure AfterConstruction; override;
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

function TLazLog.AddTimeStamp(const s: string): string;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS:ZZZ', Now) + ' ' + s;
end;

procedure TLazLog.DebugLn(const s: string);
begin
  fLog.DebugLn(AddTimeStamp(s));
end;

procedure TLazLog.DebugLn(const s: String; Args: array of const);
begin
  fLog.DebugLn(AddTimeStamp(s), Args);
end;

procedure TLazLog.DebugLnEnter(const s: string);
begin
  fLog.DebugLnEnter(AddTimeStamp(s));
end;

procedure TLazLog.DebugLnEnter(s: string; Args: array of const);
begin
  fLog.DebugLnEnter(AddTimeStamp(s), Args);
end;

procedure TLazLog.DebugLnExit(const s: string);
begin
  fLog.DebugLnExit(AddTimeStamp(s));
end;

procedure TLazLog.DebugLnExit(s: string; Args: array of const);
begin
  fLog.DebugLnExit(AddTimeStamp(s), Args);
end;

function TLazLog.GetVisible: Boolean;
begin
  Result := True;
end;

procedure TLazLog.SetVisible(AValue: Boolean);
begin

end;

procedure TLazLog.AfterConstruction;
begin
  inherited AfterConstruction;
  fLog := TLazLoggerFile.Create;
end;

destructor TLazLog.Destroy;
begin
  FreeAndNil(fLog);
  inherited Destroy;
end;

end.

