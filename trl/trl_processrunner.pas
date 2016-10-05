unit trl_processrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, strutils, RegExpr;

type

  ECompileException = class(Exception)
  end;

  TCompileProcessState = (cpsNone, cpsPausing, cpsPaused, cpsResuming, cpsResumed, cpsTerminating, cpsTerminated);

  { TProcessRunnerSync }

  TProcessRunnerSync = class
  public type
    TDataKind = (dkOut, dkErr);
  public type
    TOnSyncEvent = procedure(const AData: string; const AKind: TDataKind) of object;
  private
    fData: string;
    fKind: TDataKind;
    fOnSync: TOnSyncEvent;
  public
    constructor Create(const AData: string; AKind: TDataKind;
      AOnSync: TOnSyncEvent);
    procedure Sync;
  end;

  { TProcessRunnerInfo }

  TProcessRunnerInfo = class
  private
    fBatch: string;
    fCommand: string;
    fWorkingDir: string;
    fParameters: TStringList;
    fEnvironment: TStringList;
    fPrefillCurentEnvironment: Boolean;
    function GetEnvironment: TStringList;
    function GetParameters: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AInfo: TProcessRunnerInfo);
    property Batch: string read fBatch write fBatch;
    property Command: string read fCommand write fCommand;
    property WorkingDir: string read fWorkingDir write fWorkingDir;
    property Parameters: TStringList read GetParameters;
    property Environment: TStringList read GetEnvironment;
    property PrefillCurentEnvironment: Boolean read fPrefillCurentEnvironment write fPrefillCurentEnvironment;
  end;

  { TProcessRunnerThread }

  TProcessRunnerThread = class(TThread)
  private
    fInfo: TProcessRunnerInfo;
    fProcess: TProcess;
    fOnSync: TProcessRunnerSync.TOnSyncEvent;
    fCompileState: TCompileProcessState;
  public
    fOutput: string;
  protected
    procedure AddOrReplaceEnvVariable(const AName, AValue: string);
    procedure FillCurrentEnvVariables(AEnvVariables: TStrings);
    procedure ExpandLine(AExpandVars, ASysVars: TStrings;
      const AIndex: integer; const AInProcess: TIntegerSet);
    procedure ExpandEnvVariables(AEnvVariables: TStrings);
    procedure FillEnvVariables(AEnvVariables: TStrings; APrefillCurentEnvironment: Boolean);
    function ExpandParamLine(const ALine: string; AEnvVariables: TStrings): string;
    procedure ExpandParameters(AParameters: TStrings);
    procedure FillParameters(AParameters: TStrings);
    procedure ProcessStdErr;
    procedure Execute; override;
  public
    constructor Create(AInfo: TProcessRunnerInfo; AOnSync: TProcessRunnerSync.TOnSyncEvent);
    destructor Destroy; override;
    procedure Start(AInfo: TProcessRunnerInfo);
    procedure PauseProcess;
    procedure ContinueProcess;
    procedure TerminateProcess;
    function IsRunning: Boolean;
    function IsPaused: Boolean;
  end;

  TProcessRunner = class
  public type
    TPushOutput = procedure(const AData: string) of object;
    TFinished = procedure(const AExitCode: integer) of object;
  private
    fCT: TProcessRunnerThread;
    fInfo: TProcessRunnerInfo;
    fOnPushOutput, fOnPushErrOutput: TPushOutput;
    fOnFinish: TFinished;
    fOutput, fErrOutput: string;
    fProcess: TProcess;
    procedure SyncOutput(const AData: string; const AKind: TProcessRunnerSync.TDataKind);
    procedure ProcessOutput(const AData: string);
    procedure ProcessErrOutput(const AData: string);
  protected
    function GetBatch: string;
    function GetCommand: string;
    function GetWorkingDir: string;
    function GetPrefillCurentEnvironment: Boolean;
    procedure SetBatch(AValue: string);
    procedure SetCommand(AValue: string);
    procedure SetWorkingDir(AValue: string);
    procedure SetPrefillCurentEnvironment(AValue: Boolean);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Launch(const AWorkingDir, ACommand, ABatch: string);
    procedure Start;
    procedure Pause;
    procedure Continue;
    procedure Terminate;
    function IsRunning: Boolean;
    function IsPaused: Boolean;
  public
    procedure ClearParameters;
    procedure AddParameter(const AParameter: string);
    procedure ClearEnvVariables;
    procedure AddEnvVariable(const AName, AValue: string);
    property WorkingDir: string read GetWorkingDir write SetWorkingDir;
    property Command: string read GetCommand write SetCommand;
    property Batch: string read GetBatch write SetBatch;
    property PrefillCurentEnvironment: Boolean read GetPrefillCurentEnvironment write SetPrefillCurentEnvironment;
  published
    property OnPushOutput: TPushOutput read fOnPushOutput write fOnPushOutput;
    property OnPushErrOutput: TPushOutput read fOnPushErrOutput write fOnPushErrOutput;
    property OnFinish: TFinished read fOnFinish write fOnFinish;
  end;

implementation

const
  cFinished = '46272787-A235-4598-9767-001E642750C2';
  cExitCode = 'exitcode';

{ TProcessRunnerInfo }

function TProcessRunnerInfo.GetEnvironment: TStringList;
begin
  Result := fEnvironment;
end;

function TProcessRunnerInfo.GetParameters: TStringList;
begin
  Result := fParameters;
end;

constructor TProcessRunnerInfo.Create;
begin
  fParameters := TStringList.Create;
  fEnvironment := TStringList.Create;
  {$IFDEF UNIX}
  fEnvironment.CaseSensitive := True;
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  fEnvironment.CaseSensitive := False;
  {$ENDIF UNIX}
end;

destructor TProcessRunnerInfo.Destroy;
begin
  FreeAndNil(fParameters);
  FreeAndNil(fEnvironment);
  inherited Destroy;
end;

procedure TProcessRunnerInfo.Assign(AInfo: TProcessRunnerInfo);
begin
  Batch := AInfo.Batch;
  Command := AInfo.Command;
  WorkingDir := AInfo.WorkingDir;
  Parameters.Assign(AInfo.Parameters);
  Environment.Assign(AInfo.Environment);
  PrefillCurentEnvironment := AInfo.PrefillCurentEnvironment;
end;

{ TProcessRunnerSync }

constructor TProcessRunnerSync.Create(const AData: string; AKind: TDataKind;
  AOnSync: TOnSyncEvent);
begin
  fData := AData;
  fKind := AKind;
  fOnSync := AOnSync;
end;

procedure TProcessRunnerSync.Sync;
begin
  fOnSync(fData, fKind);
  Free;
end;

{ TProcessRunner }

procedure TProcessRunner.SyncOutput(const AData: string; const AKind: TProcessRunnerSync.TDataKind);
begin
  case AKind of
    dkOut: ProcessOutput(AData);
    dkErr: ProcessErrOutput(AData);
  end;
end;

procedure TProcessRunner.ProcessOutput(const AData: string);
var
  mPos: integer;
  mLine: string;
  mFinishList: TStringList;
begin
  if not Assigned(OnPushOutput) then
    Exit;
  if AnsiStartsStr(cFinished, AData) then
  begin
    if fOutput <> '' then begin
      fOnPushOutput(fOutput);
      fOutput := '';
    end;
    mFinishList := TStringList.Create;
    try
      mFinishList.Text := AData;
      fOnFinish(StrToInt(mFinishList.Values[cExitCode]));
    finally
      mFinishList.Free;
    end;
  end
  else
  begin
    fOutput := fOutput + AData;
    mPos := Pos(#10, fOutput);
    while mPos > 0 do
    begin
     mLine := copy(fOutput, 1, mPos - 1);
     if (Length(mLine) > 0) and (mLine[Length(mLine)] = #13) then
       Delete(mLine, Length(mLine), 1);
     OnPushOutput(mLine);
     Delete(fOutput, 1, mPos);
     mPos := Pos(#10, fOutput);
    end;
  end;
end;

procedure TProcessRunner.ProcessErrOutput(const AData: string);
var
  mPos: integer;
  mLine: string;
  mFinishList: TStringList;
begin
  if not Assigned(OnPushErrOutput) then
    Exit;
  fErrOutput := fErrOutput + AData;
  mPos := Pos(#10, fErrOutput);
  while mPos > 0 do
  begin
    mLine := copy(fErrOutput, 1, mPos - 1);
    if (Length(mLine) > 0) and (mLine[Length(mLine)] = #13) then
      Delete(mLine, Length(mLine), 1);
    OnPushErrOutput(mLine);
    Delete(fErrOutput, 1, mPos);
    mPos := Pos(#10, fErrOutput);
  end;
end;

function TProcessRunner.GetPrefillCurentEnvironment: Boolean;
begin
  Result := fInfo.PrefillCurentEnvironment;
end;

procedure TProcessRunner.SetPrefillCurentEnvironment(AValue: Boolean);
begin
  fInfo.PrefillCurentEnvironment := AValue;
end;

function TProcessRunner.GetBatch: string;
begin
  Result := fInfo.Batch;
end;

function TProcessRunner.GetCommand: string;
begin
  Result := fInfo.Command;
end;

function TProcessRunner.GetWorkingDir: string;
begin
  Result := fInfo.WorkingDir;
end;

procedure TProcessRunner.SetBatch(AValue: string);
begin
  fInfo.Batch := AValue;
end;

procedure TProcessRunner.SetCommand(AValue: string);
begin
  fInfo.Command := AValue;
end;

procedure TProcessRunner.SetWorkingDir(AValue: string);
begin
  fInfo.WorkingDir := AValue;
end;

procedure TProcessRunner.AfterConstruction;
begin
  inherited AfterConstruction;
  fInfo := TProcessRunnerInfo.Create;
  //fCT := TProcessRunnerThread.Create(@SyncOutput);
end;

destructor TProcessRunner.Destroy;
begin
  //fCT.TerminateProcess;
  //fCT.WaitFor;
  //FreeAndNil(fCT);
  FreeAndNil(fInfo);
  inherited Destroy;
end;

procedure TProcessRunner.Launch(const AWorkingDir, ACommand, ABatch: string);
begin
  {
  if fCT.fCompileState <> cpsNone then
    raise ECompileException.Create('Compilation already runned.');
  if not fCT.Suspended then
    raise  ECompileException.Create('Compilation thread is not suspended.');
  fCT.fBatch := ABatch;
  fCT.fCommand := ACommand;
  fCT.fWorkingDir := AWorkingDir;
  fCT.Start;
  }
end;

procedure TProcessRunner.Start;
begin
  if fCT <> nil then begin
    if not (fCT.fCompileState in [cpsNone, cpsTerminated]) then
      raise ECompileException.Create('Cannot start - process is still running.');
    fCT.WaitFor;
    FreeAndNil(fCT);
  end;
  fCT := TProcessRunnerThread.Create(fInfo, @SyncOutput);
end;

procedure TProcessRunner.Pause;
begin
  fCT.PauseProcess;
end;

procedure TProcessRunner.Continue;
begin
  fCT.ContinueProcess;
end;

procedure TProcessRunner.Terminate;
begin
  fCT.TerminateProcess;
end;

function TProcessRunner.IsRunning: Boolean;
begin
  Result := (fCT <> nil) and fCT.IsRunning;
end;

function TProcessRunner.IsPaused: Boolean;
begin
  Result := (fCT <> nil) and fCT.IsPaused;
end;

procedure TProcessRunner.ClearParameters;
begin
  fInfo.Parameters.Clear;
end;

procedure TProcessRunner.AddParameter(const AParameter: string);
begin
  fInfo.Parameters.Add(AParameter);
end;

procedure TProcessRunner.ClearEnvVariables;
begin
  fInfo.Environment.Clear;
end;

procedure TProcessRunner.AddEnvVariable(const AName, AValue: string);
var
  mInd: integer;
begin
  mInd := fInfo.Environment.IndexOfName(AName);
  if mInd = -1 then
    fInfo.Environment.Add(AName + '=' + AValue)
  else
    fInfo.Environment[mInd] := AName + '=' + AValue;
end;

{ TProcessRunnerThread }

procedure TProcessRunnerThread.AddOrReplaceEnvVariable(const AName, AValue: string);
var
  mInd: integer;
begin
  mInd := fProcess.Environment.IndexOfName(AName);
  if mInd = -1 then
    fProcess.Environment.Add(AName + '=' + AValue)
  else
    fProcess.Environment[mInd] := AName + '=' + AValue;
end;

procedure TProcessRunnerThread.FillCurrentEnvVariables(AEnvVariables: TStrings);
var
  i: integer;
begin
  for i := 1 to GetEnvironmentVariableCount do
  begin
    AEnvVariables.Add(GetEnvironmentString(i));
  end;
end;

procedure TProcessRunnerThread.ExpandLine(AExpandVars, ASysVars: TStrings;
  const AIndex: integer; const AInProcess: TIntegerSet);
var
  mRegEx: TRegExpr;
  mRepIndex: integer;
  mMatch: string;
  mMatched: Boolean;
  mLine: string;
  m1,m2: string;
begin
  mRegEx := TRegExpr.Create;
  try
    {$IFDEF UNIX}
    mRegEx.Expression := '\$\(?(\w+)\)?';
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mRegEx.Expression := '%(\w+)%';
    {$ENDIF UNIX}
    mLine := AExpandVars[AIndex];
    mMatched := mRegEx.Exec(AExpandVars[AIndex]);
    while mMatched do begin
      mMatch := mRegEx.Match[1];
      m1 := mRegEx.Match[0];
      mRepIndex := AExpandVars.IndexOfName(mMatch);
      if (mRepIndex <> -1)  // found in user defined
         and (mRepIndex <> AIndex) // not self
         and not (mRepIndex in AInProcess) // and not in acutal process(against endless loop)
      then begin
        ExpandLine(AExpandVars, ASysVars, mRepIndex, AInProcess + [mRepIndex]);
        mLine := ReplaceStr(mLine, mRegEx.Match[0], AExpandVars.ValueFromIndex[mRepIndex]);
      end
      else
      begin
        mRepIndex := ASysVars.IndexOfName(mMatch);
        if mRepIndex <> -1 then
          mLine := ReplaceStr(mLine, mRegEx.Match[0], ASysVars.ValueFromIndex[mRepIndex]);
      end;
      mMatched := mRegEx.ExecNext;
    end;
    AExpandVars[AIndex] := mLine;
  finally
    mRegEx.Free;
  end;
end;

procedure TProcessRunnerThread.ExpandEnvVariables(AEnvVariables: TStrings);
var
  mSysVars: TStringList;
  i: integer;
begin
  mSysVars := TStringList.Create;
  try
    {$IFDEF UNIX}
    mSysVars.CaseSensitive := True;
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mSysVars.CaseSensitive := False;
    {$ENDIF UNIX}
    FillCurrentEnvVariables(mSysVars);
    for i := 0 to AEnvVariables.Count - 1 do begin
      ExpandLine(AEnvVariables, mSysVars, i, [i]);
    end;
  finally
    mSysVars.Free;
  end;
end;

procedure TProcessRunnerThread.FillEnvVariables(AEnvVariables: TStrings; APrefillCurentEnvironment: Boolean);
var
  i: integer;
begin
  ExpandEnvVariables(AEnvVariables);
  if APrefillCurentEnvironment then
    FillCurrentEnvVariables(fProcess.Environment);
  for i := 0 to AEnvVariables.Count - 1 do
  begin
    AddOrReplaceEnvVariable(AEnvVariables.Names[i], AEnvVariables.ValueFromIndex[i]);
  end;
end;

function TProcessRunnerThread.ExpandParamLine(const ALine: string;
  AEnvVariables: TStrings): string;
var
  mRegEx: TRegExpr;
  mRepIndex: integer;
  mMatch: string;
  mMatched: Boolean;
begin
  Result := ALine;
  mRegEx := TRegExpr.Create;
  try
    {$IFDEF UNIX}
    mRegEx.Expression := '\$\(?(\w+)\)?';
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mRegEx.Expression := '%(\w+)%';
    {$ENDIF UNIX}
    mMatched := mRegEx.Exec(ALine);
    while mMatched do begin
      mMatch := mRegEx.Match[1];
      mRepIndex := AEnvVariables.IndexOfName(mMatch);
      if (mRepIndex <> -1) then
        Result := ReplaceStr(Result, mRegEx.Match[0], AEnvVariables.ValueFromIndex[mRepIndex]);
      mMatched := mRegEx.ExecNext;
    end;
  finally
    mRegEx.Free;
  end;
end;

procedure TProcessRunnerThread.ExpandParameters(AParameters: TStrings);
var
  mEnv: TStringList;
  i: integer;
  mLine: string;
begin
  mEnv := TStringList.Create;
  try
    mEnv.Assign(fProcess.Environment);
    {$IFDEF UNIX}
    mEnv.CaseSensitive := True;
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mEnv.CaseSensitive := False;
    {$ENDIF UNIX}
    for i := 0 to AParameters.Count - 1 do begin
      mLine := ExpandParamLine(AParameters[i], mEnv);
      AParameters[i] := mLine;
    end;
  finally
    mEnv.Free;
  end;
end;

procedure TProcessRunnerThread.FillParameters(AParameters: TStrings);
begin
  fProcess.Parameters.Assign(AParameters);
  ExpandParameters(fProcess.Parameters);
end;

procedure TProcessRunnerThread.ProcessStdErr;
var
  mOutRead: integer;
  mData: string;
  mSyncOutput: TProcessRunnerSync;
begin
  while fProcess.Stderr.NumBytesAvailable > 0 do
  begin
    SetLength(mData, 500);
    mOutRead := fProcess.Stderr.Read(mData[1], 500);
    if mOutRead = 0 then
      Break;
    SetLength(mData, mOutRead);
    mSyncOutput := TProcessRunnerSync.Create(mData, dkErr, fOnSync);
    Queue(@mSyncOutput.Sync);
  end;
end;

procedure TProcessRunnerThread.Execute;
var
  mOutRead: integer;
  mData: string;
  mSyncOutput: TProcessRunnerSync;
  mFinishList: TStringList;
  mBatch: string;
  mExitCode: integer;
  mev: string;
begin
  mExitCode := 0;
  fCompileState := cpsNone;
  fProcess := TProcess.Create(nil);
  try
    try
      fProcess.CurrentDirectory := fInfo.WorkingDir;
      {$IFDEF windows}
      fProcess.Options := fProcess.Options + [poUsePipes, poNewConsole];
      fProcess.ShowWindow := swoHIDE;
      {$ELSE}
      fProcess.Options := fProcess.Options + [poUsePipes, poNoConsole];
      {$ENDIF}
      fProcess.Executable := fInfo.Command;
      FillEnvVariables(fInfo.Environment, fInfo.PrefillCurentEnvironment);
      FillParameters(fInfo.Parameters);
      mev := fProcess.Environment.Text;
      try
        fProcess.Execute;
        fCompileState := cpsResumed;
        if fInfo.Batch <> '' then begin
           mBatch := fInfo.Batch;
          fProcess.Input.Write(mBatch[1], Length(mBatch));
        end;
        fProcess.CloseInput;
        while True do
        begin
          //change of state
          case fCompileState of
             cpsPausing:
               begin
                 fProcess.Suspend;
                 fCompileState := cpsPaused;
               end;
             cpsResuming:
               begin
                 fProcess.Resume;
                 fCompileState := cpsResumed;
               end;
             cpsTerminating:
               begin
                 fProcess.Terminate(-1);
                 fCompileState := cpsTerminated;
               end;
          end;
          //process
          case fCompileState of
            cpsResumed:
              begin
                ProcessStdErr;
                SetLength(mData, 500);
                mOutRead := fProcess.Output.Read(mData[1], 500);
                if (mOutRead = 0) and not fProcess.Active then
                begin
                  fCompileState := cpsTerminated;
                  Break;
                end;
                SetLength(mData, mOutRead);
                mSyncOutput := TProcessRunnerSync.Create(mData, dkOut, fOnSync);
                Queue(@mSyncOutput.Sync);
              end;
            cpsTerminated:
             begin
               Break;
             end;
            cpsPaused:
             begin
               Sleep(500);
             end;
           end;
        end;
      finally
        if fProcess.Active then begin
          fProcess.Terminate(0);
        end;
        fCompileState := cpsTerminated;
      end;
      mExitCode := fProcess.ExitCode;
    except
      on E: Exception do begin
        mExitCode := -11111;
        mData := E.ClassName + ' ' + E.Message;
        mSyncOutput := TProcessRunnerSync.Create(mData, dkErr, fOnSync);
        Queue(@mSyncOutput.Sync);
      end;
    end;
    ProcessStdErr;
    mFinishList := TStringList.Create;
    try
      mFinishList.Add(cFinished);
      mFinishList.Add(cExitCode + '=' + IntToStr(mExitCode));
      mSyncOutput := TProcessRunnerSync.Create(mFinishList.Text, dkOut, fOnSync);
    finally
      mFinishList.Free;
    end;
    Queue(@mSyncOutput.Sync);
  finally
    FreeAndNil(fProcess);
  end;
end;

constructor TProcessRunnerThread.Create(AInfo: TProcessRunnerInfo;
  AOnSync: TProcessRunnerSync.TOnSyncEvent);
begin
  fOnSync := AOnSync;
  fInfo := TProcessRunnerInfo.Create;
  fInfo.Assign(AInfo);
  //FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TProcessRunnerThread.Destroy;
begin
  FreeAndNil(fInfo);
  inherited Destroy;
end;

procedure TProcessRunnerThread.Start(AInfo: TProcessRunnerInfo);
begin
  fInfo.Assign(AInfo);
  inherited Start;
end;

procedure TProcessRunnerThread.PauseProcess;
begin
  fCompileState := cpsPausing;
end;

procedure TProcessRunnerThread.ContinueProcess;
begin
  fCompileState := cpsResuming;
end;

procedure TProcessRunnerThread.TerminateProcess;
begin
  fCompileState := cpsTerminating;
end;

function TProcessRunnerThread.IsRunning: Boolean;
begin
  Result := fCompileState in [cpsNone, cpsResumed];
end;

function TProcessRunnerThread.IsPaused: Boolean;
begin
  Result := fCompileState in [cpsPaused];
end;

end.
