unit trl_uexecutor;

{$mode objfpc}{$H+}

interface

uses
  trl_iExecutor, fgl, sysutils, trl_ilog, classes;

type

  { TExecutor }

  TExecutor = class(TInterfacedObject, IExecutor)
  protected type

    { TItems }

    TItems = class(specialize TFPGInterfacedObjectList<IExecute>)
    protected
      procedure Deref(Item: Pointer); override;
    end;

  protected
    fItems: TItems;
  protected
    //IExecutor
    procedure Add(const AExecute: IExecute);
    procedure ExecuteAll;
    procedure ExecuteLoop;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  protected
    fLog: ILog;
  published
    property Log: ILog read fLog write fLog;
  end;

implementation

{ TExecutor.TItems }

procedure TExecutor.TItems.Deref(Item: Pointer);
begin
  // because of problem in function  Get(is derefering by cast T(...))
  //temporarirly solve by this
  inherited Deref(Item);
end;

{ TExecutor }

procedure TExecutor.Add(const AExecute: IExecute);
begin
  fItems.Add(AExecute);
end;

procedure TExecutor.ExecuteAll;
var
  mExecutable: IExecute;
  i, mCount: integer;
  mo: TInterfacedObject;
  m1,m2: integer;
begin
  //Log.DebugLnEnter({$I %CURRENTROUTINE%});
  mCount := fItems.Count;
  for i := 0 to mCount - 1 do
  begin
    mExecutable := fItems[0];
    mo := mExecutable as TInterfacedObject;
    m1:=mo.RefCount;
    fItems.Delete(0);
    m2:=mo.RefCount;
    try
      mExecutable.Execute;
    except
      on E: Exception do begin
        Log.DebugLn('%s: %s', [E.ClassName, E.Message]);
      end;
    end;
  end;
  //Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TExecutor.ExecuteLoop;
var
  mExecutable: IExecute;
  mStop: Boolean;
begin
  mStop := False;
  repeat
    if fItems.Count > 0 then begin
      mExecutable := fItems[0];
      fItems.Delete(0);
      try
        mExecutable.Execute;
      except
        on E: EExecutorStop do begin
          mStop := True;
        end;
        on E: Exception do begin
          Log.DebugLn('%s: %s', [E.ClassName, E.Message]);
        end;
      end;
    end;
  until mStop;
end;

procedure TExecutor.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TItems.Create;
end;

destructor TExecutor.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

end.

