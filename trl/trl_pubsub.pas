unit trl_pubsub;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}

interface

uses
  fgl, trl_ilog, sysutils, Generics.Collections;

type

  EPubSubUnhandled = class(Exception);

  TPubSubChannelCallback = procedure of object;
  TPubSubChannelDataCallback<T> = procedure(const AData: T) of object;

  IPubSubChannel = interface
  ['{F5B8029B-095E-4E87-A621-C6C96D8177DE}']
    procedure Publish;
    procedure Subscribe(const ACallback: TPubSubChannelCallback);
    procedure Unsubscribe(const ACallback: TPubSubChannelCallback);
  end;

  IPubSubDataChannel<T> = interface
  ['{6C3DDC7D-A867-4DAE-8B92-D74C8F878E3F}']
    procedure Publish(const AData: T);
    procedure Subscribe(const ACallback: TPubSubChannelDataCallback<T>);
    procedure Unsubscribe(const ACallback: TPubSubChannelDataCallback<T>);
  end;

  IPubSubChannelExec = interface
  ['{AA91767B-554F-4678-8250-9FF1570F237B}']
    procedure ExecEvent;
  end;

  TPubSub = class;

  IPubSub = interface
  ['{61F589A6-B483-47BB-8221-3A92B5EDD2C2}']
    function Factory: TPubSub;
    procedure ExecEvent;
  end;

  { TPubSub }

  TPubSub = class(TInterfacedObject, IPubSub)
  strict private
    fEvents: TFPGInterfacedObjectList<IPubSubChannelExec>;
    fChannels: TFPGInterfacedObjectList<IPubSubChannelExec>;
    procedure EventPublished(const AChannel: IPubSubChannelExec);
    procedure ExecEvent;
    function Factory: TPubSub;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function NewChannel: IPubSubChannel;
    function NewDataChannel<T>: IPubSubDataChannel<T>;
  protected
    fLog: ILog;
  published
    property Log: ILog read fLog write fLog;
  end;

  TPubSubEventPublishedCallback = procedure(const AChannel: IPubSubChannelExec) of object;

  { TPubSubChannel }

  TPubSubChannel = class(TInterfacedObject, IPubSubChannel, IPubSubChannelExec)
  strict private
    fPublishedCallback: TPubSubEventPublishedCallback;
    procedure ExecEvent;
  strict private
    fObservers: TList<TPubSubChannelCallback>;
    procedure Publish;
    procedure Subscribe(const ACallback: TPubSubChannelCallback);
    procedure Unsubscribe(const ACallback: TPubSubChannelCallback);
  public
    constructor Create(const APublishedCallback: TPubSubEventPublishedCallback);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TPubSubDataChannel }

  TPubSubDataChannel<T> = class(TInterfacedObject, IPubSubDataChannel<T>, IPubSubChannelExec)
  strict private type
    TCallback = TPubSubChannelDataCallback<T>;
  strict private
    fEvents: TFPGList<T>;
    fPublishedCallback: TPubSubEventPublishedCallback;
    procedure ExecEvent;
  strict private
    fObservers: TList<TCallback>;
    procedure Publish(const AData: T);
    procedure Subscribe(const ACallback: TCallback);
    procedure Unsubscribe(const ACallback: TPubSubChannelDataCallback<T>);
  public
    constructor Create(const APublishedCallback: TPubSubEventPublishedCallback);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TPubSubDataChannel }

procedure TPubSubDataChannel<T>.ExecEvent;
var
  cb: TCallback;
begin
  try
    for cb in fObservers do cb(fEvents[0]);
  finally
    fEvents.Delete(0);
  end;
end;

procedure TPubSubDataChannel<T>.Publish(const AData: T);
begin
  fEvents.Add(AData);
  fPublishedCallback(Self);
end;

procedure TPubSubDataChannel<T>.Subscribe(
  const ACallback: TCallback);
begin
  if fObservers.IndexOf(ACallback) = -1 then
    fObservers.Add(ACallback);
end;

procedure TPubSubDataChannel<T>.Unsubscribe(
  const ACallback: TPubSubChannelDataCallback<T>);
begin
  if fObservers.IndexOf(ACallback) <> -1 then
    fObservers.Remove(ACallback);
end;

constructor TPubSubDataChannel<T>.Create(
  const APublishedCallback: TPubSubEventPublishedCallback);
begin
  inherited Create;
  fPublishedCallback := APublishedCallback;
end;

procedure TPubSubDataChannel<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fEvents := TFPGList<T>.Create;
  fObservers := TList<TCallback>.Create;
end;

procedure TPubSubDataChannel<T>.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  FreeAndNil(fObservers);
  inherited BeforeDestruction;
end;

{ TPubSubChannel }

procedure TPubSubChannel.ExecEvent;
var
  cb: TPubSubChannelCallback;
begin
  for cb in fObservers do cb();
end;

procedure TPubSubChannel.Publish;
begin
  fPublishedCallback(Self);
end;

procedure TPubSubChannel.Subscribe(const ACallback: TPubSubChannelCallback);
begin
  if fObservers.IndexOf(ACallback) = -1 then
    fObservers.Add(ACallback);
end;

procedure TPubSubChannel.Unsubscribe(const ACallback: TPubSubChannelCallback);
begin
  if fObservers.IndexOf(ACallback) <> -1 then
    fObservers.Remove(ACallback);
end;

constructor TPubSubChannel.Create(
  const APublishedCallback: TPubSubEventPublishedCallback);
begin
  inherited Create;
  fPublishedCallback := APublishedCallback;
end;

procedure TPubSubChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  fObservers := TList<TPubSubChannelCallback>.Create;
end;

procedure TPubSubChannel.BeforeDestruction;
begin
  FreeAndNil(fObservers);
  inherited BeforeDestruction;
end;

{ TPubSub }

procedure TPubSub.EventPublished(const AChannel: IPubSubChannelExec);
begin
  fEvents.Add(AChannel);
end;

function TPubSub.NewChannel: IPubSubChannel;
begin
  Result := TPubSubChannel.Create(EventPublished);
  fChannels.Add(Result as IPubSubChannelExec);
end;

function TPubSub.NewDataChannel<T>: IPubSubDataChannel<T>;
begin
  Result := TPubSubDataChannel<T>.Create(EventPublished);
  fChannels.Add(Result as IPubSubChannelExec);
end;

procedure TPubSub.ExecEvent;
var
  chExec: IPubSubChannelExec;
begin
  if fEvents.Count > 0 then begin
    chExec := fEvents[0];
    fEvents.Delete(0);
    try
      chExec.ExecEvent;
    except
      on E: EPubSubUnhandled do begin
        raise;
      end;
      on E: Exception do begin
        Log.DebugLn('pubsub error when executing event - %s: %s', [E.ClassName, E.Message]);
      end;
    end;
  end;
end;

function TPubSub.Factory: TPubSub;
begin
  Result := Self;
end;

procedure TPubSub.AfterConstruction;
begin
  inherited AfterConstruction;
  fChannels := TFPGInterfacedObjectList<IPubSubChannelExec>.Create;
  fEvents := TFPGInterfacedObjectList<IPubSubChannelExec>.Create;
end;

procedure TPubSub.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  FreeAndNil(fChannels);
  inherited BeforeDestruction;
end;

end.

