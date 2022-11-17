unit trl_upubsub;

{$mode delphi}{$H+}

interface

uses
  trl_ipubsub, fgl, sysutils, trl_ilog;

type

  { TPubSubChannel }

  TPubSubChannel = class(TInterfacedObject, IPubSubChannel, IPubSubChannelExec)
  strict private
    fPublishedCallback: TPubSubEventPublishedCallback;
    procedure ExecEvent;
  strict private
    fObservers: TFPGList<TPubSubChannelCallback>;
    procedure Publish;
    procedure Subscribe(const ACallback: TPubSubChannelCallback);
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
    fObservers: TFPGList<TCallback>;
    procedure Publish(const AData: T);
    procedure Subscribe(const ACallback: TCallback);
  public
    constructor Create(const APublishedCallback: TPubSubEventPublishedCallback);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TPubSub = class(TInterfacedObject, IPubSub)
  strict private
    fEvents: TFPGInterfacedObjectList<IPubSubChannelExec>;
    procedure EventPublished(const AChannel: IPubSubChannelExec);
  strict private
    fChannels: TFPGInterfacedObjectList<IPubSubChannelExec>;
    function NewChannel: IPubSubChannel;
    function NewDataChannel<T>: IPubSubDataChannel<T>;
    procedure ExecEvents;
  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fLog: ILog;
  published
    property Log: ILog read fLog write fLog;
  end;

implementation

{ TPubSubDataChannel }

procedure TPubSubDataChannel<T>.ExecEvent;
var
  cb: TCallback;
begin
  for cb in fObservers do cb(fEvents[0]);
  fEvents.Delete(0);
end;

procedure TPubSubDataChannel<T>.Publish(const AData: T);
begin
  fEvents.Add(AData);
  fPublishedCallback(Self);
end;

procedure TPubSubDataChannel<T>.Subscribe(
  const ACallback: TCallback);
begin
  fObservers.Add(ACallback);
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
  fObservers := TFPGList<TCallback>.Create;
end;

procedure TPubSubDataChannel<T>.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  FreeAndNil(fObservers);
  inherited BeforeDestruction;
end;

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

procedure TPubSub.ExecEvents;
var
  ChannelExec: IPubSubChannelExec;
begin
  try
    for ChannelExec in fEvents do begin
      try
        ChannelExec.ExecEvent;
      except
        on E: Exception do begin
          Log.DebugLn('pubsub error when executing event - %s: %s', [E.ClassName, E.Message]);
        end;
      end;
    end;
  finally
    fEvents.Clear;
  end;
end;

constructor TPubSub.Create;
begin
  inherited Create;
  ExecEvents;
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
  fObservers.Add(ACallback);
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
  fObservers := TFPGList<TPubSubChannelCallback>.Create;
end;

procedure TPubSubChannel.BeforeDestruction;
begin
  FreeAndNil(fObservers);
  inherited BeforeDestruction;
end;

end.

