unit trl_pubsub;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  fgl, trl_ilog, sysutils, Generics.Collections;

type

  EPubSubUnhandled = class(Exception);
  EPubSubBridgeNoWay = class(Exception);

  TPubSubChannelCallback = procedure of object;
  TPubSubChannelDataCallback<T> = procedure(const AData: T) of object;

  TPubSubDataConversion<T, S> = reference to function(const AData: T): S;
  TPubSubNewData<T> = reference to function: T;

  IPubSubChannel = interface
  ['{F5B8029B-095E-4E87-A621-C6C96D8177DE}']
    procedure Publish;
    procedure Debounce;
    procedure Subscribe(const ACallback: TPubSubChannelCallback);
    procedure Unsubscribe(const ACallback: TPubSubChannelCallback);
  end;

  IPubSubDataChannel<T> = interface
  ['{6C3DDC7D-A867-4DAE-8B92-D74C8F878E3F}']
    procedure Publish(const AData: T);
    procedure Debounce(const AData: T; const Group: String = '');
    procedure Subscribe(const ACallback: TPubSubChannelDataCallback<T>);
    procedure Unsubscribe(const ACallback: TPubSubChannelDataCallback<T>);
  end;

  IPubSubChannelExec = interface
  ['{AA91767B-554F-4678-8250-9FF1570F237B}']
    procedure ExecEvent;
    procedure PublishDebounced;
  end;

  IPubSubBridge = interface
  ['{DAE9973A-F201-4AFC-A79B-47569F273B59}']
  end;

  TPubSub = class;

  IPubSub = interface
  ['{61F589A6-B483-47BB-8221-3A92B5EDD2C2}']
    function Factory: TPubSub;
    procedure ExecEvent;
    procedure PublishDebounced;
    function IsEmpty: Boolean;
  end;

  { TPubSub }

  TPubSub = class(TInterfacedObject, IPubSub)
  strict private
    fEvents: TFPGInterfacedObjectList<IPubSubChannelExec>;
    fChannels: TFPGInterfacedObjectList<IPubSubChannelExec>;
    fBridges: TFPGInterfacedObjectList<IPubSubBridge>;
    procedure EventPublished(const AChannel: IPubSubChannelExec);
    procedure ExecEvent;
    procedure PublishDebounced;
    function Factory: TPubSub;
    function IsEmpty: Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function NewChannel: IPubSubChannel;
    function NewDataChannel<T>: IPubSubDataChannel<T>;
    procedure NewDataBridge<T, S>(
      const AInChannel: IPubSubDataChannel<T>;
      const AOutChannel: IPubSubDataChannel<S>;
      AConversion: TPubSubDataConversion<T, S>);
    procedure NewDuplexDataBridge<T, S>(
      const AInChannel: IPubSubDataChannel<T>;
      const AOutChannel: IPubSubDataChannel<S>;
      AInToOutConversion: TPubSubDataConversion<T, S>;
      AOutToInConversion: TPubSubDataConversion<S, T>);
    procedure NewBridge(const AInChannel, AOutChannel: IPubSubChannel);
    procedure NewDuplexBridge(const AInChannel, AOutChannel: IPubSubChannel);
    procedure NewNonDataToDataBridge<T>(
      const AInChannel: IPubSubChannel;
      const AOutChannel: IPubSubDataChannel<T>;
      ANewData: TPubSubNewData<T>);
    procedure NewDataToNonDataBridge<T>(
      const AInChannel: IPubSubDataChannel<T>;
      const AOutChannel: IPubSubChannel);
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
    fDebouncer: Boolean;
    procedure ExecEvent;
  strict private
    fObservers: TList<TPubSubChannelCallback>;
    procedure Publish;
    procedure Debounce;
    procedure PublishDebounced;
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
    fDebouncer: TFPGMap<String, T>;
    fPublishedCallback: TPubSubEventPublishedCallback;
    procedure ExecEvent;
    procedure PublishDebounced;
  strict private
    fObservers: TList<TCallback>;
    procedure Publish(const AData: T);
    procedure Debounce(const AData: T; const Group: String = '');
    procedure Subscribe(const ACallback: TCallback);
    procedure Unsubscribe(const ACallback: TPubSubChannelDataCallback<T>);
  public
    constructor Create(const APublishedCallback: TPubSubEventPublishedCallback);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TPubSubDataBridge }

  TPubSubDataBridge<T, S> = class(TInterfacedObject, IPubSubBridge)
  private
    fInChannel: IPubSubDataChannel<T>;
    fOutChannel: IPubSubDataChannel<S>;
    fConversion: TPubSubDataConversion<T, S>;
    procedure InObserver(const AData: T);
  public
    constructor Create(const AInChannel: IPubSubDataChannel<T>; const AOutChannel: IPubSubDataChannel<S>;
      AConversion: TPubSubDataConversion<T, S>);
    destructor Destroy; override;
  end;

  { TPubSubBridge }

  TPubSubBridge = class(TInterfacedObject, IPubSubBridge)
  private
    fInChannel: IPubSubChannel;
    fOutChannel: IPubSubChannel;
    procedure InObserver;
  public
    constructor Create(const AInChannel: IPubSubChannel; const AOutChannel: IPubSubChannel);
    destructor Destroy; override;
  end;

  { TPubSubNonDataToDataBridge }

  TPubSubNonDataToDataBridge<T> = class(TInterfacedObject, IPubSubBridge)
  private
    fInChannel: IPubSubChannel;
    fOutChannel: IPubSubDataChannel<T>;
    fNewData: TPubSubNewData<T>;
    procedure InObserver;
  public
    constructor Create(
      const AInChannel: IPubSubChannel;
      const AOutChannel: IPubSubDataChannel<T>;
      ANewData: TPubSubNewData<T>);
    destructor Destroy; override;
  end;

  { TPubSubDataToNonDataBridge }

  TPubSubDataToNonDataBridge<T> = class(TInterfacedObject, IPubSubBridge)
  private
    fInChannel: IPubSubDataChannel<T>;
    fOutChannel: IPubSubChannel;
    procedure InObserver(const AData: T);
  public
    constructor Create(
      const AInChannel: IPubSubDataChannel<T>;
      const AOutChannel: IPubSubChannel);
    destructor Destroy; override;
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

procedure TPubSubDataChannel<T>.PublishDebounced;
var
  i: integer;
begin
  for i := 0 to fDebouncer.Count - 1 do
    Publish(fDebouncer.Data[i]);
  fDebouncer.Clear;
end;

procedure TPubSubDataChannel<T>.Publish(const AData: T);
begin
  fEvents.Add(AData);
  fPublishedCallback(Self);
end;

procedure TPubSubDataChannel<T>.Debounce(const AData: T; const Group: String);
begin
  fDebouncer.AddOrSetData(Group, AData);
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
  fDebouncer := TFPGMap<String, T>.Create;
end;

procedure TPubSubDataChannel<T>.BeforeDestruction;
begin
  FreeAndNil(fEvents);
  FreeAndNil(fObservers);
  FreeAndNil(fDebouncer);
  inherited BeforeDestruction;
end;

{ TPubSubDataBridge }

procedure TPubSubDataBridge<T, S>.InObserver(const AData: T);
var
  mData: S;
begin
  mData := fConversion(AData);
  fOutChannel.Publish(mData);
end;

constructor TPubSubDataBridge<T, S>.Create(
  const AInChannel: IPubSubDataChannel<T>;
  const AOutChannel: IPubSubDataChannel<S>;
  AConversion: TPubSubDataConversion<T, S>);
begin
  fInChannel := AInChannel;
  fOutChannel := AOutChannel;
  fConversion := AConversion;
  fInChannel.Subscribe(InObserver);
end;

destructor TPubSubDataBridge<T, S>.Destroy;
begin
  fInChannel.Unsubscribe(InObserver);
  inherited Destroy;
end;

{ TPubSubNonDataToDataBridge }

procedure TPubSubNonDataToDataBridge<T>.InObserver;
begin
  fOutChannel.Publish(fNewData());
end;

constructor TPubSubNonDataToDataBridge<T>.Create(
  const AInChannel: IPubSubChannel;
  const AOutChannel: IPubSubDataChannel<T>;
  ANewData: TPubSubNewData<T>);
begin
  inherited Create;
  fInChannel := AInChannel;
  fOutChannel := AOutChannel;
  fNewData := ANewData;
  fInChannel.Subscribe(InObserver);
end;

destructor TPubSubNonDataToDataBridge<T>.Destroy;
begin
  fInChannel.Unsubscribe(InObserver);
  inherited Destroy;
end;

{ TPubSubDataToNonDataBridge }

procedure TPubSubDataToNonDataBridge<T>.InObserver(const AData: T);
begin
  fOutChannel.Publish;
end;

constructor TPubSubDataToNonDataBridge<T>.Create(
  const AInChannel: IPubSubDataChannel<T>;
  const AOutChannel: IPubSubChannel);
begin
  inherited Create;
  fInChannel := AInChannel;
  fOutChannel := AOutChannel;
  fInChannel.Subscribe(InObserver);
end;

destructor TPubSubDataToNonDataBridge<T>.Destroy;
begin
  fInChannel.Unsubscribe(InObserver);
  inherited Destroy;
end;

{ TPubSubBridge }

procedure TPubSubBridge.InObserver;
begin
  fOutChannel.Publish;
end;

constructor TPubSubBridge.Create(
  const AInChannel: IPubSubChannel;
  const AOutChannel: IPubSubChannel);
begin
  fInChannel := AInChannel;
  fOutChannel := AOutChannel;
  fInChannel.Subscribe(InObserver);
end;

destructor TPubSubBridge.Destroy;
begin
  fInChannel.Unsubscribe(InObserver);
  inherited Destroy;
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

procedure TPubSubChannel.Debounce;
begin
  fDebouncer := True;
end;

procedure TPubSubChannel.PublishDebounced;
begin
  if fDebouncer then begin
    Publish;
    fDebouncer := False;
  end;
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

procedure TPubSub.NewDuplexDataBridge<T, S>(
  const AInChannel: IPubSubDataChannel<T>;
  const AOutChannel: IPubSubDataChannel<S>;
  AInToOutConversion: TPubSubDataConversion<T, S>;
  AOutToInConversion: TPubSubDataConversion<S, T>);
begin
  NewDataBridge<T, S>(AInChannel, AOutChannel, AInToOutConversion);
  NewDataBridge<S, T>(AOutChannel, AInChannel, AOutToInConversion);
end;

procedure TPubSub.NewDataBridge<T, S>(
  const AInChannel: IPubSubDataChannel<T>;
  const AOutChannel: IPubSubDataChannel<S>;
  AConversion: TPubSubDataConversion<T, S>);
begin
  fBridges.Add(TPubSubDataBridge<T, S>.Create(AInChannel, AOutChannel, AConversion));
end;

procedure TPubSub.NewBridge(const AInChannel, AOutChannel: IPubSubChannel);
begin
  fBridges.Add(TPubSubBridge.Create(AInChannel, AOutChannel));
end;

procedure TPubSub.NewDuplexBridge(const AInChannel, AOutChannel: IPubSubChannel);
begin
  NewBridge(AInChannel, AOutChannel);
  NewBridge(AOutChannel, AInChannel)
end;

procedure TPubSub.NewNonDataToDataBridge<T>(const AInChannel: IPubSubChannel;
  const AOutChannel: IPubSubDataChannel<T>; ANewData: TPubSubNewData<T>);
begin
  fBridges.Add(TPubSubNonDataToDataBridge<T>.Create(AInChannel, AOutChannel, ANewData));
end;

procedure TPubSub.NewDataToNonDataBridge<T>(const AInChannel: IPubSubDataChannel
  <T>; const AOutChannel: IPubSubChannel);
begin
  fBridges.Add(TPubSubDataToNonDataBridge<T>.Create(AInChannel, AOutChannel));
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

procedure TPubSub.PublishDebounced;
var
  chExec: IPubSubChannelExec;
begin
  for chExec in fChannels do
   chExec.PublishDebounced;
end;

function TPubSub.Factory: TPubSub;
begin
  Result := Self;
end;

function TPubSub.IsEmpty: Boolean;
begin
  Result := fEvents.Count = 0;
end;

procedure TPubSub.AfterConstruction;
begin
  inherited AfterConstruction;
  fChannels := TFPGInterfacedObjectList<IPubSubChannelExec>.Create;
  fEvents := TFPGInterfacedObjectList<IPubSubChannelExec>.Create;
  fBridges := TFPGInterfacedObjectList<IPubSubBridge>.Create;
end;

procedure TPubSub.BeforeDestruction;
begin
  FreeAndNil(fBridges);
  FreeAndNil(fEvents);
  FreeAndNil(fChannels);
  inherited BeforeDestruction;
end;

end.

