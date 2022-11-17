unit trl_ipubsub;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}

interface

type

  TPubSubChannelCallback = reference to procedure;
  TPubSubChannelDataCallback<T> = reference to procedure(const AData: T);

  IPubSubChannel = interface
  ['{F5B8029B-095E-4E87-A621-C6C96D8177DE}']
    procedure Publish;
    procedure Subscribe(const ACallback: TPubSubChannelCallback);
  end;

  IPubSubDataChannel<T> = interface
  ['{6C3DDC7D-A867-4DAE-8B92-D74C8F878E3F}']
    procedure Publish(const AData: T);
    procedure Subscribe(const ACallback: TPubSubChannelDataCallback<T>);
  end;

  IPubSubChannelExec = interface
  ['{AA91767B-554F-4678-8250-9FF1570F237B}']
    procedure ExecEvent;
  end;

  TPubSubEventPublishedCallback = procedure(const AChannel: IPubSubChannelExec) of object;

  IPubSub = interface
  ['{61F589A6-B483-47BB-8221-3A92B5EDD2C2}']
    function NewChannel: IPubSubChannel;
    function NewDataChannel<T>: IPubSubDataChannel<T>;
    procedure ExecEvents;
  end;

implementation

end.

