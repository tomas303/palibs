unit rea_idata;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  trl_pubsub;

type

  { TFieldData }

  TFieldData = record
  strict private
    fName: String;
    fValue: String;
  public
    constructor Create(const AName, AValue: String);
    class operator equal(a,b: TFieldData): Boolean;
    public property Name: String read fName;
    public property Value: String read fValue;
  end;

  TCommandDataAction = (cdaFirst, cdaLast, cdaNext, cdaPrior);

  { TCommandData }

  TCommandData = record
  strict private
    fAction: TCommandDataAction;
  public
    constructor Create(const AAction: TCommandDataAction);
    class operator equal(a,b: TCommandData): Boolean;
    public property Action: TCommandDataAction read fAction;
  end;

  IPSFieldDataChannel = IPubSubDataChannel<TFieldData>;
  IPSCommandDataChannel = IPubSubDataChannel<TCommandData>;

  IDataConnector = interface
  ['{1653E0AC-C7FC-4773-A921-51DCA67080D9}']
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSCommandDataChannel: IPSCommandDataChannel;
  end;

implementation

{ TCommandData }

constructor TCommandData.Create(const AAction: TCommandDataAction);
begin
  fAction := AAction;
end;

class operator TCommandData.equal(a, b: TCommandData): Boolean;
begin
  Result := a.Action = b.Action;
end;

{ TFieldData }

constructor TFieldData.Create(const AName, AValue: String);
begin
  fName := AName;
  fValue := AValue;
end;

class operator TFieldData.equal(a, b: TFieldData): Boolean;
begin
  Result := (a.Name = b.Name) and (a.Value = b.Value);
end;

end.

