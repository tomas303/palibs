unit rea_idata;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  trl_pubsub, rea_ibits, trl_funcp;

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

  IDataAccessor = interface
  ['{48A69436-153D-4760-8B77-F0FF3600F8E7}']
    function GetValue(const AName: String): String;
    property Value[const AName: String]: String read GetValue; default;
  end;

  { TRecordData }

  TRecordData = record
  strict private
    fPosition: Integer;
    fAccessor: IDataAccessor;
  public
    constructor Create(APosition: Integer; const AAccessor: IDataAccessor);
    class operator equal(a,b: TRecordData): Boolean;
    public property Position: Integer read fPosition;
    public property Accessor: IDataAccessor read fAccessor;
  end;

  TCommandDataAction = (cdaFirst, cdaLast, cdaMove, cdaInfo);

  { TCommandData }

  TCommandData = record
  strict private
    fAction: TCommandDataAction;
    fDelta: Integer;
    fFromPos: Integer;
    fToPos: Integer;
  public
    class function CreateFirst: TCommandData; static;
    class function CreateLast: TCommandData; static;
    class function CreateNext: TCommandData; static;
    class function CreatePrior: TCommandData; static;
    class function CreateMove(ADelta: Integer): TCommandData; static;
    class function CreateInfo(AFromPos, AToPos: Integer): TCommandData; static;
    class operator equal(a,b: TCommandData): Boolean;
    public property Action: TCommandDataAction read fAction;
    public property Delta: Integer read fDelta;
    public property FromPos: Integer read fFromPos;
    public property ToPos: Integer read fToPos;
  end;

  { TPositionChange }

  TPositionChange = record
  private
    fDelta: TOptional<Integer>;
  public
    class function New: TPositionChange; overload; static;
    class function New(ADelta: Integer): TPositionChange; overload; static;
    class operator Initialize(var a: TPositionChange);
    class operator equal(a,b: TPositionChange): Boolean;
    property Delta: TOptional<Integer> read fDelta;
  end;

  IPSFieldDataChannel = IPubSubDataChannel<TFieldData>;
  IPSRecordDataChannel = IPubSubDataChannel<TRecordData>;
  IPSCommandDataChannel = IPubSubDataChannel<TCommandData>;
  IPSPositionChangeChannel = IPubSubDataChannel<TPositionChange>;

  IDataConnector = interface
  ['{1653E0AC-C7FC-4773-A921-51DCA67080D9}']
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandDataChannel: IPSCommandDataChannel;
    function PSPositionChangeChannel: IPSPositionChangeChannel;
    procedure RegisterField(const AName: String; const AFieldChannel: IPSTextChannel);
    procedure RegisterCommand(const AChannel: IPubSubChannel; const AData: TCommandData);
  end;

implementation

{ TPositionChange }

class function TPositionChange.New(ADelta: Integer): TPositionChange;
begin
  Result.fDelta := TOptional<Integer>.New(ADelta);
end;

class function TPositionChange.New: TPositionChange;
begin
end;

class operator TPositionChange.Initialize(var a: TPositionChange);
begin
  a.Delta.Clear;
end;

class operator TPositionChange.equal(a, b: TPositionChange): Boolean;
begin
  Result := a.Delta = b.Delta;
end;

{ TRecordData }

constructor TRecordData.Create(APosition: Integer; const AAccessor: IDataAccessor);
begin
  fPosition := APosition;
  fAccessor := AAccessor;
end;

class operator TRecordData.equal(a, b: TRecordData): Boolean;
begin
  Result := (a.Position = b.Position) and (a.Accessor = b.Accessor);
end;

{ TCommandData }

class function TCommandData.CreateFirst: TCommandData;
begin
  Result.fAction := cdaFirst;
end;

class function TCommandData.CreateLast: TCommandData;
begin
  Result.fAction := cdaLast;
end;

class function TCommandData.CreateNext: TCommandData;
begin
  Result := CreateMove(1);
end;

class function TCommandData.CreatePrior: TCommandData;
begin
  Result := CreateMove(-1);
end;

class function TCommandData.CreateMove(ADelta: Integer): TCommandData;
begin
  Result.fAction := cdaMove;
  Result.fDelta := ADelta;
end;

class function TCommandData.CreateInfo(AFromPos, AToPos: Integer): TCommandData;
begin
  Result.fAction := cdaInfo;
  Result.fFromPos := AFromPos;
  Result.fToPos := AToPos;
end;

class operator TCommandData.equal(a, b: TCommandData): Boolean;
begin
  Result := (a.Action = b.Action)
    and (a.Delta = b.Delta)
    and (a.FromPos = b.FromPos)
    and (a.ToPos = b.ToPos);
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

