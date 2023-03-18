unit rea_idata;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  trl_pubsub, rea_ibits, trl_funcp, trl_ipersist, rea_idesigncomponent, trl_irttibroker;

type

  { IDataAccessor }

  IDataAccessor = interface
  ['{48A69436-153D-4760-8B77-F0FF3600F8E7}']
    function GetValue(const AName: String): String;
    property Value[const AName: String]: String read GetValue; default;
  end;

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

  { TRecordData }

  TRecordData = record
  strict private
    fPosition: Integer;
    fAccessor: TOptional<IDataAccessor>;
  public
    constructor Create(APosition: Integer); overload;
    constructor Create(APosition: Integer; const AAccessor: IDataAccessor); overload;
    class operator equal(a,b: TRecordData): Boolean;
    public property Position: Integer read fPosition;
    public property Accessor: TOptional<IDataAccessor> read fAccessor;
  end;

  TCommandAction = (cmdFirst, cmdLast, cmdMove, cmdInfo, cmdInsert, cmdDelete);

  { TCommand }

  TCommand = record
  strict private
    fAction: TCommandAction;
    fDelta: Integer;
    fFromPos: Integer;
    fToPos: Integer;
    fData: IRBData;
    fPos: Integer;
  public
    class function CreateFirst: TCommand; static;
    class function CreateLast: TCommand; static;
    class function CreateNext: TCommand; static;
    class function CreatePrior: TCommand; static;
    class function CreateMove(ADelta: Integer): TCommand; static;
    class function CreateInfo(AFromPos, AToPos: Integer): TCommand; static;
    class function CreateInsert(APos: Integer; const AData: IRBData): TCommand; static;
    class function CreateDelete(APos: Integer): TCommand; static;
    class operator equal(a,b: TCommand): Boolean;
    public property Action: TCommandAction read fAction;
    public property Delta: Integer read fDelta;
    public property FromPos: Integer read fFromPos;
    public property ToPos: Integer read fToPos;
    public property Data: IRBData read fData;
    public property Pos: Integer read fPos;
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
  IPSCommandChannel = IPubSubDataChannel<TCommand>;
  IPSPositionChangeChannel = IPubSubDataChannel<TPositionChange>;

  IDataConnector = interface
  ['{1653E0AC-C7FC-4773-A921-51DCA67080D9}']
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandChannel: IPSCommandChannel;
    function PSPositionChangeChannel: IPSPositionChangeChannel;
    procedure RegisterEdit(const AName: String; const AEdit: IDesignComponentEdit);
    procedure RegisterMemo(const AName: String; const AEdit: IDesignComponentMemo);
    procedure RegisterText(const AName: String; const AText: IDesignComponentText);
    procedure RegisterGrid(const ANames: TArray<String>; const AGrid: IDesignComponentGrid; const AClass: TClass);
    procedure RegisterCommand(const AChannel: IPubSubChannel; const AData: TCommand);
    procedure ConnectList(const AList: IMiniList);
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

constructor TRecordData.Create(APosition: Integer);
begin
  fPosition := APosition;
  fAccessor := TOptional<IDataAccessor>.New;
end;

constructor TRecordData.Create(APosition: Integer; const AAccessor: IDataAccessor);
begin
  fPosition := APosition;
  fAccessor := TOptional<IDataAccessor>.New(AAccessor);
end;

class operator TRecordData.equal(a, b: TRecordData): Boolean;
begin
  Result := (a.Position = b.Position) and (a.Accessor = b.Accessor);
end;

{ TCommand }

class function TCommand.CreateFirst: TCommand;
begin
  Result.fAction := cmdFirst;
end;

class function TCommand.CreateLast: TCommand;
begin
  Result.fAction := cmdLast;
end;

class function TCommand.CreateNext: TCommand;
begin
  Result := CreateMove(1);
end;

class function TCommand.CreatePrior: TCommand;
begin
  Result := CreateMove(-1);
end;

class function TCommand.CreateMove(ADelta: Integer): TCommand;
begin
  Result.fAction := cmdMove;
  Result.fDelta := ADelta;
end;

class function TCommand.CreateInfo(AFromPos, AToPos: Integer): TCommand;
begin
  Result.fAction := cmdInfo;
  Result.fFromPos := AFromPos;
  Result.fToPos := AToPos;
end;

class function TCommand.CreateInsert(APos: Integer; const AData: IRBData): TCommand;
begin
  Result.fAction := cmdInsert;
  Result.fPos:=  APos;
  Result.fData := AData;
end;

class function TCommand.CreateDelete(APos: Integer): TCommand;
begin
  Result.fAction := cmdDelete;
  Result.fPos := APos;
end;

class operator TCommand.equal(a, b: TCommand): Boolean;
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

