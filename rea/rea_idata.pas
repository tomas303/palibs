(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
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
    function GetList(const AName: String): IMiniList;
    property Value[const AName: String]: String read GetValue; default;
    property List[const AName: String]: IMiniList read GetList;
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

  { TListChange }

  TListChange = record
  private
    fList: IMiniList;
  public
    class function New(const AList: IMiniList): TListChange; static;
    class operator equal(a,b: TListChange): Boolean;
    property List: IMiniList read fList;
  end;

  IPSFieldDataChannel = IPubSubDataChannel<TFieldData>;
  IPSRecordDataChannel = IPubSubDataChannel<TRecordData>;
  IPSCommandChannel = IPubSubDataChannel<TCommand>;
  IPSPositionChangeChannel = IPubSubDataChannel<TPositionChange>;
  IPSListChangeChannel = IPubSubDataChannel<TListChange>;
  IPSSubDataChangeChannel = IPubSubChannel;
  IPSDataChangeChannel = IPubSubChannel;

  IDataConnector = interface
  ['{1653E0AC-C7FC-4773-A921-51DCA67080D9}']
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandChannel: IPSCommandChannel;
    function PSPositionChangeChannel: IPSPositionChangeChannel;
    function PSListChangeChannel: IPSListChangeChannel;
    function PSSubDataChangeChannel: IPSSubDataChangeChannel;
    function PSDataChangeChannel: IPSDataChangeChannel;
    procedure RegisterEdit(const AName: String; const AEdit: IDesignComponentEdit);
    procedure RegisterMemo(const AName: String; const AEdit: IDesignComponentMemo);
    procedure RegisterText(const AName: String; const AText: IDesignComponentText);
    procedure RegisterGrid(const ANames: TArray<String>; const AGrid: IDesignComponentGrid; const AClass: TClass);
    procedure RegisterConnector(const AName: String; const AConnector: IDataConnector);
    procedure RegisterCommand(const AChannel: IPubSubChannel; const AData: TCommand);
  end;

implementation

{ TListChange }

class function TListChange.New(const AList: IMiniList): TListChange;
begin
  Result.fList := AList;
end;

class operator TListChange.equal(a, b: TListChange): Boolean;
begin
  Result := a.List = b.List;
end;

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

