unit rea_ustoreconnector;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  rea_idata, trl_ipersist, trl_irttibroker, trl_pubsub, rea_ibits,
  trl_funcp, sysutils, rea_idesigncomponent, trl_udifactory;

type

  { TStoreConnector }

  TStoreConnector =  class(TInterfacedObject, IDataConnector)
  private type

    { TAccessor }

    TAccessor = class(TInterfacedObject, IDataAccessor)
    private
      fData: IRBData;
      fList: IMiniList;
      fIndex: Integer;
      function GetValue(const AName: String): String;
      function GetList(const AName: String): IMiniList;
    public
      constructor Create(const AList: IMiniList; AIndex: Integer);
    end;

    { TEmptyAccessor }

    TEmptyAccessor = class(TInterfacedObject, IDataAccessor)
    private
      function GetValue(const AName: String): String;
      function GetList(const AName: String): IMiniList;
    end;

  private
    procedure PublishActualRecord;
    procedure PublishRecords(AFrom, ATo: Integer);
    function NewAccessor(AIndex: Integer): IDataAccessor;
  private
    fActualIndex: TOptional<Integer>;
    fPSFieldDataChannel: IPSFieldDataChannel;
    fPSRecordDataChannel: IPSRecordDataChannel;
    fPSCommandChannel: IPSCommandChannel;
    fPSPositionChangeChannel: IPSPositionChangeChannel;
    fPSListChangeChannel: IPSListChangeChannel;
    fPSSubDataChangeChannel: IPSSubDataChangeChannel;
    procedure PSFieldDataChannelObserver(const AData: TFieldData);
    procedure PSCommandChannelObserver(const AData: TCommand);
    procedure PSListChangeChannelObserver(const AListChange: TListChange);
    procedure PSSubDataChangeChannelObserver;
  protected
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandChannel: IPSCommandChannel;
    function PSPositionChangeChannel: IPSPositionChangeChannel;
    function PSListChangeChannel: IPSListChangeChannel;
    function PSSubDataChangeChannel: IPSSubDataChangeChannel;
    procedure RegisterEdit(const AName: String; const AEdit: IDesignComponentEdit);
    procedure RegisterMemo(const AName: String; const AEdit: IDesignComponentMemo);
    procedure RegisterGrid(const ANames: TArray<String>; const AGrid: IDesignComponentGrid; const AClass: TClass);
    procedure RegisterText(const AName: String; const AText: IDesignComponentText);
    procedure RegisterConnector(const AName: String; const AConnector: IDataConnector);
    procedure RegisterCommand(const AChannel: IPubSubChannel; const AData: TCommand);
  public
    procedure BeforeDestruction; override;
  protected
    fFactory2: TDIFactory2;
    fPubSub: IPubSub;
    fList: IMiniList;
    procedure SetPubSub(AValue: IPubSub);
  published
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property PubSub: IPubSub read fPubSub write SetPubSub;
  end;

implementation

{ TStoreConnector.TEmptyAccessor }

function TStoreConnector.TEmptyAccessor.GetValue(const AName: String): String;
begin
  Result := '';
end;

function TStoreConnector.TEmptyAccessor.GetList(const AName: String): IMiniList;
begin
  Result := nil;
end;

{ TStoreConnector.TAccessor }

function TStoreConnector.TAccessor.GetValue(const AName: String): String;
begin
  Result := fList.Field[fIndex, AName];
end;

function TStoreConnector.TAccessor.GetList(const AName: String): IMiniList;
begin
  Result := fList.List[fIndex, AName];
end;

constructor TStoreConnector.TAccessor.Create(const AList: IMiniList; AIndex: Integer);
begin
  fList := AList;
  fIndex := AIndex;
end;

{ TStoreConnector }

procedure TStoreConnector.PublishActualRecord;
begin
  if fActualIndex.HasValue then
    fPSRecordDataChannel.Publish(TRecordData.Create(0, TAccessor.Create(fList, fActualIndex.Value)))
  else
    fPSRecordDataChannel.Publish(TRecordData.Create(0))
end;

procedure TStoreConnector.PublishRecords(AFrom, ATo: Integer);
var
  i, mInd: integer;
begin
  if fActualIndex.HasValue then begin
    if ATo >= AFrom then begin
      for i := AFrom to ATo do begin
        mInd := fActualIndex.Value + i;
        if (mInd < 0) or (mInd > fList.Count - 1) then
          fPSRecordDataChannel.Publish(TRecordData.Create(i))
        else
          fPSRecordDataChannel.Publish(TRecordData.Create(i, NewAccessor(mInd)));
      end;
    end else begin
      for i := AFrom downto ATo do begin
        mInd := fActualIndex.Value + i;
        if (mInd < 0) or (mInd > fList.Count - 1) then
          fPSRecordDataChannel.Publish(TRecordData.Create(i))
        else
          fPSRecordDataChannel.Publish(TRecordData.Create(i, NewAccessor(mInd)));
      end;
    end;
  end else begin
    if ATo >= AFrom then begin
      for i := AFrom to ATo do begin
          fPSRecordDataChannel.Publish(TRecordData.Create(i))
      end;
    end else begin
      for i := AFrom downto ATo do begin
          fPSRecordDataChannel.Publish(TRecordData.Create(i))
      end;
    end;
  end;
end;

function TStoreConnector.NewAccessor(AIndex: Integer): IDataAccessor;
begin
  if (AIndex < 0) or (AIndex > fList.Count - 1) then
    Result := TEmptyAccessor.Create
  else
    Result := TAccessor.Create(fList, AIndex);
end;

procedure TStoreConnector.PSFieldDataChannelObserver(const AData: TFieldData);
var
  mActualData: IRBData;
begin
  if fActualIndex.HasValue then begin
    fList.Field[fActualIndex.Value, AData.Name] := AData.Value;
    PublishActualRecord;
  end;
end;

procedure TStoreConnector.PSCommandChannelObserver(
  const AData: TCommand);
var
  mNewIndex: Integer;
  mDelta: Integer;
  mIndex: Integer;
begin
  case AData.Action of
    TCommandAction.cmdMove: begin
      if fActualIndex.HasValue then begin
        mNewIndex := fActualIndex.Value + AData.Delta;
        if mNewIndex < 0 then
          mDelta := -fActualIndex.Value
        else if mNewIndex > fList.Count - 1 then
          mDelta := fList.Count - 1 - fActualIndex.Value
        else
          mDelta := AData.Delta;
        fActualIndex := TOptional<Integer>.New(fActualIndex.Value + mDelta);
        fPSPositionChangeChannel.Publish(TPositionChange.New(mDelta));
        PublishActualRecord;
      end else begin
        fPSPositionChangeChannel.Publish(TPositionChange.New(0));
        PublishActualRecord;
      end;
    end;
    TCommandAction.cmdFirst: begin
      if fList.Count > 0 then begin
        fActualIndex := TOptional<Integer>.New(0);
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishActualRecord;
      end;
    end;
    TCommandAction.cmdLast: begin
      if fList.Count > 0 then begin
        fActualIndex := TOptional<Integer>.New(fList.Count - 1);
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishActualRecord;
      end;
    end;
    TCommandAction.cmdInfo: begin
      PublishRecords(AData.FromPos, AData.ToPos);
    end;
    TCommandAction.cmdInsert: begin
      if fActualIndex.HasValue then begin
        fList.Insert(AData.Pos);
        fActualIndex := TOptional<Integer>.New(AData.Pos + fActualIndex.Value);
      end else begin
        fList.Insert(0);
        fActualIndex := TOptional<Integer>.New(0);
      end;
      fPSPositionChangeChannel.Publish(TPositionChange.New);
      PublishActualRecord;
    end;
    TCommandAction.cmdDelete: begin
      if fActualIndex.HasValue then begin
        mIndex := AData.Pos + fActualIndex.Value;
        fList.Delete(mIndex);
        if fList.Count = 0 then begin
          fActualIndex := TOptional<Integer>.New;
        end else if fActualIndex.Value > fList.Count - 1 then begin
          fActualIndex := TOptional<Integer>.New(fList.Count - 1);
        end;
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishActualRecord;
      end;
    end;
  end;
end;

procedure TStoreConnector.PSListChangeChannelObserver(const AListChange: TListChange);
begin
  if fList = AListChange.List then Exit;
  fList := AListChange.List;
  if fList.Count > 0 then begin
    fActualIndex := TOptional<Integer>.New(0);
  end else begin
    fActualIndex := TOptional<Integer>.New;
  end;
  fPSPositionChangeChannel.Publish(TPositionChange.New);
  PublishActualRecord;
end;

procedure TStoreConnector.PSSubDataChangeChannelObserver;
begin
  if (fList.PSPersistChannel <> nil) then begin
    if fActualIndex.HasValue then
      fList.PSPersistChannel.Publish(TPersistInfo.Create(fList.Data[fActualIndex.Value], paChange));
  end else begin
    PublishActualRecord;
  end;
end;

procedure TStoreConnector.SetPubSub(AValue: IPubSub);
begin
  if fPubSub = AValue then Exit;
  fPubSub := AValue;
  fPSFieldDataChannel := fPubSub.Factory.NewDataChannel<TFieldData>;
  fPSFieldDataChannel.Subscribe(PSFieldDataChannelObserver);
  fPSRecordDataChannel := fPubSub.Factory.NewDataChannel<TRecordData>;
  fPSCommandChannel := fPubSub.Factory.NewDataChannel<TCommand>;
  fPSCommandChannel.Subscribe(PSCommandChannelObserver);
  fPSPositionChangeChannel := fPubSub.Factory.NewDataChannel<TPositionChange>;
  fPSPositionChangeChannel.Publish(TPositionChange.New);
  fPSListChangeChannel := fPubSub.Factory.NewDataChannel<TListChange>;
  fPSListChangeChannel.Subscribe(PSListChangeChannelObserver);
  fPSSubDataChangeChannel := fPubSub.Factory.NewChannel;
  fPSSubDataChangeChannel.Subscribe(PSSubDataChangeChannelObserver);
end;

function TStoreConnector.PSFieldDataChannel: IPSFieldDataChannel;
begin
  Result := fPSFieldDataChannel;
end;

function TStoreConnector.PSRecordDataChannel: IPSRecordDataChannel;
begin
  Result := fPSRecordDataChannel;
end;

function TStoreConnector.PSCommandChannel: IPSCommandChannel;
begin
  Result := fPSCommandChannel;
end;

function TStoreConnector.PSPositionChangeChannel: IPSPositionChangeChannel;
begin
  Result := fPSPositionChangeChannel;
end;

function TStoreConnector.PSListChangeChannel: IPSListChangeChannel;
begin
  Result := fPSListChangeChannel;
end;

function TStoreConnector.PSSubDataChangeChannel: IPSSubDataChangeChannel;
begin
  Result := fPSSubDataChangeChannel;
end;

procedure TStoreConnector.RegisterEdit(const AName: String;
  const AEdit: IDesignComponentEdit);
begin
  fPubSub.Factory.NewDataBridge<String, TFieldData>(
    AEdit.PSTextChannel,
    PSFieldDataChannel,
    function (const AData: String): TFieldData
    begin
      Result := TFieldData.Create(AName, AData);
    end);
  fPubSub.Factory.NewDataBridge<TRecordData, String>(
   PSRecordDataChannel,
   AEdit.PSTextChannel,
   function (const AData: TRecordData): String
   begin
     if (AData.Position = 0) and AData.Accessor.HasValue then
       Result := AData.Accessor.Value[AName]
     else
       raise EPubSubBridgeNoWay.Create('');
   end);
end;

procedure TStoreConnector.RegisterMemo(const AName: String;
  const AEdit: IDesignComponentMemo);
begin
  fPubSub.Factory.NewDataBridge<String, TFieldData>(
    AEdit.PSTextChannel,
    PSFieldDataChannel,
    function (const AData: String): TFieldData
    begin
      Result := TFieldData.Create(AName, AData);
    end);
  fPubSub.Factory.NewDataBridge<TRecordData, String>(
   PSRecordDataChannel,
   AEdit.PSTextChannel,
   function (const AData: TRecordData): String
   begin
     if (AData.Position = 0) and AData.Accessor.HasValue then
       Result := AData.Accessor.Value[AName]
     else
       raise EPubSubBridgeNoWay.Create('');
   end);
end;

procedure TStoreConnector.RegisterGrid(const ANames: TArray<String>;
  const AGrid: IDesignComponentGrid; const AClass: TClass);
begin
  fPubSub.Factory.NewDataBridge<TGridCmdMove, TCommand>(
    AGrid.PSGridCmdMoveChannel,
    PSCommandChannel,
    function (const x: TGridCmdMove): TCommand
    begin
      Result := TCommand.CreateMove(x.Delta);
    end);

  fPubSub.Factory.NewDataBridge<TGridCmdInfo, TCommand>(
    AGrid.PSGridCmdInfoChannel,
    PSCommandChannel,
    function (const x: TGridCmdInfo): TCommand
    begin
      Result := TCommand.CreateInfo(x.FromPos, x.ToPos);
    end);

  fPubSub.Factory.NewDataBridge<TGridCmdRow, TCommand>(
    AGrid.PSGridCmdRowChannel,
    PSCommandChannel,
    function(const x: TGridCmdRow): TCommand
    begin
      case x.Action of
        TGridCmdRowAction.cmdNew: begin
          Result := TCommand.CreateInsert(x.Pos, Factory2.Locate<IRBData>(AClass.ClassName));
        end;
        TGridCmdRowAction.cmdDelete: begin
          Result := TCommand.CreateDelete(x.Pos);
        end;
      else
        raise Exception.Create('unknown cmdrow command');
      end;
    end);

  fPubSub.Factory.NewDataBridge<TGridCmdField, TFieldData>(
    AGrid.PSGridCmdFieldChannel,
    PSFieldDataChannel,
    function (const x: TGridCmdField): TFieldData
    begin
      Result := TFieldData.Create(ANames[x.Col], x.Value);
    end);

  fPubSub.Factory.NewDataBridge<TRecordData, TGridRecord>(
    PSRecordDataChannel,
    AGrid.PSGridRecordChannel,
    function (const x: TRecordData): TGridRecord
    var
      mVals: TArray<String>;
      i: integer;
    begin
      if x.Accessor.HasValue then begin
        SetLength(mVals, Length(ANames));
        for i := Low(ANames) to High(ANames) do
          mVals[i] := x.Accessor.Value[ANames[i]];
        Result := TGridRecord.Create(x.Position, mVals);
      end else begin
        Result := TGridRecord.Create(x.Position);
      end;
    end);

  fPubSub.Factory.NewDataBridge<TPositionChange, TGridMover>(
    PSPositionChangeChannel,
    AGrid.PSGridMoverChannel,
    function (const x: TPositionChange): TGridMover
    begin
      if x.Delta.HasValue then
        Result := TGridMover.New(x.Delta.Value)
      else
        Result := TGridMover.New;
    end);

end;

procedure TStoreConnector.RegisterText(const AName: String; const AText: IDesignComponentText);
begin
  fPubSub.Factory.NewDataBridge<String, TFieldData>(
    AText.PSTextChannel,
    PSFieldDataChannel,
    function (const AData: String): TFieldData
    begin
      Result := TFieldData.Create(AName, AData);
    end);
  fPubSub.Factory.NewDataBridge<TRecordData, String>(
   PSRecordDataChannel,
   AText.PSTextChannel,
   function (const AData: TRecordData): String
   begin
     if (AData.Position = 0) and AData.Accessor.HasValue then
       Result := AData.Accessor.Value[AName]
     else
       raise EPubSubBridgeNoWay.Create('');
   end);
end;

procedure TStoreConnector.RegisterConnector(const AName: String; const AConnector: IDataConnector);
begin
  fPubSub.Factory.NewDataToNonDataBridge<TRecordData>(
    AConnector.PSRecordDataChannel,
    PSSubDataChangeChannel
  );
  fPubSub.Factory.NewDataBridge<TRecordData, TListChange>(
   PSRecordDataChannel,
   AConnector.PSListChangeChannel,
   function (const AData: TRecordData): TListChange
   begin
     if (AData.Position = 0) and AData.Accessor.HasValue then
       Result := TListChange.New(AData.Accessor.Value.List[AName])
     else
       raise EPubSubBridgeNoWay.Create('');
   end);
end;

procedure TStoreConnector.RegisterCommand(const AChannel: IPubSubChannel;
  const AData: TCommand);
begin
  fPubSub.Factory.NewNonDataToDataBridge<TCommand>(
    AChannel,
    PSCommandChannel,
    function: TCommand
    begin
      Result := AData
    end);
end;

procedure TStoreConnector.BeforeDestruction;
begin
  fPSFieldDataChannel.Unsubscribe(PSFieldDataChannelObserver);
  fPSCommandChannel.Unsubscribe(PSCommandChannelObserver);
  fPSListChangeChannel.Unsubscribe(PSListChangeChannelObserver);
  fPSSubDataChangeChannel.Unsubscribe(PSSubDataChangeChannelObserver);
  inherited BeforeDestruction;
end;

end.

