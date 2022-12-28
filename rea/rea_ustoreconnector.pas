unit rea_ustoreconnector;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  rea_idata, trl_ipersist, trl_irttibroker, trl_pubsub, rea_ibits,
  trl_funcp, sysutils;

type

  { TStoreConnector }

  TStoreConnector =  class(TInterfacedObject, IDataConnector)
  private type

    { TAccessor }

    TAccessor = class(TInterfacedObject, IDataAccessor)
    private
      fData: IRBData;
      function GetValue(const AName: String): String;
    public
      constructor Create(const AData: IRBData);
    end;

    { TEmptyAccessor }

    TEmptyAccessor = class(TInterfacedObject, IDataAccessor)
    private
      function GetValue(const AName: String): String;
    end;

  private
    procedure PublishActualRecord;
    procedure PublishRecords(AFrom, ATo: Integer);
    function NewAccessor(AIndex: Integer): IDataAccessor;
  private
    fActualData: TOptional<IRBData>;
    fActualIndex: TOptional<Integer>;
    fPSFieldDataChannel: IPSFieldDataChannel;
    fPSRecordDataChannel: IPSRecordDataChannel;
    fPSCommandChannel: IPSCommandChannel;
    fPSPositionChangeChannel: IPSPositionChangeChannel;
    procedure PSFieldDataChannelObserver(const AData: TFieldData);
    procedure PSCommandChannelObserver(const AData: TCommand);
  protected
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandChannel: IPSCommandChannel;
    function PSPositionChangeChannel: IPSPositionChangeChannel;
    procedure RegisterField(const AName: String; const AFieldChannel: IPSTextChannel);
    procedure RegisterCommand(const AChannel: IPubSubChannel; const AData: TCommand);
  public
    procedure BeforeDestruction; override;
  protected
    fPubSub: IPubSub;
    fStore: IPersistStore;
    fList: IPersistRefList;
    procedure SetList(AValue: IPersistRefList);
    procedure SetPubSub(AValue: IPubSub);
  published
    property PubSub: IPubSub read fPubSub write SetPubSub;
    property Store: IPersistStore read fStore write fStore;
    property List: IPersistRefList read fList write SetList;
  end;

implementation

{ TStoreConnector.TEmptyAccessor }

function TStoreConnector.TEmptyAccessor.GetValue(const AName: String): String;
begin
  Result := '';
end;

{ TStoreConnector.TAccessor }

function TStoreConnector.TAccessor.GetValue(const AName: String): String;
begin
  Result := fData.ItemByName[AName].AsString;
end;

constructor TStoreConnector.TAccessor.Create(const AData: IRBData);
begin
  fData := AData;
end;

{ TStoreConnector }

procedure TStoreConnector.PublishActualRecord;
begin
  if fActualData.HasValue then
    fPSRecordDataChannel.Publish(TRecordData.Create(0, TAccessor.Create(fActualData.Value)))
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
    Result := TAccessor.Create(fList.Data[AIndex]);
end;

procedure TStoreConnector.PSFieldDataChannelObserver(const AData: TFieldData
  );
begin
  if fActualData.HasValue then begin
    fActualData.Value.ItemByName[AData.Name].AsString := AData.Value;
    fStore.Save(fActualData.Value);
  end;
end;

procedure TStoreConnector.PSCommandChannelObserver(
  const AData: TCommand);
var
  mNewIndex: Integer;
  mDelta: Integer;
begin
  case AData.Action of
    cmdMove: begin
      if fActualIndex.HasValue then begin
        mNewIndex := fActualIndex.Value + AData.Delta;
        if mNewIndex < 0 then
          mDelta := -fActualIndex.Value
        else if mNewIndex > fList.Count - 1 then
          mDelta := fList.Count - 1 - fActualIndex.Value
        else
          mDelta := AData.Delta;
        fActualIndex := TOptional<Integer>.New(fActualIndex.Value + mDelta);
        fActualData := TOptional<IRBData>.New(fList.Data[fActualIndex.Value]);
        fPSPositionChangeChannel.Publish(TPositionChange.New(mDelta));
        PublishActualRecord;
      end else begin
        fPSPositionChangeChannel.Publish(TPositionChange.New(0));
        PublishActualRecord;
      end;
    end;
    cmdFirst: begin
      if fList.Count > 0 then begin
        fActualIndex := TOptional<Integer>.New(0);
        fActualData := TOptional<IRBData>.New(fList.Data[fActualIndex.Value]);
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishActualRecord;
      end;
    end;
    cmdLast: begin
      if fList.Count > 0 then begin
        fActualIndex := TOptional<Integer>.New(fList.Count - 1);
        fActualData := TOptional<IRBData>.New(fList.Data[fActualIndex.Value]);
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishActualRecord;
      end;
    end;
    cmdInfo: begin
      PublishRecords(AData.FromPos, AData.ToPos);
    end;
    cmdInsert: begin
      if fActualIndex.HasValue then begin
        fList.Insert(AData.Pos + fActualIndex.Value, AData.Ref);
        fActualIndex := TOptional<Integer>.New(AData.Pos + fActualIndex.Value);
        fActualData := TOptional<IRBData>.New(AData.Ref.Data);
      end else begin
        fList.Insert(0, AData.Ref);
        fActualIndex := TOptional<Integer>.New(0);
        fActualData := TOptional<IRBData>.New(AData.Ref.Data);
      end;
      fPSPositionChangeChannel.Publish(TPositionChange.New);
    end;
    cmdDelete: begin
      fList.Delete(AData.Pos + fActualIndex.Value);
      if fList.Count = 0 then begin
        fActualIndex := TOptional<Integer>.New;
        fActualData := TOptional<IRBData>.New;
      end;
      fPSPositionChangeChannel.Publish(TPositionChange.New);
    end;
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
end;

procedure TStoreConnector.SetList(AValue: IPersistRefList);
begin
  if fList = AValue then Exit;
  fList := AValue;
  if fList.Count > 0 then begin
    fActualIndex := TOptional<Integer>.New(0);
    fActualData := TOptional<IRBData>.New(fList.Data[fActualIndex.Value]);
  end else begin
    fActualIndex := TOptional<Integer>.New;
    fActualData := TOptional<IRBData>.New;
  end;
  PublishActualRecord;
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

procedure TStoreConnector.RegisterField(const AName: String;
  const AFieldChannel: IPSTextChannel);
begin
  fPubSub.Factory.NewDataBridge<String, TFieldData>(
    AFieldChannel,
    PSFieldDataChannel,
    function (const AData: String): TFieldData
    begin
      Result := TFieldData.Create(AName, AData);
    end);
  fPubSub.Factory.NewDataBridge<TRecordData, String>(
   PSRecordDataChannel,
   AFieldChannel,
   function (const AData: TRecordData): String
   begin
     if (AData.Position = 0) and AData.Accessor.HasValue then
       Result := AData.Accessor.Value[AName]
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
  inherited BeforeDestruction;
end;

end.

