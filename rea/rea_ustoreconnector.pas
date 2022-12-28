unit rea_ustoreconnector;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  rea_idata, trl_ipersist, trl_irttibroker, trl_pubsub, rea_ibits;

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
    procedure PublishData;
    procedure PublishInfo(AFrom, ATo: Integer);
    function NewAccessor(AIndex: Integer): IDataAccessor;
  private
    fActualData: IRBData;
    fActualIndex: integer;
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

procedure TStoreConnector.PublishData;
begin
  if fActualData = nil then
    Exit;
  fPSRecordDataChannel.Publish(TRecordData.Create(0, TAccessor.Create(fActualData)));
end;

procedure TStoreConnector.PublishInfo(AFrom, ATo: Integer);
var
  i, mInd: integer;
begin
  if ATo >= AFrom then begin
    for i := AFrom to ATo do begin
      mInd := fActualIndex + i;
      if (mInd < 0) or (mInd > fList.Count - 1) then
        fPSRecordDataChannel.Publish(TRecordData.Create(i))
      else
        fPSRecordDataChannel.Publish(TRecordData.Create(i, NewAccessor(mInd)));
    end;
  end else begin
    for i := AFrom downto ATo do begin
      mInd := fActualIndex + i;
      if (mInd < 0) or (mInd > fList.Count - 1) then
        fPSRecordDataChannel.Publish(TRecordData.Create(i))
      else
        fPSRecordDataChannel.Publish(TRecordData.Create(i, NewAccessor(mInd)));
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
  if fActualData = nil then
    Exit;
  fActualData.ItemByName[AData.Name].AsString := AData.Value;
  fStore.Save(fActualData);
end;

procedure TStoreConnector.PSCommandChannelObserver(
  const AData: TCommand);
var
  mNewIndex: Integer;
  mDelta: Integer;
begin
  case AData.Action of
    cmdMove: begin
      mNewIndex := fActualIndex + AData.Delta;
      if mNewIndex < 0 then
        mDelta := -fActualIndex
      else if mNewIndex > fList.Count - 1 then
        mDelta := fList.Count - 1 - fActualIndex
      else
      mDelta := AData.Delta;
      fActualIndex := fActualIndex + mDelta;
      fActualData := fList.Data[fActualIndex];
      fPSPositionChangeChannel.Publish(TPositionChange.New(mDelta));
      PublishData;
    end;
    cmdFirst: begin
      if fList.Count > 0 then begin
        fActualIndex := 0;
        fActualData := fList.Data[fActualIndex];
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishData;
      end;
    end;
    cmdLast: begin
      if fList.Count > 0 then begin
        fActualIndex := fList.Count - 1;
        fActualData := fList.Data[fActualIndex];
        fPSPositionChangeChannel.Publish(TPositionChange.New);
        PublishData;
      end;
    end;
    cmdInfo: begin
      PublishInfo(AData.FromPos, AData.ToPos);
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
  fList:=AValue;
  if fList.Count > 0 then
    fActualData := fList.Data[0];
  PublishData;
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

