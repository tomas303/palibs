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
    fPSCommandDataChannel: IPSCommandDataChannel;
    fPSInfoDataChannel: IPSInfoDataChannel;
    fPSPositionChangeChannel: IPSPositionChangeChannel;
    procedure PSFieldDataChannelObserver(const AData: TFieldData);
    procedure PSCommandDataChannelObserver(const AData: TCommandData);
  protected
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandDataChannel: IPSCommandDataChannel;
    function PSInfoDataChannel: IPSInfoDataChannel;
    function PSPositionChangeChannel: IPSPositionChangeChannel;
    procedure RegisterField(const AName: String; const AFieldChannel: IPSTextChannel);
    procedure RegisterCommand(const AChannel: IPubSubChannel; const AData: TCommandData);
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
  fPSRecordDataChannel.Publish(TRecordData.Create(TAccessor.Create(fActualData)));
end;

procedure TStoreConnector.PublishInfo(AFrom, ATo: Integer);
var
  i: integer;
begin
  if ATo >= AFrom then begin
    for i := AFrom to ATo do begin
      fPSInfoDataChannel.Publish(TInfoData.Create(i, NewAccessor(fActualIndex + i)));
    end;
  end else begin
    for i := AFrom downto ATo do begin
      fPSInfoDataChannel.Publish(TInfoData.Create(i, NewAccessor(fActualIndex + i)));
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

procedure TStoreConnector.PSCommandDataChannelObserver(
  const AData: TCommandData);
var
  mNewIndex: Integer;
begin
  case AData.Action of
    cdaMove: begin
      mNewIndex := fActualIndex + AData.Delta;
      if (mNewIndex >= 0) and (mNewIndex <= fList.Count -1) then begin
        fActualIndex := mNewIndex;
        fActualData := fList.Data[fActualIndex];
        PublishData;
        fPSPositionChangeChannel.Publish(TPositionChange.New(AData.Delta));
      end;
    end;
    cdaFirst: begin
      if fList.Count > 0 then begin
        fActualIndex := 0;
        fActualData := fList.Data[fActualIndex];
        PublishData;
        fPSPositionChangeChannel.Publish(TPositionChange.New);
      end;
    end;
    cdaLast: begin
      if fList.Count > 0 then begin
        fActualIndex := fList.Count - 1;
        fActualData := fList.Data[fActualIndex];
        PublishData;
        fPSPositionChangeChannel.Publish(TPositionChange.New);
      end;
    end;
    cdaInfo: begin
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
  fPSCommandDataChannel := fPubSub.Factory.NewDataChannel<TCommandData>;
  fPSCommandDataChannel.Subscribe(PSCommandDataChannelObserver);
  fPSInfoDataChannel := fPubSub.Factory.NewDataChannel<TInfoData>;
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

function TStoreConnector.PSCommandDataChannel: IPSCommandDataChannel;
begin
  Result := fPSCommandDataChannel;
end;

function TStoreConnector.PSInfoDataChannel: IPSInfoDataChannel;
begin
  Result := fPSInfoDataChannel;
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
     Result := AData.Accessor[AName];
   end);
end;

procedure TStoreConnector.RegisterCommand(const AChannel: IPubSubChannel;
  const AData: TCommandData);
begin
  fPubSub.Factory.NewNonDataToDataBridge<TCommandData>(
    AChannel,
    PSCommandDataChannel,
    function: TCommandData
    begin
      Result := AData
    end);
end;

procedure TStoreConnector.BeforeDestruction;
begin
  fPSFieldDataChannel.Unsubscribe(PSFieldDataChannelObserver);
  fPSCommandDataChannel.Unsubscribe(PSCommandDataChannelObserver);
  inherited BeforeDestruction;
end;

end.

