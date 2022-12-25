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

  private
    procedure PublishData;
  private
    fActualData: IRBData;
    fActualIndex: integer;
    fPSFieldDataChannel: IPSFieldDataChannel;
    fPSRecordDataChannel: IPSRecordDataChannel;
    fPSCommandDataChannel: IPSCommandDataChannel;
    procedure PSFieldDataChannelObserver(const AData: TFieldData);
    procedure PSCommandDataChannelObserver(const AData: TCommandData);
  protected
    function PSFieldDataChannel: IPSFieldDataChannel;
    function PSRecordDataChannel: IPSRecordDataChannel;
    function PSCommandDataChannel: IPSCommandDataChannel;
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
var
  i: integer;
begin
  if fActualData = nil then
    Exit;
  {
  for i := 0 to fActualData.Count - 1 do begin
    fPSFieldDataChannel.Publish(TFieldData.Create(
      fActualData.Items[i].Name,
      fActualData.Items[i].AsString
    ));
  end;
  }
  fPSRecordDataChannel.Publish(TRecordData.Create(TAccessor.Create(fActualData)));
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
begin
  case AData.Action of
    cdaNext:
      begin
        if fActualIndex < fList.Count -1 then begin
          inc(fActualIndex);
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
    cdaPrior:
      begin
        if fActualIndex > 0 then begin
          dec(fActualIndex);
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
    cdaFirst:
      begin
        if fList.Count > 0 then begin
          fActualIndex := 0;
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
      end;
    cdaLast:
      begin
        if fList.Count > 0 then begin
          fActualIndex := fList.Count - 1;
          fActualData := fList.Data[fActualIndex];
          PublishData;
        end;
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

