unit rtti_serializer_uMoniker;

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, typinfo,
  rtti_serializer_uSerialObject;

type
  { TSerialMoniker }

  generic TSerialMoniker<T: TRBCustomObject> = class(TRBCustomObject)
  private
    fRefID: integer;
    fIDObject: T;
    fStore: IRBStore;
    function GetIDObject: T;
  public
    constructor Create(AStore: IRBStore); reintroduce;
    property IDObject: T read GetIDObject;
  published
    property RefID: integer read fRefID write fRefID;
  end;

implementation

{ TSerialMoniker<T> }

function TSerialMoniker.GetIDObject: T;
begin
  if fIDObject = nil then
  begin
    //fIDObject := fStore.Load(TRBCustomObjectClass(T.ClassType), RefID) as T;
    //fIDObject := fStore.Load(T.ClassName, RefID) as T;
    assert(false);
  end;
  Result := fIDObject;
end;

constructor TSerialMoniker.Create(AStore: IRBStore);
begin
  fStore := AStore;
end;

end.
