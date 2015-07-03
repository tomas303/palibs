unit rtti_serializer_uSerialObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, rtti_broker_uData;

type

{$M+}

  { TRBCustomObject }

  TRBCustomObject =  class(TObject, IRBData)
  private
    fRBData: IRBData;
    function GetRBData: IRBData;
  protected
    property RBData: IRBData read GetRBData implements IRBData;
  public
    constructor Create; virtual;
  end;

{$M-}

  TRBCustomObjectClass = class of TRBCustomObject;

implementation

{ TRBCustomObject }

function TRBCustomObject.GetRBData: IRBData;
begin
  if fRBData = nil then
  begin
    fRBData := TRBData.Create(Self);
  end;
  Result := fRBData;
end;

constructor TRBCustomObject.Create;
begin

end;

end.

