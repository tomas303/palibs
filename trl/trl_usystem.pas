unit trl_usystem;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, trl_uprops;

type

  { TDynaObject }

  TDynaObject = class(TInterfacedObject)
  public
    class function newinstance : tobject;override;
  protected
    fSelfProps: IProps;
  published
    property SelfProps: IProps read fSelfProps;
  end;

implementation

{ TDynaObject }

class function TDynaObject.newinstance: tobject;
begin
  Result := inherited newinstance;
  (Result as TDynaObject).fSelfProps := TProps.Create;
end;

end.

