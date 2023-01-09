unit trl_usystem;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, trl_uprops, trl_irttibroker, trl_urttibroker;

type

  { TDynaObject }

  TDynaObject = class(TInterfacedObject)
  public
    class function newinstance : tobject;override;
  protected
    fSelfProps: IProps;
    procedure InitValues; virtual;
    procedure LocateFinished(var Msg); message 'LocateFinished';
  published
    property SelfProps: IProps read fSelfProps;
  end;

  { TPlainObject }

  TPlainObject = class(TObject)
  private
    procedure FreePublishedObjects;
  public
    procedure BeforeDestruction; override;
  end;

implementation

{ TPlainObject }

procedure TPlainObject.FreePublishedObjects;
var
  mRB: IRBData;
  i: integer;
begin
  mRB := TRBData.Create(Self);
  for i := 0 to mRB.Count - 1 do begin
    if mRB[i].TypeKind = tkClass then begin
      mRB[i].AsObject.Free;
    end;
  end;
end;

procedure TPlainObject.BeforeDestruction;
begin
  FreePublishedObjects;
  inherited BeforeDestruction;
end;

{ TDynaObject }

class function TDynaObject.newinstance: tobject;
begin
  Result := inherited newinstance;
  (Result as TDynaObject).fSelfProps := TProps.Create;
end;

procedure TDynaObject.InitValues;
begin
end;

procedure TDynaObject.LocateFinished(var Msg);
begin
  InitValues;
end;

end.

