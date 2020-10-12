unit rdx_ufunc;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, trl_igenericaccess;

type

  { TRdxFunc }

  TRdxFunc = class(TInterfacedObject, IFluxFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); virtual; abstract;
  protected
    // IFluxFunc
    procedure Execute(const AAction: IFluxAction);
    function RunAsync: Boolean;
    function GetID: integer;
  protected
    fID: integer;
    fState: IGenericAccess;
  published
    property State: IGenericAccess read fState write fState;
    property ID: integer read GetID write fID;
  end;

implementation

{ TRdxFunc }

procedure TRdxFunc.Execute(const AAction: IFluxAction);
begin
  DoExecute(AAction);
end;

function TRdxFunc.RunAsync: Boolean;
begin
  Result := False;
end;

function TRdxFunc.GetID: integer;
begin
  Result := fID;
end;

end.

