unit rtti_broker_uPersistClassRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, fgl;

type

  { TRBPersistClassRegister }

  TRBPersistClassRegister = class(TInterfacedObject, IRBPersistClassRegister)
  private type
    TClasses = specialize TFPGList<TPersistentClass>;
  private
    fClasses: TClasses;
  protected
    procedure Add(const AClass: TPersistentClass);
    function GetItem(AIndex: integer): TPersistentClass;
    function GetCount: integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;


implementation

{ TRBPersistClassRegister }

procedure TRBPersistClassRegister.Add(const AClass: TPersistentClass);
begin
  if fClasses.IndexOf(AClass) = -1 then
    fClasses.Add(AClass);
end;

function TRBPersistClassRegister.GetItem(AIndex: integer): TPersistentClass;
begin
  Result := fClasses[AIndex];
end;

function TRBPersistClassRegister.GetCount: integer;
begin
  Result := fClasses.Count;
end;

procedure TRBPersistClassRegister.AfterConstruction;
begin
  fClasses := TClasses.Create;
end;

procedure TRBPersistClassRegister.BeforeDestruction;
begin
  FreeAndNil(fClasses);
end;

end.

