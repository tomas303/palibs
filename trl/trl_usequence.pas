unit trl_usequence;

{$mode objfpc}{$H+}

interface

uses
  trl_isequence;

type

  { TSequence }

  TSequence = class(TInterfacedObject, ISequence)
  private
    fValue: integer;
  protected
    // ISequence = interface
    function Next: integer;
  end;

implementation

{ TSequence }

function TSequence.Next: integer;
begin
  Result := InterlockedIncrement(fValue);
end;

end.

