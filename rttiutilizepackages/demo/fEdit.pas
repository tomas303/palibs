unit fEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, uPerson, rtti_serializer_iManager, rtti_broker_iBroker,
  rtti_broker_uData;

type

  { TForm1 }

  TForm1 = class(TForm)
    sg: TStringGrid;
  private
    fStore: ISerialStore;
  public
    procedure AttachSerialManager(const AStore: ISerialStore);
    procedure FillPersons(APersons: IRBDataList);
  end;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AttachSerialManager(const AStore: ISerialStore);
begin
  fStore := AStore;
end;

procedure TForm1.FillPersons(APersons: IRBDataList);
var
  mData: IRBData;
  i, j: integer;
begin
  sg.FixedRows := 1;
  sg.FixedCols := 0;
  sg.RowCount := APersons.Count + 1;
  if APersons.Count = 0 then
  begin
    sg.ColCount := 0;
    Exit;
  end;
  mData := APersons.AsData[0];
  sg.ColCount := mData.Count;
  for i := 0 to mData.Count - 1 do
    sg.Cells[i, 0] := mData[i].Name;
  for i := 0 to APersons.Count - 1 do
  begin
    mData := APersons.AsData[i];
    for j := 0 to mData.Count - 1 do
    begin
      sg.Cells[j, i + 1] := mData[j].AsString;
    end;
  end;
end;

end.

