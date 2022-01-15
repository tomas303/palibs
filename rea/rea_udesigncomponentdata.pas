unit rea_udesigncomponentdata;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, sysutils;

type

  { TFormData }

  TFormData = class
  private
    fLeft: Integer;
    fTop: Integer;
    fWidth: Integer;
    fHeight: Integer;
  published
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
  end;

  { TEditData }

  TEditData = class
  private
    fText: String;
    fFocused: Boolean;
  published
    property Text: String read fText write fText;
    property Focused: Boolean read fFocused write fFocused;
  end;

  { TGridData }

  TGridData = class
  private type
   TMatrix = array of array of string;
  private
   fData: TMatrix;
   fEditData: TEditData;
   fDataRow: integer;
   fProvider: IGridDataProvider;
   fColCount: Integer;
   fRowCount: Integer;
   fCurrentRow: Integer;
   fCurrentCol: Integer;
   fBrowseMode: Boolean;
   function GetValue(Row, Col: Integer): string;
   procedure SetColCount(AValue: Integer);
   procedure SetCurrentCol(AValue: Integer);
   procedure SetCurrentRow(AValue: Integer);
   procedure SetRowCount(AValue: Integer);
   procedure SetValue(Row, Col: Integer; AValue: string);
   procedure Move(ADelta: Integer);
   procedure ReadDataRow(ARow: Integer);
   procedure ClearDataRow(ARow: Integer);
   procedure SynchronizeEditText;
  public
   constructor Create(const AProvider: IGridDataProvider);
   procedure BeforeDestruction; override;
   procedure ReadData;
   property ColCount: Integer read fColCount write SetColCount;
   property RowCount: Integer read fRowCount write SetRowCount;
   property CurrentRow: Integer read fCurrentRow write SetCurrentRow;
   property CurrentCol: Integer read fCurrentCol write SetCurrentCol;
   property BrowseMode: Boolean read fBrowseMode write fBrowseMode;
   property Value[Row, Col: Integer]: string read GetValue write SetValue; default;
   property EditData: TEditData read fEditData;
  end;

  { TPagerData }

  TPagerData = class
  private
    fActiveIndex: Integer;
  published
    property ActiveIndex: Integer read fActiveIndex write fActiveIndex;
  end;

implementation

{ TGridData }

function TGridData.GetValue(Row, Col: Integer): string;
begin
  Result := fData[Row, Col];
end;

procedure TGridData.SetColCount(AValue: Integer);
begin
  if fColCount = AValue then Exit;
  fColCount := AValue;
end;

procedure TGridData.SetCurrentCol(AValue: Integer);
begin
  if fCurrentCol = AValue then Exit;
  if (AValue >= Low(fData[CurrentRow])) or (AValue <= High(fData[CurrentRow])) then begin
    fCurrentCol := AValue;
    SynchronizeEditText;
  end;
end;

procedure TGridData.SetCurrentRow(AValue: Integer);
begin
  if fCurrentRow = AValue then Exit;
  if (AValue >= Low(fData)) or (AValue <= High(fData)) then begin
    fCurrentRow := AValue;
    SynchronizeEditText;
  end;
end;

procedure TGridData.SetRowCount(AValue: Integer);
begin
  if fRowCount=AValue then Exit;
  fRowCount:=AValue;
end;

procedure TGridData.SetValue(Row, Col: Integer; AValue: string);
begin
  fData[Row, Col] := AValue;
  Move(Row - fDataRow);
  fProvider[Col] := AValue;
end;

procedure TGridData.Move(ADelta: Integer);
var
  i: integer;
begin
  if ADelta > 0 then begin
    for i := 1 to ADelta do begin
      fProvider.Next;
      Inc(fDataRow);
    end;
  end else if ADelta < 0 then begin
    for i := ADelta to -1 do begin
      fProvider.Prev;
      Dec(fDataRow);
    end;
  end;
end;

procedure TGridData.ReadData;
var
  i: integer;
  mMoved: Boolean;
begin
  SetLength(fData, RowCount, ColCount);
  Move(-fDataRow);
  if fProvider.IsEmpty then begin
    for i := 0 to RowCount - 1 do begin
      ClearDataRow(i);
    end;
  end else begin
    for i := 0 to RowCount - 1 do begin
      if mMoved then begin
        ReadDataRow(fDataRow);
        mMoved := fProvider.Next;
        if mMoved then
          Inc(fDataRow);
      end else begin
        ClearDataRow(i);
      end;
    end;
  end;
  SynchronizeEditText;
end;

procedure TGridData.ReadDataRow(ARow: Integer);
var
  i: integer;
begin
  for i := 0 to ColCount - 1 do
    Value[ARow, i] := fProvider[i];
end;

procedure TGridData.ClearDataRow(ARow: Integer);
var
  i: integer;
begin
  for i := 0 to ColCount - 1 do
    Value[ARow, i] := '';
end;

procedure TGridData.SynchronizeEditText;
begin
  EditData.Text := Value[CurrentRow, CurrentCol];
  EditData.Focused := True;
end;

constructor TGridData.Create(const AProvider: IGridDataProvider);
begin
  fProvider := AProvider;
  fBrowseMode := True;
  fEditData := TEditData.Create;
end;

procedure TGridData.BeforeDestruction;
begin
  FreeAndNil(fEditData);
  inherited BeforeDestruction;
end;

end.

