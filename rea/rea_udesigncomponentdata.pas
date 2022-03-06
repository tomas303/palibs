unit rea_udesigncomponentdata;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, sysutils, rea_iflux;

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
    fChangedNotifier: IFluxNotifier;
  published
    property Text: String read fText write fText;
    property Focused: Boolean read fFocused write fFocused;
    property ChangedNotifier: IFluxNotifier read fChangedNotifier write fChangedNotifier;
  end;

  { TGridData }

  TGridData = class
  private type
   TMatrix = array of array of string;
  private
   fData: TMatrix;
   fEditData: TEditData;
   fColCount: Integer;
   fRowCount: Integer;
   fCurrentRow: Integer;
   fCurrentCol: Integer;
   fBrowseMode: Boolean;
   fDataRow: integer;
   fLastDataRow: integer;
   function GetValue(Row, Col: Integer): string;
   procedure SetColCount(AValue: Integer);
   procedure SetCurrentCol(AValue: Integer);
   procedure SetCurrentRow(AValue: Integer);
   procedure SetRowCount(AValue: Integer);
   procedure SetValue(Row, Col: Integer; AValue: string);
   procedure SynchronizeEditText;
  public
   constructor Create;
   procedure BeforeDestruction; override;
   property ColCount: Integer read fColCount write SetColCount;
   property RowCount: Integer read fRowCount write SetRowCount;
   property CurrentRow: Integer read fCurrentRow write SetCurrentRow;
   property CurrentCol: Integer read fCurrentCol write SetCurrentCol;
   property BrowseMode: Boolean read fBrowseMode write fBrowseMode;
   property Value[Row, Col: Integer]: string read GetValue write SetValue; default;
   property EditData: TEditData read fEditData;
   property DataRow: integer read fDataRow write fDataRow;
   property LastDataRow: integer read fLastDataRow write fLastDataRow;
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
  SetLength(fData, RowCount, ColCount);
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
  if fRowCount = AValue then Exit;
  fRowCount := AValue;
  SetLength(fData, RowCount, ColCount);
end;

procedure TGridData.SetValue(Row, Col: Integer; AValue: string);
begin
  fData[Row, Col] := AValue;
  if (Row = CurrentRow) and (Col = CurrentCol) then
  SynchronizeEditText;
end;

procedure TGridData.SynchronizeEditText;
begin
  fEditData.Text := Value[CurrentRow, CurrentCol];
  fEditData.Focused := True;
end;

constructor TGridData.Create;
begin
  fBrowseMode := True;
  fEditData := TEditData.Create;
  fDataRow := -1;
  fLastDataRow := -1;
end;

procedure TGridData.BeforeDestruction;
begin
  FreeAndNil(fEditData);
  inherited BeforeDestruction;
end;

end.

