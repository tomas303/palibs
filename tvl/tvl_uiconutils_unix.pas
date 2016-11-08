unit tvl_uiconutils_unix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tvl_iiconutils, FPimage, Graphics, LCLIntf, LCLType;

type

  { TIconUtilsUnix }

  TIconUtilsUnix = class(TInterfacedObject, IIconUtils)
  public const
    cHiColorPath = '/usr/share/icons/hicolor/';
  protected type
    TIconDirHeight = (ic16=16,ic22=22,ic24=24,ic32=32,ic36=36,ic48=48,
      ic64=64,ic72=72,ic96=96,ic128=128,ic192=192,ic259=256,ic512=512);
  protected
    fPaths: TStringList;
    fIconHeight: integer;
    function GetPaths: TStringList;
    procedure SetIconHeight(AValue: integer);
    property Paths: TStringList read GetPaths;
    property IconHeight: integer read fIconHeight write SetIconHeight;
  protected
    function ConstructPath(ADirHeight: TIconDirHeight): string;
    function FindDirHeight(AHeight: integer): TIconDirHeight;
    function ScanPath(const APath: string): string;
    function FindInPaths(const AApplication: string; const AMask: string): string;
    function FindAppIconFile(const AApplication: string): string;
  protected
    procedure RenderAppIcon(const AApplication: string; ABitmap: TBitmap;
      AIconHeight: Integer);
  public
    procedure BeforeDestruction; override;
  end;

implementation

{ TIconUtilsUnix }

procedure TIconUtilsUnix.SetIconHeight(AValue: integer);
begin
  if fIconHeight <> AValue then
  begin
    fIconHeight := AValue;
    FreeAndNil(fPaths);
  end;
end;

function TIconUtilsUnix.GetPaths: TStringList;
var
  mActDirHeight, mDirHeight: TIconDirHeight;
  mDir: string;
begin
  if fPaths = nil then begin;
    fPaths := TStringList.Create;
    mActDirHeight := FindDirHeight(IconHeight);
    for mDirHeight := mActDirHeight downto Low(TIconDirHeight) do begin
      mDir := ConstructPath(mDirHeight);
      fPaths.Add(mDir);
    end;
    for mDirHeight := mActDirHeight to High(TIconDirHeight) do begin
      mDir := ConstructPath(mDirHeight);
      fPaths.Add(mDir);
    end;
    fPaths.Add('scalable' + DirectorySeparator + 'apps' + DirectorySeparator);
  end;
  Result := fPaths;
end;

function TIconUtilsUnix.ConstructPath(ADirHeight: TIconDirHeight): string;
begin
  Result := IntToStr(Ord(ADirHeight));
  Result  := Result  + 'x' + Result + DirectorySeparator + 'apps' + DirectorySeparator;
end;

function TIconUtilsUnix.ScanPath(const APath: string): string;
var
  mRec: TRawByteSearchRec;
  mExt: string;
begin
  Result := '';
  if FindFirst(APath, faanyfile, mRec) = 0 then
  repeat
    mExt := ExtractFileExt(mRec.Name);
    if SameText('.png', mExt) or SameText('.ico', mExt) or SameText('.svg', mExt) then
    begin
      Result := mRec.Name;
      Exit;
    end;
  until FindNext(mRec) <> 0;
end;

function TIconUtilsUnix.FindDirHeight(AHeight: integer): TIconDirHeight;
var
  mDirHeight: TIconDirHeight;
begin
  Result := High(TIconDirHeight);
  for mDirHeight := High(TIconDirHeight) downto Low(TIconDirHeight) do
  begin
    if AHeight >= Ord(mDirHeight) then
    begin
      Result := mDirHeight;
      Break;
    end;
  end;
end;

function TIconUtilsUnix.FindInPaths(const AApplication: string; const AMask: string): string;
var
  i: Integer;
begin
  for i := 0 to Paths.Count - 1 do begin
    Result := ScanPath(cHiColorPath + Paths[i] + AApplication + AMask);
    if Result <> '' then begin
      Result := cHiColorPath + Paths[i] + Result;
      Break;
    end;
  end;
end;

function TIconUtilsUnix.FindAppIconFile(const AApplication: string): string;
begin
  Result := FindInPaths(AApplication, '.*');
  if Result = '' then
    Result := FindInPaths(AApplication, '*.*');
end;

procedure TIconUtilsUnix.RenderAppIcon(const AApplication: string; ABitmap: TBitmap;
  AIconHeight: Integer);
var
  mGraphic: TPortableNetworkGraphic;
  mFile: string;
begin
  IconHeight := AIconHeight;
  mFile := FindAppIconFile(AApplication);
  mGraphic := nil;
  if SameText(ExtractFileExt(mFile), '.svg') then begin
    //AddVectorialImage(mIcoFile, ABitmap, AHeight);
  end else if SameText(ExtractFileExt(mFile), '.png') then begin
    mGraphic := TPortableNetworkGraphic.Create;
  end;
  if mGraphic = nil then
    Exit;
  try
    mGraphic.LoadFromFile(mFile);
    if mGraphic.Height <= AIconHeight then
      ABitmap.Assign(mGraphic)
    else begin
      ABitmap.Width := AIconHeight;
      ABitmap.Height := AIconHeight;
      ABitmap.TransparentMode := mGraphic.TransparentMode;
      ABitmap.TransparentColor := mGraphic.TransparentColor;
      ABitmap.PixelFormat := mGraphic.PixelFormat;
      ABitmap.Masked := mGraphic.Masked;
      ABitmap.Canvas.StretchDraw(TRect.Create(0, 0, ABitmap.Width - 1, ABitmap.Height - 1), mGraphic);
    end;
  finally
    mGraphic.Free;
  end;
end;

procedure TIconUtilsUnix.BeforeDestruction;
begin
  FreeAndNil(fPaths);
  inherited BeforeDestruction;
end;

end.

