unit tvl_uiconutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tvl_iiconutils, FPimage, Graphics, LCLIntf, LCLType;

type

  { TIconUtils }

  TIconUtils = class(TInterfacedObject, IIconUtils)
  public const
    cHiColorPath = '/usr/share/icons/hicolor/';
  protected type
    TIconDirHeight = (ic16=16,ic22=22,ic24=24,ic32=32,ic36=36,ic48=48,
      ic64=64,ic72=72,ic96=96,ic128=128,ic192=192,ic259=256,ic512=512);
  protected
    fPaths: TStringList;
    fIconHeight: integer;
    function GetPaths: TStringList;
    property Paths: TStringList read GetPaths;
  protected
    function ConstructPath(ADirHeight: TIconDirHeight): string;
    function FindDirHeight(AHeight: integer): TIconDirHeight;
    function ScanPath(const APath: string): string;
    function FindInPaths(const AApplication: string; const AMask: string): string;
  protected
    // IIconUtils
    function FindAppIconFile(const AApplication: string): string;
    procedure RenderAppIcon(const AFile: string; ABitmap: TBitmap);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property IconHeight: integer read fIconHeight write fIconHeight;
  end;

implementation

{ TIconUtils }

function TIconUtils.GetPaths: TStringList;
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

function TIconUtils.ConstructPath(ADirHeight: TIconDirHeight): string;
begin
  Result := IntToStr(Ord(ADirHeight));
  Result  := Result  + 'x' + Result + DirectorySeparator + 'apps' + DirectorySeparator;
end;

function TIconUtils.ScanPath(const APath: string): string;
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

function TIconUtils.FindDirHeight(AHeight: integer): TIconDirHeight;
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

function TIconUtils.FindInPaths(const AApplication: string; const AMask: string): string;
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

function TIconUtils.FindAppIconFile(const AApplication: string): string;
begin
  Result := FindInPaths(AApplication, '.*');
  if Result = '' then
    Result := FindInPaths(AApplication, '*.*');
end;

procedure TIconUtils.RenderAppIcon(const AFile: string; ABitmap: TBitmap);
var
  mGraphic: TPortableNetworkGraphic;
begin
  mGraphic := nil;
  if SameText(ExtractFileExt(AFile), '.svg') then begin
    //AddVectorialImage(mIcoFile, ABitmap, AHeight);
  end else if SameText(ExtractFileExt(AFile), '.png') then begin
    mGraphic := TPortableNetworkGraphic.Create;
  end;
  if mGraphic = nil then
    Exit;
  try
    mGraphic.LoadFromFile(AFile);
    if mGraphic.Height <= IconHeight then
      ABitmap.Assign(mGraphic)
    else begin
      ABitmap.Width := IconHeight;
      ABitmap.Height := IconHeight;
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

procedure TIconUtils.AfterConstruction;
begin
  inherited AfterConstruction;
  IconHeight := GetSystemMetrics(SM_CYMENU);
end;

procedure TIconUtils.BeforeDestruction;
begin
  FreeAndNil(fPaths);
  inherited BeforeDestruction;
end;

end.

