(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit tvl_uiconutils_unix;

{$mode objfpc}{$H+}

interface

{$IFDEF UNIX}

uses
  Classes, SysUtils, tvl_iiconutils, FPimage, Graphics, LCLIntf, LCLType,
  tvl_uiconutils_common;

type

  { TIconUtilsUnix }

  TIconUtilsUnix = class(TIconUtilsCommon)
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

{$ENDIF UNIX}

implementation

{$IFDEF UNIX}

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
  mIcon: TPortableNetworkGraphic;
  mFile: string;
begin
  IconHeight := AIconHeight;
  PrepareBitmap(ABitmap, AIconHeight);
  mFile := FindAppIconFile(AApplication);
  mIcon := nil;
  if SameText(ExtractFileExt(mFile), '.svg') then begin
    //AddVectorialImage(mIcoFile, ABitmap, AHeight);
  end else if SameText(ExtractFileExt(mFile), '.png') then begin
    mIcon := TPortableNetworkGraphic.Create;
  end;
  if mIcon = nil then begin
    Exit;
  end;
  try
    mIcon.LoadFromFile(mFile);
    RenderBitmap(ABitmap, mIcon);
  finally
    mIcon.Free;
  end;
end;

procedure TIconUtilsUnix.BeforeDestruction;
begin
  FreeAndNil(fPaths);
  inherited BeforeDestruction;
end;

{$ENDIF UNIX}

end.

