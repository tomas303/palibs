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
unit rea_ulayout;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rea_ilayout, trl_itree, fgl,
  rea_ibits;

type

  // hide horizontal / vertical implementation
  IUniItem = interface
  ['{BFC52343-B9AE-4908-9D2F-E7A234421EF1}']
    function GetPlace: integer;
    function GetPlaceSize: integer;
    function GetSize: integer;
    function GetStart: integer;
    procedure SetSize(AValue: integer);
    procedure SetStart(AValue: integer);
    property Place: integer read GetPlace;
    property PlaceSize: integer read GetPlaceSize;
    property Start: integer read GetStart write SetStart;
    property Size: integer read GetSize write SetSize;
  end;

  { TUniItem }

  TUniItem = class(TInterfacedObject, IUniItem)
  protected
    // IUniItem = interface
    function GetPlace: integer; virtual; abstract;
    function GetPlaceSize: integer; virtual; abstract;
    function GetSize: integer; virtual; abstract;
    function GetStart: integer; virtual; abstract;
    procedure SetSize(AValue: integer); virtual; abstract;
    procedure SetStart(AValue: integer); virtual; abstract;
    property Place: integer read GetPlace;
    property Start: integer read GetStart write SetStart;
    property Size: integer read GetSize write SetSize;
  protected
    fItem: INode;
  public
    constructor Create(const AItem: INode);
  end;

  TUniItemClass = class of TUniItem;

  { TUniHorizontalItem }

  TUniHorizontalItem = class(TUniItem)
  protected
    // IUniItem = interface
    function GetPlace: integer; override;
    function GetPlaceSize: integer; override;
    function GetSize: integer; override;
    function GetStart: integer; override;
    procedure SetSize(AValue: integer); override;
    procedure SetStart(AValue: integer); override;
  end;

  { TUniVerticalItem }

  TUniVerticalItem = class(TUniItem)
  protected
    // IUniItem = interface
    function GetPlace: integer; override;
    function GetPlaceSize: integer; override;
    function GetSize: integer; override;
    function GetStart: integer; override;
    procedure SetSize(AValue: integer); override;
    procedure SetStart(AValue: integer); override;
  end;

  { TDesktopTiler }

  TDesktopTiler = class(TInterfacedObject, ITiler)
  protected type
    TPlaceItems = specialize TFPGInterfacedObjectList<IUniItem>;
  protected
    procedure Reposition(const ANode: INode; const AClass: TUniItemClass; AStart, ASize: integer);
  protected
    function ResizeFixed(const ANode: INode; const AClass: TUniItemClass): integer;
    function ResizeElastic(const ANode: INode; const AClass: TUniItemClass; AFreeSpace: integer): integer;
    procedure Spread(const ANode: INode; const AClass: TUniItemClass; ASize, AUsedSize: integer);
    procedure Replace(const ANode: INode; const AClass: TUniItemClass; ASize: integer);
  protected
    // ITiler
    procedure ReplaceChildren(const AContainer: IBit; ABorder: Integer = 0);
  end;

  { TScale }

  TScale = class(TInterfacedObject, IScale)
  protected
    // IScale
    function Scale(const ASize: integer): integer;
    function Unscale(const ASize: integer): integer;
  public
    procedure AfterConstruction; override;
  protected
    fMultiplicator, fDivider: integer;
  published
    property Multiplicator: integer read fMultiplicator write fMultiplicator;
    property Divider: integer read fDivider write fDivider;
  end;

implementation

{ TScale }

function TScale.Scale(const ASize: integer): integer;
begin
  Result := Round(ASize * Multiplicator / Divider);
end;

function TScale.Unscale(const ASize: integer): integer;
begin
  Result := Round(ASize * Divider / Multiplicator);
end;

procedure TScale.AfterConstruction;
begin
  inherited AfterConstruction;
  fMultiplicator := 1;
  fDivider := 1;
end;

{ TDesktopTiler }

function TDesktopTiler.ResizeFixed(const ANode: INode; const AClass: TUniItemClass): integer;
var
  mChild: INode;
  mUni: IUniItem;
begin
  Result := 0;
  for mChild in ANode do begin
    mUni := AClass.Create(mChild);
    case mUni.Place of
      cPlace.FixFront, cPlace.FixMiddle, cPlace.FixBack:
        begin
          mUni.Size := mUni.PlaceSize;
          Result := Result + mUni.Size;
        end;
    end;
  end;
end;

function TDesktopTiler.ResizeElastic(const ANode: INode; const AClass: TUniItemClass;
  AFreeSpace: integer): integer;
var
  mChild: INode;
  mUni: IUniItem;
  mAutoSize, mAutoSizeRest: integer;
  mAutoSizeCnt: integer;
begin
  // when no size is specified for elastic, then all will get same size from FreeSpace
  Result := 0;
  mAutoSizeCnt := 0;
  for mChild in ANode do begin
    mUni := AClass.Create(mChild);
    case mUni.Place of
      cPlace.Elastic:
        begin
          inc(mAutoSizeCnt);
        end;
    end;
  end;
  if mAutoSizeCnt > 0 then begin
    mAutoSize := Round(AFreeSpace / mAutoSizeCnt);
    mAutoSizeRest := AFreeSpace - mAutoSize * mAutoSizeCnt;
  end else
    mAutoSize := 0;
  for mChild in ANode do begin
    mUni := AClass.Create(mChild);
    case mUni.Place of
      cPlace.Elastic:
        begin
          mUni.Size := mAutoSize;
          // even distribution of rounded error
          if mAutoSizeRest < 0  then begin
            mUni.Size := mUni.Size - 1;
            mAutoSizeRest := mAutoSizeRest + 1;
          end
          else if mAutoSizeRest > 0  then begin
            mUni.Size := mUni.Size + 1;
            mAutoSizeRest := mAutoSizeRest - 1;
          end;
          Result := Result + mUni.Size;
        end;
    end;
  end;
end;

procedure TDesktopTiler.Spread(const ANode: INode;
  const AClass: TUniItemClass; ASize, AUsedSize: integer);
var
  mChild: INode;
  mUni: IUniItem;
  mStart, mRightStart: integer;
  mMiddles: TPlaceItems;
  mFixBackHit: Boolean;
  mDelta: integer;
  mCnt: integer;
begin
  mStart := 0;
  mFixBackHit := False;
  mMiddles := TPlaceItems.Create;
  try
    for mChild in ANode do begin
      mUni := AClass.Create(mChild);
      case mUni.Place of
        cPlace.FixMiddle:
          begin
            mMiddles.Add(mUni);
          end;
        cPlace.FixBack:
          begin
             mFixBackHit := True;
             // move postion so last place will touch opposite line(uiPlaceFixBack will push
             // all what remains to oposite line)
             mRightStart := mStart - 1 + ASize - AUsedSize;
             if mRightStart > mStart then
               mStart := mRightStart;
          end;
        else
          begin
            if not mFixBackHit then
              mMiddles.Clear;
          end;
      end;
      mUni.Start := mStart;
      mStart := mUni.Start + mUni.Size;
    end;
    mDelta := (ASize - AUsedSize) div (mMiddles.Count + 1);
    if mDelta > 0 then begin
      mCnt := 0;
      for mUni in mMiddles do
      begin
        inc(mCnt);
        mUni.Start := mUni.Start + mDelta * mCnt;
      end;
    end;
  finally
    mMiddles.Free;
  end;
end;

procedure TDesktopTiler.Reposition(const ANode: INode;
  const AClass: TUniItemClass; AStart, ASize: integer);
var
  mChild: INode;
  mUni: IUniItem;
begin
  for mChild in ANode do begin
    mUni := AClass.Create(mChild);
    mUni.Start := AStart;
    mUni.Size := ASize;
  end;
end;

procedure TDesktopTiler.Replace(const ANode: INode;
  const AClass: TUniItemClass; ASize: integer);
var
  mFixedSize: integer;
  mElasticSize: integer;
begin
  mFixedSize := ResizeFixed(ANode, AClass);
  mElasticSize := ResizeElastic(ANode, AClass, ASize - mFixedSize);
  Spread(ANode, AClass, ASize, mFixedSize + mElasticSize);
end;

procedure TDesktopTiler.ReplaceChildren(const AContainer: IBit; ABorder: Integer = 0);
begin
  // setup same height and count width of fixed fields
  case (AContainer as IBitPosition).Layout of
    cLayout.Horizontal:
      begin
        Replace(AContainer as INode, TUniHorizontalItem, (AContainer as IBitPosition).Width - 2*ABorder);
        Reposition(AContainer as INode, TUniVerticalItem, 0, (AContainer as IBitPosition).Height - 2*ABorder);
      end;
    cLayout.Vertical:
      begin
        Replace(AContainer as INode, TUniVerticalItem, (AContainer as IBitPosition).Height - 2*ABorder);
        Reposition(AContainer as INode, TUniHorizontalItem, 0, (AContainer as IBitPosition).Width - 2*ABorder);
      end;
    cLayout.Overlay:
      begin
        Reposition(AContainer as INode, TUniHorizontalItem, 0, (AContainer as IBitPosition).Width - 2*ABorder);
        Reposition(AContainer as INode, TUniVerticalItem, 0, (AContainer as IBitPosition).Height - 2*ABorder);
      end;
  end;
end;

{ TUniVerticalItem }

function TUniVerticalItem.GetPlace: integer;
begin
  Result := (fItem as IBitPosition).Place;
end;

function TUniVerticalItem.GetPlaceSize: integer;
begin
  Result := (fItem as IBitPosition).PlaceSize;
end;

function TUniVerticalItem.GetSize: integer;
begin
  Result := (fItem as IBitPosition).Height;
end;

function TUniVerticalItem.GetStart: integer;
begin
  Result := (fItem as IBitPosition).Top;
end;

procedure TUniVerticalItem.SetSize(AValue: integer);
begin
  (fItem as IBitPosition).Height := AValue;
end;

procedure TUniVerticalItem.SetStart(AValue: integer);
begin
  (fItem as IBitPosition).Top := AValue;
end;

{ TUniItem }

constructor TUniItem.Create(const AItem: INode);
begin
  fItem := AItem;
end;

{ TUniHorizontalItem }

function TUniHorizontalItem.GetPlace: integer;
begin
  Result := (fItem as IBitPosition).Place;
end;

function TUniHorizontalItem.GetPlaceSize: integer;
begin
  Result := (fItem as IBitPosition).PlaceSize;
end;

function TUniHorizontalItem.GetSize: integer;
begin
  Result := (fItem as IBitPosition).Width;
end;

function TUniHorizontalItem.GetStart: integer;
begin
  Result := (fItem as IBitPosition).Left;
end;

procedure TUniHorizontalItem.SetSize(AValue: integer);
begin
  (fItem as IBitPosition).Width := AValue;
end;

procedure TUniHorizontalItem.SetStart(AValue: integer);
begin
  (fItem as IBitPosition).Left := AValue;
end;

end.

