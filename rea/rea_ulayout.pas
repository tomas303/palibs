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
    function GetMMSize: integer;
    function GetPlace: integer;
    function GetSize: integer;
    function GetStart: integer;
    procedure SetMMSize(AValue: integer);
    procedure SetPlace(AValue: integer);
    procedure SetSize(AValue: integer);
    procedure SetStart(AValue: integer);
    property Place: integer read GetPlace write SetPlace;
    property MMSize: integer read GetMMSize write SetMMSize;
    property Start: integer read GetStart write SetStart;
    property Size: integer read GetSize write SetSize;
  end;

  { TUniItem }

  TUniItem = class(TInterfacedObject, IUniItem)
  protected
    // IUniItem = interface
    function GetMMSize: integer; virtual; abstract;
    function GetPlace: integer; virtual; abstract;
    function GetSize: integer; virtual; abstract;
    function GetStart: integer; virtual; abstract;
    procedure SetMMSize(AValue: integer); virtual; abstract;
    procedure SetPlace(AValue: integer); virtual; abstract;
    procedure SetSize(AValue: integer); virtual; abstract;
    procedure SetStart(AValue: integer); virtual; abstract;
    property Place: integer read GetPlace write SetPlace;
    property MMSize: integer read GetMMSize write SetMMSize;
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
    function GetMMSize: integer; override;
    function GetPlace: integer; override;
    function GetSize: integer; override;
    function GetStart: integer; override;
    procedure SetMMSize(AValue: integer); override;
    procedure SetPlace(AValue: integer); override;
    procedure SetSize(AValue: integer); override;
    procedure SetStart(AValue: integer); override;
  end;

  { TUniVerticalItem }

  TUniVerticalItem = class(TUniItem)
  protected
    // IUniItem = interface
    function GetMMSize: integer; override;
    function GetPlace: integer; override;
    function GetSize: integer; override;
    function GetStart: integer; override;
    procedure SetMMSize(AValue: integer); override;
    procedure SetPlace(AValue: integer); override;
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
    function ElasticMMSize(const ANode: INode; const AClass: TUniItemClass): integer;
    function ResizeFixed(const ANode: INode; const AClass: TUniItemClass; const AScale: IScale): integer;
    function ResizeElastic(const ANode: INode; const AClass: TUniItemClass; const AElasticMMTotal, AElasticTotal: integer): integer;
    procedure Spread(const ANode: INode; const AClass: TUniItemClass; ASize, AUsedSize: integer);
    procedure Replace(const ANode: INode; const AClass: TUniItemClass; ASize: integer; const AScale: IScale);
  protected
    // ITiler
    procedure ReplaceChildren(const AContainer: IBit);
  protected
    fHScale: IScale;
    fVScale: IScale;
  published
    property HScale: IScale read fHScale write fHScale;
    property VScale: IScale read fVScale write fVScale;
  end;

  { TScale }

  TScale = class(TInterfacedObject, IScale)
  protected
    // IScale
    function Scale(const ASize: integer): integer;
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

procedure TScale.AfterConstruction;
begin
  inherited AfterConstruction;
  fMultiplicator := 1;
  fDivider := 1;
end;

{ TDesktopTiler }

function TDesktopTiler.ElasticMMSize(const ANode: INode; const AClass: TUniItemClass): integer;
var
  mChild: INode;
  mUni: IUniItem;
begin
  Result := 0;
  for mChild in ANode do begin
    mUni := AClass.Create(mChild);
    case mUni.Place of
      cPlace.Elastic:
        Result := Result + mUni.MMSize;
    end;
  end;
end;

function TDesktopTiler.ResizeFixed(const ANode: INode; const AClass: TUniItemClass;
  const AScale: IScale): integer;
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
          mUni.Size :=  AScale.Scale(mUni.MMSize);
          Result := Result + mUni.Size;
        end;
    end;
  end;
end;

function TDesktopTiler.ResizeElastic(const ANode: INode; const AClass: TUniItemClass;
  const AElasticMMTotal, AElasticTotal: integer): integer;
var
  mChild: INode;
  mUni: IUniItem;
begin
  Result := 0;
  for mChild in ANode do begin
    mUni := AClass.Create(mChild);
    case mUni.Place of
      cPlace.Elastic:
        begin
          if AElasticMMTotal = 0 then
          begin
            // this is special case when all sizes are 0 - so we resize them equally
            mUni.Size :=  Round(AElasticTotal / ANode.Count);
          end
          else
          begin
            mUni.Size :=  Round(AElasticTotal * mUni.MMSize / AElasticMMTotal);
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
  const AClass: TUniItemClass; ASize: integer; const AScale: IScale);
var
  mElasticMMTotal: integer;
  mFixedSize: integer;
  mElasticSize: integer;
begin
  mElasticMMTotal := ElasticMMSize(ANode, AClass);
  mFixedSize := ResizeFixed(ANode, AClass, AScale);
  mElasticSize := ResizeElastic(ANode, AClass, mElasticMMTotal, ASize - mFixedSize);
  Spread(ANode, AClass, ASize, mFixedSize + mElasticSize);
end;

procedure TDesktopTiler.ReplaceChildren(const AContainer: IBit);
begin
  // setup same height and count width of fixed fields
  case (AContainer as IPlacement).Layout of
    cLayout.Horizontal:
      begin
        Replace(AContainer as INode, TUniHorizontalItem, (AContainer as IPlace).Width, HScale);
        Reposition(AContainer as INode, TUniVerticalItem, 0, (AContainer as IPlace).Height);
      end;
    cLayout.Vertical:
      begin
        Replace(AContainer as INode, TUniVerticalItem, (AContainer as IPlace).Height, VScale);
        Reposition(AContainer as INode, TUniHorizontalItem, 0, (AContainer as IPlace).Width);
      end;
    cLayout.Overlay:
      begin
        Reposition(AContainer as INode, TUniHorizontalItem, 0, (AContainer as IPlace).Width);
        Reposition(AContainer as INode, TUniVerticalItem, 0, (AContainer as IPlace).Height);
      end;
  end;
end;

{ TUniVerticalItem }

function TUniVerticalItem.GetMMSize: integer;
begin
  Result := (fItem as IPlacement).MMHeight;
end;

function TUniVerticalItem.GetPlace: integer;
begin
  Result := (fItem as IPlacement).Place;
end;

function TUniVerticalItem.GetSize: integer;
begin
  Result := (fItem as IPlace).Height;
end;

function TUniVerticalItem.GetStart: integer;
begin
  Result := (fItem as IPlace).Top;
end;

procedure TUniVerticalItem.SetMMSize(AValue: integer);
begin
  (fItem as IPlacement).MMHeight := AValue;
end;

procedure TUniVerticalItem.SetPlace(AValue: integer);
begin
  (fItem as IPlacement).Place := AValue;
end;

procedure TUniVerticalItem.SetSize(AValue: integer);
begin
  (fItem as IPlace).Height := AValue;
end;

procedure TUniVerticalItem.SetStart(AValue: integer);
begin
  (fItem as IPlace).Top := AValue;
end;

{ TUniItem }

constructor TUniItem.Create(const AItem: INode);
begin
  fItem := AItem;
end;

{ TUniHorizontalItem }

function TUniHorizontalItem.GetMMSize: integer;
begin
  Result := (fItem as IPlacement).MMWidth;
end;

function TUniHorizontalItem.GetPlace: integer;
begin
  Result := (fItem as IPlacement).Place;
end;

function TUniHorizontalItem.GetSize: integer;
begin
  Result := (fItem as IPlace).Width;
end;

function TUniHorizontalItem.GetStart: integer;
begin
  Result := (fItem as IPlace).Left;
end;

procedure TUniHorizontalItem.SetMMSize(AValue: integer);
begin
  (fItem as IPlacement).MMWidth := AValue;
end;

procedure TUniHorizontalItem.SetPlace(AValue: integer);
begin
  (fItem as IPlacement).Place := AValue;
end;

procedure TUniHorizontalItem.SetSize(AValue: integer);
begin
  (fItem as IPlace).Width := AValue;
end;

procedure TUniHorizontalItem.SetStart(AValue: integer);
begin
  (fItem as IPlace).Left := AValue;
end;

end.

