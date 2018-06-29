unit rea_ilayout;

{$mode objfpc}{$H+}

interface

uses
  rea_ibits;

//const
//  // place - how inner control should be placed within container
//  uiPlaceElastic = 0;  // will use all free space if any
//  uiPlaceFixFront = 1;  // push to front(usually left or top)
//  uiPlaceFixMiddle = 2; // if in the middle, then space will be distrubeted evenly(otherwise is pushed)
//  uiPlaceFixBack = 3; // push to back(usually right or botttom)
//
//  // layout - in which direction container should place chidren
//  uiLayoutHorizontal = 0;  // horizontally
//  uiLayoutVertical = 1;  //vertically
//  uiLayoutOverlay = 2;  // all same size as container(suppose app. will make visible one of them)

type

  cPlace = class
  public const
    Elastic = 0;  // will use all free space if any
    FixFront = 1;  // push to front(usually left or top)
    FixMiddle = 2; // if in the middle, then space will be distrubeted evenly(otherwise is pushed)
    FixBack = 3; // push to back(usually right or botttom)
  end;

  cLayout = class
  public const
    Horizontal = 0;  // horizontally
    Vertical = 1;  //vertically
    Overlay = 2;  // all same size as container(suppose app. will make visible one of them)
  end;

  { IBitPosition }

  IBitPosition = interface
  ['{6F549B99-D125-4D6C-B93D-E4ACB2B59E4D}']
    function GetLayout: integer;
    function GetPlace: integer;
    property Layout: integer read GetLayout;
    property Place: integer read GetPlace;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure SetHeight(AValue: integer);
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  end;

  { ITiler }

  ITiler = interface
  ['{E658DAE2-51E6-4CC0-86F6-055E5888E5E4}']
    procedure ReplaceChildren(const AContainer: IBit);
  end;

  { IScale }

  IScale = interface
  ['{1AD4124D-F767-4E1E-AC33-303384E9FBE9}']
    function Scale(const ASize: integer): integer;
    function Unscale(const ASize: integer): integer;
  end;


implementation

end.

