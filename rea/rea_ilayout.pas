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

  { IPlacement }

  IPlacement = interface
  ['{197AD85C-B4FC-4AC8-8951-9BC04E06FB25}']
    function GetLayout: integer;
    function GetPlace: integer;
    function GetMMWidth: integer;
    function GetMMHeight: integer;
    procedure SetLayout(AValue: integer);
    procedure SetPlace(AValue: integer);
    procedure SetMMWidth(AValue: integer);
    procedure SetMMHeight(AValue: integer);
    // how organize child elements
    property Layout: integer read GetLayout write SetLayout;
    // how element should be placed inside parent
    property Place: integer read GetPlace write SetPlace;
    // horizontal width in 10 x milimmeters
    property MMWidth: integer read GetMMWidth write SetMMWidth;
    // vertical height in 10 x milimmeters
    property MMHeight: integer read GetMMHeight write SetMMHeight;
  end;

  { IPlace }

  IPlace = interface
  ['{CF0A8291-FA25-4D20-B12C-8A417E32B3D4}']
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure SetHeight(AValue: integer);
    // pixels left
    property Left: integer read GetLeft write SetLeft;
    // pixels top
    property Top: integer read GetTop write SetTop;
    // pixels width
    property Width: integer read GetWidth write SetWidth;
    // pixels height
    property Height: integer read GetHeight write SetHeight;
  end;

  { IUIRelativePoint }

  ITiler = interface
  ['{E658DAE2-51E6-4CC0-86F6-055E5888E5E4}']
    procedure ReplaceChildren(const AContainer: IBit);
  end;

  IScale = interface
  ['{1AD4124D-F767-4E1E-AC33-303384E9FBE9}']
    function Scale(const ASize: integer): integer;
  end;


implementation

end.

