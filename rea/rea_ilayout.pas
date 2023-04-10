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

  cFontDirection = class
  public const
    Horizontal = 0;
    VertLeft = 1;
    VertRight = 2;
  end;

  { IBitPosition }

  IBitPosition = interface
  ['{6F549B99-D125-4D6C-B93D-E4ACB2B59E4D}']
    function GetLayout: integer;
    function GetPlace: integer;
    function GetPlaceSize: integer;
    property Layout: integer read GetLayout;
    property Place: integer read GetPlace;
    property PlaceSize: integer read GetPlaceSize;
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
    procedure ReplaceChildren(const AContainer: IBit; ABorder: Integer = 0);
  end;

  { IScale }

  IScale = interface
  ['{1AD4124D-F767-4E1E-AC33-303384E9FBE9}']
    function Scale(const ASize: integer): integer;
    function Unscale(const ASize: integer): integer;
  end;


implementation

end.

