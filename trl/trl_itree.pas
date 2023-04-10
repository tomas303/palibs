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
unit trl_itree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  INode = interface;

  INodeEnumerator = interface
  ['{0C7E3A23-2552-4D0A-82D1-51496D5BFF96}']
    function MoveNext: Boolean;
    function GetCurrent: INode;
    property Current: INode read GetCurrent;
  end;

  INode = interface
  ['{74337291-3342-4AA5-A3D1-90F430315C23}']
    //function GetParent: INode;
    //property Parent: INode read GetParent;
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure ExchangeChild(const AFromNode, AToNode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function GetEnumerator: INodeEnumerator;
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    property Child[const AIndex: integer]: INode read GetChild; default;
  end;

implementation

end.

