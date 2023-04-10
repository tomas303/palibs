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
unit trl_ulink;

{$mode objfpc}{$H+}

interface

uses
  trl_ilink, sysutils;

type

  { TLink }

  TLink = class(TInterfacedObject, ILink)
  protected
    fNext: ILink;
  protected
    // ILink
    function Next: ILink;
    function Last: ILink;
    function Insert(const ALink: ILink): ILink;
    function Split: ILink;
  end;

implementation

{ TLink }

function TLink.Last: ILink;
begin
  Result := Self;
  while Result.Next <> nil do
    Result := Result.Next;
end;

function TLink.Next: ILink;
begin
  Result := fNext;
end;

function TLink.Insert(const ALink: ILink): ILink;
begin
  if Next <> nil then
    ALink.Last.Insert(Next);
  fNext := ALink;
  Result := ALink;
end;

function TLink.Split: ILink;
begin
  Result := fNext;
  fNext := nil;
end;

end.

