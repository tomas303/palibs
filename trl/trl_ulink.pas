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

