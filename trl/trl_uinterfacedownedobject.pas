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
unit trl_uinterfacedownedobject;

{$mode objfpc}{$H+}

interface

type

  { TInterfacedOwnedObject }

  {
   When OwningObject is not set, class beahves exactly like TInterfacedObject.
   If set, then class share RefCounter wit it in similar manner like TContainedObject.
   When OwningObject is released, this class release itself automaticly.
   And why it was invented:
       When class implements interface, one of posible way is to redirect
       implementation via interface property. And for this you need shared refcounter
       between class and property and somehow manage to automaticly free object
       under this property.
  }

  TInterfacedOwnedObject = class(TInterfacedObject)
  private
    fOwningObject: TInterfacedObject;
    function OwningAsUnknown: IUnknown;
    procedure SetOwningObject(AValue: TInterfacedObject);
  protected
    { implement methods of IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  published
    property OwningObject: TInterfacedObject read fOwningObject write SetOwningObject;
  end;

implementation

{ TInterfacedOwnedObject }

function TInterfacedOwnedObject.OwningAsUnknown: IUnknown;
begin
  Result := IUnknown(fOwningObject);
end;

procedure TInterfacedOwnedObject.SetOwningObject(AValue: TInterfacedObject);
var
  i: integer;
begin
  fOwningObject := AValue;
  for i := 1 to frefcount do
    OwningAsUnknown._AddRef;
  InterLockedExchange(frefcount, 0);
end;

function TInterfacedOwnedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;
  out obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TInterfacedOwnedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if fOwningObject = nil then
    Result := inherited _AddRef
  else
    Result := OwningAsUnknown._AddRef;
end;

function TInterfacedOwnedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if fOwningObject = nil then
    Result := inherited _Release
  else
  begin
    if fOwningObject.RefCount = 0 then
      self.Destroy
    else
      Result := OwningAsUnknown._Release;
  end;
end;

end.

