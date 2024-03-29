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
unit trl_udifactory;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_idifactory, trl_dicontainer, trl_iprops, TypInfo;

type

  { TDIFactory }

  TDIFactory = class(TCustomDIFactory, IDIFactory)
  protected
    //IDIFactory
    function Locate(AClass: TClass; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function Locate(AInterface: TGUID; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function Locate(const AClass: string; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function CanLocateAs(AClass: TClass; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(AClass: TClass; const AID: string; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AInterface: TGUID; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AInterface: TGUID; const AID: string; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AClass: string; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AClass: string; const AID: string; const AAsInterface: TGUID): Boolean; overload;
  end;

  { TDIFactory2 }

  TDIFactory2 = class(TCustomDIFactory)
  //todo ... generic function are not accessible via interface, do some memory management
  // now each instance will cause memory leak
  private
    function GetGuid<I: IUnknown>: TGuid;
  public
    function Locate<I: IUnknown>: I; overload;
    function Locate<I: IUnknown>(const AID: string): I; overload;
    function Locate<I: IUnknown>(const AClass: TClass): I; overload;
    function Locate<I: IUnknown>(const AProps: IProps): I; overload;
    function Locate<I: IUnknown>(const AID: string; const AProps: IProps): I; overload;
    function Locate<I: IUnknown>(const AClass: TClass; const AProps: IProps): I; overload;
    function LocateC<C: TObject>: C;
  end;

implementation

{ TDIFactory2 }

function TDIFactory2.GetGuid<I>: TGuid;
var
  mP: PInterfaceData;
begin
  mP := PInterfaceData(GetTypeData(TypeInfo(I)));
  Result := mP.GUID;
end;

function TDIFactory2.Locate<I>: I;
begin
  Result := I(Container.Locate(GetGuid<I>, '', nil));
end;

function TDIFactory2.Locate<I>(const AID: string): I;
begin
  Result := I(Container.Locate(GetGuid<I>, AID, nil));
end;

function TDIFactory2.Locate<I>(const AClass: TClass): I;
begin
  Result := Locate<I>(AClass.ClassName);
end;

function TDIFactory2.Locate<I>(const AProps: IProps): I;
begin
  Result := I(Container.Locate(GetGuid<I>, '', AProps));
end;

function TDIFactory2.Locate<I>(const AID: string; const AProps: IProps): I;
begin
  Result := I(Container.Locate(GetGuid<I>, AID, AProps));
end;

function TDIFactory2.Locate<I>(const AClass: TClass; const AProps: IProps): I;
begin
  Result := Locate<I>(AClass.ClassName, AProps);
end;

function TDIFactory2.LocateC<C>: C;
begin
  Result := C(Container.Locate(C));
end;


{ TDIFactory }

function TDIFactory.Locate(AClass: TClass; const AID: string; const AProps: IProps = nil): pointer;
begin
  Result := Container.Locate(AClass, AID, AProps);
end;

function TDIFactory.Locate(AInterface: TGUID; const AID: string; const AProps: IProps = nil): pointer;
begin
  Result := Container.Locate(AInterface, AID, AProps);
end;

function TDIFactory.Locate(const AClass: string; const AID: string; const AProps: IProps = nil): pointer;
begin
  Result := Container.Locate(AClass, AID, AProps);
end;

function TDIFactory.CanLocateAs(AClass: TClass; const AAsInterface: TGUID
  ): Boolean;
begin
  Result := Container.CanLocateAs(AClass, AAsInterface);
end;

function TDIFactory.CanLocateAs(AClass: TClass; const AID: string;
  const AAsInterface: TGUID): Boolean;
begin
  Result := Container.CanLocateAs(AClass, AID, AAsInterface);
end;

function TDIFactory.CanLocateAs(const AInterface: TGUID;
  const AAsInterface: TGUID): Boolean;
begin
  Result := Container.CanLocateAs(AInterface, AAsInterface);
end;

function TDIFactory.CanLocateAs(const AInterface: TGUID; const AID: string;
  const AAsInterface: TGUID): Boolean;
begin
  Result := Container.CanLocateAs(AInterface, AID, AAsInterface);
end;

function TDIFactory.CanLocateAs(const AClass: string; const AAsInterface: TGUID
  ): Boolean;
begin
  Result := Container.CanLocateAs(AClass, AAsInterface);
end;

function TDIFactory.CanLocateAs(const AClass: string; const AID: string;
  const AAsInterface: TGUID): Boolean;
begin
  Result := Container.CanLocateAs(AClass, AID, AAsInterface);
end;

end.

