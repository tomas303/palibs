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
unit trl_umetaelementfactory;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelementfactory, trl_udifactory, trl_imetaelement, trl_ilog,
  trl_iprops, trl_uprops, SysUtils, trl_itree;

type
  { TMetaElementFactory }

  TMetaElementFactory = class(TDIFactory, IMetaElementFactory)
  protected
    //IMetaElementFactory
    function CreateElement(const ATypeGuid: TGuid; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AChildren: array of IMetaElement;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps;
      const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string;
      const AProps: IProps; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string;
      const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps;
      const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
  protected
    fLog: ILog;
  published
    property Log: ILog read fLog write fLog;
  end;

implementation

{ TMetaElementFactory }

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', TProps.New, [], AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const AProps: IProps; AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', AProps, [], AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', TProps.New, AChildren, AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const AProps: IProps; const AChildren: array of IMetaElement;
  AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, '', AProps, AChildren, AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, ATypeID, TProps.New, [], AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; const AProps: IProps;
  AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, ATypeID, AProps, [], AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; const AChildren: array of IMetaElement;
  AIsMetaElementProvider: Boolean = false): IMetaElement;
begin
  Result := CreateElement(ATypeGuid, ATypeID, TProps.New, AChildren, AIsMetaElementProvider);
end;

function TMetaElementFactory.CreateElement(const ATypeGuid: TGuid;
  const ATypeID: string; const AProps: IProps;
  const AChildren: array of IMetaElement;
  AIsMetaElementProvider: Boolean = false): IMetaElement;
var
  mChild: IMetaElement;
  mP: Pointer;
begin
  mP := Locate(IMetaElement, '',
    TProps.New
    .SetStr('TypeGuid', GUIDToString(ATypeGuid))
    .SetStr('TypeID', ATypeID)
    .SetIntf('Props', AProps)
    .SetBool('IsMetaElementProvider', AIsMetaElementProvider)
    );
  Result := IMetaElement(mP);
  for mChild in AChildren do begin
    (Result as INode).AddChild(mChild as INode);
  end;
end;

end.

