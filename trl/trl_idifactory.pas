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
unit trl_idifactory;

{$mode delphi}{$H+}

interface

uses
  trl_iprops;

type
  IDIFactory = interface
  ['{80F7C3AD-0120-4792-9404-F48B6695A14D}']
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

implementation

end.

