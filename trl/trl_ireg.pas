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
unit trl_ireg;

{$mode objfpc}{$H+}

interface

uses
  trl_dicontainer;

type
  IReg = interface
  ['{57660B86-1277-47D3-845B-FB81A70203D0}']
    function RegisterSysUtils: TDIReg;
    function RegisterDIOwner: TDIReg;
    function RegisterDIFactory: TDIReg;
    function RegisterDIFactory2: TDIReg;
    function RegisterInjector: TDIReg;
    function RegisterProps: TDIReg;
    procedure RegisterTreeNodes;
    procedure RegisterLink;
    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReconciler: TDIReg;
    function RegisterSequence(const AID: string; ACreateKind: TDIRegCreateKind = ckSingle): TDIReg;
    function RegisterPubSub: TDIReg;
    procedure RegisterCommon;
  end;

implementation

end.

