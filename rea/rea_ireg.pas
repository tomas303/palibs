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
unit rea_ireg;

{$mode objfpc}{$H+}

interface

uses
  Classes, trl_dicontainer;

const
  cR_DesktopTiler = 'desktoptiler';

type
  IReg = interface
  ['{E5558E11-D2B3-439A-AAB0-A124C82F9D1D}']
    function RegisterBitTerminus(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string): TDIReg;
    function RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string; const ATilerID: string): TDIReg;
    function RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid; const ATilerID: string): TDIReg;
    function RegisterBitTiler(ATilerClass: TClass; ATilerInterface: TGuid; const ATilerID: string;
      AScaleClass: TClass): TDIReg;
    function RegisterMessageObservable: TDIReg;
    procedure RegisterScales;
    procedure RegisterCommon;
    function RegisterDesignComponent(AComponentClass: TClass; AComponentInterface: TGuid): TDIReg;
    function RegisterRenderer: TDIReg;
    function RegisterPubSubLauncher: TDIReg;
    function RegisterStoreConnector: TDIReg;
    function RegisterMorph: TDIReg;
  end;

implementation

end.

