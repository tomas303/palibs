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
unit trl_ilog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  ILogSupport = interface
  ['{BCAA7B90-8D8A-46CB-8F72-B1FB29A3D399}']
    function LogInfo: string;
  end;

  { ILog }

  ILog = interface
  ['{5F619DCC-AC09-4617-B9F5-7789E36FA39C}']
    procedure DebugLn(const s: string = ''); overload;
    procedure DebugLn(const S: String; Args: array of const); overload;
    procedure DebugLnEnter(const s: string = ''); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s: string = ''); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

implementation

end.

