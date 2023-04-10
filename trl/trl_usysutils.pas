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
unit trl_usysutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_isysutils, base64;

type

  { TSysUtils }

  TSysUtils = class(TInterfacedObject, ISysUtils)
  protected
    // ISysUtils
    function NewGID: string;
    function NewGuid: TGuid;
  end;

implementation

{ TSysUtils }

function TSysUtils.NewGID: string;
var
  mGuid: TGuid;
  mEncStream: TBase64EncodingStream;
  mStrStream: TStringStream;
begin
  mGuid := NewGuid;
  mStrStream := TStringStream.Create;
  try
    mEncStream := TBase64EncodingStream.Create(mStrStream);
    try
      mEncStream.Write(mGuid, SizeOf(mGuid));
    finally
      mEncStream.Free;
    end;
    Result := mStrStream.DataString;
    while Result[Length(Result)] = '=' do
      Delete(Result, Length(Result), 1);
  finally
    mStrStream.Free;
  end;
end;

function TSysUtils.NewGuid: TGuid;
begin
  CreateGUID(Result);
end;

end.

