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
unit trl_usystem;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, trl_uprops, trl_irttibroker, trl_urttibroker;

type

  { TDynaObject }

  TDynaObject = class(TInterfacedObject)
  public
    class function newinstance : tobject;override;
  protected
    fSelfProps: IProps;
    procedure InitValues; virtual;
    procedure LocateFinished(var Msg); message 'LocateFinished';
  published
    property SelfProps: IProps read fSelfProps;
  end;

  { TPlainObject }

  TPlainObject = class(TObject)
  private
    fRB: IRBData;
    procedure FreePublishedObjects;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property RB: IRBData read fRB;
  end;

implementation

{ TPlainObject }

procedure TPlainObject.FreePublishedObjects;
var
  i: integer;
begin
  for i := 0 to RB.Count - 1 do begin
    if RB[i].TypeKind = tkClass then begin
      RB[i].AsObject.Free;
    end;
  end;
end;

procedure TPlainObject.AfterConstruction;
begin
  inherited AfterConstruction;
  fRB := TRBData.Create(Self);
end;

procedure TPlainObject.BeforeDestruction;
begin
  FreePublishedObjects;
  inherited BeforeDestruction;
end;

{ TDynaObject }

class function TDynaObject.newinstance: tobject;
begin
  Result := inherited newinstance;
  (Result as TDynaObject).fSelfProps := TProps.Create;
end;

procedure TDynaObject.InitValues;
begin
end;

procedure TDynaObject.LocateFinished(var Msg);
begin
  InitValues;
end;

end.

