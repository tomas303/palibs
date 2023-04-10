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
unit trl_uinjector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_iinjector, trl_iprops, trl_irttibroker, trl_urttibroker, typinfo;

type

  { TInjector }

  TInjector = class(TInterfacedObject, IInjector)
  protected
    function FindSelfProps(const ARBData: IRBData): IProps;
  protected
    // IInjector
    procedure Write(AInstance: TObject; const AProps: IProps);
    procedure Read(AInstance: TObject; const AProps: IProps);
    procedure Read(AInstance: TObject; const AName: string; const AProps: IProps);
  public
    class function New: IInjector;
  end;

implementation

  { TInjector }

  function TInjector.FindSelfProps(const ARBData: IRBData): IProps;
  var
    mRBItem: IRBDataItem;
  begin
    mRBItem := ARBData.FindItem('SELFPROPS');
    if mRBItem <> nil then
      Result := mRBItem.AsInterface as IProps
    else
      Result := nil;
  end;

  procedure TInjector.Write(AInstance: TObject; const AProps: IProps);
  var
    mRB: IRBData;
    mRBItem: IRBDataItem;
    i: integer;
    mSelfProps: IProps;
  begin
    if AProps = nil then
      Exit;
    mRB := TRBData.Create(AInstance, True);
    mSelfProps := FindSelfProps(mRB);
    for i := 0 to AProps.Count - 1 do begin
      mRBItem := mRB.FindItem(AProps.Name(i));
      if mRBItem = nil then
        Continue;
      case AProps.PropType(i) of
        ptInt:
          mRBItem.AsInteger := AProps.AsInt(i);
        ptStr:
          mRBItem.AsString := AProps.AsStr(i);
        ptBool:
          mRBItem.AsBoolean := AProps.AsBool(i);
        ptGuid:
          case mRBItem.TypeKind of
            tkAString:
              mRBItem.AsString := GUIDToString(AProps.AsGuid(i));
            else
              raise Exception.CreateFmt('Error when injecting property "%s.%s": ' + LineEnding, [mRB.ClassName, mRBItem.Name]);
          end;
        ptInterface:
          mRBItem.AsInterface := AProps.AsIntf(i);
      end;
      if mSelfProps <> nil then
        mSelfProps.SetProp(AProps[i].Name, AProps[i]);
    end;
  end;

  procedure TInjector.Read(AInstance: TObject; const AProps: IProps);
  begin
    Read(AInstance, '', AProps);
  end;

  procedure TInjector.Read(AInstance: TObject; const AName: string;
    const AProps: IProps);
  var
    mRB: IRBData;
    mRBItem: IRBDataItem;
    i: integer;
  //  mInterface: IUnknown;
  begin
    if AProps = nil then
      Exit;
    if AName <> '' then
      mRB := TRBData.Create(AInstance, AName)
    else
      mRB := TRBData.Create(AInstance, True);
    for i := 0 to AProps.Count - 1 do begin
      mRBItem := mRB.FindItem(AProps.Name(i));
      if mRBItem = nil then
        Continue;
      case AProps.PropType(i) of
        ptInt:
          AProps.SetInt(AProps.Name(i), mRBItem.AsInteger);
        ptStr:
          AProps.SetStr(AProps.Name(i), mRBItem.AsString);
        ptBool:
          AProps.SetBool(AProps.Name(i), mRBItem.AsBoolean);
        ptGuid:
          case mRBItem.TypeKind of
            tkAString:
              AProps.SetGuid(AProps.Name(i), StringToGUID(mRBItem.AsString));
            else
              raise Exception.CreateFmt('Error when injecting property "%s.%s": ' + LineEnding, [mRB.ClassName, mRBItem.Name]);
          end;
        ptInterface:
          AProps.SetIntf(AProps.Name(i), mRBItem.AsInterface);
      end;
    end;
  end;

  class function TInjector.New: IInjector;
  begin
    Result := TInjector.Create;
  end;

end.

