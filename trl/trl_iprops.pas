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
unit trl_iprops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPropType = (ptUndefined, ptInt, ptStr, ptBool, ptGuid, ptInterface, ptObject);
  TPropDiffMode = (pdmAll, pdmDifferent);

  IProp = interface
  ['{B36E5E43-AFB2-4A28-8E09-E7BC682BE1F0}']
    function Equals(const AProp: IProp): Boolean;
    function Clone(const AName: string = ''): IProp;
    function GetName: string;
    function GetPropType: TPropType; //??? probably own type and if necessary map it to typekind
    function GetDebugInfo: string;
    function GetAsInteger: integer;
    procedure SetAsInteger(AValue: integer);
    function GetAsString: string;
    procedure SetAsString(AValue: string);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(AValue: Boolean);
    function GetAsObject: TObject;
    procedure SetAsObject(AValue: TObject);
    function GetAsVariant: Variant;
    procedure SetAsVariant(AValue: Variant);
    function GetAsPtrInt: PtrInt;
    procedure SetAsPtrInt(AValue: PtrInt);
    function GetAsInterface: IUnknown;
    procedure SetAsInterface(AValue: IUnknown);
    function GetAsTGuid: TGuid;
    procedure SetAsTGuid(AValue: TGuid);
    property Name: string read GetName;
    property PropType: TPropType read GetPropType;
    property DebugInfo: string read GetDebugInfo;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property AsPtrInt: PtrInt read GetAsPtrInt write SetAsPtrInt;
    property AsInterface: IUnknown read GetAsInterface write SetAsInterface;
    property AsTGuid: TGuid read GetAsTGuid write SetAsTGuid;
  end;

  // maybe good make it immutable ... like Lock without unlock, or readonly
  // SetIt.SetIt.SetIt.ReadOnly;
  // better to use Writeable field(strict private) so by default is false
  // and is set only in some builder methods - then and back

  { IProps }

  IProps = interface
  ['{D3841623-443A-4177-8FED-DDA65A75CD43}']
    function Count: integer;
    function GetProp(AIndex: integer): IProp;
    property Prop[AIndex: integer]: IProp read GetProp; default;
    function GetPropByName(const AName: string): IProp;
    property PropByName[const AName: string]: IProp read GetPropByName;
    function Equals(const AProps: IProps): Boolean;
    function Clone: IProps;
    function Clone(ANames: array of string): IProps;
    function PropType(const AName: string): TPropType;
    function PropType(const AIndex: integer): TPropType;
    function Name(const AIndex: integer): string;
    function SetIt(const AName: string): IProps;
    function SetStr(const AName: string; const AValue: string): IProps;
    function SetInt(const AName: string; const AValue: integer): IProps;
    function SetBool(const AName: string; const AValue: Boolean): IProps;
    function SetGuid(const AName: string; const AValue: TGUID): IProps;
    function SetIntf(const AName: string; const AValue: IUnknown): IProps;
    function SetObject(const AName: string; const AValue: TObject): IProps;
    function SetStr(const AIndex: integer; const AValue: string): IProps;
    function SetInt(const AIndex: integer; const AValue: integer): IProps;
    function SetBool(const AIndex: integer; const AValue: Boolean): IProps;
    function SetGuid(const AIndex: integer; const AValue: TGUID): IProps;
    function SetIntf(const AIndex: integer; const AValue: IUnknown): IProps;
    function SetObject(const AIndex: integer; const AValue: TObject): IProps;
    function SetProp(const AName: string; const AProp: IProp): IProps;
    function SetProp(const AProp: IProp): IProps;
    function AsStr(const AName: string): string;
    function AsInt(const AName: string): integer;
    function AsBool(const AName: string): Boolean;
    function AsGuid(const AName: string): TGUID;
    function AsIntf(const AName: string): IUnknown;
    function AsObject(const AName: string): TObject;
    function AsStr(const AIndex: integer): string;
    function AsInt(const AIndex: integer): integer;
    function AsBool(const AIndex: integer): Boolean;
    function AsGuid(const AIndex: integer): TGUID;
    function AsIntf(const AIndex: integer): IUnknown;
    function AsObject(const AIndex: integer): TObject;
    function Diff(const AProps: IProps; AMode: TPropDiffMode): IProps;
    function Info: string;
  end;

  TPropPath = array of string;

  IPropFinder = interface
  ['{982480ED-701A-4D12-8569-9C5B14F37502}']
    function Find(const AID: string): IProp;
  end;

  IPropsMap = interface
  ['{DA286FCA-FD6B-40F6-A713-95B933CF0F32}']
    procedure Map(const AProps: IProps);
  end;


implementation

end.

